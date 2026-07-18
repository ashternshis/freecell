"""Polite crawler for www.motoroccasion.nl -> SQLite.

Discovery order:
1. sitemap.xml (advertised in robots.txt) — filtered to listing URLs.
2. Fallback: crawl /motoren for brand pages (/motoren/zoeken/<brand>-b<id>.html),
   collect listing links, and page through each brand's results via the
   site's /mz.php AJAX endpoint (best-effort; the first 20 per brand come
   from the HTML page regardless).

The site sits behind Cloudflare. Requests from datacenter IPs get a
managed challenge ("Just a moment..."); from a residential connection the
scraper is expected to pass. A challenge is detected and reported clearly
instead of being retried.

Usage:
    python -m motoroccasion.scrape --db motoroccasion.sqlite3
    python -m motoroccasion.scrape --brand yamaha --max-listings 100
"""

import argparse
import sys
import time
import urllib.robotparser
from collections import deque
from urllib.parse import urljoin, urlparse

import requests

from . import db as dbmod
from .parse import (LISTING_HREF_RE, extract_index_links,
                    extract_listing_links, extract_pagination_requests,
                    is_cloudflare_challenge, listing_id_from_url,
                    parse_listing_page, parse_sitemap)

DEFAULT_BASE = "https://www.motoroccasion.nl"
USER_AGENT = ("Mozilla/5.0 (X11; Linux x86_64) motoroccasion-scraper/0.2 "
              "(personal use; contact: ashternshis@gmail.com)")


class ChallengeBlocked(Exception):
    """Cloudflare answered with a managed challenge; retrying won't help."""


class Crawler:
    def __init__(self, base_url, delay=2.0, timeout=30):
        self.base_url = base_url.rstrip("/")
        self.delay = delay
        self.timeout = timeout
        self.session = requests.Session()
        self.session.headers.update({
            "User-Agent": USER_AGENT,
            "Accept": "text/html,application/xhtml+xml,application/xml",
            "Accept-Language": "nl,en;q=0.8",
        })
        self._last_fetch = 0.0
        self.pages_fetched = 0
        self.robots = self._load_robots()

    def _throttled_get(self, url, **kwargs):
        wait = self.delay - (time.monotonic() - self._last_fetch)
        if wait > 0:
            time.sleep(wait)
        self._last_fetch = time.monotonic()
        return self.session.get(url, timeout=self.timeout, **kwargs)

    def _load_robots(self):
        rp = urllib.robotparser.RobotFileParser()
        try:
            resp = self._throttled_get(f"{self.base_url}/robots.txt")
            rp.parse(resp.text.splitlines() if resp.status_code == 200 else [])
        except requests.RequestException:
            rp.parse([])
        return rp

    def allowed(self, url):
        return self.robots.can_fetch(USER_AGENT, url)

    def fetch(self, url, params=None, referer=None):
        """Rate-limited GET. Returns text, or None on a non-200/error.

        Raises ChallengeBlocked on a Cloudflare managed challenge, since
        every subsequent request would fail the same way.
        """
        if not self.allowed(url):
            print(f"  robots.txt disallows {url}, skipping", file=sys.stderr)
            return None
        headers = {"Referer": referer} if referer else None
        try:
            resp = self._throttled_get(url, params=params, headers=headers)
        except requests.RequestException as exc:
            print(f"  fetch failed for {url}: {exc}", file=sys.stderr)
            return None
        self.pages_fetched += 1
        if "charset" not in resp.headers.get("Content-Type", "").lower():
            resp.encoding = "utf-8"
        if is_cloudflare_challenge(resp.status_code, resp.headers, resp.text):
            raise ChallengeBlocked(url)
        if resp.status_code != 200:
            print(f"  HTTP {resp.status_code} for {url}", file=sys.stderr)
            return None
        return resp.text

    def same_host(self, url):
        return urlparse(url).netloc.endswith(
            urlparse(self.base_url).netloc.removeprefix("www."))


def discover_from_sitemap(crawler, max_sitemaps=50):
    """Collect listing URLs from sitemap.xml (recursing into indexes)."""
    listing_urls, queue, seen = [], deque(), set()
    queue.append(f"{crawler.base_url}/sitemap.xml")
    while queue and len(seen) < max_sitemaps:
        sm_url = queue.popleft()
        if sm_url in seen:
            continue
        seen.add(sm_url)
        text = crawler.fetch(sm_url)
        if text is None:
            continue
        sitemaps, pages = parse_sitemap(text, sm_url)
        queue.extend(sitemaps)
        for page in pages:
            if LISTING_HREF_RE.search(page):
                listing_urls.append(page)
    return listing_urls


def discover_from_brand_pages(crawler, brands=None):
    """Yield listing URLs by walking brand pages + mz.php pagination."""
    root = f"{crawler.base_url}/motoren"
    html = crawler.fetch(root)
    if html is None:
        return
    index_pages = extract_index_links(html, root)
    if brands:
        wanted = [b.lower() for b in brands]
        index_pages = [u for u in index_pages
                       if any(b in u.lower() for b in wanted)]
    for page_url in index_pages:
        html = crawler.fetch(page_url)
        if html is None:
            continue
        yield from extract_listing_links(html, page_url)
        # Best-effort AJAX pagination: same params the site's own
        # getPagination() sends, brand context carried by session + referer.
        for params in extract_pagination_requests(html):
            fragment = crawler.fetch(urljoin(crawler.base_url, "/mz.php"),
                                     params=params, referer=page_url)
            if not fragment:
                break
            links = extract_listing_links(fragment, page_url)
            if not links:
                break
            yield from links


def crawl(conn, args):
    crawler = Crawler(args.base_url, delay=args.delay)
    run_id = dbmod.start_run(conn)
    found = new = 0
    status, note = "ok", None

    def store(url):
        nonlocal found, new
        html = crawler.fetch(url)
        if html is None:
            return
        listing = parse_listing_page(html, url)
        if not listing:
            print(f"? no data extracted from {url}", file=sys.stderr)
            return
        listing["site_id"] = listing_id_from_url(url)
        found += 1
        if dbmod.upsert_listing(conn, listing):
            new += 1
            print(f"+ {listing.get('title') or url}")
        else:
            print(f"= {listing.get('title') or url}")

    try:
        seen = set()

        def budget_left():
            return (crawler.pages_fetched < args.max_pages
                    and (args.max_listings is None
                         or found < args.max_listings))

        listing_urls = [] if args.no_sitemap else \
            discover_from_sitemap(crawler)
        if listing_urls:
            print(f"sitemap: {len(listing_urls)} listing URLs")
            for url in listing_urls:
                if not budget_left():
                    break
                if url not in seen:
                    seen.add(url)
                    store(url)
        else:
            if not args.no_sitemap:
                print("sitemap yielded no listings, crawling brand pages")
            for url in discover_from_brand_pages(crawler, args.brands):
                if not budget_left():
                    break
                if url not in seen:
                    seen.add(url)
                    store(url)

        if crawler.pages_fetched == 0:
            status, note = "error", "No pages could be fetched."
    except ChallengeBlocked as exc:
        status = "blocked"
        note = (f"Cloudflare presented a managed challenge at {exc}. "
                "This is IP-reputation based: run the scraper from a "
                "residential connection instead of a datacenter/VPN.")
    except KeyboardInterrupt:
        status, note = "interrupted", "stopped by user"
    finally:
        dbmod.finish_run(conn, run_id, crawler.pages_fetched, found, new,
                         status, note)
    return crawler.pages_fetched, found, new, status, note


def main(argv=None):
    ap = argparse.ArgumentParser(
        description="Scrape www.motoroccasion.nl listings into SQLite.")
    ap.add_argument("--db", default="motoroccasion.sqlite3",
                    help="SQLite database path (default: %(default)s)")
    ap.add_argument("--base-url", default=DEFAULT_BASE)
    ap.add_argument("--brand", action="append", dest="brands",
                    help="Limit the brand-page crawl to brands whose page "
                         "URL contains this string (e.g. yamaha); repeatable")
    ap.add_argument("--no-sitemap", action="store_true",
                    help="Skip sitemap discovery, go straight to brand pages")
    ap.add_argument("--delay", type=float, default=2.0,
                    help="Seconds between requests (default: %(default)s)")
    ap.add_argument("--max-pages", type=int, default=500,
                    help="Stop after this many HTTP fetches "
                         "(default: %(default)s)")
    ap.add_argument("--max-listings", type=int, default=None,
                    help="Stop after processing this many listings")
    args = ap.parse_args(argv)

    conn = dbmod.connect(args.db)
    pages, found, new, status, note = crawl(conn, args)

    total = conn.execute("SELECT COUNT(*) FROM listings").fetchone()[0]
    print(f"\nRun finished: status={status}, pages fetched={pages}, "
          f"listings found={found} ({new} new). "
          f"Database now holds {total} listings at {args.db}.")
    if note:
        print(f"Note: {note}")
    return 0 if status == "ok" and pages > 0 else 1


if __name__ == "__main__":
    sys.exit(main())
