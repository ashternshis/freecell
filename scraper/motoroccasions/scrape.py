"""Polite crawler for motoroccasions.nl -> SQLite.

Usage:
    python -m motoroccasions.scrape --db motoroccasions.sqlite3
    python -m motoroccasions.scrape --start-url https://www.motoroccasions.nl/occasions/ \
        --max-pages 50 --delay 2.0
"""

import argparse
import sys
import time
import urllib.robotparser
from collections import deque
from urllib.parse import urlparse

import requests

from . import db as dbmod
from .parse import (extract_listing_links, extract_pagination_links,
                    parse_listing_page)

DEFAULT_BASE = "https://www.motoroccasions.nl"
USER_AGENT = ("motoroccasions-scraper/0.1 "
              "(personal use; contact: ashternshis@gmail.com)")


class Crawler:
    def __init__(self, base_url, delay=2.0, timeout=30):
        self.base_url = base_url.rstrip("/")
        self.delay = delay
        self.timeout = timeout
        self.session = requests.Session()
        self.session.headers["User-Agent"] = USER_AGENT
        self.robots = self._load_robots()
        self._last_fetch = 0.0
        self.pages_fetched = 0

    def _load_robots(self):
        rp = urllib.robotparser.RobotFileParser()
        try:
            resp = self.session.get(f"{self.base_url}/robots.txt",
                                    timeout=self.timeout)
            if resp.status_code == 200:
                rp.parse(resp.text.splitlines())
            else:
                rp.parse([])
        except requests.RequestException:
            rp.parse([])
        return rp

    def allowed(self, url):
        return self.robots.can_fetch(USER_AGENT, url)

    def fetch(self, url):
        """Rate-limited GET. Returns response text or None."""
        if not self.allowed(url):
            print(f"  robots.txt disallows {url}, skipping", file=sys.stderr)
            return None
        wait = self.delay - (time.monotonic() - self._last_fetch)
        if wait > 0:
            time.sleep(wait)
        self._last_fetch = time.monotonic()
        try:
            resp = self.session.get(url, timeout=self.timeout)
        except requests.RequestException as exc:
            print(f"  fetch failed for {url}: {exc}", file=sys.stderr)
            return None
        self.pages_fetched += 1
        if resp.status_code != 200:
            print(f"  HTTP {resp.status_code} for {url}", file=sys.stderr)
            return None
        return resp.text

    def same_host(self, url):
        return urlparse(url).netloc.endswith(
            urlparse(self.base_url).netloc.removeprefix("www."))


def crawl(conn, start_urls, base_url, delay, max_pages, max_listings):
    crawler = Crawler(base_url, delay=delay)
    run_id = dbmod.start_run(conn)
    found = new = 0
    index_queue = deque(start_urls)
    listing_queue = deque()
    visited = set()
    status, note = "ok", None

    try:
        while (index_queue or listing_queue) \
                and crawler.pages_fetched < max_pages:
            if listing_queue and (max_listings is None
                                  or found < max_listings):
                url = listing_queue.popleft()
                if url in visited:
                    continue
                visited.add(url)
                html = crawler.fetch(url)
                if html is None:
                    continue
                listing = parse_listing_page(html, url)
                if listing:
                    found += 1
                    if dbmod.upsert_listing(conn, listing):
                        new += 1
                        print(f"+ {listing.get('title') or url}")
                    else:
                        print(f"= {listing.get('title') or url}")
                continue

            if not index_queue:
                break
            url = index_queue.popleft()
            if url in visited:
                continue
            visited.add(url)
            html = crawler.fetch(url)
            if html is None:
                continue
            for link in extract_listing_links(html, url):
                if crawler.same_host(link) and link not in visited:
                    listing_queue.append(link)
            for link in extract_pagination_links(html, url):
                if crawler.same_host(link) and link not in visited:
                    index_queue.append(link)

        if crawler.pages_fetched == 0:
            status, note = "error", (
                "No pages could be fetched. As of 2026-07 the site serves a "
                "self-signed certificate and returns HTTP 503 — it appears "
                "to be offline.")
    except KeyboardInterrupt:
        status, note = "interrupted", "stopped by user"
    finally:
        dbmod.finish_run(conn, run_id, crawler.pages_fetched, found, new,
                         status, note)
    return crawler.pages_fetched, found, new, status, note


def main(argv=None):
    ap = argparse.ArgumentParser(
        description="Scrape motoroccasions.nl listings into SQLite.")
    ap.add_argument("--db", default="motoroccasions.sqlite3",
                    help="SQLite database path (default: %(default)s)")
    ap.add_argument("--base-url", default=DEFAULT_BASE)
    ap.add_argument("--start-url", action="append", dest="start_urls",
                    help="Index page(s) to start from; may repeat. "
                         "Defaults to the site root.")
    ap.add_argument("--delay", type=float, default=2.0,
                    help="Seconds between requests (default: %(default)s)")
    ap.add_argument("--max-pages", type=int, default=200,
                    help="Stop after this many HTTP fetches "
                         "(default: %(default)s)")
    ap.add_argument("--max-listings", type=int, default=None,
                    help="Stop after storing this many listings")
    args = ap.parse_args(argv)

    conn = dbmod.connect(args.db)
    start_urls = args.start_urls or [args.base_url]
    pages, found, new, status, note = crawl(
        conn, start_urls, args.base_url, args.delay, args.max_pages,
        args.max_listings)

    total = conn.execute("SELECT COUNT(*) FROM listings").fetchone()[0]
    print(f"\nRun finished: status={status}, pages fetched={pages}, "
          f"listings found={found} ({new} new). "
          f"Database now holds {total} listings at {args.db}.")
    if note:
        print(f"Note: {note}")
    return 0 if status == "ok" and pages > 0 else 1


if __name__ == "__main__":
    sys.exit(main())
