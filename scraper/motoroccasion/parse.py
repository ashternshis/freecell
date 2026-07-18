"""Listing extraction for www.motoroccasion.nl.

Page structure (verified against the live site's markup as archived through
September 2025 by the Wayback Machine):

- Listing detail URLs look like ``/motoren/<slug>-m<id>.html``.
- Detail pages carry no JSON-LD. The header is
  ``<h1><span>BRAND</span> <span>MODEL - YEAR</span></h1>``, the price sits
  in ``span.full-tile-price`` ("€ 6.996,-"), and specs are label/value div
  pairs ("Bouwjaar:", "Kleur:", "Teller:", "Vermogen:", "Rijbewijs:",
  "Garantie:"). The dealer block is a table whose bold cell holds the
  dealer name, followed by street and city lines.
- Brand/search pages (``/motoren/zoeken/<brand>-b<id>.html``) list 20
  bikes and paginate via an AJAX call:
  ``getPagination({... s: '20', c: '20'}, '/mz.php', '#motors-wrapper')``.

A schema.org JSON-LD path is kept first anyway (future-proof), and generic
Dutch-label heuristics remain as a fallback for layout changes.
"""

import json
import re
from urllib.parse import urljoin
from xml.etree import ElementTree

from bs4 import BeautifulSoup

PRICE_RE = re.compile(
    r"(?:€|EUR)\s*([\d.,]+)|(?:prijs|vraagprijs)\D{0,10}([\d.,]+)",
    re.IGNORECASE,
)
YEAR_RE = re.compile(r"(?:bouwjaar|year)\D{0,10}((?:19|20)\d\d)",
                     re.IGNORECASE)
MILEAGE_RE = re.compile(
    r"(?:kilometerstand|km[- ]stand|teller(?:stand)?)\D{0,10}([\d.,]+)|"
    r"([\d.,]+)\s*km\b",
    re.IGNORECASE,
)
LISTING_HREF_RE = re.compile(r"/motoren/[^/?#]*-m(\d+)\.html", re.IGNORECASE)
PAGINATION_ONCLICK_RE = re.compile(r"getPagination\(\s*\{(.*?)\}", re.DOTALL)


def _to_number(text):
    """Parse a Dutch-formatted number: '6.996,-' -> 6996, '12.500,50' -> 12500.5."""
    if text is None:
        return None
    t = text.strip().replace("\xa0", "").replace(" ", "").rstrip(",-")
    if not t:
        return None
    if "," in t:
        t = t.replace(".", "").replace(",", ".")
    elif t.count(".") == 1 and len(t.split(".")[1]) != 3:
        pass  # single dot with non-thousands tail: already decimal
    else:
        t = t.replace(".", "")
    try:
        return float(t)
    except ValueError:
        return None


def listing_id_from_url(url):
    m = LISTING_HREF_RE.search(url)
    return m.group(1) if m else None


# --- JSON-LD path (not currently emitted by the site, kept future-proof) ---

def _iter_jsonld(soup):
    for tag in soup.find_all("script", type="application/ld+json"):
        try:
            data = json.loads(tag.string or "")
        except (json.JSONDecodeError, TypeError):
            continue
        items = data if isinstance(data, list) else [data]
        for item in items:
            if isinstance(item, dict):
                yield from item.get("@graph", []) if "@graph" in item \
                    else [item]


def _listing_from_jsonld(item, url):
    types = item.get("@type", "")
    if isinstance(types, list):
        types = " ".join(types)
    if not any(t in types for t in ("Product", "Vehicle", "Motorcycle", "Car")):
        return None
    listing = {"url": url, "raw": item}
    listing["title"] = item.get("name")
    listing["description"] = item.get("description")
    brand = item.get("brand")
    if isinstance(brand, dict):
        brand = brand.get("name")
    listing["brand"] = brand
    listing["model"] = item.get("model") or None
    year = item.get("vehicleModelDate") or item.get("productionDate") \
        or item.get("dateVehicleFirstRegistered")
    if year:
        m = re.search(r"(19|20)\d\d", str(year))
        listing["year"] = int(m.group(0)) if m else None
    mileage = item.get("mileageFromOdometer")
    if isinstance(mileage, dict):
        mileage = mileage.get("value")
    if mileage is not None:
        n = _to_number(str(mileage))
        listing["mileage_km"] = int(n) if n is not None else None
    offers = item.get("offers") or {}
    if isinstance(offers, list):
        offers = offers[0] if offers else {}
    price = offers.get("price")
    if price is not None:
        listing["price_eur"] = _to_number(str(price))
    image = item.get("image")
    if isinstance(image, list):
        image = image[0] if image else None
    if isinstance(image, dict):
        image = image.get("url")
    listing["image_url"] = image
    return listing


# --- Site-specific extraction ---

def _specs_from_rows(soup):
    """Collect 'Label:' -> value pairs from the spec div rows."""
    specs = {}
    for label_div in soup.find_all("div"):
        label = label_div.get_text(strip=True)
        if not label.endswith(":") or len(label) > 20:
            continue
        value_div = label_div.find_next_sibling("div")
        if value_div is None:
            continue
        value = value_div.get_text(" ", strip=True)
        if value:
            specs[label.rstrip(":").lower()] = value
    return specs


def _listing_from_site(soup, url):
    listing = {"url": url, "raw": {}}

    h1 = soup.find("h1")
    if h1:
        spans = h1.find_all("span")
        if len(spans) >= 2:
            brand = spans[0].get_text(" ", strip=True)
            rest = spans[1].get_text(" ", strip=True)
            listing["brand"] = brand.title() if brand.isupper() else brand
            m = re.match(r"(.*?)\s*-\s*((?:19|20)\d\d)\s*$", rest)
            if m:
                model, year = m.group(1), int(m.group(2))
            else:
                model, year = rest, None
            listing["model"] = model.title() if model.isupper() else model
            listing["year"] = year
            listing["title"] = f"{listing['brand']} {listing['model']}".strip()
        else:
            listing["title"] = h1.get_text(" ", strip=True)

    og_title = soup.find("meta", property="og:title")
    if not listing.get("title") and og_title:
        listing["title"] = og_title.get("content")
    og_image = soup.find("meta", property="og:image")
    if og_image and og_image.get("content"):
        listing["image_url"] = urljoin(url, og_image["content"])
    og_desc = soup.find("meta", property="og:description")
    if og_desc:
        listing["description"] = og_desc.get("content")

    price_el = soup.find(class_="full-tile-price")
    if price_el:
        m = re.search(r"\d[\d.,]*(?:,-)?", price_el.get_text(strip=True))
        listing["price_eur"] = _to_number(m.group(0)) if m else None

    specs = _specs_from_rows(soup)
    listing["raw"]["specs"] = specs
    if listing.get("year") is None and "bouwjaar" in specs:
        m = re.search(r"(19|20)\d\d", specs["bouwjaar"])
        listing["year"] = int(m.group(0)) if m else None
    if "teller" in specs:
        n = _to_number(re.sub(r"(?i)\s*km\.?$", "", specs["teller"]))
        listing["mileage_km"] = int(n) if n is not None else None

    dealer_link = soup.find("a", href=re.compile(r"/adressen/dealers/.*-d\d+"))
    dealer_cell = soup.find("td", onclick=re.compile("openMaps"))
    if dealer_cell:
        listing["seller"] = dealer_cell.get_text(strip=True)
        addr_row = dealer_cell.find_parent("tr")
        addr_next = addr_row.find_next_sibling("tr") if addr_row else None
        if addr_next:
            lines = [ln.strip() for ln
                     in addr_next.get_text("\n", strip=True).split("\n")
                     if ln.strip() and "route" not in ln.lower()]
            if lines:
                listing["location"] = lines[-1]
    elif dealer_link:
        listing["seller"] = dealer_link.get_text(strip=True) or None

    if listing.get("price_eur") is None and listing.get("year") is None \
            and listing.get("mileage_km") is None:
        return None
    return listing


# --- Generic fallback ---

def _listing_from_html(soup, url):
    listing = {"url": url, "raw": {}}
    og_title = soup.find("meta", property="og:title")
    h1 = soup.find("h1")
    listing["title"] = (og_title.get("content") if og_title else None) \
        or (h1.get_text(" ", strip=True) if h1 else None) \
        or (soup.title.get_text(strip=True) if soup.title else None)
    og_image = soup.find("meta", property="og:image")
    if og_image:
        listing["image_url"] = urljoin(url, og_image.get("content", ""))
    og_desc = soup.find("meta", property="og:description")
    if og_desc:
        listing["description"] = og_desc.get("content")

    text = soup.get_text(" ", strip=True)
    m = PRICE_RE.search(text)
    if m:
        listing["price_eur"] = _to_number(m.group(1) or m.group(2))
    m = YEAR_RE.search(text)
    if m:
        listing["year"] = int(m.group(1))
    m = MILEAGE_RE.search(text)
    if m:
        n = _to_number(m.group(1) or m.group(2))
        listing["mileage_km"] = int(n) if n is not None else None
    if not listing.get("title") or not any(
            listing.get(k) is not None
            for k in ("price_eur", "year", "mileage_km")):
        return None
    return listing


def parse_listing_page(html, url):
    """Extract one listing dict from a detail page, or None."""
    soup = BeautifulSoup(html, "html.parser")
    for item in _iter_jsonld(soup):
        listing = _listing_from_jsonld(item, url)
        if listing:
            return listing
    return _listing_from_site(soup, url) or _listing_from_html(soup, url)


def extract_listing_links(html, base_url):
    """Find listing-detail URLs (/motoren/...-m<id>.html) on any page."""
    soup = BeautifulSoup(html, "html.parser")
    links, seen = [], set()
    for a in soup.find_all("a", href=True):
        href = urljoin(base_url, a["href"]).split("#", 1)[0]
        if href not in seen and LISTING_HREF_RE.search(href):
            seen.add(href)
            links.append(href)
    return links


def extract_index_links(html, base_url):
    """Find brand/search index pages (/motoren/zoeken/<brand>-b<id>.html)."""
    soup = BeautifulSoup(html, "html.parser")
    links = set()
    for a in soup.find_all("a", href=True):
        href = urljoin(base_url, a["href"]).split("#", 1)[0]
        if re.search(r"/motoren/zoeken/[^/?#]+-b\d+\.html", href):
            links.add(href)
    return sorted(links)


def extract_pagination_requests(html):
    """Yield param dicts for the /mz.php AJAX pagination endpoint.

    Buttons carry ``onclick="getPagination({ order: 'default', ...,
    s: '20', c: '20' }, '/mz.php', '#motors-wrapper')"``; ``s`` is the
    result offset. Returns unique param dicts, offset ascending, skipping
    offset 0 (the page itself).
    """
    requests_seen = {}
    for m in PAGINATION_ONCLICK_RE.finditer(html):
        params = dict(re.findall(r"(\w+)\s*:\s*'([^']*)'", m.group(1)))
        try:
            offset = int(params.get("s", "0"))
        except ValueError:
            continue
        if offset > 0:
            requests_seen[offset] = params
    return [requests_seen[k] for k in sorted(requests_seen)]


def parse_sitemap(xml_text, base_url):
    """Return (sitemap_urls, page_urls) from a sitemap or sitemap index."""
    sitemaps, pages = [], []
    try:
        root = ElementTree.fromstring(xml_text.strip())
    except ElementTree.ParseError:
        root = None
    if root is not None:
        for entry in root:
            kind = entry.tag.rsplit("}", 1)[-1]
            for child in entry:
                if child.tag.rsplit("}", 1)[-1] == "loc" and child.text:
                    target = urljoin(base_url, child.text.strip())
                    (sitemaps if kind == "sitemap" else pages).append(target)
    if not sitemaps and not pages:
        # Some sitemaps are plain text, one URL per line.
        for line in xml_text.splitlines():
            line = line.strip()
            if line.startswith(("http://", "https://")):
                pages.append(line)
    return sitemaps, pages


def is_cloudflare_challenge(status_code, headers, body):
    if headers.get("cf-mitigated") == "challenge":
        return True
    return status_code in (403, 503) and "Just a moment" in (body or "")
