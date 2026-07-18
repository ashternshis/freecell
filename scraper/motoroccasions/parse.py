"""Listing extraction from motoroccasions.nl pages.

Strategy, most reliable first:
1. schema.org JSON-LD blocks (Product / Vehicle / Motorcycle with offers) —
   common on classifieds sites and stable across redesigns.
2. Open Graph meta tags for title/image.
3. Heuristic text patterns for Dutch classifieds fields (bouwjaar,
   kilometerstand, prijs).
"""

import json
import re
from urllib.parse import urljoin

from bs4 import BeautifulSoup

KNOWN_BRANDS = [
    "Aprilia", "Benelli", "BMW", "CFMoto", "Ducati", "Harley-Davidson",
    "Harley Davidson", "Honda", "Husqvarna", "Indian", "Kawasaki", "KTM",
    "Kymco", "Moto Guzzi", "MV Agusta", "Piaggio", "Royal Enfield", "Suzuki",
    "SYM", "Triumph", "Vespa", "Yamaha", "Zero",
]

PRICE_RE = re.compile(
    r"(?:€|EUR)\s*([\d.,]+)|(?:prijs|vraagprijs)\D{0,10}([\d.,]+)",
    re.IGNORECASE,
)
YEAR_RE = re.compile(r"(?:bouwjaar|year)\D{0,10}((?:19|20)\d\d)",
                     re.IGNORECASE)
MILEAGE_RE = re.compile(
    r"(?:kilometerstand|km[- ]stand|tellerstand)\D{0,10}([\d.,]+)|"
    r"([\d.,]+)\s*km\b",
    re.IGNORECASE,
)


def _to_number(text):
    """Parse a Dutch-formatted number: 12.500 or 12.500,50 -> 12500(.5)."""
    if text is None:
        return None
    t = text.strip().replace(" ", "")
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


def _guess_brand(title):
    if not title:
        return None, None
    low = title.lower()
    for brand in KNOWN_BRANDS:
        if low.startswith(brand.lower()) or f" {brand.lower()}" in f" {low}":
            rest = re.sub(re.escape(brand), "", title, count=1,
                          flags=re.IGNORECASE).strip(" -–|,")
            model = rest.split(",")[0].strip() or None
            return brand.replace("Harley Davidson", "Harley-Davidson"), model
    return None, None


def _listing_from_html(soup, url):
    listing = {"url": url, "raw": {}}
    og_title = soup.find("meta", property="og:title")
    h1 = soup.find("h1")
    listing["title"] = (og_title.get("content") if og_title else None) \
        or (h1.get_text(strip=True) if h1 else None) \
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
    return listing


def parse_listing_page(html, url):
    """Extract one listing dict from a detail page, or None."""
    soup = BeautifulSoup(html, "html.parser")
    for item in _iter_jsonld(soup):
        listing = _listing_from_jsonld(item, url)
        if listing:
            if not listing.get("brand"):
                listing["brand"], guessed_model = _guess_brand(
                    listing.get("title"))
                listing.setdefault("model", None)
                if not listing["model"]:
                    listing["model"] = guessed_model
            return listing
    listing = _listing_from_html(soup, url)
    # Without structured data, only accept pages that show at least one
    # vehicle attribute — a bare title is likely an index or info page.
    if not listing.get("title") or not any(
            listing.get(k) is not None
            for k in ("price_eur", "year", "mileage_km")):
        return None
    brand, model = _guess_brand(listing["title"])
    listing["brand"], listing["model"] = brand, model
    return listing


LISTING_HREF_RE = re.compile(
    r"/(occasion|motor|advertentie|aanbod|voorraad|detail)[s]?/", re.IGNORECASE
)
PAGINATION_HREF_RE = re.compile(r"[?&](page|p|pagina)=\d+", re.IGNORECASE)


def extract_listing_links(html, base_url):
    """Find candidate listing-detail URLs on an index/search page."""
    soup = BeautifulSoup(html, "html.parser")
    links = []
    seen = set()
    for a in soup.find_all("a", href=True):
        href = urljoin(base_url, a["href"])
        if "#" in href:
            href = href.split("#", 1)[0]
        if href in seen or not href.startswith(("http://", "https://")):
            continue
        if LISTING_HREF_RE.search(href) \
                and not PAGINATION_HREF_RE.search(href):
            seen.add(href)
            links.append(href)
    return links


def extract_pagination_links(html, base_url):
    """Find next/numbered pagination URLs on an index page."""
    soup = BeautifulSoup(html, "html.parser")
    links = set()
    for a in soup.find_all("a", href=True):
        href = urljoin(base_url, a["href"])
        label = a.get_text(strip=True).lower()
        rel = " ".join(a.get("rel", []))
        if rel == "next" or label in ("volgende", "next", "»", ">") \
                or re.search(r"[?&](page|p|pagina)=\d+", href):
            links.add(href.split("#", 1)[0])
    return sorted(links)
