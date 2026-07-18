import sqlite3
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from motoroccasions import db as dbmod
from motoroccasions.parse import (extract_listing_links,
                                  extract_pagination_links,
                                  parse_listing_page)

FIXTURES = Path(__file__).parent / "fixtures"


def read(name):
    return (FIXTURES / name).read_text(encoding="utf-8")


def test_parse_jsonld_detail_page():
    listing = parse_listing_page(
        read("detail_jsonld.html"),
        "https://www.motoroccasions.nl/occasions/yamaha-mt-07-2021")
    assert listing["title"] == "Yamaha MT-07 ABS"
    assert listing["brand"] == "Yamaha"
    assert listing["year"] == 2021
    assert listing["mileage_km"] == 8450
    assert listing["price_eur"] == 6499.0
    assert listing["image_url"].endswith("mt07.jpg")


def test_parse_plain_html_detail_page():
    listing = parse_listing_page(
        read("detail_plain.html"),
        "https://www.motoroccasions.nl/occasions/honda-cb500f-2018")
    assert listing["title"] == "Honda CB500F"
    assert listing["brand"] == "Honda"
    assert listing["price_eur"] == 4750.0
    assert listing["year"] == 2018
    assert listing["mileage_km"] == 21300


def test_parse_non_listing_page_returns_none():
    assert parse_listing_page("<html><body><p>hoi</p></body></html>",
                              "https://www.motoroccasions.nl/x") is None


def test_extract_links_and_pagination():
    html = read("index.html")
    base = "https://www.motoroccasions.nl/occasions/"
    links = extract_listing_links(html, base)
    assert "https://www.motoroccasions.nl/occasions/yamaha-mt-07-2021" in links
    assert "https://www.motoroccasions.nl/occasions/honda-cb500f-2018" in links
    assert all("motoroccasions.nl" in l for l in links)
    pages = extract_pagination_links(html, base)
    assert "https://www.motoroccasions.nl/occasions/?page=2" in pages


def test_db_upsert_roundtrip(tmp_path):
    conn = dbmod.connect(str(tmp_path / "test.sqlite3"))
    listing = {
        "url": "https://www.motoroccasions.nl/occasions/yamaha-mt-07-2021",
        "title": "Yamaha MT-07 ABS", "brand": "Yamaha", "model": "MT-07",
        "year": 2021, "mileage_km": 8450, "price_eur": 6499.0,
        "raw": {"@type": "Motorcycle"},
    }
    assert dbmod.upsert_listing(conn, listing) is True
    listing["price_eur"] = 6299.0
    assert dbmod.upsert_listing(conn, listing) is False
    row = conn.execute("SELECT * FROM listings").fetchone()
    assert row["price_eur"] == 6299.0
    assert row["brand"] == "Yamaha"
    assert conn.execute("SELECT COUNT(*) FROM listings").fetchone()[0] == 1


def test_run_bookkeeping(tmp_path):
    conn = dbmod.connect(str(tmp_path / "test.sqlite3"))
    run_id = dbmod.start_run(conn)
    dbmod.finish_run(conn, run_id, 5, 3, 2, "ok", None)
    row = conn.execute("SELECT * FROM scrape_runs WHERE id = ?",
                       (run_id,)).fetchone()
    assert row["status"] == "ok"
    assert row["pages_fetched"] == 5
    assert row["listings_new"] == 2
    assert row["finished_at"] is not None
