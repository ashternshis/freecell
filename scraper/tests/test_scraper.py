import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from motoroccasion import db as dbmod
from motoroccasion.parse import (extract_index_links, extract_listing_links,
                                 extract_pagination_requests,
                                 is_cloudflare_challenge, listing_id_from_url,
                                 parse_listing_page, parse_sitemap)

FIXTURES = Path(__file__).parent / "fixtures"


def read(name):
    return (FIXTURES / name).read_text(encoding="utf-8")


DETAIL_URL = ("https://www.motoroccasion.nl/motoren/"
              "ace-motorcycles-hillclimber-m1585320.html")


def test_parse_real_detail_page():
    listing = parse_listing_page(read("detail_real.html"), DETAIL_URL)
    assert listing["brand"] == "Ace Motorcycles"
    assert listing["model"] == "Hillclimber"
    assert listing["title"] == "Ace Motorcycles Hillclimber"
    assert listing["year"] == 2015
    assert listing["price_eur"] == 6996.0
    assert listing["mileage_km"] == 470
    assert listing["seller"] == "Motorhuis Seppe"
    assert listing["location"] == "Bosschenhoofd"
    assert listing["image_url"] == "https://cdn.qonnex.com/abc123.jpg"
    assert listing["raw"]["specs"]["kleur"] == "Blauw"
    assert listing["raw"]["specs"]["rijbewijs"] == "A"


def test_parse_jsonld_detail_page():
    listing = parse_listing_page(
        read("detail_jsonld.html"),
        "https://www.motoroccasion.nl/motoren/yamaha-mt-07-m1234567.html")
    assert listing["title"] == "Yamaha MT-07 ABS"
    assert listing["brand"] == "Yamaha"
    assert listing["year"] == 2021
    assert listing["mileage_km"] == 8450
    assert listing["price_eur"] == 6499.0


def test_parse_non_listing_page_returns_none():
    assert parse_listing_page("<html><body><h1>Over ons</h1></body></html>",
                              "https://www.motoroccasion.nl/x") is None


def test_listing_id_from_url():
    assert listing_id_from_url(DETAIL_URL) == "1585320"
    assert listing_id_from_url("https://www.motoroccasion.nl/motoren") is None


def test_extract_listing_and_index_links():
    html = read("brand_page.html")
    base = "https://www.motoroccasion.nl/motoren/zoeken/bmw-b4.html"
    links = extract_listing_links(html, base)
    assert "https://www.motoroccasion.nl/motoren/bmw-r-51-3-m1516102.html" \
        in links
    assert "https://www.motoroccasion.nl/motoren/bmw-r-26-m1521337.html" \
        in links
    # #beta anchor variant must not create a duplicate
    assert len([l for l in links if "m1521337" in l]) == 1
    assert len(links) == 3
    index = extract_index_links(html, base)
    assert index == \
        ["https://www.motoroccasion.nl/motoren/zoeken/yamaha-b40.html"]


def test_extract_pagination_requests():
    reqs = extract_pagination_requests(read("brand_page.html"))
    offsets = [int(r["s"]) for r in reqs]
    assert offsets == [20, 40]
    assert reqs[0]["c"] == "20"
    assert reqs[0]["order"] == "default"


def test_parse_sitemap_index_and_urlset():
    sitemaps, pages = parse_sitemap(read("sitemap_index.xml"),
                                    "https://www.motoroccasion.nl/sitemap.xml")
    assert sitemaps == [
        "https://www.motoroccasion.nl/sitemap-motoren.xml",
        "https://www.motoroccasion.nl/sitemap-dealers.xml",
    ]
    assert pages == []
    sitemaps, pages = parse_sitemap(read("sitemap_listings.xml"),
                                    "https://www.motoroccasion.nl/sitemap.xml")
    assert sitemaps == []
    assert len(pages) == 3
    assert pages[0].endswith("-m1585320.html")


def test_cloudflare_challenge_detection():
    assert is_cloudflare_challenge(403, {"cf-mitigated": "challenge"}, "")
    assert is_cloudflare_challenge(403, {}, "<title>Just a moment...</title>")
    assert not is_cloudflare_challenge(200, {}, "<html>ok</html>")
    assert not is_cloudflare_challenge(404, {}, "not found")


def test_db_upsert_roundtrip(tmp_path):
    conn = dbmod.connect(str(tmp_path / "test.sqlite3"))
    listing = {
        "url": DETAIL_URL, "site_id": "1585320",
        "title": "Ace Motorcycles Hillclimber", "brand": "Ace Motorcycles",
        "model": "Hillclimber", "year": 2015, "mileage_km": 470,
        "price_eur": 6996.0, "raw": {"specs": {"kleur": "Blauw"}},
    }
    assert dbmod.upsert_listing(conn, listing) is True
    listing["price_eur"] = 6500.0
    assert dbmod.upsert_listing(conn, listing) is False
    row = conn.execute("SELECT * FROM listings").fetchone()
    assert row["price_eur"] == 6500.0
    assert row["site_id"] == "1585320"
    assert conn.execute("SELECT COUNT(*) FROM listings").fetchone()[0] == 1


def test_run_bookkeeping(tmp_path):
    conn = dbmod.connect(str(tmp_path / "test.sqlite3"))
    run_id = dbmod.start_run(conn)
    dbmod.finish_run(conn, run_id, 5, 3, 2, "blocked", "challenge")
    row = conn.execute("SELECT * FROM scrape_runs WHERE id = ?",
                       (run_id,)).fetchone()
    assert row["status"] == "blocked"
    assert row["pages_fetched"] == 5
    assert row["listings_new"] == 2
    assert row["finished_at"] is not None
