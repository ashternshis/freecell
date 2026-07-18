"""SQLite storage layer for scraped motorcycle listings."""

import json
import sqlite3
from datetime import datetime, timezone

SCHEMA = """
CREATE TABLE IF NOT EXISTS listings (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    url TEXT NOT NULL UNIQUE,
    title TEXT,
    brand TEXT,
    model TEXT,
    year INTEGER,
    mileage_km INTEGER,
    price_eur REAL,
    location TEXT,
    seller TEXT,
    description TEXT,
    image_url TEXT,
    raw_json TEXT,
    first_seen_at TEXT NOT NULL,
    last_seen_at TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS scrape_runs (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    started_at TEXT NOT NULL,
    finished_at TEXT,
    pages_fetched INTEGER NOT NULL DEFAULT 0,
    listings_found INTEGER NOT NULL DEFAULT 0,
    listings_new INTEGER NOT NULL DEFAULT 0,
    status TEXT NOT NULL DEFAULT 'running',
    note TEXT
);

CREATE INDEX IF NOT EXISTS idx_listings_brand ON listings(brand);
CREATE INDEX IF NOT EXISTS idx_listings_price ON listings(price_eur);
"""


def utcnow() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="seconds")


def connect(path: str) -> sqlite3.Connection:
    conn = sqlite3.connect(path)
    conn.row_factory = sqlite3.Row
    conn.executescript(SCHEMA)
    return conn


def start_run(conn: sqlite3.Connection) -> int:
    cur = conn.execute(
        "INSERT INTO scrape_runs (started_at) VALUES (?)", (utcnow(),)
    )
    conn.commit()
    return cur.lastrowid


def finish_run(conn, run_id, pages_fetched, listings_found, listings_new,
               status="ok", note=None):
    conn.execute(
        """UPDATE scrape_runs
           SET finished_at = ?, pages_fetched = ?, listings_found = ?,
               listings_new = ?, status = ?, note = ?
           WHERE id = ?""",
        (utcnow(), pages_fetched, listings_found, listings_new, status, note,
         run_id),
    )
    conn.commit()


def upsert_listing(conn: sqlite3.Connection, listing: dict) -> bool:
    """Insert or refresh a listing keyed by URL. Returns True if new."""
    now = utcnow()
    raw = json.dumps(listing.get("raw", {}), ensure_ascii=False)
    exists = conn.execute(
        "SELECT 1 FROM listings WHERE url = ?", (listing["url"],)
    ).fetchone()
    conn.execute(
        """INSERT INTO listings
               (url, title, brand, model, year, mileage_km, price_eur,
                location, seller, description, image_url, raw_json,
                first_seen_at, last_seen_at)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
           ON CONFLICT(url) DO UPDATE SET
               title = excluded.title,
               brand = COALESCE(excluded.brand, listings.brand),
               model = COALESCE(excluded.model, listings.model),
               year = COALESCE(excluded.year, listings.year),
               mileage_km = COALESCE(excluded.mileage_km, listings.mileage_km),
               price_eur = COALESCE(excluded.price_eur, listings.price_eur),
               location = COALESCE(excluded.location, listings.location),
               seller = COALESCE(excluded.seller, listings.seller),
               description = COALESCE(excluded.description,
                                      listings.description),
               image_url = COALESCE(excluded.image_url, listings.image_url),
               raw_json = excluded.raw_json,
               last_seen_at = excluded.last_seen_at""",
        (listing["url"], listing.get("title"), listing.get("brand"),
         listing.get("model"), listing.get("year"), listing.get("mileage_km"),
         listing.get("price_eur"), listing.get("location"),
         listing.get("seller"), listing.get("description"),
         listing.get("image_url"), raw, now, now),
    )
    conn.commit()
    return exists is None
