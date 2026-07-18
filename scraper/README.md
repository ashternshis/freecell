# motoroccasions.nl → SQLite scraper

Crawls motorcycle listings from motoroccasions.nl into a local SQLite
database.

> **Site status (July 2026):** www.motoroccasions.nl is currently
> unreachable — the server presents a self-signed `CN=localhost`
> certificate and returns HTTP 503, and the Wayback Machine shows only a
> cPanel placeholder page since 2017. The scraper detects this, records
> the failed run in `scrape_runs`, and exits non-zero. The parser is
> therefore built defensively: it prefers schema.org JSON-LD (stable
> across redesigns), then falls back to Open Graph tags and Dutch-label
> heuristics (`bouwjaar`, `kilometerstand`, `vraagprijs`), so it should
> work with little or no adjustment if/when the site comes back online.
> If you meant a different site (e.g. www.motoroccasion.nl, singular,
> which is live but bot-protected), the crawler can be pointed at it via
> `--base-url` once access is sorted.

## Setup

```sh
pip install -r requirements.txt
```

## Usage

```sh
cd scraper
python -m motoroccasions.scrape --db motoroccasions.sqlite3
```

Options:

| Flag | Default | Meaning |
|---|---|---|
| `--db` | `motoroccasions.sqlite3` | SQLite file to create/update |
| `--base-url` | `https://www.motoroccasions.nl` | Site root |
| `--start-url` | site root | Index page(s) to crawl from; repeatable |
| `--delay` | `2.0` | Seconds between requests (be polite) |
| `--max-pages` | `200` | Hard cap on HTTP fetches per run |
| `--max-listings` | unlimited | Stop after storing N listings |

Re-runs are incremental: listings are upserted by URL, `first_seen_at`
is preserved and `last_seen_at` refreshed, so the database can track a
listing's lifetime across runs.

## Database schema

- **`listings`** — one row per advert URL: `title`, `brand`, `model`,
  `year`, `mileage_km`, `price_eur`, `location`, `seller`,
  `description`, `image_url`, plus `raw_json` with the full extracted
  structured data and `first_seen_at` / `last_seen_at` timestamps.
- **`scrape_runs`** — one row per crawl: timestamps, pages fetched,
  listings found/new, status (`ok` / `error` / `interrupted`) and a note.

Example query:

```sql
SELECT brand, COUNT(*) n, ROUND(AVG(price_eur)) avg_price
FROM listings GROUP BY brand ORDER BY n DESC;
```

## Behaviour

- Honours `robots.txt` and rate-limits itself (default 2 s between
  requests), with an identifying User-Agent.
- Discovers listing links and pagination from index pages, then parses
  each detail page.
- Never disables TLS verification.

## Tests

```sh
cd scraper
python -m pytest tests/
```

Tests cover JSON-LD parsing, heuristic HTML parsing, link/pagination
extraction, and database upsert/run bookkeeping against fixture pages.
There is also a live end-to-end path: serve `tests/fixtures/` over
`python -m http.server` and point `--base-url`/`--start-url` at it.
