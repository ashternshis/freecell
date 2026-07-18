# motoroccasion.nl → SQLite scraper

Crawls motorcycle listings from www.motoroccasion.nl into a local SQLite
database.

## Setup

```sh
pip install -r requirements.txt
```

## Usage

```sh
cd scraper
python -m motoroccasion.scrape --db motoroccasion.sqlite3
```

Options:

| Flag | Default | Meaning |
|---|---|---|
| `--db` | `motoroccasion.sqlite3` | SQLite file to create/update |
| `--brand yamaha` | all brands | Limit the brand-page crawl (repeatable) |
| `--no-sitemap` | off | Skip sitemap discovery, crawl brand pages |
| `--delay` | `2.0` | Seconds between requests (be polite) |
| `--max-pages` | `500` | Hard cap on HTTP fetches per run |
| `--max-listings` | unlimited | Stop after processing N listings |
| `--base-url` | `https://www.motoroccasion.nl` | Site root |

Re-runs are incremental: listings are upserted by URL, `first_seen_at`
is preserved and `last_seen_at` refreshed, so the database tracks a
listing's lifetime across runs.

## Important: run this from a residential connection

The site sits behind Cloudflare with a managed challenge that is
IP-reputation based. From datacenter/cloud IPs (including the sandbox
this was developed in) every request gets a "Just a moment..." challenge
page; from an ordinary home connection the challenge is not expected to
trigger for polite request rates. The scraper detects the challenge,
records the run as `blocked` in `scrape_runs` with an explanatory note,
and exits non-zero instead of hammering the site.

## How it discovers listings

1. **Sitemap** — robots.txt advertises `/sitemap.xml`; it is fetched
   (recursing into sitemap indexes) and filtered to listing URLs, which
   look like `/motoren/<slug>-m<id>.html`.
2. **Brand pages** (fallback, or `--no-sitemap`) — `/motoren` links to
   ~70 brand pages (`/motoren/zoeken/<brand>-b<id>.html`), each showing
   20 bikes. Further pages load via the site's `/mz.php` AJAX endpoint;
   the scraper replays those calls best-effort with the same parameters
   the site's own `getPagination()` sends.

## How it parses a listing

Verified against the site's real markup (via Wayback Machine captures
through September 2025):

- `<h1><span>BRAND</span> <span>MODEL - YEAR</span></h1>`
- price in `span.full-tile-price` (e.g. `€ 6.996,-`, Dutch number format)
- spec label/value rows: `Bouwjaar:`, `Kleur:`, `Teller:` (odometer),
  `Vermogen:`, `Rijbewijs:`, `Garantie:` — all captured into `raw_json`,
  with year/mileage promoted to columns
- dealer name and city from the dealer address block

A schema.org JSON-LD path runs first (future-proof; the site doesn't
currently emit it) and generic Dutch-label heuristics remain as a last
resort, so moderate redesigns degrade gracefully instead of breaking.

## Database schema

- **`listings`** — one row per advert: `url`, `site_id` (the `m<id>`
  number), `title`, `brand`, `model`, `year`, `mileage_km`, `price_eur`,
  `location`, `seller`, `description`, `image_url`, `raw_json` (full
  spec table), `first_seen_at`, `last_seen_at`.
- **`scrape_runs`** — one row per crawl: timestamps, pages fetched,
  listings found/new, status (`ok` / `blocked` / `error` /
  `interrupted`) and a note.

Example query:

```sql
SELECT brand, COUNT(*) n, ROUND(AVG(price_eur)) avg_price
FROM listings GROUP BY brand ORDER BY n DESC;
```

## Behaviour

- Honours `robots.txt` and rate-limits itself (default 2 s between
  requests) with an identifying User-Agent.
- Detects the Cloudflare challenge and stops immediately rather than
  retrying.
- Never disables TLS verification.

## Tests

```sh
cd scraper
python -m pytest tests/
```

Fixtures mirror the site's real detail-page, brand-page, and sitemap
markup. For a full end-to-end check, serve a fake site locally (see the
fixture files) and point `--base-url` at it.

## Note on the domain

The original request mentioned motoroccasions.nl (plural). That domain
has been a parked cPanel page since ~2017 and currently serves a
self-signed certificate with HTTP 503. This scraper targets the live
classifieds site www.motoroccasion.nl (singular).
