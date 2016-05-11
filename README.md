torscraper
==========

simple stupid scraper for torrents written in purescript and node

## Usage

Requires a `config.json` with this schema:

```json
{
  "url": "urlForSearchResults",
  "selector": ".whateverClassName a",
  "blacklist": [
    "Substring of title you don't want to download"
  ]
}
```
