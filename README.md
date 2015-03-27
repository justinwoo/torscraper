torscraper
==========

the simplest torrent scraper, doesn't handle anything special

WARNING: please don't actually use this. if you really need reasons not to, read the source code.

HELP WANTED: i want this to get rewritten in a better language/platform and to use a whitelist to just download magnet links to a directory using better match/filtering. anyone????


## Usage

Requires a `config.json` with this schema:

```json
{
  "url": "urlForSearchResults",
  "selector": ".whateverClassName a",
  "blacklist": [
    "Substring of title you don't want to open right away"
  ]
}
```
