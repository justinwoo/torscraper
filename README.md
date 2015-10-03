torscraper
==========

crappy scraper written in JS and Elm using webpack targeting node

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
