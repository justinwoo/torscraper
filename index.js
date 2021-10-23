const cp = require("child_process");
const path = require("path");
const fs = require("fs");

const cheerio = require("cheerio");

const config = require("./config.json");

// get links from html string
// Array { href: "/view/...", title: "[...] ... [720p].mkv"
function getLinks(htmlString) {
  const $ = cheerio.load(htmlString);
  const anchors = $(".torrent-list tbody tr.success a");
  const filtered = [];

  anchors.map((_, ele) => {
    const attribs = ele.attribs;
    if (
      !!attribs.href &&
      attribs.href.indexOf("/view") == 0 &&
      !!attribs.title &&
      attribs.title.indexOf("720p") !== -1
    ) {
      filtered.push({
        href: attribs.href,
        title: attribs.title,
      });
    }
  });

  return filtered;
}

function test_scrape() {
  const htmlString = fs.readFileSync("scratch.html").toString();
  const links = getLinks(htmlString);
  links.map((x) => console.log(x));
}

function fetch(url) {
  const result = cp.spawnSync("curl", ["-L", url]);
  if (result.error) {
    console.log(`failed to fetch: ${url}`);
    console.log(result.error);
    process.exit(1);
  } else {
    return result.stdout.toString();
  }
}

function test_fetch() {
  const url = "https://justinwoo.github.io/hash/index.html";
  const htmlString = fetch(url);
  console.log("htmlString:", htmlString);
}

function download(baseUrl, link) {
  const url = path.join(
    baseUrl,
    link.href.replace("view", "download"),
    "/torrent"
  );
  const filepath = path.join("./downloads", `${link.title}.torrent`);

  const result = cp.spawnSync("curl", ["-L", url, "-o", filepath]);
  if (result.error) {
    console.log(`failed to download ${url}:`);
    console.log(result.error);
    process.exit(1);
  } else {
    console.log(`downloaded ${filepath}`);
  }
}

function mkTestExists() {
  const existing = fs.readdirSync("./downloads");
  function exists(link) {
    return (
      existing.indexOf(`${link.title}.torrent`) !== -1 ||
      existing.indexOf(`${link.title}.torrent.added`) !== -1
    );
  }

  return exists;
}

function main() {
  const urls = config.urls;
  const htmls = [];
  urls.map((url) => {
    htmls.push(fetch(url));
  });

  let links = [];
  htmls.map((html) => {
    links = links.concat(getLinks(html));
  });

  let testExists = mkTestExists();
  let filtered = [];
  links.map((link) => {
    let exists = testExists(link);

    let blacklisted = false;
    for (let i = 0; i < config.blacklist.length; i++) {
      let word = config.blacklist[i];
      if (link.title.indexOf(word) !== -1) {
        blacklisted = true;
        break;
      }
    }

    let hasNumber = /- \d+/.test(link.title);
    if (!exists && !blacklisted && hasNumber) {
      filtered.push(link);
    }
  });

  if (filtered.length == 0) {
    console.log("nothing new to download");
  } else {
    filtered.map((link) => {
      download(config.baseUrl, link);
    });
  }
}

// test_scrape();
// test_fetch();

main();
