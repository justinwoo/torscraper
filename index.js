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
  const joined = path.join(baseUrl, link.href.replace("view", "download"));
  const url = `${joined}.torrent`;
  const filepath = path.join("./downloads", `${link.title}.torrent`);

  const result = cp.spawnSync("curl", [
    "-L",
    url,
    "-o",
    filepath,
    "--fail",
    "--silent",
  ]);

  if (result.error) {
    console.log(`failed to download ${url}: ${result.error}`);
    if (fs.existsSync(filepath)) {
      fs.unlinkSync(filepath);
    }
    return false;
  }

  if (result.status !== 0) {
    console.log(
      `failed to download ${url}: HTTP error (status ${result.status})`
    );
    if (fs.existsSync(filepath)) {
      fs.unlinkSync(filepath);
    }
    return false;
  }

  if (!fs.existsSync(filepath) || fs.statSync(filepath).size === 0) {
    console.log(`downloaded file is empty or missing: ${filepath}`);
    if (fs.existsSync(filepath)) {
      fs.unlinkSync(filepath);
    }
    return false;
  }

  console.log(`downloaded ${filepath} (${fs.statSync(filepath).size} bytes)`);
  return true;
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

function cleanupZeroByte() {
  const existing = fs.readdirSync("./downloads");
  let cleaned = 0;

  existing.forEach((filename) => {
    const filepath = path.join("./downloads", filename);
    if (filename.endsWith(".torrent") && !filename.endsWith(".torrent.added")) {
      const stats = fs.statSync(filepath);
      if (stats.size === 0) {
        console.log(`removing 0-byte file: ${filename}`);
        fs.unlinkSync(filepath);
        cleaned++;
      }
    }
  });

  if (cleaned > 0) {
    console.log(`cleaned up ${cleaned} empty torrent files`);
  }
}

function main() {
  cleanupZeroByte();

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
    let successCount = 0;
    filtered.forEach((link) => {
      if (download(config.baseUrl, link)) {
        successCount++;
      }
    });
    console.log(
      `successfully downloaded ${successCount}/${filtered.length} torrents`
    );
  }
}

// test_scrape();
// test_fetch();

main();
