//module Main

var fs = require('fs');
var request = require('request');
var cheerio = require('cheerio');
var spawn = require('child_process').spawn;
var path = require('path');
var chalk = require('chalk');

var CONFIG_PATH = './config.json';
var DOWNLOAD_DIR = './downloads';

exports.getConfig = function (callback) { // accepts a callback
  return function () { // returns an effect
    fs.readFile(CONFIG_PATH, 'utf8', function (err, data) {
      callback(JSON.parse(data))(); // callback itself returns an effect
    });
  }
}

exports.getDownloadedFiles = function (callback) {
  return function () {
    fs.readdir(DOWNLOAD_DIR, function (err, data) {
      callback(data)();
    });
  }
}

exports.getFetchedTargets = function (callback) {
  return function (config) {
    function scrapeHtml(html) {
      var $ = cheerio.load(html);
      var targets = [];

      $(config.selector).each(function (i, e) {
        var $e = $(e);
        var target = {
          name: $e.text(),
          url: $e.attr('href')
        };
        targets.push(target);
      });

      return targets.reverse();
    }

    return function () {
      request(config.url, function () {
        var err = arguments[0];
        var body = arguments[2];
        if (err) {
          throw err;
        } else {
          return callback(scrapeHtml(body))();
        }
      });
    }
  }
}

exports.kickOffDownloads = function (targets) {
  return function () {
    if (targets.length === 0) {
      console.log(chalk.green('nothing new to download'));
      return;
    }
    targets.forEach(function (target) {
      console.log(chalk.yellow('downloading', target.name));

      var url = target.url.replace('view', 'download').replace(/^\/\//, '');
      var filepath = path.join(DOWNLOAD_DIR, target.name + '.torrent');
      var curl = spawn('curl', [url, '-o', filepath]);

      curl.on('close', function () {
        console.log(chalk.green('downloaded', filepath));
      });
    });
  }
}
