//module Main

var fs = require('fs');
var request = require('request');
var cheerio = require('cheerio');
var spawn = require('child_process').spawn;
var path = require('path');
var chalk = require('chalk');

var CONFIG_PATH = './config.json';
var DOWNLOAD_DIR = './downloads';

exports.configPath = CONFIG_PATH;

exports.downloadDir = DOWNLOAD_DIR;

exports.parseConfigFile = function (string) {
  var config = JSON.parse(string);

  if (!config.url) throw "config is missing the url";
  if (!config.selector) throw "config is missing the selector";
  if (!config.blacklist) throw "config is missing the blacklist";

  return config;
}

exports.scrapeHtml = function (selector) {
  return function (html) {
    var $ = cheerio.load(html);
    var targets = [];

    $(selector).each(function (i, e) {
      var $e = $(e);
      var target = {
        name: $e.text(),
        url: $e.attr('href')
      };
      targets.push(target);
    });

    return targets.reverse();
  }
}

exports.getTargetsPage = function (callback) {
  return function (url) {
    return function () {
      request(url, function () {
        var err = arguments[0];
        var body = arguments[2];
        if (err) {
          throw err;
        } else {
          return callback(body)();
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
