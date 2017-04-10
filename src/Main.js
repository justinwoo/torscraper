var request = require('request');
var cheerio = require('cheerio');
var he = require('he');

var DOWNLOAD_DIR = './downloads';

exports.decodeHtml = function (string) {
  return he.decode(string);
};

exports.ajaxGet = function (url, errorCallback, callback) {
  return function () {
    request(url, function (error, response) {
      if (error) {
        return errorCallback(error)();
      } else {
        return callback(response.body)();
      }
    });
  };
};
