var request = require('request');
var he = require('he');

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
