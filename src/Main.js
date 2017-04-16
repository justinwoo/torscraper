var request = require('request');

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
