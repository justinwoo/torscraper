var system = require('system');
var page = require('webpage').create();
var config = require('../config');

function processLinks(links) {
  var jobs = [].concat(links);
  while (jobs.length > 0) {
    var job = jobs.pop();
    system.stdout.writeLine(JSON.stringify(job));
  }
  if (jobs.length === 0) {
    phantom.exit();
  }
}

page.open(config.url, function () {
  var links = page.evaluate(function (selector) {
    var nodes = document.querySelectorAll(selector);
    return Array.prototype.map.call(nodes, function (a) {
      return {
        descriptor: a.innerHTML,
        link: a.href
      }
    });
  }, config.selector);
  processLinks(links);
});
