import fs from 'fs';
import {spawn} from 'child_process';
import path from 'path';
import Rx from 'rx';
import cheerio from 'cheerio';
import chalk from 'chalk';
import request from 'request';

import Elm from './Main.elm';

var DOWNLOAD_DIR = './downloads';

var readFile = Rx.Observable.fromNodeCallback(fs.readFile);

var config$ = readFile('./config.json', 'utf8').map(function (file) {
  return JSON.parse(file);
}).subscribe(main);

function main(config) {
  var readdir = Rx.Observable.fromNodeCallback(fs.readdir);
  var getPage = Rx.Observable.fromCallback(request, null, function () {
    var err = arguments[0];
    var body = arguments[2];
    if (err) {
      throw err;
    } else {
      return body;
    }
  });

  var downloadedFiles$ = readdir(DOWNLOAD_DIR);

  var fetchedTargets$ = getPage(config.url).map(function (html) {
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
  });

  Rx.Observable.combineLatest(
    downloadedFiles$,
    fetchedTargets$,
    function (
      downloadedFiles,
      fetchedTargets
    ) {
      var worker = Elm.worker(Elm.Main, {
        bannedWordsSignal: [],
        downloadedFilesSignal: [],
        fetchedTargetsSignal: []
      });

      worker.ports.bannedWordsSignal.send(config.blacklist);
      worker.ports.downloadedFilesSignal.send(downloadedFiles);
      worker.ports.fetchedTargetsSignal.send(fetchedTargets);

      return worker;
    }
  ).subscribe(function (worker) {
    worker.ports.requestDownloadSignal.subscribe(function (targets) {
      if (targets.length === 0) {
        console.log(chalk.green('nothing new to download'));
        return;
      }
      targets.forEach(function (target) {
        console.log(chalk.yellow('downloading', target.name));

        var url = target.url.replace('view', 'download');
        var filepath = path.join(DOWNLOAD_DIR, target.name + '.torrent');
        var curl = spawn('curl', [url, '-o', filepath]);

        curl.on('close', function () {
          console.log(chalk.green('downloaded', filepath));
        });
      });
    });
  });
}
