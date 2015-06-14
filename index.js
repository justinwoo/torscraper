var Rx = require('rx');
var fs = require('fs');
var spawn = require('child_process').spawn;
var chalk = require('chalk');

var config = require('./config');

var downloadDir = __dirname + '/downloads/'

function matchName(base, target) {
  return base.indexOf(target) !== -1;
}

function isBlacklisted(descriptor) {
  for (var i = 0; i < config.blacklist.length; i++) {
    var blacklisted = config.blacklist[i];
    if (matchName(descriptor, blacklisted)) {
      return true;
    }
  }
  return false;
}

function isDownloaded(fileDetails, filename) {
  for (var i = 0; i < fileDetails.length; i++) {
    var downloadedFilename = fileDetails[i].filename;
    if (matchName(downloadedFilename, filename)) {
      return true;
    }
  }
  return false;
}

var downloadedFilesStream = new Rx.Subject();
var fileDetailsStream = new Rx.Subject();
var fileStatsStream = new Rx.Subject();

console.log(chalk.green('reading previously downloaded files'));
fs.readdir(downloadDir, function (err, files) {
  downloadedFilesStream.onNext(files);
});

var downloadedFilesCount;

downloadedFilesStream.subscribe(function (files) {
  console.log(chalk.green('getting downloaded files\' details'));
  downloadedFilesCount = files.length;
  files.map(function (file) {
    fs.stat(downloadDir + file, function (err, stat) {
      fileStatsStream.onNext({
        filename: file,
        time: stat.mtime.getTime()
      });
    });
  });
});

var fileDetails = [];
fileStatsStream.subscribe(function (item) {
  fileDetails.push(item);

  if (fileDetails.length === downloadedFilesCount) {
    fileDetailsStream.onNext(fileDetails);
  }
});

console.log(chalk.green('fetching URLs'));
var jobsStream = new Rx.Subject();
var output = '';
var path = __dirname + '/phantom/getUrls.js';
var getUrls = spawn('phantomjs', [path]);
getUrls.stdout.on('data', function (data) {
  output += data;
});
getUrls.on('close', function () {
  console.log(chalk.green('fetching finished'));
  console.log(chalk.green('beginning output processing'));
  var lines = output.split('\n');
  lines.reverse().forEach(function (a) {
    if (a.length > 1) {
      var job = JSON.parse(a);
      jobsStream.onNext(job);
    }
  });
});

var downloadJobStream = new Rx.Subject();

Rx.Observable.combineLatest(
  fileDetailsStream,
  jobsStream,
  function (fileDetails, job) {
    return {
      fileDetails: fileDetails,
      job: job
    };
  }
).subscribe(function (latest) {
  var job = latest.job;
  var fileDetails = latest.fileDetails;
  if (!isDownloaded(fileDetails, job.descriptor)) {
    if (!isBlacklisted(job.descriptor)) {
      downloadJobStream.onNext(job);
    }
  }
});

downloadJobStream.subscribe(function (job) {
  console.log(chalk.yellow('downloading', job.descriptor));
  var newLink = job.link.replace('view', 'download');
  var filepath = downloadDir + job.descriptor + '.torrent';
  var curl = spawn('curl', [newLink, '-o', filepath]);
  curl.on('close', function () {
    if (!isBlacklisted(job.descriptor)) {
      console.log(chalk.green('downloaded', filepath));
    } else {
      console.log(chalk.red('blacklisted file', filepath, 'will not be opened.'));
    }
  });
});
