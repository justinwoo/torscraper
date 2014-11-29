var fs = require('fs');
var spawn = require('child_process').spawn;
var chalk = require('chalk');

var downloadedFiles;

function isDownloaded (filename) {
  for (var i = 0; i < downloadedFiles.length; i++) {
    var downloadedFile = downloadedFiles[i];
    if (downloadedFile.indexOf(filename) !== -1) {
      return true;
    }
  }
  return false;
}

function handleJob(job) {
  console.log(
    chalk.yellow('handling'),
    job.descriptor
  );
  if (!isDownloaded(job.descriptor)) {
    console.log(chalk.yellow('file not downloaded yet. getting now.'));
    var newLink = job.link.replace('view', 'download');
    var curl = spawn('curl', [newLink, '-o', './downloads/' + job.descriptor + '.torrent']);
  } else {
    console.log(chalk.green('file previously downloaded.'));
  }
}

function processOutput(output) {
  var newDownloads = [];
  console.log(chalk.green('beginning output processing'));
  var lines = output.split('\n');
  lines.forEach(function (a) {
    if (a.length > 1) {
      var job = JSON.parse(a);
      var jobResult = handleJob(job);
      if (jobResult) {
        newDownloads.push(jobResult);
      }
    }
  });
  console.log(chalk.green('processing finished'));
  if (newDownloads.length > 0) {
    console.log(chalk.green('New Files downloaded:'));
    newDownloads.foreach(function (a) {
      console.log(chalk.green(a));
    });
    console.log(chalk.green('See ./downloads'));
  }
}

downloadedFiles = fs.readdirSync('./downloads');
console.log(chalk.green('fetching URLs'));
var output = '';
var path = __dirname + '/phantom/getUrls.js';
var getUrls = spawn('phantomjs', [path]);
getUrls.stdout.on('data', function (data) {
  output += data;
});
getUrls.on('close', function () {
  console.log(chalk.green('fetching finished'));
  processOutput(output);
});
