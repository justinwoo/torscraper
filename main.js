var fs = require('fs');
var spawn = require('child_process').spawn;
var chalk = require('chalk');

var downloadDir = __dirname + '/downloads/'
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
    var curl = spawn('curl', [newLink, '-o', downloadDir + job.descriptor + '.torrent']);
    return job.descriptor;
  } else {
    console.log(chalk.green('file previously downloaded.'));
  }
}

function processOutput(output) {
  var newDownloads = [];
  console.log(chalk.green('beginning output processing'));
  var lines = output.split('\n');
  lines.reverse().forEach(function (a) {
    if (a.length > 1) {
      var job = JSON.parse(a);
      var jobResult = handleJob(job);
      if (jobResult) {
        newDownloads.push(jobResult);
      }
    }
  });
  console.log(chalk.green('processing finished'));
  console.log();
  if (newDownloads.length > 0) {
    console.log(chalk.yellow('New Files downloaded:'));
    newDownloads.forEach(function (a) {
      console.log(a + '.torrent');
    });
    console.log(chalk.green('See', downloadDir));
  } else {
    console.log(chalk.yellow('Nothing new.'));
  }
  console.log();
  console.log(chalk.yellow('Previous:'));
  var j = 10;
  for (var i = 1; i < j; i++) {
    var filename = downloadedFiles[downloadedFiles.length - i];
    switch (filename) {
      case '.DS_Store':
      case '.gitkeep':
        j++;
        break;
      default:
        console.log(filename);
    }
  }
}

downloadedFiles = fs.readdirSync(downloadDir);
downloadedFiles.sort(function(a, b) {
  return fs.statSync(downloadDir + a).mtime.getTime() > fs.statSync(downloadDir + b).mtime.getTime();
});
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
