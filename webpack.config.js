var path = require('path');
var webpack = require('webpack');

module.exports = {
  entry: [
    './src/index'
  ],
  output: {
    filename: 'index.js'
  },
  resolve: {
    alias: {
      'react': path.join(__dirname, 'node_modules/react')
    }
  },
  module: {
    loaders: [{
      test: /\.js$/,
      loaders: ['babel'],
      exclude: /node_modules/,
      include: __dirname
    }, {
      test: /\.elm$/,
      loaders: ['elm-simple-loader'],
      exclude: /node_modules/
    }, {
      test: /\.json$/,
      loader: "json-loader"
    }]
  },
  target: 'node'
};
