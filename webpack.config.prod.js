var webpack = require('webpack');

var config = require('./webpack.config.base');

config.plugins = [
  new webpack.optimize.UglifyJsPlugin()
];

module.exports = config;