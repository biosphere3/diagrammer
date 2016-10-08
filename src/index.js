'use strict';

require('./index.html');
require('./index.css');
var Elm = require('./elm/Main');

var libraryConfig = require("json!yaml!./library.yaml");

const flags = {
  library: libraryConfig
};

Elm.Main.embed(
  document.getElementById('main'),
  flags
);
