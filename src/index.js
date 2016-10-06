'use strict';

require('./index.html');
require('./index.css');
var Elm = require('./elm/Main');

var libraryConfig = require("json!yaml!./library.yaml");

console.debug(libraryConfig);

Elm.Main.embed(document.getElementById('main'));
