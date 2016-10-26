'use strict';

require('./index.html');
require('./index.css');
var Elm = require('./elm/Main');

var libraryConfig = require("json!yaml!./library.yaml");

const flags = {
  library: libraryConfig
};

const app = Elm.Main.embed(
  document.getElementById('main'),
  flags
);

document.addEventListener("keydown", e => {
  app.ports.keyDown.send(e.keyCode);
});
