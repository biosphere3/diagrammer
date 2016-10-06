'use strict';

require('./index.html');
require('./index.css');
var Elm = require('./elm/Main');

Elm.Main.embed(document.getElementById('main'));
