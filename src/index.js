'use strict';

require('./index.html');
require('./index.css');
var Elm = require('./Main');

Elm.Main.embed(document.getElementById('main'));
