//require('../css/main.scss');

const { Elm } = require('../../src/Main.elm');
const mountNode = document.getElementById('main');

Elm.Main.init({
  node: document.getElementById('main')
});
