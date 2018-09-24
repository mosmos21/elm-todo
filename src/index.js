require('./main.scss');

const { Elm } = require('./Main.elm');
const mountNode = document.getElementById('main');

const app = Elm.Main.init({
  node: mountNode
});
