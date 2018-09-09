const mousewheel = function(callback){
  require("domready")(function () {
    require("mouse-wheel")(document.body, callback);
  });
};

var rollbar = require('rollbar');

var Rollbar = rollbar.init({});
window.Rollbar = Rollbar;

var sha2 = require('sha2');

module.exports = {
  mousewheel: mousewheel,
  sha2: sha2
};
