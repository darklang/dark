const mousewheel = function(callback){
  require("domready")(function () {
    require("mouse-wheel")(document.body, callback);
  });
};

var rollbar = require('rollbar');

var Rollbar = rollbar.init({});
window.Rollbar = Rollbar;

var sha512 = require('js-sha512');

module.exports = {
  mousewheel: mousewheel,
  sha512: sha512
};
