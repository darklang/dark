const mousewheel = function(callback){
  require("domready")(function () {
    require("mouse-wheel")(document.body, callback);
  });
};

var rollbar = require('rollbar');

var Rollbar = rollbar.init({});
window.Rollbar = Rollbar;

module.exports = {
  mousewheel: mousewheel
};
