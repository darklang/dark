const mousewheel = function(callback){
  require("domready")(function () {
    require("mouse-wheel")(document.body, callback);
  });
};

module.exports = {
  mousewheel: mousewheel
};
