document.title = window.location.hostname.split('.')[0] + " - Dark";

class RopArrow extends HTMLElement {
  constructor() {
    super();
  }
  update() {
    var tlid = this.getAttribute("tlid");
    var target = document.querySelector(".tl-" + tlid + " .rop-rail");
    if(target) {
      const targetPos = target.getBoundingClientRect();
      const sourcePos = this.getBoundingClientRect();
      const arrowHeadLength = 12;
      const x2 = targetPos.right - sourcePos.left - arrowHeadLength;
      const x1 = x2 / 3;

      var arrowRight = document.querySelector("rop-arrow > svg > path");
      var svg = document.querySelector("rop-arrow > svg");
      svg.setAttribute("width", x2 + arrowHeadLength);

      var d = "M 0,20 C 0,8 " + x1 + ",8 " + x2 + ",15";
      arrowRight.setAttribute("d", d);
    }
  }
  connectedCallback() {
    this.update();
  }
  static get observedAttributes() {
    return ['update'];
  }
  attributeChangedCallback(attr, _, val) {
    this.update();
  }
}
window.customElements.define('rop-arrow', RopArrow);

const mousewheel = function(callback){
  require("domready")(function () {
    require("mouse-wheel")(document.body, callback);
  });
};

// Allows us capture certain keys and stop them from affecting the browser.
function stopKeys(event) {
  if (event.keyCode == 9) { // Tab
    event.preventDefault();
  }
  if (event.keyCode == 32 // Space
      && !event.target.parentNode.className.includes("string-container")) {
    event.preventDefault();
  }
  if (event.keyCode == 13 // Enter
      && !event.target.parentNode.className.includes("large-string")) {
    event.preventDefault();
  }
  if (event.keyCode == 38 // Up
      || event.keyCode == 40) {  // Down
    if (document.activeElement.tagName.toLowerCase() !== 'textarea')
      event.preventDefault();
  }
}
window.stopKeys = stopKeys;

// ---------------------------
// Rollbar
// ---------------------------
var rollbar = require('rollbar');

var Rollbar = rollbar.init({});
window.Rollbar = Rollbar;

// ---------------------------
// Exports
// ---------------------------
module.exports = {
  mousewheel: mousewheel
};
