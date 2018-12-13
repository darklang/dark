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
window.Rollbar.configure(rollbarConfig);

window.Dark = {
  caret: {
    // find current loc in 'old' node
    findCaretPointWithinTextElement: function(el_id) {
      let el = document.getElementById(el_id);
      if (el == null) { return {x: 0, y: 0}; }

      let node = Array.from(el.childNodes).find(n => (n.nodeName == '#text'));
      let currOffset = document.getElementById('entry-box').selectionEnd;

      let range = document.createRange();
      range.setStart(node, currOffset);
      range.setEnd(node, currOffset);

      let rect = range.getClientRects()[0];
      if (rect === undefined) {
        console.log("Error: no rect found. Likely, nodename = '#text' only applies to some dom constructions, not all?")
        return {x: 0, y: 0}
      }
      let retval = {x: rect.left, y: rect.bottom};

      return retval;
    },
    // get target offset for 'new' node
    // CLEANUP: we don't use the y param, drop it from the sig?
    //
    findLogicalOffsetWithinTextElement: function(el_id, x, y) {
      let el = document.getElementById(el_id);
      if (el == null) {
        return false;
      }
      let node = Array.from(el.childNodes).find(n => (n.nodeName == '#text'));
      if (node === undefined) {
        console.error("No childNode found with nodeName === '#text', returning offset 0.");
        return 0;
      }

      y = el.getBoundingClientRect().bottom;

      if (el.getBoundingClientRect().right < x) {
        console.log("X is to the right, returning offset: -1");
        return -1;
      } else if (el.getBoundingClientRect().left > x) {
        console.log("X is to the left, returning offset: 0");
        return 0;
      }

      let range = document.createRange();
      let length = node.textContent.length;
      function isClickInRects(rects) {
        return Array.from(rects).some(r => (r.left<x && r.right>x));
      }

      for (let i = 0; i < length; i++) {
        range.setStart(node, i);
        range.setEnd(node, i + 1);
        if (isClickInRects(range.getClientRects())) {
          return i;
        }
      }

      console.error("We failed to set a correct offset!");
      return 0;
    },
    getLength: function (el_id) {
      let el = document.getElementById(el_id);
      if (el) {
        let node = Array.from(el.childNodes).find(n => (n.nodeName == '#text'));
        if (node) return node.textContent.length;
        else console.error("No text childNode found");
      }
      else {
        let node = document.getElementById("entry-box");
        if (node) return node.value.length;
        else console.error("Could not find an entry-box");
      }
      return null;
    },
    /* either we have room to move the caret in the node, or we return false and
     * move to another node */
    moveCaretLeft: function(el_id) {
      let length = Dark.caret.getLength(el_id)
      if (length === null) { return false; }
      let currOffset = document.getElementById('entry-box').selectionEnd;

      if (currOffset <= 0) {
        return false;
      }

      // selectionStart here because selectionEnd results in moving two cells at
      // a time. :shrug:
      document.getElementById('entry-box').selectionStart -= 1;
      return true;
    },
    moveCaretRight: function(el_id) {
      let length = Dark.caret.getLength(el_id);
      if (length === null) { return false; }
      let currOffset = document.getElementById('entry-box').selectionEnd;
      if (currOffset >= length) {
        return false;
      }
      document.getElementById('entry-box').selectionEnd += 1;
      return true;
    }
  },
  analysis: {
    requestAnalysis : function (params) {
      if (!window.analysisWorker) {
        console.log("analysisworker not loaded yet");
        return;
      }

      const handler = params.handler;
      const bToString = (blankOr) => blankOr[2] || null;
      const spec = params.handler.spec;
      const route = `${bToString(spec.module)}, ${bToString(spec.name)}, ${bToString(spec.modifier)}`;
      const tlid = handler.tlid;
      const trace = params.trace.id;

      window.analysisWorker.postMessage(
        { proto: window.location.protocol,
          params: JSON.stringify (params)
        }
      );

      window.analysisWorker.onmessage = function (e) {
        var result = e.data.analysis;
        var error = e.data.error;

        if (!error) {
          var event = new CustomEvent('receiveAnalysis', {detail: result});
          document.dispatchEvent(event);
        } else {
          var errorName = null;
          var errorMsg = null;
          try { errorName = error[1][1].c; } catch (_) {}
          try { errorMsg = error[2][1].c; } catch (_) {}
          try { if (!errorMsg) { errorMsg = error[2].c; } } catch (_) {}
          const errorStr = `${errorName} - ${errorMsg}`;

          // send to rollbar
          Rollbar.error( errorStr
                       , error
                       , { route: route
                         , tlid: tlid
                         , trace: trace });

          // log to console
          console.log(`Error processing analysis in (${route}, ${tlid}, ${trace})`, errorStr, error);

          // send to client
          displayError(`Error while executing (${route}, ${tlid}, ${trace}): ${errorStr}`);
        }
      }
    }
  },
  ast: {
    positions: function (tlid) {
      var extractId = function (elem) {
        var className = elem.className;
        var matches = /.*id-(\S+).*/g.exec(className);
        var id = matches[1];

        if (typeof id === 'undefined')
          throw 'Dark.ast.atomPositions: Cannot match Blank(id) regex for '+className;

        return id;
      };

      var find = function (tl, nested) {
        var atoms = [];
        tl.querySelectorAll(nested ? '.blankOr.nested' : '.blankOr:not(.nested)')
        .forEach((v,i,l) => {
          var rect = v.getBoundingClientRect();
          atoms.push({
            id: extractId(v),
            left: "" + (rect.left | 0),
            right: "" + (rect.right | 0),
            top: "" + (rect.top | 0),
            bottom: "" + (rect.bottom | 0)
          });
        })
        return atoms;
      }

      var toplevels = document.getElementsByClassName('toplevel tl-'+tlid);

      if (toplevels.length == 0)
        throw 'Dark.ast.atomPositions: Cannot find toplevel: '+tlid;

      var tl = toplevels[0];

      return {
        atoms: find(tl, false),
        nested: find(tl, true)
      }
    }
  }
}

function displayError (msg){
  var event = new CustomEvent('displayError', {detail: msg});
  document.dispatchEvent(event);
}

function windowFocusChange (visible){
  var event = new CustomEvent('windowFocusChange', {detail: visible});
  document.dispatchEvent(event);
}

window.onerror = function (msg, url, line, col, error) {
  window.Rollbar.error(msg, error);
  displayError(msg);
};


var pageHidden = false;

function visibilityCheck(){
  var hidden = false;
  if (typeof document.hidden !== 'undefined') {
    hidden = document.hidden;
  } else if (typeof document.mozHidden !== 'undefined') {
    hidden = document.mozHidden;
  } else if (typeof document.msHidden !== 'undefined') {
    hidden = document.msHidden;
  } else if (typeof document.webkitHidden !== 'undefined') {
    hidden = document.webkitHidden;
  }

  if (pageHidden != hidden) {
    windowFocusChange(hidden);
    pageHidden = hidden;
  }
}

function addWheelListener(elem){
  var prefix = "";
  var _addEventListener;
  var support;

  // detect event model
  if ( window.addEventListener ) {
      _addEventListener = "addEventListener";
  } else {
      _addEventListener = "attachEvent";
      prefix = "on";
  }

  // detect available wheel event
  support = "onwheel" in document.createElement("div") ? "wheel" : // Modern browsers support "wheel"
            document.onmousewheel !== undefined ? "mousewheel" : // Webkit and IE support at least "mousewheel"
            "DOMMouseScroll"; // let's assume that remaining browsers are older Firefox

  var listener = function( elem, useCapture ) {
      _addWheelListener( elem, support, useCapture );

      // handle MozMousePixelScroll in older Firefox
      if( support == "DOMMouseScroll" ) {
          _addWheelListener( elem, "MozMousePixelScroll", useCapture );
      }
  };

  function _addWheelListener( elem, eventName, useCapture ) {
      elem[ _addEventListener ](prefix + eventName, function( originalEvent ) {
          !originalEvent && ( originalEvent = window.event );

          // create a normalized event object
          var event = {
              // keep a ref to the original event object
              originalEvent: originalEvent,
              target: originalEvent.target || originalEvent.srcElement,
              type: "wheel",
              deltaMode: originalEvent.type == "MozMousePixelScroll" ? 0 : 1,
              deltaX: 0,
              deltaY: 0,
              deltaZ: 0,
              preventDefault: function() {
                  originalEvent.preventDefault ?
                      originalEvent.preventDefault() :
                      originalEvent.returnValue = false;
              }
          };

          // calculate deltaY (and deltaX) according to the event
          if ( support == "mousewheel" ) {
              event.deltaY = - 1/40 * originalEvent.wheelDelta;
              // Webkit also support wheelDeltaX
              originalEvent.wheelDeltaX && ( event.deltaX = - 1/40 * originalEvent.wheelDeltaX );
          } else {
              event.deltaY = originalEvent.deltaY || originalEvent.detail;
          }

      }, useCapture || false );
  }

  return listener(elem);
}

setTimeout(function(){
  const canvasName = new URL(window.location).pathname.split("/")[2];
  const params = JSON.stringify(
    {
      editorState: window.localStorage.getItem('editorState-' + canvasName),
      complete: complete,
      userContentHost: userContentHost,
      environment: environmentName,
      csrfToken: csrfToken
    });
  var urlParams = new URLSearchParams(window.location.search);
  if (urlParams.has('debug')) {
    app = app.debugging(document.body, params);
  } else {
    app = app.normal(document.body, params);
  }

  window.onresize = function(evt){
    const size = {
      width : window.innerWidth,
      height: window.innerHeight
    }
    var event = new CustomEvent('windowResize',
      { detail : size })
    document.dispatchEvent(event)
  };

  let analysisjs = fetch("//" + staticUrl + "/analysis.js").then(r => r.text());
  let analysissupportjs = fetch("//" + staticUrl + "/analysissupport.js").then(r => r.text());
  var analysisWorkerUrl;
  (async function () {
    analysisWorkerUrl = window.URL.createObjectURL(
      new Blob(
        [ await analysisjs
        , "\n\n"
        , await analysissupportjs
        ]));
    window.analysisWorker = new Worker(analysisWorkerUrl);
  })();

  window.onfocus = function(evt){ windowFocusChange(true) };
  window.onblur = function(evt){ windowFocusChange(false) };
  setInterval(visibilityCheck, 2000);
  addWheelListener(document);

}, 1)
// ---------------------------
// Exports
// ---------------------------
module.exports = {
  mousewheel: mousewheel
};
