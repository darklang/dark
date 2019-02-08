document.title = window.location.hostname.split('.')[0] + " - Dark";

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

function displayError (msg){
  var event = new CustomEvent('displayError', {detail: msg});
  document.dispatchEvent(event);
}

window.onerror = function (msg, url, line, col, error) {
  window.Rollbar.error(msg, error);
  window.lastError = error;
  console.error("Uncaught exception", message, source, lineno, colno, error);
  displayError(msg);
};

window.onunhandledrejection = function (e) {
  window.lastRejection = e;
  window.Rollbar.error("Unhandled promise rejection", e.type, e.reason);
  console.error("Unhandled promise rejecton", e.type, e.reason, e);
  displayError("Unhandled promise rejecton: " + e.type + ", " + e.reason);
};




// ---------------------------
// Pusher
// ---------------------------

var Pusher = require('pusher-js');

if (pusherConfig.enabled) {
  var pusherConnection = new Pusher(pusherConfig.key, {
    cluster: pusherConfig.cluster,
    forceTLS: true,
  });
}

// ---------------------------
// Entrybox Caret
// ---------------------------

// The autocomplete box has the id 'search-container', and a number of
// subnodes, notably 'entry-box' and 'fluidWidthSpan'. 'entry-box' is where we
// write code, and where the cursor is. fluidWidthSpan has the text content of
// the box.
// However, for string entries, there is a textbox with the id 'entry-box'. This
// does have the text content.

function entrybox() {
  return document.getElementById('entry-box');
}

function fluidWidthSpan() {
  return document.getElementById("fluidWidthSpan");
}

// utils
function getTextNode(node) {
  return Array.from(node.childNodes).find(n => (n.nodeName == '#text'));
}

function getFnCallNode(node) {
  return Array.from(node.childNodes).find(n => (n.className == 'namegroup atom'));
}


function isNonStringNode(node) {
  return node.nodeType == node.TEXT_NODE;
}

// string entry box
function stringContent() {
  let el = entrybox();
  if (!el) return null;
  return el.value;
}

function stringContentNode() {
  return entrybox();
}

// other (non string) entry box
function nonStringContentNode() {
  let node = fluidWidthSpan();
  if (!node) return null;
  return getTextNode(node);
}

function nonStringContent() {
  let node = nonStringContentNode();
  if (!node) return null;
  return node.textContent;
}

// generic interface
function getContent () {
  return nonStringContent() || stringContent();
}

function getContentNode () {
  return nonStringContentNode() || stringContentNode();
}

function getSelectionNode() {
  return entrybox();
}

function getContentLength() {
  return getContent().length;
}

function getSelectionEnd() {
  return getSelectionNode().selectionEnd;
}

// Rendered means when we're not showing the input box. For strings, this includes quotes.
function getBoundsOfRendered(element) {
  let rect = element.getBoundingClientRect();
  if (element.classList.contains("tstr")) {
    return [rect.left+8, rect.right-8];
  } else {
    return [rect.left, rect.right];
  }
}


// Find location of the 'old' node (where the cursor is), in browser coordinates.
function findCaretXPos() {
  let contentNode = getContentNode();
  if (!contentNode) { return 0; }
  if (isNonStringNode(contentNode)) {
    let selectionNode = getSelectionNode();
    let offset = selectionNode.selectionEnd;
    if (offset != 0) {
      let range = document.createRange();
      range.setStart(contentNode, offset);
      range.setEnd(contentNode, offset);
      return range.getClientRects()[0].left;
    } else {
      // the above returns an empty getClientRects list for empty blanks,
      // so special-case it
      return selectionNode.getBoundingClientRect().x;
    }
  } else {
    // There appears to be no good way to get the actual coordinates of the
    // cursor in a textarea, without making a clone, adding a span at the cursor,
    // then finding the position of the span.
    // So we simulate.
    // Note that the text area does not include quotes, so they do not need to
    // be accounted for.
    // There is a small offset bug somewhere, where the expected x position of
    // the cursor is ever so slightly less than the bounds of the target. This
    // seems to only happen with strings, so this seems the obvious place to
    // handle it.
    let selectionNode = contentNode;
    let offset = selectionNode.selectionEnd; // already have the selection
    return contentNode.getBoundingClientRect().left
            + (offset * 8)
            + 0.04; // offset bug
  }
}

// Get target offset for 'new' node. Takes browser x/y coords in pixels, returns
// offset in characters.
function findLogicalOffset(targetBlankOrId, x) {
  let target = document.getElementById(targetBlankOrId);
  if (!target) { return false; }

  let [tleft, tright] = getBoundsOfRendered(target);
  if (tright <= x) {
    console.log("X is to the right of target, returning offset: -1");
    return -1;
  } else if (tleft >= x) {
    console.log("X is to the left of target, returning offset: 0");
    return 0;
  }

  function isClickInRects(rects) {
    return Array.from(rects).some(r => (r.left<=x && x<r.right));
  }

  let targetNode = getTextNode(target);
  if (targetNode) {
    // go through the characters and see if our x value is within any of them
    let range = document.createRange();
    let length = targetNode.textContent.length; // rendered, so must have a textcontent
    for (let i = 0; i < length; i++) {
      range.setStart(targetNode, i);
      range.setEnd(targetNode, i + 1);
      if (isClickInRects(range.getClientRects())) {
        return i;
      }
    }
  } else {
    targetNode = getFnCallNode(target);
    if (targetNode) {
      return Math.round ((x - tleft) / 8);
    }
  }


  console.error("We failed to set a correct offset!");
  return 0;
}

/* either we have room to move the caret in the node, or we return false and
  * move to another node */
function moveCaretLeft() {
  let length = getContentLength()
  if (length === null) { return false; }
  let currOffset = getSelectionEnd();

  if (currOffset <= 0) {
    return false;
  }

  // selectionStart here because selectionEnd results in moving two cells at
  // a time. :shrug:
  entrybox().selectionStart -= 1;
  return true;
}

function moveCaretRight() {
  let length = getContentLength();
  if (length === null) { return false; }
  let currOffset = getSelectionEnd();
  if (currOffset >= length) {
    return false;
  }
  entrybox().selectionEnd += 1;
  return true;
}

const entryboxCaret = {
  moveCaretLeft: moveCaretLeft,
  moveCaretRight: moveCaretRight,
  findCaretXPos: findCaretXPos,
  findLogicalOffset: findLogicalOffset
}

// ---------------------------
// Analysis
// ---------------------------

window.Dark = {
  caret: entryboxCaret,
  traceFetcher: {
    fetch : function(params) {
      if (!window.fetcherWorker) {
        return
      }

      window.fetcherWorker.postMessage(params);

      window.fetcherWorker.onmessage = function (e) {
        var event = new CustomEvent('receiveTraces', { "detail" : e.data });
        document.dispatchEvent(event);
      }
    }
  },
  analysis: {
    requestAnalysis : function (params) {
      if (!window.analysisWorker) {
        console.log("analysisworker not loaded yet");
        return;
      }

      // const handler = params.handler;
      // const bToString = (blankOr) => blankOr[2] || null;
      // const spec = params.handler.spec;
      // const route = `${bToString(spec.module)}, ${bToString(spec.name)}, ${bToString(spec.modifier)}`;
      // const tlid = handler.tlid;
      // const trace = params.trace.id;

      window.analysisWorker.postMessage(params);

      window.analysisWorker.onmessage = function (e) {
        var result = e.data;
        // var error = e.data.error;

        // if (!error) {
        var event = new CustomEvent('receiveAnalysis', {detail: result});
        document.dispatchEvent(event);
        // } else {
        //   var errorName = null;
        //   var errorMsg = null;
        //   try { errorName = error[1][1].c; } catch (_) {}
        //   try { errorMsg = error[2][1].c; } catch (_) {}
        //   try { if (!errorMsg) { errorMsg = error[2].c; } } catch (_) {}
        //   const errorStr = `${errorName} - ${errorMsg}`;

        //   // send to rollbar
        //   Rollbar.error( errorStr
        //                , error
        //                , { route: route
        //                  , tlid: tlid
        //                  , trace: trace });

        //   // log to console
        //   console.log(`Error processing analysis in (${route}, ${tlid}, ${trace})`, errorStr, error);

        //   // send to client
        //   displayError(`Error while executing (${route}, ${tlid}, ${trace}): ${errorStr}`);
        // }
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



function windowFocusChange (visible){
  var event = new CustomEvent('windowFocusChange', {detail: visible});
  document.dispatchEvent(event);
}


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
  var enableDebugger = urlParams.get("debugger");
  if (enableDebugger === "0" || enableDebugger === "false" || enableDebugger === null) {
    app = app.normal(document.body, params);
  } else {
    app = app.debugging(document.body, params);
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

  async function fetcher(url) {
    url = "//" + staticUrl + url;
    return fetch(url)
      .then(resp => {
        if (resp.ok) {
          return resp.text();
        } else {
          console.log("Response error fetching", url, resp.text());
          throw resp.text();
        }
      })
      .catch(err => {
        console.log("Network error fetching", url);
        throw err;
      });
  }

  let analysisjs = fetcher("/analysis.js");
  let analysiswrapperjs = fetcher("/analysiswrapper.js");
  let fetcherjs = fetcher("/tracefetcher.js");
  (async function () {
    var strings = [ await analysisjs, "\n\n", await analysiswrapperjs ];
    var analysisWorkerUrl = window.URL.createObjectURL(new Blob(strings));
    window.analysisWorker = new Worker(analysisWorkerUrl);
    window.analysisWorker.onerror = window.onerror;
    window.analysisWorker.onunhandledrejection = window.onunhandledrejection;
  })();
  (async function () {
    var strings = [ await fetcherjs ];
    var fetcherWorkerUrl = window.URL.createObjectURL(new Blob(strings));
    window.fetcherWorker = new Worker(fetcherWorkerUrl);
    window.fetcherWorker.onerror = window.onerror;
    window.fetcherWorker.onunhandledrejection = window.onunhandledrejection;
  })();


  window.onfocus = function(evt){ windowFocusChange(true) };
  window.onblur = function(evt){ windowFocusChange(false) };
  setInterval(visibilityCheck, 2000);
  addWheelListener(document);

  if (pusherConfig.enabled) {
    var pusherChannel = pusherConnection.subscribe(`canvas_${canvasId}`);
    pusherChannel.bind('new_trace', data => {
      var event = new CustomEvent('newTracePush', {detail: data});
      document.dispatchEvent(event);
    });
  }
}, 1)
// ---------------------------
// Exports
// ---------------------------
module.exports = {
  mousewheel: mousewheel
};
