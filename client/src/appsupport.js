document.title = window.location.hostname.split(".")[0] + " - Dark";

const mousewheel = function(callback) {
  require("domready")(function() {
    require("mouse-wheel")(document.body, callback);
  });
};

// ---------------------------
// Allows us capture certain keys and stop them from affecting the browser.
// ---------------------------
function stopKeys(event) {
  // Fluid
  if (event.target.id === "fluid-editor") {
    fluidStopKeys(event);
  }

  if (event.keyCode == 9) {
    // Tab
    event.preventDefault();
  }
  if (
    event.keyCode == 32 && // Space
    !event.target.parentNode.className.includes("string-container")
  ) {
    event.preventDefault();
  }
  if (
    event.keyCode == 13 && // Enter
    !event.target.parentNode.className.includes("large-string")
  ) {
    event.preventDefault();
  }
  if (
    event.keyCode == 38 || // Up
    event.keyCode == 40
  ) {
    // Down
    if (document.activeElement.tagName.toLowerCase() !== "textarea")
      event.preventDefault();
  }
  // Cmd/Ctrl + S (keyCode: 83)
  if ((event.ctrlKey || event.metaKey) && event.keyCode === 83) {
    event.preventDefault();
  }

  if (
    event.ctrlKey &&
    event.keyCode === 75 && // Ctrl-K
    window.navigator.platform.includes("Linux")
  ) {
    // `Ctrl-K` is meant to open the omnibox on Linux, but without this preventDefault
    // it will focus the browser's URL bar after creating the omnibox.
    event.preventDefault();
  }
}
window.stopKeys = stopKeys;

// ---------------------------
// Fluid
// ---------------------------

function isHandledByFluid(event) {
  var isMac = window.navigator.platform == "MacIntel";
  var isLinux = window.navigator.platform.includes("Linux");
  var knownPlatform = isMac || isLinux;

  // Keys specifically handled by fluid
  var isCtrlFluidKeyPress = event.ctrlKey && ["a", "e", "d"].includes(event.key);
  var isAltFluidKeyPress = event.altKey && event.keyCode == 88; // Alt-x
  var isFluidKeyPress = !event.metaKey && !event.ctrlKey && !event.altKey;

  return !knownPlatform || (isCtrlFluidKeyPress || isAltFluidKeyPress || isFluidKeyPress);
}

function fluidStopKeys(event) {
  if (isHandledByFluid(event)) {
    event.preventDefault();
  }
}

function getBrowserPlatform(event) {
  // Checks if mac
  var isMac = window.navigator.platform == "MacIntel";
  // Check if Linux
  var isLinux = window.navigator.platform.includes("Linux");
  // Check if Windows
  var isWindows = window.navigator.platform == "Win32";
  // known platform
  if (isMac) {
    return 0;
  } else if (isLinux) {
    return 1;
  } else if (isWindows) {
    return 2;
  } else {
    return 3;
  }
}
window.getBrowserPlatform = getBrowserPlatform;

// ---------------------------
// Rollbar
// ---------------------------
var rollbar = require("rollbar");
rollbarConfig.payload = rollbarConfig.payload || {};
rollbarConfig.payload.person = { id: userId, username: username };
var Rollbar = rollbar.init(rollbarConfig);
window.Rollbar = Rollbar;

function displayError(msg) {
  var event = new CustomEvent("displayError", { detail: msg });
  document.dispatchEvent(event);
}

window.onerror = function(msg, url, lineno, colno, error) {
  window.Rollbar.error(msg, error);
  window.lastError = error;
  console.error("Uncaught exception", msg, url, lineno, colno, error);
  displayError(msg);
};

window.onunhandledrejection = function(e) {
  window.lastRejection = e;
  window.Rollbar.error("Unhandled promise rejection", e.type, e.reason);
  console.error("Unhandled promise rejection", e.type, e.reason, e);
  displayError("Unhandled promise rejection: " + e.type + ", " + e.reason);
};

// ---------------------------
// Pusher
// ---------------------------

var Pusher = require("pusher-js");

if (pusherConfig.enabled) {
  var pusherConnection = new Pusher(pusherConfig.key, {
    cluster: pusherConfig.cluster,
    forceTLS: true,
  });
}

// ---------------------------
// Fluid
// ---------------------------
// https://stackoverflow.com/a/41034697/104021
function isChildOfEditor(node) {
  while (node !== null) {
    if (node.id === "fluid-editor") {
      return true;
    }
    node = node.parentNode;
  }

  return false;
}

// getFluidSelectionRange() returns the [begin, end] indices of the last
// selection/caret placement within a fluid editor, or undefined if
// there has been no selection/caret placement, or if the selected nodes are not
// part of a fluid editor. Begin may be > or < end,
// depending on the selection direction. If there is no true selection
// but rather a placement, begin == end.
// This function assumes there is a flat list of .fluid-entry tokens
// in the editor, and that any nesting is limited to stylistic nesting within a
// .fluid-entry node (such as that used for parens).
function getFluidSelectionRange() {
  function _parentCrawlToFluidEntryParentOrUndefined(node) {
    while (node !== null) {
      if (node.classList.contains("fluid-entry")) {
        return node;
      }
      node = node.parentNode;
    }
    return undefined;
  }

  // OPT(JULIAN): There's likely a more performant way of getting selection
  // start and end than to walk all prior nodes twice.
  let sel = window.getSelection();
  if (
    sel.focusNode == null ||
    !isChildOfEditor(sel.focusNode) ||
    !isChildOfEditor(sel.anchorNode)
  )
    return undefined;
  // The 'anchorNode' is the node where the selection began
  // and the offset is the relative position where the selection began
  // within the text of that node.
  let selBeginIdx = sel.anchorOffset;
  {
    let node = _parentCrawlToFluidEntryParentOrUndefined(sel.anchorNode.parentNode);
    if (!node) {
      // If this happens, it means that the selection wasn't inside a .fluid-entry node
      return undefined;
    }
    // This only works because we expect .fluid-entry nodes to be siblings with no nesting
    while (node.previousSibling) {
      node = node.previousSibling;
      selBeginIdx += node.textContent.length;
    }
  }
  // The 'focusNode' is the node where the selection ended
  // and the offset is the relative position where the selection ended
  // (and where the cursor is now located) within the text of that node.
  let selEndIdx = sel.focusOffset;
  {
    let node = _parentCrawlToFluidEntryParentOrUndefined(sel.focusNode.parentNode);
    if (!node) {
      // If this happens, it means that the selection wasn't inside a .fluid-entry node
      return undefined;
    }
    // This only works because we expect .fluid-entry nodes to be siblings with no nesting
    while (node.previousSibling) {
      node = node.previousSibling;
      selEndIdx += node.textContent.length;
    }
  }
  return [selBeginIdx, selEndIdx];
}

// setFluidSelectionRange([beginIdx, endIdx]) attempts to select the passed
// region in the currently selected fluid editor, if there is one.
// If beginIdx == endIdx, it sets the caret position (0-width selection).
// This function assumes we never want to place the selection within a
// nested DOM node (it crawls siblings).
// See getFluidSelectionRange for the counterpart.
function setFluidSelectionRange([beginIdx, endIdx]) {
  let clamp = function(num, min, max) {
    if (num < min) {
      return min;
    }
    if (num > max) {
      return max;
    }
    return num;
  };

  let editorOrNull = document.querySelector(".selected #fluid-editor");
  if (!editorOrNull) {
    return;
  }
  let editor = editorOrNull;
  let maxChars = editor.textContent.length;
  let anchorNodeOffset = clamp(beginIdx, 0, maxChars);
  let focusNodeOffset = clamp(endIdx, 0, maxChars);
  let anchorNode, focusNode; // The beginIdx is in the anchorNode and the endIdx is in the focusNode
  let childNodes = editor.childNodes;
  // This only works because nesting is so limited
  // TODO(JULIAN): We need to revisit the assumptions here if we need multi-char nesting.
  for (let i = 0; i < childNodes.length; i++) {
    let nodeLen = childNodes[i].textContent.length;
    if (anchorNodeOffset <= nodeLen /* the beginIdx must be inside this node */) {
      // We need to get the text node rather than the span,
      // which is why we do 'firstChild'
      anchorNode = childNodes[i].firstChild;
      break;
    } else {
      anchorNodeOffset -= nodeLen;
    }
  }
  // This only works because nesting is so limited
  // TODO(JULIAN): We need to revisit the assumptions here if we need multi-char nesting.
  for (let i = 0; i < childNodes.length; i++) {
    let nodeLen = childNodes[i].textContent.length;
    if (focusNodeOffset <= nodeLen /* the endIdx must be inside this node */) {
      // We need to get the text node rather than the span,
      // which is why we do 'firstChild'
      focusNode = childNodes[i].firstChild;
      break;
    } else {
      focusNodeOffset -= nodeLen;
    }
  }
  if (
    anchorNode &&
    focusNode &&
    isChildOfEditor(anchorNode) &&
    isChildOfEditor(focusNode)
  ) {
    window
      .getSelection()
      .setBaseAndExtent(anchorNode, anchorNodeOffset, focusNode, focusNodeOffset);
  }
}

window.getFluidSelectionRange = getFluidSelectionRange;
window.setFluidSelectionRange = setFluidSelectionRange;

// ---------------------------
// Analysis
// ---------------------------

window.Dark = {
  fetcher: {
    fetch: function(params) {
      if (!window.fetcherWorker) {
        console.log("FetchWorker not loaded yet");
        setTimeout(function() {
          console.log("Trying FetchWorker again");
          window.Dark.fetcher.fetch(params);
        }, 100);
        return;
      }

      window.fetcherWorker.postMessage(params);

      window.fetcherWorker.onmessage = function(e) {
        var event = new CustomEvent("receiveFetch", { detail: e.data });
        document.dispatchEvent(event);
      };
    },
  },
  analysis: {
    requestAnalysis: function(params) {
      if (!window.analysisWorker) {
        console.log("AnalysisWorker not loaded yet");
        setTimeout(function() {
          console.log("Trying AnalysisWorker again");
          window.Dark.analysis.requestAnalysis(params);
        }, 100);
        return;
      }

      window.analysisWorker.postMessage(params);

      window.analysisWorker.onmessage = function(e) {
        var result = e.data;

        var event = new CustomEvent("receiveAnalysis", { detail: result });
        document.dispatchEvent(event);
      };
    },
  },
  ast: {
    positions: function(tlid) {
      var extractId = function(elem) {
        var className = elem.className;
        var matches = /.*id-(\S+).*/g.exec(className);
        var id = matches[1];

        if (typeof id === "undefined")
          throw "Dark.ast.atomPositions: Cannot match Blank(id) regex for " + className;

        return id;
      };

      var find = function(tl, nested) {
        var atoms = [];
        tl.querySelectorAll(nested ? ".blankOr.nested" : ".blankOr:not(.nested)").forEach(
          (v, i, l) => {
            var rect = v.getBoundingClientRect();
            atoms.push({
              id: extractId(v),
              left: "" + (rect.left | 0),
              right: "" + (rect.right | 0),
              top: "" + (rect.top | 0),
              bottom: "" + (rect.bottom | 0),
            });
          },
        );
        return atoms;
      };

      var toplevels = document.getElementsByClassName("toplevel tl-" + tlid);

      if (toplevels.length == 0)
        throw "Dark.ast.atomPositions: Cannot find toplevel: " + tlid;

      var tl = toplevels[0];

      return {
        atoms: find(tl, false),
        nested: find(tl, true),
      };
    },
  },
  view: {
    capture: function() {
      var html2canvas = require("html2canvas");
      html2canvas(document.getElementById("app"), {
        backgroundColor: "#484848",
        ignoreElements: e => e.id === "sidebar-left" || e.id === "minimap",
      }).then(
        canvas => {
          const data = canvas.toDataURL("image/jpeg");
          const event = new CustomEvent("captureView", { detail: data });
          document.dispatchEvent(event);
        },
        err => {
          console.error("captureView", err);
        },
      );
    },
  },
};

function windowFocusChange(visible) {
  var event = new CustomEvent("windowFocusChange", { detail: visible });
  document.dispatchEvent(event);
}

var pageHidden = false;

function visibilityCheck() {
  var hidden = false;
  if (typeof document.hidden !== "undefined") {
    hidden = document.hidden;
  } else if (typeof document.mozHidden !== "undefined") {
    hidden = document.mozHidden;
  } else if (typeof document.msHidden !== "undefined") {
    hidden = document.msHidden;
  } else if (typeof document.webkitHidden !== "undefined") {
    hidden = document.webkitHidden;
  }

  if (pageHidden != hidden) {
    windowFocusChange(hidden);
    pageHidden = hidden;
  }
}

function addWheelListener(elem) {
  var prefix = "";
  var _addEventListener;
  var support;

  // detect event model
  if (window.addEventListener) {
    _addEventListener = "addEventListener";
  } else {
    _addEventListener = "attachEvent";
    prefix = "on";
  }

  // detect available wheel event
  support =
    "onwheel" in document.createElement("div")
      ? "wheel" // Modern browsers support "wheel"
      : document.onmousewheel !== undefined
      ? "mousewheel" // Webkit and IE support at least "mousewheel"
      : "DOMMouseScroll"; // let's assume that remaining browsers are older Firefox

  var listener = function(elem, useCapture) {
    _addWheelListener(elem, support, useCapture);

    // handle MozMousePixelScroll in older Firefox
    if (support == "DOMMouseScroll") {
      _addWheelListener(elem, "MozMousePixelScroll", useCapture);
    }
  };

  function _addWheelListener(elem, eventName, useCapture) {
    elem[_addEventListener](
      prefix + eventName,
      function(originalEvent) {
        !originalEvent && (originalEvent = window.event);

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
            originalEvent.preventDefault
              ? originalEvent.preventDefault()
              : (originalEvent.returnValue = false);
          },
        };

        // calculate deltaY (and deltaX) according to the event
        if (support == "mousewheel") {
          event.deltaY = (-1 / 40) * originalEvent.wheelDelta;
          // Webkit also support wheelDeltaX
          originalEvent.wheelDeltaX &&
            (event.deltaX = (-1 / 40) * originalEvent.wheelDeltaX);
        } else {
          event.deltaY = originalEvent.deltaY || originalEvent.detail;
        }
      },
      useCapture || false,
    );
  }

  return listener(elem);
}

setTimeout(function() {
  const canvasName = new URL(window.location).pathname.split("/")[2];
  const params = JSON.stringify({
    editorState: window.localStorage.getItem("editorState-" + canvasName),
    complete: complete,
    userContentHost: userContentHost,
    environment: environmentName,
    csrfToken: csrfToken,
    isAdmin: isAdmin,
    buildHash: buildHash,
    username: username,
  });
  var urlParams = new URLSearchParams(window.location.search);
  var enableDebugger = urlParams.get("debugger");
  if (enableDebugger === "0" || enableDebugger === "false" || enableDebugger === null) {
    app = app.normal(document.body, params);
  } else {
    app = app.debugging(document.body, params);
  }

  async function fetcher(url) {
    url = "//" + staticUrl + (hashReplacements[url] || url);
    return fetch(url)
      .then(resp => {
        if (resp.ok) {
          return resp.text();
        } else {
          console.error("Response error fetching", url, resp.text());
          throw resp.text();
        }
      })
      .catch(err => {
        console.error("Network error fetching", url);
        throw err;
      });
  }
  let rollbarConfigSetup =
    "const rollbarConfig = '" + JSON.stringify(rollbarConfig) + "';\n\n";

  let analysisjs = fetcher("/analysis.js");
  let analysiswrapperjs = fetcher("/analysiswrapper.js");
  let fetcherjs = fetcher("/fetcher.js");
  (async function() {
    var strings = [rollbarConfigSetup, await analysisjs, "\n\n", await analysiswrapperjs];
    var analysisWorkerUrl = window.URL.createObjectURL(new Blob(strings));
    window.analysisWorker = new Worker(analysisWorkerUrl);
  })();
  (async function() {
    var strings = [rollbarConfigSetup, await fetcherjs];
    var fetcherWorkerUrl = window.URL.createObjectURL(new Blob(strings));
    window.fetcherWorker = new Worker(fetcherWorkerUrl);
  })();

  window.onfocus = function(evt) {
    windowFocusChange(true);
  };
  window.onblur = function(evt) {
    windowFocusChange(false);
  };
  setInterval(visibilityCheck, 2000);
  addWheelListener(document);

  if (pusherConfig.enabled) {
    var pusherChannel = pusherConnection.subscribe(`canvas_${canvasId}`);
    pusherChannel.bind("new_trace", data => {
      var event = new CustomEvent("newTracePush", { detail: data });
      document.dispatchEvent(event);
    });
    pusherChannel.bind("new_404", data => {
      var event = new CustomEvent("new404Push", { detail: data });
      document.dispatchEvent(event);
    });
    pusherChannel.bind("new_static_deploy", data => {
      var event = new CustomEvent("newStaticDeploy", { detail: data });
      document.dispatchEvent(event);
    });
    pusherChannel.bind("new_presence", data => {
      var event = new CustomEvent("newPresencePush", { detail: data });
      document.dispatchEvent(event);
    });
    pusherChannel.bind("add_op", data => {
      var event = new CustomEvent("addOp", { detail: data });
      document.dispatchEvent(event);
    });
    pusherChannel.bind("worker_state", data => {
      var event = new CustomEvent("workerStatePush", { detail: data });
      document.dispatchEvent(event);
    });
  }
}, 1);
// ---------------------------
// Exports
// ---------------------------
module.exports = {
  mousewheel: mousewheel,
};
