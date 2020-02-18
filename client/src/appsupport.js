document.title = window.location.hostname.split(".")[0] + " - Dark";

const mousewheel = function(callback) {
  require("domready")(function() {
    require("mouse-wheel")(document.body, callback);
  });
};

function unsupportedBrowser() {
  var isChrome =
    /Chrome/.test(navigator.userAgent) && /Google Inc/.test(navigator.vendor);
  var isMobile = /Android|BlackBerry|iPhone|iPad|iPod|Opera Mini|IEMobile/.test(
    navigator.userAgent,
  );
  var isDesktopApp = /DarkLang\/Editor/.test(navigator.userAgent);
  var isSupported = isDesktopApp || (isChrome && !isMobile);
  return !isSupported;
}

window.unsupportedBrowser = unsupportedBrowser;

if (unsupportedBrowser) {
  // Reload page if user tries to delete overlay
  addEventListener("DOMContentLoaded", e => {
    let observer = new MutationObserver(records => {
      let reload = false;

      records.forEach(record => {
        record.removedNodes.forEach(node => {
          if (node.id === "unsupportedBrowser") {
            reload = true;
          }
        });
        if (reload) window.location.reload(true);
      });
    });

    observer.observe(document.documentElement, {
      attributes: true,
      childList: true,
      subtree: true,
      attributeOldValue: true,
      characterData: true,
    });
  });
}

// ---------------------------
// Allows us capture certain keys and stop them from affecting the browser.
// ---------------------------
function stopKeys(event) {
  // Don't ever attempt to save the HTML of the page.
  if ((event.ctrlKey || event.metaKey) && event.key === "s") {
    event.preventDefault();
  }

  // `Ctrl-K` is meant to open the omnibox on Linux, but without this preventDefault
  // it will focus the browser's URL bar after creating the omnibox.
  if (event.ctrlKey && event.key === "k" && window.navigator.platform.includes("Linux")) {
    event.preventDefault();
  }
}
window.stopKeys = stopKeys;

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
// Validation
// ---------------------------

var validator = require("validator");

function validateEmail(email) {
  return validator.isEmail(email);
}
window.validateEmail = validateEmail;

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
// Analytics
// ---------------------------

var Analytics = require("analytics-node");
var analytics = new Analytics("fVtoR1kNIsfZ484ovfavEybnNubNNVi8");
analytics.page({
  userId: `user-${username}`,
  name: "Canvas",
  properties: {
    url: document.URL,
    path: location.pathname,
    title: document.title,
    referrer: document.referrer,
  },
});

function sendSegmentMessage(event) {
  analytics.track({
    userId: `user-${username}`,
    event: event,
  });
  return;
}

window.sendSegmentMessage = sendSegmentMessage;

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
    complete: complete,
    canvasName: canvasName,
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
