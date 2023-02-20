const FullStory = require("@fullstory/browser");

const mousewheel = function (callback) {
  require("domready")(function () {
    require("mouse-wheel")(document.body, callback);
  });
};

// ---------------------------
// If you need to deploy app.js again with a different sha, increment this number
// Cache break: 1
// ---------------------------

// ---------------------------
// Check unsupported browser
// ---------------------------
function unsupportedBrowser() {
  var isChrome =
    /Chrome/.test(navigator.userAgent) && /Google Inc/.test(navigator.vendor);
  var isMobile = /Android|BlackBerry|iPhone|iPad|iPod|Opera Mini|IEMobile/.test(
    navigator.userAgent,
  );
  var isDesktopApp = /DarkLang\/Editor/i.test(navigator.userAgent);
  var isSupported = isDesktopApp || (isChrome && !isMobile);
  return !isSupported;
}

// This requires being loaded by defer, not as async
document.addEventListener("DOMContentLoaded", event => {
  if (unsupportedBrowser()) {
    let htmlString =
      "<div class='modal-overlay' id='unsupportedBrowser'><div class='modal'><div class='warning'><p class='title'>Dark is only fully supported on Desktop Chrome right now. We have a desktop client you can use if you prefer.</p><p class='title'>Unfortunately, as a small team, we don’t have the capacity to support other browsers at the moment. Once we’ve made the experience excellent on Chrome, we'll address cross-browser compatibility issues to support Firefox, Safari, and mobile devices.</p><p class='title'>Thanks for understanding ❤️</p></div></div></div>";
    var div = document.createElement("div");
    div.innerHTML = htmlString;
    document.body.append(div);
  }
});

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
// Analytics
// ---------------------------
require("../static/vendor/heapio.js");
heapio.load(heapioID);
heapio.identify(userID);
heapio.addUserProperties({
  handle: username,
  username: username,
  email: userEmail,
  name: userFullname,
  created_at: userCreatedAt,
});

window["_fs_ready"] = function () {
  heapio.track("FullStory Session", {
    "Fullstory Session URL": FS.getCurrentSessionURL(true),
  });
  heapio.addUserProperties({
    "Latest FullStory Session": FS.getCurrentSessionURL(),
  });
};

function sendHeapioMessage(event_name) {
  heapio.track(event_name);
  return;
}

window.sendHeapioMessage = sendHeapioMessage;

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
  if (
    event.ctrlKey &&
    event.key === "k" &&
    window.navigator.platform.includes("Linux")
  ) {
    event.preventDefault();
  }
}
window.stopKeys = stopKeys;

// ---------------------------
// Is it chrome?
// ---------------------------
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
//rollbarConfig.payload.person = { id: userID, username: username };

let searchParams = new URL(window.location.href).searchParams;
if (searchParams.get("use-assets-tunnel") !== null)
  rollbarConfig.enabled = false;

var Rollbar = rollbar.init(rollbarConfig);
window.Rollbar = Rollbar;

function displayError(msg) {
  var event = new CustomEvent("displayError", { detail: msg });
  document.dispatchEvent(event);
}

window.onerror = function (msg, url, lineno, colno, error) {
  console.error("Uncaught exception", msg, url, lineno, colno, error);
  window.Rollbar.error(msg, error);
  window.lastError = error;
  displayError(msg);
};

window.onunhandledrejection = function (e) {
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

window.Dark = {
  // ---------------------------
  // Worker to fetch and decode traces in the background
  // ---------------------------
  fetcher: {
    debugCount: 0,
    fetch: function (params) {
      if (!window.fetcherWorker) {
        if (window.Dark.fetcher.debugCount < 10) {
          if (window.Dark.fetcher.debugCount == 0)
            console.log("Fetcher: waiting for worker to initialize");
          window.Dark.fetcher.debugCount++;
        } else {
          window.Dark.fetcher.debugCount = 0;
        }
        setTimeout(function () {
          window.Dark.fetcher.fetch(params);
        }, 100);
        return;
      }

      window.fetcherWorker.postMessage(params);

      window.fetcherWorker.onmessage = function (e) {
        var event = new CustomEvent("receiveFetch", { detail: e.data });
        document.dispatchEvent(event);
      };
    },
  },

  // ---------------------------
  // Run analysis
  // ---------------------------
  analysis: {
    /* Next and busy are used to queue analyses. If busy is false, run
      immediately; else wait until the analysis is done and then run next. If
      next is not set, reset busy. */
    next: null,
    busy: false,
    /* Records the last time a result returned. So Integration tests will know has analysis finished running since a given timestamp */
    utils: require("../../lib/js/client/workers/AnalysisWrapper.bs.js"),
    debug: (function () {
      const urlParams = new URLSearchParams(window.location.search);
      return urlParams.get("debug-analysis") == "true";
    })(),
    callback: function (event) {
      const analysis = window.Dark.analysis;
      const worker = window.BlazorWorker;
      if (!analysis.initialized) {
        // We do some warm-up analysis that we don't need to send back to the user
        console.log("Blazor: received warm-up callback", event.data);
        analysis.initialized = true;
      } else {
        var result = analysis.utils.decodeOutput(event.data);

        var event = new CustomEvent("receiveAnalysis", { detail: result });
        document.dispatchEvent(event);

        // analysis queue: run the next analysis or mark not busy
        let params = analysis.next;
        if (params === null) {
          // no analyses waiting, we're done
          analysis.busy = false;
        } else {
          // an analysis is waiting, run it
          analysis.next = null;
          worker.postMessage(params);
        }

        window.Dark.analysis.lastRun = new Date();
      }
    },
    errorCallback: function (error) {
      console.log(
        error,
        "\nNote that errors related to the following exceptions can be ignored. They do no seem to cause an issue, and we believe they will go away when we upgrade to .NET 7.",
        " - 'System.ArgumentNullException: Value cannot be null'",
        " - 'System.Runtime.InteropServices.JavaScript.Runtime.ReleaseJSOwnedObjectByGCHandle(Int32 gcHandle)'",
      );

      // These errors are so pervasive, harmless, and come with a bunch of other
      // errors that are hard to define, so the best approach here is to ignore all
      // errors for now, and reinstate them once we upgradce to .NET7.
      // window.onerror("Blazor worker failure", error);
    },
    initialized: false,
    debugCount: 0,
    requestAnalysis: function (params) {
      const analysis = window.Dark.analysis;
      const worker = window.BlazorWorker;
      if (!analysis.initialized) {
        if (analysis.debugCount < 10) {
          if (analysis.debugCount == 0)
            console.log("Blazor: waiting for worker to initialize");
          analysis.debugCount++;
        } else {
          analysis.debugCount = 0;
        }
        setTimeout(function () {
          analysis.requestAnalysis(params);
        }, 500);
        return;
      }

      // OCaml would take a value that would be converted on the other side of
      // the worker, we need to stringify here to get the value to the worker.
      params = analysis.utils.stringifyInput(params);
      if (params.responseType === "error") {
        console.log("error calling F# analysis", params.json, params);
      } else {
        params = params.json;
      }

      if (analysis.busy) {
        // analysis queue: run immediately or store if busy
        // busy: record for next time
        analysis.next = params;
      } else {
        // not busy: run it immediately

        if (analysis.debug) {
          console.log("Requesting analysis", params);
        }
        worker.postMessage(params);
        analysis.busy = true;
      }
    },
    initializeBlazorWorker: function () {
      const analysis = window.Dark.analysis;
      let initializedCallback = () => {
        console.log("Blazor loaded, starting warm-up");
        worker.postMessage(analysis.utils.warmupValue); // "warm up" the eval with a simple `2+3` expr
      };
      // Only load when asked for
      window.BlazorWorker.initWorker(
        initializedCallback,
        analysis.callback,
        analysis.errorCallback,
      );
    },
    // Records the last time a result returned. So Integration tests will know has analysis finished running since a given timestamp
    lastRun: 0,
  },

  // ---------------------------
  // Fullstory
  // ---------------------------
  fullstory: {
    init: function (canvas) {
      const maxAccountAgeToRecordMs =
        // 2 days =
        48 /* hrs */ * 60 /* min/hr */ * 60 /* sec/min */ * 1000; /* ms/sec */
      const msSinceAccountCreated = new Date() - userCreatedAt;

      const isOlderThanWeWantToRecord =
        msSinceAccountCreated > maxAccountAgeToRecordMs;

      // the actual behavior is in FullStory.init's devMode flag, but this log
      // is here in hopes of reassuring users who look in console.
      if (userIsAdmin || isOlderThanWeWantToRecord) {
        console.log(
          "FullStory is not enabled for this user because the account is too old; console warnings that it is in dev mode may be safely ignored.",
        );
      }

      /* If devMode is set to true, FullStory will shutdown recording and all subsequent SDK method calls will be no-ops. */
      FullStory.init({
        orgId: "TMVRZ",
        devMode: userIsAdmin || isOlderThanWeWantToRecord,
      });
      FullStory.identify(username, {
        displayName: username,
        canvas,
      });

      const userStr = localStorage.getItem("userState-" + username);
      if (userStr) {
        try {
          const userSetting = JSON.parse(userStr);
          const recordConsent = userSetting.recordConsent;
          Dark.fullstory.setConsent(recordConsent);
        } catch (err) {
          console.error(err);
        }
      }
    },
    setConsent: function (consent) {
      FullStory.consent(consent);
      if (consent) {
        FullStory.restart();
      } else {
        FullStory.shutdown();
      }
    },
  },
};

// ---------------------------
// Focus
// ---------------------------
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

// ---------------------------
// Wheel
// ---------------------------
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

  var listener = function (elem, useCapture) {
    _addWheelListener(elem, support, useCapture);

    // handle MozMousePixelScroll in older Firefox
    if (support == "DOMMouseScroll") {
      _addWheelListener(elem, "MozMousePixelScroll", useCapture);
    }
  };

  function _addWheelListener(elem, eventName, useCapture) {
    elem[_addEventListener](
      prefix + eventName,
      function (originalEvent) {
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
          preventDefault: function () {
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

var moment = require("moment");

function formatDate([date, format]) {
  return moment(date).format(format);
}

window.Dark.formatDate = formatDate;

setTimeout(function () {
  // ---------------------------
  // Load the client
  // ---------------------------
  const canvasName = "stachu-scratch";
  const params = JSON.stringify({
    complete: complete,
    canvasName: canvasName,
    userContentHost: userContentHost,
    environment: environmentName,
    csrfToken: csrfToken,
    buildHash: buildHash,
    username: username,
  });
  var urlParams = new URLSearchParams(window.location.search);
  var enableDebugger = urlParams.get("debugger");
  if (
    enableDebugger === "0" ||
    enableDebugger === "false" ||
    enableDebugger === null
  ) {
    app = app.normal(document.body, params);
  } else {
    app = app.debugging(document.body, params);
  }

  // ---------------------------
  // Load webworkers
  // ---------------------------
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
  let buildHashSetup = "const buildHash = '" + buildHash + "';\n\n";

  let fetcherjs = fetcher("/fetcher.js");
  (async function () {
    var strings = [rollbarConfigSetup, buildHashSetup, await fetcherjs];
    var fetcherWorkerUrl = window.URL.createObjectURL(new Blob(strings));
    window.fetcherWorker = new Worker(fetcherWorkerUrl);
  })();

  // ---------------------------
  // Initialize blazorworker
  // ---------------------------
  //window.Dark.analysis.initializeBlazorWorker();

  // ---------------------------
  // Detect window focus change
  // ---------------------------
  window.onfocus = function (evt) {
    windowFocusChange(true);
  };
  window.onblur = function (evt) {
    windowFocusChange(false);
  };
  setInterval(visibilityCheck, 2000);

  // ---------------------------
  // Wheel
  // ---------------------------
  addWheelListener(document);

  // ---------------------------
  // Fullstory
  // ---------------------------
  Dark.fullstory.init(canvasName);

  // ---------------------------
  // Pusher channels
  // ---------------------------
  if (pusherConfig.enabled) {
    var pusherChannel = pusherConnection.subscribe(`canvas_${canvasID}`);
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
    pusherChannel.bind("v1/add_op", data => {
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
