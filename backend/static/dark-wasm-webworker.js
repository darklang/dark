"use strict";

// Script to load Dark's WebAssembly runtime in a web worker.
// (this script is meant to be run in a worker like:
// `new Worker("dark-wasm-webworker.js")`
// Forked from SpawnDev.BlazorJS.WebWorkers
//
// First, this loads Blazor WebAssembly
// Then, it inits Dark's runtime, along with the bindings to the above 'editor-bootstrap.js'

var verboseLogging = false; // TODO: avail this flag to consumer
var consoleLog = function () {
  if (verboseLogging) {
    console.log(...arguments);
  }
};

// get global `this`
var checkIfGlobalThis = function (it) {
  return it && it.Math == Math && it;
};

// https://github.com/zloirock/core-js/issues/86#issuecomment-115759028
const globalThisObj =
  // eslint-disable-next-line es/no-global-this -- safe
  checkIfGlobalThis(typeof globalThis == "object" && globalThis) ||
  checkIfGlobalThis(typeof window == "object" && window) ||
  // eslint-disable-next-line no-restricted-globals -- safe
  checkIfGlobalThis(typeof self == "object" && self) ||
  checkIfGlobalThis(typeof global == "object" && global) ||
  // eslint-disable-next-line no-new-func -- fallback
  (function () {
    return this;
  })() ||
  Function("return this")();

consoleLog("location.href", location.href);

consoleLog("loading fake window environment");
importScripts("webworker-fake-env.js");

document.baseURI = new URL(location.href).origin;
consoleLog("document.baseURI", document.baseURI);

// Firefox, and possibly some other browsers, do not support dynamic module import (`import`) in workers.
// https://bugzilla.mozilla.org/show_bug.cgi?id=1540913
// Some scripts will have to be patched on the fly if import is not supported.
async function hasDynamicImport() {
  try {
    consoleLog("import is supported.");
    await import("data:text/javascript;base64,Cg==");
    return true;
  } catch (e) {
    consoleLog("import is not supported. A workaround will be used.");
    return false;
  }
}

var initWebWorkerBlazor = async function () {
  var dynamicImportSupported = await hasDynamicImport();

  async function getText(href) {
    var response = await fetch(new URL(href, document.baseURI), {
      cache: "force-cache",
    });
    return await response.text();
  }

  globalThisObj.importOverride = async function (src) {
    consoleLog("importOverride", src);
    var jsStr = await getText(src);
    jsStr = fixModuleScript(jsStr);
    let fn = new Function(jsStr);
    var ret = fn.apply(createProxiedObject(globalThisObj), []);
    if (!ret) ret = createProxiedObject({});
    return ret;
  };

  function fixModuleScript(jsStr) {
    // handle things that are automatically handled by import
    // import.meta.url
    jsStr = jsStr.replace(
      new RegExp("\\bimport\\.meta\\.url\\b", "g"),
      `document.baseURI`,
    );
    // import.meta
    jsStr = jsStr.replace(
      new RegExp("\\bimport\\.meta\\b", "g"),
      `{ url: location.href }`,
    );
    // import
    jsStr = jsStr.replace(new RegExp("\\bimport\\(", "g"), "importOverride(");
    // export
    // https://www.geeksforgeeks.org/what-is-export-default-in-javascript/
    // handle exports from
    // lib modules
    // Ex(_content/BlazorJS/BlazorJS.lib.module.js)
    // export function beforeStart(options, extensions) {
    // export function afterStarted(options, extensions) {
    var exportPatt = /\bexport[ \t]+function[ \t]+([^ \t(]+)/g;
    jsStr = jsStr.replace(exportPatt, "_exportsOverride.$1 = function $1");

    // handle exports from
    // dotnet.7.0.0.amub20uvka.js
    // export default createDotnetRuntime
    exportPatt = /\bexport[ \t]+default[ \t]+([^ \t;]+)/g;
    jsStr = jsStr.replace(exportPatt, "_exportsOverride.default = $1");
    // export { dotnet, exit, INTERNAL };
    exportPatt = /\bexport[ \t]+(\{[^}]+\})/g;
    jsStr = jsStr.replace(
      exportPatt,
      "_exportsOverride = Object.assign(_exportsOverride, $1)",
    );
    var modulize = `let _exportsOverride = {}; ${jsStr}; return _exportsOverride;`;
    return modulize;
  }

  async function initDocumentWithBlazor() {
    // set up standard document
    var htmlEl = document.appendChild(document.createElement("html"));
    var _headEl = htmlEl.appendChild(document.createElement("head"));
    var bodyEl = htmlEl.appendChild(document.createElement("body"));

    // Note: SpawnDev.BlazorJS had these divs, but they are not needed for our use case
    // // add <div id="app"> for Blazor
    // var appDiv = bodyEl.appendChild(document.createElement("div"));
    // appDiv.setAttribute("id", "app");

    // // add <div id="blazor-error-ui"> for Blazor
    // // if so, let's add an event listener so we're aware of any updates
    // var errorDiv = bodyEl.appendChild(document.createElement("div"));
    // errorDiv.setAttribute("id", "blazor-error-ui");

    // add <script src="blazor.webassembly.js" autostart="false"></script>
    let s =
      "http://dark-serve-static.dlio.localhost:11001/dark_wasm/blazor.webassembly.js";
    let scriptEl = bodyEl.appendChild(document.createElement("script"));
    scriptEl.setAttribute("src", s);
    scriptEl.setAttribute("autostart", "false");

    if (!dynamicImportSupported) {
      // convert dynamic imports in blazorWebAssembly and its imports
      let jsStr = await getText(s);
      jsStr = fixModuleScript(jsStr);
      scriptEl.text = jsStr;
    }

    document.initDocument();
  }

  await initDocumentWithBlazor();

  await Blazor.start({
    loadBootResource: function (_type, name, _defaultUri, _integrity) {
      return `http://dark-serve-static.dlio.localhost:11001/dark_wasm/${name}`;
    },
  });

  let invoke = async function (method, ...args) {
    return await DotNet.invokeMethodAsync("Wasm", method, ...args);
  };

  await invoke("InitializeDarkRuntime");

  // Receives events from the main thread,
  // and passes them off to Dark code to be handled.
  //
  // The results of that eval are handled separately
  onmessage = async function (e) {
    const evt = e.data;

    if (typeof evt !== "object") {
      console.error("Unexpected non-obj message in dark webworker", evt);
      return;
    }

    switch (evt.type) {
      case "load-client":
        const canvasName = evt.data.canvasName;
        await invoke("LoadClient", canvasName);
        self.postMessage({ type: "client-loaded" });
        break;

      case "handle-event":
        consoleLog("event received in webworker", evt.data);
        await invoke("HandleEvent", evt.data);

        break;
    }
  };

  // ugh, a hack for now I guess
  self.receiveEvalResult = function (state) {
    self.postMessage({ type: "eval-result", data: state });
  };

  self.postMessage({ type: "blazor-loaded" });
};

initWebWorkerBlazor();
