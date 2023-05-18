'use strict';

// Todd Tanner
// 2022 - 2023
// SpawnDev.BlazorJS.WebWorkers
// _content/SpawnDev.BlazorJS.WebWorkers/spawndev.blazorjs.webworkers.js

var checkIfGlobalThis = function (it) {
    return it && it.Math == Math && it;
};

// https://github.com/zloirock/core-js/issues/86#issuecomment-115759028
const globalThisObj =
    // eslint-disable-next-line es/no-global-this -- safe
    checkIfGlobalThis(typeof globalThis == 'object' && globalThis) ||
    checkIfGlobalThis(typeof window == 'object' && window) ||
    // eslint-disable-next-line no-restricted-globals -- safe
    checkIfGlobalThis(typeof self == 'object' && self) ||
    checkIfGlobalThis(typeof global == 'object' && global) ||
    // eslint-disable-next-line no-new-func -- fallback
    (function () { return this; })() || Function('return this')();

const globalThisTypeName = globalThisObj.constructor.name;

// important for SharedWorker
// catch any incoming connetions that happen while .Net is loading
var _missedConnections = [];
function takeOverOnConnectEvent(newConnectFunction) {
    var tmp = _missedConnections;
    _missedConnections = [];
    globalThisObj.onconnect = newConnectFunction;
    return tmp;
}

if (globalThisTypeName == 'SharedWorkerGlobalScope') {
    globalThisObj.onconnect = function (e) {
        _missedConnections.push(e.ports[0]);
    };
}

var disableHotReload = true;
var verboseWebWorkers = location.search.indexOf('verbose=true') > -1;
var consoleLog = function () {
    if (!verboseWebWorkers) return;
    console.log(...arguments);
};

consoleLog('spawndev.blazorjs.webworkers: started');
consoleLog('location.href', location.href);
// location.href is this script
// location.href == 'https://localhost:7191/_content/SpawnDev.BlazorJS.WebWorkers/spawndev.blazorjs.webworkers.js?verbose=false'

consoleLog('spawndev.blazorjs.webworkers: loading fake window environment');
importScripts('spawndev.blazorjs.webworkers.faux-env.js');
// barebones dom has been created
// set document.baseURI
document.baseURI = new URL(`../../`, location.href).toString();
consoleLog('document.baseURI', document.baseURI);
// document.baseURI == 'https://localhost:7191/'

var stallForDebuggerSeconds = 0;
if (stallForDebuggerSeconds) {
    console.log(`Waiting ${stallForDebuggerSeconds} seconds for debugging`);
    var e = new Date().getTime() + (seconds * 1000);
    while (new Date().getTime() <= e) { }
    console.log(`Resuming`);
}

if (disableHotReload) {
    consoleLog('disabling hot reload on this thread');
    const scriptInjectedSentinel = '_dotnet_watch_ws_injected'
    globalThisObj[scriptInjectedSentinel] = true
}

async function hasDynamicImport() {
    try {
        await import('data:text/javascript;base64,Cg==');
        return true;
    } catch (e) {
        return false;
    }
}

var initWebWorkerBlazor = async function () {
    var dynamicImportSupported = await hasDynamicImport();
    // Firefox, and possibly some other browsers, do not support dynamic module import (import) in workers.
    // https://bugzilla.mozilla.org/show_bug.cgi?id=1540913
    // Some scripts will have to be patched on the fly if import is not supported.
    if (!dynamicImportSupported) {
        consoleLog("import is not supported. A workaround will be used.");
    } else {
        consoleLog('import is supported.');
    }

    async function getText(href) {
        var response = await fetch(new URL(href, document.baseURI), {
            cache: 'force-cache',
        });
        return await response.text();
    }
    // Get index.html
    var indexHtmlSrc = await getText('index.html');
    var indexHtmlScripts = [];
    var blazorWebAssemblyJSIndex = -1;
    function getIndexHtmlScripts() {
        var scriptPatt = new RegExp('<script\\s+(.*?)(?:\\s+\\/>|\\s*><\\/script>)', 'gm');
        var m;
        do {
            m = scriptPatt.exec(indexHtmlSrc);
            if (m) {
                let scriptTagBody = m[1];
                let scriptSrc = /src="(.+?)"/.exec(scriptTagBody)[1];
                let webworkerEnabled = !!/\bwebworker-enabled\b/.exec(scriptTagBody);
                let isBlazorWebAssemblyJS = scriptSrc == '_framework/blazor.webassembly.js';
                consoleLog('webworkerEnabled', webworkerEnabled, scriptSrc);
                if (webworkerEnabled || isBlazorWebAssemblyJS) {
                    if (isBlazorWebAssemblyJS) {
                        blazorWebAssemblyJSIndex = indexHtmlScripts.length;
                    }
                    indexHtmlScripts.push(scriptSrc);
                }
            }
        } while (m);
        if (blazorWebAssemblyJSIndex == -1) {
            blazorWebAssemblyJSIndex = indexHtmlScripts.length;
            indexHtmlScripts.push('_framework/blazor.webassembly.js');
        }
    }
    getIndexHtmlScripts();
    globalThisObj.importOverride = async function (src) {
        consoleLog('importOverride', src);
        var jsStr = await getText(src);
        jsStr = fixModuleScript(jsStr);
        let fn = new Function(jsStr);
        var ret = fn.apply(createProxiedObject(globalThisObj), []);
        if (!ret) ret = createProxiedObject({});
        return ret;
    }
    function fixModuleScript(jsStr) {
        // handle things that are automatically handled by import
        // import.meta.url
        jsStr = jsStr.replace(new RegExp('\\bimport\\.meta\\.url\\b', 'g'), `document.baseURI`);
        // import.meta
        jsStr = jsStr.replace(new RegExp('\\bimport\\.meta\\b', 'g'), `{ url: location.href }`);
        // import
        jsStr = jsStr.replace(new RegExp('\\bimport\\(', 'g'), 'importOverride(');
        // export
        // https://www.geeksforgeeks.org/what-is-export-default-in-javascript/
        // handle exports from
        // lib modules
        // Ex(_content/SpawnDev.BlazorJS/SpawnDev.BlazorJS.lib.module.js)
        // export function beforeStart(options, extensions) {
        // export function afterStarted(options, extensions) {
        var exportPatt = /\bexport[ \t]+function[ \t]+([^ \t(]+)/g;
        jsStr = jsStr.replace(exportPatt, '_exportsOverride.$1 = function $1');
        // handle exports from
        // dotnet.7.0.0.amub20uvka.js
        // export default createDotnetRuntime
        exportPatt = /\bexport[ \t]+default[ \t]+([^ \t;]+)/g;
        jsStr = jsStr.replace(exportPatt, '_exportsOverride.default = $1');
        // export { dotnet, exit, INTERNAL };
        exportPatt = /\bexport[ \t]+(\{[^}]+\})/g;
        jsStr = jsStr.replace(exportPatt, '_exportsOverride = Object.assign(_exportsOverride, $1)');
        //var n = 0;
        //var m = null;
        //exportPatt = new RegExp('\\bexport\\b.*?(?:;|$)', 'gm');
        //do {
        //    m = exportPatt.exec(jsStr);
        //    if (m) {
        //        n++;
        //        console.log('export', n, m[0]);
        //    }
        //} while (m);
        var modulize = `let _exportsOverride = {}; ${jsStr}; return _exportsOverride;`;
        return modulize;
    }
    async function initializeBlazor() {

        // setup standard document
        var htmlEl = document.appendChild(document.createElement('html'));
        var headEl = htmlEl.appendChild(document.createElement('head'));
        var bodyEl = htmlEl.appendChild(document.createElement('body'));
        // add blazor specific stuff
        // <div id="app">
        var appDiv = bodyEl.appendChild(document.createElement('div'));
        appDiv.setAttribute('id', 'app');
        // <div id="blazor-error-ui">
        var errorDiv = bodyEl.appendChild(document.createElement('div'));
        errorDiv.setAttribute('id', 'blazor-error-ui');
        // <script src="_framework/blazor.webassembly.js" autostart="false"></script>
        // load webworker-enabled scripts in order found in index.html (and _framework/blazor.webassembly.js)
        for (var i = 0; i < indexHtmlScripts.length; i++) {
            let s = indexHtmlScripts[i];
            let scriptEl = bodyEl.appendChild(document.createElement('script'));
            scriptEl.setAttribute('src', s);
            if (i == blazorWebAssemblyJSIndex) {
                scriptEl.setAttribute('autostart', "false");
                if (!dynamicImportSupported) {
                    // convert dynamic imports in blazorWebAssembly and its imports
                    let jsStr = await getText(s);
                    jsStr = fixModuleScript(jsStr);
                    scriptEl.text = jsStr;
                }
            }
        }
        // init document
        document.initDocument();
    }
    await initializeBlazor();

    // Blazor startup configuration
    // https://learn.microsoft.com/en-us/aspnet/core/blazor/fundamentals/environments?view=aspnetcore-7.0
    Blazor.start({
        loadBootResource: function (type, name, defaultUri, integrity) {
            var newURL = new URL(defaultUri, document.baseURI);
            return newURL.toString();
        }
    });
};
initWebWorkerBlazor();