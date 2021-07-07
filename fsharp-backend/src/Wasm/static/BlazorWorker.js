// This file taken from https://github.com/Tewr/BlazorWorker/blob/073c7b01e79319119947b427674b91804b784632/src/BlazorWorker/BlazorWorker.js

// MIT license
// Copyright (c) 2019-2020 Tor
// https://github.com/Tewr/BlazorWorker/blob/073c7b01e79319119947b427674b91804b784632/LICENSE

window.BlazorWorker = (function () {
  let worker;
  const workerDef = function () {
    const initConf = JSON.parse('$initConf$');

    const nonExistingDlls = [];
    let blazorBootManifest = {
      resources: { assembly: { "AssemblyName.dll": "sha256-<sha256>" } },
    };

    const onReady = () => {
      console.log("OnReady called")
      const messageHandler = Module.mono_bind_static_method(
        "[Wasm]Wasm.EvalService:OnMessage"
      );
      // Future messages goes directly to the message handler
      self.onmessage = msg => {
        console.log("Calling into WASM")
        messageHandler(msg.data);
      };
      console.log("postmessage is", self.postMessage);
      self.postMessage("darkWebWorkerInitializedMessage");
    };

    const onError = err => {
      console.error(err);
    };

    function asyncLoad(url, reponseType) {
      return new Promise((resolve, reject) => {
        const xhr = new XMLHttpRequest();
        const arrayBufferType = "arraybuffer";
        xhr.open("GET", url, /* async: */ true);
        xhr.responseType = reponseType || arrayBufferType;
        xhr.onload = function xhr_onload() {
          if (xhr.status == 200 || (xhr.status == 0 && xhr.response)) {
            if (this.responseType === arrayBufferType) {
              const asm = new Uint8Array(xhr.response);
              resolve(asm);
            } else {
              resolve(xhr.response);
            }
          } else {
            reject(xhr);
          }
        };
        xhr.onerror = reject;
        xhr.send(undefined);
      });
    }

    var config = {};
    var Module = {};

    const wasmBinaryFile = `${initConf.appRoot}/${initConf.wasmRoot}/dotnet.wasm`;
    const suppressMessages = ["DEBUGGING ENABLED"];
    const appBinDirName = "appBinDir";

    Module.print = line =>
      suppressMessages.indexOf(line) < 0 && console.log(`WASM-WORKER: ${line}`);

    Module.preRun = [];
    Module.postRun = [];
    Module.preloadPlugins = [];

    Module.locateFile = fileName => {
      switch (fileName) {
        case "dotnet.wasm":
          return wasmBinaryFile;
        default:
          return fileName;
      }
    };

    Module.preRun.push(() => {
      const mono_wasm_add_assembly = Module.cwrap(
        "mono_wasm_add_assembly",
        null,
        ["string", "number", "number"],
      );

      mono_string_get_utf8 = Module.cwrap(
        "mono_wasm_string_get_utf8",
        "number",
        ["number"],
      );

      MONO.loaded_files = [];
      var baseUrl = `${initConf.appRoot}/${initConf.deploy_prefix}`;

      Object.keys(Module.blazorboot.resources.assembly).forEach(url => {
        if (!blazorBootManifest.resources.assembly.hasOwnProperty(url)) {
          //Do not attempt to load a dll which is not present anyway
          nonExistingDlls.push(url);
          return;
        }

        const runDependencyId = `blazor:${url}`;
        addRunDependency(runDependencyId);

        asyncLoad(baseUrl + "/" + url).then(
          data => {
            const heapAddress = Module._malloc(data.length);
            const heapMemory = new Uint8Array(
              Module.HEAPU8.buffer,
              heapAddress,
              data.length,
            );
            heapMemory.set(data);
            mono_wasm_add_assembly(url, heapAddress, data.length);
            MONO.loaded_files.push(url);
            removeRunDependency(runDependencyId);
          },
          errorInfo => {
            const isPdb404 =
              errorInfo instanceof XMLHttpRequest &&
              errorInfo.status === 404 &&
              url.match(/\.pdb$/);
            if (!isPdb404) {
              onError(errorInfo);
            }
            removeRunDependency(runDependencyId);
          },
        );
      });
    });

    Module.postRun.push(() => {
      MONO.mono_wasm_setenv("MONO_URI_DOTNETRELATIVEORABSOLUTE", "true");
      const load_runtime = Module.cwrap("mono_wasm_load_runtime", null, [
        "string",
        "number",
      ]);
      load_runtime(appBinDirName, 0);
      MONO.mono_wasm_runtime_is_ready = true;
      onReady();
      if (initConf.debug && nonExistingDlls.length > 0) {
        console.warn(
          `BlazorWorker: Module.postRun: ${nonExistingDlls.length} assemblies was specified as a dependency for the worker but was not present in the bootloader. This may be normal if trimmming is used. To remove this warning, either configure the linker not to trim the specified assemblies if they were removed in error, or conditionally remove the specified dependencies for builds that uses trimming. If trimming is not used, make sure that the assembly is included in the build.`,
          nonExistingDlls,
        );
      }
    });

    config.file_list = [];

    global = globalThis;
    self.Module = Module;

    //TODO: This call could/should be session cached. But will the built-in blazor fetch service worker override
    // (PWA et al) do this already if configured ?
    asyncLoad(`${initConf.appRoot}/${initConf.blazorBoot}`, "json").then(
      blazorboot => {
        Module.blazorboot = blazorboot;
        blazorBootManifest = blazorboot;
        let dotnetjsfilename = "";
        const runttimeSection = blazorboot.resources.runtime;
        for (var p in runttimeSection) {
          if (
            Object.prototype.hasOwnProperty.call(runttimeSection, p) &&
            p.endsWith(".js")
          ) {
            dotnetjsfilename = p;
          }
        }

        if (dotnetjsfilename === "") {
          throw "BlazorWorker: Unable to locate dotnetjs file in blazor boot config.";
        }

        self.importScripts(
          `${initConf.appRoot}/${initConf.wasmRoot}/${dotnetjsfilename}`,
        );
      },
      errorInfo => onError(errorInfo),
    );

    // Import script from a path relative to approot
    self.importLocalScripts = (...urls) => {
      self.importScripts(
        urls.map(
          url => initConf.appRoot + (url.startsWith("/") ? "" : "/") + url,
        ),
      );
    };

    self.isObjectDefined = workerScopeObject => {
      return getChildFromDotNotation(workerScopeObject) !== empty;
    };
  };

  const inlineWorker = `self.onmessage = ${workerDef}()`;

  const initWorker = function (id, initCallback, onMessageCallback, initOptions) {
    let appRoot =
      (
        document.getElementsByTagName("base")[0] || {
          href: window.location.origin,
        }
      ).href || "";
    if (appRoot.endsWith("/")) {
      appRoot = appRoot.substr(0, appRoot.length - 1);
    }

    const initConf = {
      appRoot: appRoot,
      deploy_prefix: initOptions.deployPrefix,
      wasmRoot: initOptions.wasmRoot,
      blazorBoot: `${initOptions.wasmRoot}/blazor.boot.json`,
      debug: initOptions.debug,
    };

    // Initialize worker
    const renderedConfig = JSON.stringify(initConf);
    const renderedInlineWorker = inlineWorker.replace(
      "$initConf$",
      renderedConfig,
    );
    window.URL = window.URL || window.webkitURL;
    const blob = new Blob([renderedInlineWorker], {
      type: "application/javascript",
    });
    const workerUrl = URL.createObjectURL(blob);
    worker = new Worker(workerUrl);

    worker.onmessage = function (ev) {
      if (true) {
        console.debug(
          `BlazorWorker.js:worker[]->blazor`,
          ev,
        );
      }
      if (ev.data === "darkWebWorkerInitializedMessage") {
        initCallback();
      } else {
        onMessageCallback(ev);
      }
    };
  };

  const postMessage = function (message) {
    worker.postMessage(message);
  };

  return {
    initWorker,
    postMessage,
  };
})();
