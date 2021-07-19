// This file adapted taken from https://github.com/Tewr/BlazorWorker/blob/073c7b01e79319119947b427674b91804b784632/src/BlazorWorker/BlazorWorker.js
// It seems this was the original source: https://github.com/dotnet/aspnetcore/blob/main/src/Components/Web.JS/src/Platform/Mono/MonoPlatform.ts

// MIT license
// Copyright (c) 2019-2020 Tor
// https://github.com/Tewr/BlazorWorker/blob/073c7b01e79319119947b427674b91804b784632/LICENSE

// This file loads .NET in a webWorker, including the Dark LibExecution WASM.
// It creates and initializes the webworker, which then downloads all the
// WASM/Blazor code needed from the server.

window.BlazorWorker = (function () {
  let worker;
  const workerDef = function (appRoot, hashReplacements) {
    hashReplacements = JSON.parse(hashReplacements);
    const nonExistingDlls = [];
    let blazorBootManifest;
    const hashFile = file => {
      const hash = hashReplacements[file];
      if (hash) {
        // split variable on last double-colon
        let s = hash.split(".");
        return `${s[0]}-${hash}.${s[1]}`;
      } else {
        return file;
      }
    };

    const onReady = () => {
      // Setup the onmessage handler to call F#
      const messageHandler = Module.mono_bind_static_method(
        "[Wasm]Wasm.EvalWorker:OnMessage",
      );
      self.onmessage = msg => {
        messageHandler(msg.data);
      };
      // Send a message to indicate initialization complete
      self.postMessage("darkWebWorkerInitializedMessage");
    };

    const onError = err => {
      // FSTODO: add rollbar to error handler
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

    // This module is part of Emscripten, and the functions are used to set up
    // the Wasm-compiled version of .NET
    var Module = {};

    Module.print = line => console.log(`WASM-WORKER: ${line}`);

    Module.preRun = [];
    Module.postRun = [];

    Module.locateFile = fileName => {
      switch (fileName) {
        case "dotnet.wasm":
          let hashed = hashFile("blazor/dotnet.wasm");
          return `${appRoot}/${hashed}`;
        default:
          console.log("file wanted", file, hash);
          return hashFile(fileName);
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

      Object.keys(blazorBootManifest.resources.assembly).forEach(url => {
        if (!blazorBootManifest.resources.assembly.hasOwnProperty(url)) {
          // Do not attempt to load a dll which is not present anyway
          nonExistingDlls.push(url);
          return;
        }

        const runDependencyId = `blazor:${url}`;
        addRunDependency(runDependencyId);

        asyncLoad(`${appRoot}/blazor/${url}`).then(
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
      load_runtime("appBinDir", 0); // why appBinDir? Dunno
      MONO.mono_wasm_runtime_is_ready = true;
      onReady();
      if (nonExistingDlls.length > 0) {
        console.warn(
          `BlazorWorker: Module.postRun: ${nonExistingDlls.length} assemblies was specified as a dependency for the worker but was not present in the bootloader. This may be normal if trimmming is used. To remove this warning, either configure the linker not to trim the specified assemblies if they were removed in error, or conditionally remove the specified dependencies for builds that uses trimming. If trimming is not used, make sure that the assembly is included in the build.`,
          nonExistingDlls,
        );
      }
    });

    self.Module = Module;

    //TODO: This call could/should be session cached. But will the built-in blazor fetch service worker override
    // (PWA et al) do this already if configured ?
    let hashed = hashFile("blazor/blazor.boot.json");
    asyncLoad(`${appRoot}/${hashed}`, "json").then(
      blazorboot => {
        // Save this for loading other scripts later
        blazorBootManifest = blazorboot;

        // Start loading scripts
        let runtimeResources = Object.keys(blazorboot.resources.runtime);
        let dotnetjsfilename = runtimeResources.find(
          p => p.startsWith("dotnet.") && p.endsWith(".js"),
        );
        if (dotnetjsfilename === "") {
          throw "BlazorWorker: Unable to locate dotnetjs file in blazor boot config.";
        }
        self.importScripts(`${appRoot}/blazor/${dotnetjsfilename}`);
      },
      errorInfo => onError(errorInfo),
    );
  };

  // Initialize the worker
  let hashes = JSON.stringify(hashReplacements);
  const inlineWorker = `self.onmessage = ${workerDef}("${window.location.protocol}//${staticUrl}", "${hashes}")`;

  const initWorker = function (initCallback, onMessageCallback) {
    const blob = new Blob([inlineWorker], {
      type: "application/javascript",
    });
    worker = new Worker(URL.createObjectURL(blob));

    worker.onmessage = function (ev) {
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
