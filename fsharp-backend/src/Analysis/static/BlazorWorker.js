// Adapted from github/dotnet/aspnetcore/src/Components/Web.JS/src/Platform
// https://github.com/dotnet/aspnetcore/tree/504c502b6b783cf3ebc48623e2099f09fe1af74d/src/Components/Web.JS/src/Platform
// MIT license

// This file copied a number of Blazor files to adapt them for a non-blazor world. The primary differences are:
// - no Blazor object
// - no Window object (as this is run in a web worker)
// - not typescript

// The files adapted are:
// - MonoPlatform.ts - does nearly all of the platform loading and initialization
// - WebAssemblyConfigLoader.ts - to load/cache config from the server
// - WebAssemblyResourceLoader.ts - to load/cache resouces (assemblies and such) from the server
// - BootConfig.ts - utility class around blazor.boot.json

// I've made a number of notes to discuss when I changed thing:
// D-REMOVED: explains why something was removed
// D-ADDED: explains why something was added
// D-CHANGED:  explains why something was changed
// Note that I didn't make notes when typescript types were removed

// =======================
// MonoPlatform.ts
// =======================
// https://github.com/dotnet/aspnetcore/blob/9b1de73ab6a18cbb3efbcb129a0ef911da3c4b5b/src/Components/Web.JS/src/Platform/Mono/MonoPlatform.ts

// This file loads .NET in a webWorker, including the Dark LibExecution WASM.
// It creates and initializes the webworker, which then downloads all the
// WASM/Blazor code needed from the server.

/* D-REMOVED
import { DotNet } from '@microsoft/dotnet-js-interop';
import { attachDebuggerHotkey, hasDebuggingEnabled } from './MonoDebugger';
import { showErrorNotification } from '../../BootErrors';
import { WebAssemblyResourceLoader, LoadingResource } from '../WebAssemblyResourceLoader';
import { Platform, System_Array, Pointer, System_Object, System_String, HeapLock } from '../Platform';
import { WebAssemblyBootResourceType } from '../WebAssemblyStartOptions';
import { BootJsonData, ICUDataMode } from '../BootConfig';
import { Blazor } from '../../GlobalExports';

declare let Module: EmscriptenModule;

let mono_wasm_add_assembly: (name: string, heapAddress: number, length: number) => void;
D-REMOVED */
// D-MOVED: to inside monoPlatform
// const appBinDirName = "appBinDir";
/* D-REMOVED
const uint64HighOrderShift = Math.pow(2, 32);
const maxSafeNumberHighPart = Math.pow(2, 21) - 1; // The high-order int32 from Number.MAX_SAFE_INTEGER

let currentHeapLock: MonoHeapLock | null = null;

// Memory access helpers
// The implementations are exactly equivalent to what the global getValue(addr, type) function does,
// except without having to parse the 'type' parameter, and with less risk of mistakes at the call site
function getValueI16(ptr: number) {
  return Module.HEAP16[ptr >> 1];
}
function getValueI32(ptr: number) {
  return Module.HEAP32[ptr >> 2];
}
function getValueFloat(ptr: number) {
  return Module.HEAPF32[ptr >> 2];
}
function getValueU64(ptr: number) {
  // There is no Module.HEAPU64, and Module.getValue(..., 'i64') doesn't work because the implementation
  // treats 'i64' as being the same as 'i32'. Also we must take care to read both halves as unsigned.
  const heapU32Index = ptr >> 2;
  const highPart = Module.HEAPU32[heapU32Index + 1];
  if (highPart > maxSafeNumberHighPart) {
    throw new Error(`Cannot read uint64 with high order part ${highPart}, because the result would exceed Number.MAX_SAFE_INTEGER.`);
  }

  return (highPart * uint64HighOrderShift) + Module.HEAPU32[heapU32Index];
}
D-REMOVED */
// D-CHANGED: turned into a function to serialize better to be passed to a webworker
// const monoPlatform = () => {
window.BlazorWorker = (function () {
  function workerDef() {
    // D-ADDED: from BootConfig
    // export enum ICUDataMode {
    //   Sharded,
    //   All,
    //   Invariant
    // }
    const ICUDataMode = {
      Sharded: 0,
      All: 1,
      Invariant: 2,
    };
    /* D-REMOVED
  start: function start(resourceLoader: WebAssemblyResourceLoader) {
    return new Promise<void>((resolve, reject) => {
      attachDebuggerHotkey(resourceLoader);

      // dotnet.js assumes the existence of this
      window['Browser'] = {
        // eslint-disable-next-line @typescript-eslint/no-empty-function
        init: () => { },
      };

      // Emscripten works by expecting the module config to be a global
      // For compatibility with macOS Catalina, we have to assign a temporary value to window.Module
      // before we start loading the WebAssembly files
      addGlobalModuleScriptTagsToDocument(() => {
        window['Module'] = createEmscriptenModuleInstance(resourceLoader, resolve, reject);
        addScriptTagsToDocument(resourceLoader);
      });
    });
  },

  callEntryPoint: async function callEntryPoint(assemblyName: string) : Promise<any> {
    const emptyArray = [[]];

    try {
      await BINDING.call_assembly_entry_point(assemblyName, emptyArray, 'm');
    } catch (error) {
      console.error(error);
      showErrorNotification();
    }
  },

  toUint8Array: function toUint8Array(array: System_Array<any>): Uint8Array {
    const dataPtr = getArrayDataPointer(array);
    const length = getValueI32(dataPtr);
    const uint8Array = new Uint8Array(length);
    uint8Array.set(Module.HEAPU8.subarray(dataPtr + 4, dataPtr + 4 + length));
    return uint8Array;
  },

  getArrayLength: function getArrayLength(array: System_Array<any>): number {
    return getValueI32(getArrayDataPointer(array));
  },

  getArrayEntryPtr: function getArrayEntryPtr<TPtr extends Pointer>(array: System_Array<TPtr>, index: number, itemSize: number): TPtr {
    // First byte is array length, followed by entries
    const address = getArrayDataPointer(array) + 4 + index * itemSize;
    return address as any as TPtr;
  },

  getObjectFieldsBaseAddress: function getObjectFieldsBaseAddress(referenceTypedObject: System_Object): Pointer {
    // The first two int32 values are internal Mono data
    return (referenceTypedObject as any as number + 8) as any as Pointer;
  },

  readInt16Field: function readHeapInt16(baseAddress: Pointer, fieldOffset?: number): number {
    return getValueI16((baseAddress as any as number) + (fieldOffset || 0));
  },

  readInt32Field: function readHeapInt32(baseAddress: Pointer, fieldOffset?: number): number {
    return getValueI32((baseAddress as any as number) + (fieldOffset || 0));
  },

  readUint64Field: function readHeapUint64(baseAddress: Pointer, fieldOffset?: number): number {
    return getValueU64((baseAddress as any as number) + (fieldOffset || 0));
  },

  readFloatField: function readHeapFloat(baseAddress: Pointer, fieldOffset?: number): number {
    return getValueFloat((baseAddress as any as number) + (fieldOffset || 0));
  },

  readObjectField: function readHeapObject<T extends System_Object>(baseAddress: Pointer, fieldOffset?: number): T {
    return getValueI32((baseAddress as any as number) + (fieldOffset || 0)) as any as T;
  },

  readStringField: function readHeapObject(baseAddress: Pointer, fieldOffset?: number, readBoolValueAsString?: boolean): string | null {
    const fieldValue = getValueI32((baseAddress as any as number) + (fieldOffset || 0));
    if (fieldValue === 0) {
      return null;
    }

    if (readBoolValueAsString) {
      // Some fields are stored as a union of bool | string | null values, but need to read as a string.
      // If the stored value is a bool, the behavior we want is empty string ('') for true, or null for false.
      const unboxedValue = BINDING.unbox_mono_obj(fieldValue as any as System_Object);
      if (typeof (unboxedValue) === 'boolean') {
        return unboxedValue ? '' : null;
      }
      return unboxedValue;
    }

    let decodedString: string | null | undefined;
    if (currentHeapLock) {
      decodedString = currentHeapLock.stringCache.get(fieldValue);
      if (decodedString === undefined) {
        decodedString = BINDING.conv_string(fieldValue as any as System_String);
        currentHeapLock.stringCache.set(fieldValue, decodedString);
      }
    } else {
      decodedString = BINDING.conv_string(fieldValue as any as System_String);
    }

    return decodedString;
  },

  readStructField: function readStructField<T extends Pointer>(baseAddress: Pointer, fieldOffset?: number): T {
    return ((baseAddress as any as number) + (fieldOffset || 0)) as any as T;
  },

  beginHeapLock: function() {
    assertHeapIsNotLocked();
    currentHeapLock = new MonoHeapLock();
    return currentHeapLock;
  },

  invokeWhenHeapUnlocked: function(callback) {
    // This is somewhat like a sync context. If we're not locked, just pass through the call directly.
    if (!currentHeapLock) {
      callback();
    } else {
      currentHeapLock.enqueuePostReleaseAction(callback);
    }
  },

function addScriptTagsToDocument(resourceLoader: WebAssemblyResourceLoader) {
  const browserSupportsNativeWebAssembly = typeof WebAssembly !== 'undefined' && WebAssembly.validate;
  if (!browserSupportsNativeWebAssembly) {
    throw new Error('This browser does not support WebAssembly.');
  }

  // The dotnet.*.js file has a version or hash in its name as a form of cache-busting. This is needed
  // because it's the only part of the loading process that can't use cache:'no-cache' (because it's
  // not a 'fetch') and isn't controllable by the developer (so they can't put in their own cache-busting
  // querystring). So, to find out the exact URL we have to search the boot manifest.
  const dotnetJsResourceName = Object
    .keys(resourceLoader.bootConfig.resources.runtime)
    .filter(n => n.startsWith('dotnet.') && n.endsWith('.js'))[0];
  const dotnetJsContentHash = resourceLoader.bootConfig.resources.runtime[dotnetJsResourceName];
  const scriptElem = document.createElement('script');
  scriptElem.src = `_framework/${dotnetJsResourceName}`;
  scriptElem.defer = true;

  // For consistency with WebAssemblyResourceLoader, we only enforce SRI if caching is allowed
  if (resourceLoader.bootConfig.cacheBootResources) {
    scriptElem.integrity = dotnetJsContentHash;
    scriptElem.crossOrigin = 'anonymous';
  }

  // Allow overriding the URI from which the dotnet.*.js file is loaded
  if (resourceLoader.startOptions.loadBootResource) {
    const resourceType: WebAssemblyBootResourceType = 'dotnetjs';
    const customSrc = resourceLoader.startOptions.loadBootResource(resourceType, dotnetJsResourceName, scriptElem.src, dotnetJsContentHash);
    if (typeof (customSrc) === 'string') {
      scriptElem.src = customSrc;
    } else if (customSrc) {
      // Since we must load this via a <script> tag, it's only valid to supply a URI (and not a Request, say)
      throw new Error(`For a ${resourceType} resource, custom loaders must supply a URI string.`);
    }
  }

  document.body.appendChild(scriptElem);
}

// Due to a strange behavior in macOS Catalina, we have to delay loading the WebAssembly files
// until after it finishes evaluating a <script> element that assigns a value to window.Module.
// This may be fixed in a later version of macOS/iOS, or even if not it may be possible to reduce
// this to a smaller workaround.
function addGlobalModuleScriptTagsToDocument(callback: () => void) {
  const scriptElem = document.createElement('script');

  // This pollutes global but is needed so it can be called from the script.
  // The callback is put in the global scope so that it can be run after the script is loaded.
  // onload cannot be used in this case for non-file scripts.
  window['__wasmmodulecallback__'] = callback;

  // Note: Any updates to the following script will require updating the inline script hash if using CSP
  scriptElem.text = 'var Module; window.__wasmmodulecallback__(); delete window.__wasmmodulecallback__;';

  document.body.appendChild(scriptElem);
}
D-REMOVED */

    function createEmscriptenModuleInstance(resourceLoader, onReady, onError) {
      const resources = resourceLoader.bootConfig.resources;
      const module = {};
      const suppressMessages = ["DEBUGGING ENABLED"];

      module.print = line =>
        suppressMessages.indexOf(line) < 0 && console.log(line);

      module.printErr = line => {
        // If anything writes to stderr, treat it as a critical exception. The underlying runtime writes
        // to stderr if a truly critical problem occurs outside .NET code. Note that .NET unhandled
        // exceptions also reach this, but via a different code path - see dotNetCriticalError below.
        self.postMessage({ type: "blazorError", err: line });
        // D-REMOVED: blazor thing
        // showErrorNotification();
      };

      module.preRun = module.preRun || [];
      module.postRun = module.postRun || [];
      module.preloadPlugins = [];

      const dotnetWasmResourceName = "dotnet.wasm";
      const assembliesBeingLoaded = resourceLoader.loadResources(
        resources.assembly,
        filename => `_framework/${filename}`,
        "assembly",
      );
      const pdbsBeingLoaded = resourceLoader.loadResources(
        resources.pdb || {},
        filename => `_framework/${filename}`,
        "pdb",
      );
      const wasmBeingLoaded = resourceLoader.loadResource(
        /* name */ dotnetWasmResourceName,
        /* url */ `_framework/${dotnetWasmResourceName}`,
        /* hash */ resourceLoader.bootConfig.resources.runtime[
          dotnetWasmResourceName
        ],
        /* type */ "dotnetwasm",
      );

      const dotnetTimeZoneResourceName = "dotnet.timezones.blat";
      let timeZoneResource = undefined;
      if (
        resourceLoader.bootConfig.resources.runtime.hasOwnProperty(
          dotnetTimeZoneResourceName,
        )
      ) {
        timeZoneResource = resourceLoader.loadResource(
          dotnetTimeZoneResourceName,
          `_framework/${dotnetTimeZoneResourceName}`,
          resourceLoader.bootConfig.resources.runtime[
            dotnetTimeZoneResourceName
          ],
          "globalization",
        );
      }

      let icuDataResource = undefined;
      if (resourceLoader.bootConfig.icuDataMode !== ICUDataMode.Invariant) {
        const applicationCulture =
          resourceLoader.startOptions.applicationCulture ||
          (navigator.languages && navigator.languages[0]);
        const icuDataResourceName = getICUResourceName(
          resourceLoader.bootConfig,
          applicationCulture,
        );
        icuDataResource = resourceLoader.loadResource(
          icuDataResourceName,
          `_framework/${icuDataResourceName}`,
          resourceLoader.bootConfig.resources.runtime[icuDataResourceName],
          "globalization",
        );

        module.instantiateWasm = (imports, successCallback) => {
          (async () => {
            let compiledInstance;
            try {
              const dotnetWasmResource = await wasmBeingLoaded;
              compiledInstance = await compileWasmModule(
                dotnetWasmResource,
                imports,
              );
            } catch (ex) {
              module.printErr(ex.toString());
              throw ex;
            }
            successCallback(compiledInstance);
          })();
          return []; // No exports
        };

        module.onRuntimeInitialized = () => {
          if (!icuDataResource) {
            // Use invariant culture if the app does not carry icu data.
            MONO.mono_wasm_setenv("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT", "1");
          }
        }; // D-CHANGED

        module.preRun.push(() => {
          // By now, emscripten should be initialised enough that we can capture these methods for later use
          mono_wasm_add_assembly = cwrap("mono_wasm_add_assembly", null, [
            "string",
            "number",
            "number",
          ]);
          MONO.loaded_files = [];

          if (timeZoneResource) {
            loadTimezone(timeZoneResource);
          }

          if (icuDataResource) {
            loadICUData(icuDataResource);
          }

          // Fetch the assemblies and PDBs in the background, telling Mono to wait until they are loaded
          // Mono requires the assembly filenames to have a '.dll' extension, so supply such names regardless
          // of the extensions in the URLs. This allows loading assemblies with arbitrary filenames.
          assembliesBeingLoaded.forEach(r =>
            addResourceAsAssembly(r, changeExtension(r.name, ".dll")),
          );
          pdbsBeingLoaded.forEach(r => addResourceAsAssembly(r, r.name));

          /* D-REMOVED: we don't have a blazor object
          Blazor._internal.dotNetCriticalError = (message) => {
            module.printErr(BINDING.conv_string(message) || '(null)');
          };
          D-REMOVED */

          /* D-REMOVED: we don't have a blazor object. But we might need satelite
             assemblies so possibly we might need to come back to this.
          Wire-up callbacks for satellite assemblies. Blazor will call these as part of the application
          startup sequence to load satellite assemblies for the application's culture.
          Blazor._internal.getSatelliteAssemblies = (culturesToLoadDotNetArray) => {
            const culturesToLoad = BINDING.mono_array_to_js_array(culturesToLoadDotNetArray);
            const satelliteResources = resourceLoader.bootConfig.resources.satelliteResources;
            if (satelliteResources) {
              const resourcePromises = Promise.all(culturesToLoad
                .filter(culture => satelliteResources.hasOwnProperty(culture))
                .map(culture => resourceLoader.loadResources(satelliteResources[culture], fileName => `_framework/${fileName}`, 'assembly'))
                .reduce((previous, next) => previous.concat(next), new Array())
                .map(async resource => (await resource.response).arrayBuffer()));
              return BINDING.js_to_mono_obj(resourcePromises.then(resourcesToLoad => {
                if (resourcesToLoad.length) {
                  Blazor._internal.readSatelliteAssemblies = () => {
                    const array = BINDING.mono_obj_array_new(resourcesToLoad.length);
                    for (let i = 0; i < resourcesToLoad.length; i++) {
                      BINDING.mono_obj_array_set(array, i, BINDING.js_typed_array_to_array(new Uint8Array(resourcesToLoad[i])));
                    }
                    return array;
                  };
                }
                return resourcesToLoad.length;
              }));
            }
            return BINDING.js_to_mono_obj(Promise.resolve(0));
          };
          D-REMOVED */

          /* D-REMOVED: we don't have a blazor object. But we probably need lazy assemblies so come back to this
          const lazyResources = {};
          Blazor._internal.getLazyAssemblies = (assembliesToLoadDotNetArray) => {
            const assembliesToLoad = BINDING.mono_array_to_js_array(assembliesToLoadDotNetArray);
            const lazyAssemblies = resourceLoader.bootConfig.resources.lazyAssembly;

            if (!lazyAssemblies) {
              throw new Error("No assemblies have been marked as lazy-loadable. Use the 'BlazorWebAssemblyLazyLoad' item group in your project file to enable lazy loading an assembly.");
            }

            const assembliesMarkedAsLazy = assembliesToLoad.filter(assembly => lazyAssemblies.hasOwnProperty(assembly));

            if (assembliesMarkedAsLazy.length !== assembliesToLoad.length) {
              const notMarked = assembliesToLoad.filter(assembly => !assembliesMarkedAsLazy.includes(assembly));
              throw new Error(`${notMarked.join()} must be marked with 'BlazorWebAssemblyLazyLoad' item group in your project file to allow lazy-loading.`);
            }

            let pdbPromises;
            if (hasDebuggingEnabled()) {
              const pdbs = resourceLoader.bootConfig.resources.pdb;
              const pdbsToLoad = assembliesMarkedAsLazy.map(a => changeExtension(a, '.pdb'));
              if (pdbs) {
                pdbPromises = Promise.all(pdbsToLoad
                  .map(pdb => lazyAssemblies.hasOwnProperty(pdb) ? resourceLoader.loadResource(pdb, `_framework/${pdb}`, lazyAssemblies[pdb], 'pdb') : null)
                  .map(async resource => resource ? (await resource.response).arrayBuffer() : null));
              }
            }

            const resourcePromises = Promise.all(assembliesMarkedAsLazy
              .map(assembly => resourceLoader.loadResource(assembly, `_framework/${assembly}`, lazyAssemblies[assembly], 'assembly'))
              .map(async resource => (await resource.response).arrayBuffer()));

            return BINDING.js_to_mono_obj(Promise.all([resourcePromises, pdbPromises]).then(values => {
              lazyResources['assemblies'] = values[0];
              lazyResources['pdbs'] = values[1];
              if (lazyResources['assemblies'].length) {
                Blazor._internal.readLazyAssemblies = () => {
                  const { assemblies } = lazyResources;
                  if (!assemblies) {
                    return BINDING.mono_obj_array_new(0);
                  }
                  const assemblyBytes = BINDING.mono_obj_array_new(assemblies.length);
                  for (let i = 0; i < assemblies.length; i++) {
                    const assembly = assemblies[i];
                    BINDING.mono_obj_array_set(assemblyBytes, i, BINDING.js_typed_array_to_array(new Uint8Array(assembly)));
                  }
                  return assemblyBytes;
                };

                Blazor._internal.readLazyPdbs = () => {
                  const { assemblies, pdbs } = lazyResources;
                  if (!assemblies) {
                    return BINDING.mono_obj_array_new(0);
                  }
                  const pdbBytes = BINDING.mono_obj_array_new(assemblies.length);
                  for (let i = 0; i < assemblies.length; i++) {
                    const pdb = pdbs && pdbs[i] ? new Uint8Array(pdbs[i]) : new Uint8Array();
                    BINDING.mono_obj_array_set(pdbBytes, i, BINDING.js_typed_array_to_array(pdb));
                  }
                  return pdbBytes;
                };
              }

              return lazyResources['assemblies'].length;
            }));
          };
          D-REMOVED */
        });
        module.postRun.push(() => {
          if (
            resourceLoader.bootConfig.debugBuild &&
            resourceLoader.bootConfig.cacheBootResources
          ) {
            resourceLoader.logToConsole();
          }
          resourceLoader.purgeUnusedCacheEntriesAsync(); // Don't await - it's fine to run in background

          if (resourceLoader.bootConfig.icuDataMode === ICUDataMode.Sharded) {
            MONO.mono_wasm_setenv("__BLAZOR_SHARDED_ICU", "1");

            if (resourceLoader.startOptions.applicationCulture) {
              // If a culture is specified via start options use that to initialize the Emscripten \  .NET culture.
              MONO.mono_wasm_setenv(
                "LANG",
                `${resourceLoader.startOptions.applicationCulture}.UTF-8`,
              );
            }
          }
          let timeZone = "UTC";
          try {
            timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
            // eslint-disable-next-line no-empty
          } catch {}
          MONO.mono_wasm_setenv("TZ", timeZone || "UTC");
          if (resourceLoader.bootConfig.modifiableAssemblies) {
            // Configure the app to enable hot reload in Development.
            MONO.mono_wasm_setenv(
              "DOTNET_MODIFIABLE_ASSEMBLIES",
              resourceLoader.bootConfig.modifiableAssemblies,
            );
          }

          if (resourceLoader.bootConfig.aspnetCoreBrowserTools) {
            // See https://github.com/dotnet/aspnetcore/issues/37357#issuecomment-941237000
            MONO.mono_wasm_setenv(
              "__ASPNETCORE_BROWSER_TOOLS",
              resourceLoader.bootConfig.aspnetCoreBrowserTools,
            );
          }

          const load_runtime = cwrap("mono_wasm_load_runtime", null, [
            "string",
            "number",
          ]);
          // -1 enables debugging with logging disabled. 0 disables debugging entirely.

          const appBinDirName = "appBinDir"; // D-MOVED from global scope
          load_runtime(appBinDirName, hasDebuggingEnabled() ? -1 : 0);
          MONO.mono_wasm_runtime_ready();
          // D-REMOVED
          // TODO: this might be a better to way to invoke things than what we have hacked in
          // attachInteropInvoker();
          onReady();
        });

        return module;

        async function addResourceAsAssembly(dependency, loadAsName) {
          const runDependencyId = `blazor:${dependency.name}`;
          addRunDependency(runDependencyId);

          try {
            // Wait for the data to be loaded and verified
            const dataBuffer = await dependency.response.then(r =>
              r.arrayBuffer(),
            );

            // Load it into the Mono runtime
            const data = new Uint8Array(dataBuffer);
            const heapAddress = Module._malloc(data.length);
            const heapMemory = new Uint8Array(
              Module.HEAPU8.buffer,
              heapAddress,
              data.length,
            );
            heapMemory.set(data);
            mono_wasm_add_assembly(loadAsName, heapAddress, data.length);
            MONO.loaded_files.push(toAbsoluteUrl(dependency.url));
          } catch (errorInfo) {
            onError(errorInfo);
            return;
          }

          removeRunDependency(runDependencyId);
        }
      }

      // D-REMOVED
      // const anchorTagForAbsoluteUrlConversions = document.createElement("a");
      function toAbsoluteUrl(possiblyRelativeUrl) {
        return new URL(possiblyRelativeUrl).href;
        // D-CHANGED - not supported in a web worker
        // anchorTagForAbsoluteUrlConversions.href = possiblyRelativeUrl;
        // return anchorTagForAbsoluteUrlConversions.href;
      }

      /* D-REMOVED
      function getArrayDataPointer(array){
        return array + 12; // First byte from here is length, then following bytes are entries
      }

      function bindStaticMethod(assembly, typeName, method) {
        // Fully qualified name looks like this: "[debugger-test] Math:IntAdd"
        const fqn = `[${assembly}] ${typeName}:${method}`;
        return BINDING.bind_static_method(fqn);
      }

      export let byteArrayBeingTransferred = null;
      function attachInteropInvoker() {
        const dotNetDispatcherInvokeMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'InvokeDotNet');
        const dotNetDispatcherBeginInvokeMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'BeginInvokeDotNet');
        const dotNetDispatcherEndInvokeJSMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'EndInvokeJS');
        const dotNetDispatcherNotifyByteArrayAvailableMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'NotifyByteArrayAvailable');

        DotNet.attachDispatcher({
          beginInvokeDotNetFromJS: (callId , assemblyName , methodIdentifier, dotNetObjectId , argsJson ) => {
            assertHeapIsNotLocked();
            if (!dotNetObjectId && !assemblyName) {
              throw new Error('Either assemblyName or dotNetObjectId must have a non null value.');
            }
            // As a current limitation, we can only pass 4 args. Fortunately we only need one of
            // 'assemblyName' or 'dotNetObjectId', so overload them in a single slot
            const assemblyNameOrDotNetObjectId = dotNetObjectId
              ? dotNetObjectId.toString()
              : assemblyName;

            dotNetDispatcherBeginInvokeMethodHandle(
              callId ? callId.toString() : null,
              assemblyNameOrDotNetObjectId,
              methodIdentifier,
              argsJson,
            );
          },
          endInvokeJSFromDotNet: (asyncHandle, succeeded, serializedArgs) => {
            dotNetDispatcherEndInvokeJSMethodHandle(serializedArgs);
          },
          sendByteArray: (id , data ) => {
            byteArrayBeingTransferred = data;
            dotNetDispatcherNotifyByteArrayAvailableMethodHandle(id);
          },
          invokeDotNetFromJS: (assemblyName, methodIdentifier, dotNetObjectId, argsJson) => {
            assertHeapIsNotLocked();
            return dotNetDispatcherInvokeMethodHandle(
              assemblyName ? assemblyName : null,
              methodIdentifier,
              dotNetObjectId ? dotNetObjectId.toString() : null,
              argsJson,
            );
          },
        });
      }
      D-REMOVED */

      async function loadTimezone(timeZoneResource) {
        const runDependencyId = "blazor:timezonedata";
        addRunDependency(runDependencyId);

        const request = await timeZoneResource.response;
        const arrayBuffer = await request.arrayBuffer();

        Module["FS_createPath"]("/", "usr", true, true);
        Module["FS_createPath"]("/usr/", "share", true, true);
        Module["FS_createPath"]("/usr/share/", "zoneinfo", true, true);
        MONO.mono_wasm_load_data_archive(
          new Uint8Array(arrayBuffer),
          "/usr/share/zoneinfo/",
        );

        removeRunDependency(runDependencyId);
      }

      function getICUResourceName(bootConfig, culture) {
        const combinedICUResourceName = "icudt.dat";
        if (!culture || bootConfig.icuDataMode === ICUDataMode.All) {
          return combinedICUResourceName;
        }

        const prefix = culture.split("-")[0];
        if (["en", "fr", "it", "de", "es"].includes(prefix)) {
          return "icudt_EFIGS.dat";
        } else if (["zh", "ko", "ja"].includes(prefix)) {
          return "icudt_CJK.dat";
        } else {
          return "icudt_no_CJK.dat";
        }
      }

      async function loadICUData(icuDataResource) {
        const runDependencyId = "blazor:icudata";
        addRunDependency(runDependencyId);

        const request = await icuDataResource.response;
        const array = new Uint8Array(await request.arrayBuffer());

        const offset = MONO.mono_wasm_load_bytes_into_heap(array);
        if (!MONO.mono_wasm_load_icu_data(offset)) {
          throw new Error("Error loading ICU asset.");
        }
        removeRunDependency(runDependencyId);
      }

      async function compileWasmModule(wasmResource, imports) {
        // This is the same logic as used in emscripten's generated js. We can't use emscripten's js because
        // it doesn't provide any method for supplying a custom response provider, and we want to integrate
        // with our resource loader cache.

        if (typeof WebAssembly["instantiateStreaming"] === "function") {
          try {
            const streamingResult = await WebAssembly["instantiateStreaming"](
              wasmResource.response,
              imports,
            );
            return streamingResult.instance;
          } catch (ex) {
            console.info(
              "Streaming compilation failed. Falling back to ArrayBuffer instantiation. ",
              ex,
            );
          }
        }

        // If that's not available or fails (e.g., due to incorrect content-type header),
        // fall back to ArrayBuffer instantiation
        const arrayBuffer = await wasmResource.response.then(r =>
          r.arrayBuffer(),
        );
        const arrayBufferResult = await WebAssembly.instantiate(
          arrayBuffer,
          imports,
        );
        return arrayBufferResult.instance;
      }

      function changeExtension(filename, newExtensionWithLeadingDot) {
        const lastDotIndex = filename.lastIndexOf(".");
        if (lastDotIndex < 0) {
          throw new Error(`No extension to replace in '${filename}'`);
        }

        return filename.substr(0, lastDotIndex) + newExtensionWithLeadingDot;
      }

      /* D-REMOVED
      function assertHeapIsNotLocked() {
        if (currentHeapLock) {
          throw new Error("Assertion failed - heap is currently locked");
        }
      }

      class MonoHeapLock implements HeapLock {
        // Within a given heap lock, it's safe to cache decoded strings since the memory can't change
        stringCache = new Map<number, string | null>();

        private postReleaseActions?: Function[];

        enqueuePostReleaseAction(callback: Function) {
          if (!this.postReleaseActions) {
            this.postReleaseActions = [];
          }

          this.postReleaseActions.push(callback);
        }

        release() {
          if (currentHeapLock !== this) {
            throw new Error('Trying to release a lock which isn\'t current');
          }

          currentHeapLock = null;

          while (this.postReleaseActions?.length) {
            const nextQueuedAction = this.postReleaseActions.shift()!;

            // It's possible that the action we invoke here might itself take a succession of heap locks,
            // but since heap locks must be released synchronously, by the time we get back to this stack
            // frame, we know the heap should no longer be locked.
            nextQueuedAction();
            assertHeapIsNotLocked();
          }
        }
      }
      D-REMOVED */
    }

    // =======================
    // WebAssemblyStartOptions.ts
    // =======================
    let startOptions = {
      environment: "development",
      applicationCulture: "en-US",
      loadBootResource: (type, name, defaultUri, integrity) => null,
    };
    // =======================
    // WebAssemblyResourceLoader.ts
    // =======================
    const resourceLoader = bootConfig => {
      // export interface LoadingResource {
      //   name: string;
      //   url: string;
      //   response: Promise<Response>;
      // }
      // export type ResourceList = { [name: string]: string };
      const loadResources = (resources, urlFn, resourceType) => {
        return Object.keys(resources).map(name =>
          loadResource(name, urlFn(name), resources[name], resourceType),
        );
      };
      // This corresponds to loadResourceWithoutCaching. In the future we should add caching.
      const loadResource = (name, url, contentHash, resourceType) => {
        url = toDarklangSetupUri(url);
        let response = fetch(url, {
          cache: "no-cache",
          integrity: bootConfig.cacheBootResources ? contentHash : undefined,
        });
        return { name, url, response };
      };
      return {
        loadResources: loadResources,
        loadResource: loadResource,
        logToConsole: () => {},
        bootConfig: bootConfig,
        startOptions: startOptions,
        purgeUnusedCacheEntriesAsync: () => Promise.resolve(),
      };
    };
    // =======================
    // MonoDebugger.ts
    // =======================
    function hasDebuggingEnabled() {
      return true;
    }

    // =======================
    // BootConfig.ts
    // =======================
    const onError = err => {
      self.postMessage({ type: "blazorError", err: err });
    };

    function bootConfig(onReady) {
      return fetch(toDarklangSetupUri("blazor.boot.json"))
        .then(response => response.json())
        .then(blazorBoot => {
          let loader = resourceLoader(blazorBoot);
          self.Module = createEmscriptenModuleInstance(
            loader,
            onReady,
            onError,
          );

          // Start loading scripts
          let runtimeResources = Object.keys(blazorBoot.resources.runtime);
          let dotnetjsfilename = runtimeResources.find(
            p => p.startsWith("dotnet.") && p.endsWith(".js"),
          );
          if (dotnetjsfilename === "") {
            throw "BlazorWorker: Unable to locate dotnetjs file in blazor boot config.";
          }
          self.importScripts(toDarklangSetupUri(dotnetjsfilename));
        })
        .catch(onError);
    }

    // =======================
    // Glue it together with our setup
    // =======================
    hashReplacements = JSON.parse(hashReplacements);
    function toDarklangSetupUri(file) {
      file = file.replace(/_framework\//, "");
      file = "/blazor/" + file;
      let hashed = hashReplacements[file];
      if (hashed) {
        // Remove the starting slash
        hashed = hashed.substring(1);
      } else {
        hashed = file;
      }
      return `${appRoot}/${hashed}`;
    }

    // =======================
    // Initialize
    // =======================

    const twoPlusThree = `[
      "AnalyzeHandler",
      {
          "handler": {
              "tlid": "163992584",
              "spec": {
                  "name": [ "Filled", "173388941", "violentTamarin" ],
                  "module": [ "Filled", "2107282711", "REPL" ],
                  "modifier": [ "Filled", "2133308295", "_" ],
                  "types": {
                      "input": [ "Blank", "848577556" ],
                      "output": [ "Blank", "1904777602" ]
                  }
              },
              "ast": [
                  "EBinOp",
                  "133960504",
                  "+",
                  [ "EInteger", "36273220", "2" ],
                  [ "EInteger", "196615730", "3" ],
                  ["NoRail" ]
              ]
          },
          "trace_id": "7d495105-946f-5ad8-8db9-4fd70e6eff67",
          "trace_data": {
              "input": [],
              "timestamp": "1970-01-01T00:00:00Z",
              "function_results": []
          },
          "dbs": [],
          "user_fns": [],
          "user_tipes": [],
          "secrets": []
      }
    ]`;

    const onReady = () => {
      console.log("Calling onReady");

      // Call anything we need to startup within Dark
      const initFn = Module.mono_bind_static_method(
        "[Analysis]Analysis.EvalWorker:InitializeDarkRuntime",
      );
      initFn();

      // Setup the onmessage handler to call F#
      const messageHandler = Module.mono_bind_static_method(
        "[Analysis]Analysis.EvalWorker:OnMessage",
      );
      self.onmessage = msg => {
        messageHandler(msg.data);
      };
      self.onerror = msg => {
        self.postMessage({ type: "blazorError", err: msg });
      };

      // "warm up" the eval with a simple `2+3` expr
      messageHandler(twoPlusThree);

      // Send a message to indicate initialization complete
      self.postMessage("darkWebWorkerInitializedMessage");
    };

    bootConfig(onReady);
    // doesn't return anything
  }

  // ===========================
  // Initialize the worker
  // ===========================
  const inlineWorker = `
    let appRoot = "${window.location.protocol}//${staticUrl}";
    let hashReplacements = '${JSON.stringify(hashReplacements)}';
    self.onmessage = (${workerDef})(appRoot, hashReplacements)`;

  const initWorker = function (
    initCallback,
    onMessageCallback,
    onErrorCallback,
  ) {
    const blob = new Blob([inlineWorker], {
      type: "application/javascript",
    });
    worker = new Worker(URL.createObjectURL(blob));

    worker.onmessage = function (ev) {
      if (ev.data.type === "blazorError") {
        onErrorCallback(ev.data.err);
      } else if (ev.data === "darkWebWorkerInitializedMessage") {
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
