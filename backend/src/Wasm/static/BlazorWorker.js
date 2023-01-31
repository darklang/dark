"use strict";

var DotNet;
(function (DotNet) {
  window.DotNet = DotNet; // Ensure reachable from anywhere
  class JSObject {
    constructor(_jsObject) {
      this._jsObject = _jsObject;
      this._cachedFunctions = new Map();
    }
    findFunction(identifier) {
      const cachedFunction = this._cachedFunctions.get(identifier);
      if (cachedFunction) {
        return cachedFunction;
      }
      let result = this._jsObject;
      let lastSegmentValue;
      identifier.split(".").forEach(segment => {
        if (segment in result) {
          lastSegmentValue = result;
          result = result[segment];
        } else {
          throw new Error(
            `Could not find '${identifier}' ('${segment}' was undefined).`,
          );
        }
      });
      if (result instanceof Function) {
        result = result.bind(lastSegmentValue);
        this._cachedFunctions.set(identifier, result);
        return result;
      }
      throw new Error(`The value '${identifier}' is not a function.`);
    }
    getWrappedObject() {
      return this._jsObject;
    }
  }
  const windowJSObjectId = 0;
  const cachedJSObjectsById = {
    [windowJSObjectId]: new JSObject(window),
  };
  cachedJSObjectsById[windowJSObjectId]._cachedFunctions.set(
    "import",
    url => {
      // In most cases developers will want to resolve dynamic imports relative to the base HREF.
      // However since we're the one calling the import keyword, they would be resolved relative to
      // this framework bundle URL. Fix this by providing an absolute URL.
      if (typeof url === "string" && url.startsWith("./")) {
        url = document.baseURI + url.substr(2);
      }
      return import(/* webpackIgnore: true */ url);
    },
  );
  let dotNetDispatcher = null;
  /**
   * Sets the specified .NET call dispatcher as the current instance so that it will be used
   * for future invocations.
   *
   * @param dispatcher An object that can dispatch calls from JavaScript to a .NET runtime.
   */
  function attachDispatcher(dispatcher) {
    dotNetDispatcher = dispatcher;
  }
  DotNet.attachDispatcher = attachDispatcher;
  /**
   * Invokes the specified .NET public method synchronously. Not all hosting scenarios support
   * synchronous invocation, so if possible use invokeMethodAsync instead.
   *
   * @param assemblyName The short name (without key/version or .dll extension) of the .NET assembly containing the method.
   * @param methodIdentifier The identifier of the method to invoke. The method must have a [JSInvokable] attribute specifying this identifier.
   * @param args Arguments to pass to the method, each of which must be JSON-serializable.
   * @returns The result of the operation.
   */
  function invokeMethod(assemblyName, methodIdentifier, ...args) {
    return invokePossibleInstanceMethod(
      assemblyName,
      methodIdentifier,
      null,
      args,
    );
  }
  DotNet.invokeMethod = invokeMethod;

  function parseJson(json) {
    return json ? JSON.parse(json) : null;
  }

  function invokePossibleInstanceMethod(
    assemblyName,
    methodIdentifier,
    dotNetObjectId,
    args,
  ) {
    const dispatcher = getRequiredDispatcher();
    if (dispatcher.invokeDotNetFromJS) {
      const argsJson = JSON.stringify(args);
      const _resultJson = dispatcher.invokeDotNetFromJS(
        assemblyName,
        methodIdentifier,
        dotNetObjectId,
        argsJson,
      );
      return null;
    }
    throw new Error(
      "The current dispatcher does not support synchronous calls from JS to .NET. Use invokeMethodAsync instead.",
    );
  }

  function getRequiredDispatcher() {
    if (dotNetDispatcher !== null) {
      return dotNetDispatcher;
    }
    throw new Error("No .NET call dispatcher has been set.");
  }

  /**
   * Receives incoming calls from .NET and dispatches them to JavaScript.
   */
  DotNet.jsCallDispatcher = {
    /**
     * Invokes the specified synchronous JavaScript function.
     *
     * @param identifier Identifies the globally-reachable function to invoke.
     * @param argsJson JSON representation of arguments to be passed to the function.
     * @param targetInstanceId The instance ID of the target JS object.
     * @returns JSON representation of the invocation result.
     */
    invokeJSFromDotNet: (
      identifier,
      argsJson,
      targetInstanceId,
    ) => {
      const _returnValue = findJSFunction(identifier, targetInstanceId).apply(
        null,
        parseJson(argsJson),
      );

      return null;
    },
  };

  function findJSFunction(identifier, targetInstanceId) {
    const targetInstance = cachedJSObjectsById[targetInstanceId];
    if (targetInstance) {
      return targetInstance.findFunction(identifier);
    }
    throw new Error(
      `JS object instance with ID ${targetInstanceId} does not exist (has it been disposed?).`,
    );
  }
})(DotNet || (DotNet = {}));



const Blazor = { _internal: { } };

// Make the following APIs available in global scope for invocation from JS
window["Blazor"] = Blazor;

// CONCATENATED MODULE: ./Environment.ts
let platform;
function setPlatform(platformInstance) {
  platform = platformInstance;
  return platform;
}


class BootConfigResult {
  constructor(bootConfig, applicationEnvironment) {
    this.bootConfig = bootConfig;
    this.applicationEnvironment = applicationEnvironment;
  }

  static async initAsync(loadBootResource, environment) {
    const loaderResponse =
      loadBootResource !== undefined
        ? loadBootResource(
            "manifest",
            "blazor.boot.json",
            "_framework/blazor.boot.json",
            "",
          )
        : defaultLoadBlazorBootJson("_framework/blazor.boot.json");

    let bootConfigResponse;
    if (!loaderResponse) {
      bootConfigResponse = await defaultLoadBlazorBootJson(
        "_framework/blazor.boot.json",
      );
    } else if (typeof loaderResponse === "string") {
      bootConfigResponse = await defaultLoadBlazorBootJson(loaderResponse);
    } else {
      bootConfigResponse = await loaderResponse;
    }
    // While we can expect an ASP.NET Core hosted application to include the environment, other
    // hosts may not. Assume 'Production' in the absence of any specified value.
    const applicationEnvironment =
      environment ||
      bootConfigResponse.headers.get("Blazor-Environment") ||
      "Production";
    const bootConfig = await bootConfigResponse.json();
    bootConfig.modifiableAssemblies = bootConfigResponse.headers.get(
      "DOTNET-MODIFIABLE-ASSEMBLIES",
    );
    bootConfig.aspnetCoreBrowserTools = bootConfigResponse.headers.get(
      "ASPNETCORE-BROWSER-TOOLS",
    );
    return new BootConfigResult(bootConfig, applicationEnvironment);
    function defaultLoadBlazorBootJson(url) {
      return fetch(url, {
        method: "GET",
        credentials: "include",
        cache: "no-cache",
      });
    }
  }
}

var ICUDataMode;
(function (ICUDataMode) {
  ICUDataMode[(ICUDataMode["Sharded"] = 0)] = "Sharded";
  ICUDataMode[(ICUDataMode["All"] = 1)] = "All";
  ICUDataMode[(ICUDataMode["Invariant"] = 2)] = "Invariant";
})(ICUDataMode || (ICUDataMode = {}));


// CONCATENATED MODULE: ./Platform/Mono/MonoPlatform.ts

// initially undefined and only fully initialized after createEmscriptenModuleInstance()
let BINDING = undefined;
let MONO = undefined;
let Module = undefined;
let IMPORTS = undefined;
const appBinDirName = "appBinDir";
const uint64HighOrderShift = Math.pow(2, 32);
const maxSafeNumberHighPart = Math.pow(2, 21) - 1; // The high-order int32 from Number.MAX_SAFE_INTEGER
let currentHeapLock = null;
// Memory access helpers
// The implementations are exactly equivalent to what the global getValue(addr, type) function does,
// except without having to parse the 'type' parameter, and with less risk of mistakes at the call site
function getValueI16(ptr) {
  return MONO.getI16(ptr);
}
function getValueI32(ptr) {
  return MONO.getI32(ptr);
}
function getValueFloat(ptr) {
  return MONO.getF32(ptr);
}
function getValueU64(ptr) {
  // There is no Module.HEAPU64, and Module.getValue(..., 'i64') doesn't work because the implementation
  // treats 'i64' as being the same as 'i32'. Also we must take care to read both halves as unsigned.
  const heapU32Index = ptr >> 2;
  const highPart = Module.HEAPU32[heapU32Index + 1];
  if (highPart > maxSafeNumberHighPart) {
    throw new Error(
      `Cannot read uint64 with high order part ${highPart}, because the result would exceed Number.MAX_SAFE_INTEGER.`,
    );
  }
  return highPart * uint64HighOrderShift + Module.HEAPU32[heapU32Index];
}
const monoPlatform = {
  start: async function start(resourceLoader) {
    await createEmscriptenModuleInstance(resourceLoader);
  },
  callEntryPoint: async function callEntryPoint(assemblyName) {
    const emptyArray = [[]];
    try {
      await BINDING.call_assembly_entry_point(assemblyName, emptyArray, "m");
    } catch (error) {
      console.error(error);
    }
  },
  toUint8Array: function toUint8Array(array) {
    const dataPtr = getArrayDataPointer(array);
    const length = getValueI32(dataPtr);
    const uint8Array = new Uint8Array(length);
    uint8Array.set(Module.HEAPU8.subarray(dataPtr + 4, dataPtr + 4 + length));
    return uint8Array;
  },
  getArrayLength: function getArrayLength(array) {
    return getValueI32(getArrayDataPointer(array));
  },
  getArrayEntryPtr: function getArrayEntryPtr(array, index, itemSize) {
    // First byte is array length, followed by entries
    const address = getArrayDataPointer(array) + 4 + index * itemSize;
    return address;
  },
  getObjectFieldsBaseAddress: function getObjectFieldsBaseAddress(
    referenceTypedObject,
  ) {
    // The first two int32 values are internal Mono data
    return referenceTypedObject + 8;
  },
  readInt16Field: function readHeapInt16(baseAddress, fieldOffset) {
    return getValueI16(baseAddress + (fieldOffset || 0));
  },
  readInt32Field: function readHeapInt32(baseAddress, fieldOffset) {
    return getValueI32(baseAddress + (fieldOffset || 0));
  },
  readUint64Field: function readHeapUint64(baseAddress, fieldOffset) {
    return getValueU64(baseAddress + (fieldOffset || 0));
  },
  readFloatField: function readHeapFloat(baseAddress, fieldOffset) {
    return getValueFloat(baseAddress + (fieldOffset || 0));
  },
  readObjectField: function readHeapObject(baseAddress, fieldOffset) {
    return getValueI32(baseAddress + (fieldOffset || 0));
  },
  readStringField: function readHeapObject(
    baseAddress,
    fieldOffset,
    readBoolValueAsString,
  ) {
    const fieldValue = getValueI32(baseAddress + (fieldOffset || 0));
    if (fieldValue === 0) {
      return null;
    }
    if (readBoolValueAsString) {
      // Some fields are stored as a union of bool | string | null values, but need to read as a string.
      // If the stored value is a bool, the behavior we want is empty string ('') for true, or null for false.
      const unboxedValue = BINDING.unbox_mono_obj(fieldValue);
      if (typeof unboxedValue === "boolean") {
        return unboxedValue ? "" : null;
      }
      return unboxedValue;
    }
    let decodedString;
    if (currentHeapLock) {
      decodedString = currentHeapLock.stringCache.get(fieldValue);
      if (decodedString === undefined) {
        decodedString = BINDING.conv_string(fieldValue);
        currentHeapLock.stringCache.set(fieldValue, decodedString);
      }
    } else {
      decodedString = BINDING.conv_string(fieldValue);
    }
    return decodedString;
  },
  readStructField: function readStructField(baseAddress, fieldOffset) {
    return baseAddress + (fieldOffset || 0);
  },
  beginHeapLock: function () {
    assertHeapIsNotLocked();
    currentHeapLock = new MonoHeapLock();
    return currentHeapLock;
  },
  invokeWhenHeapUnlocked: function (callback) {
    // This is somewhat like a sync context. If we're not locked, just pass through the call directly.
    if (!currentHeapLock) {
      callback();
    } else {
      currentHeapLock.enqueuePostReleaseAction(callback);
    }
  },
};

async function importDotnetJs(resourceLoader) {
  // The dotnet.*.js file has a version or hash in its name as a form of cache-busting. This is needed
  // because it's the only part of the loading process that can't use cache:'no-cache' (because it's
  // not a 'fetch') and isn't controllable by the developer (so they can't put in their own cache-busting
  // querystring). So, to find out the exact URL we have to search the boot manifest.
  const dotnetJsResourceName = Object.keys(
    resourceLoader.bootConfig.resources.runtime,
  ).filter(n => n.startsWith("dotnet.") && n.endsWith(".js"))[0];

  const dotnetJsContentHash =
    resourceLoader.bootConfig.resources.runtime[dotnetJsResourceName];

  let src = `_framework/${dotnetJsResourceName}`;

  // Allow overriding the URI from which the dotnet.*.js file is loaded
  if (resourceLoader.startOptions.loadBootResource) {
    const resourceType = "dotnetjs";
    const customSrc = resourceLoader.startOptions.loadBootResource(
      resourceType,
      dotnetJsResourceName,
      src,
      dotnetJsContentHash,
    );
    if (typeof customSrc === "string") {
      src = customSrc;
    } else if (customSrc) {
      // Since we must load this via a import, it's only valid to supply a URI (and not a Request, say)
      throw new Error(
        `For a ${resourceType} resource, custom loaders must supply a URI string.`,
      );
    }
  }

  // GOTCHA: remove this once runtime switched to ES6
  // this is capturing the export via callback we have in CJS version of the runtime
  let cjsExportResolve = undefined;
  const cjsExport = new Promise(resolve => {
    cjsExportResolve = resolve;
  });
  globalThis.__onDotnetRuntimeLoaded = createDotnetRuntime => {
    delete globalThis.__onDotnetRuntimeLoaded;
    cjsExportResolve(createDotnetRuntime);
  };
  const absoluteSrc = new URL(src, document.baseURI).toString();
  const { default: createDotnetRuntime } = await import(
    /* webpackIgnore: true */ absoluteSrc
  );
  if (createDotnetRuntime) {
    // this runs when loaded module was ES6
    delete globalThis.__onDotnetRuntimeLoaded;
    return createDotnetRuntime;
  }
  return await cjsExport;
}

async function createEmscriptenModuleInstance(resourceLoader) {
  let runtimeReadyResolve = undefined;
  let runtimeReadyReject = undefined;
  const runtimeReady = new Promise((resolve, reject) => {
    runtimeReadyResolve = resolve;
    runtimeReadyReject = reject;
  });
  const dotnetJsBeingLoaded = importDotnetJs(resourceLoader);
  const resources = resourceLoader.bootConfig.resources;
  const moduleConfig = window["Module"] || {};
  const suppressMessages = ["DEBUGGING ENABLED"];
  const print = line =>
    suppressMessages.indexOf(line) < 0 && console.log(line);
  const printErr = line => {
    // If anything writes to stderr, treat it as a critical exception. The underlying runtime writes
    // to stderr if a truly critical problem occurs outside .NET code. Note that .NET unhandled
    // exceptions also reach this, but via a different code path - see dotNetCriticalError below.
    console.error(line);
  };
  const existingPreRun = moduleConfig.preRun || [];
  const existingPostRun = moduleConfig.postRun || [];
  moduleConfig.preloadPlugins = [];

  // Begin loading the .dll/.pdb/.wasm files, but don't block here. Let other loading processes run in parallel.
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
  const totalResources = assembliesBeingLoaded.concat(
    pdbsBeingLoaded,
    wasmBeingLoaded,
  );
  const dotnetTimeZoneResourceName = "dotnet.timezones.blat";
  let timeZoneResource;
  if (
    resourceLoader.bootConfig.resources.runtime.hasOwnProperty(
      dotnetTimeZoneResourceName,
    )
  ) {
    timeZoneResource = resourceLoader.loadResource(
      dotnetTimeZoneResourceName,
      `_framework/${dotnetTimeZoneResourceName}`,
      resourceLoader.bootConfig.resources.runtime[dotnetTimeZoneResourceName],
      "globalization",
    );
    totalResources.push(timeZoneResource);
  }
  let icuDataResource;
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
    totalResources.push(icuDataResource);
  }

  const createDotnetRuntime = await dotnetJsBeingLoaded;
  await createDotnetRuntime(api => {
    const {
      MONO: mono,
      BINDING: binding,
      Module: module,
      IMPORTS: imports,
    } = api;
    Module = module;
    BINDING = binding;
    MONO = mono;
    IMPORTS = imports;

    // Override the mechanism for fetching the main wasm file so we can connect it to our cache
    const instantiateWasm = (wasmImports, successCallback) => {
      (async () => {
        let compiledInstance;
        try {
          const dotnetWasmResource = await wasmBeingLoaded;
          compiledInstance = await compileWasmModule(
            dotnetWasmResource,
            wasmImports,
          );
        } catch (ex) {
          printErr(ex.toString());
          throw ex;
        }
        successCallback(compiledInstance);
      })();
      return []; // No exports
    };

    const onRuntimeInitialized = () => {
      if (!icuDataResource) {
        // Use invariant culture if the app does not carry icu data.
        MONO.mono_wasm_setenv("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT", "1");
      }
    };
    const preRun = () => {
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
        addResourceAsAssembly(r, r.name),
      );
      pdbsBeingLoaded.forEach(r => addResourceAsAssembly(r, r.name));
      Blazor._internal.dotNetCriticalError = message =>
        printErr(message || "(null)");
      // Wire-up callbacks for satellite assemblies. Blazor will call these as part of the application
      // startup sequence to load satellite assemblies for the application's culture.
      Blazor._internal.getSatelliteAssemblies = culturesToLoadDotNetArray => {
        const culturesToLoad = BINDING.mono_array_to_js_array(
          culturesToLoadDotNetArray,
        );
        const satelliteResources =
          resourceLoader.bootConfig.resources.satelliteResources;
        if (satelliteResources) {
          const resourcePromises = Promise.all(
            culturesToLoad
              .filter(culture => satelliteResources.hasOwnProperty(culture))
              .map(culture =>
                resourceLoader.loadResources(
                  satelliteResources[culture],
                  fileName => `_framework/${fileName}`,
                  "assembly",
                ),
              )
              .reduce((previous, next) => previous.concat(next), new Array())
              .map(async resource => (await resource.response).arrayBuffer()),
          );
          return BINDING.js_to_mono_obj(
            resourcePromises.then(resourcesToLoad => {
              if (resourcesToLoad.length) {
                Blazor._internal.readSatelliteAssemblies = () => {
                  const array = BINDING.mono_obj_array_new(
                    resourcesToLoad.length,
                  );
                  for (let i = 0; i < resourcesToLoad.length; i++) {
                    BINDING.mono_obj_array_set(
                      array,
                      i,
                      BINDING.js_typed_array_to_array(
                        new Uint8Array(resourcesToLoad[i]),
                      ),
                    );
                  }
                  return array;
                };
              }
              return resourcesToLoad.length;
            }),
          );
        }
        return BINDING.js_to_mono_obj(Promise.resolve(0));
      };
    };

    const postRun = () => {
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
      } catch {}
      MONO.mono_wasm_setenv("TZ", timeZone || "UTC");
      if (resourceLoader.bootConfig.modifiableAssemblies) {
        // Configure the app to enable hot reload in Development.
        MONO.mono_wasm_setenv(
          "DOTNET_MODIFIABLE_ASSEMBLIES",
          resourceLoader.bootConfig.modifiableAssemblies,
        );
      }

      // -1 enables debugging with logging disabled. 0 disables debugging entirely.
      MONO.mono_wasm_load_runtime(
        appBinDirName,
        0,
      );
      MONO.mono_wasm_runtime_ready();
      try {
        BINDING.bind_static_method("invalid-fqn", "");
      } catch (e) {
        // HOTFIX: until https://github.com/dotnet/runtime/pull/72275
        // this would always throw, but it will initialize runtime interop as side-effect
      }
      // makes Blazor._internal visible to [JSImport]
      IMPORTS.Blazor = { _internal: Blazor._internal };
      attachInteropInvoker();
      runtimeReadyResolve(api);
    };
    async function addResourceAsAssembly(dependency, loadAsName) {
      const runDependencyId = `blazor:${dependency.name}`;
      Module.addRunDependency(runDependencyId);
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
        MONO.mono_wasm_add_assembly(loadAsName, heapAddress, data.length);
        MONO.loaded_files.push(toAbsoluteUrl(dependency.url));
      } catch (errorInfo) {
        runtimeReadyReject(errorInfo);
        return;
      }
      Module.removeRunDependency(runDependencyId);
    }
    const dotnetModuleConfig = {
      ...moduleConfig,
      disableDotnet6Compatibility: false,
      preRun: [preRun, ...existingPreRun],
      postRun: [postRun, ...existingPostRun],
      print,
      printErr,
      instantiateWasm,
      onRuntimeInitialized,
    };
    return dotnetModuleConfig;
  });
  return await runtimeReady;
}

const anchorTagForAbsoluteUrlConversions = document.createElement("a");
function toAbsoluteUrl(possiblyRelativeUrl) {
  //return new URL(possiblyRelativeUrl).href;

  anchorTagForAbsoluteUrlConversions.href = possiblyRelativeUrl;
  return anchorTagForAbsoluteUrlConversions.href;
}
function getArrayDataPointer(array) {
  return array + 12; // First byte from here is length, then following bytes are entries
}
function bindStaticMethod(assembly, typeName, method) {
  // Fully qualified name looks like this: "[debugger-test] Math:IntAdd"
  const fqn = `[${assembly}] ${typeName}:${method}`;
  return BINDING.bind_static_method(fqn);
}


function attachInteropInvoker() {
  const dotNetDispatcherInvokeMethodHandle = bindStaticMethod(
    "Microsoft.AspNetCore.Components.WebAssembly",
    "Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime",
    "InvokeDotNet",
  );

  DotNet.attachDispatcher({
    invokeDotNetFromJS: (
      assemblyName,
      methodIdentifier,
      dotNetObjectId,
      argsJson,
    ) => {
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

async function loadTimezone(timeZoneResource) {
  const runDependencyId = "blazor:timezonedata";
  Module.addRunDependency(runDependencyId);
  const request = await timeZoneResource.response;
  const arrayBuffer = await request.arrayBuffer();
  Module["FS_createPath"]("/", "usr", true, true);
  Module["FS_createPath"]("/usr/", "share", true, true);
  Module["FS_createPath"]("/usr/share/", "zoneinfo", true, true);
  MONO.mono_wasm_load_data_archive(
    new Uint8Array(arrayBuffer),
    "/usr/share/zoneinfo/",
  );
  Module.removeRunDependency(runDependencyId);
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
  Module.addRunDependency(runDependencyId);
  const request = await icuDataResource.response;
  const array = new Uint8Array(await request.arrayBuffer());
  const offset = MONO.mono_wasm_load_bytes_into_heap(array);
  if (!MONO.mono_wasm_load_icu_data(offset)) {
    throw new Error("Error loading ICU asset.");
  }
  Module.removeRunDependency(runDependencyId);
}

async function compileWasmModule(wasmResource, imports) {
  var _a;
  const wasmResourceResponse = await wasmResource.response;
  // The instantiateStreaming spec explicitly requires the following exact MIME type (with no trailing parameters, etc.)
  // https://webassembly.github.io/spec/web-api/#dom-webassembly-instantiatestreaming
  const hasWasmContentType =
    ((_a = wasmResourceResponse.headers) === null || _a === void 0
      ? void 0
      : _a.get("content-type")) === "application/wasm";
  if (
    hasWasmContentType &&
    typeof WebAssembly.instantiateStreaming === "function"
  ) {
    // We can use streaming compilation. We know this shouldn't fail due to the content-type header being wrong,
    // as we already just checked that. So if this fails for some other reason we'll treat it as fatal.
    const streamingResult = await WebAssembly.instantiateStreaming(
      wasmResourceResponse,
      imports,
    );
    return streamingResult.instance;
  } else {
    // TODO: consider removing this. It seems really doubtful we'll ever hit this, and it would allow us to removew some LOC
    if (!hasWasmContentType) {
      // In most cases the developer should fix this. It's unusual enough that we don't mind logging a warning each time.
      console.warn(
        'WebAssembly resource does not have the expected content type "application/wasm", so falling back to slower ArrayBuffer instantiation.',
      );
    }
    // Fall back on ArrayBuffer instantiation.
    const arrayBuffer = await wasmResourceResponse.arrayBuffer();
    const arrayBufferResult = await WebAssembly.instantiate(
      arrayBuffer,
      imports,
    );
    return arrayBufferResult.instance;
  }
}

function assertHeapIsNotLocked() {
  if (currentHeapLock) {
    throw new Error("Assertion failed - heap is currently locked");
  }
}
class MonoHeapLock {
  constructor() {
    // Within a given heap lock, it's safe to cache decoded strings since the memory can't change
    this.stringCache = new Map();
  }
  // eslint-disable-next-line @typescript-eslint/ban-types
  enqueuePostReleaseAction(callback) {
    if (!this.postReleaseActions) {
      this.postReleaseActions = [];
    }
    this.postReleaseActions.push(callback);
  }
  release() {
    var _a;
    if (currentHeapLock !== this) {
      throw new Error("Trying to release a lock which isn't current");
    }
    currentHeapLock = null;
    while (
      (_a = this.postReleaseActions) === null || _a === void 0
        ? void 0
        : _a.length
    ) {
      const nextQueuedAction = this.postReleaseActions.shift();
      // It's possible that the action we invoke here might itself take a succession of heap locks,
      // but since heap locks must be released synchronously, by the time we get back to this stack
      // frame, we know the heap should no longer be locked.
      nextQueuedAction();
      assertHeapIsNotLocked();
    }
  }
}

const networkFetchCacheMode = "no-cache";
class WebAssemblyResourceLoader {
  constructor(bootConfig, startOptions) {
    this.bootConfig = bootConfig;
    this.startOptions = startOptions;
  }
  static async initAsync(bootConfig, startOptions) {
    const cache = null;
    return new WebAssemblyResourceLoader(bootConfig, startOptions);
  }
  loadResources(resources, url, resourceType) {
    return Object.keys(resources).map(name =>
      this.loadResource(name, url(name), resources[name], resourceType),
    );
  }
  loadResource(name, url, contentHash, resourceType) {
    const response = this.loadResourceWithoutCaching(name, url, contentHash, resourceType);
    return { name, url, response };
  }

  loadResourceWithoutCaching(name, url, contentHash, resourceType) {
    // Allow developers to override how the resource is loaded
    if (this.startOptions.loadBootResource) {
      const customLoadResult = this.startOptions.loadBootResource(
        resourceType,
        name,
        url,
        contentHash,
      );
      if (customLoadResult instanceof Promise) {
        // They are supplying an entire custom response, so just use that
        return customLoadResult;
      } else if (typeof customLoadResult === "string") {
        // They are supplying a custom URL, so use that with the default fetch behavior
        url = customLoadResult;
      }
    }
    // Note that if cacheBootResources was explicitly disabled, we also bypass hash checking
    // This is to give developers an easy opt-out from the entire caching/validation flow if
    // there's anything they don't like about it.
    return fetch(url, {
      cache: networkFetchCacheMode,
      integrity: this.bootConfig.cacheBootResources ? contentHash : undefined,
    });
  }
}


// CONCATENATED MODULE: ./Platform/WebAssemblyConfigLoader.ts
class WebAssemblyConfigLoader {
  static async initAsync(bootConfigResult) {
    Blazor._internal.getApplicationEnvironment = () =>
      BINDING.js_string_to_mono_string(
        bootConfigResult.applicationEnvironment,
      );

    if ((bootConfigResult.bootConfig.config || []).length > 0){
      console.error('BlazorWorker is not set up to deal with configs provided')
    }
  }
}

// CONCATENATED MODULE: ./Boot.WebAssembly.ts
let started = false;
async function boot(options) {
  if (started) {
    throw new Error("Blazor has already started.");
  }
  started = true;
  Blazor._internal.getApplyUpdateCapabilities = () =>
    DotNet.invokeMethod(
      "Microsoft.AspNetCore.Components.WebAssembly",
      "GetApplyUpdateCapabilities",
    );

    // Configure JS interop
  Blazor._internal.invokeJSFromDotNet = invokeJSFromDotNet;
  // Configure environment for execution under Mono WebAssembly with shared-memory rendering
  const platform = setPlatform(monoPlatform);

  const candidateOptions =
    options !== null && options !== void 0 ? options : {};
  // Get the custom environment setting and blazorBootJson loader if defined
  const environment = candidateOptions.environment;
  // Fetch the resources and prepare the Mono runtime
  const bootConfigPromise = BootConfigResult.initAsync(
    candidateOptions.loadBootResource,
    environment,
  );
  const bootConfigResult = await bootConfigPromise;

  if (bootConfigResult.bootConfig.resources.libraryInitializers) {
    console.error('not prepared for import initializers (e.g. libraryInitializers in blazor.boot.json)')
  }

  const [resourceLoader] = await Promise.all([
    WebAssemblyResourceLoader.initAsync(
      bootConfigResult.bootConfig,
      candidateOptions || {},
    ),
    WebAssemblyConfigLoader.initAsync(bootConfigResult),
  ]);

  try {
    await platform.start(resourceLoader);
  } catch (ex) {
    throw new Error(`Failed to start platform. Reason: ${ex}`);
  }
  // Start up the application
  platform.callEntryPoint(resourceLoader.bootConfig.entryAssembly);
}

function invokeJSFromDotNet(callInfo, _arg0, _arg1, _arg2) {
  const functionIdentifier = monoPlatform.readStringField(callInfo, 0);
  const _resultType = monoPlatform.readInt32Field(callInfo, 4);
  const marshalledCallArgsJson = monoPlatform.readStringField(callInfo, 8);
  const targetInstanceId = monoPlatform.readUint64Field(callInfo, 20);

  const _resultJson = DotNet.jsCallDispatcher.invokeJSFromDotNet(
    functionIdentifier,
    marshalledCallArgsJson,
    targetInstanceId,
  );

  return 0;
}

Blazor.start = boot;
