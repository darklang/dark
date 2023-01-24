(() => {
  "use strict";
  var __webpack_exports__ = {};

  var DotNet;
  (function (DotNet) {
    window.DotNet = DotNet; // Ensure reachable from anywhere
    const jsonRevivers = [];
    const byteArraysToBeRevived = new Map();
    const pendingDotNetToJSStreams = new Map();
    const jsObjectIdKey = "__jsObjectId";
    const dotNetObjectRefKey = "__dotNetObject";
    const byteArrayRefKey = "__byte[]";
    const dotNetStreamRefKey = "__dotNetStream";
    const jsStreamReferenceLengthKey = "__jsStreamReferenceLength";
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
    const pendingAsyncCalls = {};
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
    let nextAsyncCallId = 1; // Start at 1 because zero signals "no response needed"
    let nextJsObjectId = 1; // Start at 1 because zero is reserved for "window"
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
     * Adds a JSON reviver callback that will be used when parsing arguments received from .NET.
     * @param reviver The reviver to add.
     */
    function attachReviver(reviver) {
      jsonRevivers.push(reviver);
    }
    DotNet.attachReviver = attachReviver;
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
    /**
     * Invokes the specified .NET public method asynchronously.
     *
     * @param assemblyName The short name (without key/version or .dll extension) of the .NET assembly containing the method.
     * @param methodIdentifier The identifier of the method to invoke. The method must have a [JSInvokable] attribute specifying this identifier.
     * @param args Arguments to pass to the method, each of which must be JSON-serializable.
     * @returns A promise representing the result of the operation.
     */
    function invokeMethodAsync(assemblyName, methodIdentifier, ...args) {
      return invokePossibleInstanceMethodAsync(
        assemblyName,
        methodIdentifier,
        null,
        args,
      );
    }
    DotNet.invokeMethodAsync = invokeMethodAsync;
    /**
     * Creates a JavaScript object reference that can be passed to .NET via interop calls.
     *
     * @param jsObject The JavaScript Object used to create the JavaScript object reference.
     * @returns The JavaScript object reference (this will be the same instance as the given object).
     * @throws Error if the given value is not an Object.
     */
    function createJSObjectReference(jsObject) {
      if (jsObject && typeof jsObject === "object") {
        cachedJSObjectsById[nextJsObjectId] = new JSObject(jsObject);
        const result = {
          [jsObjectIdKey]: nextJsObjectId,
        };
        nextJsObjectId++;
        return result;
      }
      throw new Error(
        `Cannot create a JSObjectReference from the value '${jsObject}'.`,
      );
    }
    DotNet.createJSObjectReference = createJSObjectReference;
    /**
     * Creates a JavaScript data reference that can be passed to .NET via interop calls.
     *
     * @param streamReference The ArrayBufferView or Blob used to create the JavaScript stream reference.
     * @returns The JavaScript data reference (this will be the same instance as the given object).
     * @throws Error if the given value is not an Object or doesn't have a valid byteLength.
     */
    function createJSStreamReference(streamReference) {
      let length = -1;
      // If we're given a raw Array Buffer, we interpret it as a `Uint8Array` as
      // ArrayBuffers' aren't directly readable.
      if (streamReference instanceof ArrayBuffer) {
        streamReference = new Uint8Array(streamReference);
      }
      if (streamReference instanceof Blob) {
        length = streamReference.size;
      } else if (streamReference.buffer instanceof ArrayBuffer) {
        if (streamReference.byteLength === undefined) {
          throw new Error(
            `Cannot create a JSStreamReference from the value '${streamReference}' as it doesn't have a byteLength.`,
          );
        }
        length = streamReference.byteLength;
      } else {
        throw new Error("Supplied value is not a typed array or blob.");
      }
      const result = {
        [jsStreamReferenceLengthKey]: length,
      };
      try {
        const jsObjectReference = createJSObjectReference(streamReference);
        result[jsObjectIdKey] = jsObjectReference[jsObjectIdKey];
      } catch (error) {
        throw new Error(
          `Cannot create a JSStreamReference from the value '${streamReference}'.`,
        );
      }
      return result;
    }
    DotNet.createJSStreamReference = createJSStreamReference;
    /**
     * Disposes the given JavaScript object reference.
     *
     * @param jsObjectReference The JavaScript Object reference.
     */
    function disposeJSObjectReference(jsObjectReference) {
      const id = jsObjectReference && jsObjectReference[jsObjectIdKey];
      if (typeof id === "number") {
        disposeJSObjectReferenceById(id);
      }
    }
    DotNet.disposeJSObjectReference = disposeJSObjectReference;
    /**
     * Parses the given JSON string using revivers to restore args passed from .NET to JS.
     *
     * @param json The JSON stirng to parse.
     */
    function parseJsonWithRevivers(json) {
      return json
        ? JSON.parse(json, (key, initialValue) => {
            // Invoke each reviver in order, passing the output from the previous reviver,
            // so that each one gets a chance to transform the value
            return jsonRevivers.reduce(
              (latestValue, reviver) => reviver(key, latestValue),
              initialValue,
            );
          })
        : null;
    }
    function invokePossibleInstanceMethod(
      assemblyName,
      methodIdentifier,
      dotNetObjectId,
      args,
    ) {
      const dispatcher = getRequiredDispatcher();
      if (dispatcher.invokeDotNetFromJS) {
        const argsJson = stringifyArgs(args);
        const resultJson = dispatcher.invokeDotNetFromJS(
          assemblyName,
          methodIdentifier,
          dotNetObjectId,
          argsJson,
        );
        return resultJson ? parseJsonWithRevivers(resultJson) : null;
      }
      throw new Error(
        "The current dispatcher does not support synchronous calls from JS to .NET. Use invokeMethodAsync instead.",
      );
    }
    function invokePossibleInstanceMethodAsync(
      assemblyName,
      methodIdentifier,
      dotNetObjectId,
      args,
    ) {
      if (assemblyName && dotNetObjectId) {
        throw new Error(
          `For instance method calls, assemblyName should be null. Received '${assemblyName}'.`,
        );
      }
      const asyncCallId = nextAsyncCallId++;
      const resultPromise = new Promise((resolve, reject) => {
        pendingAsyncCalls[asyncCallId] = { resolve, reject };
      });
      try {
        const argsJson = stringifyArgs(args);
        getRequiredDispatcher().beginInvokeDotNetFromJS(
          asyncCallId,
          assemblyName,
          methodIdentifier,
          dotNetObjectId,
          argsJson,
        );
      } catch (ex) {
        // Synchronous failure
        completePendingCall(asyncCallId, false, ex);
      }
      return resultPromise;
    }
    function getRequiredDispatcher() {
      if (dotNetDispatcher !== null) {
        return dotNetDispatcher;
      }
      throw new Error("No .NET call dispatcher has been set.");
    }
    function completePendingCall(asyncCallId, success, resultOrError) {
      if (!pendingAsyncCalls.hasOwnProperty(asyncCallId)) {
        throw new Error(
          `There is no pending async call with ID ${asyncCallId}.`,
        );
      }
      const asyncCall = pendingAsyncCalls[asyncCallId];
      delete pendingAsyncCalls[asyncCallId];
      if (success) {
        asyncCall.resolve(resultOrError);
      } else {
        asyncCall.reject(resultOrError);
      }
    }
    /**
     * Represents the type of result expected from a JS interop call.
     */
    // eslint-disable-next-line no-shadow
    let JSCallResultType;
    (function (JSCallResultType) {
      JSCallResultType[(JSCallResultType["Default"] = 0)] = "Default";
      JSCallResultType[(JSCallResultType["JSObjectReference"] = 1)] =
        "JSObjectReference";
      JSCallResultType[(JSCallResultType["JSStreamReference"] = 2)] =
        "JSStreamReference";
      JSCallResultType[(JSCallResultType["JSVoidResult"] = 3)] = "JSVoidResult";
    })(
      (JSCallResultType =
        DotNet.JSCallResultType || (DotNet.JSCallResultType = {})),
    );
    /**
     * Receives incoming calls from .NET and dispatches them to JavaScript.
     */
    DotNet.jsCallDispatcher = {
      /**
       * Finds the JavaScript function matching the specified identifier.
       *
       * @param identifier Identifies the globally-reachable function to be returned.
       * @param targetInstanceId The instance ID of the target JS object.
       * @returns A Function instance.
       */
      findJSFunction,
      /**
       * Disposes the JavaScript object reference with the specified object ID.
       *
       * @param id The ID of the JavaScript object reference.
       */
      disposeJSObjectReferenceById,
      /**
       * Invokes the specified synchronous JavaScript function.
       *
       * @param identifier Identifies the globally-reachable function to invoke.
       * @param argsJson JSON representation of arguments to be passed to the function.
       * @param resultType The type of result expected from the JS interop call.
       * @param targetInstanceId The instance ID of the target JS object.
       * @returns JSON representation of the invocation result.
       */
      invokeJSFromDotNet: (
        identifier,
        argsJson,
        resultType,
        targetInstanceId,
      ) => {
        const returnValue = findJSFunction(identifier, targetInstanceId).apply(
          null,
          parseJsonWithRevivers(argsJson),
        );
        const result = createJSCallResult(returnValue, resultType);
        return result === null || result === undefined
          ? null
          : stringifyArgs(result);
      },
      /**
       * Invokes the specified synchronous or asynchronous JavaScript function.
       *
       * @param asyncHandle A value identifying the asynchronous operation. This value will be passed back in a later call to endInvokeJSFromDotNet.
       * @param identifier Identifies the globally-reachable function to invoke.
       * @param argsJson JSON representation of arguments to be passed to the function.
       * @param resultType The type of result expected from the JS interop call.
       * @param targetInstanceId The ID of the target JS object instance.
       */
      beginInvokeJSFromDotNet: (
        asyncHandle,
        identifier,
        argsJson,
        resultType,
        targetInstanceId,
      ) => {
        // Coerce synchronous functions into async ones, plus treat
        // synchronous exceptions the same as async ones
        const promise = new Promise(resolve => {
          const synchronousResultOrPromise = findJSFunction(
            identifier,
            targetInstanceId,
          ).apply(null, parseJsonWithRevivers(argsJson));
          resolve(synchronousResultOrPromise);
        });
        // We only listen for a result if the caller wants to be notified about it
        if (asyncHandle) {
          // On completion, dispatch result back to .NET
          // Not using "await" because it codegens a lot of boilerplate
          promise
            .then(result =>
              stringifyArgs([
                asyncHandle,
                true,
                createJSCallResult(result, resultType),
              ]),
            )
            .then(
              result =>
                getRequiredDispatcher().endInvokeJSFromDotNet(
                  asyncHandle,
                  true,
                  result,
                ),
              error =>
                getRequiredDispatcher().endInvokeJSFromDotNet(
                  asyncHandle,
                  false,
                  JSON.stringify([asyncHandle, false, formatError(error)]),
                ),
            );
        }
      },
      /**
       * Receives notification that an async call from JS to .NET has completed.
       * @param asyncCallId The identifier supplied in an earlier call to beginInvokeDotNetFromJS.
       * @param success A flag to indicate whether the operation completed successfully.
       * @param resultJsonOrExceptionMessage Either the operation result as JSON, or an error message.
       */
      endInvokeDotNetFromJS: (
        asyncCallId,
        success,
        resultJsonOrExceptionMessage,
      ) => {
        const resultOrError = success
          ? parseJsonWithRevivers(resultJsonOrExceptionMessage)
          : new Error(resultJsonOrExceptionMessage);
        completePendingCall(parseInt(asyncCallId, 10), success, resultOrError);
      },
      /**
       * Receives notification that a byte array is being transferred from .NET to JS.
       * @param id The identifier for the byte array used during revival.
       * @param data The byte array being transferred for eventual revival.
       */
      receiveByteArray: (id, data) => {
        byteArraysToBeRevived.set(id, data);
      },
      /**
       * Supplies a stream of data being sent from .NET.
       *
       * @param streamId The identifier previously passed to JSRuntime's BeginTransmittingStream in .NET code
       * @param stream The stream data.
       */
      supplyDotNetStream: (streamId, stream) => {
        if (pendingDotNetToJSStreams.has(streamId)) {
          // The receiver is already waiting, so we can resolve the promise now and stop tracking this
          const pendingStream = pendingDotNetToJSStreams.get(streamId);
          pendingDotNetToJSStreams.delete(streamId);
          pendingStream.resolve(stream);
        } else {
          // The receiver hasn't started waiting yet, so track a pre-completed entry it can attach to later
          const pendingStream = new PendingStream();
          pendingStream.resolve(stream);
          pendingDotNetToJSStreams.set(streamId, pendingStream);
        }
      },
    };
    function formatError(error) {
      if (error instanceof Error) {
        return `${error.message}\n${error.stack}`;
      }
      return error ? error.toString() : "null";
    }
    function findJSFunction(identifier, targetInstanceId) {
      const targetInstance = cachedJSObjectsById[targetInstanceId];
      if (targetInstance) {
        return targetInstance.findFunction(identifier);
      }
      throw new Error(
        `JS object instance with ID ${targetInstanceId} does not exist (has it been disposed?).`,
      );
    }
    function disposeJSObjectReferenceById(id) {
      delete cachedJSObjectsById[id];
    }
    class DotNetObject {
      // eslint-disable-next-line no-empty-function
      constructor(_id) {
        this._id = _id;
      }
      invokeMethod(methodIdentifier, ...args) {
        return invokePossibleInstanceMethod(
          null,
          methodIdentifier,
          this._id,
          args,
        );
      }
      invokeMethodAsync(methodIdentifier, ...args) {
        return invokePossibleInstanceMethodAsync(
          null,
          methodIdentifier,
          this._id,
          args,
        );
      }
      dispose() {
        const promise = invokePossibleInstanceMethodAsync(
          null,
          "__Dispose",
          this._id,
          null,
        );
        promise.catch(error => console.error(error));
      }
      serializeAsArg() {
        return { __dotNetObject: this._id };
      }
    }
    DotNet.DotNetObject = DotNetObject;
    attachReviver(function reviveReference(key, value) {
      if (value && typeof value === "object") {
        if (value.hasOwnProperty(dotNetObjectRefKey)) {
          return new DotNetObject(value[dotNetObjectRefKey]);
        } else if (value.hasOwnProperty(jsObjectIdKey)) {
          const id = value[jsObjectIdKey];
          const jsObject = cachedJSObjectsById[id];
          if (jsObject) {
            return jsObject.getWrappedObject();
          }
          throw new Error(
            `JS object instance with Id '${id}' does not exist. It may have been disposed.`,
          );
        } else if (value.hasOwnProperty(byteArrayRefKey)) {
          const index = value[byteArrayRefKey];
          const byteArray = byteArraysToBeRevived.get(index);
          if (byteArray === undefined) {
            throw new Error(`Byte array index '${index}' does not exist.`);
          }
          byteArraysToBeRevived.delete(index);
          return byteArray;
        } else if (value.hasOwnProperty(dotNetStreamRefKey)) {
          return new DotNetStream(value[dotNetStreamRefKey]);
        }
      }
      // Unrecognized - let another reviver handle it
      return value;
    });
    class DotNetStream {
      constructor(streamId) {
        // This constructor runs when we're JSON-deserializing some value from the .NET side.
        // At this point we might already have started receiving the stream, or maybe it will come later.
        // We have to handle both possible orderings, but we can count on it coming eventually because
        // it's not something the developer gets to control, and it would be an error if it doesn't.
        if (pendingDotNetToJSStreams.has(streamId)) {
          // We've already started receiving the stream, so no longer need to track it as pending
          this._streamPromise =
            pendingDotNetToJSStreams.get(streamId).streamPromise;
          pendingDotNetToJSStreams.delete(streamId);
        } else {
          // We haven't started receiving it yet, so add an entry to track it as pending
          const pendingStream = new PendingStream();
          pendingDotNetToJSStreams.set(streamId, pendingStream);
          this._streamPromise = pendingStream.streamPromise;
        }
      }
      /**
       * Supplies a readable stream of data being sent from .NET.
       */
      stream() {
        return this._streamPromise;
      }
      /**
       * Supplies a array buffer of data being sent from .NET.
       * Note there is a JavaScript limit on the size of the ArrayBuffer equal to approximately 2GB.
       */
      async arrayBuffer() {
        return new Response(await this.stream()).arrayBuffer();
      }
    }
    class PendingStream {
      constructor() {
        this.streamPromise = new Promise((resolve, reject) => {
          this.resolve = resolve;
          this.reject = reject;
        });
      }
    }
    function createJSCallResult(returnValue, resultType) {
      switch (resultType) {
        case JSCallResultType.Default:
          return returnValue;
        case JSCallResultType.JSObjectReference:
          return createJSObjectReference(returnValue);
        case JSCallResultType.JSStreamReference:
          return createJSStreamReference(returnValue);
        case JSCallResultType.JSVoidResult:
          return null;
        default:
          throw new Error(`Invalid JS call result type '${resultType}'.`);
      }
    }
    let nextByteArrayIndex = 0;
    function stringifyArgs(args) {
      nextByteArrayIndex = 0;
      return JSON.stringify(args, argReplacer);
    }
    function argReplacer(key, value) {
      if (value instanceof DotNetObject) {
        return value.serializeAsArg();
      } else if (value instanceof Uint8Array) {
        dotNetDispatcher.sendByteArray(nextByteArrayIndex, value);
        const jsonValue = { [byteArrayRefKey]: nextByteArrayIndex };
        nextByteArrayIndex++;
        return jsonValue;
      }
      return value;
    }
  })(DotNet || (DotNet = {})); // CONCATENATED MODULE: ./Rendering/Renderer.ts
  //# sourceMappingURL=Microsoft.JSInterop.js.map
  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  const browserRenderers = {};
  let shouldResetScrollAfterNextBatch = false;
  function renderBatch(browserRendererId, batch) {
    const browserRenderer = browserRenderers[browserRendererId];
    if (!browserRenderer) {
      throw new Error(
        `There is no browser renderer with ID ${browserRendererId}.`,
      );
    }
    const arrayRangeReader = batch.arrayRangeReader;
    const updatedComponentsRange = batch.updatedComponents();
    const updatedComponentsValues = arrayRangeReader.values(
      updatedComponentsRange,
    );
    const updatedComponentsLength = arrayRangeReader.count(
      updatedComponentsRange,
    );
    const referenceFrames = batch.referenceFrames();
    const referenceFramesValues = arrayRangeReader.values(referenceFrames);
    const diffReader = batch.diffReader;
    for (let i = 0; i < updatedComponentsLength; i++) {
      const diff = batch.updatedComponentsEntry(updatedComponentsValues, i);
      const componentId = diffReader.componentId(diff);
      const edits = diffReader.edits(diff);
      browserRenderer.updateComponent(
        batch,
        componentId,
        edits,
        referenceFramesValues,
      );
    }
    const disposedComponentIdsRange = batch.disposedComponentIds();
    const disposedComponentIdsValues = arrayRangeReader.values(
      disposedComponentIdsRange,
    );
    const disposedComponentIdsLength = arrayRangeReader.count(
      disposedComponentIdsRange,
    );
    for (let i = 0; i < disposedComponentIdsLength; i++) {
      const componentId = batch.disposedComponentIdsEntry(
        disposedComponentIdsValues,
        i,
      );
      browserRenderer.disposeComponent(componentId);
    }
    const disposedEventHandlerIdsRange = batch.disposedEventHandlerIds();
    const disposedEventHandlerIdsValues = arrayRangeReader.values(
      disposedEventHandlerIdsRange,
    );
    const disposedEventHandlerIdsLength = arrayRangeReader.count(
      disposedEventHandlerIdsRange,
    );
    for (let i = 0; i < disposedEventHandlerIdsLength; i++) {
      const eventHandlerId = batch.disposedEventHandlerIdsEntry(
        disposedEventHandlerIdsValues,
        i,
      );
      browserRenderer.disposeEventHandler(eventHandlerId);
    }
    resetScrollIfNeeded();
  }
  function resetScrollAfterNextBatch() {
    shouldResetScrollAfterNextBatch = true;
  }
  function resetScrollIfNeeded() {
    if (shouldResetScrollAfterNextBatch) {
      shouldResetScrollAfterNextBatch = false;
      // This assumes the scroller is on the window itself. There isn't a general way to know
      // if some other element is playing the role of the primary scroll region.
      window.scrollTo && window.scrollTo(0, 0);
    }
  } // CONCATENATED MODULE: ./Services/NavigationManager.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  let hasEnabledNavigationInterception = false;
  let hasRegisteredNavigationEventListeners = false;
  let hasLocationChangingEventListeners = false;
  let currentHistoryIndex = 0;
  let currentLocationChangingCallId = 0;
  // Will be initialized once someone registers
  let notifyLocationChangedCallback = null;
  let notifyLocationChangingCallback = null;
  let popStateCallback = onBrowserInitiatedPopState;
  let resolveCurrentNavigation = null;
  // These are the functions we're making available for invocation from .NET
  const internalFunctions = {
    listenForNavigationEvents,
    enableNavigationInterception,
    setHasLocationChangingListeners,
    endLocationChanging,
    navigateTo: navigateToFromDotNet,
    getBaseURI: () => document.baseURI,
    getLocationHref: () => location.href,
  };
  function listenForNavigationEvents(
    locationChangedCallback,
    locationChangingCallback,
  ) {
    var _a, _b;
    notifyLocationChangedCallback = locationChangedCallback;
    notifyLocationChangingCallback = locationChangingCallback;
    if (hasRegisteredNavigationEventListeners) {
      return;
    }
    hasRegisteredNavigationEventListeners = true;
    window.addEventListener("popstate", onPopState);
    currentHistoryIndex =
      (_b =
        (_a = history.state) === null || _a === void 0 ? void 0 : _a._index) !==
        null && _b !== void 0
        ? _b
        : 0;
  }
  function enableNavigationInterception() {
    hasEnabledNavigationInterception = true;
  }
  function setHasLocationChangingListeners(hasListeners) {
    hasLocationChangingEventListeners = hasListeners;
  }
  function attachToEventDelegator(eventDelegator) {
    // We need to respond to clicks on <a> elements *after* the EventDelegator has finished
    // running its simulated bubbling process so that we can respect any preventDefault requests.
    // So instead of registering our own native event, register using the EventDelegator.
    eventDelegator.notifyAfterClick(event => {
      if (!hasEnabledNavigationInterception) {
        return;
      }
      if (event.button !== 0 || eventHasSpecialKey(event)) {
        // Don't stop ctrl/meta-click (etc) from opening links in new tabs/windows
        return;
      }
      if (event.defaultPrevented) {
        return;
      }
      // Intercept clicks on all <a> elements where the href is within the <base href> URI space
      // We must explicitly check if it has an 'href' attribute, because if it doesn't, the result might be null or an empty string depending on the browser
      const anchorTarget = findAnchorTarget(event);
      if (anchorTarget && canProcessAnchor(anchorTarget)) {
        const href = anchorTarget.getAttribute("href");
        const absoluteHref = toAbsoluteUri(href);
        if (isWithinBaseUriSpace(absoluteHref)) {
          event.preventDefault();
          performInternalNavigation(
            absoluteHref,
            /* interceptedLink */ true,
            /* replace */ false,
          );
        }
      }
    });
  }
  function navigateTo(
    uri,
    forceLoadOrOptions,
    replaceIfUsingOldOverload = false,
  ) {
    // Normalize the parameters to the newer overload (i.e., using NavigationOptions)
    const options =
      forceLoadOrOptions instanceof Object
        ? forceLoadOrOptions
        : {
            forceLoad: forceLoadOrOptions,
            replaceHistoryEntry: replaceIfUsingOldOverload,
          };
    navigateToCore(uri, options);
  }
  function navigateToFromDotNet(uri, options) {
    // The location changing callback is called from .NET for programmatic navigations originating from .NET.
    // In this case, we shouldn't invoke the callback again from the JS side.
    navigateToCore(uri, options, /* skipLocationChangingCallback */ true);
  }
  function navigateToCore(uri, options, skipLocationChangingCallback = false) {
    const absoluteUri = toAbsoluteUri(uri);
    if (!options.forceLoad && isWithinBaseUriSpace(absoluteUri)) {
      performInternalNavigation(
        absoluteUri,
        false,
        options.replaceHistoryEntry,
        options.historyEntryState,
        skipLocationChangingCallback,
      );
    } else {
      // For external navigation, we work in terms of the originally-supplied uri string,
      // not the computed absoluteUri. This is in case there are some special URI formats
      // we're unable to translate into absolute URIs.
      performExternalNavigation(uri, options.replaceHistoryEntry);
    }
  }
  function performExternalNavigation(uri, replace) {
    if (location.href === uri) {
      // If you're already on this URL, you can't append another copy of it to the history stack,
      // so we can ignore the 'replace' flag. However, reloading the same URL you're already on
      // requires special handling to avoid triggering browser-specific behavior issues.
      // For details about what this fixes and why, see https://github.com/dotnet/aspnetcore/pull/10839
      const temporaryUri = uri + "?";
      history.replaceState(null, "", temporaryUri);
      location.replace(uri);
    } else if (replace) {
      location.replace(uri);
    } else {
      location.href = uri;
    }
  }
  async function performInternalNavigation(
    absoluteInternalHref,
    interceptedLink,
    replace,
    state = undefined,
    skipLocationChangingCallback = false,
  ) {
    ignorePendingNavigation();
    if (!skipLocationChangingCallback && hasLocationChangingEventListeners) {
      const shouldContinueNavigation = await notifyLocationChanging(
        absoluteInternalHref,
        state,
        interceptedLink,
      );
      if (!shouldContinueNavigation) {
        return;
      }
    }
    // Since this was *not* triggered by a back/forward gesture (that goes through a different
    // code path starting with a popstate event), we don't want to preserve the current scroll
    // position, so reset it.
    // To avoid ugly flickering effects, we don't want to change the scroll position until
    // we render the new page. As a best approximation, wait until the next batch.
    resetScrollAfterNextBatch();
    if (!replace) {
      currentHistoryIndex++;
      history.pushState(
        {
          userState: state,
          _index: currentHistoryIndex,
        },
        /* ignored title */ "",
        absoluteInternalHref,
      );
    } else {
      history.replaceState(
        {
          userState: state,
          _index: currentHistoryIndex,
        },
        /* ignored title */ "",
        absoluteInternalHref,
      );
    }
    await notifyLocationChanged(interceptedLink);
  }
  function navigateHistoryWithoutPopStateCallback(delta) {
    return new Promise(resolve => {
      const oldPopStateCallback = popStateCallback;
      popStateCallback = () => {
        popStateCallback = oldPopStateCallback;
        resolve();
      };
      history.go(delta);
    });
  }
  function ignorePendingNavigation() {
    if (resolveCurrentNavigation) {
      resolveCurrentNavigation(false);
      resolveCurrentNavigation = null;
    }
  }
  function notifyLocationChanging(uri, state, intercepted) {
    return new Promise(resolve => {
      ignorePendingNavigation();
      if (!notifyLocationChangingCallback) {
        resolve(false);
        return;
      }
      currentLocationChangingCallId++;
      resolveCurrentNavigation = resolve;
      notifyLocationChangingCallback(
        currentLocationChangingCallId,
        uri,
        state,
        intercepted,
      );
    });
  }
  function endLocationChanging(callId, shouldContinueNavigation) {
    if (resolveCurrentNavigation && callId === currentLocationChangingCallId) {
      resolveCurrentNavigation(shouldContinueNavigation);
      resolveCurrentNavigation = null;
    }
  }
  async function onBrowserInitiatedPopState(state) {
    var _a, _b, _c;
    ignorePendingNavigation();
    if (hasLocationChangingEventListeners) {
      const index =
        (_b =
          (_a = state.state) === null || _a === void 0 ? void 0 : _a._index) !==
          null && _b !== void 0
          ? _b
          : 0;
      const userState =
        (_c = state.state) === null || _c === void 0 ? void 0 : _c.userState;
      const delta = index - currentHistoryIndex;
      const uri = location.href;
      // Temporarily revert the navigation until we confirm if the navigation should continue.
      await navigateHistoryWithoutPopStateCallback(-delta);
      const shouldContinueNavigation = await notifyLocationChanging(
        uri,
        userState,
        false,
      );
      if (!shouldContinueNavigation) {
        return;
      }
      await navigateHistoryWithoutPopStateCallback(delta);
    }
    await notifyLocationChanged(false);
  }
  async function notifyLocationChanged(interceptedLink) {
    var _a;
    if (notifyLocationChangedCallback) {
      await notifyLocationChangedCallback(
        location.href,
        (_a = history.state) === null || _a === void 0 ? void 0 : _a.userState,
        interceptedLink,
      );
    }
  }
  async function onPopState(state) {
    var _a, _b;
    if (popStateCallback) {
      await popStateCallback(state);
    }
    currentHistoryIndex =
      (_b =
        (_a = history.state) === null || _a === void 0 ? void 0 : _a._index) !==
        null && _b !== void 0
        ? _b
        : 0;
  }
  let testAnchor;
  function toAbsoluteUri(relativeUri) {
    testAnchor = testAnchor || document.createElement("a");
    testAnchor.href = relativeUri;
    return testAnchor.href;
  }
  function findAnchorTarget(event) {
    // _blazorDisableComposedPath is a temporary escape hatch in case any problems are discovered
    // in this logic. It can be removed in a later release, and should not be considered supported API.
    const path =
      !window["_blazorDisableComposedPath"] &&
      event.composedPath &&
      event.composedPath();
    if (path) {
      // This logic works with events that target elements within a shadow root,
      // as long as the shadow mode is 'open'. For closed shadows, we can't possibly
      // know what internal element was clicked.
      for (let i = 0; i < path.length; i++) {
        const candidate = path[i];
        if (candidate instanceof Element && candidate.tagName === "A") {
          return candidate;
        }
      }
      return null;
    } else {
      // Since we're adding use of composedPath in a patch, retain compatibility with any
      // legacy browsers that don't support it by falling back on the older logic, even
      // though it won't work properly with ShadowDOM. This can be removed in the next
      // major release.
      return findClosestAnchorAncestorLegacy(event.target, "A");
    }
  }
  function findClosestAnchorAncestorLegacy(element, tagName) {
    return !element
      ? null
      : element.tagName === tagName
      ? element
      : findClosestAnchorAncestorLegacy(element.parentElement, tagName);
  }
  function isWithinBaseUriSpace(href) {
    const baseUriWithoutTrailingSlash = toBaseUriWithoutTrailingSlash(
      document.baseURI,
    );
    const nextChar = href.charAt(baseUriWithoutTrailingSlash.length);
    return (
      href.startsWith(baseUriWithoutTrailingSlash) &&
      (nextChar === "" ||
        nextChar === "/" ||
        nextChar === "?" ||
        nextChar === "#")
    );
  }
  function toBaseUriWithoutTrailingSlash(baseUri) {
    return baseUri.substring(0, baseUri.lastIndexOf("/"));
  }
  function eventHasSpecialKey(event) {
    return event.ctrlKey || event.shiftKey || event.altKey || event.metaKey;
  }
  function canProcessAnchor(anchorTarget) {
    const targetAttributeValue = anchorTarget.getAttribute("target");
    const opensInSameFrame =
      !targetAttributeValue || targetAttributeValue === "_self";
    return (
      opensInSameFrame &&
      anchorTarget.hasAttribute("href") &&
      !anchorTarget.hasAttribute("download")
    );
  } // CONCATENATED MODULE: ./DomWrapper.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  const domFunctions = {
    focus: DomWrapper_focus,
    focusBySelector,
  };
  function DomWrapper_focus(element, preventScroll) {
    if (element instanceof HTMLElement) {
      element.focus({ preventScroll });
    } else if (element instanceof SVGElement) {
      if (element.hasAttribute("tabindex")) {
        element.focus({ preventScroll });
      } else {
        throw new Error(
          "Unable to focus an SVG element that does not have a tabindex.",
        );
      }
    } else {
      throw new Error("Unable to focus an invalid element.");
    }
  }
  function focusBySelector(selector) {
    const element = document.querySelector(selector);
    if (element) {
      // If no explicit tabindex is defined, mark it as programmatically-focusable.
      // This does actually add a new HTML attribute, but it shouldn't interfere with
      // diffing because diffing only deals with the attributes you have in your code.
      if (!element.hasAttribute("tabindex")) {
        element.tabIndex = -1;
      }
      element.focus();
    }
  } // CONCATENATED MODULE: ./Virtualize.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  const Virtualize = {
    init,
    dispose,
  };
  const observersByDotNetId = {};
  function findClosestScrollContainer(element) {
    // If we recurse up as far as body or the document root, return null so that the
    // IntersectionObserver observes intersection with the top-level scroll viewport
    // instead of the with body/documentElement which can be arbitrarily tall.
    // See https://github.com/dotnet/aspnetcore/issues/37659 for more about what this fixes.
    if (
      !element ||
      element === document.body ||
      element === document.documentElement
    ) {
      return null;
    }
    const style = getComputedStyle(element);
    if (style.overflowY !== "visible") {
      return element;
    }
    return findClosestScrollContainer(element.parentElement);
  }
  function init(dotNetHelper, spacerBefore, spacerAfter, rootMargin = 50) {
    // Overflow anchoring can cause an ongoing scroll loop, because when we resize the spacers, the browser
    // would update the scroll position to compensate. Then the spacer would remain visible and we'd keep on
    // trying to resize it.
    const scrollContainer = findClosestScrollContainer(spacerBefore);
    (scrollContainer || document.documentElement).style.overflowAnchor = "none";
    const rangeBetweenSpacers = document.createRange();
    if (isValidTableElement(spacerAfter.parentElement)) {
      spacerBefore.style.display = "table-row";
      spacerAfter.style.display = "table-row";
    }
    const intersectionObserver = new IntersectionObserver(
      intersectionCallback,
      {
        root: scrollContainer,
        rootMargin: `${rootMargin}px`,
      },
    );
    intersectionObserver.observe(spacerBefore);
    intersectionObserver.observe(spacerAfter);
    const mutationObserverBefore = createSpacerMutationObserver(spacerBefore);
    const mutationObserverAfter = createSpacerMutationObserver(spacerAfter);
    observersByDotNetId[dotNetHelper._id] = {
      intersectionObserver,
      mutationObserverBefore,
      mutationObserverAfter,
    };
    function createSpacerMutationObserver(spacer) {
      // Without the use of thresholds, IntersectionObserver only detects binary changes in visibility,
      // so if a spacer gets resized but remains visible, no additional callbacks will occur. By unobserving
      // and reobserving spacers when they get resized, the intersection callback will re-run if they remain visible.
      const observerOptions = { attributes: true };
      const mutationObserver = new MutationObserver((mutations, observer) => {
        if (isValidTableElement(spacer.parentElement)) {
          observer.disconnect();
          spacer.style.display = "table-row";
          observer.observe(spacer, observerOptions);
        }
        intersectionObserver.unobserve(spacer);
        intersectionObserver.observe(spacer);
      });
      mutationObserver.observe(spacer, observerOptions);
      return mutationObserver;
    }
    function intersectionCallback(entries) {
      entries.forEach(entry => {
        var _a;
        if (!entry.isIntersecting) {
          return;
        }
        // To compute the ItemSize, work out the separation between the two spacers. We can't just measure an individual element
        // because each conceptual item could be made from multiple elements. Using getBoundingClientRect allows for the size to be
        // a fractional value. It's important not to add or subtract any such fractional values (e.g., to subtract the 'top' of
        // one item from the 'bottom' of another to get the distance between them) because floating point errors would cause
        // scrolling glitches.
        rangeBetweenSpacers.setStartAfter(spacerBefore);
        rangeBetweenSpacers.setEndBefore(spacerAfter);
        const spacerSeparation =
          rangeBetweenSpacers.getBoundingClientRect().height;
        const containerSize =
          (_a = entry.rootBounds) === null || _a === void 0
            ? void 0
            : _a.height;
        if (entry.target === spacerBefore) {
          dotNetHelper.invokeMethodAsync(
            "OnSpacerBeforeVisible",
            entry.intersectionRect.top - entry.boundingClientRect.top,
            spacerSeparation,
            containerSize,
          );
        } else if (
          entry.target === spacerAfter &&
          spacerAfter.offsetHeight > 0
        ) {
          // When we first start up, both the "before" and "after" spacers will be visible, but it's only relevant to raise a
          // single event to load the initial data. To avoid raising two events, skip the one for the "after" spacer if we know
          // it's meaningless to talk about any overlap into it.
          dotNetHelper.invokeMethodAsync(
            "OnSpacerAfterVisible",
            entry.boundingClientRect.bottom - entry.intersectionRect.bottom,
            spacerSeparation,
            containerSize,
          );
        }
      });
    }
    function isValidTableElement(element) {
      if (element === null) {
        return false;
      }
      return (
        (element instanceof HTMLTableElement && element.style.display === "") ||
        element.style.display === "table" ||
        (element instanceof HTMLTableSectionElement &&
          element.style.display === "") ||
        element.style.display === "table-row-group"
      );
    }
  }
  function dispose(dotNetHelper) {
    const observers = observersByDotNetId[dotNetHelper._id];
    if (observers) {
      observers.intersectionObserver.disconnect();
      observers.mutationObserverBefore.disconnect();
      observers.mutationObserverAfter.disconnect();
      dotNetHelper.dispose();
      delete observersByDotNetId[dotNetHelper._id];
    }
  } // CONCATENATED MODULE: ./Rendering/LogicalElements.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  /*
  A LogicalElement plays the same role as an Element instance from the point of view of the
  API consumer. Inserting and removing logical elements updates the browser DOM just the same.

  The difference is that, unlike regular DOM mutation APIs, the LogicalElement APIs don't use
  the underlying DOM structure as the data storage for the element hierarchy. Instead, the
  LogicalElement APIs take care of tracking hierarchical relationships separately. The point
  of this is to permit a logical tree structure in which parent/child relationships don't
  have to be materialized in terms of DOM element parent/child relationships. And the reason
  why we want that is so that hierarchies of Razor components can be tracked even when those
  components' render output need not be a single literal DOM element.

  Consumers of the API don't need to know about the implementation, but how it's done is:
  - Each LogicalElement is materialized in the DOM as either:
    - A Node instance, for actual Node instances inserted using 'insertLogicalChild' or
      for Element instances promoted to LogicalElement via 'toLogicalElement'
    - A Comment instance, for 'logical container' instances inserted using 'createAndInsertLogicalContainer'
  - Then, on that instance (i.e., the Node or Comment), we store an array of 'logical children'
    instances, e.g.,
      [firstChild, secondChild, thirdChild, ...]
    ... plus we store a reference to the 'logical parent' (if any)
  - The 'logical children' array means we can look up in O(1):
    - The number of logical children (not currently implemented because not required, but trivial)
    - The logical child at any given index
  - Whenever a logical child is added or removed, we update the parent's array of logical children
*/
  const logicalChildrenPropname = createSymbolOrFallback(
    "_blazorLogicalChildren",
  );
  const logicalParentPropname = createSymbolOrFallback("_blazorLogicalParent");
  const logicalEndSiblingPropname = createSymbolOrFallback("_blazorLogicalEnd");
  function toLogicalRootCommentElement(start, end) {
    // Now that we support start/end comments as component delimiters we are going to be setting up
    // adding the components rendered output as siblings of the start/end tags (between).
    // For that to work, we need to appropriately configure the parent element to be a logical element
    // with all their children being the child elements.
    // For example, imagine you have
    // <app>
    // <div><p>Static content</p></div>
    // <!-- start component
    // <!-- end component
    // <footer>Some other content</footer>
    // <app>
    // We want the parent element to be something like
    // *app
    // |- *div
    // |- *component
    // |- *footer
    if (!start.parentNode) {
      throw new Error(`Comment not connected to the DOM ${start.textContent}`);
    }
    const parent = start.parentNode;
    const parentLogicalElement = LogicalElements_toLogicalElement(
      parent,
      /* allow existing contents */ true,
    );
    const children = getLogicalChildrenArray(parentLogicalElement);
    Array.from(parent.childNodes).forEach(n => children.push(n));
    start[logicalParentPropname] = parentLogicalElement;
    // We might not have an end comment in the case of non-prerendered components.
    if (end) {
      start[logicalEndSiblingPropname] = end;
      LogicalElements_toLogicalElement(end);
    }
    return LogicalElements_toLogicalElement(start);
  }
  function LogicalElements_toLogicalElement(element, allowExistingContents) {
    // Normally it's good to assert that the element has started empty, because that's the usual
    // situation and we probably have a bug if it's not. But for the element that contain prerendered
    // root components, we want to let them keep their content until we replace it.
    if (element.childNodes.length > 0 && !allowExistingContents) {
      throw new Error(
        "New logical elements must start empty, or allowExistingContents must be true",
      );
    }
    if (!(logicalChildrenPropname in element)) {
      // If it's already a logical element, leave it alone
      element[logicalChildrenPropname] = [];
    }
    return element;
  }
  function emptyLogicalElement(element) {
    const childrenArray = getLogicalChildrenArray(element);
    while (childrenArray.length) {
      removeLogicalChild(element, 0);
    }
  }
  function createAndInsertLogicalContainer(parent, childIndex) {
    const containerElement = document.createComment("!");
    insertLogicalChild(containerElement, parent, childIndex);
    return containerElement;
  }
  function insertLogicalChild(child, parent, childIndex) {
    const childAsLogicalElement = child;
    if (child instanceof Comment) {
      const existingGrandchildren = getLogicalChildrenArray(
        childAsLogicalElement,
      );
      if (
        existingGrandchildren &&
        getLogicalChildrenArray(childAsLogicalElement).length > 0
      ) {
        // There's nothing to stop us implementing support for this scenario, and it's not difficult
        // (after inserting 'child' itself, also iterate through its logical children and physically
        // put them as following-siblings in the DOM). However there's no scenario that requires it
        // presently, so if we did implement it there'd be no good way to have tests for it.
        throw new Error(
          "Not implemented: inserting non-empty logical container",
        );
      }
    }
    if (getLogicalParent(childAsLogicalElement)) {
      // Likewise, we could easily support this scenario too (in this 'if' block, just splice
      // out 'child' from the logical children array of its previous logical parent by using
      // Array.prototype.indexOf to determine its previous sibling index).
      // But again, since there's not currently any scenario that would use it, we would not
      // have any test coverage for such an implementation.
      throw new Error("Not implemented: moving existing logical children");
    }
    const newSiblings = getLogicalChildrenArray(parent);
    if (childIndex < newSiblings.length) {
      // Insert
      const nextSibling = newSiblings[childIndex];
      nextSibling.parentNode.insertBefore(child, nextSibling);
      newSiblings.splice(childIndex, 0, childAsLogicalElement);
    } else {
      // Append
      appendDomNode(child, parent);
      newSiblings.push(childAsLogicalElement);
    }
    childAsLogicalElement[logicalParentPropname] = parent;
    if (!(logicalChildrenPropname in childAsLogicalElement)) {
      childAsLogicalElement[logicalChildrenPropname] = [];
    }
  }
  function removeLogicalChild(parent, childIndex) {
    const childrenArray = getLogicalChildrenArray(parent);
    const childToRemove = childrenArray.splice(childIndex, 1)[0];
    // If it's a logical container, also remove its descendants
    if (childToRemove instanceof Comment) {
      const grandchildrenArray = getLogicalChildrenArray(childToRemove);
      if (grandchildrenArray) {
        while (grandchildrenArray.length > 0) {
          removeLogicalChild(childToRemove, 0);
        }
      }
    }
    // Finally, remove the node itself
    const domNodeToRemove = childToRemove;
    domNodeToRemove.parentNode.removeChild(domNodeToRemove);
  }
  function getLogicalParent(element) {
    return element[logicalParentPropname] || null;
  }
  function getLogicalSiblingEnd(element) {
    return element[logicalEndSiblingPropname] || null;
  }
  function getLogicalChild(parent, childIndex) {
    return getLogicalChildrenArray(parent)[childIndex];
  }
  // SVG elements support `foreignObject` children that can hold arbitrary HTML.
  // For these scenarios, the parent SVG and `foreignObject` elements should
  // be rendered under the SVG namespace, while the HTML content should be rendered
  // under the XHTML namespace. If the correct namespaces are not provided, most
  // browsers will fail to render the foreign object content. Here, we ensure that if
  // we encounter a `foreignObject` in the SVG, then all its children will be placed
  // under the XHTML namespace.
  function isSvgElement(element) {
    // Note: This check is intentionally case-sensitive since we expect this element
    // to appear as a child of an SVG element and SVGs are case-sensitive.
    const closestElement = getClosestDomElement(element);
    return (
      closestElement.namespaceURI === "http://www.w3.org/2000/svg" &&
      closestElement["tagName"] !== "foreignObject"
    );
  }
  function getLogicalChildrenArray(element) {
    return element[logicalChildrenPropname];
  }
  function permuteLogicalChildren(parent, permutationList) {
    // The permutationList must represent a valid permutation, i.e., the list of 'from' indices
    // is distinct, and the list of 'to' indices is a permutation of it. The algorithm here
    // relies on that assumption.
    // Each of the phases here has to happen separately, because each one is designed not to
    // interfere with the indices or DOM entries used by subsequent phases.
    // Phase 1: track which nodes we will move
    const siblings = getLogicalChildrenArray(parent);
    permutationList.forEach(listEntry => {
      listEntry.moveRangeStart = siblings[listEntry.fromSiblingIndex];
      listEntry.moveRangeEnd = findLastDomNodeInRange(listEntry.moveRangeStart);
    });
    // Phase 2: insert markers
    permutationList.forEach(listEntry => {
      const marker = document.createComment("marker");
      listEntry.moveToBeforeMarker = marker;
      const insertBeforeNode = siblings[listEntry.toSiblingIndex + 1];
      if (insertBeforeNode) {
        insertBeforeNode.parentNode.insertBefore(marker, insertBeforeNode);
      } else {
        appendDomNode(marker, parent);
      }
    });
    // Phase 3: move descendants & remove markers
    permutationList.forEach(listEntry => {
      const insertBefore = listEntry.moveToBeforeMarker;
      const parentDomNode = insertBefore.parentNode;
      const elementToMove = listEntry.moveRangeStart;
      const moveEndNode = listEntry.moveRangeEnd;
      let nextToMove = elementToMove;
      while (nextToMove) {
        const nextNext = nextToMove.nextSibling;
        parentDomNode.insertBefore(nextToMove, insertBefore);
        if (nextToMove === moveEndNode) {
          break;
        } else {
          nextToMove = nextNext;
        }
      }
      parentDomNode.removeChild(insertBefore);
    });
    // Phase 4: update siblings index
    permutationList.forEach(listEntry => {
      siblings[listEntry.toSiblingIndex] = listEntry.moveRangeStart;
    });
  }
  function getClosestDomElement(logicalElement) {
    if (
      logicalElement instanceof Element ||
      logicalElement instanceof DocumentFragment
    ) {
      return logicalElement;
    } else if (logicalElement instanceof Comment) {
      return logicalElement.parentNode;
    } else {
      throw new Error("Not a valid logical element");
    }
  }
  function getLogicalNextSibling(element) {
    const siblings = getLogicalChildrenArray(getLogicalParent(element));
    const siblingIndex = Array.prototype.indexOf.call(siblings, element);
    return siblings[siblingIndex + 1] || null;
  }
  function appendDomNode(child, parent) {
    // This function only puts 'child' into the DOM in the right place relative to 'parent'
    // It does not update the logical children array of anything
    if (parent instanceof Element || parent instanceof DocumentFragment) {
      parent.appendChild(child);
    } else if (parent instanceof Comment) {
      const parentLogicalNextSibling = getLogicalNextSibling(parent);
      if (parentLogicalNextSibling) {
        // Since the parent has a logical next-sibling, its appended child goes right before that
        parentLogicalNextSibling.parentNode.insertBefore(
          child,
          parentLogicalNextSibling,
        );
      } else {
        // Since the parent has no logical next-sibling, keep recursing upwards until we find
        // a logical ancestor that does have a next-sibling or is a physical element.
        appendDomNode(child, getLogicalParent(parent));
      }
    } else {
      // Should never happen
      throw new Error(
        `Cannot append node because the parent is not a valid logical element. Parent: ${parent}`,
      );
    }
  }
  // Returns the final node (in depth-first evaluation order) that is a descendant of the logical element.
  // As such, the entire subtree is between 'element' and 'findLastDomNodeInRange(element)' inclusive.
  function findLastDomNodeInRange(element) {
    if (element instanceof Element || element instanceof DocumentFragment) {
      return element;
    }
    const nextSibling = getLogicalNextSibling(element);
    if (nextSibling) {
      // Simple case: not the last logical sibling, so take the node before the next sibling
      return nextSibling.previousSibling;
    } else {
      // Harder case: there's no logical next-sibling, so recurse upwards until we find
      // a logical ancestor that does have one, or a physical element
      const logicalParent = getLogicalParent(element);
      return logicalParent instanceof Element ||
        logicalParent instanceof DocumentFragment
        ? logicalParent.lastChild
        : findLastDomNodeInRange(logicalParent);
    }
  }
  function createSymbolOrFallback(fallback) {
    return typeof Symbol === "function" ? Symbol() : fallback;
  } // CONCATENATED MODULE: ./PageTitle.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  const PageTitle = {
    getAndRemoveExistingTitle,
  };
  function getAndRemoveExistingTitle() {
    var _a;
    // Other <title> elements may exist outside <head> (e.g., inside <svg> elements) but they aren't page titles
    const titleElements = document.head
      ? document.head.getElementsByTagName("title")
      : [];
    if (titleElements.length === 0) {
      return null;
    }
    let existingTitle = null;
    for (let index = titleElements.length - 1; index >= 0; index--) {
      const currentTitleElement = titleElements[index];
      const previousSibling = currentTitleElement.previousSibling;
      const isBlazorTitle =
        previousSibling instanceof Comment &&
        getLogicalParent(previousSibling) !== null;
      if (isBlazorTitle) {
        continue;
      }
      if (existingTitle === null) {
        existingTitle = currentTitleElement.textContent;
      }
      (_a = currentTitleElement.parentNode) === null || _a === void 0
        ? void 0
        : _a.removeChild(currentTitleElement);
    }
    return existingTitle;
  } // CONCATENATED MODULE: ./Rendering/Events/EventTypes.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  const eventTypeRegistry = new Map();
  const browserEventNamesToAliases = new Map();
  const createBlankEventArgsOptions = { createEventArgs: () => ({}) };
  const eventNameAliasRegisteredCallbacks = [];
  function registerCustomEventType(eventName, options) {
    if (!options) {
      throw new Error("The options parameter is required.");
    }
    // There can't be more than one registration for the same event name because then we wouldn't
    // know which eventargs data to supply.
    if (eventTypeRegistry.has(eventName)) {
      throw new Error(`The event '${eventName}' is already registered.`);
    }
    // If applicable, register this as an alias of the given browserEventName
    if (options.browserEventName) {
      const aliasGroup = browserEventNamesToAliases.get(
        options.browserEventName,
      );
      if (aliasGroup) {
        aliasGroup.push(eventName);
      } else {
        browserEventNamesToAliases.set(options.browserEventName, [eventName]);
      }
      // For developer convenience, it's allowed to register the custom event type *after*
      // some listeners for it are already present. Once the event name alias gets registered,
      // we have to notify any existing event delegators so they can update their delegated
      // events list.
      eventNameAliasRegisteredCallbacks.forEach(callback =>
        callback(eventName, options.browserEventName),
      );
    }
    eventTypeRegistry.set(eventName, options);
  }
  function getEventTypeOptions(eventName) {
    return eventTypeRegistry.get(eventName);
  }
  function getEventNameAliases(eventName) {
    return browserEventNamesToAliases.get(eventName);
  }
  function getBrowserEventName(possibleAliasEventName) {
    const eventOptions = eventTypeRegistry.get(possibleAliasEventName);
    return (
      (eventOptions === null || eventOptions === void 0
        ? void 0
        : eventOptions.browserEventName) || possibleAliasEventName
    );
  }
  function registerBuiltInEventType(eventNames, options) {
    eventNames.forEach(eventName => eventTypeRegistry.set(eventName, options));
  }
  registerBuiltInEventType(["input", "change"], {
    createEventArgs: parseChangeEvent,
  });
  registerBuiltInEventType(["copy", "cut", "paste"], {
    createEventArgs: e => parseClipboardEvent(e),
  });
  registerBuiltInEventType(
    [
      "drag",
      "dragend",
      "dragenter",
      "dragleave",
      "dragover",
      "dragstart",
      "drop",
    ],
    {
      createEventArgs: e => parseDragEvent(e),
    },
  );
  registerBuiltInEventType(["focus", "blur", "focusin", "focusout"], {
    createEventArgs: e => parseFocusEvent(e),
  });
  registerBuiltInEventType(["keydown", "keyup", "keypress"], {
    createEventArgs: e => parseKeyboardEvent(e),
  });
  registerBuiltInEventType(
    [
      "contextmenu",
      "click",
      "mouseover",
      "mouseout",
      "mousemove",
      "mousedown",
      "mouseup",
      "mouseleave",
      "mouseenter",
      "dblclick",
    ],
    {
      createEventArgs: e => parseMouseEvent(e),
    },
  );
  registerBuiltInEventType(["error"], {
    createEventArgs: e => parseErrorEvent(e),
  });
  registerBuiltInEventType(
    ["loadstart", "timeout", "abort", "load", "loadend", "progress"],
    {
      createEventArgs: e => parseProgressEvent(e),
    },
  );
  registerBuiltInEventType(
    [
      "touchcancel",
      "touchend",
      "touchmove",
      "touchenter",
      "touchleave",
      "touchstart",
    ],
    {
      createEventArgs: e => parseTouchEvent(e),
    },
  );
  registerBuiltInEventType(
    [
      "gotpointercapture",
      "lostpointercapture",
      "pointercancel",
      "pointerdown",
      "pointerenter",
      "pointerleave",
      "pointermove",
      "pointerout",
      "pointerover",
      "pointerup",
    ],
    {
      createEventArgs: e => parsePointerEvent(e),
    },
  );
  registerBuiltInEventType(["wheel", "mousewheel"], {
    createEventArgs: e => parseWheelEvent(e),
  });
  registerBuiltInEventType(["toggle"], createBlankEventArgsOptions);
  function parseChangeEvent(event) {
    const element = event.target;
    if (isTimeBasedInput(element)) {
      const normalizedValue = normalizeTimeBasedValue(element);
      return { value: normalizedValue };
    } else if (isMultipleSelectInput(element)) {
      const selectElement = element;
      const selectedValues = Array.from(selectElement.options)
        .filter(option => option.selected)
        .map(option => option.value);
      return { value: selectedValues };
    } else {
      const targetIsCheckbox = isCheckbox(element);
      const newValue = targetIsCheckbox
        ? !!element["checked"]
        : element["value"];
      return { value: newValue };
    }
  }
  function parseWheelEvent(event) {
    return {
      ...parseMouseEvent(event),
      deltaX: event.deltaX,
      deltaY: event.deltaY,
      deltaZ: event.deltaZ,
      deltaMode: event.deltaMode,
    };
  }
  function parsePointerEvent(event) {
    return {
      ...parseMouseEvent(event),
      pointerId: event.pointerId,
      width: event.width,
      height: event.height,
      pressure: event.pressure,
      tiltX: event.tiltX,
      tiltY: event.tiltY,
      pointerType: event.pointerType,
      isPrimary: event.isPrimary,
    };
  }
  function parseTouchEvent(event) {
    return {
      detail: event.detail,
      touches: parseTouch(event.touches),
      targetTouches: parseTouch(event.targetTouches),
      changedTouches: parseTouch(event.changedTouches),
      ctrlKey: event.ctrlKey,
      shiftKey: event.shiftKey,
      altKey: event.altKey,
      metaKey: event.metaKey,
      type: event.type,
    };
  }
  function parseFocusEvent(event) {
    return {
      type: event.type,
    };
  }
  function parseClipboardEvent(event) {
    return {
      type: event.type,
    };
  }
  function parseProgressEvent(event) {
    return {
      lengthComputable: event.lengthComputable,
      loaded: event.loaded,
      total: event.total,
      type: event.type,
    };
  }
  function parseErrorEvent(event) {
    return {
      message: event.message,
      filename: event.filename,
      lineno: event.lineno,
      colno: event.colno,
      type: event.type,
    };
  }
  function parseKeyboardEvent(event) {
    return {
      key: event.key,
      code: event.code,
      location: event.location,
      repeat: event.repeat,
      ctrlKey: event.ctrlKey,
      shiftKey: event.shiftKey,
      altKey: event.altKey,
      metaKey: event.metaKey,
      type: event.type,
    };
  }
  function parseDragEvent(event) {
    return {
      ...parseMouseEvent(event),
      dataTransfer: event.dataTransfer
        ? {
            dropEffect: event.dataTransfer.dropEffect,
            effectAllowed: event.dataTransfer.effectAllowed,
            files: Array.from(event.dataTransfer.files).map(f => f.name),
            items: Array.from(event.dataTransfer.items).map(i => ({
              kind: i.kind,
              type: i.type,
            })),
            types: event.dataTransfer.types,
          }
        : null,
    };
  }
  function parseTouch(touchList) {
    const touches = [];
    for (let i = 0; i < touchList.length; i++) {
      const touch = touchList[i];
      touches.push({
        identifier: touch.identifier,
        clientX: touch.clientX,
        clientY: touch.clientY,
        screenX: touch.screenX,
        screenY: touch.screenY,
        pageX: touch.pageX,
        pageY: touch.pageY,
      });
    }
    return touches;
  }
  function parseMouseEvent(event) {
    return {
      detail: event.detail,
      screenX: event.screenX,
      screenY: event.screenY,
      clientX: event.clientX,
      clientY: event.clientY,
      offsetX: event.offsetX,
      offsetY: event.offsetY,
      pageX: event.pageX,
      pageY: event.pageY,
      movementX: event.movementX,
      movementY: event.movementY,
      button: event.button,
      buttons: event.buttons,
      ctrlKey: event.ctrlKey,
      shiftKey: event.shiftKey,
      altKey: event.altKey,
      metaKey: event.metaKey,
      type: event.type,
    };
  }
  function isCheckbox(element) {
    return (
      !!element &&
      element.tagName === "INPUT" &&
      element.getAttribute("type") === "checkbox"
    );
  }
  const timeBasedInputs = ["date", "datetime-local", "month", "time", "week"];
  function isTimeBasedInput(element) {
    return timeBasedInputs.indexOf(element.getAttribute("type")) !== -1;
  }
  function isMultipleSelectInput(element) {
    return (
      element instanceof HTMLSelectElement && element.type === "select-multiple"
    );
  }
  function normalizeTimeBasedValue(element) {
    const value = element.value;
    const type = element.type;
    switch (type) {
      case "date":
      case "month":
        return value;
      case "datetime-local":
        return value.length === 16 ? value + ":00" : value; // Convert yyyy-MM-ddTHH:mm to yyyy-MM-ddTHH:mm:00
      case "time":
        return value.length === 5 ? value + ":00" : value; // Convert hh:mm to hh:mm:00
      case "week":
        // For now we are not going to normalize input type week as it is not trivial
        return value;
    }
    throw new Error(`Invalid element type '${type}'.`);
  } // CONCATENATED MODULE: ./InputFile.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  const InputFile = {
    init: InputFile_init,
    toImageFile,
    readFileData,
  };
  function InputFile_init(callbackWrapper, elem) {
    elem._blazorInputFileNextFileId = 0;
    elem.addEventListener("click", function () {
      // Permits replacing an existing file with a new one of the same file name.
      elem.value = "";
    });
    elem.addEventListener("change", function () {
      // Reduce to purely serializable data, plus an index by ID.
      elem._blazorFilesById = {};
      const fileList = Array.prototype.map.call(elem.files, function (file) {
        const result = {
          id: ++elem._blazorInputFileNextFileId,
          lastModified: new Date(file.lastModified).toISOString(),
          name: file.name,
          size: file.size,
          contentType: file.type,
          readPromise: undefined,
          arrayBuffer: undefined,
          blob: file,
        };
        elem._blazorFilesById[result.id] = result;
        return result;
      });
      callbackWrapper.invokeMethodAsync("NotifyChange", fileList);
    });
  }
  async function toImageFile(elem, fileId, format, maxWidth, maxHeight) {
    const originalFile = getFileById(elem, fileId);
    const loadedImage = await new Promise(function (resolve) {
      const originalFileImage = new Image();
      originalFileImage.onload = function () {
        URL.revokeObjectURL(originalFileImage.src);
        resolve(originalFileImage);
      };
      originalFileImage.onerror = function () {
        originalFileImage.onerror = null;
        URL.revokeObjectURL(originalFileImage.src);
      };
      originalFileImage.src = URL.createObjectURL(originalFile["blob"]);
    });
    const resizedImageBlob = await new Promise(function (resolve) {
      var _a;
      const desiredWidthRatio = Math.min(1, maxWidth / loadedImage.width);
      const desiredHeightRatio = Math.min(1, maxHeight / loadedImage.height);
      const chosenSizeRatio = Math.min(desiredWidthRatio, desiredHeightRatio);
      const canvas = document.createElement("canvas");
      canvas.width = Math.round(loadedImage.width * chosenSizeRatio);
      canvas.height = Math.round(loadedImage.height * chosenSizeRatio);
      (_a = canvas.getContext("2d")) === null || _a === void 0
        ? void 0
        : _a.drawImage(loadedImage, 0, 0, canvas.width, canvas.height);
      canvas.toBlob(resolve, format);
    });
    const result = {
      id: ++elem._blazorInputFileNextFileId,
      lastModified: originalFile.lastModified,
      name: originalFile.name,
      size:
        (resizedImageBlob === null || resizedImageBlob === void 0
          ? void 0
          : resizedImageBlob.size) || 0,
      contentType: format,
      blob: resizedImageBlob ? resizedImageBlob : originalFile.blob,
    };
    elem._blazorFilesById[result.id] = result;
    return result;
  }
  async function readFileData(elem, fileId) {
    const file = getFileById(elem, fileId);
    return file.blob;
  }
  function getFileById(elem, fileId) {
    const file = elem._blazorFilesById[fileId];
    if (!file) {
      throw new Error(
        `There is no file with ID ${fileId}. The file list may have changed. See https://aka.ms/aspnet/blazor-input-file-multiple-selections.`,
      );
    }
    return file;
  } // CONCATENATED MODULE: ./NavigationLock.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  const registeredLocks = new Set();
  const NavigationLock = {
    enableNavigationPrompt,
    disableNavigationPrompt,
  };
  function onBeforeUnload(event) {
    event.preventDefault();
    // Modern browsers display a confirmation prompt when returnValue is some value other than
    // null or undefined.
    // See: https://developer.mozilla.org/en-US/docs/Web/API/Window/beforeunload_event#compatibility_notes
    event.returnValue = true;
  }
  function enableNavigationPrompt(id) {
    if (registeredLocks.size === 0) {
      window.addEventListener("beforeunload", onBeforeUnload);
    }
    registeredLocks.add(id);
  }
  function disableNavigationPrompt(id) {
    registeredLocks.delete(id);
    if (registeredLocks.size === 0) {
      window.removeEventListener("beforeunload", onBeforeUnload);
    }
  } // CONCATENATED MODULE: ./StreamingInterop.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  async function getNextChunk(data, position, nextChunkSize) {
    if (data instanceof Blob) {
      return await getChunkFromBlob(data, position, nextChunkSize);
    } else {
      return getChunkFromArrayBufferView(data, position, nextChunkSize);
    }
  }
  async function getChunkFromBlob(data, position, nextChunkSize) {
    const chunkBlob = data.slice(position, position + nextChunkSize);
    const arrayBuffer = await chunkBlob.arrayBuffer();
    const nextChunkData = new Uint8Array(arrayBuffer);
    return nextChunkData;
  }
  function getChunkFromArrayBufferView(data, position, nextChunkSize) {
    const nextChunkData = new Uint8Array(
      data.buffer,
      data.byteOffset + position,
      nextChunkSize,
    );
    return nextChunkData;
  }
  const transmittingDotNetToJSStreams = new Map();
  function receiveDotNetDataStream(streamId, data, bytesRead, errorMessage) {
    let streamController = transmittingDotNetToJSStreams.get(streamId);
    if (!streamController) {
      const readableStream = new ReadableStream({
        start(controller) {
          transmittingDotNetToJSStreams.set(streamId, controller);
          streamController = controller;
        },
      });
      DotNet.jsCallDispatcher.supplyDotNetStream(streamId, readableStream);
    }
    if (errorMessage) {
      streamController.error(errorMessage);
      transmittingDotNetToJSStreams.delete(streamId);
    } else if (bytesRead === 0) {
      streamController.close();
      transmittingDotNetToJSStreams.delete(streamId);
    } else {
      streamController.enqueue(
        data.length === bytesRead ? data : data.subarray(0, bytesRead),
      );
    }
  } // CONCATENATED MODULE: ./Rendering/JSRootComponents.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  const pendingRootComponentContainerNamePrefix = "__bl-dynamic-root:";
  const pendingRootComponentContainers = new Map();
  let nextPendingDynamicRootComponentIdentifier = 0;
  let manager;
  let jsComponentParametersByIdentifier;
  // These are the public APIs at Blazor.rootComponents.*
  const RootComponentsFunctions = {
    async add(toElement, componentIdentifier, initialParameters) {
      if (!initialParameters) {
        throw new Error("initialParameters must be an object, even if empty.");
      }
      // Track the container so we can use it when the component gets attached to the document via a selector
      const containerIdentifier =
        pendingRootComponentContainerNamePrefix +
        (++nextPendingDynamicRootComponentIdentifier).toString();
      pendingRootComponentContainers.set(containerIdentifier, toElement);
      // Instruct .NET to add and render the new root component
      const componentId = await getRequiredManager().invokeMethodAsync(
        "AddRootComponent",
        componentIdentifier,
        containerIdentifier,
      );
      const component = new DynamicRootComponent(
        componentId,
        jsComponentParametersByIdentifier[componentIdentifier],
      );
      await component.setParameters(initialParameters);
      return component;
    },
  };
  function JSRootComponents_getAndRemovePendingRootComponentContainer(
    containerIdentifier,
  ) {
    const container = pendingRootComponentContainers.get(containerIdentifier);
    if (container) {
      pendingRootComponentContainers.delete(containerIdentifier);
      return container;
    }
  }
  class EventCallbackWrapper {
    invoke(arg) {
      return this._callback(arg);
    }
    setCallback(callback) {
      if (!this._selfJSObjectReference) {
        this._selfJSObjectReference = DotNet.createJSObjectReference(this);
      }
      this._callback = callback;
    }
    getJSObjectReference() {
      return this._selfJSObjectReference;
    }
    dispose() {
      if (this._selfJSObjectReference) {
        DotNet.disposeJSObjectReference(this._selfJSObjectReference);
      }
    }
  }
  class DynamicRootComponent {
    constructor(componentId, parameters) {
      this._jsEventCallbackWrappers = new Map();
      this._componentId = componentId;
      for (const parameter of parameters) {
        if (parameter.type === "eventcallback") {
          this._jsEventCallbackWrappers.set(
            parameter.name.toLowerCase(),
            new EventCallbackWrapper(),
          );
        }
      }
    }
    setParameters(parameters) {
      const mappedParameters = {};
      const entries = Object.entries(parameters || {});
      const parameterCount = entries.length;
      for (const [key, value] of entries) {
        const callbackWrapper = this._jsEventCallbackWrappers.get(
          key.toLowerCase(),
        );
        if (!callbackWrapper || !value) {
          mappedParameters[key] = value;
          continue;
        }
        callbackWrapper.setCallback(value);
        mappedParameters[key] = callbackWrapper.getJSObjectReference();
      }
      return getRequiredManager().invokeMethodAsync(
        "SetRootComponentParameters",
        this._componentId,
        parameterCount,
        mappedParameters,
      );
    }
    async dispose() {
      if (this._componentId !== null) {
        await getRequiredManager().invokeMethodAsync(
          "RemoveRootComponent",
          this._componentId,
        );
        this._componentId = null; // Ensure it can't be used again
        for (const jsEventCallbackWrapper of this._jsEventCallbackWrappers.values()) {
          jsEventCallbackWrapper.dispose();
        }
      }
    }
  }
  // Called by the framework
  function enableJSRootComponents(
    managerInstance,
    jsComponentParameters,
    jsComponentInitializers,
  ) {
    if (manager) {
      // This will only happen in very nonstandard cases where someone has multiple hosts.
      // It's up to the developer to ensure that only one of them enables dynamic root components.
      throw new Error("Dynamic root components have already been enabled.");
    }
    manager = managerInstance;
    jsComponentParametersByIdentifier = jsComponentParameters;
    // Call the registered initializers. This is an arbitrary subset of the JS component types that are registered
    // on the .NET side - just those of them that require some JS-side initialization (e.g., to register them
    // as custom elements).
    for (const [initializerIdentifier, componentIdentifiers] of Object.entries(
      jsComponentInitializers,
    )) {
      const initializerFunc = DotNet.jsCallDispatcher.findJSFunction(
        initializerIdentifier,
        0,
      );
      for (const componentIdentifier of componentIdentifiers) {
        const parameters = jsComponentParameters[componentIdentifier];
        initializerFunc(componentIdentifier, parameters);
      }
    }
  }
  function getRequiredManager() {
    if (!manager) {
      throw new Error(
        "Dynamic root components have not been enabled in this application.",
      );
    }
    return manager;
  } // CONCATENATED MODULE: ./Rendering/WebRendererInteropMethods.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  const interopMethodsByRenderer = new Map();
  let resolveRendererAttached;
  const rendererAttached = new Promise(resolve => {
    resolveRendererAttached = resolve;
  });
  function attachWebRendererInterop(
    rendererId,
    interopMethods,
    jsComponentParameters,
    jsComponentInitializers,
  ) {
    if (interopMethodsByRenderer.has(rendererId)) {
      throw new Error(
        `Interop methods are already registered for renderer ${rendererId}`,
      );
    }
    interopMethodsByRenderer.set(rendererId, interopMethods);
    if (Object.keys(jsComponentParameters).length > 0) {
      const manager = getInteropMethods(rendererId);
      enableJSRootComponents(
        manager,
        jsComponentParameters,
        jsComponentInitializers,
      );
    }
    resolveRendererAttached();
  }
  function dispatchEvent(browserRendererId, eventDescriptor, eventArgs) {
    return dispatchEventMiddleware(
      browserRendererId,
      eventDescriptor.eventHandlerId,
      () => {
        const interopMethods = getInteropMethods(browserRendererId);
        return interopMethods.invokeMethodAsync(
          "DispatchEventAsync",
          eventDescriptor,
          eventArgs,
        );
      },
    );
  }
  function getInteropMethods(rendererId) {
    const interopMethods = interopMethodsByRenderer.get(rendererId);
    if (!interopMethods) {
      throw new Error(
        `No interop methods are registered for renderer ${rendererId}`,
      );
    }
    return interopMethods;
  }
  let dispatchEventMiddleware = (
    browserRendererId,
    eventHandlerId,
    continuation,
  ) => continuation();
  function setDispatchEventMiddleware(middleware) {
    dispatchEventMiddleware = middleware;
  } // CONCATENATED MODULE: ./GlobalExports.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  const Blazor = {
    navigateTo: navigateTo,
    registerCustomEventType: registerCustomEventType,
    rootComponents: RootComponentsFunctions,
    _internal: {
      navigationManager: internalFunctions,
      domWrapper: domFunctions,
      Virtualize: Virtualize,
      PageTitle: PageTitle,
      InputFile: InputFile,
      NavigationLock: NavigationLock,
      getJSDataStreamChunk: getNextChunk,
      receiveDotNetDataStream: receiveDotNetDataStream,
      attachWebRendererInterop: attachWebRendererInterop,
    },
  };
  // Make the following APIs available in global scope for invocation from JS
  window["Blazor"] = Blazor; // CONCATENATED MODULE: ./Environment.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  let platform;
  function setPlatform(platformInstance) {
    platform = platformInstance;
    return platform;
  } // CONCATENATED MODULE: ./Platform/Mono/MonoDebugger.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  var _a, _b;
  const navigatorUA = navigator;
  const brands = navigatorUA.userAgentData && navigatorUA.userAgentData.brands;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const currentBrowserIsChromeOrEdge = brands
    ? brands.some(
        b => b.brand === "Google Chrome" || b.brand === "Microsoft Edge",
      )
    : window.chrome;
  const MonoDebugger_platform =
    (_b =
      (_a = navigatorUA.userAgentData) === null || _a === void 0
        ? void 0
        : _a.platform) !== null && _b !== void 0
      ? _b
      : navigator.platform;
  let hasReferencedPdbs = false;
  let debugBuild = false;
  function hasDebuggingEnabled() {
    return (hasReferencedPdbs || debugBuild) && currentBrowserIsChromeOrEdge;
  }
  function attachDebuggerHotkey(resourceLoader) {
    hasReferencedPdbs = !!resourceLoader.bootConfig.resources.pdb;
    debugBuild = resourceLoader.bootConfig.debugBuild;
    // Use the combination shift+alt+D because it isn't used by the major browsers
    // for anything else by default
    const altKeyName = MonoDebugger_platform.match(/^Mac/i) ? "Cmd" : "Alt";
    if (hasDebuggingEnabled()) {
      console.info(
        `Debugging hotkey: Shift+${altKeyName}+D (when application has focus)`,
      );
    }
    // Even if debugging isn't enabled, we register the hotkey so we can report why it's not enabled
    document.addEventListener("keydown", evt => {
      if (evt.shiftKey && (evt.metaKey || evt.altKey) && evt.code === "KeyD") {
        if (!debugBuild && !hasReferencedPdbs) {
          console.error(
            "Cannot start debugging, because the application was not compiled with debugging enabled.",
          );
        } else if (!currentBrowserIsChromeOrEdge) {
          console.error(
            "Currently, only Microsoft Edge (80+), or Google Chrome, are supported for debugging.",
          );
        } else {
          launchDebugger();
        }
      }
    });
  }
  function launchDebugger() {
    // The noopener flag is essential, because otherwise Chrome tracks the association with the
    // parent tab, and then when the parent tab pauses in the debugger, the child tab does so
    // too (even if it's since navigated to a different page). This means that the debugger
    // itself freezes, and not just the page being debugged.
    //
    // We have to construct a link element and simulate a click on it, because the more obvious
    // window.open(..., 'noopener') always opens a new window instead of a new tab.
    const link = document.createElement("a");
    link.href = `_framework/debug?url=${encodeURIComponent(location.href)}`;
    link.target = "_blank";
    link.rel = "noopener noreferrer";
    link.click();
  } // CONCATENATED MODULE: ./BootErrors.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  let hasFailed = false;
  function showErrorNotification() {
    const errorUi = document.querySelector("#blazor-error-ui");
    if (errorUi) {
      errorUi.style.display = "block";
    }
    if (!hasFailed) {
      hasFailed = true;
      const errorUiReloads = document.querySelectorAll(
        "#blazor-error-ui .reload",
      );
      errorUiReloads.forEach(reload => {
        reload.onclick = function (e) {
          location.reload();
          e.preventDefault();
        };
      });
      const errorUiDismiss = document.querySelectorAll(
        "#blazor-error-ui .dismiss",
      );
      errorUiDismiss.forEach(dismiss => {
        dismiss.onclick = function (e) {
          const errorUi = document.querySelector("#blazor-error-ui");
          if (errorUi) {
            errorUi.style.display = "none";
          }
          e.preventDefault();
        };
      });
    }
  } // CONCATENATED MODULE: ./Platform/BootConfig.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
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
  })(ICUDataMode || (ICUDataMode = {})); // CONCATENATED MODULE: ./Platform/Mono/MonoPlatform.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  /* eslint-disable @typescript-eslint/no-explicit-any */
  /* eslint-disable @typescript-eslint/no-non-null-assertion */
  /* eslint-disable no-prototype-builtins */

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
      attachDebuggerHotkey(resourceLoader);
      await createEmscriptenModuleInstance(resourceLoader);
    },
    callEntryPoint: async function callEntryPoint(assemblyName) {
      const emptyArray = [[]];
      try {
        await BINDING.call_assembly_entry_point(assemblyName, emptyArray, "m");
      } catch (error) {
        console.error(error);
        showErrorNotification();
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
    const browserSupportsNativeWebAssembly =
      typeof WebAssembly !== "undefined" && WebAssembly.validate;
    if (!browserSupportsNativeWebAssembly) {
      throw new Error("This browser does not support WebAssembly.");
    }
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
    // For consistency with WebAssemblyResourceLoader, we only enforce SRI if caching is allowed
    if (resourceLoader.bootConfig.cacheBootResources) {
      const scriptElem = document.createElement("link");
      scriptElem.rel = "modulepreload";
      scriptElem.href = src;
      scriptElem.crossOrigin = "anonymous";
      // it will make dynamic import fail if the hash doesn't match
      // It's currently only validated by chromium browsers
      // Firefox doesn't break on it, but doesn't validate it either
      scriptElem.integrity = dotnetJsContentHash;
      document.head.appendChild(scriptElem);
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
      showErrorNotification();
    };
    const existingPreRun = moduleConfig.preRun || [];
    const existingPostRun = moduleConfig.postRun || [];
    moduleConfig.preloadPlugins = [];
    let resourcesLoaded = 0;
    function setProgress() {
      resourcesLoaded++;
      const percentage = (resourcesLoaded / totalResources.length) * 100;
      document.documentElement.style.setProperty(
        "--blazor-load-percentage",
        `${percentage}%`,
      );
      document.documentElement.style.setProperty(
        "--blazor-load-percentage-text",
        `"${Math.floor(percentage)}%"`,
      );
    }
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
    totalResources.forEach(loadingResource =>
      loadingResource.response.then(_ => setProgress()),
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
      timeZoneResource.response.then(_ => setProgress());
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
      icuDataResource.response.then(_ => setProgress());
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
          addResourceAsAssembly(r, changeExtension(r.name, ".dll")),
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
        const lazyResources = {};
        Blazor._internal.getLazyAssemblies = assembliesToLoadDotNetArray => {
          const assembliesToLoad = BINDING.mono_array_to_js_array(
            assembliesToLoadDotNetArray,
          );
          const lazyAssemblies =
            resourceLoader.bootConfig.resources.lazyAssembly;
          if (!lazyAssemblies) {
            throw new Error(
              "No assemblies have been marked as lazy-loadable. Use the 'BlazorWebAssemblyLazyLoad' item group in your project file to enable lazy loading an assembly.",
            );
          }
          const assembliesMarkedAsLazy = assembliesToLoad.filter(assembly =>
            lazyAssemblies.hasOwnProperty(assembly),
          );
          if (assembliesMarkedAsLazy.length !== assembliesToLoad.length) {
            const notMarked = assembliesToLoad.filter(
              assembly => !assembliesMarkedAsLazy.includes(assembly),
            );
            throw new Error(
              `${notMarked.join()} must be marked with 'BlazorWebAssemblyLazyLoad' item group in your project file to allow lazy-loading.`,
            );
          }
          let pdbPromises;
          if (hasDebuggingEnabled()) {
            const pdbs = resourceLoader.bootConfig.resources.pdb;
            const pdbsToLoad = assembliesMarkedAsLazy.map(a =>
              changeExtension(a, ".pdb"),
            );
            if (pdbs) {
              pdbPromises = Promise.all(
                pdbsToLoad
                  .map(pdb =>
                    lazyAssemblies.hasOwnProperty(pdb)
                      ? resourceLoader.loadResource(
                          pdb,
                          `_framework/${pdb}`,
                          lazyAssemblies[pdb],
                          "pdb",
                        )
                      : null,
                  )
                  .map(async resource =>
                    resource ? (await resource.response).arrayBuffer() : null,
                  ),
              );
            }
          }
          const resourcePromises = Promise.all(
            assembliesMarkedAsLazy
              .map(assembly =>
                resourceLoader.loadResource(
                  assembly,
                  `_framework/${assembly}`,
                  lazyAssemblies[assembly],
                  "assembly",
                ),
              )
              .map(async resource => (await resource.response).arrayBuffer()),
          );
          return BINDING.js_to_mono_obj(
            Promise.all([resourcePromises, pdbPromises]).then(values => {
              lazyResources["assemblies"] = values[0];
              lazyResources["pdbs"] = values[1];
              if (lazyResources["assemblies"].length) {
                Blazor._internal.readLazyAssemblies = () => {
                  const { assemblies } = lazyResources;
                  if (!assemblies) {
                    return BINDING.mono_obj_array_new(0);
                  }
                  const assemblyBytes = BINDING.mono_obj_array_new(
                    assemblies.length,
                  );
                  for (let i = 0; i < assemblies.length; i++) {
                    const assembly = assemblies[i];
                    BINDING.mono_obj_array_set(
                      assemblyBytes,
                      i,
                      BINDING.js_typed_array_to_array(new Uint8Array(assembly)),
                    );
                  }
                  return assemblyBytes;
                };
                Blazor._internal.readLazyPdbs = () => {
                  const { assemblies, pdbs } = lazyResources;
                  if (!assemblies) {
                    return BINDING.mono_obj_array_new(0);
                  }
                  const pdbBytes = BINDING.mono_obj_array_new(
                    assemblies.length,
                  );
                  for (let i = 0; i < assemblies.length; i++) {
                    const pdb =
                      pdbs && pdbs[i]
                        ? new Uint8Array(pdbs[i])
                        : new Uint8Array();
                    BINDING.mono_obj_array_set(
                      pdbBytes,
                      i,
                      BINDING.js_typed_array_to_array(pdb),
                    );
                  }
                  return pdbBytes;
                };
              }
              return lazyResources["assemblies"].length;
            }),
          );
        };
      };
      const postRun = () => {
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
        // -1 enables debugging with logging disabled. 0 disables debugging entirely.
        MONO.mono_wasm_load_runtime(
          appBinDirName,
          hasDebuggingEnabled() ? -1 : 0,
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
  let byteArrayBeingTransferred = null;
  function attachInteropInvoker() {
    const dotNetDispatcherInvokeMethodHandle = bindStaticMethod(
      "Microsoft.AspNetCore.Components.WebAssembly",
      "Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime",
      "InvokeDotNet",
    );
    const dotNetDispatcherBeginInvokeMethodHandle = bindStaticMethod(
      "Microsoft.AspNetCore.Components.WebAssembly",
      "Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime",
      "BeginInvokeDotNet",
    );
    const dotNetDispatcherEndInvokeJSMethodHandle = bindStaticMethod(
      "Microsoft.AspNetCore.Components.WebAssembly",
      "Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime",
      "EndInvokeJS",
    );
    const dotNetDispatcherNotifyByteArrayAvailableMethodHandle =
      bindStaticMethod(
        "Microsoft.AspNetCore.Components.WebAssembly",
        "Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime",
        "NotifyByteArrayAvailable",
      );
    DotNet.attachDispatcher({
      beginInvokeDotNetFromJS: (
        callId,
        assemblyName,
        methodIdentifier,
        dotNetObjectId,
        argsJson,
      ) => {
        assertHeapIsNotLocked();
        if (!dotNetObjectId && !assemblyName) {
          throw new Error(
            "Either assemblyName or dotNetObjectId must have a non null value.",
          );
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
      sendByteArray: (id, data) => {
        byteArrayBeingTransferred = data;
        dotNetDispatcherNotifyByteArrayAvailableMethodHandle(id);
      },
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
  function changeExtension(filename, newExtensionWithLeadingDot) {
    const lastDotIndex = filename.lastIndexOf(".");
    if (lastDotIndex < 0) {
      throw new Error(`No extension to replace in '${filename}'`);
    }
    return filename.substr(0, lastDotIndex) + newExtensionWithLeadingDot;
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
  } // CONCATENATED MODULE: ./Rendering/RenderBatch/SharedMemoryRenderBatch.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  // Used when running on Mono WebAssembly for shared-memory interop. The code here encapsulates
  // our knowledge of the memory layout of RenderBatch and all referenced types.
  //
  // In this implementation, all the DTO types are really heap pointers at runtime, hence all
  // the casts to 'any' whenever we pass them to platform.read.
  class SharedMemoryRenderBatch {
    constructor(batchAddress) {
      this.batchAddress = batchAddress;
      this.arrayRangeReader = arrayRangeReader;
      this.arrayBuilderSegmentReader = arrayBuilderSegmentReader;
      this.diffReader = diffReader;
      this.editReader = editReader;
      this.frameReader = frameReader;
    }
    // Keep in sync with memory layout in RenderBatch.cs
    updatedComponents() {
      return platform.readStructField(this.batchAddress, 0);
    }
    referenceFrames() {
      return platform.readStructField(
        this.batchAddress,
        arrayRangeReader.structLength,
      );
    }
    disposedComponentIds() {
      return platform.readStructField(
        this.batchAddress,
        arrayRangeReader.structLength * 2,
      );
    }
    disposedEventHandlerIds() {
      return platform.readStructField(
        this.batchAddress,
        arrayRangeReader.structLength * 3,
      );
    }
    updatedComponentsEntry(values, index) {
      return arrayValuesEntry(values, index, diffReader.structLength);
    }
    referenceFramesEntry(values, index) {
      return arrayValuesEntry(values, index, frameReader.structLength);
    }
    disposedComponentIdsEntry(values, index) {
      const pointer = arrayValuesEntry(values, index, /* int length */ 4);
      return platform.readInt32Field(pointer);
    }
    disposedEventHandlerIdsEntry(values, index) {
      const pointer = arrayValuesEntry(values, index, /* long length */ 8);
      return platform.readUint64Field(pointer);
    }
  }
  // Keep in sync with memory layout in ArrayRange.cs
  const arrayRangeReader = {
    structLength: 8,
    values: arrayRange => platform.readObjectField(arrayRange, 0),
    count: arrayRange => platform.readInt32Field(arrayRange, 4),
  };
  // Keep in sync with memory layout in ArrayBuilderSegment
  const arrayBuilderSegmentReader = {
    structLength: 12,
    values: arrayBuilderSegment => {
      // Evaluate arrayBuilderSegment->_builder->_items, i.e., two dereferences needed
      const builder = platform.readObjectField(arrayBuilderSegment, 0);
      const builderFieldsAddress = platform.getObjectFieldsBaseAddress(builder);
      return platform.readObjectField(builderFieldsAddress, 0);
    },
    offset: arrayBuilderSegment =>
      platform.readInt32Field(arrayBuilderSegment, 4),
    count: arrayBuilderSegment =>
      platform.readInt32Field(arrayBuilderSegment, 8),
  };
  // Keep in sync with memory layout in RenderTreeDiff.cs
  const diffReader = {
    structLength: 4 + arrayBuilderSegmentReader.structLength,
    componentId: diff => platform.readInt32Field(diff, 0),
    edits: diff => platform.readStructField(diff, 4),
    editsEntry: (values, index) =>
      arrayValuesEntry(values, index, editReader.structLength),
  };
  // Keep in sync with memory layout in RenderTreeEdit.cs
  const editReader = {
    structLength: 20,
    editType: edit => platform.readInt32Field(edit, 0),
    siblingIndex: edit => platform.readInt32Field(edit, 4),
    newTreeIndex: edit => platform.readInt32Field(edit, 8),
    moveToSiblingIndex: edit => platform.readInt32Field(edit, 8),
    removedAttributeName: edit => platform.readStringField(edit, 16),
  };
  // Keep in sync with memory layout in RenderTreeFrame.cs
  const frameReader = {
    structLength: 36,
    frameType: frame => platform.readInt16Field(frame, 4),
    subtreeLength: frame => platform.readInt32Field(frame, 8),
    elementReferenceCaptureId: frame => platform.readStringField(frame, 16),
    componentId: frame => platform.readInt32Field(frame, 12),
    elementName: frame => platform.readStringField(frame, 16),
    textContent: frame => platform.readStringField(frame, 16),
    markupContent: frame => platform.readStringField(frame, 16),
    attributeName: frame => platform.readStringField(frame, 16),
    attributeValue: frame => platform.readStringField(frame, 24, true),
    attributeEventHandlerId: frame => platform.readUint64Field(frame, 8),
  };
  function arrayValuesEntry(arrayValues, index, itemSize) {
    return platform.getArrayEntryPtr(arrayValues, index, itemSize);
  } // CONCATENATED MODULE: ./BootCommon.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  // Tells you if the script was added without <script src="..." autostart="false"></script>
  function shouldAutoStart() {
    return !!(
      document &&
      document.currentScript &&
      document.currentScript.getAttribute("autostart") !== "false"
    );
  } // CONCATENATED MODULE: ./Platform/WebAssemblyResourceLoader.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  const networkFetchCacheMode = "no-cache";
  class WebAssemblyResourceLoader {
    constructor(bootConfig, cacheIfUsed, startOptions) {
      this.bootConfig = bootConfig;
      this.cacheIfUsed = cacheIfUsed;
      this.startOptions = startOptions;
      this.usedCacheKeys = {};
      this.networkLoads = {};
      this.cacheLoads = {};
    }
    static async initAsync(bootConfig, startOptions) {
      const cache = await getCacheToUseIfEnabled(bootConfig);
      return new WebAssemblyResourceLoader(bootConfig, cache, startOptions);
    }
    loadResources(resources, url, resourceType) {
      return Object.keys(resources).map(name =>
        this.loadResource(name, url(name), resources[name], resourceType),
      );
    }
    loadResource(name, url, contentHash, resourceType) {
      const response = this.cacheIfUsed
        ? this.loadResourceWithCaching(
            this.cacheIfUsed,
            name,
            url,
            contentHash,
            resourceType,
          )
        : this.loadResourceWithoutCaching(name, url, contentHash, resourceType);
      return { name, url, response };
    }
    logToConsole() {
      const cacheLoadsEntries = Object.values(this.cacheLoads);
      const networkLoadsEntries = Object.values(this.networkLoads);
      const cacheResponseBytes = countTotalBytes(cacheLoadsEntries);
      const networkResponseBytes = countTotalBytes(networkLoadsEntries);
      const totalResponseBytes = cacheResponseBytes + networkResponseBytes;
      if (totalResponseBytes === 0) {
        // We have no perf stats to display, likely because caching is not in use.
        return;
      }
      const linkerDisabledWarning = this.bootConfig.linkerEnabled
        ? "%c"
        : "\n%cThis application was built with linking (tree shaking) disabled. Published applications will be significantly smaller.";
      console.groupCollapsed(
        `%cblazor%c Loaded ${toDataSizeString(
          totalResponseBytes,
        )} resources${linkerDisabledWarning}`,
        "background: purple; color: white; padding: 1px 3px; border-radius: 3px;",
        "font-weight: bold;",
        "font-weight: normal;",
      );
      if (cacheLoadsEntries.length) {
        console.groupCollapsed(
          `Loaded ${toDataSizeString(cacheResponseBytes)} resources from cache`,
        );
        console.table(this.cacheLoads);
        console.groupEnd();
      }
      if (networkLoadsEntries.length) {
        console.groupCollapsed(
          `Loaded ${toDataSizeString(
            networkResponseBytes,
          )} resources from network`,
        );
        console.table(this.networkLoads);
        console.groupEnd();
      }
      console.groupEnd();
    }
    async purgeUnusedCacheEntriesAsync() {
      // We want to keep the cache small because, even though the browser will evict entries if it
      // gets too big, we don't want to be considered problematic by the end user viewing storage stats
      const cache = this.cacheIfUsed;
      if (cache) {
        const cachedRequests = await cache.keys();
        const deletionPromises = cachedRequests.map(async cachedRequest => {
          if (!(cachedRequest.url in this.usedCacheKeys)) {
            await cache.delete(cachedRequest);
          }
        });
        await Promise.all(deletionPromises);
      }
    }
    async loadResourceWithCaching(cache, name, url, contentHash, resourceType) {
      // Since we are going to cache the response, we require there to be a content hash for integrity
      // checking. We don't want to cache bad responses. There should always be a hash, because the build
      // process generates this data.
      if (!contentHash || contentHash.length === 0) {
        throw new Error("Content hash is required");
      }
      const cacheKey = toAbsoluteUri(`${url}.${contentHash}`);
      this.usedCacheKeys[cacheKey] = true;
      let cachedResponse;
      try {
        cachedResponse = await cache.match(cacheKey);
      } catch {
        // Be tolerant to errors reading from the cache. This is a guard for https://bugs.chromium.org/p/chromium/issues/detail?id=968444 where
        // chromium browsers may sometimes throw when working with the cache.
      }
      if (cachedResponse) {
        // It's in the cache.
        const responseBytes = parseInt(
          cachedResponse.headers.get("content-length") || "0",
        );
        this.cacheLoads[name] = { responseBytes };
        return cachedResponse;
      } else {
        // It's not in the cache. Fetch from network.
        const networkResponse = await this.loadResourceWithoutCaching(
          name,
          url,
          contentHash,
          resourceType,
        );
        this.addToCacheAsync(cache, name, cacheKey, networkResponse); // Don't await - add to cache in background
        return networkResponse;
      }
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
    async addToCacheAsync(cache, name, cacheKey, response) {
      // We have to clone in order to put this in the cache *and* not prevent other code from
      // reading the original response stream.
      const responseData = await response.clone().arrayBuffer();
      // Now is an ideal moment to capture the performance stats for the request, since it
      // only just completed and is most likely to still be in the buffer. However this is
      // only done on a 'best effort' basis. Even if we do receive an entry, some of its
      // properties may be blanked out if it was a CORS request.
      const performanceEntry = getPerformanceEntry(response.url);
      const responseBytes =
        (performanceEntry && performanceEntry.encodedBodySize) || undefined;
      this.networkLoads[name] = { responseBytes };
      // Add to cache as a custom response object so we can track extra data such as responseBytes
      // We can't rely on the server sending content-length (ASP.NET Core doesn't by default)
      const responseToCache = new Response(responseData, {
        headers: {
          "content-type": response.headers.get("content-type") || "",
          "content-length": (
            responseBytes ||
            response.headers.get("content-length") ||
            ""
          ).toString(),
        },
      });
      try {
        await cache.put(cacheKey, responseToCache);
      } catch {
        // Be tolerant to errors writing to the cache. This is a guard for https://bugs.chromium.org/p/chromium/issues/detail?id=968444 where
        // chromium browsers may sometimes throw when performing cache operations.
      }
    }
  }
  async function getCacheToUseIfEnabled(bootConfig) {
    // caches will be undefined if we're running on an insecure origin (secure means https or localhost)
    if (!bootConfig.cacheBootResources || typeof caches === "undefined") {
      return null;
    }
    // cache integrity is compromised if the first request has been served over http (except localhost)
    // in this case, we want to disable caching and integrity validation
    if (window.isSecureContext === false) {
      return null;
    }
    // Define a separate cache for each base href, so we're isolated from any other
    // Blazor application running on the same origin. We need this so that we're free
    // to purge from the cache anything we're not using and don't let it keep growing,
    // since we don't want to be worst offenders for space usage.
    const relativeBaseHref = document.baseURI.substring(
      document.location.origin.length,
    );
    const cacheName = `blazor-resources-${relativeBaseHref}`;
    try {
      // There's a Chromium bug we need to be aware of here: the CacheStorage APIs say that when
      // caches.open(name) returns a promise that succeeds, the value is meant to be a Cache instance.
      // However, if the browser was launched with a --user-data-dir param that's "too long" in some sense,
      // then even through the promise resolves as success, the value given is `undefined`.
      // See https://stackoverflow.com/a/46626574 and https://bugs.chromium.org/p/chromium/issues/detail?id=1054541
      // If we see this happening, return "null" to mean "proceed without caching".
      return (await caches.open(cacheName)) || null;
    } catch {
      // There's no known scenario where we should get an exception here, but considering the
      // Chromium bug above, let's tolerate it and treat as "proceed without caching".
      return null;
    }
  }
  function countTotalBytes(loads) {
    return loads.reduce((prev, item) => prev + (item.responseBytes || 0), 0);
  }
  function toDataSizeString(byteCount) {
    return `${(byteCount / (1024 * 1024)).toFixed(2)} MB`;
  }
  function getPerformanceEntry(url) {
    if (typeof performance !== "undefined") {
      return performance.getEntriesByName(url)[0];
    }
  } // CONCATENATED MODULE: ./Platform/WebAssemblyConfigLoader.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  class WebAssemblyConfigLoader {
    static async initAsync(bootConfigResult) {
      Blazor._internal.getApplicationEnvironment = () =>
        BINDING.js_string_to_mono_string(
          bootConfigResult.applicationEnvironment,
        );
      const configFiles = await Promise.all(
        (bootConfigResult.bootConfig.config || [])
          .filter(
            name =>
              name === "appsettings.json" ||
              name ===
                `appsettings.${bootConfigResult.applicationEnvironment}.json`,
          )
          .map(async name => ({ name, content: await getConfigBytes(name) })),
      );
      Blazor._internal.getConfig = dotNetFileName => {
        const fileName = BINDING.conv_string(dotNetFileName);
        const resolvedFile = configFiles.find(f => f.name === fileName);
        return resolvedFile
          ? BINDING.js_typed_array_to_array(resolvedFile.content)
          : undefined;
      };
      async function getConfigBytes(file) {
        const response = await fetch(file, {
          method: "GET",
          credentials: "include",
          cache: "no-cache",
        });
        return new Uint8Array(await response.arrayBuffer());
      }
    }
  } // CONCATENATED MODULE: ./JSInitializers/JSInitializers.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  class JSInitializer {
    constructor() {
      this.afterStartedCallbacks = [];
    }
    async importInitializersAsync(initializerFiles, initializerArguments) {
      await Promise.all(
        initializerFiles.map(f => importAndInvokeInitializer(this, f)),
      );
      function adjustPath(path) {
        // This is the same we do in JS interop with the import callback
        const base = document.baseURI;
        path = base.endsWith("/") ? `${base}${path}` : `${base}/${path}`;
        return path;
      }
      async function importAndInvokeInitializer(jsInitializer, path) {
        const adjustedPath = adjustPath(path);
        const initializer = await import(
          /* webpackIgnore: true */ adjustedPath
        );
        if (initializer === undefined) {
          return;
        }
        const { beforeStart: beforeStart, afterStarted: afterStarted } =
          initializer;
        if (afterStarted) {
          jsInitializer.afterStartedCallbacks.push(afterStarted);
        }
        if (beforeStart) {
          return beforeStart(...initializerArguments);
        }
      }
    }
    async invokeAfterStartedCallbacks(blazor) {
      await rendererAttached;
      await Promise.all(
        this.afterStartedCallbacks.map(callback => callback(blazor)),
      );
    }
  } // CONCATENATED MODULE: ./JSInitializers/JSInitializers.WebAssembly.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.

  async function fetchAndInvokeInitializers(bootConfig, options) {
    const initializers = bootConfig.resources.libraryInitializers;
    const jsInitializer = new JSInitializer();
    if (initializers) {
      await jsInitializer.importInitializersAsync(Object.keys(initializers), [
        options,
        bootConfig.resources.extensions,
      ]);
    }
    return jsInitializer;
  } // CONCATENATED MODULE: ./Boot.WebAssembly.ts

  // Licensed to the .NET Foundation under one or more agreements.
  // The .NET Foundation licenses this file to you under the MIT license.
  /* eslint-disable array-element-newline */

  let started = false;
  async function boot(options) {
    console.log("booting", options);
    if (started) {
      throw new Error("Blazor has already started.");
    }
    started = true;
    Blazor._internal.applyHotReload = (
      id,
      metadataDelta,
      ilDelta,
      pdbDelta,
    ) => {
      DotNet.invokeMethod(
        "Microsoft.AspNetCore.Components.WebAssembly",
        "ApplyHotReloadDelta",
        id,
        metadataDelta,
        ilDelta,
        pdbDelta,
      );
    };
    Blazor._internal.getApplyUpdateCapabilities = () =>
      DotNet.invokeMethod(
        "Microsoft.AspNetCore.Components.WebAssembly",
        "GetApplyUpdateCapabilities",
      );
    // Configure JS interop
    Blazor._internal.invokeJSFromDotNet = invokeJSFromDotNet;
    Blazor._internal.endInvokeDotNetFromJS = endInvokeDotNetFromJS;
    Blazor._internal.receiveByteArray = receiveByteArray;
    Blazor._internal.retrieveByteArray = retrieveByteArray;
    // Configure environment for execution under Mono WebAssembly with shared-memory rendering
    const platform = setPlatform(monoPlatform);
    Blazor.platform = platform;
    Blazor._internal.renderBatch = (browserRendererId, batchAddress) => {
      // We're going to read directly from the .NET memory heap, so indicate to the platform
      // that we don't want anything to modify the memory contents during this time. Currently this
      // is only guaranteed by the fact that .NET code doesn't run during this time, but in the
      // future (when multithreading is implemented) we might need the .NET runtime to understand
      // that GC compaction isn't allowed during this critical section.
      const heapLock = monoPlatform.beginHeapLock();
      try {
        renderBatch(
          browserRendererId,
          new SharedMemoryRenderBatch(batchAddress),
        );
      } finally {
        heapLock.release();
      }
    };
    // Configure navigation via JS Interop
    const getBaseUri = Blazor._internal.navigationManager.getBaseURI;
    const getLocationHref = Blazor._internal.navigationManager.getLocationHref;
    Blazor._internal.navigationManager.getUnmarshalledBaseURI = () =>
      BINDING.js_string_to_mono_string(getBaseUri());
    Blazor._internal.navigationManager.getUnmarshalledLocationHref = () =>
      BINDING.js_string_to_mono_string(getLocationHref());
    Blazor._internal.navigationManager.listenForNavigationEvents(
      async (uri, state, intercepted) => {
        await DotNet.invokeMethodAsync(
          "Microsoft.AspNetCore.Components.WebAssembly",
          "NotifyLocationChanged",
          uri,
          state,
          intercepted,
        );
      },
      async (callId, uri, state, intercepted) => {
        const shouldContinueNavigation = await DotNet.invokeMethodAsync(
          "Microsoft.AspNetCore.Components.WebAssembly",
          "NotifyLocationChangingAsync",
          uri,
          state,
          intercepted,
        );
        Blazor._internal.navigationManager.endLocationChanging(
          callId,
          shouldContinueNavigation,
        );
      },
    );
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
    const jsInitializer = await fetchAndInvokeInitializers(
      bootConfigResult.bootConfig,
      candidateOptions,
    );
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
    // At this point .NET has been initialized (and has yielded), we can't await the promise becasue it will
    // only end when the app finishes running
    jsInitializer.invokeAfterStartedCallbacks(Blazor);
  }
  // obsolete, legacy, don't use for new code!
  function invokeJSFromDotNet(callInfo, arg0, arg1, arg2) {
    const functionIdentifier = monoPlatform.readStringField(callInfo, 0);
    const resultType = monoPlatform.readInt32Field(callInfo, 4);
    const marshalledCallArgsJson = monoPlatform.readStringField(callInfo, 8);
    const targetInstanceId = monoPlatform.readUint64Field(callInfo, 20);
    if (marshalledCallArgsJson !== null) {
      const marshalledCallAsyncHandle = monoPlatform.readUint64Field(
        callInfo,
        12,
      );
      if (marshalledCallAsyncHandle !== 0) {
        DotNet.jsCallDispatcher.beginInvokeJSFromDotNet(
          marshalledCallAsyncHandle,
          functionIdentifier,
          marshalledCallArgsJson,
          resultType,
          targetInstanceId,
        );
        return 0;
      } else {
        const resultJson = DotNet.jsCallDispatcher.invokeJSFromDotNet(
          functionIdentifier,
          marshalledCallArgsJson,
          resultType,
          targetInstanceId,
        );
        return resultJson === null
          ? 0
          : BINDING.js_string_to_mono_string(resultJson);
      }
    } else {
      const func = DotNet.jsCallDispatcher.findJSFunction(
        functionIdentifier,
        targetInstanceId,
      );
      const result = func.call(null, arg0, arg1, arg2);
      switch (resultType) {
        case DotNet.JSCallResultType.Default:
          return result;
        case DotNet.JSCallResultType.JSObjectReference:
          return DotNet.createJSObjectReference(result).__jsObjectId;
        case DotNet.JSCallResultType.JSStreamReference: {
          const streamReference = DotNet.createJSStreamReference(result);
          const resultJson = JSON.stringify(streamReference);
          return BINDING.js_string_to_mono_string(resultJson);
        }
        case DotNet.JSCallResultType.JSVoidResult:
          return null;
        default:
          throw new Error(`Invalid JS call result type '${resultType}'.`);
      }
    }
  }
  function endInvokeDotNetFromJS(callId, success, resultJsonOrErrorMessage) {
    const callIdString = BINDING.conv_string(callId);
    const successBool = success !== 0;
    const resultJsonOrErrorMessageString = BINDING.conv_string(
      resultJsonOrErrorMessage,
    );
    DotNet.jsCallDispatcher.endInvokeDotNetFromJS(
      callIdString,
      successBool,
      resultJsonOrErrorMessageString,
    );
  }
  function receiveByteArray(id, data) {
    const idLong = id;
    const dataByteArray = monoPlatform.toUint8Array(data);
    DotNet.jsCallDispatcher.receiveByteArray(idLong, dataByteArray);
  }
  function retrieveByteArray() {
    if (byteArrayBeingTransferred === null) {
      throw new Error("Byte array not available for transfer");
    }
    const typedArray = BINDING.js_typed_array_to_array(
      byteArrayBeingTransferred,
    );
    return typedArray;
  }
  Blazor.start = boot;
  if (shouldAutoStart() && false) {
    boot().catch(error => {
      if (typeof Module !== "undefined" && Module.printErr) {
        // Logs it, and causes the error UI to appear
        Module.printErr(error);
      } else {
        // The error must have happened so early we didn't yet set up the error UI, so just log to console
        console.error(error);
      }
    });
  }

  /******/
})();
