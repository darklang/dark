/******/ (() => { // webpackBootstrap
/******/ 	"use strict";
var __webpack_exports__ = {};

;// CONCATENATED MODULE: ../../../JSInterop/Microsoft.JSInterop.JS/src/dist/Microsoft.JSInterop.js
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// This is a single-file self-contained module to avoid the need for a Webpack build
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
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
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
                }
                else {
                    throw new Error(`Could not find '${identifier}' ('${segment}' was undefined).`);
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
        [windowJSObjectId]: new JSObject(window)
    };
    cachedJSObjectsById[windowJSObjectId]._cachedFunctions.set("import", (url) => {
        // In most cases developers will want to resolve dynamic imports relative to the base HREF.
        // However since we're the one calling the import keyword, they would be resolved relative to
        // this framework bundle URL. Fix this by providing an absolute URL.
        if (typeof url === "string" && url.startsWith("./")) {
            url = document.baseURI + url.substr(2);
        }
        return import(/* webpackIgnore: true */ url);
    });
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
        return invokePossibleInstanceMethod(assemblyName, methodIdentifier, null, args);
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
        return invokePossibleInstanceMethodAsync(assemblyName, methodIdentifier, null, args);
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
                [jsObjectIdKey]: nextJsObjectId
            };
            nextJsObjectId++;
            return result;
        }
        throw new Error(`Cannot create a JSObjectReference from the value '${jsObject}'.`);
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
        }
        else if (streamReference.buffer instanceof ArrayBuffer) {
            if (streamReference.byteLength === undefined) {
                throw new Error(`Cannot create a JSStreamReference from the value '${streamReference}' as it doesn't have a byteLength.`);
            }
            length = streamReference.byteLength;
        }
        else {
            throw new Error("Supplied value is not a typed array or blob.");
        }
        const result = {
            [jsStreamReferenceLengthKey]: length
        };
        try {
            const jsObjectReference = createJSObjectReference(streamReference);
            result[jsObjectIdKey] = jsObjectReference[jsObjectIdKey];
        }
        catch (error) {
            throw new Error(`Cannot create a JSStreamReference from the value '${streamReference}'.`);
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
        return json ? JSON.parse(json, (key, initialValue) => {
            // Invoke each reviver in order, passing the output from the previous reviver,
            // so that each one gets a chance to transform the value
            return jsonRevivers.reduce((latestValue, reviver) => reviver(key, latestValue), initialValue);
        }) : null;
    }
    function invokePossibleInstanceMethod(assemblyName, methodIdentifier, dotNetObjectId, args) {
        const dispatcher = getRequiredDispatcher();
        if (dispatcher.invokeDotNetFromJS) {
            const argsJson = stringifyArgs(args);
            const resultJson = dispatcher.invokeDotNetFromJS(assemblyName, methodIdentifier, dotNetObjectId, argsJson);
            return resultJson ? parseJsonWithRevivers(resultJson) : null;
        }
        throw new Error("The current dispatcher does not support synchronous calls from JS to .NET. Use invokeMethodAsync instead.");
    }
    function invokePossibleInstanceMethodAsync(assemblyName, methodIdentifier, dotNetObjectId, args) {
        if (assemblyName && dotNetObjectId) {
            throw new Error(`For instance method calls, assemblyName should be null. Received '${assemblyName}'.`);
        }
        const asyncCallId = nextAsyncCallId++;
        const resultPromise = new Promise((resolve, reject) => {
            pendingAsyncCalls[asyncCallId] = { resolve, reject };
        });
        try {
            const argsJson = stringifyArgs(args);
            getRequiredDispatcher().beginInvokeDotNetFromJS(asyncCallId, assemblyName, methodIdentifier, dotNetObjectId, argsJson);
        }
        catch (ex) {
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
            throw new Error(`There is no pending async call with ID ${asyncCallId}.`);
        }
        const asyncCall = pendingAsyncCalls[asyncCallId];
        delete pendingAsyncCalls[asyncCallId];
        if (success) {
            asyncCall.resolve(resultOrError);
        }
        else {
            asyncCall.reject(resultOrError);
        }
    }
    /**
     * Represents the type of result expected from a JS interop call.
     */
    // eslint-disable-next-line no-shadow
    let JSCallResultType;
    (function (JSCallResultType) {
        JSCallResultType[JSCallResultType["Default"] = 0] = "Default";
        JSCallResultType[JSCallResultType["JSObjectReference"] = 1] = "JSObjectReference";
        JSCallResultType[JSCallResultType["JSStreamReference"] = 2] = "JSStreamReference";
        JSCallResultType[JSCallResultType["JSVoidResult"] = 3] = "JSVoidResult";
    })(JSCallResultType = DotNet.JSCallResultType || (DotNet.JSCallResultType = {}));
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
        invokeJSFromDotNet: (identifier, argsJson, resultType, targetInstanceId) => {
            const returnValue = findJSFunction(identifier, targetInstanceId).apply(null, parseJsonWithRevivers(argsJson));
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
        beginInvokeJSFromDotNet: (asyncHandle, identifier, argsJson, resultType, targetInstanceId) => {
            // Coerce synchronous functions into async ones, plus treat
            // synchronous exceptions the same as async ones
            const promise = new Promise(resolve => {
                const synchronousResultOrPromise = findJSFunction(identifier, targetInstanceId).apply(null, parseJsonWithRevivers(argsJson));
                resolve(synchronousResultOrPromise);
            });
            // We only listen for a result if the caller wants to be notified about it
            if (asyncHandle) {
                // On completion, dispatch result back to .NET
                // Not using "await" because it codegens a lot of boilerplate
                promise.then(result => getRequiredDispatcher().endInvokeJSFromDotNet(asyncHandle, true, stringifyArgs([
                    asyncHandle,
                    true,
                    createJSCallResult(result, resultType)
                ])), error => getRequiredDispatcher().endInvokeJSFromDotNet(asyncHandle, false, JSON.stringify([
                    asyncHandle,
                    false,
                    formatError(error)
                ])));
            }
        },
        /**
         * Receives notification that an async call from JS to .NET has completed.
         * @param asyncCallId The identifier supplied in an earlier call to beginInvokeDotNetFromJS.
         * @param success A flag to indicate whether the operation completed successfully.
         * @param resultJsonOrExceptionMessage Either the operation result as JSON, or an error message.
         */
        endInvokeDotNetFromJS: (asyncCallId, success, resultJsonOrExceptionMessage) => {
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
            }
            else {
                // The receiver hasn't started waiting yet, so track a pre-completed entry it can attach to later
                const pendingStream = new PendingStream();
                pendingStream.resolve(stream);
                pendingDotNetToJSStreams.set(streamId, pendingStream);
            }
        }
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
        throw new Error(`JS object instance with ID ${targetInstanceId} does not exist (has it been disposed?).`);
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
            return invokePossibleInstanceMethod(null, methodIdentifier, this._id, args);
        }
        invokeMethodAsync(methodIdentifier, ...args) {
            return invokePossibleInstanceMethodAsync(null, methodIdentifier, this._id, args);
        }
        dispose() {
            const promise = invokePossibleInstanceMethodAsync(null, "__Dispose", this._id, null);
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
            }
            else if (value.hasOwnProperty(jsObjectIdKey)) {
                const id = value[jsObjectIdKey];
                const jsObject = cachedJSObjectsById[id];
                if (jsObject) {
                    return jsObject.getWrappedObject();
                }
                throw new Error(`JS object instance with Id '${id}' does not exist. It may have been disposed.`);
            }
            else if (value.hasOwnProperty(byteArrayRefKey)) {
                const index = value[byteArrayRefKey];
                const byteArray = byteArraysToBeRevived.get(index);
                if (byteArray === undefined) {
                    throw new Error(`Byte array index '${index}' does not exist.`);
                }
                byteArraysToBeRevived.delete(index);
                return byteArray;
            }
            else if (value.hasOwnProperty(dotNetStreamRefKey)) {
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
                this._streamPromise = pendingDotNetToJSStreams.get(streamId).streamPromise;
                pendingDotNetToJSStreams.delete(streamId);
            }
            else {
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
        }
        else if (value instanceof Uint8Array) {
            dotNetDispatcher.sendByteArray(nextByteArrayIndex, value);
            const jsonValue = { [byteArrayRefKey]: nextByteArrayIndex };
            nextByteArrayIndex++;
            return jsonValue;
        }
        return value;
    }
})(DotNet || (DotNet = {}));
//# sourceMappingURL=Microsoft.JSInterop.js.map
;// CONCATENATED MODULE: ./Rendering/RenderBatch/RenderBatch.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
var EditType;
(function (EditType) {
    // The values must be kept in sync with the .NET equivalent in RenderTreeEditType.cs
    EditType[EditType["prependFrame"] = 1] = "prependFrame";
    EditType[EditType["removeFrame"] = 2] = "removeFrame";
    EditType[EditType["setAttribute"] = 3] = "setAttribute";
    EditType[EditType["removeAttribute"] = 4] = "removeAttribute";
    EditType[EditType["updateText"] = 5] = "updateText";
    EditType[EditType["stepIn"] = 6] = "stepIn";
    EditType[EditType["stepOut"] = 7] = "stepOut";
    EditType[EditType["updateMarkup"] = 8] = "updateMarkup";
    EditType[EditType["permutationListEntry"] = 9] = "permutationListEntry";
    EditType[EditType["permutationListEnd"] = 10] = "permutationListEnd";
})(EditType || (EditType = {}));
var FrameType;
(function (FrameType) {
    // The values must be kept in sync with the .NET equivalent in RenderTreeFrameType.cs
    FrameType[FrameType["element"] = 1] = "element";
    FrameType[FrameType["text"] = 2] = "text";
    FrameType[FrameType["attribute"] = 3] = "attribute";
    FrameType[FrameType["component"] = 4] = "component";
    FrameType[FrameType["region"] = 5] = "region";
    FrameType[FrameType["elementReferenceCapture"] = 6] = "elementReferenceCapture";
    FrameType[FrameType["markup"] = 8] = "markup";
})(FrameType || (FrameType = {}));

;// CONCATENATED MODULE: ./Rendering/Events/EventFieldInfo.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
class EventFieldInfo {
    constructor(componentId, fieldValue) {
        this.componentId = componentId;
        this.fieldValue = fieldValue;
    }
    static fromEvent(componentId, event) {
        const elem = event.target;
        if (elem instanceof Element) {
            const fieldData = getFormFieldData(elem);
            if (fieldData) {
                return new EventFieldInfo(componentId, fieldData.value);
            }
        }
        // This event isn't happening on a form field that we can reverse-map back to some incoming attribute
        return null;
    }
}
function getFormFieldData(elem) {
    // The logic in here should be the inverse of the logic in BrowserRenderer's tryApplySpecialProperty.
    // That is, we're doing the reverse mapping, starting from an HTML property and reconstructing which
    // "special" attribute would have been mapped to that property.
    if (elem instanceof HTMLInputElement) {
        return (elem.type && elem.type.toLowerCase() === 'checkbox')
            ? { value: elem.checked }
            : { value: elem.value };
    }
    if (elem instanceof HTMLSelectElement || elem instanceof HTMLTextAreaElement) {
        return { value: elem.value };
    }
    return null;
}

;// CONCATENATED MODULE: ./Rendering/Events/EventTypes.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
const eventTypeRegistry = new Map();
const browserEventNamesToAliases = new Map();
const createBlankEventArgsOptions = { createEventArgs: () => ({}) };
const eventNameAliasRegisteredCallbacks = [];
function registerCustomEventType(eventName, options) {
    if (!options) {
        throw new Error('The options parameter is required.');
    }
    // There can't be more than one registration for the same event name because then we wouldn't
    // know which eventargs data to supply.
    if (eventTypeRegistry.has(eventName)) {
        throw new Error(`The event '${eventName}' is already registered.`);
    }
    // If applicable, register this as an alias of the given browserEventName
    if (options.browserEventName) {
        const aliasGroup = browserEventNamesToAliases.get(options.browserEventName);
        if (aliasGroup) {
            aliasGroup.push(eventName);
        }
        else {
            browserEventNamesToAliases.set(options.browserEventName, [eventName]);
        }
        // For developer convenience, it's allowed to register the custom event type *after*
        // some listeners for it are already present. Once the event name alias gets registered,
        // we have to notify any existing event delegators so they can update their delegated
        // events list.
        eventNameAliasRegisteredCallbacks.forEach(callback => callback(eventName, options.browserEventName));
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
    return (eventOptions === null || eventOptions === void 0 ? void 0 : eventOptions.browserEventName) || possibleAliasEventName;
}
function registerBuiltInEventType(eventNames, options) {
    eventNames.forEach(eventName => eventTypeRegistry.set(eventName, options));
}
registerBuiltInEventType(['input', 'change'], {
    createEventArgs: parseChangeEvent,
});
registerBuiltInEventType([
    'copy',
    'cut',
    'paste',
], {
    createEventArgs: e => parseClipboardEvent(e),
});
registerBuiltInEventType([
    'drag',
    'dragend',
    'dragenter',
    'dragleave',
    'dragover',
    'dragstart',
    'drop',
], {
    createEventArgs: e => parseDragEvent(e),
});
registerBuiltInEventType([
    'focus',
    'blur',
    'focusin',
    'focusout',
], {
    createEventArgs: e => parseFocusEvent(e),
});
registerBuiltInEventType([
    'keydown',
    'keyup',
    'keypress',
], {
    createEventArgs: e => parseKeyboardEvent(e),
});
registerBuiltInEventType([
    'contextmenu',
    'click',
    'mouseover',
    'mouseout',
    'mousemove',
    'mousedown',
    'mouseup',
    'mouseleave',
    'mouseenter',
    'dblclick',
], {
    createEventArgs: e => parseMouseEvent(e),
});
registerBuiltInEventType(['error'], {
    createEventArgs: e => parseErrorEvent(e),
});
registerBuiltInEventType([
    'loadstart',
    'timeout',
    'abort',
    'load',
    'loadend',
    'progress',
], {
    createEventArgs: e => parseProgressEvent(e),
});
registerBuiltInEventType([
    'touchcancel',
    'touchend',
    'touchmove',
    'touchenter',
    'touchleave',
    'touchstart',
], {
    createEventArgs: e => parseTouchEvent(e),
});
registerBuiltInEventType([
    'gotpointercapture',
    'lostpointercapture',
    'pointercancel',
    'pointerdown',
    'pointerenter',
    'pointerleave',
    'pointermove',
    'pointerout',
    'pointerover',
    'pointerup',
], {
    createEventArgs: e => parsePointerEvent(e),
});
registerBuiltInEventType(['wheel', 'mousewheel'], {
    createEventArgs: e => parseWheelEvent(e),
});
registerBuiltInEventType(['toggle'], createBlankEventArgsOptions);
function parseChangeEvent(event) {
    const element = event.target;
    if (isTimeBasedInput(element)) {
        const normalizedValue = normalizeTimeBasedValue(element);
        return { value: normalizedValue };
    }
    else if (isMultipleSelectInput(element)) {
        const selectElement = element;
        const selectedValues = Array.from(selectElement.options)
            .filter(option => option.selected)
            .map(option => option.value);
        return { value: selectedValues };
    }
    else {
        const targetIsCheckbox = isCheckbox(element);
        const newValue = targetIsCheckbox ? !!element['checked'] : element['value'];
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
        dataTransfer: event.dataTransfer ? {
            dropEffect: event.dataTransfer.dropEffect,
            effectAllowed: event.dataTransfer.effectAllowed,
            files: Array.from(event.dataTransfer.files).map(f => f.name),
            items: Array.from(event.dataTransfer.items).map(i => ({ kind: i.kind, type: i.type })),
            types: event.dataTransfer.types,
        } : null,
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
    return !!element && element.tagName === 'INPUT' && element.getAttribute('type') === 'checkbox';
}
const timeBasedInputs = [
    'date',
    'datetime-local',
    'month',
    'time',
    'week',
];
function isTimeBasedInput(element) {
    return timeBasedInputs.indexOf(element.getAttribute('type')) !== -1;
}
function isMultipleSelectInput(element) {
    return element instanceof HTMLSelectElement && element.type === 'select-multiple';
}
function normalizeTimeBasedValue(element) {
    const value = element.value;
    const type = element.type;
    switch (type) {
        case 'date':
        case 'month':
            return value;
        case 'datetime-local':
            return value.length === 16 ? value + ':00' : value; // Convert yyyy-MM-ddTHH:mm to yyyy-MM-ddTHH:mm:00
        case 'time':
            return value.length === 5 ? value + ':00' : value; // Convert hh:mm to hh:mm:00
        case 'week':
            // For now we are not going to normalize input type week as it is not trivial
            return value;
    }
    throw new Error(`Invalid element type '${type}'.`);
}

;// CONCATENATED MODULE: ./Rendering/JSRootComponents.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

const pendingRootComponentContainerNamePrefix = '__bl-dynamic-root:';
const pendingRootComponentContainers = new Map();
let nextPendingDynamicRootComponentIdentifier = 0;
let manager;
let jsComponentParametersByIdentifier;
// These are the public APIs at Blazor.rootComponents.*
const RootComponentsFunctions = {
    async add(toElement, componentIdentifier, initialParameters) {
        if (!initialParameters) {
            throw new Error('initialParameters must be an object, even if empty.');
        }
        // Track the container so we can use it when the component gets attached to the document via a selector
        const containerIdentifier = pendingRootComponentContainerNamePrefix + (++nextPendingDynamicRootComponentIdentifier).toString();
        pendingRootComponentContainers.set(containerIdentifier, toElement);
        // Instruct .NET to add and render the new root component
        const componentId = await getRequiredManager().invokeMethodAsync('AddRootComponent', componentIdentifier, containerIdentifier);
        const component = new DynamicRootComponent(componentId, jsComponentParametersByIdentifier[componentIdentifier]);
        await component.setParameters(initialParameters);
        return component;
    },
};
function getAndRemovePendingRootComponentContainer(containerIdentifier) {
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
            if (parameter.type === 'eventcallback') {
                this._jsEventCallbackWrappers.set(parameter.name.toLowerCase(), new EventCallbackWrapper());
            }
        }
    }
    setParameters(parameters) {
        const mappedParameters = {};
        const entries = Object.entries(parameters || {});
        const parameterCount = entries.length;
        for (const [key, value] of entries) {
            const callbackWrapper = this._jsEventCallbackWrappers.get(key.toLowerCase());
            if (!callbackWrapper || !value) {
                mappedParameters[key] = value;
                continue;
            }
            callbackWrapper.setCallback(value);
            mappedParameters[key] = callbackWrapper.getJSObjectReference();
        }
        return getRequiredManager().invokeMethodAsync('SetRootComponentParameters', this._componentId, parameterCount, mappedParameters);
    }
    async dispose() {
        if (this._componentId !== null) {
            await getRequiredManager().invokeMethodAsync('RemoveRootComponent', this._componentId);
            this._componentId = null; // Ensure it can't be used again
            for (const jsEventCallbackWrapper of this._jsEventCallbackWrappers.values()) {
                jsEventCallbackWrapper.dispose();
            }
        }
    }
}
// Called by the framework
function enableJSRootComponents(managerInstance, jsComponentParameters, jsComponentInitializers) {
    if (manager) {
        // This will only happen in very nonstandard cases where someone has multiple hosts.
        // It's up to the developer to ensure that only one of them enables dynamic root components.
        throw new Error('Dynamic root components have already been enabled.');
    }
    manager = managerInstance;
    jsComponentParametersByIdentifier = jsComponentParameters;
    // Call the registered initializers. This is an arbitrary subset of the JS component types that are registered
    // on the .NET side - just those of them that require some JS-side initialization (e.g., to register them
    // as custom elements).
    for (const [initializerIdentifier, componentIdentifiers] of Object.entries(jsComponentInitializers)) {
        const initializerFunc = DotNet.jsCallDispatcher.findJSFunction(initializerIdentifier, 0);
        for (const componentIdentifier of componentIdentifiers) {
            const parameters = jsComponentParameters[componentIdentifier];
            initializerFunc(componentIdentifier, parameters);
        }
    }
}
function getRequiredManager() {
    if (!manager) {
        throw new Error('Dynamic root components have not been enabled in this application.');
    }
    return manager;
}

;// CONCATENATED MODULE: ./Rendering/WebRendererInteropMethods.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

const interopMethodsByRenderer = new Map();
let resolveRendererAttached;
const rendererAttached = new Promise((resolve) => {
    resolveRendererAttached = resolve;
});
function attachWebRendererInterop(rendererId, interopMethods, jsComponentParameters, jsComponentInitializers) {
    if (interopMethodsByRenderer.has(rendererId)) {
        throw new Error(`Interop methods are already registered for renderer ${rendererId}`);
    }
    interopMethodsByRenderer.set(rendererId, interopMethods);
    if (Object.keys(jsComponentParameters).length > 0) {
        const manager = getInteropMethods(rendererId);
        enableJSRootComponents(manager, jsComponentParameters, jsComponentInitializers);
    }
    resolveRendererAttached();
}
function dispatchEvent(browserRendererId, eventDescriptor, eventArgs) {
    return dispatchEventMiddleware(browserRendererId, eventDescriptor.eventHandlerId, () => {
        const interopMethods = getInteropMethods(browserRendererId);
        return interopMethods.invokeMethodAsync('DispatchEventAsync', eventDescriptor, eventArgs);
    });
}
function getInteropMethods(rendererId) {
    const interopMethods = interopMethodsByRenderer.get(rendererId);
    if (!interopMethods) {
        throw new Error(`No interop methods are registered for renderer ${rendererId}`);
    }
    return interopMethods;
}
let dispatchEventMiddleware = (browserRendererId, eventHandlerId, continuation) => continuation();
function setDispatchEventMiddleware(middleware) {
    dispatchEventMiddleware = middleware;
}

;// CONCATENATED MODULE: ./Rendering/Events/EventDelegator.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.



const nonBubblingEvents = toLookup([
    'abort',
    'blur',
    'canplay',
    'canplaythrough',
    'change',
    'cuechange',
    'durationchange',
    'emptied',
    'ended',
    'error',
    'focus',
    'load',
    'loadeddata',
    'loadedmetadata',
    'loadend',
    'loadstart',
    'mouseenter',
    'mouseleave',
    'pause',
    'play',
    'playing',
    'progress',
    'ratechange',
    'reset',
    'scroll',
    'seeked',
    'seeking',
    'stalled',
    'submit',
    'suspend',
    'timeupdate',
    'toggle',
    'unload',
    'volumechange',
    'waiting',
    'DOMNodeInsertedIntoDocument',
    'DOMNodeRemovedFromDocument',
]);
const alwaysPreventDefaultEvents = { submit: true };
const disableableEventNames = toLookup([
    'click',
    'dblclick',
    'mousedown',
    'mousemove',
    'mouseup',
]);
// Responsible for adding/removing the eventInfo on an expando property on DOM elements, and
// calling an EventInfoStore that deals with registering/unregistering the underlying delegated
// event listeners as required (and also maps actual events back to the given callback).
class EventDelegator {
    constructor(browserRendererId) {
        this.browserRendererId = browserRendererId;
        this.afterClickCallbacks = [];
        const eventDelegatorId = ++EventDelegator.nextEventDelegatorId;
        this.eventsCollectionKey = `_blazorEvents_${eventDelegatorId}`;
        this.eventInfoStore = new EventInfoStore(this.onGlobalEvent.bind(this));
    }
    setListener(element, eventName, eventHandlerId, renderingComponentId) {
        const infoForElement = this.getEventHandlerInfosForElement(element, true);
        const existingHandler = infoForElement.getHandler(eventName);
        if (existingHandler) {
            // We can cheaply update the info on the existing object and don't need any other housekeeping
            // Note that this also takes care of updating the eventHandlerId on the existing handler object
            this.eventInfoStore.update(existingHandler.eventHandlerId, eventHandlerId);
        }
        else {
            // Go through the whole flow which might involve registering a new global handler
            const newInfo = { element, eventName, eventHandlerId, renderingComponentId };
            this.eventInfoStore.add(newInfo);
            infoForElement.setHandler(eventName, newInfo);
        }
    }
    getHandler(eventHandlerId) {
        return this.eventInfoStore.get(eventHandlerId);
    }
    removeListener(eventHandlerId) {
        // This method gets called whenever the .NET-side code reports that a certain event handler
        // has been disposed. However we will already have disposed the info about that handler if
        // the eventHandlerId for the (element,eventName) pair was replaced during diff application.
        const info = this.eventInfoStore.remove(eventHandlerId);
        if (info) {
            // Looks like this event handler wasn't already disposed
            // Remove the associated data from the DOM element
            const element = info.element;
            const elementEventInfos = this.getEventHandlerInfosForElement(element, false);
            if (elementEventInfos) {
                elementEventInfos.removeHandler(info.eventName);
            }
        }
    }
    notifyAfterClick(callback) {
        // This is extremely special-case. It's needed so that navigation link click interception
        // can be sure to run *after* our synthetic bubbling process. If a need arises, we can
        // generalise this, but right now it's a purely internal detail.
        this.afterClickCallbacks.push(callback);
        this.eventInfoStore.addGlobalListener('click'); // Ensure we always listen for this
    }
    setStopPropagation(element, eventName, value) {
        const infoForElement = this.getEventHandlerInfosForElement(element, true);
        infoForElement.stopPropagation(eventName, value);
    }
    setPreventDefault(element, eventName, value) {
        const infoForElement = this.getEventHandlerInfosForElement(element, true);
        infoForElement.preventDefault(eventName, value);
    }
    onGlobalEvent(evt) {
        if (!(evt.target instanceof Element)) {
            return;
        }
        // Always dispatch to any listeners for the original underlying browser event name
        this.dispatchGlobalEventToAllElements(evt.type, evt);
        // If this event name has aliases, dispatch for those listeners too
        const eventNameAliases = getEventNameAliases(evt.type);
        eventNameAliases && eventNameAliases.forEach(alias => this.dispatchGlobalEventToAllElements(alias, evt));
        // Special case for navigation interception
        if (evt.type === 'click') {
            this.afterClickCallbacks.forEach(callback => callback(evt));
        }
    }
    dispatchGlobalEventToAllElements(eventName, browserEvent) {
        // Note that 'eventName' can be an alias. For example, eventName may be 'click.special'
        // while browserEvent.type may be 'click'.
        // Use the event's 'path' rather than the chain of parent nodes, since the path gives
        // visibility into shadow roots.
        const path = browserEvent.composedPath();
        // Scan up the element hierarchy, looking for any matching registered event handlers
        let candidateEventTarget = path.shift();
        let eventArgs = null; // Populate lazily
        let eventArgsIsPopulated = false;
        const eventIsNonBubbling = Object.prototype.hasOwnProperty.call(nonBubblingEvents, eventName);
        let stopPropagationWasRequested = false;
        while (candidateEventTarget) {
            const candidateElement = candidateEventTarget;
            const handlerInfos = this.getEventHandlerInfosForElement(candidateElement, false);
            if (handlerInfos) {
                const handlerInfo = handlerInfos.getHandler(eventName);
                if (handlerInfo && !eventIsDisabledOnElement(candidateElement, browserEvent.type)) {
                    // We are going to raise an event for this element, so prepare info needed by the .NET code
                    if (!eventArgsIsPopulated) {
                        const eventOptionsIfRegistered = getEventTypeOptions(eventName);
                        // For back-compat, if there's no registered createEventArgs, we supply empty event args (not null).
                        // But if there is a registered createEventArgs, it can supply anything (including null).
                        eventArgs = (eventOptionsIfRegistered === null || eventOptionsIfRegistered === void 0 ? void 0 : eventOptionsIfRegistered.createEventArgs)
                            ? eventOptionsIfRegistered.createEventArgs(browserEvent)
                            : {};
                        eventArgsIsPopulated = true;
                    }
                    // For certain built-in events, having any .NET handler implicitly means we will prevent
                    // the browser's default behavior. This has to be based on the original browser event type name,
                    // not any alias (e.g., if you create a custom 'submit' variant, it should still preventDefault).
                    if (Object.prototype.hasOwnProperty.call(alwaysPreventDefaultEvents, browserEvent.type)) {
                        browserEvent.preventDefault();
                    }
                    dispatchEvent(this.browserRendererId, {
                        eventHandlerId: handlerInfo.eventHandlerId,
                        eventName: eventName,
                        eventFieldInfo: EventFieldInfo.fromEvent(handlerInfo.renderingComponentId, browserEvent),
                    }, eventArgs);
                }
                if (handlerInfos.stopPropagation(eventName)) {
                    stopPropagationWasRequested = true;
                }
                if (handlerInfos.preventDefault(eventName)) {
                    browserEvent.preventDefault();
                }
            }
            candidateEventTarget = (eventIsNonBubbling || stopPropagationWasRequested) ? undefined : path.shift();
        }
    }
    getEventHandlerInfosForElement(element, createIfNeeded) {
        if (Object.prototype.hasOwnProperty.call(element, this.eventsCollectionKey)) {
            return element[this.eventsCollectionKey];
        }
        else if (createIfNeeded) {
            return (element[this.eventsCollectionKey] = new EventHandlerInfosForElement());
        }
        else {
            return null;
        }
    }
}
EventDelegator.nextEventDelegatorId = 0;
// Responsible for adding and removing the global listener when the number of listeners
// for a given event name changes between zero and nonzero
class EventInfoStore {
    constructor(globalListener) {
        this.globalListener = globalListener;
        this.infosByEventHandlerId = {};
        this.countByEventName = {};
        eventNameAliasRegisteredCallbacks.push(this.handleEventNameAliasAdded.bind(this));
    }
    add(info) {
        if (this.infosByEventHandlerId[info.eventHandlerId]) {
            // Should never happen, but we want to know if it does
            throw new Error(`Event ${info.eventHandlerId} is already tracked`);
        }
        this.infosByEventHandlerId[info.eventHandlerId] = info;
        this.addGlobalListener(info.eventName);
    }
    get(eventHandlerId) {
        return this.infosByEventHandlerId[eventHandlerId];
    }
    addGlobalListener(eventName) {
        // If this event name is an alias, update the global listener for the corresponding browser event
        eventName = getBrowserEventName(eventName);
        if (Object.prototype.hasOwnProperty.call(this.countByEventName, eventName)) {
            this.countByEventName[eventName]++;
        }
        else {
            this.countByEventName[eventName] = 1;
            // To make delegation work with non-bubbling events, register a 'capture' listener.
            // We preserve the non-bubbling behavior by only dispatching such events to the targeted element.
            const useCapture = Object.prototype.hasOwnProperty.call(nonBubblingEvents, eventName);
            document.addEventListener(eventName, this.globalListener, useCapture);
        }
    }
    update(oldEventHandlerId, newEventHandlerId) {
        if (Object.prototype.hasOwnProperty.call(this.infosByEventHandlerId, newEventHandlerId)) {
            // Should never happen, but we want to know if it does
            throw new Error(`Event ${newEventHandlerId} is already tracked`);
        }
        // Since we're just updating the event handler ID, there's no need to update the global counts
        const info = this.infosByEventHandlerId[oldEventHandlerId];
        delete this.infosByEventHandlerId[oldEventHandlerId];
        info.eventHandlerId = newEventHandlerId;
        this.infosByEventHandlerId[newEventHandlerId] = info;
    }
    remove(eventHandlerId) {
        const info = this.infosByEventHandlerId[eventHandlerId];
        if (info) {
            delete this.infosByEventHandlerId[eventHandlerId];
            // If this event name is an alias, update the global listener for the corresponding browser event
            const eventName = getBrowserEventName(info.eventName);
            if (--this.countByEventName[eventName] === 0) {
                delete this.countByEventName[eventName];
                document.removeEventListener(eventName, this.globalListener);
            }
        }
        return info;
    }
    handleEventNameAliasAdded(aliasEventName, browserEventName) {
        // If an event name alias gets registered later, we need to update the global listener
        // registrations to match. This makes it equivalent to the alias having been registered
        // before the elements with event handlers got rendered.
        if (Object.prototype.hasOwnProperty.call(this.countByEventName, aliasEventName)) {
            // Delete old
            const countByAliasEventName = this.countByEventName[aliasEventName];
            delete this.countByEventName[aliasEventName];
            document.removeEventListener(aliasEventName, this.globalListener);
            // Ensure corresponding count is added to new
            this.addGlobalListener(browserEventName);
            this.countByEventName[browserEventName] += countByAliasEventName - 1;
        }
    }
}
class EventHandlerInfosForElement {
    constructor() {
        // Although we *could* track multiple event handlers per (element, eventName) pair
        // (since they have distinct eventHandlerId values), there's no point doing so because
        // our programming model is that you declare event handlers as attributes. An element
        // can only have one attribute with a given name, hence only one event handler with
        // that name at any one time.
        // So to keep things simple, only track one EventHandlerInfo per (element, eventName)
        this.handlers = {};
        this.preventDefaultFlags = null;
        this.stopPropagationFlags = null;
    }
    getHandler(eventName) {
        return Object.prototype.hasOwnProperty.call(this.handlers, eventName) ? this.handlers[eventName] : null;
    }
    setHandler(eventName, handler) {
        this.handlers[eventName] = handler;
    }
    removeHandler(eventName) {
        delete this.handlers[eventName];
    }
    preventDefault(eventName, setValue) {
        if (setValue !== undefined) {
            this.preventDefaultFlags = this.preventDefaultFlags || {};
            this.preventDefaultFlags[eventName] = setValue;
        }
        return this.preventDefaultFlags ? this.preventDefaultFlags[eventName] : false;
    }
    stopPropagation(eventName, setValue) {
        if (setValue !== undefined) {
            this.stopPropagationFlags = this.stopPropagationFlags || {};
            this.stopPropagationFlags[eventName] = setValue;
        }
        return this.stopPropagationFlags ? this.stopPropagationFlags[eventName] : false;
    }
}
function toLookup(items) {
    const result = {};
    items.forEach(value => {
        result[value] = true;
    });
    return result;
}
function eventIsDisabledOnElement(element, rawBrowserEventName) {
    // We want to replicate the normal DOM event behavior that, for 'interactive' elements
    // with a 'disabled' attribute, certain mouse events are suppressed
    return (element instanceof HTMLButtonElement || element instanceof HTMLInputElement || element instanceof HTMLTextAreaElement || element instanceof HTMLSelectElement)
        && Object.prototype.hasOwnProperty.call(disableableEventNames, rawBrowserEventName)
        && element.disabled;
}

;// CONCATENATED MODULE: ./Rendering/LogicalElements.ts
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
const logicalChildrenPropname = createSymbolOrFallback('_blazorLogicalChildren');
const logicalParentPropname = createSymbolOrFallback('_blazorLogicalParent');
const logicalEndSiblingPropname = createSymbolOrFallback('_blazorLogicalEnd');
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
    const parentLogicalElement = toLogicalElement(parent, /* allow existing contents */ true);
    const children = getLogicalChildrenArray(parentLogicalElement);
    Array.from(parent.childNodes).forEach(n => children.push(n));
    start[logicalParentPropname] = parentLogicalElement;
    // We might not have an end comment in the case of non-prerendered components.
    if (end) {
        start[logicalEndSiblingPropname] = end;
        toLogicalElement(end);
    }
    return toLogicalElement(start);
}
function toLogicalElement(element, allowExistingContents) {
    // Normally it's good to assert that the element has started empty, because that's the usual
    // situation and we probably have a bug if it's not. But for the element that contain prerendered
    // root components, we want to let them keep their content until we replace it.
    if (element.childNodes.length > 0 && !allowExistingContents) {
        throw new Error('New logical elements must start empty, or allowExistingContents must be true');
    }
    if (!(logicalChildrenPropname in element)) { // If it's already a logical element, leave it alone
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
    const containerElement = document.createComment('!');
    insertLogicalChild(containerElement, parent, childIndex);
    return containerElement;
}
function insertLogicalChild(child, parent, childIndex) {
    const childAsLogicalElement = child;
    if (child instanceof Comment) {
        const existingGrandchildren = getLogicalChildrenArray(childAsLogicalElement);
        if (existingGrandchildren && getLogicalChildrenArray(childAsLogicalElement).length > 0) {
            // There's nothing to stop us implementing support for this scenario, and it's not difficult
            // (after inserting 'child' itself, also iterate through its logical children and physically
            // put them as following-siblings in the DOM). However there's no scenario that requires it
            // presently, so if we did implement it there'd be no good way to have tests for it.
            throw new Error('Not implemented: inserting non-empty logical container');
        }
    }
    if (getLogicalParent(childAsLogicalElement)) {
        // Likewise, we could easily support this scenario too (in this 'if' block, just splice
        // out 'child' from the logical children array of its previous logical parent by using
        // Array.prototype.indexOf to determine its previous sibling index).
        // But again, since there's not currently any scenario that would use it, we would not
        // have any test coverage for such an implementation.
        throw new Error('Not implemented: moving existing logical children');
    }
    const newSiblings = getLogicalChildrenArray(parent);
    if (childIndex < newSiblings.length) {
        // Insert
        const nextSibling = newSiblings[childIndex];
        nextSibling.parentNode.insertBefore(child, nextSibling);
        newSiblings.splice(childIndex, 0, childAsLogicalElement);
    }
    else {
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
    return closestElement.namespaceURI === 'http://www.w3.org/2000/svg' && closestElement['tagName'] !== 'foreignObject';
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
    permutationList.forEach((listEntry) => {
        listEntry.moveRangeStart = siblings[listEntry.fromSiblingIndex];
        listEntry.moveRangeEnd = findLastDomNodeInRange(listEntry.moveRangeStart);
    });
    // Phase 2: insert markers
    permutationList.forEach((listEntry) => {
        const marker = document.createComment('marker');
        listEntry.moveToBeforeMarker = marker;
        const insertBeforeNode = siblings[listEntry.toSiblingIndex + 1];
        if (insertBeforeNode) {
            insertBeforeNode.parentNode.insertBefore(marker, insertBeforeNode);
        }
        else {
            appendDomNode(marker, parent);
        }
    });
    // Phase 3: move descendants & remove markers
    permutationList.forEach((listEntry) => {
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
            }
            else {
                nextToMove = nextNext;
            }
        }
        parentDomNode.removeChild(insertBefore);
    });
    // Phase 4: update siblings index
    permutationList.forEach((listEntry) => {
        siblings[listEntry.toSiblingIndex] = listEntry.moveRangeStart;
    });
}
function getClosestDomElement(logicalElement) {
    if (logicalElement instanceof Element || logicalElement instanceof DocumentFragment) {
        return logicalElement;
    }
    else if (logicalElement instanceof Comment) {
        return logicalElement.parentNode;
    }
    else {
        throw new Error('Not a valid logical element');
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
    }
    else if (parent instanceof Comment) {
        const parentLogicalNextSibling = getLogicalNextSibling(parent);
        if (parentLogicalNextSibling) {
            // Since the parent has a logical next-sibling, its appended child goes right before that
            parentLogicalNextSibling.parentNode.insertBefore(child, parentLogicalNextSibling);
        }
        else {
            // Since the parent has no logical next-sibling, keep recursing upwards until we find
            // a logical ancestor that does have a next-sibling or is a physical element.
            appendDomNode(child, getLogicalParent(parent));
        }
    }
    else {
        // Should never happen
        throw new Error(`Cannot append node because the parent is not a valid logical element. Parent: ${parent}`);
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
    }
    else {
        // Harder case: there's no logical next-sibling, so recurse upwards until we find
        // a logical ancestor that does have one, or a physical element
        const logicalParent = getLogicalParent(element);
        return logicalParent instanceof Element || logicalParent instanceof DocumentFragment
            ? logicalParent.lastChild
            : findLastDomNodeInRange(logicalParent);
    }
}
function createSymbolOrFallback(fallback) {
    return typeof Symbol === 'function' ? Symbol() : fallback;
}

;// CONCATENATED MODULE: ./Rendering/ElementReferenceCapture.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

function applyCaptureIdToElement(element, referenceCaptureId) {
    element.setAttribute(getCaptureIdAttributeName(referenceCaptureId), '');
}
function getElementByCaptureId(referenceCaptureId) {
    const selector = `[${getCaptureIdAttributeName(referenceCaptureId)}]`;
    return document.querySelector(selector);
}
function getCaptureIdAttributeName(referenceCaptureId) {
    return `_bl_${referenceCaptureId}`;
}
// Support receiving ElementRef instances as args in interop calls
const elementRefKey = '__internalId'; // Keep in sync with ElementRef.cs
DotNet.attachReviver((key, value) => {
    if (value && typeof value === 'object' && Object.prototype.hasOwnProperty.call(value, elementRefKey) && typeof value[elementRefKey] === 'string') {
        return getElementByCaptureId(value[elementRefKey]);
    }
    else {
        return value;
    }
});

;// CONCATENATED MODULE: ./Rendering/BrowserRenderer.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.





const deferredValuePropname = '_blazorDeferredValue';
const sharedTemplateElemForParsing = document.createElement('template');
const sharedSvgElemForParsing = document.createElementNS('http://www.w3.org/2000/svg', 'g');
const elementsToClearOnRootComponentRender = {};
const internalAttributeNamePrefix = '__internal_';
const eventPreventDefaultAttributeNamePrefix = 'preventDefault_';
const eventStopPropagationAttributeNamePrefix = 'stopPropagation_';
class BrowserRenderer {
    constructor(browserRendererId) {
        this.rootComponentIds = new Set();
        this.childComponentLocations = {};
        this.eventDelegator = new EventDelegator(browserRendererId);
        // We don't yet know whether or not navigation interception will be enabled, but in case it will be,
        // we wire up the navigation manager to the event delegator so it has the option to participate
        // in the synthetic event bubbling process later
        attachToEventDelegator(this.eventDelegator);
    }
    attachRootComponentToLogicalElement(componentId, element, appendContent) {
        this.attachComponentToElement(componentId, element);
        this.rootComponentIds.add(componentId);
        // If we want to preserve existing HTML content of the root element, we don't apply the mechanism for
        // clearing existing children. Rendered content will then append rather than replace the existing HTML content.
        if (!appendContent) {
            elementsToClearOnRootComponentRender[componentId] = element;
        }
    }
    updateComponent(batch, componentId, edits, referenceFrames) {
        var _a;
        const element = this.childComponentLocations[componentId];
        if (!element) {
            throw new Error(`No element is currently associated with component ${componentId}`);
        }
        // On the first render for each root component, clear any existing content (e.g., prerendered)
        const rootElementToClear = elementsToClearOnRootComponentRender[componentId];
        if (rootElementToClear) {
            const rootElementToClearEnd = getLogicalSiblingEnd(rootElementToClear);
            delete elementsToClearOnRootComponentRender[componentId];
            if (!rootElementToClearEnd) {
                clearElement(rootElementToClear);
            }
            else {
                clearBetween(rootElementToClear, rootElementToClearEnd);
            }
        }
        const ownerDocument = (_a = getClosestDomElement(element)) === null || _a === void 0 ? void 0 : _a.getRootNode();
        const activeElementBefore = ownerDocument && ownerDocument.activeElement;
        this.applyEdits(batch, componentId, element, 0, edits, referenceFrames);
        // Try to restore focus in case it was lost due to an element move
        if ((activeElementBefore instanceof HTMLElement) && ownerDocument && ownerDocument.activeElement !== activeElementBefore) {
            activeElementBefore.focus();
        }
    }
    disposeComponent(componentId) {
        if (this.rootComponentIds.delete(componentId)) {
            // When disposing a root component, the container element won't be removed from the DOM (because there's
            // no parent to remove that child), so we empty it to restore it to the state it was in before the root
            // component was added.
            emptyLogicalElement(this.childComponentLocations[componentId]);
        }
        delete this.childComponentLocations[componentId];
    }
    disposeEventHandler(eventHandlerId) {
        this.eventDelegator.removeListener(eventHandlerId);
    }
    attachComponentToElement(componentId, element) {
        this.childComponentLocations[componentId] = element;
    }
    applyEdits(batch, componentId, parent, childIndex, edits, referenceFrames) {
        let currentDepth = 0;
        let childIndexAtCurrentDepth = childIndex;
        let permutationList;
        const arrayBuilderSegmentReader = batch.arrayBuilderSegmentReader;
        const editReader = batch.editReader;
        const frameReader = batch.frameReader;
        const editsValues = arrayBuilderSegmentReader.values(edits);
        const editsOffset = arrayBuilderSegmentReader.offset(edits);
        const editsLength = arrayBuilderSegmentReader.count(edits);
        const maxEditIndexExcl = editsOffset + editsLength;
        for (let editIndex = editsOffset; editIndex < maxEditIndexExcl; editIndex++) {
            const edit = batch.diffReader.editsEntry(editsValues, editIndex);
            const editType = editReader.editType(edit);
            switch (editType) {
                case EditType.prependFrame: {
                    const frameIndex = editReader.newTreeIndex(edit);
                    const frame = batch.referenceFramesEntry(referenceFrames, frameIndex);
                    const siblingIndex = editReader.siblingIndex(edit);
                    this.insertFrame(batch, componentId, parent, childIndexAtCurrentDepth + siblingIndex, referenceFrames, frame, frameIndex);
                    break;
                }
                case EditType.removeFrame: {
                    const siblingIndex = editReader.siblingIndex(edit);
                    removeLogicalChild(parent, childIndexAtCurrentDepth + siblingIndex);
                    break;
                }
                case EditType.setAttribute: {
                    const frameIndex = editReader.newTreeIndex(edit);
                    const frame = batch.referenceFramesEntry(referenceFrames, frameIndex);
                    const siblingIndex = editReader.siblingIndex(edit);
                    const element = getLogicalChild(parent, childIndexAtCurrentDepth + siblingIndex);
                    if (element instanceof Element) {
                        this.applyAttribute(batch, componentId, element, frame);
                    }
                    else {
                        throw new Error('Cannot set attribute on non-element child');
                    }
                    break;
                }
                case EditType.removeAttribute: {
                    // Note that we don't have to dispose the info we track about event handlers here, because the
                    // disposed event handler IDs are delivered separately (in the 'disposedEventHandlerIds' array)
                    const siblingIndex = editReader.siblingIndex(edit);
                    const element = getLogicalChild(parent, childIndexAtCurrentDepth + siblingIndex);
                    if (element instanceof HTMLElement) {
                        const attributeName = editReader.removedAttributeName(edit);
                        // First try to remove any special property we use for this attribute
                        if (!this.tryApplySpecialProperty(batch, element, attributeName, null)) {
                            // If that's not applicable, it's a regular DOM attribute so remove that
                            element.removeAttribute(attributeName);
                        }
                    }
                    else {
                        throw new Error('Cannot remove attribute from non-element child');
                    }
                    break;
                }
                case EditType.updateText: {
                    const frameIndex = editReader.newTreeIndex(edit);
                    const frame = batch.referenceFramesEntry(referenceFrames, frameIndex);
                    const siblingIndex = editReader.siblingIndex(edit);
                    const textNode = getLogicalChild(parent, childIndexAtCurrentDepth + siblingIndex);
                    if (textNode instanceof Text) {
                        textNode.textContent = frameReader.textContent(frame);
                    }
                    else {
                        throw new Error('Cannot set text content on non-text child');
                    }
                    break;
                }
                case EditType.updateMarkup: {
                    const frameIndex = editReader.newTreeIndex(edit);
                    const frame = batch.referenceFramesEntry(referenceFrames, frameIndex);
                    const siblingIndex = editReader.siblingIndex(edit);
                    removeLogicalChild(parent, childIndexAtCurrentDepth + siblingIndex);
                    this.insertMarkup(batch, parent, childIndexAtCurrentDepth + siblingIndex, frame);
                    break;
                }
                case EditType.stepIn: {
                    const siblingIndex = editReader.siblingIndex(edit);
                    parent = getLogicalChild(parent, childIndexAtCurrentDepth + siblingIndex);
                    currentDepth++;
                    childIndexAtCurrentDepth = 0;
                    break;
                }
                case EditType.stepOut: {
                    parent = getLogicalParent(parent);
                    currentDepth--;
                    childIndexAtCurrentDepth = currentDepth === 0 ? childIndex : 0; // The childIndex is only ever nonzero at zero depth
                    break;
                }
                case EditType.permutationListEntry: {
                    permutationList = permutationList || [];
                    permutationList.push({
                        fromSiblingIndex: childIndexAtCurrentDepth + editReader.siblingIndex(edit),
                        toSiblingIndex: childIndexAtCurrentDepth + editReader.moveToSiblingIndex(edit),
                    });
                    break;
                }
                case EditType.permutationListEnd: {
                    permuteLogicalChildren(parent, permutationList);
                    permutationList = undefined;
                    break;
                }
                default: {
                    const unknownType = editType; // Compile-time verification that the switch was exhaustive
                    throw new Error(`Unknown edit type: ${unknownType}`);
                }
            }
        }
    }
    insertFrame(batch, componentId, parent, childIndex, frames, frame, frameIndex) {
        const frameReader = batch.frameReader;
        const frameType = frameReader.frameType(frame);
        switch (frameType) {
            case FrameType.element:
                this.insertElement(batch, componentId, parent, childIndex, frames, frame, frameIndex);
                return 1;
            case FrameType.text:
                this.insertText(batch, parent, childIndex, frame);
                return 1;
            case FrameType.attribute:
                throw new Error('Attribute frames should only be present as leading children of element frames.');
            case FrameType.component:
                this.insertComponent(batch, parent, childIndex, frame);
                return 1;
            case FrameType.region:
                return this.insertFrameRange(batch, componentId, parent, childIndex, frames, frameIndex + 1, frameIndex + frameReader.subtreeLength(frame));
            case FrameType.elementReferenceCapture:
                if (parent instanceof Element) {
                    applyCaptureIdToElement(parent, frameReader.elementReferenceCaptureId(frame));
                    return 0; // A "capture" is a child in the diff, but has no node in the DOM
                }
                else {
                    throw new Error('Reference capture frames can only be children of element frames.');
                }
            case FrameType.markup:
                this.insertMarkup(batch, parent, childIndex, frame);
                return 1;
            default: {
                const unknownType = frameType; // Compile-time verification that the switch was exhaustive
                throw new Error(`Unknown frame type: ${unknownType}`);
            }
        }
    }
    insertElement(batch, componentId, parent, childIndex, frames, frame, frameIndex) {
        const frameReader = batch.frameReader;
        const tagName = frameReader.elementName(frame);
        const newDomElementRaw = (tagName === 'svg' || isSvgElement(parent)) ?
            document.createElementNS('http://www.w3.org/2000/svg', tagName) :
            document.createElement(tagName);
        const newElement = toLogicalElement(newDomElementRaw);
        let inserted = false;
        // Apply attributes
        const descendantsEndIndexExcl = frameIndex + frameReader.subtreeLength(frame);
        for (let descendantIndex = frameIndex + 1; descendantIndex < descendantsEndIndexExcl; descendantIndex++) {
            const descendantFrame = batch.referenceFramesEntry(frames, descendantIndex);
            if (frameReader.frameType(descendantFrame) === FrameType.attribute) {
                this.applyAttribute(batch, componentId, newDomElementRaw, descendantFrame);
            }
            else {
                insertLogicalChild(newDomElementRaw, parent, childIndex);
                inserted = true;
                // As soon as we see a non-attribute child, all the subsequent child frames are
                // not attributes, so bail out and insert the remnants recursively
                this.insertFrameRange(batch, componentId, newElement, 0, frames, descendantIndex, descendantsEndIndexExcl);
                break;
            }
        }
        // this element did not have any children, so it's not inserted yet.
        if (!inserted) {
            insertLogicalChild(newDomElementRaw, parent, childIndex);
        }
        // We handle setting 'value' on a <select> in three different ways:
        // [1] When inserting a corresponding <option>, in case you're dynamically adding options.
        //     This is the case below.
        // [2] After we finish inserting the <select>, in case the descendant options are being
        //     added as an opaque markup block rather than individually. This is the other case below.
        // [3] In case the the value of the select and the option value is changed in the same batch.
        //     We just receive an attribute frame and have to set the select value afterwards.
        // We also defer setting the 'value' property for <input> because certain types of inputs have
        // default attribute values that may incorrectly constain the specified 'value'.
        // For example, range inputs have default 'min' and 'max' attributes that may incorrectly
        // clamp the 'value' property if it is applied before custom 'min' and 'max' attributes.
        if (newDomElementRaw instanceof HTMLOptionElement) {
            // Situation 1
            this.trySetSelectValueFromOptionElement(newDomElementRaw);
        }
        else if (deferredValuePropname in newDomElementRaw) {
            // Situation 2
            setDeferredElementValue(newDomElementRaw, newDomElementRaw[deferredValuePropname]);
        }
    }
    trySetSelectValueFromOptionElement(optionElement) {
        const selectElem = this.findClosestAncestorSelectElement(optionElement);
        if (!isBlazorSelectElement(selectElem)) {
            return false;
        }
        if (isMultipleSelectElement(selectElem)) {
            optionElement.selected = selectElem._blazorDeferredValue.indexOf(optionElement.value) !== -1;
        }
        else {
            if (selectElem._blazorDeferredValue !== optionElement.value) {
                return false;
            }
            setSingleSelectElementValue(selectElem, optionElement.value);
            delete selectElem._blazorDeferredValue;
        }
        return true;
        function isBlazorSelectElement(selectElem) {
            return !!selectElem && (deferredValuePropname in selectElem);
        }
    }
    insertComponent(batch, parent, childIndex, frame) {
        const containerElement = createAndInsertLogicalContainer(parent, childIndex);
        // All we have to do is associate the child component ID with its location. We don't actually
        // do any rendering here, because the diff for the child will appear later in the render batch.
        const childComponentId = batch.frameReader.componentId(frame);
        this.attachComponentToElement(childComponentId, containerElement);
    }
    insertText(batch, parent, childIndex, textFrame) {
        const textContent = batch.frameReader.textContent(textFrame);
        const newTextNode = document.createTextNode(textContent);
        insertLogicalChild(newTextNode, parent, childIndex);
    }
    insertMarkup(batch, parent, childIndex, markupFrame) {
        const markupContainer = createAndInsertLogicalContainer(parent, childIndex);
        const markupContent = batch.frameReader.markupContent(markupFrame);
        const parsedMarkup = parseMarkup(markupContent, isSvgElement(parent));
        let logicalSiblingIndex = 0;
        while (parsedMarkup.firstChild) {
            insertLogicalChild(parsedMarkup.firstChild, markupContainer, logicalSiblingIndex++);
        }
    }
    applyAttribute(batch, componentId, toDomElement, attributeFrame) {
        const frameReader = batch.frameReader;
        const attributeName = frameReader.attributeName(attributeFrame);
        const eventHandlerId = frameReader.attributeEventHandlerId(attributeFrame);
        if (eventHandlerId) {
            const eventName = stripOnPrefix(attributeName);
            this.eventDelegator.setListener(toDomElement, eventName, eventHandlerId, componentId);
            return;
        }
        // First see if we have special handling for this attribute
        if (!this.tryApplySpecialProperty(batch, toDomElement, attributeName, attributeFrame)) {
            // If not, treat it as a regular string-valued attribute
            toDomElement.setAttribute(attributeName, frameReader.attributeValue(attributeFrame));
        }
    }
    tryApplySpecialProperty(batch, element, attributeName, attributeFrame) {
        switch (attributeName) {
            case 'value':
                return this.tryApplyValueProperty(batch, element, attributeFrame);
            case 'checked':
                return this.tryApplyCheckedProperty(batch, element, attributeFrame);
            default: {
                if (attributeName.startsWith(internalAttributeNamePrefix)) {
                    this.applyInternalAttribute(batch, element, attributeName.substring(internalAttributeNamePrefix.length), attributeFrame);
                    return true;
                }
                return false;
            }
        }
    }
    applyInternalAttribute(batch, element, internalAttributeName, attributeFrame) {
        const attributeValue = attributeFrame ? batch.frameReader.attributeValue(attributeFrame) : null;
        if (internalAttributeName.startsWith(eventStopPropagationAttributeNamePrefix)) {
            // Stop propagation
            const eventName = stripOnPrefix(internalAttributeName.substring(eventStopPropagationAttributeNamePrefix.length));
            this.eventDelegator.setStopPropagation(element, eventName, attributeValue !== null);
        }
        else if (internalAttributeName.startsWith(eventPreventDefaultAttributeNamePrefix)) {
            // Prevent default
            const eventName = stripOnPrefix(internalAttributeName.substring(eventPreventDefaultAttributeNamePrefix.length));
            this.eventDelegator.setPreventDefault(element, eventName, attributeValue !== null);
        }
        else {
            // The prefix makes this attribute name reserved, so any other usage is disallowed
            throw new Error(`Unsupported internal attribute '${internalAttributeName}'`);
        }
    }
    tryApplyValueProperty(batch, element, attributeFrame) {
        // Certain elements have built-in behaviour for their 'value' property
        const frameReader = batch.frameReader;
        let value = attributeFrame ? frameReader.attributeValue(attributeFrame) : null;
        if (value && element.tagName === 'INPUT') {
            value = normalizeInputValue(value, element.getAttribute('type'));
        }
        switch (element.tagName) {
            case 'INPUT':
            case 'SELECT':
            case 'TEXTAREA': {
                // <select> is special, in that anything we write to .value will be lost if there
                // isn't yet a matching <option>. To maintain the expected behavior no matter the
                // element insertion/update order, preserve the desired value separately so
                // we can recover it when inserting any matching <option> or after inserting an
                // entire markup block of descendants.
                // We also defer setting the 'value' property for <input> because certain types of inputs have
                // default attribute values that may incorrectly constain the specified 'value'.
                // For example, range inputs have default 'min' and 'max' attributes that may incorrectly
                // clamp the 'value' property if it is applied before custom 'min' and 'max' attributes.
                if (value && element instanceof HTMLSelectElement && isMultipleSelectElement(element)) {
                    value = JSON.parse(value);
                }
                setDeferredElementValue(element, value);
                element[deferredValuePropname] = value;
                return true;
            }
            case 'OPTION': {
                if (value || value === '') {
                    element.setAttribute('value', value);
                }
                else {
                    element.removeAttribute('value');
                }
                // See above for why we have this special handling for <select>/<option>
                // Situation 3
                this.trySetSelectValueFromOptionElement(element);
                return true;
            }
            default:
                return false;
        }
    }
    tryApplyCheckedProperty(batch, element, attributeFrame) {
        // Certain elements have built-in behaviour for their 'checked' property
        if (element.tagName === 'INPUT') {
            const value = attributeFrame ? batch.frameReader.attributeValue(attributeFrame) : null;
            element.checked = value !== null;
            return true;
        }
        else {
            return false;
        }
    }
    findClosestAncestorSelectElement(element) {
        while (element) {
            if (element instanceof HTMLSelectElement) {
                return element;
            }
            else {
                element = element.parentElement;
            }
        }
        return null;
    }
    insertFrameRange(batch, componentId, parent, childIndex, frames, startIndex, endIndexExcl) {
        const origChildIndex = childIndex;
        for (let index = startIndex; index < endIndexExcl; index++) {
            const frame = batch.referenceFramesEntry(frames, index);
            const numChildrenInserted = this.insertFrame(batch, componentId, parent, childIndex, frames, frame, index);
            childIndex += numChildrenInserted;
            // Skip over any descendants, since they are already dealt with recursively
            index += countDescendantFrames(batch, frame);
        }
        return (childIndex - origChildIndex); // Total number of children inserted
    }
}
function parseMarkup(markup, isSvg) {
    if (isSvg) {
        sharedSvgElemForParsing.innerHTML = markup || ' ';
        return sharedSvgElemForParsing;
    }
    else {
        sharedTemplateElemForParsing.innerHTML = markup || ' ';
        return sharedTemplateElemForParsing.content;
    }
}
function normalizeInputValue(value, type) {
    // Time inputs (e.g. 'time' and 'datetime-local') misbehave on chromium-based
    // browsers when a time is set that includes a seconds value of '00', most notably
    // when entered from keyboard input. This behavior is not limited to specific
    // 'step' attribute values, so we always remove the trailing seconds value if the
    // time ends in '00'.
    switch (type) {
        case 'time':
            return value.length === 8 && value.endsWith('00')
                ? value.substring(0, 5)
                : value;
        case 'datetime-local':
            return value.length === 19 && value.endsWith('00')
                ? value.substring(0, 16)
                : value;
        default:
            return value;
    }
}
function countDescendantFrames(batch, frame) {
    const frameReader = batch.frameReader;
    switch (frameReader.frameType(frame)) {
        // The following frame types have a subtree length. Other frames may use that memory slot
        // to mean something else, so we must not read it. We should consider having nominal subtypes
        // of RenderTreeFramePointer that prevent access to non-applicable fields.
        case FrameType.component:
        case FrameType.element:
        case FrameType.region:
            return frameReader.subtreeLength(frame) - 1;
        default:
            return 0;
    }
}
function clearElement(element) {
    let childNode;
    while ((childNode = element.firstChild)) {
        element.removeChild(childNode);
    }
}
function clearBetween(start, end) {
    const logicalParent = getLogicalParent(start);
    if (!logicalParent) {
        throw new Error("Can't clear between nodes. The start node does not have a logical parent.");
    }
    const children = getLogicalChildrenArray(logicalParent);
    const removeStart = children.indexOf(start) + 1;
    const endIndex = children.indexOf(end);
    // We remove the end component comment from the DOM as we don't need it after this point.
    for (let i = removeStart; i <= endIndex; i++) {
        removeLogicalChild(logicalParent, removeStart);
    }
    // We sanitize the start comment by removing all the information from it now that we don't need it anymore
    // as it adds noise to the DOM.
    start.textContent = '!';
}
function stripOnPrefix(attributeName) {
    if (attributeName.startsWith('on')) {
        return attributeName.substring(2);
    }
    throw new Error(`Attribute should be an event name, but doesn't start with 'on'. Value: '${attributeName}'`);
}
function isMultipleSelectElement(element) {
    return element.type === 'select-multiple';
}
function setSingleSelectElementValue(element, value) {
    // There's no sensible way to represent a select option with value 'null', because
    // (1) HTML attributes can't have null values - the closest equivalent is absence of the attribute
    // (2) When picking an <option> with no 'value' attribute, the browser treats the value as being the
    //     *text content* on that <option> element. Trying to suppress that default behavior would involve
    //     a long chain of special-case hacks, as well as being breaking vs 3.x.
    // So, the most plausible 'null' equivalent is an empty string. It's unfortunate that people can't
    // write <option value=@someNullVariable>, and that we can never distinguish between null and empty
    // string in a bound <select>, but that's a limit in the representational power of HTML.
    element.value = value || '';
}
function setMultipleSelectElementValue(element, value) {
    value || (value = []);
    for (let i = 0; i < element.options.length; i++) {
        element.options[i].selected = value.indexOf(element.options[i].value) !== -1;
    }
}
function setDeferredElementValue(element, value) {
    if (element instanceof HTMLSelectElement) {
        if (isMultipleSelectElement(element)) {
            setMultipleSelectElementValue(element, value);
        }
        else {
            setSingleSelectElementValue(element, value);
        }
    }
    else {
        element.value = value;
    }
}

;// CONCATENATED MODULE: ./Rendering/Renderer.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.





const browserRenderers = {};
let shouldResetScrollAfterNextBatch = false;
function attachRootComponentToLogicalElement(browserRendererId, logicalElement, componentId, appendContent) {
    let browserRenderer = browserRenderers[browserRendererId];
    if (!browserRenderer) {
        browserRenderer = new BrowserRenderer(browserRendererId);
        browserRenderers[browserRendererId] = browserRenderer;
    }
    browserRenderer.attachRootComponentToLogicalElement(componentId, logicalElement, appendContent);
}
function attachRootComponentToElement(elementSelector, componentId, browserRendererId) {
    const afterElementSelector = '::after';
    const beforeElementSelector = '::before';
    let appendContent = false;
    if (elementSelector.endsWith(afterElementSelector)) {
        elementSelector = elementSelector.slice(0, -afterElementSelector.length);
        appendContent = true;
    }
    else if (elementSelector.endsWith(beforeElementSelector)) {
        throw new Error(`The '${beforeElementSelector}' selector is not supported.`);
    }
    const element = getAndRemovePendingRootComponentContainer(elementSelector)
        || document.querySelector(elementSelector);
    if (!element) {
        throw new Error(`Could not find any element matching selector '${elementSelector}'.`);
    }
    // 'allowExistingContents' to keep any prerendered content until we do the first client-side render
    // Only client-side Blazor supplies a browser renderer ID
    attachRootComponentToLogicalElement(browserRendererId || 0, toLogicalElement(element, /* allow existing contents */ true), componentId, appendContent);
}
function getRendererer(browserRendererId) {
    return browserRenderers[browserRendererId];
}
function renderBatch(browserRendererId, batch) {
    const browserRenderer = browserRenderers[browserRendererId];
    if (!browserRenderer) {
        throw new Error(`There is no browser renderer with ID ${browserRendererId}.`);
    }
    const arrayRangeReader = batch.arrayRangeReader;
    const updatedComponentsRange = batch.updatedComponents();
    const updatedComponentsValues = arrayRangeReader.values(updatedComponentsRange);
    const updatedComponentsLength = arrayRangeReader.count(updatedComponentsRange);
    const referenceFrames = batch.referenceFrames();
    const referenceFramesValues = arrayRangeReader.values(referenceFrames);
    const diffReader = batch.diffReader;
    for (let i = 0; i < updatedComponentsLength; i++) {
        const diff = batch.updatedComponentsEntry(updatedComponentsValues, i);
        const componentId = diffReader.componentId(diff);
        const edits = diffReader.edits(diff);
        browserRenderer.updateComponent(batch, componentId, edits, referenceFramesValues);
    }
    const disposedComponentIdsRange = batch.disposedComponentIds();
    const disposedComponentIdsValues = arrayRangeReader.values(disposedComponentIdsRange);
    const disposedComponentIdsLength = arrayRangeReader.count(disposedComponentIdsRange);
    for (let i = 0; i < disposedComponentIdsLength; i++) {
        const componentId = batch.disposedComponentIdsEntry(disposedComponentIdsValues, i);
        browserRenderer.disposeComponent(componentId);
    }
    const disposedEventHandlerIdsRange = batch.disposedEventHandlerIds();
    const disposedEventHandlerIdsValues = arrayRangeReader.values(disposedEventHandlerIdsRange);
    const disposedEventHandlerIdsLength = arrayRangeReader.count(disposedEventHandlerIdsRange);
    for (let i = 0; i < disposedEventHandlerIdsLength; i++) {
        const eventHandlerId = batch.disposedEventHandlerIdsEntry(disposedEventHandlerIdsValues, i);
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
}

;// CONCATENATED MODULE: ./Services/NavigationManager.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.


let hasEnabledNavigationInterception = false;
let hasRegisteredNavigationEventListeners = false;
// Will be initialized once someone registers
let notifyLocationChangedCallback = null;
// These are the functions we're making available for invocation from .NET
const internalFunctions = {
    listenForNavigationEvents,
    enableNavigationInterception,
    navigateTo,
    getBaseURI: () => document.baseURI,
    getLocationHref: () => location.href,
};
function listenForNavigationEvents(callback) {
    notifyLocationChangedCallback = callback;
    if (hasRegisteredNavigationEventListeners) {
        return;
    }
    hasRegisteredNavigationEventListeners = true;
    window.addEventListener('popstate', () => notifyLocationChanged(false));
}
function enableNavigationInterception() {
    hasEnabledNavigationInterception = true;
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
            const href = anchorTarget.getAttribute('href');
            const absoluteHref = toAbsoluteUri(href);
            if (isWithinBaseUriSpace(absoluteHref)) {
                event.preventDefault();
                performInternalNavigation(absoluteHref, /* interceptedLink */ true, /* replace */ false);
            }
        }
    });
}
function navigateTo(uri, forceLoadOrOptions, replaceIfUsingOldOverload = false) {
    const absoluteUri = toAbsoluteUri(uri);
    // Normalize the parameters to the newer overload (i.e., using NavigationOptions)
    const options = forceLoadOrOptions instanceof Object
        ? forceLoadOrOptions
        : { forceLoad: forceLoadOrOptions, replaceHistoryEntry: replaceIfUsingOldOverload };
    if (!options.forceLoad && isWithinBaseUriSpace(absoluteUri)) {
        performInternalNavigation(absoluteUri, false, options.replaceHistoryEntry);
    }
    else {
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
        const temporaryUri = uri + '?';
        history.replaceState(null, '', temporaryUri);
        location.replace(uri);
    }
    else if (replace) {
        location.replace(uri);
    }
    else {
        location.href = uri;
    }
}
function performInternalNavigation(absoluteInternalHref, interceptedLink, replace) {
    // Since this was *not* triggered by a back/forward gesture (that goes through a different
    // code path starting with a popstate event), we don't want to preserve the current scroll
    // position, so reset it.
    // To avoid ugly flickering effects, we don't want to change the scroll position until
    // we render the new page. As a best approximation, wait until the next batch.
    resetScrollAfterNextBatch();
    if (!replace) {
        history.pushState(null, /* ignored title */ '', absoluteInternalHref);
    }
    else {
        history.replaceState(null, /* ignored title */ '', absoluteInternalHref);
    }
    notifyLocationChanged(interceptedLink);
}
async function notifyLocationChanged(interceptedLink) {
    if (notifyLocationChangedCallback) {
        await notifyLocationChangedCallback(location.href, interceptedLink);
    }
}
let testAnchor;
function toAbsoluteUri(relativeUri) {
    testAnchor = testAnchor || document.createElement('a');
    testAnchor.href = relativeUri;
    return testAnchor.href;
}
function findAnchorTarget(event) {
    // _blazorDisableComposedPath is a temporary escape hatch in case any problems are discovered
    // in this logic. It can be removed in a later release, and should not be considered supported API.
    const path = !window['_blazorDisableComposedPath'] && event.composedPath && event.composedPath();
    if (path) {
        // This logic works with events that target elements within a shadow root,
        // as long as the shadow mode is 'open'. For closed shadows, we can't possibly
        // know what internal element was clicked.
        for (let i = 0; i < path.length; i++) {
            const candidate = path[i];
            if (candidate instanceof Element && candidate.tagName === 'A') {
                return candidate;
            }
        }
        return null;
    }
    else {
        // Since we're adding use of composedPath in a patch, retain compatibility with any
        // legacy browsers that don't support it by falling back on the older logic, even
        // though it won't work properly with ShadowDOM. This can be removed in the next
        // major release.
        return findClosestAnchorAncestorLegacy(event.target, 'A');
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
    const baseUriWithTrailingSlash = toBaseUriWithTrailingSlash(document.baseURI); // TODO: Might baseURI really be null?
    return href.startsWith(baseUriWithTrailingSlash);
}
function toBaseUriWithTrailingSlash(baseUri) {
    return baseUri.substr(0, baseUri.lastIndexOf('/') + 1);
}
function eventHasSpecialKey(event) {
    return event.ctrlKey || event.shiftKey || event.altKey || event.metaKey;
}
function canProcessAnchor(anchorTarget) {
    const targetAttributeValue = anchorTarget.getAttribute('target');
    const opensInSameFrame = !targetAttributeValue || targetAttributeValue === '_self';
    return opensInSameFrame && anchorTarget.hasAttribute('href') && !anchorTarget.hasAttribute('download');
}

;// CONCATENATED MODULE: ./DomWrapper.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

const domFunctions = {
    focus: DomWrapper_focus,
    focusBySelector,
};
function DomWrapper_focus(element, preventScroll) {
    if (element instanceof HTMLElement) {
        element.focus({ preventScroll });
    }
    else if (element instanceof SVGElement) {
        if (element.hasAttribute('tabindex')) {
            element.focus({ preventScroll });
        }
        else {
            throw new Error('Unable to focus an SVG element that does not have a tabindex.');
        }
    }
    else {
        throw new Error('Unable to focus an invalid element.');
    }
}
function focusBySelector(selector) {
    const element = document.querySelector(selector);
    if (element) {
        // If no explicit tabindex is defined, mark it as programmatically-focusable.
        // This does actually add a new HTML attribute, but it shouldn't interfere with
        // diffing because diffing only deals with the attributes you have in your code.
        if (!element.hasAttribute('tabindex')) {
            element.tabIndex = -1;
        }
        element.focus();
    }
}

;// CONCATENATED MODULE: ./Virtualize.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
const Virtualize = {
    init,
    dispose,
};
const observersByDotNetId = {};
function findClosestScrollContainer(element) {
    if (!element) {
        return null;
    }
    const style = getComputedStyle(element);
    if (style.overflowY !== 'visible') {
        return element;
    }
    return findClosestScrollContainer(element.parentElement);
}
function init(dotNetHelper, spacerBefore, spacerAfter, rootMargin = 50) {
    // Overflow anchoring can cause an ongoing scroll loop, because when we resize the spacers, the browser
    // would update the scroll position to compensate. Then the spacer would remain visible and we'd keep on
    // trying to resize it.
    const scrollContainer = findClosestScrollContainer(spacerBefore);
    (scrollContainer || document.documentElement).style.overflowAnchor = 'none';
    const rangeBetweenSpacers = document.createRange();
    if (isValidTableElement(spacerAfter.parentElement)) {
        spacerBefore.style.display = 'table-row';
        spacerAfter.style.display = 'table-row';
    }
    const intersectionObserver = new IntersectionObserver(intersectionCallback, {
        root: scrollContainer,
        rootMargin: `${rootMargin}px`,
    });
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
                spacer.style.display = 'table-row';
                observer.observe(spacer, observerOptions);
            }
            intersectionObserver.unobserve(spacer);
            intersectionObserver.observe(spacer);
        });
        mutationObserver.observe(spacer, observerOptions);
        return mutationObserver;
    }
    function intersectionCallback(entries) {
        entries.forEach((entry) => {
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
            const spacerSeparation = rangeBetweenSpacers.getBoundingClientRect().height;
            const containerSize = (_a = entry.rootBounds) === null || _a === void 0 ? void 0 : _a.height;
            if (entry.target === spacerBefore) {
                dotNetHelper.invokeMethodAsync('OnSpacerBeforeVisible', entry.intersectionRect.top - entry.boundingClientRect.top, spacerSeparation, containerSize);
            }
            else if (entry.target === spacerAfter && spacerAfter.offsetHeight > 0) {
                // When we first start up, both the "before" and "after" spacers will be visible, but it's only relevant to raise a
                // single event to load the initial data. To avoid raising two events, skip the one for the "after" spacer if we know
                // it's meaningless to talk about any overlap into it.
                dotNetHelper.invokeMethodAsync('OnSpacerAfterVisible', entry.boundingClientRect.bottom - entry.intersectionRect.bottom, spacerSeparation, containerSize);
            }
        });
    }
    function isValidTableElement(element) {
        if (element === null) {
            return false;
        }
        return ((element instanceof HTMLTableElement && element.style.display === '') || element.style.display === 'table')
            || ((element instanceof HTMLTableSectionElement && element.style.display === '') || element.style.display === 'table-row-group');
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
}

;// CONCATENATED MODULE: ./PageTitle.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

const PageTitle = {
    getAndRemoveExistingTitle,
};
function getAndRemoveExistingTitle() {
    var _a;
    // Other <title> elements may exist outside <head> (e.g., inside <svg> elements) but they aren't page titles
    const titleElements = document.head ? document.head.getElementsByTagName('title') : [];
    if (titleElements.length === 0) {
        return null;
    }
    let existingTitle = null;
    for (let index = titleElements.length - 1; index >= 0; index--) {
        const currentTitleElement = titleElements[index];
        const previousSibling = currentTitleElement.previousSibling;
        const isBlazorTitle = previousSibling instanceof Comment && getLogicalParent(previousSibling) !== null;
        if (isBlazorTitle) {
            continue;
        }
        if (existingTitle === null) {
            existingTitle = currentTitleElement.textContent;
        }
        (_a = currentTitleElement.parentNode) === null || _a === void 0 ? void 0 : _a.removeChild(currentTitleElement);
    }
    return existingTitle;
}

;// CONCATENATED MODULE: ./InputFile.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
const InputFile = {
    init: InputFile_init,
    toImageFile,
    readFileData,
};
function InputFile_init(callbackWrapper, elem) {
    elem._blazorInputFileNextFileId = 0;
    elem.addEventListener('click', function () {
        // Permits replacing an existing file with a new one of the same file name.
        elem.value = '';
    });
    elem.addEventListener('change', function () {
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
        callbackWrapper.invokeMethodAsync('NotifyChange', fileList);
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
        originalFileImage.src = URL.createObjectURL(originalFile['blob']);
    });
    const resizedImageBlob = await new Promise(function (resolve) {
        var _a;
        const desiredWidthRatio = Math.min(1, maxWidth / loadedImage.width);
        const desiredHeightRatio = Math.min(1, maxHeight / loadedImage.height);
        const chosenSizeRatio = Math.min(desiredWidthRatio, desiredHeightRatio);
        const canvas = document.createElement('canvas');
        canvas.width = Math.round(loadedImage.width * chosenSizeRatio);
        canvas.height = Math.round(loadedImage.height * chosenSizeRatio);
        (_a = canvas.getContext('2d')) === null || _a === void 0 ? void 0 : _a.drawImage(loadedImage, 0, 0, canvas.width, canvas.height);
        canvas.toBlob(resolve, format);
    });
    const result = {
        id: ++elem._blazorInputFileNextFileId,
        lastModified: originalFile.lastModified,
        name: originalFile.name,
        size: (resizedImageBlob === null || resizedImageBlob === void 0 ? void 0 : resizedImageBlob.size) || 0,
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
        throw new Error(`There is no file with ID ${fileId}. The file list may have changed. See https://aka.ms/aspnet/blazor-input-file-multiple-selections.`);
    }
    return file;
}

;// CONCATENATED MODULE: ./StreamingInterop.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

async function getNextChunk(data, position, nextChunkSize) {
    if (data instanceof Blob) {
        return await getChunkFromBlob(data, position, nextChunkSize);
    }
    else {
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
    const nextChunkData = new Uint8Array(data.buffer, data.byteOffset + position, nextChunkSize);
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
    }
    else if (bytesRead === 0) {
        streamController.close();
        transmittingDotNetToJSStreams.delete(streamId);
    }
    else {
        streamController.enqueue(data.length === bytesRead ? data : data.subarray(0, bytesRead));
    }
}

;// CONCATENATED MODULE: ./GlobalExports.ts
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
        getJSDataStreamChunk: getNextChunk,
        receiveDotNetDataStream: receiveDotNetDataStream,
        attachWebRendererInterop: attachWebRendererInterop,
    },
};
// Make the following APIs available in global scope for invocation from JS
window['Blazor'] = Blazor;

;// CONCATENATED MODULE: ./Environment.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
let platform;
function setPlatform(platformInstance) {
    platform = platformInstance;
    return platform;
}

;// CONCATENATED MODULE: ./Platform/Mono/MonoDebugger.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
const navigatorUA = navigator;
const brands = navigatorUA.userAgentData && navigatorUA.userAgentData.brands;
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const currentBrowserIsChromeOrEdge = brands
    ? brands.some(b => b.brand === 'Google Chrome' || b.brand === 'Microsoft Edge')
    : window.chrome;
const MonoDebugger_platform = navigatorUA.userAgentData ? navigatorUA.userAgentData.platform : navigator.platform;
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
    const altKeyName = MonoDebugger_platform.match(/^Mac/i) ? 'Cmd' : 'Alt';
    if (hasDebuggingEnabled()) {
        console.info(`Debugging hotkey: Shift+${altKeyName}+D (when application has focus)`);
    }
    // Even if debugging isn't enabled, we register the hotkey so we can report why it's not enabled
    document.addEventListener('keydown', evt => {
        if (evt.shiftKey && (evt.metaKey || evt.altKey) && evt.code === 'KeyD') {
            if (!debugBuild && !hasReferencedPdbs) {
                console.error('Cannot start debugging, because the application was not compiled with debugging enabled.');
            }
            else if (!currentBrowserIsChromeOrEdge) {
                console.error('Currently, only Microsoft Edge (80+), or Google Chrome, are supported for debugging.');
            }
            else {
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
    const link = document.createElement('a');
    link.href = `_framework/debug?url=${encodeURIComponent(location.href)}`;
    link.target = '_blank';
    link.rel = 'noopener noreferrer';
    link.click();
}

;// CONCATENATED MODULE: ./BootErrors.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
let hasFailed = false;
function showErrorNotification() {
    const errorUi = document.querySelector('#blazor-error-ui');
    if (errorUi) {
        errorUi.style.display = 'block';
    }
    if (!hasFailed) {
        hasFailed = true;
        const errorUiReloads = document.querySelectorAll('#blazor-error-ui .reload');
        errorUiReloads.forEach(reload => {
            reload.onclick = function (e) {
                location.reload();
                e.preventDefault();
            };
        });
        const errorUiDismiss = document.querySelectorAll('#blazor-error-ui .dismiss');
        errorUiDismiss.forEach(dismiss => {
            dismiss.onclick = function (e) {
                const errorUi = document.querySelector('#blazor-error-ui');
                if (errorUi) {
                    errorUi.style.display = 'none';
                }
                e.preventDefault();
            };
        });
    }
}

;// CONCATENATED MODULE: ./Platform/BootConfig.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
class BootConfigResult {
    constructor(bootConfig, applicationEnvironment) {
        this.bootConfig = bootConfig;
        this.applicationEnvironment = applicationEnvironment;
    }
    static async initAsync(loadBootResource, environment) {
        const loaderResponse = loadBootResource !== undefined ?
            loadBootResource('manifest', 'blazor.boot.json', '_framework/blazor.boot.json', '') :
            defaultLoadBlazorBootJson('_framework/blazor.boot.json');
        const bootConfigResponse = loaderResponse instanceof Promise ?
            await loaderResponse :
            await defaultLoadBlazorBootJson(loaderResponse !== null && loaderResponse !== void 0 ? loaderResponse : '_framework/blazor.boot.json');
        // While we can expect an ASP.NET Core hosted application to include the environment, other
        // hosts may not. Assume 'Production' in the absence of any specified value.
        const applicationEnvironment = environment || bootConfigResponse.headers.get('Blazor-Environment') || 'Production';
        const bootConfig = await bootConfigResponse.json();
        bootConfig.modifiableAssemblies = bootConfigResponse.headers.get('DOTNET-MODIFIABLE-ASSEMBLIES');
        bootConfig.aspnetCoreBrowserTools = bootConfigResponse.headers.get('ASPNETCORE-BROWSER-TOOLS');
        return new BootConfigResult(bootConfig, applicationEnvironment);
        function defaultLoadBlazorBootJson(url) {
            return fetch(url, {
                method: 'GET',
                credentials: 'include',
                cache: 'no-cache',
            });
        }
    }
}
var ICUDataMode;
(function (ICUDataMode) {
    ICUDataMode[ICUDataMode["Sharded"] = 0] = "Sharded";
    ICUDataMode[ICUDataMode["All"] = 1] = "All";
    ICUDataMode[ICUDataMode["Invariant"] = 2] = "Invariant";
})(ICUDataMode || (ICUDataMode = {}));

;// CONCATENATED MODULE: ./Platform/Mono/MonoPlatform.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
/* eslint-disable @typescript-eslint/no-explicit-any */
/* eslint-disable @typescript-eslint/no-non-null-assertion */
/* eslint-disable no-prototype-builtins */





// initially undefined and only fully initialized after createEmscriptenModuleInstance()
let BINDING = undefined;
let MONO = undefined;
let Module = undefined;
const appBinDirName = 'appBinDir';
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
        throw new Error(`Cannot read uint64 with high order part ${highPart}, because the result would exceed Number.MAX_SAFE_INTEGER.`);
    }
    return (highPart * uint64HighOrderShift) + Module.HEAPU32[heapU32Index];
}
const monoPlatform = {
    start: async function start(resourceLoader) {
        attachDebuggerHotkey(resourceLoader);
        await createEmscriptenModuleInstance(resourceLoader);
    },
    callEntryPoint: async function callEntryPoint(assemblyName) {
        const emptyArray = [[]];
        try {
            await BINDING.call_assembly_entry_point(assemblyName, emptyArray, 'm');
        }
        catch (error) {
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
    getObjectFieldsBaseAddress: function getObjectFieldsBaseAddress(referenceTypedObject) {
        // The first two int32 values are internal Mono data
        return (referenceTypedObject + 8);
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
    readStringField: function readHeapObject(baseAddress, fieldOffset, readBoolValueAsString) {
        const fieldValue = getValueI32(baseAddress + (fieldOffset || 0));
        if (fieldValue === 0) {
            return null;
        }
        if (readBoolValueAsString) {
            // Some fields are stored as a union of bool | string | null values, but need to read as a string.
            // If the stored value is a bool, the behavior we want is empty string ('') for true, or null for false.
            const unboxedValue = BINDING.unbox_mono_obj(fieldValue);
            if (typeof (unboxedValue) === 'boolean') {
                return unboxedValue ? '' : null;
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
        }
        else {
            decodedString = BINDING.conv_string(fieldValue);
        }
        return decodedString;
    },
    readStructField: function readStructField(baseAddress, fieldOffset) {
        return (baseAddress + (fieldOffset || 0));
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
        }
        else {
            currentHeapLock.enqueuePostReleaseAction(callback);
        }
    },
};
async function importDotnetJs(resourceLoader) {
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
    let src = `_framework/${dotnetJsResourceName}`;
    // Allow overriding the URI from which the dotnet.*.js file is loaded
    if (resourceLoader.startOptions.loadBootResource) {
        const resourceType = 'dotnetjs';
        const customSrc = resourceLoader.startOptions.loadBootResource(resourceType, dotnetJsResourceName, src, dotnetJsContentHash);
        if (typeof (customSrc) === 'string') {
            src = customSrc;
        }
        else if (customSrc) {
            // Since we must load this via a import, it's only valid to supply a URI (and not a Request, say)
            throw new Error(`For a ${resourceType} resource, custom loaders must supply a URI string.`);
        }
    }
    // For consistency with WebAssemblyResourceLoader, we only enforce SRI if caching is allowed
    if (resourceLoader.bootConfig.cacheBootResources) {
        const scriptElem = document.createElement('link');
        scriptElem.rel = 'modulepreload';
        scriptElem.href = src;
        scriptElem.crossOrigin = 'anonymous';
        // it will make dynamic import fail if the hash doesn't match
        // It's currently only validated by chromium browsers
        // Firefox doesn't break on it, but doesn't validate it either
        scriptElem.integrity = dotnetJsContentHash;
        document.head.appendChild(scriptElem);
    }
    // GOTCHA: remove this once runtime switched to ES6
    // this is capturing the export via callback we have in CJS version of the runtime
    let cjsExportResolve = undefined;
    const cjsExport = new Promise((resolve) => {
        cjsExportResolve = resolve;
    });
    globalThis.__onDotnetRuntimeLoaded = (createDotnetRuntime) => {
        delete globalThis.__onDotnetRuntimeLoaded;
        cjsExportResolve(createDotnetRuntime);
    };
    const absoluteSrc = (new URL(src, document.baseURI)).toString();
    const { default: createDotnetRuntime } = await import(/* webpackIgnore: true */ absoluteSrc);
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
    const moduleConfig = (window['Module'] || {});
    const suppressMessages = ['DEBUGGING ENABLED'];
    const print = line => (suppressMessages.indexOf(line) < 0 && console.log(line));
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
    // Begin loading the .dll/.pdb/.wasm files, but don't block here. Let other loading processes run in parallel.
    const dotnetWasmResourceName = 'dotnet.wasm';
    const assembliesBeingLoaded = resourceLoader.loadResources(resources.assembly, filename => `_framework/${filename}`, 'assembly');
    const pdbsBeingLoaded = resourceLoader.loadResources(resources.pdb || {}, filename => `_framework/${filename}`, 'pdb');
    const wasmBeingLoaded = resourceLoader.loadResource(
    /* name */ dotnetWasmResourceName,
    /* url */ `_framework/${dotnetWasmResourceName}`,
    /* hash */ resourceLoader.bootConfig.resources.runtime[dotnetWasmResourceName],
    /* type */ 'dotnetwasm');
    const dotnetTimeZoneResourceName = 'dotnet.timezones.blat';
    let timeZoneResource;
    if (resourceLoader.bootConfig.resources.runtime.hasOwnProperty(dotnetTimeZoneResourceName)) {
        timeZoneResource = resourceLoader.loadResource(dotnetTimeZoneResourceName, `_framework/${dotnetTimeZoneResourceName}`, resourceLoader.bootConfig.resources.runtime[dotnetTimeZoneResourceName], 'globalization');
    }
    let icuDataResource;
    if (resourceLoader.bootConfig.icuDataMode !== ICUDataMode.Invariant) {
        const applicationCulture = resourceLoader.startOptions.applicationCulture || (navigator.languages && navigator.languages[0]);
        const icuDataResourceName = getICUResourceName(resourceLoader.bootConfig, applicationCulture);
        icuDataResource = resourceLoader.loadResource(icuDataResourceName, `_framework/${icuDataResourceName}`, resourceLoader.bootConfig.resources.runtime[icuDataResourceName], 'globalization');
    }
    const createDotnetRuntime = await dotnetJsBeingLoaded;
    await createDotnetRuntime((api) => {
        const { MONO: mono, BINDING: binding, Module: module } = api;
        Module = module;
        BINDING = binding;
        MONO = mono;
        // Override the mechanism for fetching the main wasm file so we can connect it to our cache
        const instantiateWasm = (imports, successCallback) => {
            (async () => {
                let compiledInstance;
                try {
                    const dotnetWasmResource = await wasmBeingLoaded;
                    compiledInstance = await compileWasmModule(dotnetWasmResource, imports);
                }
                catch (ex) {
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
                MONO.mono_wasm_setenv('DOTNET_SYSTEM_GLOBALIZATION_INVARIANT', '1');
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
            assembliesBeingLoaded.forEach(r => addResourceAsAssembly(r, changeExtension(r.name, '.dll')));
            pdbsBeingLoaded.forEach(r => addResourceAsAssembly(r, r.name));
            Blazor._internal.dotNetCriticalError = (message) => {
                printErr(BINDING.conv_string(message) || '(null)');
            };
            // Wire-up callbacks for satellite assemblies. Blazor will call these as part of the application
            // startup sequence to load satellite assemblies for the application's culture.
            Blazor._internal.getSatelliteAssemblies = (culturesToLoadDotNetArray) => {
                const culturesToLoad = BINDING.mono_array_to_js_array(culturesToLoadDotNetArray);
                const satelliteResources = resourceLoader.bootConfig.resources.satelliteResources;
                if (satelliteResources) {
                    const resourcePromises = Promise.all(culturesToLoad
                        .filter(culture => satelliteResources.hasOwnProperty(culture))
                        .map(culture => resourceLoader.loadResources(satelliteResources[culture], fileName => `_framework/${fileName}`, 'assembly'))
                        .reduce((previous, next) => previous.concat(next), new Array())
                        .map(async (resource) => (await resource.response).arrayBuffer()));
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
                            .map(async (resource) => resource ? (await resource.response).arrayBuffer() : null));
                    }
                }
                const resourcePromises = Promise.all(assembliesMarkedAsLazy
                    .map(assembly => resourceLoader.loadResource(assembly, `_framework/${assembly}`, lazyAssemblies[assembly], 'assembly'))
                    .map(async (resource) => (await resource.response).arrayBuffer()));
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
        };
        const postRun = () => {
            if (resourceLoader.bootConfig.debugBuild && resourceLoader.bootConfig.cacheBootResources) {
                resourceLoader.logToConsole();
            }
            resourceLoader.purgeUnusedCacheEntriesAsync(); // Don't await - it's fine to run in background
            if (resourceLoader.bootConfig.icuDataMode === ICUDataMode.Sharded) {
                MONO.mono_wasm_setenv('__BLAZOR_SHARDED_ICU', '1');
                if (resourceLoader.startOptions.applicationCulture) {
                    // If a culture is specified via start options use that to initialize the Emscripten \  .NET culture.
                    MONO.mono_wasm_setenv('LANG', `${resourceLoader.startOptions.applicationCulture}.UTF-8`);
                }
            }
            let timeZone = 'UTC';
            try {
                timeZone = Intl.DateTimeFormat().resolvedOptions().timeZone;
                // eslint-disable-next-line no-empty
            }
            catch { }
            MONO.mono_wasm_setenv('TZ', timeZone || 'UTC');
            if (resourceLoader.bootConfig.modifiableAssemblies) {
                // Configure the app to enable hot reload in Development.
                MONO.mono_wasm_setenv('DOTNET_MODIFIABLE_ASSEMBLIES', resourceLoader.bootConfig.modifiableAssemblies);
            }
            if (resourceLoader.bootConfig.aspnetCoreBrowserTools) {
                // See https://github.com/dotnet/aspnetcore/issues/37357#issuecomment-941237000
                MONO.mono_wasm_setenv('__ASPNETCORE_BROWSER_TOOLS', resourceLoader.bootConfig.aspnetCoreBrowserTools);
            }
            // -1 enables debugging with logging disabled. 0 disables debugging entirely.
            MONO.mono_wasm_load_runtime(appBinDirName, hasDebuggingEnabled() ? -1 : 0);
            MONO.mono_wasm_runtime_ready();
            attachInteropInvoker();
            runtimeReadyResolve(api);
        };
        async function addResourceAsAssembly(dependency, loadAsName) {
            const runDependencyId = `blazor:${dependency.name}`;
            Module.addRunDependency(runDependencyId);
            try {
                // Wait for the data to be loaded and verified
                const dataBuffer = await dependency.response.then(r => r.arrayBuffer());
                // Load it into the Mono runtime
                const data = new Uint8Array(dataBuffer);
                const heapAddress = Module._malloc(data.length);
                const heapMemory = new Uint8Array(Module.HEAPU8.buffer, heapAddress, data.length);
                heapMemory.set(data);
                MONO.mono_wasm_add_assembly(loadAsName, heapAddress, data.length);
                MONO.loaded_files.push(toAbsoluteUrl(dependency.url));
            }
            catch (errorInfo) {
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
const anchorTagForAbsoluteUrlConversions = document.createElement('a');
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
    const dotNetDispatcherInvokeMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'InvokeDotNet');
    const dotNetDispatcherBeginInvokeMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'BeginInvokeDotNet');
    const dotNetDispatcherEndInvokeJSMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'EndInvokeJS');
    const dotNetDispatcherNotifyByteArrayAvailableMethodHandle = bindStaticMethod('Microsoft.AspNetCore.Components.WebAssembly', 'Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime', 'NotifyByteArrayAvailable');
    DotNet.attachDispatcher({
        beginInvokeDotNetFromJS: (callId, assemblyName, methodIdentifier, dotNetObjectId, argsJson) => {
            assertHeapIsNotLocked();
            if (!dotNetObjectId && !assemblyName) {
                throw new Error('Either assemblyName or dotNetObjectId must have a non null value.');
            }
            // As a current limitation, we can only pass 4 args. Fortunately we only need one of
            // 'assemblyName' or 'dotNetObjectId', so overload them in a single slot
            const assemblyNameOrDotNetObjectId = dotNetObjectId
                ? dotNetObjectId.toString()
                : assemblyName;
            dotNetDispatcherBeginInvokeMethodHandle(callId ? callId.toString() : null, assemblyNameOrDotNetObjectId, methodIdentifier, argsJson);
        },
        endInvokeJSFromDotNet: (asyncHandle, succeeded, serializedArgs) => {
            dotNetDispatcherEndInvokeJSMethodHandle(serializedArgs);
        },
        sendByteArray: (id, data) => {
            byteArrayBeingTransferred = data;
            dotNetDispatcherNotifyByteArrayAvailableMethodHandle(id);
        },
        invokeDotNetFromJS: (assemblyName, methodIdentifier, dotNetObjectId, argsJson) => {
            assertHeapIsNotLocked();
            return dotNetDispatcherInvokeMethodHandle(assemblyName ? assemblyName : null, methodIdentifier, dotNetObjectId ? dotNetObjectId.toString() : null, argsJson);
        },
    });
}
async function loadTimezone(timeZoneResource) {
    const runDependencyId = 'blazor:timezonedata';
    Module.addRunDependency(runDependencyId);
    const request = await timeZoneResource.response;
    const arrayBuffer = await request.arrayBuffer();
    Module['FS_createPath']('/', 'usr', true, true);
    Module['FS_createPath']('/usr/', 'share', true, true);
    Module['FS_createPath']('/usr/share/', 'zoneinfo', true, true);
    MONO.mono_wasm_load_data_archive(new Uint8Array(arrayBuffer), '/usr/share/zoneinfo/');
    Module.removeRunDependency(runDependencyId);
}
function getICUResourceName(bootConfig, culture) {
    const combinedICUResourceName = 'icudt.dat';
    if (!culture || bootConfig.icuDataMode === ICUDataMode.All) {
        return combinedICUResourceName;
    }
    const prefix = culture.split('-')[0];
    if ([
        'en',
        'fr',
        'it',
        'de',
        'es',
    ].includes(prefix)) {
        return 'icudt_EFIGS.dat';
    }
    else if ([
        'zh',
        'ko',
        'ja',
    ].includes(prefix)) {
        return 'icudt_CJK.dat';
    }
    else {
        return 'icudt_no_CJK.dat';
    }
}
async function loadICUData(icuDataResource) {
    const runDependencyId = 'blazor:icudata';
    Module.addRunDependency(runDependencyId);
    const request = await icuDataResource.response;
    const array = new Uint8Array(await request.arrayBuffer());
    const offset = MONO.mono_wasm_load_bytes_into_heap(array);
    if (!MONO.mono_wasm_load_icu_data(offset)) {
        throw new Error('Error loading ICU asset.');
    }
    Module.removeRunDependency(runDependencyId);
}
async function compileWasmModule(wasmResource, imports) {
    // This is the same logic as used in emscripten's generated js. We can't use emscripten's js because
    // it doesn't provide any method for supplying a custom response provider, and we want to integrate
    // with our resource loader cache.
    if (typeof WebAssembly['instantiateStreaming'] === 'function') {
        try {
            const streamingResult = await WebAssembly['instantiateStreaming'](wasmResource.response, imports);
            return streamingResult.instance;
        }
        catch (ex) {
            console.info('Streaming compilation failed. Falling back to ArrayBuffer instantiation. ', ex);
        }
    }
    // If that's not available or fails (e.g., due to incorrect content-type header),
    // fall back to ArrayBuffer instantiation
    const arrayBuffer = await wasmResource.response.then(r => r.arrayBuffer());
    const arrayBufferResult = await WebAssembly.instantiate(arrayBuffer, imports);
    return arrayBufferResult.instance;
}
function changeExtension(filename, newExtensionWithLeadingDot) {
    const lastDotIndex = filename.lastIndexOf('.');
    if (lastDotIndex < 0) {
        throw new Error(`No extension to replace in '${filename}'`);
    }
    return filename.substr(0, lastDotIndex) + newExtensionWithLeadingDot;
}
function assertHeapIsNotLocked() {
    if (currentHeapLock) {
        throw new Error('Assertion failed - heap is currently locked');
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
            throw new Error('Trying to release a lock which isn\'t current');
        }
        currentHeapLock = null;
        while ((_a = this.postReleaseActions) === null || _a === void 0 ? void 0 : _a.length) {
            const nextQueuedAction = this.postReleaseActions.shift();
            // It's possible that the action we invoke here might itself take a succession of heap locks,
            // but since heap locks must be released synchronously, by the time we get back to this stack
            // frame, we know the heap should no longer be locked.
            nextQueuedAction();
            assertHeapIsNotLocked();
        }
    }
}

;// CONCATENATED MODULE: ./Rendering/RenderBatch/SharedMemoryRenderBatch.ts
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
        return platform.readStructField(this.batchAddress, arrayRangeReader.structLength);
    }
    disposedComponentIds() {
        return platform.readStructField(this.batchAddress, arrayRangeReader.structLength * 2);
    }
    disposedEventHandlerIds() {
        return platform.readStructField(this.batchAddress, arrayRangeReader.structLength * 3);
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
    values: (arrayRange) => platform.readObjectField(arrayRange, 0),
    count: (arrayRange) => platform.readInt32Field(arrayRange, 4),
};
// Keep in sync with memory layout in ArrayBuilderSegment
const arrayBuilderSegmentReader = {
    structLength: 12,
    values: (arrayBuilderSegment) => {
        // Evaluate arrayBuilderSegment->_builder->_items, i.e., two dereferences needed
        const builder = platform.readObjectField(arrayBuilderSegment, 0);
        const builderFieldsAddress = platform.getObjectFieldsBaseAddress(builder);
        return platform.readObjectField(builderFieldsAddress, 0);
    },
    offset: (arrayBuilderSegment) => platform.readInt32Field(arrayBuilderSegment, 4),
    count: (arrayBuilderSegment) => platform.readInt32Field(arrayBuilderSegment, 8),
};
// Keep in sync with memory layout in RenderTreeDiff.cs
const diffReader = {
    structLength: 4 + arrayBuilderSegmentReader.structLength,
    componentId: (diff) => platform.readInt32Field(diff, 0),
    edits: (diff) => platform.readStructField(diff, 4),
    editsEntry: (values, index) => arrayValuesEntry(values, index, editReader.structLength),
};
// Keep in sync with memory layout in RenderTreeEdit.cs
const editReader = {
    structLength: 20,
    editType: (edit) => platform.readInt32Field(edit, 0),
    siblingIndex: (edit) => platform.readInt32Field(edit, 4),
    newTreeIndex: (edit) => platform.readInt32Field(edit, 8),
    moveToSiblingIndex: (edit) => platform.readInt32Field(edit, 8),
    removedAttributeName: (edit) => platform.readStringField(edit, 16),
};
// Keep in sync with memory layout in RenderTreeFrame.cs
const frameReader = {
    structLength: 36,
    frameType: (frame) => platform.readInt16Field(frame, 4),
    subtreeLength: (frame) => platform.readInt32Field(frame, 8),
    elementReferenceCaptureId: (frame) => platform.readStringField(frame, 16),
    componentId: (frame) => platform.readInt32Field(frame, 12),
    elementName: (frame) => platform.readStringField(frame, 16),
    textContent: (frame) => platform.readStringField(frame, 16),
    markupContent: (frame) => platform.readStringField(frame, 16),
    attributeName: (frame) => platform.readStringField(frame, 16),
    attributeValue: (frame) => platform.readStringField(frame, 24, true),
    attributeEventHandlerId: (frame) => platform.readUint64Field(frame, 8),
};
function arrayValuesEntry(arrayValues, index, itemSize) {
    return platform.getArrayEntryPtr(arrayValues, index, itemSize);
}

;// CONCATENATED MODULE: ./BootCommon.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// Tells you if the script was added without <script src="..." autostart="false"></script>
function shouldAutoStart() {
    return !!(document &&
        document.currentScript &&
        document.currentScript.getAttribute('autostart') !== 'false');
}

;// CONCATENATED MODULE: ./Platform/WebAssemblyResourceLoader.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

const networkFetchCacheMode = 'no-cache';
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
        return Object.keys(resources)
            .map(name => this.loadResource(name, url(name), resources[name], resourceType));
    }
    loadResource(name, url, contentHash, resourceType) {
        const response = this.cacheIfUsed
            ? this.loadResourceWithCaching(this.cacheIfUsed, name, url, contentHash, resourceType)
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
        const linkerDisabledWarning = this.bootConfig.linkerEnabled ? '%c' : '\n%cThis application was built with linking (tree shaking) disabled. Published applications will be significantly smaller.';
        console.groupCollapsed(`%cblazor%c Loaded ${toDataSizeString(totalResponseBytes)} resources${linkerDisabledWarning}`, 'background: purple; color: white; padding: 1px 3px; border-radius: 3px;', 'font-weight: bold;', 'font-weight: normal;');
        if (cacheLoadsEntries.length) {
            console.groupCollapsed(`Loaded ${toDataSizeString(cacheResponseBytes)} resources from cache`);
            console.table(this.cacheLoads);
            console.groupEnd();
        }
        if (networkLoadsEntries.length) {
            console.groupCollapsed(`Loaded ${toDataSizeString(networkResponseBytes)} resources from network`);
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
            const deletionPromises = cachedRequests.map(async (cachedRequest) => {
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
            throw new Error('Content hash is required');
        }
        const cacheKey = toAbsoluteUri(`${url}.${contentHash}`);
        this.usedCacheKeys[cacheKey] = true;
        let cachedResponse;
        try {
            cachedResponse = await cache.match(cacheKey);
        }
        catch {
            // Be tolerant to errors reading from the cache. This is a guard for https://bugs.chromium.org/p/chromium/issues/detail?id=968444 where
            // chromium browsers may sometimes throw when working with the cache.
        }
        if (cachedResponse) {
            // It's in the cache.
            const responseBytes = parseInt(cachedResponse.headers.get('content-length') || '0');
            this.cacheLoads[name] = { responseBytes };
            return cachedResponse;
        }
        else {
            // It's not in the cache. Fetch from network.
            const networkResponse = await this.loadResourceWithoutCaching(name, url, contentHash, resourceType);
            this.addToCacheAsync(cache, name, cacheKey, networkResponse); // Don't await - add to cache in background
            return networkResponse;
        }
    }
    loadResourceWithoutCaching(name, url, contentHash, resourceType) {
        // Allow developers to override how the resource is loaded
        if (this.startOptions.loadBootResource) {
            const customLoadResult = this.startOptions.loadBootResource(resourceType, name, url, contentHash);
            if (customLoadResult instanceof Promise) {
                // They are supplying an entire custom response, so just use that
                return customLoadResult;
            }
            else if (typeof customLoadResult === 'string') {
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
        const responseBytes = (performanceEntry && performanceEntry.encodedBodySize) || undefined;
        this.networkLoads[name] = { responseBytes };
        // Add to cache as a custom response object so we can track extra data such as responseBytes
        // We can't rely on the server sending content-length (ASP.NET Core doesn't by default)
        const responseToCache = new Response(responseData, {
            headers: {
                'content-type': response.headers.get('content-type') || '',
                'content-length': (responseBytes || response.headers.get('content-length') || '').toString(),
            },
        });
        try {
            await cache.put(cacheKey, responseToCache);
        }
        catch {
            // Be tolerant to errors writing to the cache. This is a guard for https://bugs.chromium.org/p/chromium/issues/detail?id=968444 where
            // chromium browsers may sometimes throw when performing cache operations.
        }
    }
}
async function getCacheToUseIfEnabled(bootConfig) {
    // caches will be undefined if we're running on an insecure origin (secure means https or localhost)
    if (!bootConfig.cacheBootResources || typeof caches === 'undefined') {
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
    const relativeBaseHref = document.baseURI.substring(document.location.origin.length);
    const cacheName = `blazor-resources-${relativeBaseHref}`;
    try {
        // There's a Chromium bug we need to be aware of here: the CacheStorage APIs say that when
        // caches.open(name) returns a promise that succeeds, the value is meant to be a Cache instance.
        // However, if the browser was launched with a --user-data-dir param that's "too long" in some sense,
        // then even through the promise resolves as success, the value given is `undefined`.
        // See https://stackoverflow.com/a/46626574 and https://bugs.chromium.org/p/chromium/issues/detail?id=1054541
        // If we see this happening, return "null" to mean "proceed without caching".
        return (await caches.open(cacheName)) || null;
    }
    catch {
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
    if (typeof performance !== 'undefined') {
        return performance.getEntriesByName(url)[0];
    }
}

;// CONCATENATED MODULE: ./Platform/WebAssemblyConfigLoader.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.


class WebAssemblyConfigLoader {
    static async initAsync(bootConfigResult) {
        Blazor._internal.getApplicationEnvironment = () => BINDING.js_string_to_mono_string(bootConfigResult.applicationEnvironment);
        const configFiles = await Promise.all((bootConfigResult.bootConfig.config || [])
            .filter(name => name === 'appsettings.json' || name === `appsettings.${bootConfigResult.applicationEnvironment}.json`)
            .map(async (name) => ({ name, content: await getConfigBytes(name) })));
        Blazor._internal.getConfig = (dotNetFileName) => {
            const fileName = BINDING.conv_string(dotNetFileName);
            const resolvedFile = configFiles.find(f => f.name === fileName);
            return resolvedFile ? BINDING.js_typed_array_to_array(resolvedFile.content) : undefined;
        };
        async function getConfigBytes(file) {
            const response = await fetch(file, {
                method: 'GET',
                credentials: 'include',
                cache: 'no-cache',
            });
            return new Uint8Array(await response.arrayBuffer());
        }
    }
}

;// CONCATENATED MODULE: ./Platform/WebAssemblyComponentAttacher.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

class WebAssemblyComponentAttacher {
    constructor(components) {
        this.preregisteredComponents = components;
        const componentsById = {};
        for (let index = 0; index < components.length; index++) {
            const component = components[index];
            componentsById[component.id] = component;
        }
        this.componentsById = componentsById;
    }
    resolveRegisteredElement(id) {
        const parsedId = Number.parseInt(id);
        if (!Number.isNaN(parsedId)) {
            return toLogicalRootCommentElement(this.componentsById[parsedId].start, this.componentsById[parsedId].end);
        }
        else {
            return undefined;
        }
    }
    getParameterValues(id) {
        return this.componentsById[id].parameterValues;
    }
    getParameterDefinitions(id) {
        return this.componentsById[id].parameterDefinitions;
    }
    getTypeName(id) {
        return this.componentsById[id].typeName;
    }
    getAssembly(id) {
        return this.componentsById[id].assembly;
    }
    getId(index) {
        return this.preregisteredComponents[index].id;
    }
    getCount() {
        return this.preregisteredComponents.length;
    }
}

;// CONCATENATED MODULE: ./Services/ComponentDescriptorDiscovery.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
function discoverComponents(document, type) {
    switch (type) {
        case 'webassembly':
            return discoverWebAssemblyComponents(document);
        case 'server':
            return discoverServerComponents(document);
    }
}
function discoverServerComponents(document) {
    const componentComments = resolveComponentComments(document, 'server');
    const discoveredComponents = [];
    for (let i = 0; i < componentComments.length; i++) {
        const componentComment = componentComments[i];
        const entry = new ServerComponentDescriptor(componentComment.type, componentComment.start, componentComment.end, componentComment.sequence, componentComment.descriptor);
        discoveredComponents.push(entry);
    }
    return discoveredComponents.sort((a, b) => a.sequence - b.sequence);
}
const blazorStateCommentRegularExpression = /^\s*Blazor-Component-State:(?<state>[a-zA-Z0-9+/=]+)$/;
function discoverPersistedState(node) {
    var _a;
    if (node.nodeType === Node.COMMENT_NODE) {
        const content = node.textContent || '';
        const parsedState = blazorStateCommentRegularExpression.exec(content);
        const value = parsedState && parsedState.groups && parsedState.groups['state'];
        if (value) {
            (_a = node.parentNode) === null || _a === void 0 ? void 0 : _a.removeChild(node);
        }
        return value;
    }
    if (!node.hasChildNodes()) {
        return;
    }
    const nodes = node.childNodes;
    for (let index = 0; index < nodes.length; index++) {
        const candidate = nodes[index];
        const result = discoverPersistedState(candidate);
        if (result) {
            return result;
        }
    }
    return;
}
function discoverWebAssemblyComponents(document) {
    const componentComments = resolveComponentComments(document, 'webassembly');
    const discoveredComponents = [];
    for (let i = 0; i < componentComments.length; i++) {
        const componentComment = componentComments[i];
        const entry = new WebAssemblyComponentDescriptor(componentComment.type, componentComment.start, componentComment.end, componentComment.assembly, componentComment.typeName, componentComment.parameterDefinitions, componentComment.parameterValues);
        discoveredComponents.push(entry);
    }
    return discoveredComponents.sort((a, b) => a.id - b.id);
}
function resolveComponentComments(node, type) {
    if (!node.hasChildNodes()) {
        return [];
    }
    const result = [];
    const childNodeIterator = new ComponentCommentIterator(node.childNodes);
    while (childNodeIterator.next() && childNodeIterator.currentElement) {
        const componentComment = getComponentComment(childNodeIterator, type);
        if (componentComment) {
            result.push(componentComment);
        }
        else {
            const childResults = resolveComponentComments(childNodeIterator.currentElement, type);
            for (let j = 0; j < childResults.length; j++) {
                const childResult = childResults[j];
                result.push(childResult);
            }
        }
    }
    return result;
}
const blazorCommentRegularExpression = new RegExp(/^\s*Blazor:[^{]*(?<descriptor>.*)$/);
function getComponentComment(commentNodeIterator, type) {
    const candidateStart = commentNodeIterator.currentElement;
    if (!candidateStart || candidateStart.nodeType !== Node.COMMENT_NODE) {
        return;
    }
    if (candidateStart.textContent) {
        const definition = blazorCommentRegularExpression.exec(candidateStart.textContent);
        const json = definition && definition.groups && definition.groups['descriptor'];
        if (json) {
            try {
                const componentComment = parseCommentPayload(json);
                switch (type) {
                    case 'webassembly':
                        return createWebAssemblyComponentComment(componentComment, candidateStart, commentNodeIterator);
                    case 'server':
                        return createServerComponentComment(componentComment, candidateStart, commentNodeIterator);
                }
            }
            catch (error) {
                throw new Error(`Found malformed component comment at ${candidateStart.textContent}`);
            }
        }
        else {
            return;
        }
    }
}
function parseCommentPayload(json) {
    const payload = JSON.parse(json);
    const { type } = payload;
    if (type !== 'server' && type !== 'webassembly') {
        throw new Error(`Invalid component type '${type}'.`);
    }
    return payload;
}
function createServerComponentComment(payload, start, iterator) {
    const { type, descriptor, sequence, prerenderId } = payload;
    if (type !== 'server') {
        return undefined;
    }
    if (!descriptor) {
        throw new Error('descriptor must be defined when using a descriptor.');
    }
    if (sequence === undefined) {
        throw new Error('sequence must be defined when using a descriptor.');
    }
    if (!Number.isInteger(sequence)) {
        throw new Error(`Error parsing the sequence '${sequence}' for component '${JSON.stringify(payload)}'`);
    }
    if (!prerenderId) {
        return {
            type,
            sequence: sequence,
            descriptor,
            start,
        };
    }
    else {
        const end = getComponentEndComment(prerenderId, iterator);
        if (!end) {
            throw new Error(`Could not find an end component comment for '${start}'`);
        }
        return {
            type,
            sequence,
            descriptor,
            start,
            prerenderId,
            end,
        };
    }
}
function createWebAssemblyComponentComment(payload, start, iterator) {
    const { type, assembly, typeName, parameterDefinitions, parameterValues, prerenderId } = payload;
    if (type !== 'webassembly') {
        return undefined;
    }
    if (!assembly) {
        throw new Error('assembly must be defined when using a descriptor.');
    }
    if (!typeName) {
        throw new Error('typeName must be defined when using a descriptor.');
    }
    if (!prerenderId) {
        return {
            type,
            assembly,
            typeName,
            // Parameter definitions and values come Base64 encoded from the server, since they contain random data and can make the
            // comment invalid. We could unencode them in .NET Code, but that would be slower to do and we can leverage the fact that
            // JS provides a native function that will be much faster and that we are doing this work while we are fetching
            // blazor.boot.json
            parameterDefinitions: parameterDefinitions && atob(parameterDefinitions),
            parameterValues: parameterValues && atob(parameterValues),
            start,
        };
    }
    else {
        const end = getComponentEndComment(prerenderId, iterator);
        if (!end) {
            throw new Error(`Could not find an end component comment for '${start}'`);
        }
        return {
            type,
            assembly,
            typeName,
            // Same comment as above.
            parameterDefinitions: parameterDefinitions && atob(parameterDefinitions),
            parameterValues: parameterValues && atob(parameterValues),
            start,
            prerenderId,
            end,
        };
    }
}
function getComponentEndComment(prerenderedId, iterator) {
    while (iterator.next() && iterator.currentElement) {
        const node = iterator.currentElement;
        if (node.nodeType !== Node.COMMENT_NODE) {
            continue;
        }
        if (!node.textContent) {
            continue;
        }
        const definition = blazorCommentRegularExpression.exec(node.textContent);
        const json = definition && definition[1];
        if (!json) {
            continue;
        }
        validateEndComponentPayload(json, prerenderedId);
        return node;
    }
    return undefined;
}
function validateEndComponentPayload(json, prerenderedId) {
    const payload = JSON.parse(json);
    if (Object.keys(payload).length !== 1) {
        throw new Error(`Invalid end of component comment: '${json}'`);
    }
    const prerenderedEndId = payload.prerenderId;
    if (!prerenderedEndId) {
        throw new Error(`End of component comment must have a value for the prerendered property: '${json}'`);
    }
    if (prerenderedEndId !== prerenderedId) {
        throw new Error(`End of component comment prerendered property must match the start comment prerender id: '${prerenderedId}', '${prerenderedEndId}'`);
    }
}
class ComponentCommentIterator {
    constructor(childNodes) {
        this.childNodes = childNodes;
        this.currentIndex = -1;
        this.length = childNodes.length;
    }
    next() {
        this.currentIndex++;
        if (this.currentIndex < this.length) {
            this.currentElement = this.childNodes[this.currentIndex];
            return true;
        }
        else {
            this.currentElement = undefined;
            return false;
        }
    }
}
class ServerComponentDescriptor {
    constructor(type, start, end, sequence, descriptor) {
        this.type = type;
        this.start = start;
        this.end = end;
        this.sequence = sequence;
        this.descriptor = descriptor;
    }
    toRecord() {
        const result = { type: this.type, sequence: this.sequence, descriptor: this.descriptor };
        return result;
    }
}
class WebAssemblyComponentDescriptor {
    constructor(type, start, end, assembly, typeName, parameterDefinitions, parameterValues) {
        this.id = WebAssemblyComponentDescriptor.globalId++;
        this.type = type;
        this.assembly = assembly;
        this.typeName = typeName;
        this.parameterDefinitions = parameterDefinitions;
        this.parameterValues = parameterValues;
        this.start = start;
        this.end = end;
    }
}
WebAssemblyComponentDescriptor.globalId = 1;

;// CONCATENATED MODULE: ./JSInitializers/JSInitializers.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

class JSInitializer {
    constructor() {
        this.afterStartedCallbacks = [];
    }
    async importInitializersAsync(initializerFiles, initializerArguments) {
        await Promise.all(initializerFiles.map(f => importAndInvokeInitializer(this, f)));
        function adjustPath(path) {
            // This is the same we do in JS interop with the import callback
            const base = document.baseURI;
            path = base.endsWith('/') ? `${base}${path}` : `${base}/${path}`;
            return path;
        }
        async function importAndInvokeInitializer(jsInitializer, path) {
            const adjustedPath = adjustPath(path);
            const initializer = await import(/* webpackIgnore: true */ adjustedPath);
            if (initializer === undefined) {
                return;
            }
            const { beforeStart: beforeStart, afterStarted: afterStarted } = initializer;
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
        await Promise.all(this.afterStartedCallbacks.map(callback => callback(blazor)));
    }
}

;// CONCATENATED MODULE: ./JSInitializers/JSInitializers.WebAssembly.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

async function fetchAndInvokeInitializers(bootConfig, options) {
    const initializers = bootConfig.resources.libraryInitializers;
    const jsInitializer = new JSInitializer();
    if (initializers) {
        await jsInitializer.importInitializersAsync(Object.keys(initializers), [options, bootConfig.resources.extensions]);
    }
    return jsInitializer;
}

;// CONCATENATED MODULE: ./Boot.WebAssembly.ts
// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
/* eslint-disable array-element-newline */














let started = false;
async function boot(options) {
    if (started) {
        throw new Error('Blazor has already started.');
    }
    started = true;
    if (inAuthRedirectIframe()) {
        // eslint-disable-next-line @typescript-eslint/no-empty-function
        await new Promise(() => { }); // See inAuthRedirectIframe for explanation
    }
    setDispatchEventMiddleware((browserRendererId, eventHandlerId, continuation) => {
        // It's extremely unusual, but an event can be raised while we're in the middle of synchronously applying a
        // renderbatch. For example, a renderbatch might mutate the DOM in such a way as to cause an <input> to lose
        // focus, in turn triggering a 'change' event. It may also be possible to listen to other DOM mutation events
        // that are themselves triggered by the application of a renderbatch.
        const renderer = getRendererer(browserRendererId);
        if (renderer.eventDelegator.getHandler(eventHandlerId)) {
            monoPlatform.invokeWhenHeapUnlocked(continuation);
        }
    });
    Blazor._internal.applyHotReload = (id, metadataDelta, ilDelta, pdbDelta) => {
        DotNet.invokeMethod('Microsoft.AspNetCore.Components.WebAssembly', 'ApplyHotReloadDelta', id, metadataDelta, ilDelta, pdbDelta);
    };
    Blazor._internal.getApplyUpdateCapabilities = () => DotNet.invokeMethod('Microsoft.AspNetCore.Components.WebAssembly', 'GetApplyUpdateCapabilities');
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
            renderBatch(browserRendererId, new SharedMemoryRenderBatch(batchAddress));
        }
        finally {
            heapLock.release();
        }
    };
    // Configure navigation via JS Interop
    const getBaseUri = Blazor._internal.navigationManager.getBaseURI;
    const getLocationHref = Blazor._internal.navigationManager.getLocationHref;
    Blazor._internal.navigationManager.getUnmarshalledBaseURI = () => BINDING.js_string_to_mono_string(getBaseUri());
    Blazor._internal.navigationManager.getUnmarshalledLocationHref = () => BINDING.js_string_to_mono_string(getLocationHref());
    Blazor._internal.navigationManager.listenForNavigationEvents(async (uri, intercepted) => {
        await DotNet.invokeMethodAsync('Microsoft.AspNetCore.Components.WebAssembly', 'NotifyLocationChanged', uri, intercepted);
    });
    const candidateOptions = options !== null && options !== void 0 ? options : {};
    // Get the custom environment setting and blazorBootJson loader if defined
    const environment = candidateOptions.environment;
    // Fetch the resources and prepare the Mono runtime
    const bootConfigPromise = BootConfigResult.initAsync(candidateOptions.loadBootResource, environment);
    // Leverage the time while we are loading boot.config.json from the network to discover any potentially registered component on
    // the document.
    const discoveredComponents = discoverComponents(document, 'webassembly');
    const componentAttacher = new WebAssemblyComponentAttacher(discoveredComponents);
    Blazor._internal.registeredComponents = {
        getRegisteredComponentsCount: () => componentAttacher.getCount(),
        getId: (index) => componentAttacher.getId(index),
        getAssembly: (id) => BINDING.js_string_to_mono_string(componentAttacher.getAssembly(id)),
        getTypeName: (id) => BINDING.js_string_to_mono_string(componentAttacher.getTypeName(id)),
        getParameterDefinitions: (id) => BINDING.js_string_to_mono_string(componentAttacher.getParameterDefinitions(id) || ''),
        getParameterValues: (id) => BINDING.js_string_to_mono_string(componentAttacher.getParameterValues(id) || ''),
    };
    Blazor._internal.getPersistedState = () => BINDING.js_string_to_mono_string(discoverPersistedState(document) || '');
    Blazor._internal.attachRootComponentToElement = (selector, componentId, rendererId) => {
        const element = componentAttacher.resolveRegisteredElement(selector);
        if (!element) {
            attachRootComponentToElement(selector, componentId, rendererId);
        }
        else {
            attachRootComponentToLogicalElement(rendererId, element, componentId, false);
        }
    };
    const bootConfigResult = await bootConfigPromise;
    const jsInitializer = await fetchAndInvokeInitializers(bootConfigResult.bootConfig, candidateOptions);
    const [resourceLoader] = await Promise.all([
        WebAssemblyResourceLoader.initAsync(bootConfigResult.bootConfig, candidateOptions || {}),
        WebAssemblyConfigLoader.initAsync(bootConfigResult),
    ]);
    try {
        await platform.start(resourceLoader);
    }
    catch (ex) {
        throw new Error(`Failed to start platform. Reason: ${ex}`);
    }
    // Start up the application
    platform.callEntryPoint(resourceLoader.bootConfig.entryAssembly);
    // At this point .NET has been initialized (and has yielded), we can't await the promise becasue it will
    // only end when the app finishes running
    jsInitializer.invokeAfterStartedCallbacks(Blazor);
}
function invokeJSFromDotNet(callInfo, arg0, arg1, arg2) {
    const functionIdentifier = monoPlatform.readStringField(callInfo, 0);
    const resultType = monoPlatform.readInt32Field(callInfo, 4);
    const marshalledCallArgsJson = monoPlatform.readStringField(callInfo, 8);
    const targetInstanceId = monoPlatform.readUint64Field(callInfo, 20);
    if (marshalledCallArgsJson !== null) {
        const marshalledCallAsyncHandle = monoPlatform.readUint64Field(callInfo, 12);
        if (marshalledCallAsyncHandle !== 0) {
            DotNet.jsCallDispatcher.beginInvokeJSFromDotNet(marshalledCallAsyncHandle, functionIdentifier, marshalledCallArgsJson, resultType, targetInstanceId);
            return 0;
        }
        else {
            const resultJson = DotNet.jsCallDispatcher.invokeJSFromDotNet(functionIdentifier, marshalledCallArgsJson, resultType, targetInstanceId);
            return resultJson === null ? 0 : BINDING.js_string_to_mono_string(resultJson);
        }
    }
    else {
        const func = DotNet.jsCallDispatcher.findJSFunction(functionIdentifier, targetInstanceId);
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
    const resultJsonOrErrorMessageString = BINDING.conv_string(resultJsonOrErrorMessage);
    DotNet.jsCallDispatcher.endInvokeDotNetFromJS(callIdString, successBool, resultJsonOrErrorMessageString);
}
function receiveByteArray(id, data) {
    const idLong = id;
    const dataByteArray = monoPlatform.toUint8Array(data);
    DotNet.jsCallDispatcher.receiveByteArray(idLong, dataByteArray);
}
function retrieveByteArray() {
    if (byteArrayBeingTransferred === null) {
        throw new Error('Byte array not available for transfer');
    }
    const typedArray = BINDING.js_typed_array_to_array(byteArrayBeingTransferred);
    return typedArray;
}
function inAuthRedirectIframe() {
    // We don't want the .NET runtime to start up a second time inside the AuthenticationService.ts iframe. It uses resources
    // unnecessarily and can lead to errors (#37355), plus the behavior is not well defined as the frame will be terminated shortly.
    // So, if we're in that situation, block the startup process indefinitely so that anything chained to Blazor.start never happens.
    // The detection logic here is based on the equivalent check in AuthenticationService.ts.
    // TODO: Later we want AuthenticationService.ts to become responsible for doing this via a JS initializer. Doing it here is a
    //       tactical fix for .NET 6 so we don't have to change how authentication is initialized.
    if (window.parent !== window && !window.opener && window.frameElement) {
        const settingsJson = window.sessionStorage && window.sessionStorage['Microsoft.AspNetCore.Components.WebAssembly.Authentication.CachedAuthSettings'];
        const settings = settingsJson && JSON.parse(settingsJson);
        return settings && settings.redirect_uri && location.href.startsWith(settings.redirect_uri);
    }
    return false;
}
Blazor.start = boot;
if (shouldAutoStart()) {
    boot().catch(error => {
        if (typeof Module !== 'undefined' && Module.printErr) {
            // Logs it, and causes the error UI to appear
            Module.printErr(error);
        }
        else {
            // The error must have happened so early we didn't yet set up the error UI, so just log to console
            console.error(error);
        }
    });
}

/******/ })()
;