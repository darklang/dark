'use strict';
// https://learn.microsoft.com/en-us/aspnet/core/blazor/javascript-interoperability/?view=aspnetcore-7.0#inject-a-script-after-blazor-starts

// get the globalThis instance
// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis
function checkIfGlobalThis(it) {
    // Math is known to exist as a global in every environment.
    return it && it.Math === Math && it;
}

const globalObject =
    // eslint-disable-next-line es/no-global-this -- safe
    checkIfGlobalThis(typeof globalThis == 'object' && globalThis) ||
    checkIfGlobalThis(typeof window == 'object' && window) ||
    // eslint-disable-next-line no-restricted-globals -- safe
    checkIfGlobalThis(typeof self == 'object' && self) ||
    checkIfGlobalThis(typeof global == 'object' && global) ||
    // eslint-disable-next-line no-new-func -- fallback
    (function () { return this; })() || Function('return this')();

var initFunc = function () {
    if (globalObject.JSInterop) return;
    const JSInterop = {};
    globalObject.JSInterop = JSInterop;
    JSInterop.globalObject = globalObject;
    JSInterop.debugLevel = 0;
    JSInterop.pathObjectInfo = function (rootObject, path) {
        var parent = rootObject ?? globalObject;
        var target;
        var propertyName = null;
        if (typeof path === 'string' && path.length) {
            if (path === 'Symbol.asyncIterator' && parent !== null) {
                // get the async iterator
                target = parent[Symbol.asyncIterator]();
            } else {
                var parts = path.split('.');
                propertyName = parts[parts.length - 1];
                for (var i = 0; i < parts.length - 1; i++) {
                    parent = parent[parts[i]];
                }
                target = parent[propertyName];
            }
        }
        else if (typeof path === 'number') {
            propertyName = path;
            target = parent[propertyName];
        } else {
            target = parent;
            parent = null;
        }
        var targetType = typeof target;
        var exists = targetType !== 'undefined' || (parent && propertyName && propertyName in parent);
        return {
            parent,         // may be null even if target exists (Ex. if no path was given)
            propertyName,   // may be null even if target exists (Ex. if no path was given)
            target,         // may be undefined if it does not currently exist
            targetType,     // will always be a string of the target type (Ex. 'undefined', 'object', 'string', 'number')
            exists,         // will always be a bool value (true or false)
        };
    };

    JSInterop.pathToTarget = function (rootObject, path) {
        return JSInterop.pathObjectInfo(rootObject, path).target;
    };

    JSInterop.pathToParent = function (rootObject, path) {
        return JSInterop.pathObjectInfo(rootObject, path).parent;
    };

    JSInterop.setPromiseThenCatch = function(promise, thenCallback, catchCallback){
        promise.then(thenCallback).catch(function (ex) {
            let err = "";
            if (ex) {
                if (typeof ex == 'string') {
                    err = ex;
                } else if (typeof ex == 'object') {
                    if (ex.constructor.name == 'Error') {
                        err = ex.message;
                    }
                }
            }
            if (!err) err = 'unknown error';
            catchCallback(err);
        });
    };

    JSInterop.__equals = function (obj1, obj2) {
        return obj1 === obj2;
    };

    JSInterop._returnMe = function (obj, returnType) {
        return serializeToDotNet(obj, returnType);
    };

    JSInterop._returnNew = function (obj, className, args, returnType) {
        var { target, parent, targetType } = JSInterop.pathObjectInfo(obj, className);
        var ret = !args ? new target() : new target(...args);
        return serializeToDotNet(ret, returnType);
    };

    JSInterop._getPropertyNames = function (obj, name, hasOwnProperty) {
        var target = JSInterop.pathToTarget(obj, name);
        var ret = [];
        for (var k in target) {
            if (!hasOwnProperty || (typeof target.hasOwnProperty === 'function' && target.hasOwnProperty(k))) ret.push(k);
        }
        return ret;
    };

    JSInterop._instanceof = function (obj, name) {
        var target = JSInterop.pathToTarget(obj, name);
        return target && target.constructor ? target.constructor.name : '';
    };

    JSInterop._typeof = function (obj, name) {
        var target = JSInterop.pathToTarget(obj, name);
        return typeof target;
    };

    function wrapFunction(fn) {
        var retRef = {
            __wrappedFunction: fn,
            applyFn: function (thisObj, args) {
                return fn.apply(thisObj, args);
            }
        };
        return retRef;
    }

    function serializeToDotNet(value, returnType) {
        var typeOfValue = typeof value;
        if (typeOfValue === 'undefined') {
            value = null;
        } else if (typeOfValue === 'function') {
            value = wrapFunction(value);
        }
        if (!returnType) {
            return value;
        }
        let isOverridden = returnType >= 128;
        if (!isOverridden) {
            return value;
        }
        let desiredType = returnType - 128;
        switch (desiredType) {
            case DotNet.JSCallResultType.Default:
                return value;
            case DotNet.JSCallResultType.JSObjectReference:
                return value === null ? null : DotNet.createJSObjectReference(value);
            case DotNet.JSCallResultType.JSStreamReference: {
                const n = DotNet.createJSStreamReference(value),
                    r = JSON.stringify(n);
                return ct.js_string_to_mono_string(r)
            }
            case DotNet.JSCallResultType.JSVoidResult:
                return null;
            default:
                throw new Error(`Invalid JS call result type '${a}'.`)
        }
    }

    // Instance
    JSInterop._set = function (obj, identifier, value) {
        if (!obj || (typeof obj !== 'object' && typeof obj !== 'function')) throw 'obj null or undefined';
        var pathInfo = JSInterop.pathObjectInfo(obj, identifier);
        if (!pathInfo.exists) {
            if (JSInterop.debugLevel > 0) {
                var targetType = pathInfo.parent ? pathInfo.parent.constructor.name : '[NULL]';
                console.log('NOTICE: JSInterop._set - property being set does not exist', targetType, identifier);
            }
        }
        pathInfo.parent[pathInfo.propertyName] = value;
    };

    JSInterop._get = function (obj, identifier, returnType) {
        var ret = null;
        if (!obj || (typeof obj !== 'object' && typeof obj !== 'function')) throw 'obj null or undefined';
        var { target, parent, targetType } = JSInterop.pathObjectInfo(obj, identifier);
        if (targetType === "function") {
            ret = target.bind(parent);
        } else {
            ret = target;
        }
        return serializeToDotNet(ret, returnType);
    };

    JSInterop._call = function (obj, identifier, args, returnType) {
        var ret = null;
        if (!obj || (typeof obj !== 'object' && typeof obj !== 'function')) throw 'obj null or undefined';
        var { target, parent, targetType } = JSInterop.pathObjectInfo(obj, identifier);
        if (targetType === "function") {
            //var fnBound = target.bind(parent);
            //ret = fnBound.apply(null, args);
            ret = target.apply(parent, args);
        } else {
            throw 'Call target is not a function';
        }
        return serializeToDotNet(ret, returnType);
    };

    //Global
    JSInterop._setGlobal = function (identifier, value) {
        JSInterop._set(globalObject, identifier, value);
    };

    JSInterop._getGlobal = function (identifier, returnType) {
        return JSInterop._get(globalObject, identifier, returnType);
    };

    JSInterop._callGlobal = function (identifier, args, returnType) {
        return JSInterop._call(globalObject, identifier, args, returnType);
    };

    const callbacks = {};
    JSInterop.DisposeCallbacker = function (callbackerID) {
        if (callbacks[callbackerID]) delete callbacks[callbackerID];
    };
    DotNet.attachReviver(function (key, value) {
        if (value && typeof value === 'object' && typeof value.__undefinedref__ !== 'undefined') {
            return;
        }
        else 
        if (value && typeof value === 'object' && typeof value.__wrappedFunction === 'function') {
            return value.__wrappedFunction;
        }
        else if (value && typeof value === 'object' && typeof value._callbackId !== 'undefined') {
            let callbackId = value._callbackId;
            if (!callbacks[callbackId]) {
                callbacks[callbackId] = function fn() {
                    var ret = null;
                    if (!callbacks[callbackId] || fn !== callbacks[callbackId]) return;
                    var args = ["Invoke"];
                    // When the Callback is created the argument types are enumerated so they can be passed back to .Net correctly when the callback is called
                    var paramTypes = value._paramTypes;
                    for (var i = 0; i < paramTypes.length; i++) {
                        var v = i < arguments.length ? arguments[i] : null;
                        let jsCallResultType = paramTypes[i];
                        v = serializeToDotNet(v, jsCallResultType);
                        args.push(v);
                    }
                    try {
                        ret = value._callback.invokeMethod.apply(value._callback, args);// ('Callback.Invoke');
                        if (!value._returnVoid) {
                            return ret;
                        }
                    } catch (ex) {
                        if (args && args.length > 0) {
                            console.log('args[0].constructor.name', args[0].constructor.name);
                        }
                        console.log(args);
                        console.log('callback invokeMethod error:', ret, ex);
                        //console.log('disposing callback');
                        //value.isDisposed = true;    //
                        //DisposeCallbacker(callbackId);
                    }
                };
            }
            return callbacks[callbackId];
        } else {
            return value;
        }
    });
};
initFunc();


//export function beforeStart(options, extensions) {
//    console.log("blazorjs beforeStart");
//}

//export function afterStarted(blazor) {

//    Object.defineProperty(Blazor, 'DotNet', {
//        value: globalThis.DotNet,
//        writable: false,
//    });
//    ////console.log("blazorjs afterStarted");
//    //if (typeof JSInterop._afterStarted === 'function') {
//    //    let callback = JSInterop._afterStarted;
//    //    delete JSInterop._afterStarted;
//    //    callback();
//    //}
//}
