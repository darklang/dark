using Microsoft.JSInterop;
using SpawnDev.BlazorJS.JSObjects;
using SpawnDev.BlazorJS.JsonConverters;

namespace SpawnDev.BlazorJS {
    public static partial class JSInterop {
        static IJSInProcessRuntime _js = BlazorJSRuntime._js;

        internal static void SetPromiseThenCatch<T>(Promise<T> promise, ActionCallback<T> thenCallback, ActionCallback<string> catchCallback)
        {
            BlazorJSRuntime.JS.CallVoid("JSInterop.setPromiseThenCatch", promise, thenCallback, catchCallback);
        }
        internal static void SetPromiseThenCatch(Promise promise, ActionCallback thenCallback, ActionCallback<string> catchCallback)
        {
            BlazorJSRuntime.JS.CallVoid("JSInterop.setPromiseThenCatch", promise, thenCallback, catchCallback);
        }
        internal static void SetPromiseThenCatch<TResult>(Promise promise, ActionCallback<TResult> thenCallback, ActionCallback<string> catchCallback)
        {
            BlazorJSRuntime.JS.CallVoid("JSInterop.setPromiseThenCatch", promise, thenCallback, catchCallback);
        }

        // *********************************************************************************************************************
        internal static void SetGlobal(string identifier, object? value) {
            _js.InvokeVoid("JSInterop._setGlobal", identifier, value);
        }
        internal static void SetGlobal(int identifier, object? value) {
            _js.InvokeVoid("JSInterop._setGlobal", identifier, value);
        }

        internal static void CallGlobalVoid(string identifier, object?[]? args) {
            _js.InvokeVoid("JSInterop._callGlobal", identifier, args, JSCallResultType.JSVoidResult);
        }

        internal static Task CallGlobalVoidAsync(string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<Task>();
            return _js.Invoke<Task>("JSInterop._callGlobal", identifier, args, jsCallResultType);
        }

        internal static T CallGlobal<T>(string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._callGlobal", identifier, args, jsCallResultType);
        }

        internal static Task<T> CallGlobalAsync<T>(string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<Task<T>>();
            return _js.Invoke<Task<T>>("JSInterop._callGlobal", identifier, args, jsCallResultType);
        }

        internal static object? CallGlobal(Type returnType, string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._callGlobal", identifier, args, jsCallResultType);
        }

        internal static T GetGlobal<T>(string identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._getGlobal", identifier, jsCallResultType);
        }

        internal static Task<T> GetGlobalAsync<T>(string identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<Task<T>>();
            return _js.Invoke<Task<T>>("JSInterop._getGlobal", identifier, jsCallResultType);
        }

        internal static object? GetGlobal(Type returnType, string identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._getGlobal", identifier, jsCallResultType);
        }

        internal static T Call<T>(IJSInProcessObjectReference targetObject, string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._call", targetObject, identifier, args, jsCallResultType);
        }

        internal static Task<T> CallAsync<T>(IJSInProcessObjectReference targetObject, string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<Task<T>>();
            return _js.Invoke<Task<T>>("JSInterop._call", targetObject, identifier, args, jsCallResultType);
        }

        internal static object? Call(Type returnType, IJSInProcessObjectReference targetObject, string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._call", targetObject, identifier, args, jsCallResultType);
        }

        internal static void CallVoid(IJSInProcessObjectReference targetObject, string identifier, object?[]? args) {
            _js.InvokeVoid("JSInterop._call", targetObject, identifier, args, JSCallResultType.JSVoidResult);
        }

        internal static Task CallVoidAsync(IJSInProcessObjectReference targetObject, string identifier, object?[]? args) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<Task>();
            return _js.Invoke<Task>("JSInterop._call", targetObject, identifier, args, jsCallResultType);
        }

        internal static T Get<T>(IJSInProcessObjectReference targetObject, string identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._get", targetObject, identifier, jsCallResultType);
        }

        internal static Task<T> GetAsync<T>(IJSInProcessObjectReference targetObject, string identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<Task<T>>();
            return _js.Invoke<Task<T>>("JSInterop._get", targetObject, identifier, jsCallResultType);
        }

        internal static object? Get(Type returnType, IJSInProcessObjectReference targetObject, string identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._get", targetObject, identifier, jsCallResultType);
        }

        internal static Task<T> GetAsync<T>(IJSInProcessObjectReference targetObject, int identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<Task<T>>();
            return _js.Invoke<Task<T>>("JSInterop._get", targetObject, identifier, jsCallResultType);
        }

        internal static T Get<T>(IJSInProcessObjectReference targetObject, int identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._get", targetObject, identifier, jsCallResultType);
        }

        internal static object? Get(Type returnType, IJSInProcessObjectReference targetObject, int identifier) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._get", targetObject, identifier, jsCallResultType);
        }

        internal static T ReturnMe<T>(object? obj1) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._returnMe", obj1, jsCallResultType);
        }

        internal static object? ReturnMe(Type returnType, object? obj1) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._returnMe", obj1, jsCallResultType);
        }

        internal static IJSInProcessObjectReference ReturnNew(IJSInProcessObjectReference targetObject, object?[]? args = null)
        {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<IJSInProcessObjectReference>();
            return _js.Invoke<IJSInProcessObjectReference>("JSInterop._returnNew", targetObject, null, args, jsCallResultType);
        }

        internal static T ReturnNew<T>(IJSInProcessObjectReference targetObject, object?[]? args = null)
        {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._returnNew", targetObject, null, args, jsCallResultType);
        }

        internal static object? ReturnNew(IJSInProcessObjectReference targetObject, Type returnType, object?[]? args = null)
        {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._returnNew", targetObject, null, args, jsCallResultType);
        }

        internal static IJSInProcessObjectReference ReturnNew(string className, object?[]? args = null)
        {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<IJSInProcessObjectReference>();
            return _js.Invoke<IJSInProcessObjectReference>("JSInterop._returnNew", null, className, args, jsCallResultType);
        }

        internal static T ReturnNew<T>(string className, object?[]? args = null) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric<T>();
            return _js.Invoke<T>("JSInterop._returnNew", null, className, args, jsCallResultType);
        }

        internal static object? ReturnNew(Type returnType, string className, object?[]? args = null) {
            var jsCallResultType = JSCallResultTypeHelperOverride.FromGeneric(returnType);
            return _js.Invoke(returnType, "JSInterop._returnNew", null, className, args, jsCallResultType);
        }

        internal static void Set(IJSInProcessObjectReference targetObject, string identifier, object? value) {
            _js.InvokeVoid("JSInterop._set", targetObject, identifier, value);
        }

        internal static void Set(IJSInProcessObjectReference targetObject, int identifier, object? value) {
            _js.InvokeVoid("JSInterop._set", targetObject, identifier, value);
        }

        internal static void DisposeCallbacker(string callbackId) {
            _js.InvokeVoid("JSInterop.DisposeCallbacker", callbackId);
        }

        internal static bool IsEqual(object? obj1, object? obj2) {
            return _js.Invoke<bool>("JSInterop.__equals", obj1, obj2);
        }

        internal static string TypeOf(object? obj, string identifier = "") {
            return _js.Invoke<string>("JSInterop._typeof", obj, identifier);
        }

        internal static string InstanceOf(object? obj, string identifier = "") {
            return _js.Invoke<string>("JSInterop._instanceof", obj, identifier);
        }

        internal static List<string> GetPropertyNames(object? obj, string identifier = "", bool hasOwnProperty = true) {
            return _js.Invoke<List<string>>("JSInterop._getPropertyNames", obj, identifier, hasOwnProperty);
        }

    }
}
