using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using Microsoft.JSInterop;
using SpawnDev.BlazorJS.JSObjects;
using SpawnDev.BlazorJS.JsonConverters;
using System.Reflection;
using System.Text.Json;

namespace SpawnDev.BlazorJS
{
    public partial class BlazorJSRuntime : IBlazorJSRuntime
    {
        internal static readonly IJSInProcessRuntime _js;
        public static BlazorJSRuntime JS { get; internal set; }
        internal static JsonSerializerOptions? RuntimeJsonSerializerOptions { get; private set; }
        public Window? WindowThis { get; private set; } = null;
        public DedicatedWorkerGlobalScope? DedicateWorkerThis { get; private set; } = null;
        public SharedWorkerGlobalScope? SharedWorkerThis { get; private set; } = null;
        public string GlobalThisTypeName { get; private set; }
        public JSObject GlobalThis { get; private set; }
        public bool IsWindow => GlobalThis is Window;
        public bool IsWorker => IsDedicatedWorkerGlobalScope || IsSharedWorkerGlobalScope || IsServiceWorkerGlobalScope;
        public bool IsDedicatedWorkerGlobalScope => GlobalThis is DedicatedWorkerGlobalScope;
        public bool IsSharedWorkerGlobalScope => GlobalThis is SharedWorkerGlobalScope;
        public bool IsServiceWorkerGlobalScope => GlobalThis is ServiceWorkerGlobalScope;

        static BlazorJSRuntime()
        {
            _js = (IJSInProcessRuntime)typeof(WebAssemblyHost).Assembly.GetType("Microsoft.AspNetCore.Components.WebAssembly.Services.DefaultWebAssemblyJSRuntime").GetField("Instance", BindingFlags.NonPublic | BindingFlags.Static).GetValue(null);
            RuntimeJsonSerializerOptions = (JsonSerializerOptions)typeof(JSRuntime).GetProperty("JsonSerializerOptions", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(_js, null);
            RuntimeJsonSerializerOptions.Converters.Add(new JSObjectConverterFactory());
            RuntimeJsonSerializerOptions.Converters.Add(new IJSObjectConverterFactory());
            RuntimeJsonSerializerOptions.Converters.Add(new ActionConverterFactory());
        }

        internal BlazorJSRuntime()
        {
            GlobalThisTypeName = GetConstructorName("globalThis");
            switch (GlobalThisTypeName)
            {
                case nameof(Window):
                    WindowThis = Get<Window>("globalThis");
                    GlobalThis = WindowThis;
                    break;
                case nameof(DedicatedWorkerGlobalScope):
                    DedicateWorkerThis = Get<DedicatedWorkerGlobalScope>("globalThis");
                    GlobalThis = DedicateWorkerThis;
                    break;
                case nameof(SharedWorkerGlobalScope):
                    SharedWorkerThis = Get<SharedWorkerGlobalScope>("globalThis");
                    GlobalThis = SharedWorkerThis;
                    break;
                default:
                    GlobalThis = Get<JSObject>("globalThis");
                    break;
            }
#if DEBUG
            Log("JS.GlobalThisTypeName", GlobalThisTypeName);
            Set("JSInterop.debugLevel", 1);
#endif
        }


        public string InformationalVersion { get; } = typeof(JSObject).Assembly.GetAssemblyInformationalVersion();
        public string FileVersion { get; } = typeof(JSObject).Assembly.GetAssemblyFileVersion();

        public void DisposeCallback(string callbackerID) => JSInterop.DisposeCallbacker(callbackerID);

        // document
        // document.CreateElement
        public static bool JSEquals(object obj1, object obj2) => JSInterop.IsEqual(obj1, obj2);
        public T ReturnMe<T>(object obj) => JSInterop.ReturnMe<T>(obj);
        public T ReturnMe<T>(T obj) => JSInterop.ReturnMe<T>(obj);
        public IJSInProcessObjectReference ToJSRef(ElementReference elementRef) => ReturnMe<IJSInProcessObjectReference>(elementRef);
        public IJSInProcessObjectReference NewApply(string className, object?[]? args = null) => JSInterop.ReturnNew<IJSInProcessObjectReference>(className, args);
        public IJSInProcessObjectReference New(string className) => NewApply(className);
        public IJSInProcessObjectReference New(string className, object arg0) => NewApply(className, new object[] { arg0 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1) => NewApply(className, new object[] { arg0, arg1 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2) => NewApply(className, new object[] { arg0, arg1, arg2 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3) => NewApply(className, new object[] { arg0, arg1, arg2, arg3 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4) => NewApply(className, new object[] { arg0, arg1, arg2, arg3, arg4 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5) => NewApply(className, new object[] { arg0, arg1, arg2, arg3, arg4, arg5 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6) => NewApply(className, new object[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7) => NewApply(className, new object[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8) => NewApply(className, new object[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 });
        public IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9) => NewApply(className, new object[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 });
        public bool IsUndefined(JSObject obj, string identifier = "") => JSInterop.TypeOf(obj, identifier) == "undefined";
        public bool IsUndefined(string identifier) => JSInterop.TypeOf(null, identifier) == "undefined";
        public string TypeOf(JSObject obj, string identifier = "") => JSInterop.TypeOf(obj, identifier);
        public string TypeOf(string identifier) => JSInterop.TypeOf(null, identifier);
        public void Log(params object?[] args) => CallApplyVoid("console.log", args);
        public string GetConstructorName(string identifier) => Get<string>($"{identifier}.constructor.name");
    }
}
