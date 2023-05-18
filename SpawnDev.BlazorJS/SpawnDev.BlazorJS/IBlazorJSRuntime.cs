using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;
using SpawnDev.BlazorJS.JSObjects;

namespace SpawnDev.BlazorJS
{
    public interface IBlazorJSRuntime
    {
        DedicatedWorkerGlobalScope? DedicateWorkerThis { get; }
        string FileVersion { get; }
        JSObject GlobalThis { get; }
        string GlobalThisTypeName { get; }
        string InformationalVersion { get; }
        bool IsDedicatedWorkerGlobalScope { get; }
        bool IsServiceWorkerGlobalScope { get; }
        bool IsSharedWorkerGlobalScope { get; }
        bool IsWindow { get; }
        bool IsWorker { get; }
        SharedWorkerGlobalScope? SharedWorkerThis { get; }
        Window? WindowThis { get; }

        object? Call(Type returnType, string identifier);
        object? Call(Type returnType, string identifier, object? arg0);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8);
        object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9);
        T Call<T>(string identifier);
        T Call<T>(string identifier, object? arg0);
        T Call<T>(string identifier, object? arg0, object? arg1);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8);
        T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9);
        object? CallApply(Type returnType, string identifier, object?[]? args = null);
        T CallApply<T>(string identifier, object?[]? args = null);
        Task<T> CallApplyAsync<T>(string identifier, object?[]? args = null);
        void CallApplyVoid(string identifier, object?[]? args = null);
        Task CallApplyVoidAsync(string identifier, object?[]? args = null);
        Task<T> CallAsync<T>(string identifier);
        Task<T> CallAsync<T>(string identifier, object? arg0);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8);
        Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9);
        void CallVoid(string identifier);
        void CallVoid(string identifier, object? arg0);
        void CallVoid(string identifier, object? arg0, object? arg1);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8);
        void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9);
        Task CallVoidAsync(string identifier);
        Task CallVoidAsync(string identifier, object? arg0);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8);
        Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9);
        void DisposeCallback(string callbackerID);
        object? Get(Type returnType, string identifier);
        T Get<T>(string identifier);
        Task<T> GetAsync<T>(string identifier);
        string GetConstructorName(string identifier);
        bool IsUndefined(JSObject obj, string identifier = "");
        bool IsUndefined(string identifier);
        void Log(params object?[] args);
        IJSInProcessObjectReference New(string className);
        IJSInProcessObjectReference New(string className, object arg0);
        IJSInProcessObjectReference New(string className, object arg0, object arg1);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8);
        IJSInProcessObjectReference New(string className, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8, object arg9);
        IJSInProcessObjectReference NewApply(string className, object?[]? args = null);
        T ReturnMe<T>(object obj);
        void Set(string identifier, object? value);
        IJSInProcessObjectReference ToJSRef(ElementReference elementRef);
        string TypeOf(JSObject obj, string identifier = "");
        string TypeOf(string identifier);
    }
}