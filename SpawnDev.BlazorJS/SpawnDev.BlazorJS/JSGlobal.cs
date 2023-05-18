namespace SpawnDev.BlazorJS {
    public partial class BlazorJSRuntime {

        public void Set(string identifier, object? value) => JSInterop.SetGlobal(identifier, value);
        public T Get<T>(string identifier) => JSInterop.GetGlobal<T>(identifier);
        public object? Get(Type returnType, string identifier) => JSInterop.GetGlobal(returnType, identifier);
        public Task<T> GetAsync<T>(string identifier) => JSInterop.GetGlobalAsync<T>(identifier);
        public T CallApply<T>(string identifier, object?[]? args = null) => JSInterop.CallGlobal<T>(identifier, args);
        public object? CallApply(Type returnType, string identifier, object?[]? args = null) => JSInterop.CallGlobal(returnType, identifier, args);
        public void CallApplyVoid(string identifier, object?[]? args = null) => JSInterop.CallGlobalVoid(identifier, args);
        public Task<T> CallApplyAsync<T>(string identifier, object?[]? args = null) => JSInterop.CallGlobalAsync<T>(identifier, args);
        public Task CallApplyVoidAsync(string identifier, object?[]? args = null) => JSInterop.CallGlobalVoidAsync(identifier, args);

        public T Call<T>(string identifier) => CallApply<T>(identifier);
        public T Call<T>(string identifier, object? arg0) => CallApply<T>(identifier, new object?[] { arg0 });
        public T Call<T>(string identifier, object? arg0, object? arg1) => CallApply<T>(identifier, new object?[] { arg0, arg1 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2, arg3 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 });
        public T Call<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9) => CallApply<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 });

        public object? Call(Type returnType, string identifier) => CallApply(returnType, identifier);
        public object? Call(Type returnType, string identifier, object? arg0) => CallApply(returnType, identifier, new object?[] { arg0 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1) => CallApply(returnType, identifier, new object?[] { arg0, arg1 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2, arg3 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2, arg3, arg4 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 });
        public object? Call(Type returnType, string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9) => CallApply(returnType, identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 });

        public void CallVoid(string identifier) => CallApplyVoid(identifier);
        public void CallVoid(string identifier, object? arg0) => CallApplyVoid(identifier, new object?[] { arg0 });
        public void CallVoid(string identifier, object? arg0, object? arg1) => CallApplyVoid(identifier, new object?[] { arg0, arg1 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2, arg3 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 });
        public void CallVoid(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9) => CallApplyVoid(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 });

        public Task<T> CallAsync<T>(string identifier) => CallApplyAsync<T>(identifier);
        public Task<T> CallAsync<T>(string identifier, object? arg0) => CallApplyAsync<T>(identifier, new object?[] { arg0 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2, arg3 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 });
        public Task<T> CallAsync<T>(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9) => CallApplyAsync<T>(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 });

        public Task CallVoidAsync(string identifier) => CallApplyVoidAsync(identifier);
        public Task CallVoidAsync(string identifier, object? arg0) => CallApplyVoidAsync(identifier, new object?[] { arg0 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2, arg3 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 });
        public Task CallVoidAsync(string identifier, object? arg0, object? arg1, object? arg2, object? arg3, object? arg4, object? arg5, object? arg6, object? arg7, object? arg8, object? arg9) => CallApplyVoidAsync(identifier, new object?[] { arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9 });
    }
}
