using Microsoft.JSInterop;
using static SpawnDev.BlazorJS.JSObjects.Promise;

namespace SpawnDev.BlazorJS.JSObjects {
    public class Promise<TResult> : JSObject {
        public static explicit operator Promise<TResult>(Task<TResult> t) => new Promise<TResult>(t);
        public static explicit operator Promise<TResult>(Func<Task<TResult>> t) => new Promise<TResult>(t);

        public Promise(Func<Task<TResult>> task) : base(NullRef) {
            FromReference(JS.New("Promise", Callback.CreateOne((Function resolveFunc, Function rejectFunc) => {
                ResolveFunc = resolveFunc;
                RejectFunc = rejectFunc;
                task().ContinueWith(t => {
                    if (t.IsFaulted) {
                        Reject();
                    }
                    else if (t.IsCanceled) {
                        Reject();
                    }
                    else {
                        Resolve(t.Result);
                    }
                });
            })));
        }

        public Promise(Task<TResult> task) : base(NullRef) {
            FromReference(JS.New("Promise", Callback.CreateOne((Function resolveFunc, Function rejectFunc) => {
                ResolveFunc = resolveFunc;
                RejectFunc = rejectFunc;
                task.ContinueWith(t => {
                    if (t.IsFaulted) {
                        Reject();
                    }
                    else if (t.IsCanceled) {
                        Reject();
                    }
                    else {
                        Resolve(t.Result);
                    }
                });
            })));
        }

        public Promise() : base(NullRef) {
            FromReference(JS.New("Promise", Callback.CreateOne((Function resolveFunc, Function rejectFunc) => {
                ResolveFunc = resolveFunc;
                RejectFunc = rejectFunc;
            })));
        }

        public Promise(Action<Function, Function> executor) : base(JS.New("Promise", Callback.CreateOne(executor))) {

        }

        protected override void LosingReference()
        {
            if (IsWrapperDisposed) return;
            ResolveFunc?.Dispose();
            ResolveFunc = null;
            ResolveFunc?.Dispose();
            ResolveFunc = null;
        }

        public Function? ResolveFunc { get; protected set; }
        public Function? RejectFunc { get; protected set; }

        public void Then(ActionCallback thenCallback, ActionCallback catchCallback) => JSRef.CallVoid("then", thenCallback, catchCallback);
        public void Then(ActionCallback<TResult> thenCallback, ActionCallback catchCallback) => JSRef.CallVoid("then", thenCallback, catchCallback);

        public void Resolve(TResult result) => ResolveFunc.CallVoid(null, result);
        public void Reject(object reason) => RejectFunc.CallVoid(null, reason);
        public void Reject() => RejectFunc.CallVoid();

        public Promise(IJSInProcessObjectReference _ref) : base(_ref) { }

        public Task<TResult> ThenAsync(int timeoutMS = 0) {
            var t = new TaskCompletionSource<TResult>();
            var callbacks = new CallbackGroup();
            var cancellationTokenSource = timeoutMS > 0 ? new CancellationTokenSource() : null;
            Then(Callback.Create<TResult>((result) => {
                if (t.TrySetResult(result)) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            }, callbacks), Callback.Create(() => {
                if (t.TrySetException(new Exception("Unknown error"))) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            }, callbacks));
            cancellationTokenSource?.Token.Register(() => {
                if (t.TrySetException(new Exception("Timed out"))) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            });
            cancellationTokenSource?.CancelAfter(timeoutMS);
            return t.Task;
        }
        public Task<TResult> ThenAsync(CancellationToken cancellationToken) {
            var t = new TaskCompletionSource<TResult>();
            var callbacks = new CallbackGroup();
            Then(Callback.Create<TResult>((result) => {
                if (t.TrySetResult(result)) {
                    callbacks.Dispose();
                }
            }, callbacks), Callback.Create(() => {
                if (t.TrySetException(new Exception("Unknown error"))) {
                    callbacks.Dispose();
                }
            }, callbacks));
            if (cancellationToken != CancellationToken.None) {
                cancellationToken.Register(() => {
                    if (t.TrySetException(new Exception("Timed out"))) {
                        callbacks.Dispose();
                    }
                });
            }
            return t.Task;
        }
    }

    public class Promise : JSObject {
        public static explicit operator Promise(Task t) => new Promise(t);
        public static explicit operator Promise(Func<Task> t) => new Promise(t);

        public Promise(Func<Task> task) : base(NullRef) {
            FromReference(JS.New("Promise", Callback.CreateOne((Function resolveFunc, Function rejectFunc) => {
                ResolveFunc = resolveFunc;
                RejectFunc = rejectFunc;
                task().ContinueWith(t => {
                    if (t.IsFaulted) {
                        Reject();
                    }
                    else if (t.IsCanceled) {
                        Reject();
                    }
                    else {
                        Resolve();
                    }
                });
            })));
        }

        public Promise(Task task) : base(NullRef) {
            FromReference(JS.New("Promise", Callback.CreateOne((Function resolveFunc, Function rejectFunc) => {
                ResolveFunc = resolveFunc;
                RejectFunc = rejectFunc;
                task.ContinueWith(t => {
                    if (t.IsFaulted) {
                        Reject();
                    }
                    else if (t.IsCanceled) {
                        Reject();
                    }
                    else {
                        Resolve();
                    }
                });
            })));
        }

        public Promise() : base(NullRef) {
            FromReference(JS.New("Promise", Callback.CreateOne((Function resolveFunc, Function rejectFunc) => {
                ResolveFunc = resolveFunc;
                RejectFunc = rejectFunc;
            })));
        }

        public Promise(Action<Function, Function> executor) : base(JS.New("Promise", Callback.CreateOne(executor))) { }


        protected override void LosingReference()
        {
            if (IsWrapperDisposed) return;
            ResolveFunc?.Dispose();
            ResolveFunc = null;
            ResolveFunc?.Dispose();
            ResolveFunc = null;
            base.Dispose();
        }

        public Function? ResolveFunc { get; protected set; }
        public Function? RejectFunc { get; protected set; }

        public void ThenCatch<TError>(ActionCallback thenCallback, ActionCallback<TError> catchCallback) => JSRef.CallVoid("then", thenCallback, catchCallback);
        public void Then(ActionCallback thenCallback, ActionCallback catchCallback) => JSRef.CallVoid("then", thenCallback, catchCallback);
        public void Then<TResult>(ActionCallback<TResult> thenCallback, ActionCallback catchCallback) => JSRef.CallVoid("then", thenCallback, catchCallback);
        public void ThenCatch<TResult, TError>(ActionCallback<TResult> thenCallback, ActionCallback<TError> catchCallback) => JSRef.CallVoid("then", thenCallback, catchCallback);

        public void Resolve(object? value) => ResolveFunc.CallVoid(null, value);
        public void Resolve() => ResolveFunc.CallVoid();
        public void Reject() => RejectFunc.CallVoid();
        public void Reject(object? reason) => RejectFunc.CallVoid(null, reason);

        public Promise(IJSInProcessObjectReference _ref) : base(_ref) { }

        public Task ThenAsync(int timeoutMS = 0) {
            var t = new TaskCompletionSource();
            var callbacks = new CallbackGroup();
            var cancellationTokenSource = timeoutMS > 0 ? new CancellationTokenSource() : null;
            Then(Callback.Create(() => {
                if (t.TrySetResult()) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            }, callbacks), Callback.Create(() => {
                if (t.TrySetException(new Exception("Unknown error"))) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            }, callbacks));
            cancellationTokenSource?.Token.Register(() => {
                if (t.TrySetException(new Exception("Timed out"))) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            });
            cancellationTokenSource?.CancelAfter(timeoutMS);
            return t.Task;
        }
        public class PromiseCatchException<T> : Exception
        {
            public T CatchValue { get; set; } 
            public PromiseCatchException(T catchValue)
            {
                CatchValue = catchValue;
            }
        }

        public Task ThenCatchAsync<TCatch>(int timeoutMS = 0)
        {
            var t = new TaskCompletionSource();
            var callbacks = new CallbackGroup();
            var cancellationTokenSource = timeoutMS > 0 ? new CancellationTokenSource() : null;
            ThenCatch(Callback.Create(() => {
                if (t.TrySetResult())
                {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            }, callbacks), callbacks.Add(new ActionCallback<TCatch>((catchValue) => {
                if (t.TrySetException(new PromiseCatchException<TCatch>(catchValue)))
                {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            })));
            cancellationTokenSource?.Token.Register(() => {
                if (t.TrySetException(new Exception("Timed out")))
                {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            });
            cancellationTokenSource?.CancelAfter(timeoutMS);
            return t.Task;
        }
        public Task<TResult> ThenAsync<TResult>(int timeoutMS = 0) {
            var t = new TaskCompletionSource<TResult>();
            var callbacks = new CallbackGroup();
            var cancellationTokenSource = timeoutMS > 0 ? new CancellationTokenSource() : null;
            Then(Callback.Create<TResult>((result) => {
                if (t.TrySetResult(result)) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            }, callbacks), Callback.Create(() => {
                if (t.TrySetException(new Exception("Unknown error"))) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            }, callbacks));
            cancellationTokenSource?.Token.Register(() => {
                if (t.TrySetException(new Exception("Timed out"))) {
                    cancellationTokenSource?.Dispose();
                    callbacks.Dispose();
                }
            });
            cancellationTokenSource?.CancelAfter(timeoutMS);
            return t.Task;
        }
        public Task ThenAsync(CancellationToken cancellationToken)
        {
            var t = new TaskCompletionSource();
            var callbacks = new CallbackGroup();
            Then(Callback.Create(() => {
                if (t.TrySetResult())
                {
                    callbacks.Dispose();
                }
            }, callbacks), Callback.Create(() => {
                if (t.TrySetException(new Exception("Unknown error")))
                {
                    callbacks.Dispose();
                }
            }, callbacks));
            if (cancellationToken != CancellationToken.None)
            {
                cancellationToken.Register(() => {
                    if (t.TrySetException(new Exception("Timed out")))
                    {
                        callbacks.Dispose();
                    }
                });
            }
            return t.Task;
        }
        public Task<TResult> ThenAsync<TResult>(CancellationToken cancellationToken) {
            var t = new TaskCompletionSource<TResult>();
            var callbacks = new CallbackGroup();
            Then(Callback.Create<TResult>((result) => {
                if (t.TrySetResult(result)) {
                    callbacks.Dispose();
                }
            }, callbacks), Callback.Create(() => {
                if (t.TrySetException(new Exception("Unknown error"))) {
                    callbacks.Dispose();
                }
            }, callbacks));
            if (cancellationToken != CancellationToken.None) {
                cancellationToken.Register(() => {
                    if (t.TrySetException(new Exception("Timed out"))) {
                        callbacks.Dispose();
                    }
                });
            }
            return t.Task;
        }
    }
}
