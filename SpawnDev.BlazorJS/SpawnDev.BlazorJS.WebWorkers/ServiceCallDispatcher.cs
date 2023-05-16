using Microsoft.Extensions.DependencyInjection;
using Microsoft.JSInterop;
using SpawnDev.BlazorJS.JSObjects;
using System.Reflection;
using System.Runtime.CompilerServices;

namespace SpawnDev.BlazorJS.WebWorkers {
    internal class WebWorkerCallMessageTask {
        public Type? ReturnValueType { get; set; }
        public string RequestId => webWorkerCallMessageOutgoing.RequestId;
        public Action<IJSInProcessObjectReference>? OnComplete { get; set; }
        public CancellationToken? CancellationToken { get; set; }
        public WebWorkerMessageOut webWorkerCallMessageOutgoing { get; set; }

        public WebWorkerCallMessageTask(Action<IJSInProcessObjectReference>? onComplete) {
            OnComplete = onComplete;
        }
    }
    public class ServiceCallDispatcher : IDisposable {
        public static List<Type> _transferableTypes { get; } = new List<Type> {
            typeof(ArrayBuffer),
            typeof(MessagePort),
        };
        public static bool IsTransferable(Type type) => _transferableTypes.Contains(type);
        public static bool IsTransferable<T>() => _transferableTypes.Contains(typeof(T));
        public static bool IsTransferable<T>(T obj) => _transferableTypes.Contains(typeof(T));
        public IMessagePort _port { get; set; }

        public bool WaitingForResponse => _waiting.Count > 0;

        Dictionary<string, WebWorkerCallMessageTask> _waiting = new Dictionary<string, WebWorkerCallMessageTask>();
        protected IServiceProvider _serviceProvider;

        public ServiceCallDispatcher(IServiceProvider serviceProvider, IMessagePort port) {
            _serviceProvider = serviceProvider;
            _port = port;
            _port.OnMessage += _worker_OnMessage;
            _port.OnMessageError += _port_OnError;
            additionalCallArgs.Add(new CallSideParamter("caller", () => this, typeof(ServiceCallDispatcher)));
        }
        protected static BlazorJSRuntime JS = BlazorJSRuntime.JS;
        private void _port_OnError() {
            JS.Log("_port_OnError");
        }

        public bool RecevdInit { get; private set; }
        private TaskCompletionSource<int> _oninit = new TaskCompletionSource<int>();
        public Task WhenReady => _oninit.Task;
        public void SendReadyFlag() {
            _port.PostMessage(new WebWorkerMessageBase { TargetType = "__init" });
        }

        public event Action<ServiceCallDispatcher, WebWorkerMessageIn> OnMessage;

        static Dictionary<string, Type?> typeCache = new Dictionary<string, Type>();

        /// For whatever reason Type.GetType was failing
        // when trying to find a Type in the same assembly as this class... no idea why.
        // Below code worked when it failed
        public static Type? GetType(string typeName) {
            Type? t = null;
            lock (typeCache) {
                if (!typeCache.TryGetValue(typeName, out t)) {
                    foreach (Assembly a in AppDomain.CurrentDomain.GetAssemblies()) {
                        t = a.GetType(typeName);
                        if (t != null) break;
                    }
                    typeCache[typeName] = t;
                }
            }
            return t;
        }

        public delegate void BusyStateChangedDelegate(ServiceCallDispatcher sender, bool busy);
        public event BusyStateChangedDelegate OnBusyStateChanged;
        private bool _busy = false;

        void CheckBusyStateChanged() {
            if (_busy && _waiting.Count == 0) {
                _busy = false;
                OnBusyStateChanged?.Invoke(this, _busy);
            }
            else if (!_busy && _waiting.Count > 0) {
                _busy = true;
                OnBusyStateChanged?.Invoke(this, _busy);
            }
        }

        protected async void _worker_OnMessage(MessageEvent e) {
            IJSInProcessObjectReference? args = null;
            try {
                var msgBase = e.GetData<WebWorkerMessageIn>();
                msgBase._msg = e;
                if (msgBase.TargetType == "__") {
                    Console.WriteLine("ping --__--");
                }
                else if (msgBase.TargetType == "__init") {
                    if (!RecevdInit) {
                        RecevdInit = true;
                        _oninit.TrySetResult(0);
                        OnBusyStateChanged?.Invoke(this, _busy);
                    }
                }
                else if (msgBase.TargetType == "__action" && !string.IsNullOrEmpty(msgBase.RequestId)) {
                    if (_waiting.TryGetValue(msgBase.RequestId, out var req)) {
                        var actionId = msgBase.TargetName;
                        if (_actionHandles.TryGetValue(actionId, out var actionHandle)) {
                            var actionArgs = new object[actionHandle.ParameterTypes.Length];
                            if (actionArgs.Length > 0) {
                                // Does not handle multiple args at the moment due to the added complexity with refelction
                                args = msgBase.GetData<IJSInProcessObjectReference>();
                                // JS.Log("msgBasemsgBasemsgBase", msgBase);
                                var argsLength = args != null ? args.Get<int>("length") : 0;
                                if (actionArgs.Length != argsLength) {
                                    Console.WriteLine("Action paramter count does not match incoking");
                                }
                                for (var n = 0; n < actionArgs.Length; n++) {
                                    actionArgs[n] = args.Get(actionHandle.ParameterTypes[n], n);
                                }
                            }
                            actionHandle.Target.DynamicInvoke(actionArgs);
                        }
                    }
                }
                else if (msgBase.TargetType == "__callback" && !string.IsNullOrEmpty(msgBase.RequestId)) {
                    if (_waiting.TryGetValue(msgBase.RequestId, out var req)) {
                        _waiting.Remove(msgBase.RequestId);
                        try {
                            args = msgBase.GetData<IJSInProcessObjectReference>();
                        }
                        catch { }
                        if (args != null) {
                            req.OnComplete?.Invoke(args);
                        }
                        else {
                            var checkThis = true;
                        }
                        CheckBusyStateChanged();
                    }
                }
                else if (msgBase.TargetType == "event" && !string.IsNullOrEmpty(msgBase.TargetName)) {
                    OnMessage?.Invoke(this, msgBase);
                }
                else if (!string.IsNullOrEmpty(msgBase.TargetType) && !string.IsNullOrEmpty(msgBase.TargetName)) {
                    //Console.WriteLine($"msgBase.MethodName: {msgBase.ServiceTypeFullName}::{msgBase.MethodName}");
                    try {
                        args = msgBase.GetData<IJSInProcessObjectReference>();
                    }
                    catch { }
                    var argsLength = args != null ? args.Get<int>("length") : 0;
                    var serviceType = GetType(msgBase.TargetType);
                    if (serviceType == null) {
                        throw new Exception($"ERROR: {nameof(ServiceCallDispatcher)} OnMessage - Service type not found {msgBase.TargetType}");
                    }
                    object? service = null;
                    try {
                        service = _serviceProvider.GetRequiredService(serviceType);
                    }
                    catch {
                        throw new Exception($"ERROR: {nameof(ServiceCallDispatcher)} OnMessage - Service not registered {serviceType.FullName}");
                    }
                    var methodInfo = GetBestInstanceMethod(serviceType, msgBase.TargetName, argsLength);
                    var returnType = methodInfo.ReturnType;
                    var isAwaitable = returnType.IsAsync();
                    var finalReturnType = isAwaitable ? returnType.AsyncReturnType() : returnType;
                    var finalReturnTypeIsVoid = finalReturnType == typeof(void);
                    var finalReturnTypeAllowTransfer = true;
                    var transferAttrMethod = finalReturnTypeIsVoid ? null : (WorkerTransferAttribute?)finalReturnType.GetCustomAttribute(typeof(WorkerTransferAttribute), false);
                    if (transferAttrMethod != null) {
                        finalReturnTypeAllowTransfer = transferAttrMethod.Transfer;
                    }
                    var callArgs0 = PostDeserializeArgs(msgBase.RequestId, methodInfo, argsLength, args.Get);
                    // call the requested method
                    // TODO ... should the called args dispose of the incoming parameters?
                    // dispose any parameters that implement IDisposable and were created for the call
                    var hasReturnValue = !finalReturnTypeIsVoid;
                    object? retValue = null;
                    string? err = null;
                    try {
                        var retv = methodInfo.Invoke(service, callArgs0);
                        if (retv is Task t) {
                            await t;
                            if (t.Exception != null) {
                                err = t.Exception.Message;
                            }
                            else if (!finalReturnTypeIsVoid) {
                                retValue = returnType.GetProperty("Result").GetValue(retv, null);
                            }
                        }
                        else if (retv is ValueTask vt) {
                            await vt;
                            if (vt.IsFaulted) {
                                err = "Call failed";
                            }
                            else if (!finalReturnTypeIsVoid) {
                                retValue = returnType.GetProperty("Result").GetValue(retv, null);
                            }
                        }
                        else if (!finalReturnTypeIsVoid) {
                            retValue = retv;
                        }
                    }
                    catch (Exception ex) {
                        err = ex.Message;
                    }

                    var autoDisposeArgs = true;
                    if (autoDisposeArgs) {
                        foreach (var ca in callArgs0) {
                            if (ca is IDisposable disposable) {
                                disposable.Dispose();
                            }
                        }
                    }
                    if (!string.IsNullOrEmpty(msgBase.RequestId)) {
                        // send notification of completeion is there is a requestid
                        var callbackMsg = new WebWorkerMessageOut { TargetType = "__callback", RequestId = msgBase.RequestId };
                        object[] transfer = new object?[0];
                        if (hasReturnValue && retValue != null) {
                            if (finalReturnTypeAllowTransfer) {
                                var conversionInfo = TypeConversionInfo.GetTypeConversionInfo(finalReturnType);
                                transfer = conversionInfo.GetTransferablePropertyValues(retValue);
                            }
                            //if (transfer.Length > 0) Console.WriteLine($"transfering: {transfer.Length} {transfer[0].GetType().Name}");
                        }
                        callbackMsg.Data = new object?[] { err, retValue };
                        _port.PostMessage(callbackMsg, transfer);
                        // dispose the return value if it is IJSInProcessObjectReference or JSObject
                        if (retValue is IDisposable disposable) {
                            disposable.Dispose();
                        }
                    }
                }
            }
            catch (Exception ex) {
                BlazorJSRuntime.JS.Log("ERROR: ", e);
                Console.WriteLine($"ERROR: {ex.Message}");
                Console.WriteLine($"ERROR stacktrace: {ex.StackTrace}");
            }
            finally {
                args?.Dispose();
                e?.Dispose();
            }
        }

        public void SendEvent(string eventName, object? data = null) {
            _port.PostMessage(new WebWorkerMessageOut { TargetType = "event", TargetName = eventName, Data = data });
        }

        //
        public Task<object?> InvokeAsync(Type serviceType, string methodName, params object?[]? args) => CallAsync(serviceType, methodName, args ?? new object[0]);
        public Task<object?> CallAsync(Type serviceType, string methodName, object?[]? args = null) {
            var argsLength = args != null ? args.Length : 0;
            var methodInfo = GetBestInstanceMethod(serviceType, methodName, argsLength);
            var returnType = methodInfo.ReturnType;
            var isAwaitable = returnType.IsAsync();
            var finalReturnType = isAwaitable ? returnType.AsyncReturnType() : returnType;
            var finalReturnTypeIsVoid = finalReturnType == typeof(void);
            var requestId = Guid.NewGuid().ToString();
            var msgData = PreSerializeArgs(requestId, methodInfo, args, out var transferable);
            var workerMsg = new WebWorkerMessageOut {
                RequestId = requestId,
                TargetName = methodName,
                TargetType = serviceType.FullName,
                Data = msgData,
            };
            var t = new TaskCompletionSource<object?>();
            var workerTask = new WebWorkerCallMessageTask((args) => {
                // remove any callbacks
                var keysToRemove = _actionHandles.Values.Where(o => o.RequestId == requestId).Select(o => o.Id).ToArray();
                foreach (var key in keysToRemove) _actionHandles.Remove(key);
                // get result or exception and fire results handlers
                var argsLength = args != null ? args.Get<int>("length") : 0;
                object? retVal = null;
                string? err = null;
                if (argsLength > 0) {
                    err = args.Get<string?>(0);
                    if (string.IsNullOrEmpty(err)) retVal = (object?)args.Get(finalReturnType, 1);
                }
                if (!string.IsNullOrEmpty(err))
                    t.TrySetException(new Exception(err));
                else
                    t.TrySetResult(retVal);
            });
            workerTask.ReturnValueType = finalReturnType;
            workerTask.webWorkerCallMessageOutgoing = workerMsg;
            _waiting.Add(workerTask.RequestId, workerTask);
            _port.PostMessage(workerMsg, transferable);
            CheckBusyStateChanged();
            return t.Task;
        }
        public Task<TResult> InvokeAsync<TService, TResult>(string methodName, params object?[]? args) => CallAsync<TService, TResult>(methodName, args ?? new object[0]);
        public Task<TResult> CallAsync<TService, TResult>(string methodName, object?[]? args = null) {
            var argsLength = args != null ? args.Length : 0;
            var serviceType = typeof(TService);
            var methodInfo = GetBestInstanceMethod(serviceType, methodName, argsLength);
            var returnType = methodInfo.ReturnType;
            var isAwaitable = returnType.IsAsync();
            var finalReturnType = isAwaitable ? returnType.AsyncReturnType() : returnType;
            var finalReturnTypeIsVoid = finalReturnType == typeof(void);
            var requestId = Guid.NewGuid().ToString();
            var msgData = PreSerializeArgs(requestId, methodInfo, args, out var transferable);
            var workerMsg = new WebWorkerMessageOut {
                RequestId = requestId,
                TargetName = methodName,
                TargetType = typeof(TService).FullName,
                Data = msgData,
            };
            var t = new TaskCompletionSource<TResult>();
            if (typeof(TResult) != finalReturnType) {
#if DEBUG
                Console.WriteLine($"WARNING: Worker service {serviceType.Name} method {methodName} has a different return type {finalReturnType.Name} then the callee requested type {finalReturnType.Name}");
#endif
            }
            var workerTask = new WebWorkerCallMessageTask((args) => {
                // remove any callbacks
                var keysToRemove = _actionHandles.Values.Where(o => o.RequestId == requestId).Select(o => o.Id).ToArray();
                foreach (var key in keysToRemove) _actionHandles.Remove(key);
                //var finalReturnTypeName = finalReturnType.Name;
                var argsLength = args == null ? 0 : args.Get<int>("length");
                if (args == null || argsLength != 2) {
                    Console.WriteLine($"ServiceCallDispatcher Invalid return args for workerTask");
                    t.TrySetException(new Exception("Invalid return args for workerTask"));
                    return;
                }
                var err = args.Get<string?>(0);
                if (!string.IsNullOrEmpty(err)) {
                    t.TrySetException(new Exception(err));
                    return;
                }
                try {
                    var ret = args.Get<TResult>(1);
                    t.TrySetResult(ret);
                    return;
                }
                catch {
#if DEBUG && false
                            Console.WriteLine($"ServiceCallDispatcher 'retVal = args.Get<TResult?>(1);'");
#endif
                    t.TrySetResult(default);
                }
            });
            workerTask.ReturnValueType = finalReturnType;
            workerTask.webWorkerCallMessageOutgoing = workerMsg;
            _waiting.Add(workerTask.RequestId, workerTask);
            _port.PostMessage(workerMsg, transferable);
            CheckBusyStateChanged();
            return t.Task;
        }

        public void CallAsync<TService>(string methodName, object?[]? args, Action<Exception?, object?> callback) where TService : class => CallAsync(typeof(TService), methodName, args, callback);
        public void CallAsync(Type serviceType, string methodName, object?[]? args, Action<Exception?, object?> callback) {
            var argsLength = args != null ? args.Length : 0;
            var methodInfo = GetBestInstanceMethod(serviceType, methodName, argsLength);
            var returnType = methodInfo.ReturnType;
            var isAwaitable = returnType.IsAsync();
            var finalReturnType = isAwaitable ? returnType.AsyncReturnType() : returnType;
            var finalReturnTypeIsVoid = finalReturnType == typeof(void);
            var requestId = Guid.NewGuid().ToString();
            var msgData = PreSerializeArgs(requestId, methodInfo, args, out var transferable);
            var workerMsg = new WebWorkerMessageOut {
                RequestId = requestId,
                TargetName = methodName,
                TargetType = serviceType.FullName,
                Data = msgData,
            };
            var workerTask = new WebWorkerCallMessageTask((args) => {
                Exception? retExc = null;
                object? retVal = null;
                // remove any callbacks
                var keysToRemove = _actionHandles.Values.Where(o => o.RequestId == requestId).Select(o => o.Id).ToArray();
                foreach (var key in keysToRemove) _actionHandles.Remove(key);
                //var finalReturnTypeName = finalReturnType.Name;
                var argsLength = args == null ? 0 : args.Get<int>("length");
                if (args == null || argsLength != 2) {
                    Console.WriteLine($"ServiceCallDispatcher Invalid return args for workerTask");
                    retExc = new Exception("Invalid return args for workerTask");
                }
                else {
                    var err = args.Get<string?>(0);
                    if (!string.IsNullOrEmpty(err)) {
                        retExc = new Exception(err);
                    }
                    else if (!finalReturnTypeIsVoid) {
                        try {
                            retVal = args.Get(finalReturnType, 1);
                        }
                        catch (Exception ex) {
                            retExc = ex;
                        }
                    }
                }
                callback?.Invoke(retExc, retVal);
            });
            workerTask.ReturnValueType = finalReturnType;
            workerTask.webWorkerCallMessageOutgoing = workerMsg;
            _waiting.Add(workerTask.RequestId, workerTask);
            _port.PostMessage(workerMsg, transferable);
            CheckBusyStateChanged();
        }


        public static object GetDefault(Type type) {
            if (type.IsValueType) {
                return Activator.CreateInstance(type);
            }
            return null;
        }

        Dictionary<string, CallbackAction> _actionHandles = new Dictionary<string, CallbackAction>();

        class CallbackAction {
            public Delegate Target { get; set; }
            public MethodInfo Method { get; set; }
            public string Id { get; set; } = Guid.NewGuid().ToString();
            public string RequestId { get; set; } = "";
            public Type[] ParameterTypes { get; set; }
        }

        object?[]? PreSerializeArgs(string requestId, MethodInfo methodInfo, object?[]? args, out object[] transferable) {
            var trasnferableList = new List<object>();
            var methodsParamTypes = methodInfo.GetParameters();
            object?[]? ret = null;
            if (args != null) {
                ret = new object?[args.Length];
                var argI = 0;
                for (var i = 0; i < methodsParamTypes.Length; i++) {
                    var methodParam = methodsParamTypes[i];
                    var methodParamType = methodParam.ParameterType;
                    var methodParamTypeIsTransferable = IsTransferable(methodParamType);
                    var allowTransferable = methodParamType.IsClass;
                    if (allowTransferable) {
                        var transferAttr = (WorkerTransferAttribute?)methodParam.GetCustomAttribute(typeof(WorkerTransferAttribute), false);
                        if (transferAttr != null && !transferAttr.Transfer) {
                            // this property has been marked as non-stransferable
                            allowTransferable = false;
                        }
                    }
                    var methodParamTypeName = methodParam.ParameterType.Name;
                    var genericTypes = methodParamType.GenericTypeArguments;
                    var genericTypeNames = methodParamType.GenericTypeArguments.Select(o => o.Name).ToArray();
                    var callSideArg = GetCallSideParameter(methodParam);
                    if (callSideArg != null) {
                        // skip... it will be handles on the other side
                    }
                    else if (typeof(Delegate).IsAssignableFrom(methodParamType)) {
                        var argVal = (Delegate)args[argI];
                        argI++;
                        var cb = new CallbackAction {
                            Method = methodInfo,
                            RequestId = requestId,
                            ParameterTypes = genericTypes,
                            Target = argVal,
                        };
                        _actionHandles[cb.Id] = cb;
                        ret[i] = cb.Id;
                    }
                    else if (argI < args.Length) {
                        var argVal = args[argI];
                        if (allowTransferable) {
                            var covnersionInfo = TypeConversionInfo.GetTypeConversionInfo(methodParamType);
                            var propTransferable = covnersionInfo.GetTransferablePropertyValues(argVal);
                            trasnferableList.AddRange(propTransferable);
                        }
                        ret[i] = argVal;
                        argI++;
                    }
                    else {
                        // should not get here
                    }
                }
            }
            transferable = trasnferableList.ToArray();
            return ret;
        }

        public static Action<T0> CreateTypedActionT1<T0>(Action<object?[]> arg) => new Action<T0>((t0) => arg(new object[] { t0 }));
        public static Action<T0, T1> CreateTypedActionT2<T0, T1>(Action<object?[]> arg) => new Action<T0, T1>((t0, t1) => arg(new object[] { t0, t1 }));
        public static Action<T0, T1, T2> CreateTypedActionT3<T0, T1, T2>(Action<object?[]> arg) => new Action<T0, T1, T2>((t0, t1, t2) => arg(new object[] { t0, t1 }));
        public static Action<T0, T1, T2, T3> CreateTypedActionT4<T0, T1, T2, T3>(Action<object?[]> arg) => new Action<T0, T1, T2, T3>((t0, t1, t2, t3) => arg(new object[] { t0, t1, t2, t3 }));
        public static Action<T0, T1, T2, T3, T4> CreateTypedActionT4<T0, T1, T2, T3, T4>(Action<object?[]> arg) => new Action<T0, T1, T2, T3, T4>((t0, t1, t2, t3, t4) => arg(new object[] { t0, t1, t2, t3, t4 }));

        public object CreateTypedAction(Type[] typ1, Action<object?[]> arg) {
            var meth = typeof(ServiceCallDispatcher).GetMethod($"CreateTypedActionT{typ1.Length}", BindingFlags.Public | BindingFlags.Static);
            var gmeth = meth.MakeGenericMethod(typ1);
            var genericAction = gmeth.Invoke(null, new object[] { arg });
            return genericAction;
        }

        object?[]? PostDeserializeArgs(string requestId, MethodInfo methodInfo, int argsLength, Func<Type, int, object?> getArg) {
            var methodsParamTypes = methodInfo.GetParameters();
            var ret = new object?[argsLength];
            var argI = 0;
            for (var i = 0; i < methodsParamTypes.Length; i++) {
                var methodParam = methodsParamTypes[i];
                var methodParamType = methodParam.ParameterType;
                var methodParamTypeName = methodParam.ParameterType.Name;
                var genericTypes = methodParamType.GenericTypeArguments;
                var genericTypeNames = methodParamType.GenericTypeArguments.Select(o => o.Name).ToArray();
                var callSideArg = GetCallSideParameter(methodParam);
                if (callSideArg != null) {

                    ret[i] = callSideArg.GetValue();
                    continue;
                }
                else if (typeof(Delegate).IsAssignableFrom(methodParamType)) {
                    var actionId = (string)getArg(typeof(string), argI);
                    argI++;
                    var hasParams = methodParam.ParameterType.IsGenericType;
                    if (genericTypes.Length == 0) {
                        ret[i] = new Action(() => {
                            //JS.Log($"Action called: {actionId}");
                            var callbackMsg = new WebWorkerMessageOut { TargetType = "__action", RequestId = requestId, TargetName = actionId };
                            _port.PostMessage(callbackMsg);
                        });
                    }
                    else {
                        ret[i] = CreateTypedAction(genericTypes, new Action<object?[]>((args) => {
                            //JS.Log($"Action called: {actionId} {o}");
                            var callbackMsg = new WebWorkerMessageOut { TargetType = "__action", RequestId = requestId, TargetName = actionId };
                            callbackMsg.Data = args;
                            _port.PostMessage(callbackMsg);
                        }));
                    }
                }
                else if (argI < argsLength) {
                    ret[i] = getArg(methodParamType, argI);
                    argI++;
                }
            }
            return ret;
        }
        public Task InvokeVoidAsync<TService>(string methodName, params object?[]? args) => CallVoidAsync<TService>(methodName, args ?? new object[0]);
        public Task CallVoidAsync<TService>(string methodName, object?[]? args = null) {
            var argsLength = args != null ? args.Length : 0;
            var serviceType = typeof(TService);
            var methodInfo = GetBestInstanceMethod(serviceType, methodName, argsLength);
            var returnType = methodInfo.ReturnType;
            var isAwaitable = returnType.IsAsync();
            var finalReturnType = isAwaitable ? returnType.AsyncReturnType() : returnType;
            var finalReturnTypeIsVoid = finalReturnType == typeof(void);
            var requestId = Guid.NewGuid().ToString();
            var msgData = PreSerializeArgs(requestId, methodInfo, args, out var transferable);
            var workerMsg = new WebWorkerMessageOut {
                RequestId = requestId,
                TargetName = methodName,
                TargetType = typeof(TService).FullName,
                Data = msgData,
            };
            var t = new TaskCompletionSource<int>();
            if (typeof(void) != finalReturnType) {
#if DEBUG
                Console.WriteLine($"WARNING: Worker service {serviceType.Name} method {methodName} has a different return type {typeof(void).Name} then the callee requested type {finalReturnType.Name}");
#endif
            }
            var workerTask = new WebWorkerCallMessageTask((args) => {
                // remove any callbacks
                var keysToRemove = _actionHandles.Values.Where(o => o.RequestId == requestId).Select(o => o.Id).ToArray();
                foreach (var key in keysToRemove) _actionHandles.Remove(key);
                //
                var argsLength = args != null ? args.Get<int>("length") : 0;
                string? err = null;
                if (argsLength > 0) {
                    err = args.Get<string?>(0);
                }
                if (!string.IsNullOrEmpty(err))
                    t.TrySetException(new Exception(err));
                else
                    t.TrySetResult(0);
            });
            workerTask.ReturnValueType = finalReturnType;
            workerTask.webWorkerCallMessageOutgoing = workerMsg;
            _waiting.Add(workerTask.RequestId, workerTask);
            _port.PostMessage(workerMsg, transferable);
            CheckBusyStateChanged();
            return t.Task;
        }

        List<CallSideParamter> additionalCallArgs { get; } = new List<CallSideParamter>();

        class CallSideParamter {
            public string Name { get; }
            public Type Type { get; }
            public Func<object?> GetValue;
            public CallSideParamter(string name, Func<object?> getter, Type type) {
                Name = name;
                GetValue = getter;
                Type = type;
            }
        }

        CallSideParamter? GetCallSideParameter(ParameterInfo p) {
            return additionalCallArgs.Where(o => o.Name == p.Name && o.Type == p.ParameterType).FirstOrDefault();
        }

        private static Type AsyncStateMachineAttributeType = typeof(AsyncStateMachineAttribute);
        private static bool IsAsyncMethod(MethodInfo method) => method.GetCustomAttribute(AsyncStateMachineAttributeType) != null;
        private MethodInfo? GetBestInstanceMethod<T>(string identifier, int paramCount) => GetBestInstanceMethod(typeof(T), identifier, paramCount);
        private MethodInfo? GetBestInstanceMethod(Type classType, string identifier, int paramCount) {
            // TODO - cache
            MethodInfo? best = null;
            var instanceMethods = classType
            .GetMethods(BindingFlags.Public | BindingFlags.Instance)
            .Where(m => m.Name == identifier)
            .Where(m => !m.IsGenericMethod)
            // don't count parameters that will be added at call time
            .Where(m => m.GetParameters().Where(o => GetCallSideParameter(o) == null).Count() == paramCount)
            .ToList();
            if (instanceMethods.Count > 0) {
                best = instanceMethods[0];
            }
            return best;
        }

        public bool IsDisposed { get; private set; } = false;
        public virtual void Dispose(bool disposing) {
            if (IsDisposed) return;
            IsDisposed = true;
            if (disposing) {

            }
        }
        public virtual void Dispose() {
            Dispose(true);
        }
        ~ServiceCallDispatcher() {
            Dispose(false);
            GC.SuppressFinalize(this);
        }

        Dictionary<Type, object> ServiceInterfaces = new Dictionary<Type, object>();

        public TServiceInterface GetService<TServiceInterface>() where TServiceInterface : class {
            var typeofT = typeof(TServiceInterface);
            if (ServiceInterfaces.TryGetValue(typeofT, out var serviceWorker)) return (TServiceInterface)serviceWorker;
            var ret = WebWorkerServiceProxy<TServiceInterface>.GetWorkerService(this);
            ServiceInterfaces[typeofT] = ret;
            return ret;
        }
    }
}
