using Microsoft.JSInterop;
using System.Reflection;

namespace SpawnDev.BlazorJS {
    public interface IJSObject { }
    public class IJSObjectProxy : DispatchProxy {
        public IJSInProcessObjectReference JSRef { get; private set; }
        public Type InterfaceType { get; private set; }

        static Dictionary<Type, MethodInfo?> GetInterfaceCache = new Dictionary<Type, MethodInfo?>();
        static MethodInfo? _GetInterfaceInfo = null;
        static MethodInfo? GetGetInterfaceGeneric(Type type) {
            if (GetInterfaceCache.TryGetValue(type, out var methodInfo)) return methodInfo;
            if (_GetInterfaceInfo == null) {
                var thenASyncParamTypes = new Type[] { typeof(IJSInProcessObjectReference) };
                var infos = typeof(IJSObjectProxy).GetMethods().Where(o => {
                    if (o.Name != "GetInterface") return false;
                    if (!o.IsGenericMethod) return false;
                    var paramInfos = o.GetParameters();
                    var paramTypes = paramInfos.Select(o => o.ParameterType).ToList();
                    if (!paramTypes.SequenceEqual(thenASyncParamTypes)) return false;
                    return true;
                }).ToList();
                _GetInterfaceInfo = infos.FirstOrDefault();
            }
            var methodInfoTyped = _GetInterfaceInfo.MakeGenericMethod(type);
            GetInterfaceCache[type] = methodInfoTyped;
            return methodInfoTyped;
        }

        public static object GetInterface(Type interfaceType, IJSInProcessObjectReference _ref) {
            var mi = GetGetInterfaceGeneric(interfaceType);
            var ret = mi.Invoke(null, new object[] { _ref });
            return ret;
        }

        public static T GetInterface<T>(IJSInProcessObjectReference _ref) where T : class, IJSObject {
            var typeofT = typeof(T);
            if (!typeofT.IsInterface) throw new Exception("GetInterface must be called with an interface");
            var proxy = Create<T, IJSObjectProxy>() as IJSObjectProxy;
            proxy.JSRef = _ref;
            proxy.InterfaceType = typeofT;
            return proxy as T;
        }

        string GetTargetMethodName(MethodInfo? targetMethod) {
            if (targetMethod == null) throw new ArgumentNullException(nameof(targetMethod));
            var name = targetMethod.Name;
            if (!string.IsNullOrEmpty(name)) {
                name = name.Substring(0, 1).ToLowerInvariant() + name.Substring(1);
            }
            // todo support JSName attribute
            return name;
        }

        string GetTargetPropertyName(MethodInfo? targetMethod) {
            if (targetMethod == null) throw new ArgumentNullException(nameof(targetMethod));
            var methodName = targetMethod.Name;
            var name = methodName.Substring(4);
            if (!string.IsNullOrEmpty(name)) {
                name = name.Substring(0, 1).ToLowerInvariant() + name.Substring(1);
            }
            // todo support JSName attribute
            return name;
        }

        protected override object? Invoke(MethodInfo? targetMethod, object?[]? args) {
            object? ret = null;
            if (JSRef == null) throw new Exception("IJSObject.Invoke exception: reference has been disposed.");
            if (targetMethod == null) return ret;
            var methodName = targetMethod.Name;
            var returnType = targetMethod.ReturnType;
            var argsCount = args == null ? 0 : args.Length;
            try {
                if (targetMethod.IsSpecialName) {
                    if (methodName.StartsWith("get_")) {
                        var propName = GetTargetPropertyName(targetMethod);
                        ret = JSRef.Get(returnType, propName);
                    }
                    else if (methodName.StartsWith("set_")) {
                        var propName = GetTargetPropertyName(targetMethod);
                        JSRef.Set(propName, args[0]);
                    }
                    else {
                        var nmt = true;
                    }
                }
                else {
                    var mName = GetTargetMethodName(targetMethod);
                    if (returnType == typeof(void)) {
                        JSRef.CallApplyVoid(mName, args);
                    }
                    else {
                        ret = JSRef.CallApply(returnType, mName, args);
                    }
                }
            }
            catch (Exception ex) {
                var ttt = true;
            }
            return ret;
        }

        ~IJSObjectProxy() {
            // IJSObjects dispose of their IJSInProcessObjectReference objects in the finalizer (here)
            JSRef?.Dispose();
            JSRef = null;
        }
    }
}
