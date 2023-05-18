using Microsoft.JSInterop;
using System.Reflection;

namespace SpawnDev.BlazorJS.JsonConverters {
    /// <summary>
    /// This class is used to determine if the JSCallResultType that the JSRuntime call will use needs to be overridden
    /// If the value needs to be overridden, the new valueis returned with 128 added to the new JSCallResultType
    /// </summary>
    internal class JSCallResultTypeHelperOverride {
        const int OverrideFlag = 128;
        private static MethodInfo? FromGenericMethodInfo = null;
        static Dictionary<Type, MethodInfo> FromGenericTypedCache = new Dictionary<Type, MethodInfo>();
        static MethodInfo GetFromGenericTyped(Type type) {
            MethodInfo result;
            if (FromGenericTypedCache.TryGetValue(type, out result)) return result;
            if (FromGenericMethodInfo == null) {
                FromGenericMethodInfo = typeof(JSCallResultType).Assembly.GetType("Microsoft.JSInterop.JSCallResultTypeHelper").GetMethod("FromGeneric", BindingFlags.Public | BindingFlags.Static);
            }
            result = FromGenericMethodInfo.MakeGenericMethod(type);
            FromGenericTypedCache[type] = result;
            return result;
        }

        /// <summary>
        /// By default all callback args are passed as JSCallResultType.Default
        /// The return value from this method tells the JSInterop.serializeToDotNet method that we want to pre-serialize to the format returned before the default serialization (json)
        /// </summary>
        /// <param name="returnType"></param>
        /// <returns></returns>
        public static JSCallResultType FromGenericForCallback(Type returnType)
        {
            var ret = JSCallResultType.Default;
            var jsonConverter = BlazorJSRuntime.RuntimeJsonSerializerOptions.GetConverter(returnType);
            if (jsonConverter is IJSInProcessObjectReferenceConverter)
            {
                ret = JSCallResultType.JSObjectReference;
            }
            else
            {
                ret = FromGenericOrig(returnType);
            }
            return ret == JSCallResultType.Default ? ret : ret + OverrideFlag;
        }

        public static JSCallResultType FromGenericForCallback<TResult>()
        {
            var returnType = typeof(TResult);
            var ret = JSCallResultType.Default;
            var jsonConverter = BlazorJSRuntime.RuntimeJsonSerializerOptions.GetConverter(returnType);
            if (jsonConverter is IJSInProcessObjectReferenceConverter)
            {
                ret = JSCallResultType.JSObjectReference;
            }
            else
            {
                ret = FromGenericOrig(returnType);
            }
            return ret == JSCallResultType.Default ? ret : ret + OverrideFlag;
        }


        /// <summary>
        /// FromGeneric returns the value from the original FromGeneric method unless the default is json and IJSInprocessObjectReference is desired instead
        /// </summary>
        /// <param name="returnType"></param>
        /// <returns></returns>
        public static JSCallResultType FromGeneric(Type returnType) {
            var jsonConverter = BlazorJSRuntime.RuntimeJsonSerializerOptions.GetConverter(returnType);
            var resultTypeOrig = FromGenericOrig(returnType);
            if (resultTypeOrig == JSCallResultType.Default)
            {
                if (jsonConverter is IJSInProcessObjectReferenceConverter)
                {
                    resultTypeOrig = JSCallResultType.JSObjectReference + OverrideFlag;
                }
            }
            return resultTypeOrig;
        }

        public static JSCallResultType FromGeneric<TResult>() {
            var returnType = typeof(TResult);
            var jsonConverter = BlazorJSRuntime.RuntimeJsonSerializerOptions.GetConverter(returnType);
            var resultTypeOrig = FromGenericOrig(returnType);
            if (resultTypeOrig == JSCallResultType.Default)
            {
                if (jsonConverter is IJSInProcessObjectReferenceConverter)
                {
                    resultTypeOrig = JSCallResultType.JSObjectReference + OverrideFlag;
                }
            }
            return resultTypeOrig;
        }

        public static JSCallResultType FromGenericOrig(Type resultType) {
            var fromGenericTyped = GetFromGenericTyped(resultType);
            return (JSCallResultType)fromGenericTyped.Invoke(null, null);
        }

        public static JSCallResultType FromGenericOrig<TResult>() {
            var fromGenericTyped = GetFromGenericTyped(typeof(TResult));
            return (JSCallResultType)fromGenericTyped.Invoke(null, null);
        }
    }
}
