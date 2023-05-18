using Microsoft.JSInterop;
using SpawnDev.BlazorJS.JSObjects;
using System.Reflection;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace SpawnDev.BlazorJS.JsonConverters {
    public class ActionConverterFactory : JsonConverterFactory {
        static Dictionary<Type, Type> SupportedGenericTypes = new Dictionary<Type, Type> {
            { typeof(Action), typeof(ActionConverter) },
            { typeof(Action<>), typeof(ActionConverter<>) },
            { typeof(Action<,>), typeof(ActionConverter<,>) },
            { typeof(Action<,,>), typeof(ActionConverter<,,>) },
            { typeof(Action<,,,>), typeof(ActionConverter<,,,>) },
        };
        public override bool CanConvert(Type type) {
            var baseType = type.IsGenericType ? type.GetGenericTypeDefinition() : type;
            if (SupportedGenericTypes.ContainsKey(baseType))
            {
                return true;
            }
            return false;
        }

        public override JsonConverter? CreateConverter(Type type, JsonSerializerOptions options) {
            var baseType = type.IsGenericType ? type.GetGenericTypeDefinition() : type;
            var genericTypes = type.GetGenericArguments();
            if (SupportedGenericTypes.TryGetValue(baseType, out var converterBaseType)) {
                var converterType = type.IsGenericType ? converterBaseType.MakeGenericType(genericTypes) : converterBaseType;
                JsonConverter converter = (JsonConverter)Activator.CreateInstance(converterType, BindingFlags.Instance | BindingFlags.Public, binder: null, args: new object[] { }, culture: null)!;
                return converter;
            }
            return null;
        }
    }
    public class ActionConverter : JsonConverter<Action>, IJSInProcessObjectReferenceConverter {
        public override bool CanConvert(Type type) {
            return type.GetGenericTypeDefinition() == typeof(Action);
        }
        public override Action Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            var _ref = JsonSerializer.Deserialize<IJSInProcessObjectReference>(ref reader, options);
            var fn = new Function(_ref);
            var ret = fn.ToAction();
            return ret;
        }
        public override void Write(Utf8JsonWriter writer, Action value, JsonSerializerOptions options) {
            var ret = value.CallbackGet(true);
            JsonSerializer.Serialize(writer, ret, options);
        }
    }
    public class ActionConverter<T0> : JsonConverter<Action<T0>>, IJSInProcessObjectReferenceConverter {

        public override bool CanConvert(Type type) {
            return type.GetGenericTypeDefinition() == typeof(Action<>);
        }
        public override Action<T0> Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            var _ref = JsonSerializer.Deserialize<IJSInProcessObjectReference>(ref reader, options);
            var fn = new Function(_ref);
            var ret = fn.ToAction<T0>();
            return ret;
        }
        public override void Write(Utf8JsonWriter writer, Action<T0> value, JsonSerializerOptions options) {
            var ret = value.CallbackGet(true);
            JsonSerializer.Serialize(writer, ret, options);
        }
    }
    public class ActionConverter<T0, T1> : JsonConverter<Action<T0, T1>>, IJSInProcessObjectReferenceConverter {

        public override bool CanConvert(Type type) {
            return type.GetGenericTypeDefinition() == typeof(Action<,>);
        }
        public override Action<T0, T1> Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            var _ref = JsonSerializer.Deserialize<IJSInProcessObjectReference>(ref reader, options);
            var fn = new Function(_ref);
            var ret = fn.ToAction<T0, T1>();
            return ret;
        }
        public override void Write(Utf8JsonWriter writer, Action<T0, T1> value, JsonSerializerOptions options) {
            var ret = value.CallbackGet(true);
            JsonSerializer.Serialize(writer, ret, options);
        }
    }
    public class ActionConverter<T0, T1, T2> : JsonConverter<Action<T0, T1, T2>>, IJSInProcessObjectReferenceConverter {

        public override bool CanConvert(Type type) {
            return type.GetGenericTypeDefinition() == typeof(Action<,,>);
        }
        public override Action<T0, T1, T2> Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            var _ref = JsonSerializer.Deserialize<IJSInProcessObjectReference>(ref reader, options);
            var fn = new Function(_ref);
            var ret = fn.ToAction<T0, T1, T2>();
            return ret;
        }
        public override void Write(Utf8JsonWriter writer, Action<T0, T1, T2> value, JsonSerializerOptions options) {
            var ret = value.CallbackGet(true);
            JsonSerializer.Serialize(writer, ret, options);
        }
    }

    public class ActionConverter<T0, T1, T2, T3> : JsonConverter<Action<T0, T1, T2, T3>>, IJSInProcessObjectReferenceConverter {

        public override bool CanConvert(Type type) {
            return type.GetGenericTypeDefinition() == typeof(Action<,,,>);
        }
        public override Action<T0, T1, T2, T3> Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            var _ref = JsonSerializer.Deserialize<IJSInProcessObjectReference>(ref reader, options);
            var fn = new Function(_ref);
            var ret = fn.ToAction<T0, T1, T2, T3>();
            return ret;
        }
        public override void Write(Utf8JsonWriter writer, Action<T0, T1, T2, T3> value, JsonSerializerOptions options) {
            var ret = value.CallbackGet(true);
            JsonSerializer.Serialize(writer, ret, options);
        }
    }
}
