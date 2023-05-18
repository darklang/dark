using Microsoft.JSInterop;
using System.Reflection;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace SpawnDev.BlazorJS.JsonConverters {

    public class IJSObjectConverterFactory : JsonConverterFactory {
        public override bool CanConvert(Type type) {
            return CanConvertType(type);
        }

        public static bool CanConvertType(Type type) {
            return typeof(IJSObject).IsAssignableFrom(type);
        }

        public override JsonConverter? CreateConverter(Type typeToConvert, JsonSerializerOptions options) {
            var covnerterType = typeof(IJSObjectConverter<>).MakeGenericType(new Type[] { typeToConvert });
            JsonConverter converter = (JsonConverter)Activator.CreateInstance(covnerterType, BindingFlags.Instance | BindingFlags.Public, binder: null, args: new object[] { }, culture: null)!;
            return converter;
        }
    }

    // https://github.com/dotnet/aspnetcore/blob/ccb861b89f62c445f175f6a3ca2142f93e7ce5db/src/Components/WebAssembly/JSInterop/src/WebAssemblyJSObjectReferenceJsonConverter.cs#L10
    // WebAssemblyJSObjectReferenceJsonConverter.cs
    public class IJSObjectConverter<TInterface> : JsonConverter<TInterface>, IJSInProcessObjectReferenceConverter where TInterface : class, IJSObject
    {
        public override bool CanConvert(Type type) {
            return typeof(TInterface).IsAssignableFrom(type);
        }
        public override TInterface Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options) {
            var _ref = JsonSerializer.Deserialize<IJSInProcessObjectReference>(ref reader, options);
            return _ref == null ? null : IJSObjectProxy.GetInterface<TInterface>(_ref);
        }
        public override void Write(Utf8JsonWriter writer, TInterface value, JsonSerializerOptions options) {
            var obj = value as IJSObjectProxy;
            var _ref = obj == null ? null : obj.JSRef;
            JsonSerializer.Serialize(writer, _ref, options);
        }
    }
}
