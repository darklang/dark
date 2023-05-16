using Microsoft.JSInterop;
using System.Reflection;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace SpawnDev.BlazorJS.JsonConverters
{
    public class JSObjectConverterFactory : JsonConverterFactory
    {
        public override bool CanConvert(Type type)
        {
            return typeof(JSObject).IsAssignableFrom(type);
        }

        public override JsonConverter? CreateConverter(Type typeToConvert, JsonSerializerOptions options)
        {
            var covnerterType = typeof(JSObjectConverter<>).MakeGenericType(new Type[] { typeToConvert });
            JsonConverter converter = (JsonConverter)Activator.CreateInstance(covnerterType, BindingFlags.Instance | BindingFlags.Public, binder: null, args: new object[] { }, culture: null)!;
            return converter;
        }
    }

    public class JSObjectConverter<TJSObject> : JsonConverter<TJSObject>, IJSInProcessObjectReferenceConverter where TJSObject : JSObject
    {
        public override bool CanConvert(Type type)
        {
            return typeof(TJSObject).IsAssignableFrom(type);
        }
        public override TJSObject Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
        {
            var _ref = JsonSerializer.Deserialize<IJSInProcessObjectReference>(ref reader, options);
            return _ref == null ? null : (TJSObject)Activator.CreateInstance(typeof(TJSObject), _ref);
        }
        public override void Write(Utf8JsonWriter writer, TJSObject value, JsonSerializerOptions options)
        {
            var obj = value as JSObject;
            if (value.IsJSRefUndefined)
            {
                // cast to JSInProcessObjectReferenceUndefined so it gets picked up by the JSInProcessObjectReferenceUndefinedConverter
                JsonSerializer.Serialize(writer, (JSInProcessObjectReferenceUndefined)value.JSRef, options);
            }
            else
            {
                var _ref = obj == null ? null : obj.JSRef;
                JsonSerializer.Serialize(writer, _ref, options);
            }
        }
    }
}
