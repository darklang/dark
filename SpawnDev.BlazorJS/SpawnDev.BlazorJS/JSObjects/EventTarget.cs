using Microsoft.JSInterop;
using System.Text.Json.Serialization;

namespace SpawnDev.BlazorJS.JSObjects {
    public class AddEventListenerOptions {
        [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        public bool Capture { get; set; } = false;

        [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        public bool? Once { get; set; } = null;

        [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        public bool? Passive { get; set; } = null;
    }

    public class EventTarget : JSObject {
        public EventTarget(IJSInProcessObjectReference _ref) : base(_ref) { }

        public bool DispatchEvent(string type) => JSRef.Call<bool>("dispatchEvent", type);

        public void AddEventListener(string type, Callback listener, bool useCapture = false) => JSRef.CallVoid("addEventListener", type, listener, useCapture);

        public void AddEventListener(string type, Callback listener, AddEventListenerOptions options) => JSRef.CallVoid("addEventListener", type, listener, options);

        public void RemoveEventListener(string type, Callback listener, bool useCapture = false) => JSRef.CallVoid("removeEventListener", type, listener, useCapture);
    }
}