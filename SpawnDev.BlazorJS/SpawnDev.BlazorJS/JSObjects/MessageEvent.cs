using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    /// The MessageEvent interface represents a message received by a target object.
    public class MessageEvent : JSObject {
        public MessageEvent(IJSInProcessObjectReference _ref) : base(_ref) { }
        /// The data sent by the message emitter. (data property with T typed get accessor)
        public T GetData<T>() => JSRef.Get<T>("data");
    }
}
