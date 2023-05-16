using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public class SharedWorker : EventTarget {
        public MessagePort Port => JSRef.Get<MessagePort>("port");
        public SharedWorker(IJSInProcessObjectReference _ref) : base(_ref) { }
        public SharedWorker(string url) : base(JS.New(nameof(SharedWorker), url)) { }
        public SharedWorker(string url, string name) : base(JS.New(nameof(SharedWorker), url, name)) { }
    }
}
