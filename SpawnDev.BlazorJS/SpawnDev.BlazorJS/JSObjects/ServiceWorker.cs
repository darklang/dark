using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public class ServiceWorker : EventTarget {
        CallbackGroup _callbacks = new CallbackGroup();
        public string State => JSRef.Get<string>("state");
        public string ScriptURL => JSRef.Get<string>("scriptURL");
        public delegate void MessageDelegate(MessageEvent msg);
        public event MessageDelegate OnStateChange;
        public static bool IsSupported => !JS.IsUndefined("navigator.serviceWorker");
        public ServiceWorker(IJSInProcessObjectReference _ref) : base(_ref) { }


        protected override void FromReference(IJSInProcessObjectReference _ref) {
            base.FromReference(_ref);
            AddEventListener("statechange", Callback.Create<MessageEvent>((e) => {
                OnStateChange?.Invoke(e);
                e.Dispose();
            }, _callbacks));
        }
        protected override void LosingReference()
        {
            _callbacks.Dispose();
        }
    }
}
