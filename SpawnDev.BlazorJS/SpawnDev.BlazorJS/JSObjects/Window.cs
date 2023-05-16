using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public class Window : EventTarget {
        public Window() : base(JS.Get<IJSInProcessObjectReference>("window")) { }
        public Window(IJSInProcessObjectReference _ref) : base(_ref) { }
        public string? Name { get => JSRef.Get<string>("name"); set => JSRef.Set("name", value); }
        public void Alert(string msg = "") => JSRef.CallVoid("alert", msg);
        public long SetTimeout(Callback callback, double delay) => JSRef.Call<long>("setTimeout", callback, delay);
        public void ClearTimeout(long requestId) => JSRef.CallVoid("clearTimeout", requestId);
        public double DevicePixelRatio { get { var tmp = JSRef.Get<double>("devicePixelRatio"); return tmp > 0d ? tmp : 1d; } }
        public int InnerWidth => JSRef.Get<int>("innerWidth");
        public int InnerHeight => JSRef.Get<int>("innerHeight");
    }
}
