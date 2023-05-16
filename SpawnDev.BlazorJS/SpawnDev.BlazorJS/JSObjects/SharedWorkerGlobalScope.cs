using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public class SharedWorkerGlobalScope : WorkerGlobalScope {
        public SharedWorkerGlobalScope(IJSInProcessObjectReference _ref) : base(_ref) { }
        public void Close() => JSRef.CallVoid("close");
    }
}
