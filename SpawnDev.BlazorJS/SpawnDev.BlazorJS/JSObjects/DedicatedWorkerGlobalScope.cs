using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public class DedicatedWorkerGlobalScope : WorkerGlobalScope {
        public DedicatedWorkerGlobalScope(IJSInProcessObjectReference _ref) : base(_ref) { }
    }
}
