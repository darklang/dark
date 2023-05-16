using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public class WorkerGlobalScope : EventTarget {
        public WorkerGlobalScope(IJSInProcessObjectReference _ref) : base(_ref) { }
        public void ImportScripts(params string[] scripts) => JSRef.CallApplyVoid("importScripts", scripts);
    }
}
