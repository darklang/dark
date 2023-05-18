using SpawnDev.BlazorJS.JSObjects;

namespace SpawnDev.BlazorJS.WebWorkers {
    public class SharedWebWorker : ServiceCallDispatcher, IDisposable {
        public static bool Supported;
        static SharedWebWorker() {
            Supported = !JS.IsUndefined("SharedWorker");
        }
        SharedWorker _shareWorker { get; set; }
        public SharedWebWorker(string name, SharedWorker sharedWorker, IServiceProvider serviceProvider) : base(serviceProvider, sharedWorker.Port) {
            _shareWorker = sharedWorker;
            if (_port is MessagePort port) port.Start();
        }

        public override void Dispose(bool disposing) {
            if (IsDisposed) return;
            _shareWorker?.Dispose();
            _port?.Dispose();
            base.Dispose(disposing);
        }
    }
}
