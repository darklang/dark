using SpawnDev.BlazorJS.JSObjects;

namespace SpawnDev.BlazorJS.WebWorkers {
    public class WebWorker : ServiceCallDispatcher, IDisposable {
        public static bool Supported;
        static WebWorker() {
            Supported = !JS.IsUndefined("Worker");
        }
        Worker _worker;
        public WebWorker(Worker worker, IServiceProvider serviceProvider) : base(serviceProvider, worker) {
            _worker = worker;
        }

        public override void Dispose(bool disposing) {
            if (IsDisposed) return;
            try {
                _worker?.Terminate();
            }
            catch { }
            _worker?.Dispose();
            base.Dispose(disposing);
        }
    }
}
