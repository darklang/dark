using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;
using SpawnDev.BlazorJS.JSObjects;
using System.Collections.Specialized;
using System.Web;

namespace SpawnDev.BlazorJS.WebWorkers {
    // chrome://inspect/#workers
    public class WebWorkerService : IDisposable, IBackgroundService
    {
        public bool SharedWebWorkerSupported { get; private set; }
        public bool WebWorkerSupported { get; private set; }
        public List<WebWorker> Workers { get; } = new List<WebWorker>();
        public List<SharedWebWorker> SharedWorkers { get; } = new List<SharedWebWorker>();
        IServiceProvider _serviceProvider;
        public string AppBaseUri { get; }
        public bool BeenInit { get; private set; }
        DateTime StartTime = DateTime.Now;
        public int MaxWorkerCount { get; private set; } = 0;
        public string ThisSharedWorkerName { get; private set; } = "";

        CallbackGroup _callbackGroup = new CallbackGroup();
        string InstanceId { get; } = Guid.NewGuid().ToString();
        static string WebWorkerJSScript = "_content/SpawnDev.BlazorJS.WebWorkers/spawndev.blazorjs.webworkers.js";
        BlazorJSRuntime JS;
        public WebWorkerService(IServiceProvider serviceProvider, IWebAssemblyHostEnvironment hostEnvironment, BlazorJSRuntime js) {
            JS = js;
            WebWorkerSupported = !JS.IsUndefined("Worker");
            SharedWebWorkerSupported = !JS.IsUndefined("SharedWorker");
            _serviceProvider = serviceProvider;
            AppBaseUri = JS.Get<string>("document.baseURI");
            var appBaseUriObj = new Uri(AppBaseUri);
            var workerScriptUri = new Uri(appBaseUriObj, WebWorkerJSScript);
            WebWorkerJSScript = workerScriptUri.ToString();
#if DEBUG
            Console.WriteLine("hostEnvironment.BaseAddress: " + hostEnvironment.BaseAddress);
            Console.WriteLine("AppBaseUri: " + AppBaseUri);
            Console.WriteLine("WebWorkerJSScript: " + WebWorkerJSScript);
#endif
            var hardwareConcurrency = JS.Get<int?>("navigator.hardwareConcurrency");
            MaxWorkerCount = hardwareConcurrency == null || hardwareConcurrency.Value == 0 ? 0 : hardwareConcurrency.Value;
        }

        class WebWorkerServiceEventMsgBase {
            public string SrcId { get; set; } = "";
            public string Event { get; set; } = "";
        }
        class WebWorkerServiceEventMsgOutoing : WebWorkerServiceEventMsgBase {
            public object? Data { get; set; } = null;
        }
        public class RunningInstance {
            public string SharedWorkerName { get; set; } = "";
            public string GlobalThisTypeName { get; set; } = "";
            public string SrcId { get; set; } = "";
            public string AppBaseURI { get; set; } = "";
            public DateTime StartTime { get; set; }
            public DateTime LastSeen { get; set; } = DateTime.Now;
        }

        RunningInstance ThisInstance() => new RunningInstance {
            AppBaseURI = AppBaseUri,
            GlobalThisTypeName = JS.GlobalThisTypeName,
            SharedWorkerName = ThisSharedWorkerName,
            SrcId = InstanceId,
            StartTime = StartTime,
            LastSeen = DateTime.Now,
        };

        public ServiceCallDispatcher? DedicatedWorkerParent { get; private set; } = null;

        public void SendEventToParents(string eventName, object? data = null) {
            if (DedicatedWorkerParent != null) {
                DedicatedWorkerParent.SendEvent(eventName, data);
            }
            foreach (var p in SharedWorkerIncomingConnections) {
                p.SendEvent(eventName, data);
            }
        }

        public async Task InitAsync() {
            if (BeenInit) return;
            BeenInit = true;
            //_eventChannel.OnMessage += _eventChannel_OnMessage;
            await Task.Delay(1);
            if (JS.IsDedicatedWorkerGlobalScope) {
                var incomingPort = JS.Get<MessagePort>("self");
                DedicatedWorkerParent = new ServiceCallDispatcher(_serviceProvider, incomingPort);
                //_sharedWorkerIncomingConnections.Add(incomingHandler);
                DedicatedWorkerParent.SendReadyFlag();
            }
            else if (JS.IsSharedWorkerGlobalScope) {
                var missedConnections = JS.Call<MessagePort[]>("takeOverOnConnectEvent", Callback.Create<MessageEvent>(OnSharedWorkerConnect, _callbackGroup));
                if (missedConnections != null) {
                    foreach (var m in missedConnections) {
                        AddIncomingPort(m);
                    }
                }
                try {
                    var tmpName = JS.Get<string?>("name");
                    if (!string.IsNullOrEmpty(tmpName)) {
                        ThisSharedWorkerName = tmpName;
                    }
                }
                catch { }
                JS.Log($"SharedWorker {ThisSharedWorkerName} listening for connections: took {SharedWorkerIncomingConnections.Count} missed connections");
            }
            //BroadcastPing();
        }

        int pingWaitTime = 1000;
        async Task<List<RunningInstance>> BroadcastPing() {
            BroadcastEvent("ping", ThisInstance());
            await Task.Delay(pingWaitTime);
            var now = DateTime.Now;
            var active = KnownRunning.Values.Where(o => now - o.LastSeen < TimeSpan.FromMilliseconds(pingWaitTime)).ToList();
            var inactive = KnownRunning.Values.Where(o => now - o.LastSeen > TimeSpan.FromMilliseconds(pingWaitTime)).ToList();
            KnownRunning = active.ToDictionary(o => o.SrcId, o => o);
            return active;
        }

        void BroadcastPong() {
            BroadcastEvent("pong", ThisInstance());
        }

        void BroadcastEvent(string eventName, object? data = null) {
            var m = new WebWorkerServiceEventMsgOutoing {
                SrcId = InstanceId,
                Event = eventName,
                Data = data
            };
            //_eventChannel.PostMessaage(m);
        }

        Dictionary<string, RunningInstance> KnownRunning = new Dictionary<string, RunningInstance>();

        void InstanceSeen(RunningInstance pongData) {
            var isNew = false;
            RunningInstance? entry = null;
            if (!KnownRunning.TryGetValue(pongData.SrcId, out entry)) {
                JS.Log("Instance first seen:", pongData);
                isNew = true;
            }
            KnownRunning[pongData.SrcId] = pongData;
        }

        private void _eventChannel_OnMessage(MessageEvent msg) {
            try {
                var m = msg.GetData<WebWorkerServiceEventMsgBase>();
                switch (m.Event) {
                    case "ping":
                        var pingData = msg.JSRef.Get<RunningInstance>("data.data");
                        JS.Log($"ping", pingData);
                        InstanceSeen(pingData);
                        BroadcastPong();
                        break;
                    case "pong":
                        var pongData = msg.JSRef.Get<RunningInstance>("data.data");
                        JS.Log($"pong", pongData);
                        InstanceSeen(pongData);
                        break;
                    default:
                        JS.Log("UNHANDLED msg: WebWorkerServiceEventMsg", m);
                        break;
                }
            }
            catch (Exception ex) {
                JS.Log("ERROR: _eventChannel_OnMessage");
            }
        }

        public List<ServiceCallDispatcher> SharedWorkerIncomingConnections { get; private set; } = new List<ServiceCallDispatcher>();

        void OnSharedWorkerConnect(MessageEvent e) {
            JS.Log("OnSharedWorkerConnect");
            using var ports = e.JSRef.Get<JSObject>("ports");
            var incomingPort = ports.JSRef.Get<MessagePort>(0);
            AddIncomingPort(incomingPort);
            e.Dispose();
        }

        void AddIncomingPort(MessagePort incomingPort) {
            var incomingHandler = new ServiceCallDispatcher(_serviceProvider, incomingPort);
            SharedWorkerIncomingConnections.Add(incomingHandler);
            incomingPort.Start();
            JS.Log("AddIncomingPort", SharedWorkerIncomingConnections.Count);
            incomingHandler.SendReadyFlag();
        }

        string ToQueryString(NameValueCollection source) {
            return string.Join("&", source.AllKeys.SelectMany(source.GetValues, (k, v) => $"{HttpUtility.UrlEncode(k)}={HttpUtility.UrlEncode(v)}"));
        }

        public async Task<WebWorker?> GetWebWorker(bool verboseMode = false, bool awaitWhenReady = true) {
            if (!WebWorkerSupported) return null;
            var queryArgs = new NameValueCollection();
            queryArgs.Add("verbose", verboseMode ? "true" : "false");
            var worker = new Worker($"{WebWorkerJSScript}?{ToQueryString(queryArgs)}");
            var webWorker = new WebWorker(worker, _serviceProvider);
            Workers.Add(webWorker);
            if (awaitWhenReady) await webWorker.WhenReady;
            return webWorker;
        }

        /// <summary>
        /// Returns a SharedWebWorker
        /// </summary>
        /// <param name="sharedWorkerName">SharedWebWorkers are identified by name. 1 shared worker will be created per name.</param>
        /// <param name="verboseMode"></param>
        /// <param name="awaitWhenReady"></param>
        /// <returns></returns>
        public async Task<SharedWebWorker?> GetSharedWebWorker(string sharedWorkerName = "", bool verboseMode = false, bool awaitWhenReady = true) {
            if (!SharedWebWorkerSupported) return null;
            var queryArgs = new NameValueCollection();
            queryArgs.Add("verbose", verboseMode ? "true" : "false");
            var worker = new SharedWorker($"{WebWorkerJSScript}?{ToQueryString(queryArgs)}", sharedWorkerName);
            var webWorker = new SharedWebWorker(sharedWorkerName, worker, _serviceProvider);
            if (awaitWhenReady) await webWorker.WhenReady;
            return webWorker;
        }

        public void Dispose()
        {
            _callbackGroup.Dispose();
        }
    }
}
