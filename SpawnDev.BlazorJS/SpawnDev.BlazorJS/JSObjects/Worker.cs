using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    // https://developer.mozilla.org/en-US/docs/Web/API/Worker
    public class Worker : EventTarget, IMessagePort {
        protected CallbackGroup _callbacks = new CallbackGroup();

        public Worker(IJSInProcessObjectReference _ref) : base(_ref) {
            InitEventsHandlers();
        }
        public Worker(string url) : base(JS.New(nameof(Worker), url)) {
            InitEventsHandlers();
        }

        private void InitEventsHandlers() {
            _OnMessageCallback = Callback.Create<MessageEvent>((e) => _OnMessage?.Invoke(e), _callbacks);
            _OnMessageErrorCallback = Callback.Create(() => _OnMessageError?.Invoke(), _callbacks);
            _OnErrorCallback = Callback.Create(() => _OnError?.Invoke(), _callbacks);
            _OnRejectionHandledCallback = Callback.Create(() => _OnRejectionHandled?.Invoke(), _callbacks);
            _OnUnhandledRejectionCallback = Callback.Create(() => _OnUnhandledRejection?.Invoke(), _callbacks);
        }

        private Callback _OnErrorCallback;
        private event Action _OnError;
        public event Action OnError {
            add {
                _OnError += value;
                if (_OnError.GetInvocationList().Length == 1)
                    AddEventListener("error", _OnErrorCallback);
            }
            remove {
                if (_OnError.GetInvocationList().Length == 1)
                    RemoveEventListener("error", _OnErrorCallback);
                _OnError -= value;
            }
        }

        private Callback _OnMessageCallback;
        private event Action<MessageEvent> _OnMessage;
        public event Action<MessageEvent> OnMessage {
            add {
                _OnMessage += value;
                if (_OnMessage.GetInvocationList().Length == 1)
                    AddEventListener("message", _OnMessageCallback);
            }
            remove {
                if (_OnMessage.GetInvocationList().Length == 1)
                    RemoveEventListener("message", _OnMessageCallback);
                _OnMessage -= value;
            }
        }

        private Callback _OnMessageErrorCallback;
        private event Action _OnMessageError;
        public event Action OnMessageError {
            add {
                _OnMessageError += value;
                if (_OnMessageError.GetInvocationList().Length == 1)
                    AddEventListener("messageerror", _OnMessageErrorCallback);
            }
            remove {
                if (_OnMessageError.GetInvocationList().Length == 1)
                    RemoveEventListener("messageerror", _OnMessageErrorCallback);
                _OnMessageError -= value;
            }
        }

        private Callback _OnRejectionHandledCallback;
        private event Action _OnRejectionHandled;
        public event Action OnRejectionHandled {
            add {
                _OnRejectionHandled += value;
                if (_OnRejectionHandled.GetInvocationList().Length == 1)
                    AddEventListener("rejectionhandled", _OnRejectionHandledCallback);
            }
            remove {
                if (_OnRejectionHandled.GetInvocationList().Length == 1)
                    RemoveEventListener("rejectionhandled", _OnRejectionHandledCallback);
                _OnRejectionHandled -= value;
            }
        }

        private Callback _OnUnhandledRejectionCallback;
        private event Action _OnUnhandledRejection;
        public event Action OnUnhandledRejection {
            add {
                _OnUnhandledRejection += value;
                if (_OnUnhandledRejection.GetInvocationList().Length == 1)
                    AddEventListener("unhandledrejection", _OnUnhandledRejectionCallback);
            }
            remove {
                if (_OnUnhandledRejection.GetInvocationList().Length == 1)
                    RemoveEventListener("unhandledrejection", _OnUnhandledRejectionCallback);
                _OnUnhandledRejection -= value;
            }
        }

        public void Start() => JSRef.CallVoid("start");
        public void Terminate() => JSRef.CallVoid("terminate");
        public void PostMessage(object message) => JSRef.CallVoid("postMessage", message);
        public void PostMessage(object message, object[] transfer) => JSRef.CallVoid("postMessage", message, transfer);

        protected override void LosingReference()
        {
            _callbacks.Dispose();
        }
    }
}
