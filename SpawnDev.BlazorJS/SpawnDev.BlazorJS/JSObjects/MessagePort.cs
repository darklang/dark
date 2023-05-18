using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public interface IMessagePort {
        event Action OnMessageError;
        event Action<MessageEvent> OnMessage;

        void Dispose();
        void PostMessage(object message);
        void PostMessage(object message, object[] transfer);
    }

    // https://developer.mozilla.org/en-US/docs/Web/API/MessagePort

    public class MessagePort : EventTarget, IMessagePort {
        CallbackGroup _callbacks = new CallbackGroup();

        public MessagePort(IJSInProcessObjectReference _ref) : base(_ref) {
            InitEventsHandlers();
        }

        private void InitEventsHandlers() {
            _OnMessageCallback = Callback.Create<MessageEvent>((e) => _OnMessage?.Invoke(e), _callbacks);
            _OnMessageErrorCallback = Callback.Create(() => _OnMessageError?.Invoke(), _callbacks);
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

        public void Start() => JSRef.CallVoid("start");
        public void Close() => JSRef.CallVoid("close");
        public void PostMessage(object message) => JSRef.CallVoid("postMessage", message);
        public void PostMessage(object message, object[] transfer) => JSRef.CallVoid("postMessage", message, transfer);

        protected override void LosingReference()
        {
            _callbacks.Dispose();
        }
    }
}
