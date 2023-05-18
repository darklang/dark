using Microsoft.JSInterop;
using System.Text.Json.Serialization;

namespace SpawnDev.BlazorJS
{
    // Example use: (inside a JSObject class)
    // public EventCallback<MediaRecorderErrorEvent> OnError { get => new EventCallback<MediaRecorderErrorEvent>(o => AddEventListener("error", o), o => RemoveEventListener("error", o)); set { } }
    public abstract class EventCallbackBase
    {

    }
    public class AttachedEventInfoBase
    {
        public int RefCount { get; set; }
    }
    public class AttachedEventInfo : AttachedEventInfoBase
    {
        public ActionCallback Callback { get; }
        public Action Action { get; }
        public AttachedEventInfo(Action action, ActionCallback callback) => (Action, Callback) = (action, callback);
    }
    public class EventCallback : EventCallbackBase
    {
        static Dictionary<Action, AttachedEventInfo> attachedEvents = new Dictionary<Action, AttachedEventInfo>();
        public Action<ActionCallback> On { get; private set; }
        public Action<ActionCallback>? Off { get; private set; }
        public static EventCallback operator +(EventCallback a, Action b)
        {
            if (!attachedEvents.TryGetValue(b, out var info))
            {
                info = new AttachedEventInfo(b, new ActionCallback(b));
                attachedEvents[b] = info;
            }
            info.RefCount += 1;
            a.On(info.Callback);
            return a;
        }

        public static EventCallback operator -(EventCallback a, Action b)
        {
            if (!attachedEvents.TryGetValue(b, out var info)) return a;
            a.Off?.Invoke(info.Callback);
            info.RefCount -= 1;
            if (info.RefCount <= 0)
            {
                info.Callback.Dispose();
                attachedEvents.Remove(b);
            }
            return a;
        }
        public EventCallback(Action<ActionCallback> on, Action<ActionCallback>? off = null) : base()
        {
            On = on;
            Off = off;
        }
        public EventCallback(IJSInProcessObjectReference jsRef, string name, string onFn, string offFn = "") : base()
        {
            On = (o) => jsRef.CallVoid(onFn, name, o);
            if (!string.IsNullOrEmpty(offFn)) Off = (o) => jsRef.CallVoid(offFn, name, o);
        }
    }
    public class AttachedEventInfo<T1> : AttachedEventInfoBase
    {
        public ActionCallback<T1> Callback { get; }
        public Action<T1> Action { get; }
        public AttachedEventInfo(Action<T1> action, ActionCallback<T1> callback) => (Action, Callback) = (action, callback);
    }
    public class EventCallback<T1> : EventCallbackBase
    {
        static Dictionary<Action<T1>, AttachedEventInfo<T1>> attachedEvents = new Dictionary<Action<T1>, AttachedEventInfo<T1>>();
        public Action<ActionCallback<T1>> On { get; private set; }
        public Action<ActionCallback<T1>>? Off { get; private set; }
        public static EventCallback<T1> operator +(EventCallback<T1> a, Action<T1> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info))
            {
                info = new AttachedEventInfo<T1>(b, new ActionCallback<T1>(b));
                attachedEvents[b] = info;
            }
            info.RefCount += 1;
            a.On(info.Callback);
            return a;
        }

        public static EventCallback<T1> operator -(EventCallback<T1> a, Action<T1> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info)) return a;
            a.Off?.Invoke(info.Callback);
            info.RefCount -= 1;
            if (info.RefCount <= 0)
            {
                info.Callback.Dispose();
                attachedEvents.Remove(b);
            }
            return a;
        }
        public EventCallback(Action<ActionCallback<T1>> on, Action<ActionCallback<T1>>? off = null) : base()
        {
            On = on;
            Off = off;
        }
        public EventCallback(IJSInProcessObjectReference jsRef, string name, string onFn, string offFn = "") : base()
        {
            On = (o) => jsRef.CallVoid(onFn, name, o);
            if (!string.IsNullOrEmpty(offFn)) Off = (o) => jsRef.CallVoid(offFn, name, o);
        }
    }
    public class AttachedEventInfo<T1, T2> : AttachedEventInfoBase
    {
        public ActionCallback<T1, T2> Callback { get; }
        public Action<T1, T2> Action { get; }
        public AttachedEventInfo(Action<T1, T2> action, ActionCallback<T1, T2> callback) => (Action, Callback) = (action, callback);
    }
    public class EventCallback<T1, T2> : EventCallbackBase
    {
        static Dictionary<Action<T1, T2>, AttachedEventInfo<T1, T2>> attachedEvents = new Dictionary<Action<T1, T2>, AttachedEventInfo<T1, T2>>();
        public Action<ActionCallback<T1, T2>> On { get; private set; }
        public Action<ActionCallback<T1, T2>>? Off { get; private set; }
        public static EventCallback<T1, T2> operator +(EventCallback<T1, T2> a, Action<T1, T2> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info))
            {
                info = new AttachedEventInfo<T1, T2>(b, new ActionCallback<T1, T2>(b));
                attachedEvents[b] = info;
            }
            info.RefCount += 1;
            a.On(info.Callback);
            return a;
        }

        public static EventCallback<T1, T2> operator -(EventCallback<T1, T2> a, Action<T1, T2> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info)) return a;
            a.Off?.Invoke(info.Callback);
            info.RefCount -= 1;
            if (info.RefCount <= 0)
            {
                info.Callback.Dispose();
                attachedEvents.Remove(b);
            }
            return a;
        }
        public EventCallback(Action<ActionCallback<T1, T2>> on, Action<ActionCallback<T1, T2>>? off = null) : base()
        {
            On = on;
            Off = off;
        }
        public EventCallback(IJSInProcessObjectReference jsRef, string name, string onFn, string offFn = "") : base()
        {
            On = (o) => jsRef.CallVoid(onFn, name, o);
            if (!string.IsNullOrEmpty(offFn)) Off = (o) => jsRef.CallVoid(offFn, name, o);
        }
    }
    public class AttachedEventInfo<T1, T2, T3> : AttachedEventInfoBase
    {
        public ActionCallback<T1, T2, T3> Callback { get; }
        public Action<T1, T2, T3> Action { get; }
        public AttachedEventInfo(Action<T1, T2, T3> action, ActionCallback<T1, T2, T3> callback) => (Action, Callback) = (action, callback);
    }
    public class EventCallback<T1, T2, T3> : EventCallbackBase
    {
        static Dictionary<Action<T1, T2, T3>, AttachedEventInfo<T1, T2, T3>> attachedEvents = new Dictionary<Action<T1, T2, T3>, AttachedEventInfo<T1, T2, T3>>();
        public Action<ActionCallback<T1, T2, T3>> On { get; private set; }
        public Action<ActionCallback<T1, T2, T3>>? Off { get; private set; }
        public static EventCallback<T1, T2, T3> operator +(EventCallback<T1, T2, T3> a, Action<T1, T2, T3> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info))
            {
                info = new AttachedEventInfo<T1, T2, T3>(b, new ActionCallback<T1, T2, T3>(b));
                attachedEvents[b] = info;
            }
            info.RefCount += 1;
            a.On(info.Callback);
            return a;
        }

        public static EventCallback<T1, T2, T3> operator -(EventCallback<T1, T2, T3> a, Action<T1, T2, T3> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info)) return a;
            a.Off?.Invoke(info.Callback);
            info.RefCount -= 1;
            if (info.RefCount <= 0)
            {
                info.Callback.Dispose();
                attachedEvents.Remove(b);
            }
            return a;
        }
        public EventCallback(Action<ActionCallback<T1, T2, T3>> on, Action<ActionCallback<T1, T2, T3>>? off = null) : base()
        {
            On = on;
            Off = off;
        }
        public EventCallback(IJSInProcessObjectReference jsRef, string name, string onFn, string offFn = "") : base()
        {
            On = (o) => jsRef.CallVoid(onFn, name, o);
            if (!string.IsNullOrEmpty(offFn)) Off = (o) => jsRef.CallVoid(offFn, name, o);
        }
    }
    public class AttachedEventInfo<T1, T2, T3, T4> : AttachedEventInfoBase
    {
        public ActionCallback<T1, T2, T3, T4> Callback { get; }
        public Action<T1, T2, T3, T4> Action { get; }
        public AttachedEventInfo(Action<T1, T2, T3, T4> action, ActionCallback<T1, T2, T3, T4> callback) => (Action, Callback) = (action, callback);
    }
    public class EventCallback<T1, T2, T3, T4> : EventCallbackBase
    {
        static Dictionary<Action<T1, T2, T3, T4>, AttachedEventInfo<T1, T2, T3, T4>> attachedEvents = new Dictionary<Action<T1, T2, T3, T4>, AttachedEventInfo<T1, T2, T3, T4>>();
        public Action<ActionCallback<T1, T2, T3, T4>> On { get; private set; }
        public Action<ActionCallback<T1, T2, T3, T4>>? Off { get; private set; }
        public static EventCallback<T1, T2, T3, T4> operator +(EventCallback<T1, T2, T3, T4> a, Action<T1, T2, T3, T4> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info))
            {
                info = new AttachedEventInfo<T1, T2, T3, T4>(b, new ActionCallback<T1, T2, T3, T4>(b));
                attachedEvents[b] = info;
            }
            info.RefCount += 1;
            a.On(info.Callback);
            return a;
        }

        public static EventCallback<T1, T2, T3, T4> operator -(EventCallback<T1, T2, T3, T4> a, Action<T1, T2, T3, T4> b)
        {
            if (!attachedEvents.TryGetValue(b, out var info)) return a;
            a.Off?.Invoke(info.Callback);
            info.RefCount -= 1;
            if (info.RefCount <= 0)
            {
                info.Callback.Dispose();
                attachedEvents.Remove(b);
            }
            return a;
        }
        public EventCallback(Action<ActionCallback<T1, T2, T3, T4>> on, Action<ActionCallback<T1, T2, T3, T4>>? off = null) : base()
        {
            On = on;
            Off = off;
        }
        public EventCallback(IJSInProcessObjectReference jsRef, string name, string onFn, string offFn = "") : base()
        {
            On = (o) => jsRef.CallVoid(onFn, name, o);
            if (!string.IsNullOrEmpty(offFn)) Off = (o) => jsRef.CallVoid(offFn, name, o);
        }
    }
}
