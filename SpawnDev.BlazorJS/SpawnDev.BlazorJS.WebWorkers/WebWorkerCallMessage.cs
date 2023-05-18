using SpawnDev.BlazorJS.JSObjects;
using System.Text.Json.Serialization;

namespace SpawnDev.BlazorJS.WebWorkers {
    public interface IWebWorkerCallMessageBase {
        string TargetName { get; set; }
        string RequestId { get; set; }
        string TargetType { get; set; }
    }

    public class WebWorkerMessageBase : IWebWorkerCallMessageBase {
        public string TargetType { get; set; } = "";
        public string TargetName { get; set; } = "";
        public string RequestId { get; set; } = "";
    }

    internal class WebWorkerMessageOut : WebWorkerMessageBase {
        public object? Data { get; set; } = null;
    }
    public class WebWorkerMessageIn : WebWorkerMessageBase {
        [JsonIgnore]
        public MessageEvent? _msg { get; set; }
        public T? GetData<T>() => _msg == null ? default : _msg.JSRef.Get<T>("data.data");
        public object? GetData(Type type) => _msg == null ? default : _msg.JSRef.Get(type, "data.data");
    }
}
