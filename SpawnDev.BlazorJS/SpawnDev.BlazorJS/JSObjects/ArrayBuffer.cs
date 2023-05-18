using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {
    public class ArrayBuffer : JSObject {
        public int ByteLength => JSRef.Get<int>("byteLength");
        public ArrayBuffer(long length) : base(JS.New(nameof(ArrayBuffer), length)) { }
        public ArrayBuffer(IJSInProcessObjectReference _ref) : base(_ref) { }
        public byte[] ReadBytes() {
            using var uint8array = new Uint8Array(this);
            return uint8array.ReadBytes();
        }
    }
}
