using Microsoft.JSInterop;

namespace SpawnDev.BlazorJS.JSObjects {

    public class Uint8ClampedArray : JSObject {
        public Uint8ClampedArray(IJSInProcessObjectReference _ref) : base(_ref) { }
        public ArrayBuffer Buffer => JSRef.Get<ArrayBuffer>("buffer");
        public int ByteLength => JSRef.Get<int>("byteLength");
        public static Uint8ClampedArray FromBytes(byte[] bytes) {
            using var arrayBuffer = new Uint8Array(bytes);
            return new Uint8ClampedArray(arrayBuffer);
        }
        public Uint8ClampedArray(Uint8Array uint8Array) : base(JS.New(nameof(Uint8ClampedArray), uint8Array)) { }
        public Uint8ClampedArray(int length) : base(JS.New(nameof(Uint8ClampedArray), length)) { }
        public byte[] ReadBytes() {
            using var buffer = Buffer;
            using var tmp = new Uint8Array(buffer);
            return tmp.ReadBytes();
        }
    }
}
