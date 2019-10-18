//Provides: dark_arrayBuffer_from_b64
function dark_arrayBuffer_from_b64(base64) {
  console.log("DECODING: "+base64);
  // TODO(JULIAN): Actually import https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js as a lib and use decode here
  var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  // Use a lookup table to find the index.
  var lookup = new Uint8Array(256);
  for (var i = 0; i < chars.length; i++) {
    lookup[chars.charCodeAt(i)] = i;
  }


  var bufferLength = base64.length * 0.75, len = base64.length, i, p = 0, encoded1, encoded2, encoded3, encoded4;

  if (base64[base64.length - 1] === "=") {
    bufferLength--;
    if (base64[base64.length - 2] === "=") {
      bufferLength--;
    }
  }

  var arraybuffer = new ArrayBuffer(bufferLength),
  bytes = new Uint8Array(arraybuffer);

  for (i = 0; i < len; i+=4) {
    encoded1 = lookup[base64.charCodeAt(i)];
    encoded2 = lookup[base64.charCodeAt(i+1)];
    encoded3 = lookup[base64.charCodeAt(i+2)];
    encoded4 = lookup[base64.charCodeAt(i+3)];

    bytes[p++] = (encoded1 << 2) | (encoded2 >> 4);
    bytes[p++] = ((encoded2 & 15) << 4) | (encoded3 >> 2);
    bytes[p++] = ((encoded3 & 3) << 6) | (encoded4 & 63);
  }

  console.log(arraybuffer);
  console.log(arraybuffer.byteLength);
  return arraybuffer;
}

//Provides: dark_arrayBuffer_to_b64
function dark_arrayBuffer_to_b64(arraybuffer) {
  console.log("ENCODING: ");
  console.log(arraybuffer);
  // TODO(JULIAN): Actually import https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js as a lib and use encode here
  var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  // Use a lookup table to find the index.
  var lookup = new Uint8Array(256);
  for (var i = 0; i < chars.length; i++) {
    lookup[chars.charCodeAt(i)] = i;
  }

    var bytes = new Uint8Array(arraybuffer),
    i, len = bytes.length, base64 = "";

    for (i = 0; i < len; i+=3) {
      base64 += chars[bytes[i] >> 2];
      base64 += chars[((bytes[i] & 3) << 4) | (bytes[i + 1] >> 4)];
      base64 += chars[((bytes[i + 1] & 15) << 2) | (bytes[i + 2] >> 6)];
      base64 += chars[bytes[i + 2] & 63];
    }

    if ((len % 3) === 2) {
      base64 = base64.substring(0, base64.length - 1) + "=";
    } else if (len % 3 === 1) {
      base64 = base64.substring(0, base64.length - 2) + "==";
    }

    return base64;
}

//Provides: dark_targetjs_digest384
function dark_targetjs_digest384(s) {
  var b64 = analysiswrapper.sha2.SHA384(s).toString("base64");
  // the built-in Buffer.toString uses a different alphabet than the server.
  return b64.replace(/\//g, "_").replace(/\+/g, "-");
}

//Provides: dark_targetjs_digest384_bytes
function dark_targetjs_digest384_bytes(uint8Array) {
  var b64 = analysiswrapper.sha2.SHA384(uint8Array).toString("base64");
  // the built-in Buffer.toString uses a different alphabet than the server.
  return b64.replace(/\//g, "_").replace(/\+/g, "-");
}

//Provides: dark_targetjs_digest256
function dark_targetjs_digest256(s) {
  var b64 = analysiswrapper.sha2.SHA256(s).toString("base64");
  // the built-in Buffer.toString uses a different alphabet than the server.
  return b64.replace(/\//g, "_").replace(/\+/g, "-");
}

// Temporary, should be fixed in js_of_ocaml >3.2.1:
// https://github.com/ocsigen/js_of_ocaml/issues/693
// https://github.com/ocsigen/js_of_ocaml/commit/390fd5f1d2bbaed8d9da3df2a77a6b67f248ebbb

//Provides: caml_restore_raw_backtrace
function caml_restore_raw_backtrace(exn, bt) {
  return 0;
}