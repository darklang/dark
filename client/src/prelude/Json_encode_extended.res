include Json.Encode

let variant = (constructor, vals) => jsonArray(Array.of_list(list{string(constructor), ...vals}))

let tcStrSet = set =>
  set |> Tc.Set.toList |> Tc.List.map(~f=Js.Json.string) |> Belt.List.toArray |> jsonArray

let tcStrDict = (f, dict) =>
  dict |> Tc.Map.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_

let beltStrDict = (f, dict) =>
  dict |> Belt.Map.String.toList |> Tc.List.map(~f=((k, v)) => (k, f(v))) |> object_

let date = (d: Js.Date.t) => string(Js.Date.toString(d))

let int64 = (i: int64) =>
  if i > 9007199254740992L {
    i->Int64.to_string->string
  } else if i < -9007199254740992L {
    i->Int64.to_string->string
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53.
    i->Int64.to_float->Json.Encode.float
  }

let uint64 = (i: UInt64.t) =>
  // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
  // 2^53.
  switch UInt64.toFloat(i) {
  | Some(f) => f->Json.Encode.float
  | None => i->UInt64.toString->string
  }

let float' = (f: float): Js.Json.t =>
  if Js.Float.isNaN(f) {
    string("NaN")
  } else if f == Tc.Float.infinity {
    string("Infinity")
  } else if f == Tc.Float.negativeInfinity {
    string("-Infinity")
  } else {
    Json.Encode.float(f)
  }

let result = (fOk, fErr, result): Js.Json.t =>
  switch result {
  | Ok(v) => variant("Ok", list{fOk(v)})
  | Error(v) => variant("Error", list{fErr(v)})
  }

let tuple5 = (encodeA, encodeB, encodeC, encodeD, encodeE, (a, b, c, d, e)) =>
  jsonArray([encodeA(a), encodeB(b), encodeC(c), encodeD(d), encodeE(e)])

module Base64 = {
  @deriving(abstract) type jsUint8Array
  @new external createUint8Array: int => jsUint8Array = "Uint8Array"
  @set_index external setUint8ArrayIdx: (jsUint8Array, int, int) => unit = ""

  let _bytes_to_uint8Array = (input: Bytes.t): jsUint8Array => {
    let len = Bytes.length(input)
    let buf = createUint8Array(len)
    for i in 0 to len - 1 {
      setUint8ArrayIdx(buf, i, int_of_char(Bytes.get(input, i)))
    }
    buf
  }

  let dark_arrayBuffer_to_b64url = %raw(`
  function (arraybuffer) {
    // From https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";
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
      // Dont use padding, to match the backend's urlEncode fn
      if ((len % 3) === 2) {
        base64 = base64.substring(0, base64.length - 1);
      } else if (len % 3 === 1) {
        base64 = base64.substring(0, base64.length - 2);
      }
      return base64;
  }
  `)

  let base64url_bytes = (input: Bytes.t): string =>
    input |> _bytes_to_uint8Array |> dark_arrayBuffer_to_b64url
}

let base64ToString = Base64.base64url_bytes

let base64EncodedBytes = b => b |> Base64.base64url_bytes |> string
