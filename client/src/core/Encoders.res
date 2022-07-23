open Prelude
open Json.Encode

// Dark

@deriving(abstract) type jsArrayBuffer = {byteLength: int}

@deriving(abstract) type jsUint8Array

@new external createUint8Array: int => jsUint8Array = "Uint8Array"

@set_index external setUint8ArrayIdx: (jsUint8Array, int, int) => unit = ""

let dark_arrayBuffer_to_b64url = %raw(`
  function (arraybuffer) {
    // TODO: Actually import https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js as a lib and use encode here
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

      if ((len % 3) === 2) {
        base64 = base64.substring(0, base64.length - 1) + "=";
      } else if (len % 3 === 1) {
        base64 = base64.substring(0, base64.length - 2) + "==";
      }

      return base64;
  }
`)

let _bytes_to_uint8Array = (input: Bytes.t): jsUint8Array => {
  let len = Bytes.length(input)
  let buf = createUint8Array(len)
  for i in 0 to len - 1 {
    setUint8ArrayIdx(buf, i, int_of_char(Bytes.get(input, i)))
  }
  buf
}

let base64url_bytes = (input: Bytes.t): string =>
  input |> _bytes_to_uint8Array |> dark_arrayBuffer_to_b64url

let id = ID.encode
let tlid = TLID.encode

let pos = BaseTypes.encodePos

let blankOr = BaseTypes.encodeBlankOr

let blankOrData = (pd: Types.blankOrData): Js.Json.t => {
  let ev = variant
  switch pd {
  | PEventName(name) => ev("PEventName", list{blankOr(string, name)})
  | PEventModifier(modifier) => ev("PEventModifier", list{blankOr(string, modifier)})
  | PEventSpace(space) => ev("PEventSpace", list{blankOr(string, space)})
  | PDBName(name) => ev("PDBName", list{blankOr(string, name)})
  | PDBColName(colname) => ev("PDBColName", list{blankOr(string, colname)})
  | PDBColType(coltype) => ev("PDBColType", list{blankOr(string, coltype)})
  | PFnName(msg) => ev("PFnName", list{blankOr(string, msg)})
  | PFnReturnTipe(msg) => ev("PFnReturnTipe", list{blankOr(DType.encode, msg)})
  | PParamName(msg) => ev("PParamName", list{blankOr(string, msg)})
  | PParamTipe(msg) => ev("PParamTipe", list{blankOr(DType.encode, msg)})
  | PTypeName(n) => ev("PTypeName", list{blankOr(string, n)})
  | PTypeFieldName(n) => ev("PTypeFieldName", list{blankOr(string, n)})
  | PTypeFieldTipe(t) => ev("PTypeFieldTipe", list{blankOr(DType.encode, t)})
  }
}

and ops = (ops: list<PT.Op.t>): Js.Json.t =>
  list(
    PT.Op.encode,
    switch ops {
    | list{UndoTL(_)} => ops
    | list{RedoTL(_)} => ops
    | list{} => ops
    | _ =>
      let savepoints = List.map(~f=op => PT.Op.TLSavepoint(PT.Op.tlidOf(op)), ops)

      Belt.List.concat(savepoints, ops)
    },
  )

let httpError = (e: Tea.Http.error<string>): Js.Json.t => {
  module Http = Tea.Http
  let responseBody = (r: Http.responseBody) =>
    switch r {
    | NoResponse => object_(list{("noResponse", null)})
    | StringResponse(str) => string(str)
    | ArrayBufferResponse() => object_(list{("arrayBufferResponse", null)})
    | BlobResponse() => object_(list{("blobResponse", null)})
    | DocumentResponse(_) => object_(list{("documentResponse", string("<documentResponse>"))})
    | JsonResponse(json) => json
    | TextResponse(text) => string(text)
    | RawResponse(str, ()) => object_(list{("rawResponse", string(str))})
    }

  let response = (r: Http.response) => {
    module StringMap = Caml.Map.Make(Tc.Caml.String)
    object_(list{
      ("url", string(r.url)),
      (
        "status",
        object_(list{("code", int(r.status.code)), ("message", string(r.status.message))}),
      ),
      (
        "headers",
        r.headers |> StringMap.bindings |> List.map(~f=((k, v)) => (k, string(v))) |> object_,
      ),
      ("body", responseBody(r.body)),
    })
  }

  switch e {
  | Http.BadUrl(url) => object_(list{("type", string("BadUrl")), ("url", string(url))})
  | Http.Timeout => object_(list{("type", string("Timeout"))})
  | Http.NetworkError => object_(list{("type", string("NetworkError"))})
  | Http.BadStatus(r) => object_(list{("type", string("BadStatus")), ("response", response(r))})
  | Http.BadPayload(msg, r) =>
    object_(list{
      ("type", string("BadPayload")),
      ("message", string(msg)),
      ("response", response(r)),
    })
  | Http.Aborted => object_(list{("type", string("Aborted"))})
  }
}
