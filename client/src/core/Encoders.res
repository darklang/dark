open Prelude
open Json.Encode

// Dark

// XXX(JULIAN): All of this should be cleaned up and moved somewhere nice!
@deriving(abstract) type jsArrayBuffer = {byteLength: int}

@deriving(abstract) type jsUint8Array

@new external createUint8Array: int => jsUint8Array = "Uint8Array"

@set_index external setUint8ArrayIdx: (jsUint8Array, int, int) => unit = ""

let dark_arrayBuffer_to_b64url = %raw(`
  function (arraybuffer) {
    // TODO(JULIAN): Actually import https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js as a lib and use encode here
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

let dval_source = (s: Types.dval_source): Js.Json.t => {
  let ev = variant
  switch s {
  | SourceNone => ev("SourceNone", list{})
  | SourceId(t, i) => ev("SourceId", list{tlid(t), id(i)})
  }
}

let rec ocamlDval = (dv: Types.dval): Js.Json.t => {
  let ev = variant
  let dhttp = h =>
    switch h {
    | Redirect(s) => ev("Redirect", list{string(s)})
    | Response(code, headers) =>
      ev("Response", list{int(code), list(tuple2(string, string), headers)})
    }

  switch dv {
  | DInt(i) => ev("DInt", list{int64(i)})
  | DFloat(f) => ev("DFloat", list{Json.Encode.float(f)})
  | DBool(b) => ev("DBool", list{bool(b)})
  | DNull => ev("DNull", list{})
  | DStr(s) => ev("DStr", list{string(s)})
  | DList(l) => ev("DList", list{array(ocamlDval, l)})
  | DTuple(first, second, theRest) =>
    ev("DTuple", list{ocamlDval(first), ocamlDval(second), array(ocamlDval, List.toArray(theRest))})
  | DObj(o) =>
    o->Belt.Map.String.map(ocamlDval)->Belt.Map.String.toList
    |> Js.Dict.fromList
    |> jsonDict
    |> (x => list{x} |> ev("DObj"))
  | DBlock({body, params, symtable}) =>
    let dblock_args = object_(list{
      ("symtable", beltStrDict(ocamlDval, symtable)),
      ("params", list(pair(id, string), params)),
      ("body", PT.Expr.encode(body)),
    })

    ev("DBlock", list{dblock_args})
  | DIncomplete(ds) => ev("DIncomplete", list{dval_source(ds)})
  // user-ish types
  | DCharacter(c) => ev("DCharacter", list{string(c)})
  | DError(ds, msg) => ev("DError", list{pair(dval_source, string, (ds, msg))})
  | DResp(h, hdv) => ev("DResp", list{tuple2(dhttp, ocamlDval, (h, hdv))})
  | DDB(name) => ev("DDB", list{string(name)})
  | DDate(date) => ev("DDate", list{string(date)})
  | DPassword(hashed) => ev("DPassword", list{string(hashed)})
  | DUuid(uuid) => ev("DUuid", list{string(uuid)})
  | DOption(opt) =>
    ev(
      "DOption",
      list{
        switch opt {
        | OptNothing => ev("OptNothing", list{})
        | OptJust(dv) => ev("OptJust", list{ocamlDval(dv)})
        },
      },
    )
  | DErrorRail(dv) => ev("DErrorRail", list{ocamlDval(dv)})
  | DResult(res) =>
    ev(
      "DResult",
      list{
        switch res {
        | ResOk(dv) => ev("ResOk", list{ocamlDval(dv)})
        | ResError(dv) => ev("ResError", list{ocamlDval(dv)})
        },
      },
    )
  | DBytes(bin) => ev("DBytes", list{string(bin |> base64url_bytes)})
  }
}

and blankOrData = (pd: Types.blankOrData): Js.Json.t => {
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

and executeFunctionAPIParams = (params: Types.executeFunctionAPIParams): Js.Json.t =>
  object_(list{
    ("tlid", tlid(params.efpTLID)),
    ("trace_id", string(params.efpTraceID)),
    ("caller_id", id(params.efpCallerID)),
    ("args", list(ocamlDval, params.efpArgs)),
    ("fnname", string(params.efpFnName)),
  })

and deleteToplevelForeverAPIParams = (params: Types.deleteToplevelForeverAPIParams): Js.Json.t =>
  object_(list{("tlid", tlid(params.dtfTLID))})

and uploadFnAPIParams = (params: Types.uploadFnAPIParams): Js.Json.t =>
  object_(list{("fn", PT.UserFunction.encode(params.uplFn))})

and triggerHandlerAPIParams = (params: Types.triggerHandlerAPIParams): Js.Json.t =>
  object_(list{
    ("tlid", tlid(params.thTLID)),
    ("trace_id", string(params.thTraceID)),
    ("input", list(tuple2(string, ocamlDval), Belt.Map.String.toList(params.thInput))),
  })

and sendPresenceParams = (params: Types.sendPresenceParams): Js.Json.t =>
  object_(list{
    ("canvasName", string(params.canvasName)),
    ("browserId", string(params.browserId)),
    ("tlid", nullable(tlid, params.tlid)),
    ("timestamp", Json.Encode.float(params.timestamp)),
  })

and sendInviteParams = (params: Types.sendInviteParams): Js.Json.t =>
  object_(list{
    ("email", string(params.email)),
    ("inviterUsername", string(params.inviterUsername)),
    ("inviterName", string(params.inviterName)),
  })

and getTraceDataAPIParams = (params: Types.getTraceDataAPIParams): Js.Json.t =>
  object_(list{("tlid", tlid(params.gtdrpTlid)), ("trace_id", traceID(params.gtdrpTraceID))})

and dbStatsAPIParams = (params: Types.dbStatsAPIParams): Js.Json.t =>
  object_(list{("tlids", list(tlid, params.dbStatsTlids))})

and workerStatsAPIParams = (params: Types.workerStatsAPIParams): Js.Json.t =>
  object_(list{("tlid", tlid(params.workerStatsTlid))})

and updateWorkerScheduleAPIParams = (params: Types.updateWorkerScheduleAPIParams): Js.Json.t =>
  object_(list{("name", string(params.workerName)), ("schedule", string(params.schedule))})

and secret = (s: SecretTypes.t): Js.Json.t =>
  object_(list{("secret_name", string(s.secretName)), ("secret_value", string(s.secretValue))})

and performHandlerAnalysisParams = (params: Types.performHandlerAnalysisParams): Js.Json.t =>
  object_(list{
    ("handler", PT.Handler.encode(params.handler)),
    ("trace_id", traceID(params.traceID)),
    ("trace_data", traceData(params.traceData)),
    ("dbs", list(PT.DB.encode, params.dbs)),
    ("user_fns", list(PT.UserFunction.encode, params.userFns)),
    ("user_tipes", list(PT.UserType.encode, params.userTipes)),
    ("secrets", list(secret, params.secrets)),
  })

and performFunctionAnalysisParams = (params: Types.performFunctionAnalysisParams): Js.Json.t =>
  object_(list{
    ("func", PT.UserFunction.encode(params.func)),
    ("trace_id", traceID(params.traceID)),
    ("trace_data", traceData(params.traceData)),
    ("dbs", list(PT.DB.encode, params.dbs)),
    ("user_fns", list(PT.UserFunction.encode, params.userFns)),
    ("user_tipes", list(PT.UserType.encode, params.userTipes)),
    ("secrets", list(secret, params.secrets)),
  })

and performAnalysisParams = (params: Types.performAnalysisParams): Js.Json.t => {
  let ev = variant
  switch params {
  | AnalyzeHandler(h) => ev("AnalyzeHandler", list{performHandlerAnalysisParams(h)})
  | AnalyzeFunction(h) => ev("AnalyzeFunction", list{performFunctionAnalysisParams(h)})
  }
}

and functionResult = (fr: Types.functionResult): Js.Json.t =>
  list(
    identity,
    list{
      string(fr.fnName),
      id(fr.callerID),
      string(fr.argHash),
      int(fr.argHashVersion),
      ocamlDval(fr.value),
    },
  )

and traceID = string

and traceData = (t: Types.traceData): Js.Json.t =>
  object_(list{
    ("input", list(tuple2(string, ocamlDval), Belt.Map.String.toList(t.input))),
    ("timestamp", string(t.timestamp)),
    ("function_results", list(functionResult, t.functionResults)),
  })

and trace = (t: Types.trace): Js.Json.t => {
  let data = v => Result.map(~f=traceData, v) |> Result.toOption |> Option.unwrap(~default=null)

  pair(traceID, data, t)
}

let fof = (fof: Types.fourOhFour): Js.Json.t =>
  object_(list{
    ("space", string(fof.space)),
    ("path", string(fof.path)),
    ("modifier", string(fof.modifier)),
  })

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
