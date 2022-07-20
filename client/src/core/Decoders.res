open Prelude
open Json.Decode

// Dark

@val external stringify: Js.Json.t => string = "JSON.stringify"

// This and tuple5 are adapted from ReScript - see tuple4 for the original
external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"

let tuple5 = (decodeA, decodeB, decodeC, decodeD, decodeE, json) =>
  if Js.Array.isArray(json) {
    let source: array<Js.Json.t> = Obj.magic((json: Js.Json.t))
    let length = Js.Array.length(source)
    if length == 5 {
      try (
        decodeA(unsafe_get(source, 0)),
        decodeB(unsafe_get(source, 1)),
        decodeC(unsafe_get(source, 2)),
        decodeD(unsafe_get(source, 3)),
        decodeE(unsafe_get(source, 4)),
      ) catch {
      | DecodeError(msg) => \"@@"(raise, DecodeError(msg ++ "\n\tin tuple5"))
      }
    } else {
      \"@@"(raise, DecodeError(j`Expected array of length 5, got array of length $length`))
    }
  } else {
    \"@@"(raise, DecodeError("Expected array, got not-an-array"))
  }

/* external jsGetFluidSelectionRange :
  unit -> int array Js.Nullable.t
  = "getFluidSelectionRange"
  [@@bs.val] [@@bs.scope "window"] */

// XXX(JULIAN): All of this should be cleaned up and moved somewhere nice!
@deriving(abstract) type jsArrayBuffer = {byteLength: int}

@deriving(abstract) type jsUint8Array

@new external createUint8Array: jsArrayBuffer => jsUint8Array = "Uint8Array"

@get_index external getUint8ArrayIdx: (jsUint8Array, int) => int = ""

@set_index external setUint8ArrayIdx: (jsUint8Array, int, int) => unit = ""

// Note: unsafe. Wrap in bytes_from_base64url, which validates the input
let dark_arrayBuffer_from_b64url = %raw(`
  function (base64) {
    // Modified version of https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js
    // Note that this version uses the url and filename safe alphabet instead of the standard b64 alphabet.
    // TODO(JULIAN): Figure out how to hoist the \`lookup\` definition out of the function,
    // since it's shared and could be cached.
    var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

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

    return arraybuffer;
  }
`)

let _bytes_from_uint8Array = (input: jsArrayBuffer): Bytes.t => {
  let len = byteLengthGet(input)
  let bytes = Bytes.create(len)
  let reader = createUint8Array(input)
  for i in 0 to len - 1 {
    let char = getUint8ArrayIdx(reader, i)
    Bytes.unsafe_set(bytes, i, char_of_int(char))
  }
  bytes
}

exception Invalid_B64(string)

let valid_rfc4648_b64_or_exn = (str: string) => {
  let rfc4648_section5_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789\\-_"

  // '=' isn't in the alphabet, but we allow it as padding
  if Regex.exactly(~re="[" ++ (rfc4648_section5_alphabet ++ ("=" ++ "]*")), str) {
    str
  } else {
    raise(Invalid_B64("Expected B64 input matching RFC4648 alphabet."))
  }
}

let bytes_from_base64url = (b64: string): Bytes.t =>
  b64 |> valid_rfc4648_b64_or_exn |> dark_arrayBuffer_from_b64url |> _bytes_from_uint8Array

let int64 = (j: Js.Json.t) =>
  if Js.typeof(j) == "string" {
    Int64.of_string(string(j))
  } else {
    // We use `float` as `int` is 32bit, and can't handle the space between 2^32 and
    // 2^53. `float` here can be considered to be `int53`, since we know we're
    // getting whole integers as anything that doesn't fit in the integer portion of
    // a float is expected to be encoded as a string
    Int64.of_float(Json.Decode.float(j))
  }

let id = ID.decode
let tlid = TLID.decode
let tlidOption = (j: Js.Json.t): option<TLID.t> => optional(tlid, j)
let tlidDict = TLID.Dict.decode
let idMap = ID.Map.decode

let pos = BaseTypes.decodePos

let blankOr = d =>
  variants(list{
    ("Filled", variant2((id, v) => F(id, v), id, d)),
    ("Blank", variant1(id => Blank(id), id)),
    ("Partial", variant2((id, _) => Blank(id), id, string)),
  })

let traceID = (j): traceID => string(j)

let jsDate = (j): Js.Date.t => Js.Date.fromString(string(j))

let blankOrData = (j): blankOrData => {
  let dv1 = variant1
  variants(
    list{
      ("PEventName", dv1(x => PEventName(x), blankOr(string))),
      ("PEventSpace", dv1(x => PEventSpace(x), blankOr(string))),
      ("PEventModifier", dv1(x => PEventModifier(x), blankOr(string))),
      ("PDBName", dv1(x => PDBName(x), blankOr(string))),
      ("PDBColName", dv1(x => PDBColName(x), blankOr(string))),
      ("PDBColType", dv1(x => PDBColType(x), blankOr(string))),
      ("PFnName", dv1(x => PFnName(x), blankOr(string))),
      ("PParamName", dv1(x => PParamName(x), blankOr(string))),
      ("PParamTipe", dv1(x => PParamTipe(x), blankOr(DType.decode))),
      ("PTypeFieldName", dv1(x => PTypeFieldName(x), blankOr(string))),
      ("PTypeFieldTipe", dv1(x => PTypeFieldTipe(x), blankOr(DType.decode))),
    },
    j,
  )
}

let rec ocamlDval = (j): dval => {
  let dv0 = variant0
  let dv1 = variant1
  let dv2 = variant2
  let dv3 = variant3
  let dd = ocamlDval

  let optionT = variants(list{
    ("OptJust", dv1(x => OptJust(x), dd)),
    ("OptNothing", dv0(OptNothing)),
  })

  let resultT = variants(list{
    ("ResOk", dv1(x => ResOk(x), dd)),
    ("ResError", dv1(x => ResError(x), dd)),
  })

  let dhttp = variants(list{
    ("Redirect", dv1(x => Redirect(x), string)),
    ("Response", dv2((a, b) => Response(a, b), int, list(tuple2(string, string)))),
  })

  let srcT = variants(list{
    ("SourceNone", dv0(SourceNone)),
    ("SourceId", dv2((x, y) => SourceId(x, y), tlid, id)),
  })

  let dblock_args = j => {
    params: field("params", list(pair(id, string)), j),
    body: field("body", PT.Expr.decode, j),
    symtable: field("symtable", beltStrDict(ocamlDval), j),
  }

  let encodedFloat = j =>
    /* We can sometimes get infinity/NaN in analysis results. While the Dark
     * language isn't intended to have them, we haven't actually managed to
     * eradicate them from the runtime, and so we'll sometimes get the from the
     * analysis code.  However, since JSON doesn't support Infinity/NaN, our
     * JSON parser crashes. So instead we encode them specially in the backend,
     * with quotes, e.g. "NaN".
     *
     * CLEANUP remove OCaml-specific parsing here.
     * The checks within `Some` are for OCaml analysis, while the ones under `None` are
     * for F#.
     */
    switch Js.Json.decodeObject(j) {
    | Some(obj) =>
      if (
        Js_dict.get(obj, "type") == Some(Js.Json.string("float")) &&
          Js_dict.get(obj, "value") == Some(Js.Json.string("NaN"))
      ) {
        Float.nan
      } else if (
        Js_dict.get(obj, "type") == Some(Js.Json.string("float")) &&
          Js_dict.get(obj, "value") == Some(Js.Json.string("Infinity"))
      ) {
        Float.infinity
      } else {
        \"@@"(raise, DecodeError("Expected float, got " ++ stringify(j)))
      }
    | None =>
      if j == Js.Json.string("Infinity") {
        Float.infinity
      } else if j == Js.Json.string("-Infinity") {
        Float.negativeInfinity
      } else if j == Js.Json.string("NaN") {
        Float.nan
      } else {
        Json.Decode.float(j)
      }
    }

  variants(
    list{
      ("DInt", dv1(x => DInt(x), int64)),
      ("DFloat", dv1(x => DFloat(x), encodedFloat)),
      ("DBool", dv1(x => DBool(x), bool)),
      ("DNull", dv0(DNull)),
      ("DCharacter", dv1(x => DCharacter(x), string)),
      ("DStr", dv1(x => DStr(x), string)),
      ("DList", dv1(x => DList(x), array(dd))),
      ("DTuple", dv3((first, second, theRest) => DTuple(first, second, theRest), dd, dd, list(dd))),
      ("DObj", dv1(x => DObj(x), beltStrDict(dd))),
      (
        "DIncomplete",
        /* catch decoding errors for backwards compatibility. if you see this
         * comment in main branch, the withDefault can be removed */
        withDefault(DIncomplete(SourceNone), dv1(x => DIncomplete(x), srcT)),
      ),
      (
        "DError",
        tryDecode2(
          dv1(x => DError(SourceNone, x), string),
          dv1(((i, msg)) => DError(i, msg), tuple2(srcT, string)),
        ),
      ),
      ("DBlock", dv1(x => DBlock(x), dblock_args)),
      ("DErrorRail", dv1(x => DErrorRail(x), dd)),
      ("DResp", dv1(((h, dv)) => DResp(h, dv), tuple2(dhttp, dd))),
      ("DDB", dv1(x => DDB(x), string)),
      ("DDate", dv1(x => DDate(x), string)),
      ("DPassword", dv1(x => DPassword(x), string)),
      ("DUuid", dv1(x => DUuid(x), string)),
      ("DOption", dv1(x => DOption(x), optionT)),
      ("DResult", dv1(x => DResult(x), resultT)),
      ("DBytes", dv1(x => {
          let x = x |> bytes_from_base64url
          DBytes(x)
        }, string)),
    },
    j,
  )
}

//
// and entering j =
// let dv1 = variant1 in
// let dv2 = variant2 in
// variants
// [ ( "Creating"
// , dv1
// (fun x -> Creating (if x = Defaults.origin then None else Some x))
// pos )
// ; ("Filling", dv2 (fun a b -> Filling (a, b)) tlid id) ]
// j
//
//
and loadable = (decoder: Js.Json.t => 'a, j: Js.Json.t): loadable<'a> =>
  variants(
    list{
      ("LoadableSuccess", variant1(a => LoadableSuccess(a), decoder)),
      ("LoadableNotInitialized", variant0(LoadableNotInitialized)),
      ("LoadableLoading", variant1(a => LoadableLoading(a), optional(decoder))),
      ("LoadableError", variant1(a => LoadableError(a), string)),
    },
    j,
  )

let executionResult = (j: Js.Json.t): executionResult =>
  variants(
    list{
      ("ExecutedResult", variant1(a => ExecutedResult(a), ocamlDval)),
      ("NonExecutedResult", variant1(a => NonExecutedResult(a), ocamlDval)),
    },
    j,
  )

let intermediateResultStore = (j: Js.Json.t): intermediateResultStore => idMap(executionResult, j)

let dvalDict = (j: Js.Json.t): dvalDict => strDict(ocamlDval, j)

let analysisEnvelope = (j: Js.Json.t): (traceID, intermediateResultStore) =>
  tuple2(string, intermediateResultStore)(j)

// let tipeString = (j): string => map(RT.tipe2str, DType.decode, j)

let fof = (j): fourOhFour => {
  space: index(0, string, j),
  path: index(1, string, j),
  modifier: index(2, string, j),
  timestamp: index(3, string, j),
  traceID: index(4, traceID, j),
}

let deployStatus = (j): deployStatus => {
  let sumtypes = list{("Deployed", variant0(Deployed)), ("Deploying", variant0(Deploying))}

  j |> variants(sumtypes)
}

let sDeploy = (j): staticDeploy => {
  deployHash: field("deploy_hash", string, j),
  url: field("url", string, j),
  lastUpdate: field("last_update", jsDate, j),
  status: field("status", deployStatus, j),
}

let serverTime = (j): Js.Date.t => Js.Date.fromString(field("value", string, j))

let inputValueDict = (j): inputValueDict =>
  j |> array(tuple2(string, ocamlDval)) |> Belt.Map.String.fromArray

let functionResult = (j): functionResult => {
  let (fnName, callerID, argHash, argHashVersion, value) = tuple5(
    string,
    id,
    string,
    int,
    ocamlDval,
    j,
  )

  {
    fnName: fnName,
    callerID: callerID,
    argHash: argHash,
    argHashVersion: argHashVersion,
    value: value,
  }
}

let traceData = (j): traceData => {
  input: field("input", inputValueDict, j),
  timestamp: field("timestamp", string, j),
  functionResults: field("function_results", list(functionResult), j),
}

let trace = (j): trace =>
  pair(traceID, optional(traceData), j) |> (
    ((id, traceData)) =>
      switch traceData {
      | None => (id, Error(NoneYet))
      | Some(traceData) => (id, Ok(traceData))
      }
  )

let traces = (j): traces => j |> list(tuple2(TLID.decode, list(trace))) |> TLID.Dict.fromList

let getUnlockedDBsAPIResult = (j): getUnlockedDBsAPIResult =>
  j |> field("unlocked_dbs", list(tlid)) |> TLID.Set.fromList

let get404sAPIResult = (j): get404sAPIResult => j |> field("f404s", list(fof))

let getTraceDataAPIResult = (j): getTraceDataAPIResult => {trace: field("trace", trace, j)}

let dbStats = (j): dbStats => {
  count: field("count", int, j),
  example: field("example", optional(tuple2(ocamlDval, string)), j),
}

let dbStatsStore = (j): dbStatsStore => strDict(dbStats, j)

let dbStatsAPIResult = j => dbStatsStore(j)

/* schedule is None here but gets updated when we create a view state
 * see createVS in ViewUtils.ml for details */
let workerStats = (j): workerStats => {count: field("count", int, j), schedule: None}

let workerStatsAPIResult = j => workerStats(j)

let updateWorkerScheduleAPIResult = (j): Map.String.t<string> => strDict(string)(j)

let allTracesResult = (j): allTracesAPIResult => {
  traces: field("traces", list(pair(tlid, traceID)), j),
}

let executeFunctionAPIResult = (j): executeFunctionAPIResult => (
  field("result", ocamlDval, j),
  field("hash", string, j),
  field("hashVersion", int, j),
  field("touched_tlids", list(tlid), j),
  field("unlocked_dbs", list(tlid), j) |> TLID.Set.fromList,
)

let uploadFnAPIResult = (_): uploadFnAPIResult => ()

let loadPackagesAPIResult = (j): loadPackagesAPIResult => list(PT.Package.Fn.decode, j)

let triggerHandlerAPIResult = (j): triggerHandlerAPIResult => field("touched_tlids", list(tlid), j)

let saveTestAPIResult = (j): saveTestAPIResult => string(j)

// --------------------------
// Dval (some here because of cyclic dependencies)
// -------------------------

let parseBasicDval = (str): dval =>
  oneOf(
    list{
      map(x => DInt(x), int64),
      map(x => DFloat(x), Json.Decode.float),
      map(x => DBool(x), bool),
      nullAs(DNull),
      map(x => DStr(x), string),
    },
    str,
  )

// Ported directly from Dval.parse in the backend
let parseDvalLiteral = (str: string): option<dval> =>
  switch String.toList(str) {
  | list{'\'', c, '\''} => Some(DCharacter(String.fromList(list{c})))
  | list{'"', ...rest} =>
    if List.last(rest) == Some('"') {
      List.initial(rest)
      |> Option.unwrap(~default=list{})
      |> String.fromList
      |> (x => Some(DStr(x)))
    } else {
      None
    }
  | _ =>
    try Some(parseBasicDval(Json.parseOrRaise(str))) catch {
    | _ => None
    }
  }

let exception_ = (j): exception_ => {
  short: field("short", string, j),
  long: field("long", optional(string), j),
  exceptionTipe: field("tipe", string, j),
  actual: field("actual", optional(string), j),
  actualType: field("actual_tipe", optional(string), j),
  expected: field("expected", optional(string), j),
  result: field("result", optional(string), j),
  resultType: field("result_tipe", optional(string), j),
  info: field("info", strDict(string), j),
  workarounds: field("workarounds", list(string), j),
}

// Wrap JSON decoders using bs-json's format, into TEA's HTTP expectation format
let wrapExpect = (fn: Js.Json.t => 'a): (string => Tea.Result.t<'ok, string>) =>
  j =>
    try Ok(fn(Json.parseOrRaise(j))) catch {
    | e =>
      reportError("unexpected json", j)
      switch e {
      | DecodeError(e) | Json.ParseError(e) => Error(e)
      | e => Error(Printexc.to_string(e))
      }
    }

// Wrap JSON decoders using bs-json's format, into TEA's JSON decoder format
let wrapDecoder = (fn: Js.Json.t => 'a): Tea.Json.Decoder.t<Js.Json.t, 'a> => Decoder(
  value =>
    try Tea_result.Ok(fn(value)) catch {
    | e =>
      reportError("undecodable json", value)
      switch e {
      | DecodeError(e) | Json.ParseError(e) => Tea_result.Error(e)
      | e => Tea_result.Error("Json error: " ++ Printexc.to_string(e))
      }
    },
)
