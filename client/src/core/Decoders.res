open Prelude
open Json.Decode

// Dark
module TL = Toplevel
module RT = Runtime

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
  if Util.Regex.exactly(~re="[" ++ (rfc4648_section5_alphabet ++ ("=" ++ "]*")), str) {
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

let pos = (j): pos => {x: field("x", int, j), y: field("y", int, j)}

let vPos = (j): vPos => {vx: field("vx", int, j), vy: field("vy", int, j)}

let blankOr = d =>
  variants(list{
    ("Filled", variant2((id, v) => F(id, v), id, d)),
    ("Blank", variant1(id => Blank(id), id)),
    ("Partial", variant2((id, _) => Blank(id), id, string)),
  })

let traceID = (j): traceID => string(j)

let jsDate = (j): Js.Date.t => Js.Date.fromString(string(j))

let sendToRail = j => {
  let dv0 = variant0
  variants(
    list{("Rail", dv0(ProgramTypes.Expr.Rail)), ("NoRail", dv0(ProgramTypes.Expr.NoRail))},
    j,
  )
}

let rec fluidPattern = (j): FluidPattern.t => {
  module P = ProgramTypes.Pattern
  let dp = fluidPattern
  let dv4 = variant4
  let dv3 = variant3
  let dv2 = variant2
  variants(
    list{
      ("FPVariable", dv3((_, b, c) => P.PVariable(b, c), id, id, string)),
      ("FPConstructor", dv4((_, b, c, d) => P.PConstructor(b, c, d), id, id, string, list(dp))),
      (
        "FPInteger",
        dv3((_, b, c) => P.PInteger(b, c), id, id, i => i |> string |> Int64.of_string),
      ),
      ("FPBool", dv3((_, b, c) => P.PBool(b, c), id, id, bool)),
      (
        "FPString",
        recordVariant3(
          (_, patternID, str) => P.PString(patternID, str),
          ("matchID", id),
          ("patternID", id),
          ("str", string),
        ),
      ),
      ("FPFloat", dv4((_, id2, whole, fraction) => {
          let (sign, whole) = if String.startsWith(~prefix="-", whole) {
            (ProgramTypes.Negative, String.dropLeft(~count=1, whole))
          } else {
            (ProgramTypes.Positive, whole)
          }
          P.PFloat(id2, sign, whole, fraction)
        }, id, id, string, string)),
      ("FPNull", dv2((_, b) => P.PNull(b), id, id)),
      ("FPBlank", dv2((_, b) => P.PBlank(b), id, id)),
    },
    j,
  )
}

let rec fluidExpr = (j: Js.Json.t): FluidExpression.t => {
  module E = ProgramTypes.Expr
  let de = fluidExpr
  let dv5 = variant5
  let dv4 = variant4
  let dv3 = variant3
  let dv2 = variant2
  let dv1 = variant1
  variants(
    list{
      ("EInteger", dv2((x, y) => E.EInteger(x, y), id, int64)),
      ("EBool", dv2((x, y) => E.EBool(x, y), id, bool)),
      ("EString", dv2((x, y) => E.EString(x, y), id, string)),
      ("EFloat", dv3((x, whole, fraction) => {
          let (sign, whole) = if String.startsWith(~prefix="-", whole) {
            (ProgramTypes.Negative, String.dropLeft(~count=1, whole))
          } else {
            (ProgramTypes.Positive, whole)
          }
          E.EFloat(x, sign, whole, fraction)
        }, id, string, string)),
      ("ENull", dv1(x => E.ENull(x), id)),
      ("EBlank", dv1(x => E.EBlank(x), id)),
      ("ELet", dv4((a, b, c, d) => E.ELet(a, b, c, d), id, string, de, de)),
      ("EIf", dv4((a, b, c, d) => E.EIf(a, b, c, d), id, de, de, de)),
      ("EBinOp", dv5((a, b, c, d, e) => E.EBinOp(a, b, c, d, e), id, string, de, de, sendToRail)),
      ("ELambda", dv3((a, b, c) => E.ELambda(a, b, c), id, list(pair(id, string)), de)),
      ("EFieldAccess", dv3((a, b, c) => E.EFieldAccess(a, b, c), id, de, string)),
      ("EVariable", dv2((x, y) => E.EVariable(x, y), id, string)),
      ("EFnCall", dv4((a, b, c, d) => E.EFnCall(a, b, c, d), id, string, list(de), sendToRail)),
      ("EPartial", dv3((a, b, c) => E.EPartial(a, b, c), id, string, de)),
      ("ELeftPartial", dv3((a, b, c) => E.ELeftPartial(a, b, c), id, string, de)),
      ("ERightPartial", dv3((a, b, c) => E.ERightPartial(a, b, c), id, string, de)),
      ("EList", dv2((x, y) => E.EList(x, y), id, list(de))),
      ("ETuple", dv4((x, y1, y2, yRest) => E.ETuple(x, y1, y2, yRest), id, de, de, list(de))),
      ("ERecord", dv2((x, y) => E.ERecord(x, y), id, list(pair(string, de)))),
      ("EPipe", dv2((x, y) =>
          switch y {
          | list{} =>
            let (e1, e2) = recover(
              "decoding a pipe with no exprs",
              ~debug=x,
              (E.EBlank(gid()), E.EBlank(gid())),
            )
            E.EPipe(x, e1, e2, list{})
          | list{e1} =>
            let e2 = recover("decoding a pipe with only one expr", ~debug=x, E.EBlank(gid()))
            E.EPipe(x, e1, e2, list{})
          | list{e1, e2, ...rest} => E.EPipe(x, e1, e2, rest)
          }
        , id, list(de))),
      ("EConstructor", dv3((a, b, c) => E.EConstructor(a, b, c), id, string, list(de))),
      ("EMatch", dv3((a, b, c) => E.EMatch(a, b, c), id, de, list(pair(fluidPattern, de)))),
      ("EPipeTarget", dv1(a => E.EPipeTarget(a), id)),
      (
        "EFeatureFlag",
        dv5((a, b, c, d, e) => E.EFeatureFlag(a, b, c, d, e), id, string, de, de, de),
      ),
    },
    j,
  )
}

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
      ("PParamTipe", dv1(x => PParamTipe(x), blankOr(DType.decodeOld))),
      ("PTypeFieldName", dv1(x => PTypeFieldName(x), blankOr(string))),
      ("PTypeFieldTipe", dv1(x => PTypeFieldTipe(x), blankOr(DType.decodeOld))),
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
    body: field("body", fluidExpr, j),
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

and exeState = (j): exeState =>
  j |> variants(list{
    ("Idle", variant0(Idle)),
    ("Executing", variant0(Executing)),
    ("Complete", variant0(Complete)),
  })

and handlerProp = (j): handlerProp => {
  hoveringReferences: field("hoveringReferences", list(id), j),
  execution: field("executing", exeState, j),
}

and savedUserSettings = (j: Js.Json.t): savedUserSettings => {
  let oldFirstVisitToDark = withDefault(
    Defaults.defaultUserSettings.firstVisitToDark,
    field("showUserWelcomeModal", bool),
    j,
  )

  let newFirstVisitToDark = withDefault(
    Defaults.defaultUserSettings.firstVisitToDark,
    field("firstVisitToDark", bool),
    j,
  )

  {
    firstVisitToDark: oldFirstVisitToDark || newFirstVisitToDark,
    recordConsent: withDefault(None, field("recordConsent", optional(bool)), j),
  }
}

and sidebarMode = (j: Js.Json.t): sidebarMode =>
  j |> variants(list{
    ("DetailedMode", variant0(DetailedMode)),
    ("AbridgedMode", variant0(AbridgedMode)),
  })

and tutorialStep = (j: Js.Json.t): tutorialStep =>
  j |> variants(list{
    ("Welcome", variant0(Welcome)),
    ("VerbChange", variant0(VerbChange)),
    ("ReturnValue", variant0(ReturnValue)),
    ("OpenTab", variant0(OpenTab)),
    ("GettingStarted", variant0(GettingStarted)),
  })

and sidebarState = (j: Js.Json.t): sidebarState => {
  mode: field("mode", sidebarMode, j),
  openedCategories: withDefault(Set.String.empty, field("openedCategories", strSet), j),
}

and savedSettings = (
  j: Js.Json.t,
): savedSettings => /* always use withDefault or optional because the field might be missing due
 * to old editors or new fields. */
{
  editorSettings: {
    runTimers: withDefault(true, field("editorSettings", field("runTimers", bool)), j),
    showHandlerASTs: withDefault(false, field("editorSettings", field("showHandlerASTs", bool)), j),
    showFluidDebugger: withDefault(
      false,
      field("editorSettings", field("showFluidDebugger", bool)),
      j,
    ),
  },
  cursorState: withDefault(Deselected, field("cursorState", cursorState), j),
  tlTraceIDs: withDefault(TLID.Dict.empty, field("tlTraceIDs", tlidDict(traceID)), j),
  featureFlags: withDefault(Map.String.empty, field("featureFlags", strDict(bool)), j),
  handlerProps: withDefault(TLID.Dict.empty, field("handlerProps", tlidDict(handlerProp)), j),
  canvasPos: withDefault(Defaults.origin, field("canvasPos", pos), j),
  lastReload: optional(field("lastReload", jsDate), j),
  sidebarState: withDefault(Defaults.defaultSidebar, field("sidebarState", sidebarState), j),
  showTopbar: withDefault(Defaults.defaultSavedSettings.showTopbar, field("showTopbar1", bool), j),
  firstVisitToThisCanvas: withDefault(
    Defaults.defaultSavedSettings.firstVisitToThisCanvas,
    field("firstVisitToThisCanvas", bool),
    j,
  ),
  userTutorial: withDefault(
    Defaults.defaultSavedSettings.userTutorial,
    field("userTutorial", optional(tutorialStep)),
    j,
  ),
  userTutorialTLID: withDefault(
    Defaults.defaultSavedSettings.userTutorialTLID,
    field("userTutorialTLID", tlidOption),
    j,
  ),
}

and cursorState = (j: Js.Json.t): cursorState => {
  let dv0 = variant0
  let dv1 = variant1
  let dv2 = variant2
  let dv3 = variant3
  let dv4 = variant4
  variants(
    list{
      ("Selecting", dv2((a, b) => Selecting(a, b), tlid, optional(id))),
      ("Omnibox", dv1(x => Omnibox(x), optional(pos))),
      ("Entering", dv2((x, y) => Entering(x, y), tlid, id)),
      (
        "Dragging" /* Deprecated via DraggingTL */,
        dv4((a, b, c, d) => DraggingTL(a, b, c, d), tlid, vPos, bool, cursorState),
      ),
      ("DraggingTL", dv4((a, b, c, d) => DraggingTL(a, b, c, d), tlid, vPos, bool, cursorState)),
      (
        "PanningCanvas",
        /* TODO: There's a danger of mismatching the encoder order here because we're using an inline record.
         * An order-independent encoding would alleviate this. */
        dv3((viewportStart, viewportCurr, prevCursorState) => PanningCanvas({
          viewportStart: viewportStart,
          viewportCurr: viewportCurr,
          prevCursorState: prevCursorState,
        }), vPos, vPos, cursorState),
      ),
      ("Deselected", dv0(Deselected)) /* Old value */,
      ("SelectingCommand", dv2((a, b) => Selecting(a, Some(b)), tlid, id)),
      ("FluidEntering", dv1(a => FluidEntering(a), tlid)),
      ("FluidMouseSelecting", dv1(a => FluidEntering(a), tlid)),
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

let handlerSpec = (j): handlerSpec => {
  space: field("module", blankOr(string), j),
  name: field("name", blankOr(string), j),
  modifier: field("modifier", blankOr(string), j),
}

let handler = (pos, j): handler => {
  ast: field("ast", j => fluidExpr(j) |> FluidAST.ofExpr, j),
  spec: field("spec", handlerSpec, j),
  hTLID: field("tlid", tlid, j),
  pos: pos,
}

let tipeString = (j): string => map(RT.tipe2str, DType.decodeOld, j)

let dbColList = (j): list<dbColumn> => list(tuple2(blankOr(string), blankOr(tipeString)), j)

let dbmColList = (j): list<dbColumn> => list(tuple2(blankOr(string), blankOr(string)), j)

let db = (pos, j): db => {
  dbTLID: field("tlid", tlid, j),
  dbName: field("name", blankOr(string), j),
  cols: field("cols", dbColList, j),
  version: field("version", int, j),
  pos: pos,
}

let toplevel = (j): toplevel => {
  let pos = field("pos", pos, j)
  let variant = variants(list{
    ("Handler", variant1(x => TLHandler(x), handler(pos))),
    ("DB", variant1(x => TLDB(x), db(pos))),
  })

  field("data", variant, j)
}

let userFunctionParameter = (j): userFunctionParameter => {
  ufpName: field("name", blankOr(string), j),
  ufpTipe: field("tipe", blankOr(DType.decodeOld), j),
  ufpBlock_args: field("block_args", list(string), j),
  ufpOptional: field("optional", bool, j),
  ufpDescription: field("description", string, j),
}

let userFunctionMetadata = (j): userFunctionMetadata => {
  ufmName: field("name", blankOr(string), j),
  ufmParameters: field("parameters", list(userFunctionParameter), j),
  ufmDescription: field("description", string, j),
  ufmReturnTipe: field("return_type", blankOr(DType.decodeOld), j),
  ufmInfix: field("infix", bool, j),
}

let userFunction = (j): userFunction => {
  ufTLID: field("tlid", tlid, j),
  ufMetadata: field("metadata", userFunctionMetadata, j),
  ufAST: field("ast", fluidExpr, j) |> FluidAST.ofExpr,
}

let packageFnParameter = (j: Js.Json.t): Types.packageFnParameter => {
  name: field("name", string, j),
  tipe: field("tipe", DType.decodeOld, j),
  description: field("description", string, j),
}

let packageFn = (j: Js.Json.t): Types.packageFn => {
  user: field("user", string, j),
  package: field("package", string, j),
  module_: field("module", string, j),
  fnname: field("fnname", string, j),
  version: field("version", int, j),
  body: field("body", fluidExpr, j),
  parameters: field("parameters", list(packageFnParameter), j),
  return_type: field("return_type", DType.decodeOld, j),
  description: field("description", string, j),
  author: field("author", string, j),
  deprecated: field("deprecated", bool, j),
  pfTLID: field("tlid", tlid, j),
}

let fof = (j): fourOhFour => {
  space: index(0, string, j),
  path: index(1, string, j),
  modifier: index(2, string, j),
  timestamp: index(3, string, j),
  traceID: index(4, traceID, j),
}

let secret = (j): SecretTypes.t => {
  secretName: field("secret_name", string, j),
  secretValue: field("secret_value", string, j),
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

let presenceMsg = (j): avatar => {
  canvasId: field("canvasId", string, j),
  canvasName: field("canvasName", string, j),
  tlid: field("tlid", optional(string), j),
  username: field("username", string, j),
  serverTime: field("serverTime", serverTime, j),
  email: field("email", string, j),
  fullname: field("name", optional(string), j),
  browserId: field("browserId", string, j),
}

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

let userRecordField = j => {
  urfName: field("name", blankOr(string), j),
  urfTipe: field("tipe", blankOr(DType.decodeOld), j),
}

let userTipeDefinition = j =>
  variants(list{("UTRecord", variant1(x => UTRecord(x), list(userRecordField)))}, j)

let userTipe = j => {
  utTLID: field("tlid", tlid, j),
  utName: field("name", blankOr(string), j),
  utVersion: field("version", int, j),
  utDefinition: field("definition", userTipeDefinition, j),
}

let permission = j =>
  variants(list{("Read", variant0(Read)), ("ReadWrite", variant0(ReadWrite))}, j)

let op = (j): op =>
  variants(
    list{
      (
        "SetHandler",
        variant3(
          (t, p, h) => SetHandler(t, p, {...h, pos: p}),
          tlid,
          pos,
          handler({x: -1286, y: -467}),
        ),
      ),
      ("CreateDB", variant3((t, p, name) => CreateDB(t, p, name), tlid, pos, string)),
      ("AddDBCol", variant3((t, cn, ct) => AddDBCol(t, cn, ct), tlid, id, id)),
      ("SetDBColName", variant3((t, i, name) => SetDBColName(t, i, name), tlid, id, string)),
      ("ChangeDBColName", variant3((t, i, name) => ChangeDBColName(t, i, name), tlid, id, string)),
      ("SetDBColType", variant3((t, i, tipe) => SetDBColType(t, i, tipe), tlid, id, string)),
      ("ChangeDBColType", variant3((t, i, tipe) => ChangeDBColName(t, i, tipe), tlid, id, string)),
      ("DeleteDBCol", variant2((t, i) => DeleteDBCol(t, i), tlid, id)),
      ("TLSavepoint", variant1(t => TLSavepoint(t), tlid)),
      ("UndoTL", variant1(t => UndoTL(t), tlid)),
      ("RedoTL", variant1(t => RedoTL(t), tlid)),
      ("DeleteTL", variant1(t => DeleteTL(t), tlid)),
      ("MoveTL", variant2((t, p) => MoveTL(t, p), tlid, pos)),
      ("SetFunction", variant1(uf => SetFunction(uf), userFunction)),
      ("DeleteFunction", variant1(t => DeleteFunction(t), tlid)),
      ("SetExpr", variant3((t, i, e) => SetExpr(t, i, e), tlid, id, fluidExpr)),
      ("RenameDBname", variant2((t, name) => RenameDBname(t, name), tlid, string)),
      (
        "CreateDBWithBlankOr",
        variant4((t, p, i, name) => CreateDBWithBlankOr(t, p, i, name), tlid, pos, id, string),
      ),
      ("SetType", variant1(t => SetType(t), userTipe)),
      ("DeleteType", variant1(t => DeleteType(t), tlid)),
    },
    j,
  )

let addOpAPIResult = (j): addOpAPIResult => {
  let tls = field("toplevels", list(toplevel), j)
  let dtls = field("deleted_toplevels", list(toplevel), j)
  {
    handlers: List.filterMap(~f=TL.asHandler, tls),
    deletedHandlers: List.filterMap(~f=TL.asHandler, dtls),
    dbs: List.filterMap(~f=TL.asDB, tls),
    deletedDBs: List.filterMap(~f=TL.asDB, dtls),
    userFunctions: field("user_functions", list(userFunction), j),
    deletedUserFunctions: field("deleted_user_functions", list(userFunction), j),
    userTipes: field("user_tipes", list(userTipe), j),
    deletedUserTipes: field("deleted_user_tipes", list(userTipe), j),
  }
}

let addOpAPI = (j: Js.Json.t): addOpAPIResponse => {result: field("result", addOpAPIResult, j)}

let addOpAPIParams = (j): addOpAPIParams => {
  ops: field("ops", list(op), j),
  opCtr: field("opCtr", int, j),
  clientOpCtrId: field("clientOpCtrId", string)(j),
}

let addOpAPIPusherMsg = (j: Js.Json.t): addOpPusherMsg => {
  result: field("result", addOpAPIResult, j),
  params: field("params", addOpAPIParams, j),
}

let getUnlockedDBsAPIResult = (j): getUnlockedDBsAPIResult =>
  j |> field("unlocked_dbs", list(tlid)) |> TLID.Set.fromList

let get404sAPIResult = (j): get404sAPIResult => j |> field("f404s", list(fof))

let insertSecretResult = (j): list<SecretTypes.t> => j |> field("secrets", list(secret))

let getTraceDataAPIResult = (j): getTraceDataAPIResult => {trace: field("trace", trace, j)}

let dbStats = (j): dbStats => {
  count: field("count", int, j),
  example: field("example", optional(tuple2(ocamlDval, string)), j),
}

let dbStatsStore = (j): dbStatsStore => strDict(dbStats, j)

let dbStatsAPIResult = j => dbStatsStore(j)

let account = (j): account => {
  name: field("name", string, j),
  email: field("email", string, j),
  username: field("username", string, j),
}

/* schedule is None here but gets updated when we create a view state
 * see createVS in ViewUtils.ml for details */
let workerStats = (j): workerStats => {count: field("count", int, j), schedule: None}

let workerStatsAPIResult = j => workerStats(j)

let updateWorkerScheduleAPIResult = (j): Map.String.t<string> => strDict(string)(j)

let initialLoadAPIResult = (j): initialLoadAPIResult => {
  let tls = field("toplevels", list(toplevel), j)
  let dtls = field("deleted_toplevels", list(toplevel), j)
  {
    handlers: List.filterMap(~f=TL.asHandler, tls),
    deletedHandlers: List.filterMap(~f=TL.asHandler, dtls),
    dbs: List.filterMap(~f=TL.asDB, tls),
    deletedDBs: List.filterMap(~f=TL.asDB, dtls),
    userFunctions: field("user_functions", list(userFunction), j),
    deletedUserFunctions: field("deleted_user_functions", list(userFunction), j),
    unlockedDBs: j |> field("unlocked_dbs", list(tlid)) |> TLID.Set.fromList,
    staticDeploys: field("assets", list(sDeploy), j),
    userTipes: field("user_tipes", list(userTipe), j),
    deletedUserTipes: field("deleted_user_tipes", list(userTipe), j),
    opCtrs: j
    |> withDefault(list{}, field("op_ctrs", list(tuple2(string, int))))
    |> Map.String.fromList,
    permission: field("permission", optional(permission), j),
    account: field("account", account, j),
    canvasList: field("canvas_list", list(string), j),
    orgs: field("orgs", list(string), j),
    orgCanvasList: field("org_canvas_list", list(string), j),
    workerSchedules: field("worker_schedules", strDict(string), j),
    secrets: field("secrets", list(secret), j),
    creationDate: field("creation_date", jsDate, j),
  }
}

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

let loadPackagesAPIResult = (j): loadPackagesAPIResult => list(packageFn, j)

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

@ocaml.doc(" [clickEvent fn] implements a decoder converting a javascript mouse event
 * into an OCaml record of type mouseEvent.
 *
 * Example usage:
 *
 *  let constructor = (fun mouseEvent -> AppMouseDown mouseEvent) in
 *  Tea.Html.onWithOptions
 *   ~key
 *   event
 *   {stopPropagation = true; preventDefault = true}
 *   (Decoders.wrapDecoder (Decoders.clickEvent constructor))
 ")
let clickEvent = (fn: mouseEvent => 'a, j): 'a =>
  fn({
    mePos: /* We decode floats b/c newer Chromes may use floats instead of ints; we
     * then truncate rather than moving to floats everywhere due to concerns
     * about sending data back to browsers whose DOMs don't support float
     * positions - see https://github.com/darklang/dark/pull/2016 for
     * discussion, and
     * https://drafts.csswg.org/cssom-view/#extensions-to-the-window-interface
     * for the spec */
    {
      vx: field("pageX", Json.Decode.float, j) |> truncate,
      vy: field("pageY", Json.Decode.float, j) |> truncate,
    },
    button: field("button", int, j),
    ctrlKey: field("ctrlKey", bool, j),
    shiftKey: field("shiftKey", bool, j),
    altKey: field("altKey", bool, j),
    detail: field("detail", int, j),
  })

@ocaml.doc(" [scrollEvent fn] implements a decoder converting a javascript scroll event
 * into an OCaml record of type scrollEvent.
 *
 * Example usage:
 *
 *  let constructor = (fun _scrollEvent -> AppScroll) in
 *  Tea.Html.onWithOptions
 *   ~key
 *   event
 *   {stopPropagation = true; preventDefault = true}
 *   (Decoders.wrapDecoder (Decoders.scrollEvent constructor))
 ")
let scrollEvent = (fn: scrollEvent => 'a, j): 'a =>
  fn({timeStamp: field("timeStamp", Json.Decode.float, j)})

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
