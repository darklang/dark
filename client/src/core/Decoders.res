open Prelude
open Json.Decode

// Dark

@val external stringify: Js.Json.t => string = "JSON.stringify"

/* external jsGetFluidSelectionRange :
  unit -> int array Js.Nullable.t
  = "getFluidSelectionRange"
  [@@bs.val] [@@bs.scope "window"] */

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

let pos = Pos.decode

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
