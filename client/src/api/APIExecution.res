module PT = ProgramTypes
module RT = RuntimeTypes

// Used to manually execute some Function or Handler
// Note: the decoders are only needed for round-tripping tests

module Function = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      tlid: TLID.t,
      traceID: TraceID.t,
      callerID: ID.t,
      args: list<RT.Dval.t>,
      fnName: string,
    }

    let decode = (j): t => {
      open Json_decode_extended
      {
        tlid: field("tlid", TLID.decode, j),
        traceID: field("trace_id", TraceID.decode, j),
        callerID: field("caller_id", ID.decode, j),
        args: field("args", list(RT.Dval.decode), j),
        fnName: field("fnname", string, j),
      }
    }
    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("tlid", TLID.encode(params.tlid)),
        ("trace_id", TraceID.encode(params.traceID)),
        ("caller_id", ID.encode(params.callerID)),
        ("args", list(RT.Dval.encode, params.args)),
        ("fnname", string(params.fnName)),
      })
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = (RT.Dval.t, string, int, list<TLID.t>, TLID.Set.t)

  let decode = (j): t => {
    open Json_decode_extended
    (
      field("result", RT.Dval.decode, j),
      field("hash", string, j),
      field("hashVersion", int, j),
      field("touched_tlids", list(TLID.decode), j),
      field("unlocked_dbs", list(TLID.decode), j) |> TLID.Set.fromList,
    )
  }
  let encode = ((result_, hash, hashVersion, touched_tlids, unlocked_dbs): t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("result", RT.Dval.encode(result_)),
      ("hash", string(hash)),
      ("hashVersion", int(hashVersion)),
      ("touched_tlids", list(TLID.encode, touched_tlids)),
      ("unlocked_dbs", list(TLID.encode, Belt.Set.toList(unlocked_dbs))),
    })
  }
}

module Handler = {
  module Params = {
    type rec t = {
      tlid: TLID.t,
      traceID: TraceID.t,
      input: AnalysisTypes.InputValueDict.t,
    }

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("tlid", TLID.encode(params.tlid)),
        ("trace_id", string(params.traceID)),
        ("input", list(tuple2(string, RT.Dval.encode), Belt.Map.String.toList(params.input))),
      })
    }
    let decode = (j): t => {
      open Json_decode_extended
      {
        tlid: field("tlid", TLID.decode, j),
        traceID: field("trace_id", string, j),
        input: field(
          "input",
          array(tuple2(string, RT.Dval.decode)),
          j,
        ) |> Belt.Map.String.fromArray,
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = list<TLID.t>
  let decode = (j): t => {
    open Json_decode_extended
    field("touched_tlids", list(TLID.decode), j)
  }

  let encode = (tlids: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("touched_tlids", list(TLID.encode, tlids))})
  }
}
