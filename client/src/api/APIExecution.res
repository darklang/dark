module PT = ProgramTypes
module RT = RuntimeTypes

module Function = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      efpTLID: TLID.t,
      efpTraceID: TraceID.t,
      efpCallerID: ID.t,
      efpArgs: list<RT.Dval.t>,
      efpFnName: string,
    }

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("tlid", TLID.encode(params.efpTLID)),
        ("trace_id", string(params.efpTraceID)),
        ("caller_id", ID.encode(params.efpCallerID)),
        ("args", list(RT.Dval.encode, params.efpArgs)),
        ("fnname", string(params.efpFnName)),
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
}

module Handler = {
  module Params = {
    type rec t = {
      thTLID: TLID.t,
      thTraceID: TraceID.t,
      thInput: AnalysisTypes.InputValueDict.t,
    }

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("tlid", TLID.encode(params.thTLID)),
        ("trace_id", string(params.thTraceID)),
        ("input", list(tuple2(string, RT.Dval.encode), Belt.Map.String.toList(params.thInput))),
      })
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = list<TLID.t>
  let decode = (j): t => {
    open Json_decode_extended
    field("touched_tlids", list(TLID.decode), j)
  }
}
