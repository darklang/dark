module WorkerStats = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {tlid: TLID.t}

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("tlid", TLID.encode(params.tlid))})
    }

    let decode = (j): t => {
      open Json_decode_extended
      {tlid: field("tlid", TLID.decode, j)}
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = {count: int}

  let decode = (j): t => {
    open Json_decode_extended
    {count: field("count", int, j)}
  }

  let encode = (ws: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("count", int(ws.count))})
  }
}

module Scheduler = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      name: string,
      schedule: AnalysisTypes.WorkerState.t,
    }

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("name", string(params.name)),
        ("schedule", AnalysisTypes.WorkerState.encode(params.schedule)),
      })
    }

    let decode = (j): t => {
      open Json_decode_extended
      {
        name: field("name", string, j),
        schedule: field("schedule", AnalysisTypes.WorkerState.decode, j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = Tc.Map.String.t<AnalysisTypes.WorkerState.t>

  let decode = (j): t => {
    open Json_decode_extended
    strDict(AnalysisTypes.WorkerState.decode)(j)
  }
  let encode = (sr: t): Js.Json.t => {
    open Json_encode_extended
    strDict(AnalysisTypes.WorkerState.encode, sr)
  }
}
