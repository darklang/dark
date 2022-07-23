module WorkerStats = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {workerStatsTlid: TLID.t}

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("tlid", TLID.encode(params.workerStatsTlid))})
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = AnalysisTypes.WorkerStats.t

  let decode = (j): t => {
    open Json_decode_extended
    {count: field("count", int, j), schedule: None}
  }
}

module Scheduler = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {
      workerName: string,
      schedule: string,
    }

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("name", string(params.workerName)), ("schedule", string(params.schedule))})
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = Tc.Map.String.t<string>

  let decode = (j): t => {
    open Json_decode_extended
    strDict(string)(j)
  }
}
