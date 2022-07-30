module RT = RuntimeTypes
module PT = ProgramTypes

module UnlockedDBs = {
  @ppx.deriving(show({with_path: false}))
  type rec t = TLID.Set.t

  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("unlocked_dbs", list(TLID.encode, Tc.Set.toList(params)))})
  }

  let decode = (j): t => {
    open Json_decode_extended
    j |> field("unlocked_dbs", list(TLID.decode)) |> TLID.Set.fromList
  }
}

module DBStats = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {dbStatsTlids: list<TLID.t>}
    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("tlids", list(TLID.encode, params.dbStatsTlids))})
    }
    let decode = (j): t => {
      open Json_decode_extended
      {
        dbStatsTlids: field("tlids", list(TLID.decode), j),
      }
    }
  }

  module Stat = {
    @ppx.deriving(show({with_path: false}))
    type rec t = AnalysisTypes.dbStats

    let decode = (j): t => {
      open Json_decode_extended
      {
        count: field("count", int, j),
        example: field("example", optional(tuple2(RT.Dval.decode, string)), j),
      }
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = Tc.Map.String.t<Stat.t>

  let decode = (j): t => Json_decode_extended.strDict(Stat.decode, j)
}
