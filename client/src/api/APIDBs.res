module RT = RuntimeTypes
module PT = ProgramTypes

@ocaml.doc("A list of user DBs which are 'unlocked', meaning their structure
  can safely change, as there's no data inside.")
module UnlockedDBs = {
  @ppx.deriving(show({with_path: false}))
  type rec t = TLID.Set.t

  let decode = (j): t => {
    open Json_decode_extended
    j |> field("unlocked_dbs", list(TLID.decode)) |> TLID.Set.fromList
  }

  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("unlocked_dbs", list(TLID.encode, Tc.Set.toList(params)))})
  }
}

@ocaml.doc("Mapping of DB->stats, where stats are metadata around the DB's
  record count, usage, etc.")
module DBStats = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {dbStatsTlids: list<TLID.t>}

    let decode = (j): t => {
      open Json_decode_extended
      {
        dbStatsTlids: field("tlids", list(TLID.decode), j),
      }
    }

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("tlids", list(TLID.encode, params.dbStatsTlids))})
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

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{
        ("count", int(params.count)),
        ("example", nullable(pair(RT.Dval.encode, string), params.example)),
      })
    }
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = Tc.Map.String.t<Stat.t>

  let decode = (j): t => Json_decode_extended.strDict(Stat.decode, j)

  let encode = (d: t): Js.Json.t => {
    open Json_encode_extended
    strDict(Stat.encode, d)
  }
}
