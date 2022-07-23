module DeleteForever = {
  module Params = {
    type t = {dtfTLID: TLID.t}

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("tlid", TLID.encode(params.dtfTLID))})
    }
  }
}
