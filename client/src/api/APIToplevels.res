module DeleteForever = {
  module Params = {
    type t = {tlid: TLID.t}

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("tlid", TLID.encode(params.tlid))})
    }

    let decode = (j): t => {
      open Json_decode_extended
      {tlid: field("tlid", TLID.decode, j)}
    }
  }
}
