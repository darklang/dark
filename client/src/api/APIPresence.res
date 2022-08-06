module Params = {
  type t = {
    browserId: string,
    tlid: option<TLID.t>,
    canvasName: string,
    timestamp: float,
  }
  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("canvasName", string(params.canvasName)),
      ("browserId", string(params.browserId)),
      ("tlid", nullable(TLID.encode, params.tlid)),
      ("timestamp", Json.Encode.float(params.timestamp)),
    })
  }
}
