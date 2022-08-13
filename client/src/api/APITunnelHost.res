module Params = {
  type t = {tunnelHost: option<string>}
  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("tunnelHost", nullable(string, params.tunnelHost))})
  }
}
@ppx.deriving(show({with_path: false}))
type rec t = bool
let decode = (j): t => {
  open Json_decode_extended
  field("success", bool, j)
}
