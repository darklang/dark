module Params = {
  type t = {tunnelHost: option<string>}
  let encode = (params: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("tunnelHost", nullable(string, params.tunnelHost))})
  }
  let decode = (j): t => {
    open Json_decode_extended
    {tunnelHost: field("tunnelHost", optional(string), j)}
  }
}
@ppx.deriving(show({with_path: false}))
type rec t = bool

let encode = (success: t): Js.Json.t => {
  open Json_encode_extended
  object_(list{("success", bool(success))})
}
let decode = (j): t => {
  open Json_decode_extended
  field("success", bool, j)
}
