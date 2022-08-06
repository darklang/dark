module Insert = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = SecretTypes.t

    let encode = (params: t): Js.Json.t => SecretTypes.encode(params)
    let decode = (j): t => SecretTypes.decode(j)
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = list<SecretTypes.t>
  let encode = (l: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("secrets", list(SecretTypes.encode, l))})
  }
  let decode = (j): t => {
    open Json_decode_extended
    field("secrets", list(SecretTypes.decode), j)
  }
}
