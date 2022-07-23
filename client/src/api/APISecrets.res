module Insert = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = SecretTypes.t

    let encode = (params: t): Js.Json.t => SecretTypes.encode(params)
  }

  @ppx.deriving(show({with_path: false}))
  type rec t = list<SecretTypes.t>
  let decode = (j): t => {
    open Json_decode_extended
    field("secrets", list(SecretTypes.decode), j)
  }
}
