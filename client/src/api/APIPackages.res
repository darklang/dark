module PT = ProgramTypes

module UploadFn = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = {uplFn: PT.UserFunction.t}

    let encode = (params: t): Js.Json.t => {
      open Json_encode_extended
      object_(list{("fn", PT.UserFunction.encode(params.uplFn))})
    }
  }
}

module AllPackages = {
  @ppx.deriving(show({with_path: false}))
  type rec t = list<PT.Package.Fn.t>

  let decode = (j): t => Json.Decode.list(PT.Package.Fn.decode, j)
}
