module Delete = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = AnalysisTypes.FourOhFour.Spec.t
    let encode = (fof: t): Js.Json.t => {
      AnalysisTypes.FourOhFour.Spec.encode(fof)
    }
    let decode = (j): t => {
      AnalysisTypes.FourOhFour.Spec.decode(j)
    }
  }
}

module List = {
  @ppx.deriving(show({with_path: false}))
  type rec t = list<AnalysisTypes.FourOhFour.t>
  let decode = (j): t => {
    open Json_decode_extended
    field("f404s", list(AnalysisTypes.FourOhFour.decode), j)
  }
  let encode = (l: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("f404s", list(AnalysisTypes.FourOhFour.encode, l))})
  }
}
