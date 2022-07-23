module Delete = {
  module Params = {
    @ppx.deriving(show({with_path: false}))
    type rec t = AnalysisTypes.FourOhFour.t

    let encode = (params: t): Js.Json.t => {
      AnalysisTypes.FourOhFour.encode(params)
    }
  }
}

module Get = {
  @ppx.deriving(show({with_path: false}))
  type rec t = list<AnalysisTypes.FourOhFour.t>

  let decode = (j): t => {
    open Json_decode_extended
    j |> field("f404s", list(AnalysisTypes.FourOhFour.decode))
  }
}
