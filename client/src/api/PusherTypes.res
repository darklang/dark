module AddOps = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    result: APITypes.AddOps.Result.t,
    params: APITypes.AddOps.Params.t,
  }

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    {
      result: field("result", APITypes.AddOps.Result.decode, j),
      params: field("params", APITypes.AddOps.Params.decode, j),
    }
  }
}
