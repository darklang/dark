module AddOps = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    result: APIAddOps.Result.t,
    params: APIAddOps.Params.t,
  }

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    {
      result: field("result", APIAddOps.Result.decode, j),
      params: field("params", APIAddOps.Params.decode, j),
    }
  }
}
