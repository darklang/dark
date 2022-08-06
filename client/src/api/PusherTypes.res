module AddOps = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    result: APIAddOps.t,
    params: APIAddOps.Params.t,
  }

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    {
      result: field("result", APIAddOps.decode, j),
      params: field("params", APIAddOps.Params.decode, j),
    }
  }

  let encode = (ops: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("result", APIAddOps.encode(ops.result)),
      ("params", APIAddOps.Params.encode(ops.params)),
    })
  }
}
