module Status = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | Deploying
    | Deployed

  let decode = (j): t => {
    open Json_decode_extended
    variants(list{("Deployed", variant0(Deployed)), ("Deploying", variant0(Deploying))}, j)
  }
}

module Deploy = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    deployHash: string,
    url: string,
    @opaque lastUpdate: Js.Date.t,
    status: Status.t,
  }

  let decode = (j): t => {
    open Json_decode_extended
    {
      deployHash: field("deploy_hash", string, j),
      url: field("url", string, j),
      lastUpdate: field("last_update", date, j),
      status: field("status", Status.decode, j),
    }
  }
}
