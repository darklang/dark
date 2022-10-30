open Prelude

module NewStaticDeployPush = {
  let decode = {
    open Tea.Json.Decoder
    let decodeStatus = v =>
      switch Obj.magic(v) {
      | list{"Deploying"} => Ok(StaticAssets.Status.Deploying)
      | list{"Deployed"} => Ok(Deployed)
      | _ => Error("Unable to decode deployStatus")
      }

    let decodeDeploy = map4((deployHash, url, lastUpdate, status) => {
      StaticAssets.Deploy.deployHash: deployHash,
      url: url,
      lastUpdate: Js.Date.fromString(lastUpdate),
      status: status,
    }, field(
      "deploy_hash",
      string,
    ), field("url", string), field("last_update", string), field("status", Decoder(decodeStatus)))

    map(msg => msg, field("detail", decodeDeploy))
  }

  let listen = tagger =>
    BrowserSubscriptions.registerGlobal("newStaticDeploy", "newStaticDeploy", tagger, decode)
}

let appendDeploy = (
  newDeploys: list<StaticAssets.Deploy.t>,
  oldDeploys: list<StaticAssets.Deploy.t>,
): list<StaticAssets.Deploy.t> => {
  // sorts into reverse order
  let deploys =
    Belt.List.concat(newDeploys, oldDeploys) |> List.sortBy(~f=(d: StaticAssets.Deploy.t) =>
      Js.Date.getTime(d.lastUpdate)
    )

  /* We get two messages from the backend for the same hash: Deploying and
   * Deployed. If we have both, we should only keep Deployed. This does that,
   * though not perfectly. If there are two consecutive deploys with the same
   * hash (eg a Deployed and a Deploying), we pick the deployed one. If there
   * are multiple deploys at the same time, these might overlap. Also this
   * reverses the list into the correct order. */
  List.fold(~initial=list{}, deploys, ~f=(
    accum: list<StaticAssets.Deploy.t>,
    d: StaticAssets.Deploy.t,
  ) =>
    switch accum {
    | list{prev, ...rest} =>
      if prev.deployHash == d.deployHash {
        if prev.status == Deployed {
          list{prev, ...rest}
        } else {
          list{d, ...rest}
        }
      } else {
        list{d, prev, ...rest}
      }
    | _ => list{d, ...accum}
    }
  )
}
