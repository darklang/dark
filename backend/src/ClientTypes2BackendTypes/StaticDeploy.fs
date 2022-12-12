module ClientTypes2BackendTypes.StaticDeploy

module SA = LibBackend.StaticAssets
module CTStaticDeploy = ClientTypes.StaticDeploy

module DeployStatus =
  let toCT (s : SA.DeployStatus) : CTStaticDeploy.DeployStatus =
    match s with
    | SA.DeployStatus.Deploying -> CTStaticDeploy.Deploying
    | SA.DeployStatus.Deployed -> CTStaticDeploy.Deployed

let toCT (d : SA.StaticDeploy) : ClientTypes.StaticDeploy.T =
  { deployHash = d.deployHash
    url = d.url
    lastUpdate = d.lastUpdate
    status = DeployStatus.toCT d.status }
