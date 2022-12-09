module ClientTypes.StaticDeploy

type DeployStatus =
  | Deploying
  | Deployed

type T =
  { deployHash : string
    url : string
    lastUpdate : NodaTime.Instant
    status : DeployStatus }
