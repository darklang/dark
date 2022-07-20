open Prelude

let markRequestInModel = (~key: string, m: AppTypes.model): AppTypes.model => {
  let syncState = Set.add(m.syncState, ~value=key)
  {...m, syncState: syncState}
}

let markResponseInModel = (~key: string, m: AppTypes.model): AppTypes.model => {
  let syncState = Set.remove(m.syncState, ~value=key)
  {...m, syncState: syncState}
}

let inFlight = (~key: string, m: AppTypes.model): bool => Set.member(m.syncState, ~value=key)

let attempt = (~force=false, ~key: string, m: AppTypes.model, cmd: AppTypes.cmd): (
  AppTypes.model,
  AppTypes.cmd,
) =>
  if inFlight(m, ~key) && !force {
    (m, Tea.Cmd.none)
  } else {
    (markRequestInModel(m, ~key), cmd)
  }
