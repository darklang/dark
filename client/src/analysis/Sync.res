open Prelude

let markRequestInModel = (~key: string, m: model): model => {
  let syncState = Set.add(m.syncState, ~value=key)
  {...m, syncState: syncState}
}

let markResponseInModel = (~key: string, m: model): model => {
  let syncState = Set.remove(m.syncState, ~value=key)
  {...m, syncState: syncState}
}

let inFlight = (~key: string, m: model): bool => Set.member(m.syncState, ~value=key)

let attempt = (~force=false, ~key: string, m: model, cmd: Tea.Cmd.t<msg>): (
  model,
  Tea.Cmd.t<msg>,
) =>
  if inFlight(m, ~key) && !force {
    (m, Tea.Cmd.none)
  } else {
    (markRequestInModel(m, ~key), cmd)
  }
