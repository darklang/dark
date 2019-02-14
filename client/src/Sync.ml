open Types

let markRequestInModel (m : model) : model =
  {m with syncState = {inFlight = true; ticks = 0}}


let markTickInModel (m : model) : model =
  let oldSyncState = m.syncState in
  {m with syncState = {oldSyncState with ticks = oldSyncState.ticks + 1}}


let markResponseInModel (m : model) : model =
  {m with syncState = {inFlight = false; ticks = 0}}


let timedOut (s : syncState) : bool = s.ticks mod 10 = 0 && s.ticks <> 0

external origin : string = "origin"
  [@@bs.val] [@@bs.scope "window", "location"]

external prefix : string = "testcafeInjectedPrefix"
  [@@bs.val] [@@bs.scope "window"]

let contextFromModel (m : model) : traceFetchContext =
  {canvasName = m.canvasName; csrfToken = m.csrfToken; origin; prefix}
