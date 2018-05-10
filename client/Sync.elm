module Sync exposing (..)

-- dark
import Types exposing (..)
import RPC

markRequestInModel : Model -> Model
markRequestInModel m =
  let oldSyncState = m.syncState
  in
      { m | syncState =
        { oldSyncState | inFlight = True, ticks = 0 } }

markTickInModel : Model -> Model
markTickInModel m =
  let oldSyncState = m.syncState
  in
      { m | syncState =
        { oldSyncState | ticks = (oldSyncState.ticks + 1) } }

markResponseInModel : Model -> Model
markResponseInModel m =
  let oldSyncState = m.syncState
  in
      { m | syncState =
        { oldSyncState | inFlight = False, ticks = 0 } }


timedOut : SyncState -> Bool
timedOut s =
 (s.ticks % 10) == 0 && s.ticks /= 0

fetch : Model -> (Model, Cmd Msg)
fetch m =
  if (not m.syncState.inFlight)
      || (timedOut m.syncState)
  then
    (markRequestInModel m) ! [RPC.getAnalysisRPC]
  else
    (markTickInModel m) ! []

