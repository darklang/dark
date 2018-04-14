module Sync exposing (..)

-- dark
import Types exposing (..)

enable : Model -> Model
enable m =
  let oldSyncState = m.syncState
  in
      { m | syncState = { oldSyncState | enabled = True } }

disable : Model -> Model
disable m =
  let oldSyncState = m.syncState
  in
      { m | syncState = { oldSyncState | enabled = False } }

toggle : Model -> Model
toggle m =
  let oldSyncState = m.syncState
  in
      { m | syncState = { oldSyncState | enabled = (not oldSyncState.enabled) } }
