module Editor exposing (..)

-- builtin
import Json.Decode as JSD

-- lib
import Json.Encode as JSE

-- dark

import Defaults
import RPC
import Types exposing (..)

fromString : Maybe String -> SerializableEditor
fromString json =
  json
  |> Maybe.map (JSD.decodeString RPC.decodeSerializableEditor)
  |> Maybe.withDefault (Ok Defaults.defaultEditor)
  |> Result.withDefault Defaults.defaultEditor

toString : SerializableEditor -> String
toString se =
  RPC.encodeSerializableEditor se
  |> JSE.encode 0


editor2model : SerializableEditor -> Model
editor2model e =
  let m = Defaults.defaultModel
      oldSyncState = m.syncState
  in
      { m | syncState = { oldSyncState | enabled = e.syncEnabled }
      , clipboard = e.clipboard
      , cursorState = e.cursorState
      }

model2editor : Model -> SerializableEditor
model2editor m =
  { clipboard = m.clipboard
  , syncEnabled = m.syncState.enabled
  , cursorState = m.cursorState
  }
