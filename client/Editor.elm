module Editor exposing (..)

-- builtin
import Json.Decode as JSD

-- lib
import Json.Encode as JSE

-- dark

import Defaults
import RPC
import Types exposing (..)

fromString : String -> SerializableEditor
fromString json =
  JSD.decodeString RPC.decodeSerializableEditor json
  |> Result.withDefault Defaults.defaultEditor

toString : SerializableEditor -> String
toString se =
  RPC.encodeSerializableEditor se
  |> JSE.encode 0


editor2model : SerializableEditor -> Model
editor2model e =
  let m = Defaults.defaultModel in
  { m | syncEnabled = e.syncEnabled
      , clipboard = e.clipboard
      , cursorState = e.cursorState
  }

model2editor : Model -> SerializableEditor
model2editor m =
  { clipboard = m.clipboard
  , syncEnabled = m.syncEnabled
  , cursorState = m.cursorState
  }
