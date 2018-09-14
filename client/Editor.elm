module Editor exposing (..)

-- builtin
import Json.Decode as JSD

-- lib
import Json.Encode as JSE

-- dark

import Defaults
import RPC
import Types exposing (..)
import Toplevel as TL

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
  in
      { m | timersEnabled = e.timersEnabled
          , clipboard = e.clipboard
          , cursorState = e.cursorState
          , lockedHandlers = e.lockedHandlers
      }

model2editor : Model -> SerializableEditor
model2editor m =
  { clipboard = m.clipboard
  , timersEnabled = m.timersEnabled
  , cursorState = m.cursorState
  , lockedHandlers = m.lockedHandlers
  }

updateLockedHandlers : TLID -> Bool -> Model -> Modification
updateLockedHandlers tlid lockHandler m =
  let tl = TL.getTL m tlid
  in case tl.data of
    TLHandler _ ->
      let lockedList =
            if lockHandler then
              tlid :: m.lockedHandlers
            else
              List.filter (\t -> t /= tlid) m.lockedHandlers
      in SetLockedHandlers lockedList
    _ -> NoChange
