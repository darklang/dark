module Editor exposing (..)

-- builtin
import Json.Decode as JSD
import Json.Encode as JSE

-- lib

-- dark

import Defaults
import RPC
import Types exposing (..)


editor2model : Editor -> Model
editor2model e =
  let m = Defaults.defaultModel in
  { m | syncEnabled = e.syncEnabled
      , clipboard = e.clipboard
                    |> JSD.decodeValue
                         (JSD.nullable RPC.decodePointerData)
                    |> Result.withDefault Nothing
  }

model2editor : Model -> Editor
model2editor m =
  { clipboard = case m.clipboard of
                  Nothing -> JSE.null
                  Just pd -> RPC.encodePointerData pd
  , syncEnabled = m.syncEnabled
  }
