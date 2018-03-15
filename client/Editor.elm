module Editor exposing (..)

-- Dark
import Defaults
import Types exposing (..)

editor2model : Editor -> Model
editor2model e =
  Defaults.defaultModel

model2editor : Model -> Editor
model2editor m =
  case m.clipboard of
    Nothing -> { clipboard = Nothing }
    Just c ->
      { clipboard = Nothing }
