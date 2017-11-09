module Defaults exposing (..)

import Types exposing (..)
import Autocomplete

escapeKeycode : Int
escapeKeycode = 27
replID : String
replID = "darkInput"
entryID : String
entryID = "darkEntry"
leftButton : Int
leftButton = 0

-- UI
nodeHeight : Int
nodeHeight = round 28
edgeStrokeColor : String
edgeStrokeColor = "#666"
edgeSize : String
edgeSize = "1px"
initialPos : VPos
initialPos = {vx=475, vy=325}
moveSize : Int
moveSize = 50
unsetInt : Int
unsetInt = 12345678

defaultEditor : Editor
defaultEditor = { }

model2editor : Model -> Editor
model2editor m = {}

defaultModel : Editor -> Model
defaultModel e = { error = Nothing
                 , lastMsg = Initialization
                 , lastMod = NoChange
                 , center = {x=initialPos.vx, y=initialPos.vy}
                 , complete = Autocomplete.empty
                 , state = Deselected
                 , tests = []
                 , topLevels = []

                 -- editor
                 -- these load before the graph does, causing
                 -- exceptions. We'll need to only run these after the
                 -- graph loads
                 }
