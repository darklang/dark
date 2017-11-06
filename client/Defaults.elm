module Defaults exposing (..)

import Dict

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
defaultEditor = { openNodes = []
                }

model2editor : Model -> Editor
model2editor m = {openNodes = List.map deID m.openNodes}

defaultModel : Editor -> Model
defaultModel e = { nodes = Dict.empty
                 , backingNodes = Dict.empty
                 , phantoms = Dict.empty
                 , error = Nothing
                 , lastMsg = Initialization
                 , lastMod = NoChange
                 , center = {x=initialPos.vx, y=initialPos.vy}
                 , tests = []
                 -- editor
                 , openNodes = List.map ID e.openNodes
                 -- these load before the graph does, causing
                 -- exceptions. We'll need to only run these after the
                 -- graph loads
                 , complete = Autocomplete.empty
                 , state = Deselected
                 }
