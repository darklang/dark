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
narrowChars : List Char
narrowChars = ['i', '[', ',', ']', 'l', 'I', 't', 'r', ':', '/', '.']
edgeGradColor : String
edgeGradColor = "#111"
edgeColor : String
edgeColor = "#444"
edgeSize : String
edgeSize = "3.25px"
dragEdgeStrokeColor : String
dragEdgeStrokeColor = "red"
dragEdgeSize : String
dragEdgeSize = "2px"
initialPos : VPos
initialPos = {vx=475, vy=325}

defaultEditor : Editor
defaultEditor = {}

model2editor : Model -> Editor
model2editor m = {}

defaultModel : Editor -> Model
defaultModel e = { nodes = Dict.empty
                 , error = ("None", 0)
                 , lastMsg = Initialization
                 , lastMod = NoChange
                 , center = {x=initialPos.vx, y=initialPos.vy}

                 -- editor TODO
                 -- these load before the graph does, causing
                 -- exceptions. We'll need to only run these after the
                 -- graph loads
                 , complete = Autocomplete.empty
                 , tempFieldName = ""
                 , state = Deselected
                 }
