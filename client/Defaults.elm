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
initialPos : Pos
initialPos = {x=200, y=200}

defaultEditor : Editor
defaultEditor = {}

model2editor : Model -> Editor
model2editor m = {}

defaultModel : Editor -> Model
defaultModel e = { nodes = Dict.empty
                 , edges = []
                 , error = ("None", 0)
                 , lastMsg = Initialization
                 , lastMod = NoChange

                 -- editor TODO
                 -- these load before the graph does, causing
                 -- exceptions. We'll need to only run these after the
                 -- graph loads
                 , complete = Autocomplete.empty
                 , tempFieldName = ""
                 , state = Deselected
                 }
