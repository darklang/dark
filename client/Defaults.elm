module Defaults exposing (..)

import Dict

import Types exposing (..)
import Autocomplete


escapeKeycode = 27
replID = "darkInput"
entryID = "darkEntry"
leftButton = 0

-- UI
nodeHeight = round 28
narrowChars = ['i', '[', ',', ']', 'l', 'I', 't', ' ']
edgeGradColor = "#111"
edgeColor = "#444"
edgeSize = "3.25px"
dragEdgeStrokeColor = "red"
dragEdgeSize = "2px"
initialPos = {x=400, y=170}

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
