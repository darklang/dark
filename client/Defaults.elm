module Defaults exposing (..)

import Dict

import Types exposing (..)


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
defaultEditor = { entryPos = initialPos
                , clickPos = {x=0, y=0}
                , entryValue = ""
                , replValue = ""
                , cursor = Nothing
                , tempFieldName = ""
                , focused = False
                }
defaultModel : Editor -> Model
defaultModel e = { nodes = Dict.empty
                 , edges = []
                 , errors = ["None"]
                 , dragPos = {x=0, y=0}
                 , drag = NoDrag
                -- these load before the graph does, causing exceptions. We'll
                -- need to only run these after the graph loads
                 -- , cursor = Maybe.map ID e.cursor
                 , cursor = Nothing
                 -- editor
                 , entryPos = e.entryPos
                 , clickPos = e.clickPos
                 , entryValue = e.entryValue
                 , replValue = e.replValue
                 , tempFieldName = e.tempFieldName
                 , focused = e.focused
                 }
