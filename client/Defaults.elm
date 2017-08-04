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
defaultEditor = { entryValue = ""
                , replValue = ""
                , cursor = (Nothing, Just initialPos)
                , tempFieldName = ""
                }

model2editor : Model -> Editor
model2editor m =
  { cursor = case m.cursor of
               Filling n hole pos -> (Just (deID n.id), Just pos)
               Creating pos -> (Nothing, Just pos)
               Deselected -> (Nothing, Nothing)
               Dragging nid -> (Just (deID nid), Nothing)
  , replValue = m.replValue
  , entryValue = m.entryValue
  , tempFieldName = m.tempFieldName
  }


defaultModel : Editor -> Model
defaultModel e = { nodes = Dict.empty
                 , edges = []
                 , error = ("None", 0)
                 , lastMsg = Initialization
                 , dragPos = {x=0, y=0}
                 , drag = NoDrag
                 , complete = Autocomplete.empty
                -- these load before the graph does, causing exceptions. We'll
                -- need to only run these after the graph loads
                 , cursor = case e.cursor of
                              (_, Just pos) -> Creating pos
                              _ -> Deselected
                 -- editor
                 , entryValue = e.entryValue
                 , replValue = e.replValue
                 , tempFieldName = e.tempFieldName
                 }
