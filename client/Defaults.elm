module Defaults exposing (..)

-- builtin
-- lib

-- dark
import Types exposing (..)
import PageVisibility
import Autocomplete


entryID : String
entryID = "entry-box"
leftButton : Int
leftButton = 0

-- UI
initialPos : VPos
initialPos = {vx=475, vy=325}

moveSize : Int
moveSize = 50

pageHeight : Int
pageHeight = 400

pageWidth : Int
pageWidth = 500

defaultEditor : SerializableEditor
defaultEditor = { clipboard = Nothing
                , syncEnabled = True
                , cursorState = Deselected
                }

defaultModel : Model
defaultModel = { error = Nothing
               , lastMsg = Initialization
               , lastMod = NoChange
               , center = {x=initialPos.vx, y=initialPos.vy}
               , complete = Autocomplete.empty
               , userFunctions = []
               , hovering = []
               , tests = []
               , toplevels = []
               , analysis = []
               , globals = []
               , integrationTestState = NoIntegrationTest
               , visibility = PageVisibility.Hidden
               -- saved in editor
               , clipboard = Nothing
               , syncEnabled = True
               , cursorState = Deselected
               }
