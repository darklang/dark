module Defaults exposing (..)

import Types exposing (..)
import Autocomplete

entryID : String
entryID = "entryBox"
leftButton : Int
leftButton = 0

-- UI
initialPos : VPos
initialPos = {vx=475, vy=325}
moveSize : Int
moveSize = 50

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
                 , toplevels = []
                 , analysis = []
                 , globals = []
                 , integrationTestState = NoIntegrationTest
                 , clipboard = Nothing
                 -- editor
                 -- these load before the graph does, causing
                 -- exceptions. We'll need to only run these after the
                 -- graph loads
                 }
