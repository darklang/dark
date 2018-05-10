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
                , timersEnabled = True
                , cursorState = Deselected
                }

defaultSyncState : SyncState
defaultSyncState = { inFlight = False
                   , ticks = 0
                   }

defaultUrlState : UrlState
defaultUrlState = { lastPos = {x=0, y=0}
                  }

defaultModel : Model
defaultModel = { error = Nothing
               , lastMsg = Initialization
               , lastMod = NoChange
               , center = {x=initialPos.vx, y=initialPos.vy}
               , complete = Autocomplete.empty
               , userFunctions = []
               , builtInFunctions = []
               , currentPage = Toplevels
               , hovering = []
               , tests = []
               , toplevels = []
               , analysis = []
               , globals = []
               , f404s = []
               , unlockedDBs = []
               , integrationTestState = NoIntegrationTest
               , visibility = PageVisibility.Hidden
               -- partially saved in editor
               , syncState = defaultSyncState
               , urlState = defaultUrlState
               , timersEnabled = True
               , computedValuesDisabled = False
               -- saved in editor
               , clipboard = Nothing
               , cursorState = Deselected
               }
