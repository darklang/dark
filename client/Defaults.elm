module Defaults exposing (..)

-- builtin
-- lib
import Dict

-- dark
import Types exposing (..)
import PageVisibility


entryID : String
entryID = "entry-box"
leftButton : Int
leftButton = 0

-- UI
initialVPos : VPos
initialVPos = {vx=475, vy=325}

initialPos : Pos
initialPos = {x=475, y=325}

fnPos : Pos
fnPos = {x=0, y=0}

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
                , lockedHandlers = []
                }

defaultSyncState : SyncState
defaultSyncState = { inFlight = False
                   , ticks = 0
                   }

defaultUrlState : UrlState
defaultUrlState = { lastPos = {x=0, y=0},
                    canvasPos = {x=0, y=0}
                  }

defaultCanvas : CanvasProps
defaultCanvas = { offset = initialPos }

defaultModel : Model
defaultModel = { error = Nothing
               , lastMsg = Initialization
               , lastMod = NoChange
               -- this is awkward, but avoids circular deps
               , complete = { functions = []
                            , admin = False
                            , completions = [[],[],[],[]]
                            , allCompletions = []
                            , index = -1
                            , value = ""
                            , tipe = Nothing
                            , target = Nothing
                            , isCommandMode = False
                            }
               , userFunctions = []
               , builtInFunctions = []
               , currentPage = Toplevels initialPos
               , hovering = []
               , tests = []
               , toplevels = []
               , deletedToplevels = []
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
               -- saved in editor
               , clipboard = Nothing
               , cursorState = Deselected
               , executingFunctions = []
               , tlCursors = Dict.empty
               , featureFlags = Dict.empty
               , lockedHandlers = []
               , canvas = defaultCanvas
               }
