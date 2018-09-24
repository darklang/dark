module Defaults exposing (..)

-- builtin
-- lib

-- dark
import Types exposing (..)
import PageVisibility
import StrDict
import IntDict


entryID : String
entryID = "entry-box"
leftButton : Int
leftButton = 0

-- UI
initialVPos : VPos
initialVPos = {vx=475, vy=200}

centerPos : Pos
centerPos = {x=475, y=200}

origin : Pos
origin = { x = 0, y = 0 }

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
defaultUrlState = { lastPos = {x=0, y=0}
                  }

defaultCanvas : CanvasProps
defaultCanvas =
  { offset = origin -- These is intended to be (Viewport.toCentedOn centerPos)
  , fnOffset = origin
  , enablePan = True
  }

defaultModel : Model
defaultModel = { error = { message = Nothing
                        , showDetails = False }
               , lastMsg = Initialization
               , lastMod = NoChange
               -- this is awkward, but avoids circular deps
               , complete = { functions = []
                            , admin = False
                            , completions = [[],[],[],[]]
                            , allCompletions = []
                            , index = -1
                            , value = ""
                            , prevValue = ""
                            , acTipe = Nothing
                            , target = Nothing
                            , isCommandMode = False
                            }
               , userFunctions = []
               , builtInFunctions = []
               , currentPage = Toplevels {x=0,y=0}
               , hovering = []
               , tests = []
               , toplevels = []
               , deletedToplevels = []
               , analyses = StrDict.empty
               , traces = IntDict.empty
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
               , tlCursors = IntDict.empty
               , featureFlags = IntDict.empty
               , lockedHandlers = []
               , canvas = defaultCanvas
               , canvasName = "builtwithdark"
               , userContentHost = "builtwithdark.com"
               , environment = "none"
               , csrfToken = ""
               }
