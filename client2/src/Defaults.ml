open Belt
open Tea
open! Porting
open Types

let entryID = "entry-box"

let leftButton = 0

let initialVPos = {vx= 475; vy= 200}

let centerPos = {x= 475; y= 200}

let origin = {x= 0; y= 0}

let moveSize = 50

let pageHeight = 400

let pageWidth = 500

let defaultEditor =
  { clipboard= None
  ; timersEnabled= true
  ; cursorState= Deselected
  ; lockedHandlers= [] }

let defaultSyncState = {inFlight= false; ticks= 0}

let defaultUrlState = {lastPos= {x= 0; y= 0}}

let defaultCanvas = {offset= origin; fnOffset= origin; enablePan= true}

let defaultModel =
  { error= {message= None; showDetails= false}
  ; lastMsg= Initialization
  ; lastMod= NoChange
  ; complete=
      { functions= []
      ; admin= false
      ; completions= [[]; []; []; []]
      ; allCompletions= []
      ; index= -1
      ; value= ""
      ; prevValue= ""
      ; tipe= None
      ; target= None
      ; isCommandMode= false }
  ; userFunctions= []
  ; builtInFunctions= []
  ; currentPage= Toplevels {x= 0; y= 0}
  ; hovering= []
  ; tests= []
  ; toplevels= []
  ; deletedToplevels= []
  ; analyses= Belt.Map.String.empty
  ; traces= Belt.Map.Int.empty
  ; globals= []
  ; f404s= []
  ; unlockedDBs= []
  ; integrationTestState= NoIntegrationTest
  ; visibility= PageVisibility.Hidden
  ; syncState= defaultSyncState
  ; urlState= defaultUrlState
  ; timersEnabled= true
  ; clipboard= None
  ; cursorState= Deselected
  ; executingFunctions= []
  ; tlCursors= Belt.Map.Int.empty
  ; featureFlags= Belt.Map.Int.empty
  ; lockedHandlers= []
  ; canvas= defaultCanvas
  ; canvasName= "builtwithdark"
  ; userContentHost= "builtwithdark.com"
  ; environment= "none" }
