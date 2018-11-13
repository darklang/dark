open Tea
open! Porting
open Types

let entryID : string = "entry-box"

let leftButton : int = 0

let initialVPos : vPos = {vx= 475; vy= 200}

let centerPos : pos = {x= 475; y= 200}

let origin : pos = {x= 0; y= 0}

let moveSize : int = 50

let pageHeight : int = 400

let pageWidth : int = 500

let defaultEditor : serializableEditor =
  { clipboard= None
  ; timersEnabled= true
  ; cursorState= Deselected
  ; lockedHandlers= [] }

let defaultSyncState : syncState = {inFlight= false; ticks= 0}

let defaultUrlState : urlState = {lastPos= {x= 0; y= 0}}

let defaultCanvas : canvasProps =
  {offset= origin; fnOffset= origin; enablePan= true}

let defaultModel : model =
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
      ; acTipe= None
      ; target= None
      ; isCommandMode= false }
  ; userFunctions= []
  ; builtInFunctions= []
  ; currentPage= Toplevels {x= 0; y= 0}
  ; hovering= []
  ; tests= []
  ; toplevels= []
  ; deletedToplevels= []
  ; analyses= StrDict.empty
  ; traces= IntDict.empty
  ; globals= []
  ; f404s= []
  ; unlockedDBs= []
  ; integrationTestState= NoIntegrationTest
  ; visibility= PageVisibility.Visible
  ; syncState= defaultSyncState
  ; urlState= defaultUrlState
  ; timersEnabled= true
  ; clipboard= None
  ; cursorState= Deselected
  ; executingFunctions= []
  ; tlCursors= IntDict.empty
  ; featureFlags= IntDict.empty
  ; lockedHandlers= []
  ; canvas= defaultCanvas
  ; canvasName= "builtwithdark"
  ; userContentHost= "builtwithdark.com"
  ; environment= "none"
  ; csrfToken = "UNSET_CSRF"
  }
