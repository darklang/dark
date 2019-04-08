open Tc
open Types

let entryID : string = "entry-box"

let leftButton : int = 0

let initialVPos : vPos = {vx = 475; vy = 200}

let centerPos : pos = {x = 475; y = 200}

let origin : pos = {x = 0; y = 0}

let moveSize : int = 50

let pageHeight : int = 400

let pageWidth : int = 500

let defaultEditor : serializableEditor =
  { timersEnabled = true
  ; cursorState = Deselected
  ; routingTableOpenDetails = StrSet.empty
  ; tlCursors = StrDict.empty
  ; featureFlags = StrDict.empty
  ; handlerProps = StrDict.empty
  ; canvasPos = origin
  ; tlReferences = StrDict.empty }


let defaultCanvasProps : canvasProps =
  { offset = origin
  ; enablePan = true
  ; lastOffset = None
  ; viewportSize = {w = 0; h = 0}
  ; panAnimation = false }


let defaultHandlerProp : handlerProp =
  {handlerLock = false; handlerState = HandlerExpanded}


let defaultModel : model =
  { error = {message = None; showDetails = false}
  ; lastMsg = IgnoreMsg
  ; lastMod = NoChange (* this is awkward, but avoids circular deps *)
  ; complete =
      { functions = []
      ; admin = false
      ; completions = []
      ; invalidCompletions = []
      ; allCompletions = []
      ; index = -1
      ; value = ""
      ; prevValue = ""
      ; target = None
      ; targetDval = None
      ; isCommandMode = false
      ; visible = true }
  ; userFunctions = []
  ; deletedUserFunctions = []
  ; builtInFunctions = []
  ; currentPage = Architecture
  ; hovering = []
  ; tests = []
  ; toplevels = []
  ; deletedToplevels = []
  ; analyses = StrDict.empty
  ; traces = StrDict.empty
  ; f404s = []
  ; unlockedDBs = StrSet.empty
  ; integrationTestState = NoIntegrationTest
  ; visibility = PageVisibility.Visible (* partially saved in editor *)
  ; syncState = StrSet.empty
  ; timersEnabled = true (* saved in editor *)
  ; cursorState = Deselected
  ; executingFunctions = []
  ; tlCursors = StrDict.empty
  ; featureFlags = StrDict.empty
  ; canvasProps = defaultCanvasProps
  ; canvasName = "builtwithdark"
  ; userContentHost = "builtwithdark.com"
  ; environment = "none"
  ; csrfToken = "UNSET_CSRF"
  ; routingTableOpenDetails = StrSet.empty
  ; usedDBs = StrDict.empty
  ; usedFns = StrDict.empty
  ; usedTipes = StrDict.empty
  ; handlerProps = StrDict.empty
  ; staticDeploys = []
  ; userTipes = []
  ; deletedUserTipes = []
  ; tlReferences = StrDict.empty }
