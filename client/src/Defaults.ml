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
  ; canvasPos = origin }


let defaultFluidState : fluidState =
  { actions = []
  ; error = None
  ; oldPos = 0
  ; newPos = 0
  ; upDownCol = None
  ; lastKey = FluidKeyboard.Escape
  ; ac =
      { functions = []
      ; index = None
      ; query = None
      ; completions = []
      ; invalidCompletions = []
      ; allCompletions = [] }
  ; cp =
      { index = 0
      ; show = false
      ; commands = []
      ; cmdOnTL = None
      ; cmdOnID = None
      ; filter = None } }


let defaultCanvasProps : canvasProps =
  { offset = origin
  ; enablePan = true
  ; lastOffset = None
  ; viewportSize = {w = 0; h = 0}
  ; panAnimation = false }


let defaultHandlerProp : handlerProp =
  { handlerLock = false
  ; handlerState = HandlerExpanded
  ; hoveringVariableName = None }


let defaultModel : model =
  { error = {message = None; showDetails = false}
  ; lastMsg = IgnoreMsg
  ; lastMod = NoChange (* this is awkward, but avoids circular deps *)
  ; complete =
      { functions = []
      ; admin = false
      ; completions = []
      ; target = None
      ; invalidCompletions = []
      ; allCompletions = []
      ; index = -1
      ; value = ""
      ; prevValue = ""
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
  ; tlUsages = []
  ; tlMeta = StrDict.empty
  ; fluidState = defaultFluidState
  ; dbStats = StrDict.empty
  ; avatarsList = [] }
