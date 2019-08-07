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
  ; handlerProps = TLIDDict.empty
  ; canvasPos = origin
  ; lastReload = None
  ; sidebarOpen = true }


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
  ; hoveringReferences = []
  ; execution = Idle
  ; showActions = false }


let defaultToast : toast = {toastMessage = None; toastPos = None}

let defaultModel : model =
  { error = {message = None; showDetails = false}
  ; lastMsg = IgnoreMsg
  ; lastMod = NoChange
  ; complete =
      (* this is awkward, but avoids circular deps *)
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
  ; builtInFunctions = []
  ; currentPage = Architecture
  ; hovering = []
  ; tests = []
  ; handlers = TLIDDict.empty
  ; deletedHandlers = TLIDDict.empty
  ; dbs = TLIDDict.empty
  ; deletedDBs = TLIDDict.empty
  ; userFunctions = TLIDDict.empty
  ; deletedUserFunctions = TLIDDict.empty
  ; userTipes = TLIDDict.empty
  ; deletedUserTipes = TLIDDict.empty
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
  ; origin = ""
  ; environment = "none"
  ; csrfToken = "UNSET_CSRF"
  ; routingTableOpenDetails = StrSet.empty
  ; usedDBs = StrDict.empty
  ; usedFns = StrDict.empty
  ; usedTipes = TLIDDict.empty
  ; handlerProps = TLIDDict.empty
  ; staticDeploys = []
  ; tlRefersTo = TLIDDict.empty
  ; tlUsedIn = TLIDDict.empty
  ; fluidState = defaultFluidState
  ; dbStats = StrDict.empty
  ; avatarsList = []
  ; browserId = ""
  ; sidebarOpen = true
  ; isAdmin = false
  ; buildHash = ""
  ; lastReload = None
  ; permission = None
  ; toast = defaultToast }
