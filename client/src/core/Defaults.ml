open Prelude

let entryID : string = "entry-box"

let leftButton : int = 0

let initialVPos : vPos = {vx = 475; vy = 200}

let centerPos : pos = {x = 475; y = 200}

let origin : pos = {x = 0; y = 0}

let moveSize : int = 50

let pageHeight : int = 400

let pageWidth : int = 500

let unsetCSRF : string = "UNSET_CSRF"

let defaultUserSettings : savedUserSettings = {showUserWelcomeModal = true}

let defaultSavedSettings : savedSettings =
  { editorSettings = {runTimers = true; showFluidDebugger = false}
  ; cursorState = Deselected
  ; routingTableOpenDetails = StrSet.empty
  ; tlTraceIDs = TLIDDict.empty
  ; featureFlags = StrDict.empty
  ; handlerProps = TLIDDict.empty
  ; canvasPos = origin
  ; lastReload = None
  ; sidebarOpen = true
  ; showTopbar = false }


let defaultFluidState : fluidState =
  { actions = []
  ; error = None
  ; oldPos = 0
  ; newPos = 0
  ; upDownCol = None
  ; lastInput =
      Keypress
        { key = FluidKeyboard.Escape
        ; shiftKey = false
        ; altKey = false
        ; metaKey = false
        ; ctrlKey = false }
  ; ac =
      { functions = []
      ; index = None
      ; query = None
      ; completions = []
      ; invalidCompletions = []
      ; allCompletions = [] }
  ; cp = {index = 0; commands = []; location = None; filter = None}
  ; selectionStart = None
  ; errorDvSrc = SourceNone
  ; midClick = false
  ; activeEditorPanelIdx = 0 }


let defaultCanvasProps : canvasProps =
  { offset = origin
  ; enablePan = true
  ; lastOffset = None
  ; panAnimation = DontAnimateTransition
  ; minimap = None }


let defaultHandlerProp : handlerProp =
  { handlerLock = false
  ; handlerState = HandlerExpanded
  ; hoveringReferences = []
  ; execution = Idle }


let defaultToast : toast = {toastMessage = None; toastPos = None}

let defaultAccount : account = {name = ""; email = ""; username = ""}

let defaultWorkerStats : workerStats = {count = 0; schedule = None}

let defaultMenu : menuState = {isOpen = false}

let defaultFnSpace : fnProps =
  {draggingParamIndex = None; dragOverSpaceIndex = None; justMovedParam = None}


let defaultModel : model =
  { error = None
  ; lastMsg = IgnoreMsg
  ; lastMod = NoChange
  ; opCtrs = StrDict.empty
  ; clientOpCtrId = ""
  ; complete =
      (* this is awkward, but avoids circular deps *)
      { admin = false
      ; completions = []
      ; target = None
      ; allCompletions = []
      ; index = -1
      ; value = ""
      ; prevValue = ""
      ; visible = true }
  ; builtInFunctions = []
  ; currentPage = Architecture
  ; hovering = []
  ; tests = []
  ; groups = TLIDDict.empty
  ; handlers = TLIDDict.empty
  ; deletedHandlers = TLIDDict.empty
  ; dbs = TLIDDict.empty
  ; deletedDBs = TLIDDict.empty
  ; userFunctions = TLIDDict.empty
  ; deletedUserFunctions = TLIDDict.empty
  ; userTipes = TLIDDict.empty
  ; deletedUserTipes = TLIDDict.empty
  ; deletedGroups = TLIDDict.empty
  ; packageFns = TLIDDict.empty
  ; analyses = StrDict.empty
  ; traces = StrDict.empty
  ; f404s = []
  ; unlockedDBs = StrSet.empty
  ; integrationTestState = NoIntegrationTest
  ; visibility = PageVisibility.Visible (* partially saved in editor *)
  ; syncState = StrSet.empty
  ; cursorState = Deselected
  ; executingFunctions = []
  ; tlTraceIDs = TLIDDict.empty
  ; featureFlags = StrDict.empty
  ; canvasProps = defaultCanvasProps
  ; canvasName = "builtwithdark"
  ; userContentHost = "builtwithdark.com"
  ; origin = ""
  ; environment = "none"
  ; csrfToken = unsetCSRF
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
  ; workerStats = StrDict.empty
  ; avatarsList = []
  ; browserId = ""
  ; sidebarOpen = true
  ; isAdmin = false
  ; buildHash = ""
  ; username = "defaultUsername"
  ; lastReload = None
  ; permission = None
  ; showTopbar = true
  ; toast = defaultToast
  ; account = defaultAccount
  ; worker_schedules = StrDict.empty
  ; searchCache = TLIDDict.empty
  ; editorSettings = {showFluidDebugger = false; runTimers = true}
  ; teaDebuggerEnabled = false
  ; unsupportedBrowser = false
  ; tlMenus = TLIDDict.empty
  ; showUserWelcomeModal = true
  ; currentUserFn = defaultFnSpace
  ; settingsView =
      {opened = false; tab = UserSettings; canvas_list = []; org_list = []} }
