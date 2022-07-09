open Prelude

let entryID: string = "entry-box"

let leftButton: int = 0

let initialVPos: vPos = {vx: 475, vy: 200}

let centerPos: pos = {x: 475, y: 200}

let origin: pos = {x: 0, y: 0}

let moveSize: int = 50

let pageHeight: int = 400

let pageWidth: int = 500

let unsetCSRF: string = "UNSET_CSRF"

let defaultUserSettings: savedUserSettings = {firstVisitToDark: true, recordConsent: None}

let defaultSidebar: sidebarState = {mode: DetailedMode, openedCategories: Set.String.empty}

let defaultSavedSettings: savedSettings = {
  editorSettings: {runTimers: true, showHandlerASTs: false, showFluidDebugger: false},
  cursorState: Deselected,
  tlTraceIDs: TLID.Dict.empty,
  featureFlags: Map.String.empty,
  handlerProps: TLID.Dict.empty,
  canvasPos: origin,
  lastReload: None,
  sidebarState: defaultSidebar,
  showTopbar: false,
  firstVisitToThisCanvas: true,
  userTutorial: None,
  userTutorialTLID: None,
}

let defaultFluidState: fluidState = {
  actions: list{},
  error: None,
  oldPos: 0,
  newPos: 0,
  upDownCol: None,
  lastInput: Keypress({
    key: FluidKeyboard.Escape,
    shiftKey: false,
    altKey: false,
    metaKey: false,
    ctrlKey: false,
  }),
  ac: {index: None, query: None, completions: list{}},
  cp: {index: 0, commands: list{}, location: None, filter: None},
  selectionStart: None,
  errorDvSrc: SourceNone,
  midClick: false,
  activeEditor: NoEditor,
}

let defaultFunctionsType = {
  builtinFunctions: list{},
  packageFunctions: TLID.Dict.empty,
  allowedFunctions: list{},
  previewUnsafeFunctions: Set.String.empty,
}

let defaultCanvasProps: canvasProps = {
  offset: origin,
  enablePan: true,
  lastOffset: None,
  panAnimation: DontAnimateTransition,
}

let defaultHandlerProp: handlerProp = {hoveringReferences: list{}, execution: Idle}

let defaultToast: toast = {toastMessage: None, toastPos: None}

let defaultAccount: account = {name: "", email: "", username: ""}

let defaultWorkerStats: workerStats = {count: 0, schedule: None}

let defaultMenu: menuState = {isOpen: false}

let defaultFnSpace: fnProps = {
  draggingParamIndex: None,
  dragOverSpaceIndex: None,
  justMovedParam: None,
}

let defaultModel: model = {
  error: Error.default,
  functions: defaultFunctionsType,
  lastMsg: IgnoreMsg("default"),
  opCtrs: Map.String.empty,
  clientOpCtrId: "",
  complete: // this is awkward, but avoids circular deps
  {
    admin: false,
    completions: list{},
    target: None,
    allCompletions: list{},
    index: -1,
    value: "",
    prevValue: "",
    visible: true,
  },
  currentPage: Architecture,
  hovering: list{},
  tests: list{},
  handlers: TLID.Dict.empty,
  deletedHandlers: TLID.Dict.empty,
  dbs: TLID.Dict.empty,
  deletedDBs: TLID.Dict.empty,
  userFunctions: TLID.Dict.empty,
  deletedUserFunctions: TLID.Dict.empty,
  userTipes: TLID.Dict.empty,
  deletedUserTipes: TLID.Dict.empty,
  analyses: Map.String.empty,
  traces: TLID.Dict.empty,
  f404s: list{},
  unlockedDBs: TLID.Set.empty,
  integrationTestState: NoIntegrationTest,
  visibility: PageVisibility.Visible /* partially saved in editor */,
  syncState: Set.String.empty,
  cursorState: Deselected,
  executingFunctions: list{},
  tlTraceIDs: TLID.Dict.empty,
  featureFlags: Map.String.empty,
  canvasProps: defaultCanvasProps,
  canvasName: "builtwithdark",
  userContentHost: "builtwithdark.com",
  origin: "",
  environment: "none",
  csrfToken: unsetCSRF,
  usedDBs: Map.String.empty,
  usedFns: Map.String.empty,
  usedTipes: Map.String.empty,
  handlerProps: TLID.Dict.empty,
  staticDeploys: list{},
  tlRefersTo: TLID.Dict.empty,
  tlUsedIn: TLID.Dict.empty,
  fluidState: defaultFluidState,
  dbStats: Map.String.empty,
  workerStats: TLID.Dict.empty,
  avatarsList: list{},
  browserId: "",
  sidebarState: defaultSidebar,
  isAdmin: false,
  buildHash: "",
  username: "defaultUsername",
  lastReload: None,
  permission: None,
  showTopbar: true,
  toast: defaultToast,
  account: defaultAccount,
  workerSchedules: Map.String.empty,
  searchCache: TLID.Dict.empty,
  editorSettings: {showFluidDebugger: false, showHandlerASTs: false, runTimers: true},
  teaDebuggerEnabled: false,
  unsupportedBrowser: false,
  tlMenus: TLID.Dict.empty,
  firstVisitToDark: true,
  tooltipState: {
    tooltipSource: None,
    fnSpace: false,
    userTutorial: {step: Some(Welcome), tlid: None},
  },
  currentUserFn: defaultFnSpace,
  firstVisitToThisCanvas: true,
  secrets: list{},
  settingsView: {
    opened: false,
    tab: UserSettings,
    canvasList: list{},
    username: "",
    orgs: list{},
    orgCanvasList: list{},
    loading: false,
    privacy: {recordConsent: None},
  },
  insertSecretModal: SecretTypes.defaultInsertModal,
}
