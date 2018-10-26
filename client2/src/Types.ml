type exception_ =
  { short: string
  ; long: string option
  ; exceptionTipe: string
  ; actual: string option
  ; actualType: string option
  ; result: string option
  ; resultType: string option
  ; expected: string option
  ; info: string Belt.Map.String.t
  ; workarounds: string list }

and tipe =
  | TInt
  | TStr
  | TChar
  | TBool
  | TFloat
  | TObj
  | TList
  | TAny
  | TNull
  | TBlock
  | TIncomplete
  | TError
  | TResp
  | TDB
  | TID
  | TDate
  | TTitle
  | TUrl
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TBelongsTo of string
  | THasMany of string
  | TDbList of tipe

and dhttp = Redirect of string | Response of int * (string * string) list

and optionT = OptJust of dval | OptNothing

and dval =
  | DInt of int
  | DFloat of float
  | DBool of bool
  | DNull
  | DChar of char
  | DStr of string
  | DList of dval list
  | DObj of dval Belt.Map.String.t
  | DIncomplete
  | DError of string
  | DBlock
  | DErrorRail of dval
  | DResp of dhttp * dval
  | DDB of string
  | DID of string
  | DDate of string
  | DTitle of string
  | DUrl of string
  | DPassword of string
  | DUuid of string
  | DOption of optionT

and pos = {x: int; y: int}

and vPos = {vx: int; vy: int}

and mouseEvent = {pos: vPos; button: int}

and isLeftButton = bool

and tlid = TLID of int

and id = ID of int

and entryCursor = Creating of pos | Filling of tlid * id

and hasMoved = bool

and cursorState =
  | Selecting of tlid * id option
  | Entering of entryCursor
  | Dragging of tlid * vPos * hasMoved * cursorState
  | SelectingCommand of tlid * id
  | Deselected

and timerAction = RefreshAnalysis | CheckUrlHashPosition

and globalVariable = string

and rpcResult =
  toplevel list
  * toplevel list
  * traces
  * globalVariable list
  * userFunction list
  * tlid list

and dvalArgsHash = string

and executeFunctionRPCResult = dval * dvalArgsHash

and getAnalysisResult =
  traces * globalVariable list * fourOhFour list * tlid list

and initialLoadResult = rpcResult

and msg =
  | GlobalClick of mouseEvent
  | NothingClick of mouseEvent
  | ToplevelMouseDown of tlid * mouseEvent
  | ToplevelMouseUp of tlid * mouseEvent
  | ToplevelClick of tlid * mouseEvent
  | DragToplevel of tlid * Tea.Mouse.position
  | EntryInputMsg of string
  | EntrySubmitMsg
  | GlobalKeyPress of Keyboard.keyEvent
  | AutocompleteClick of string
  | FocusEntry of (unit, Dom.errorEvent) Tea.Result.t
  | FocusAutocompleteItem of (unit, Dom.errorEvent) Tea.Result.t
  | RPCCallback of focus * rpcParams * (rpcResult, string Tea.Http.error) Tea.Result.t
  | SaveTestRPCCallback of (string, string Tea.Http.error) Tea.Result.t
  | GetAnalysisRPCCallback of (getAnalysisResult, string Tea.Http.error) Tea.Result.t
  | GetDelete404RPCCallback of (fourOhFour list, string Tea.Http.error) Tea.Result.t
  | InitialLoadRPCCallback of
      focus * modification * (initialLoadResult, string Tea.Http.error) Tea.Result.t
  | LocationChange of Web.Location.location
  | AddRandom
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleTimers
  | ExecuteFunctionRPCCallback of
      executeFunctionRPCParams
      * (executeFunctionRPCResult, string Tea.Http.error) Tea.Result.t
  | ExecuteFunctionButton of tlid * id * string
  | ExecuteFunctionCancel of tlid * id
  | Initialization
  | CreateHandlerFrom404 of fourOhFour
  | Delete404 of fourOhFour
  | WindowResize of int * int
  | TimerFire of timerAction * Tea.Time.t
  | JSError of string
  | PageVisibilityChange of Porting.PageVisibility.visibility
  | PageFocusChange of Porting.PageVisibility.visibility
  | StartFeatureFlag
  | EndFeatureFlag of id * pick
  | ToggleFeatureFlag of id * bool
  | DeleteUserFunctionParameter of userFunction * userFunctionParameter
  | BlankOrClick of tlid * id * mouseEvent
  | BlankOrDoubleClick of tlid * id * mouseEvent
  | BlankOrMouseEnter of tlid * id * mouseEvent
  | BlankOrMouseLeave of tlid * id * mouseEvent
  | MouseWheel of int * int
  | DataClick of tlid * int * mouseEvent
  | DataMouseEnter of tlid * int * mouseEvent
  | DataMouseLeave of tlid * int * mouseEvent
  | CreateRouteHandler
  | CreateDBTable
  | CreateFunction
  | ExtractFunction
  | DeleteUserFunction of tlid
  | RestoreToplevel of tlid
  | LockHandler of tlid * bool
  | ReceiveAnalysis of string
  | EnablePanning of bool
  | ShowErrorDetails of bool
  | StartMigration of tlid
  | AbandonMigration of tlid
  | DeleteColInDB of tlid * id
  [@@bs.deriving {accessors}]

and predecessor = pointerData option

and successor = pointerData option

and focus =
  | FocusNothing
  | FocusExact of tlid * id
  | FocusNext of tlid * id option
  | FocusPageAndCursor of page * cursorState
  | FocusSame
  | FocusNoChange

and rollbackID = id

and rollforwardID = id

and op =
  | SetHandler of tlid * pos * handler
  | CreateDB of tlid * pos * dBName
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * dBColName
  | SetDBColType of tlid * id * dBColType
  | DeleteTL of tlid
  | MoveTL of tlid * pos
  | TLSavepoint of tlid
  | UndoTL of tlid
  | RedoTL of tlid
  | SetFunction of userFunction
  | DeleteFunction of tlid
  | ChangeDBColName of tlid * id * dBColName
  | ChangeDBColType of tlid * id * dBColType
  | DeprecatedInitDbm of
      tlid * id * rollbackID * rollforwardID * dBMigrationKind
  | SetExpr of tlid * id * expr
  | CreateDBMigration of tlid * rollbackID * rollforwardID * dBColumn list
  | AddDBColToDBMigration of tlid * id * id
  | SetDBColNameInDBMigration of tlid * id * dBColName
  | SetDBColTypeInDBMigration of tlid * id * dBColType
  | DeleteColInDBMigration of tlid * id
  | AbandonDBMigration of tlid

and rpcParams = {ops: op list}

and executeFunctionRPCParams =
  { efpTLID: tlid
  ; efpTraceID: traceID
  ; efpCallerID: id
  ; efpArgs: dval list
  ; efpFnName: string }

and analysisParams = tlid list

and delete404Param = fourOhFour

and autocomplete =
  { functions: function_ list
  ; admin: bool
  ; completions: autocompleteItem list list
  ; allCompletions: autocompleteItem list
  ; index: int
  ; value: string
  ; prevValue: string
  ; target: (tlid * pointerData) option
  ; acTipe: tipe option
  ; isCommandMode: bool }

and stringEntryPermission = StringEntryAllowed | StringEntryNotAllowed

and stringEntryWidth = StringEntryNormalWidth | StringEntryShortWidth

and literal = string

and omniAction =
  | NewDB of dBName
  | NewHandler
  | NewFunction of string option
  | NewHTTPHandler
  | NewHTTPRoute of string
  | NewEventSpace of string

and keyword = KLet | KIf | KLambda

and command =
  { commandName: string
  ; action: model -> toplevel -> pointerData -> modification
  ; doc: string
  ; shortcut: string }

and autocompleteItem =
  | ACFunction of function_
  | ACField of string
  | ACVariable of varName
  | ACExtra of string
  | ACLiteral of literal
  | ACOmniAction of omniAction
  | ACKeyword of keyword
  | ACCommand of command

and target = tlid * pointerData

and autocompleteMod =
  | ACSetQuery of string
  | ACAppendQuery of string
  | ACReset
  | ACSelectDown
  | ACSelectUp
  | ACSetTarget of target option
  | ACRegenerate
  | ACEnableCommandMode

and variantTest = StubVariant

and class_ = string

and pick = PickA | PickB

and ffIsExpanded = bool

and flagsVS = ffIsExpanded Belt.Map.Int.t

and varName = string

and fnName = string

and fieldName = string

and keyName = string

and varBind = varName blankOr

and field = fieldName blankOr

and key = keyName blankOr

and lambdaParameter = varName blankOr

and expr = nExpr blankOr

and sendToRail = Rail | NoRail

and nExpr =
  | If of expr * expr * expr
  | FnCall of fnName * expr list * sendToRail
  | Variable of varName
  | Let of varBind * expr * expr
  | Lambda of lambdaParameter list * expr
  | Value of string
  | ObjectLiteral of (key * expr) list
  | ListLiteral of expr list
  | Thread of expr list
  | FieldAccess of expr * field
  | FeatureFlag of string blankOr * expr * expr * expr

and pointerData =
  | PVarBind of varBind
  | PEventName of string blankOr
  | PEventModifier of string blankOr
  | PEventSpace of string blankOr
  | PExpr of expr
  | PField of field
  | PKey of string blankOr
  | PDBColName of string blankOr
  | PDBColType of string blankOr
  | PFFMsg of string blankOr
  | PFnName of string blankOr
  | PParamName of string blankOr
  | PParamTipe of tipe blankOr

and pointerType =
  | VarBind
  | EventName
  | EventSpace
  | EventModifier
  | Expr
  | Field
  | Key
  | DBColName
  | DBColType
  | FFMsg
  | FnName
  | ParamName
  | ParamTipe

and 'a blankOr = Blank of id | F of id * 'a

and pointerOwner = POSpecHeader | POAst | PODb

and handlerSpec =
  {module_: string blankOr; name: string blankOr; modifier: string blankOr}

and handlerSpace = HSHTTP | HSCron | HSOther | HSEmpty

and handler = {ast: expr; spec: handlerSpec; tlid: tlid}

and dBName = string

and dBColName = string

and dBColType = string

and dBColumn = dBColName blankOr * dBColType blankOr

and dBMigrationKind = DeprecatedMigrationKind

and dBMigrationState = DBMigrationAbandoned | DBMigrationInitialized

and dBMigration =
  { startingVersion: int
  ; version: int
  ; state: dBMigrationState
  ; rollforward: expr
  ; rollback: expr
  ; cols: dBColumn list }

and dB =
  { dbTLID: tlid
  ; dbName: dBName
  ; cols: dBColumn list
  ; version: int
  ; oldMigrations: dBMigration list
  ; activeMigration: dBMigration option }

and tLData = TLHandler of handler | TLDB of dB | TLFunc of userFunction

and toplevel = {id: tlid; pos: pos; data: tLData}

and lvDict = dval Belt.Map.Int.t

and avDict = varName list Belt.Map.Int.t

and analysisResults = {liveValues: lvDict; availableVarnames: avDict}

and analyses = analysisResults Belt.Map.String.t

and inputValueDict = dval Belt.Map.String.t

and functionResult =
  {fnName: string; callerID: id; argHash: string; value: dval}

and traceID = string

and trace =
  { traceID: traceID
  ; input: inputValueDict
  ; functionResults: functionResult list }

and traces = trace list Belt.Map.Int.t

and fourOhFour = {space: string; path: string; modifier: string}

and name = string

and page = Toplevels of pos | Fn of tlid * pos

and clipboard = pointerData option

and syncState = {inFlight: bool; ticks: int}

and urlState = {lastPos: pos}

and tLCursors = int Belt.Map.Int.t

and canvasProps = {offset: pos; fnOffset: pos; enablePan: bool}

and model =
  { error: darkError
  ; lastMsg: msg
  ; lastMod: modification
  ; tests: variantTest list
  ; complete: autocomplete
  ; userFunctions: userFunction list
  ; builtInFunctions: function_ list
  ; cursorState: cursorState
  ; currentPage: page
  ; hovering: id list
  ; toplevels: toplevel list
  ; deletedToplevels: toplevel list
  ; traces: traces
  ; analyses: analyses
  ; globals: globalVariable list
  ; f404s: fourOhFour list
  ; unlockedDBs: tlid list
  ; integrationTestState: integrationTestState
  ; visibility: Porting.PageVisibility.visibility
  ; clipboard: clipboard
  ; syncState: syncState
  ; urlState: urlState
  ; timersEnabled: bool
  ; executingFunctions: (tlid * id) list
  ; tlCursors: tLCursors
  ; featureFlags: flagsVS
  ; lockedHandlers: tlid list
  ; canvas: canvasProps
  ; canvasName: string
  ; userContentHost: string
  ; environment: string }

and serializableEditor =
  { clipboard: pointerData option
  ; timersEnabled: bool
  ; cursorState: cursorState
  ; lockedHandlers: tlid list }

and darkError = {message: string option; showDetails: bool}

and testResult = (string, unit) Porting.Result.t

and integrationTestState =
  | IntegrationTestExpectation of (model -> testResult)
  | IntegrationTestFinished of testResult
  | NoIntegrationTest

[@bs.deriving accessors]
and modification =
  | DisplayAndReportHttpError of string * string Tea.Http.error
  | DisplayAndReportError of string
  | DisplayError of string
  | ClearError
  | Select of tlid * id option
  | SelectCommand of tlid * id
  | SetHover of id
  | ClearHover of id
  | Deselect
  | RemoveToplevel of toplevel
  | SetToplevels of toplevel list * bool
  | UpdateToplevels of toplevel list * bool
  | SetDeletedToplevels of toplevel list
  | UpdateDeletedToplevels of toplevel list
  | UpdateAnalysis of traceID * analysisResults
  | RequestAnalysis of toplevel list
  | SetGlobalVariables of globalVariable list
  | SetUserFunctions of userFunction list * bool
  | SetUnlockedDBs of tlid list
  | Set404s of fourOhFour list
  | Enter of entryCursor
  | RPCFull of (rpcParams * focus)
  | RPC of (op list * focus)
  | GetAnalysisRPC
  | NoChange
  | MakeCmd of msg Tea.Cmd.t
  | AutocompleteMod of autocompleteMod
  | Many of modification list
  | Drag of tlid * vPos * hasMoved * cursorState
  | TriggerIntegrationTest of string
  | EndIntegrationTest
  | SetCursorState of cursorState
  | SetPage of page
  | SetCenter of pos
  | CopyToClipboard of clipboard
  | SetCursor of tlid * int
  | ExecutingFunctionBegan of tlid * id
  | ExecutingFunctionRPC of tlid * id * string
  | ExecutingFunctionComplete of (tlid * id) list
  | SetLockedHandlers of tlid list
  | MoveCanvasTo of canvasProps * page * pos
  | UpdateTraces of traces
  | UpdateTraceFunctionResult of
      tlid * traceID * id * fnName * dvalArgsHash * dval
  | TweakModel of (model -> model)

and parameter =
  { paramName: string
  ; paramTipe: tipe
  ; paramBlock_args: string list
  ; paramOptional: bool
  ; paramDescription: string }

and function_ =
  { fnName: string
  ; fnParameters: parameter list
  ; fnDescription: string
  ; fnReturnTipe: tipe
  ; fnPreviewExecutionSafe: bool
  ; fnDeprecated: bool
  ; fnInfix: bool }

and userFunctionParameter =
  { ufpName: string blankOr
  ; ufpTipe: tipe blankOr
  ; ufpBlock_args: string list
  ; ufpOptional: bool
  ; ufpDescription: string }

and userFunctionMetadata =
  { ufmName: string blankOr
  ; ufmParameters: userFunctionParameter list
  ; ufmDescription: string
  ; ufmReturnTipe: tipe blankOr
  ; ufmInfix: bool }

and userFunction = {ufTLID: tlid; ufMetadata: userFunctionMetadata; ufAST: expr}
