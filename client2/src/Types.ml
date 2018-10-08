open Belt
open Porting

type exception_ =
  { short: string
  ; long: string option
  ; tipe: string
  ; actual: string option
  ; actualType: string option
  ; result: string option
  ; resultType: string option
  ; expected: string option
  ; info: string Belt.Map.String.t
  ; workarounds: string list }

type tipe =
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
  | TErroror
  | TResp
  | TDB
  | TID
  | TDate
  | TTitle
  | TUrl
  | TPassword
  | TUuid
  | TOption
  | TErrororRail
  | TBelongsTo of string
  | THasMany of string
  | TDbList of tipe

type dhttp = Redirect of string | Response of int * (string * string) list

type optionT = OptSome of dval | OptNone

type dval =
  | DInt of int
  | DFloat of float
  | DBool of bool
  | DNull
  | DChar of char
  | DStr of string
  | DList of dval list
  | DObj of dval Belt.Map.String.t
  | DIncomplete
  | DErroror of string
  | DBlock
  | DErrororRail of dval
  | DResp of dhttp * dval
  | DDB of string
  | DID of string
  | DDate of string
  | DTitle of string
  | DUrl of string
  | DPassword of string
  | DUuid of string
  | DOption of optionT

type pos = {x: int; y: int}

type vPos = {vx: int; vy: int}

type mouseEvent = {pos: vPos; button: int}

type isLeftButton = bool

type tLID = TLID of int

type iD = ID of int

type darkKeyboardEvent =
  { standard: keyboardEvent
  ; selectionStart: int option
  ; selectionEnd: int option }

type entryCursor = Creating of pos | Filling of tLID * iD

type hasMoved = bool

type cursorState =
  | Selecting of tLID * iD option
  | Entering of entryCursor
  | Dragging of tLID * vPos * hasMoved * cursorState
  | SelectingCommand of tLID * iD
  | Deselected

type timerAction = RefreshAnalysis | CheckUrlHashPosition

type globalVariable = string

type rPCResult =
  toplevel list
  * toplevel list
  * traces
  * globalVariable list
  * userFunction list
  * tLID list

type dvalArgsHash = string

type executeFunctionRPCResult = dval * dvalArgsHash

type getAnalysisResult =
  traces * globalVariable list * fourOhFour list * tLID list

type initialLoadResult = rPCResult

type msg =
  | GlobalClick of mouseEvent
  | NoneClick of mouseEvent
  | ToplevelMouseDown of tLID * mouseEvent
  | ToplevelMouseUp of tLID * mouseEvent
  | ToplevelClick of tLID * mouseEvent
  | DragToplevel of tLID * Mouse.position
  | EntryInputMsg of string
  | EntrySubmitMsg
  | GlobalKeyPress of darkKeyboardEvent
  | AutocompleteClick of string
  | FocusEntry of (Dom.error, unit) result
  | FocusAutocompleteItem of (Dom.error, unit) result
  | RPCCallback of focus * rPCParams * (Http.error, rPCResult) result
  | SaveTestRPCCallback of (Http.error, string) result
  | GetAnalysisRPCCallback of (Http.error, getAnalysisResult) result
  | GetDelete404RPCCallback of (Http.error, fourOhFour list) result
  | InitialLoadRPCCallback of
      focus * modification * (Http.error, initialLoadResult) result
  | LocationChange of Navigation.location
  | AddRandom
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleTimers
  | ExecuteFunctionRPCCallback of
      executeFunctionRPCParams * (Http.error, executeFunctionRPCResult) result
  | ExecuteFunctionButton of tLID * iD * string
  | ExecuteFunctionCancel of tLID * iD
  | Initialization
  | CreateHandlerFrom404 of fourOhFour
  | Delete404 of fourOhFour
  | WindowResize of int * int
  | TimerFire of timerAction * time
  | JSErroror of string
  | PageVisibilityChange of PageVisibility.visibility
  | PageFocusChange of PageVisibility.visibility
  | StartFeatureFlag
  | EndFeatureFlag of iD * pick
  | ToggleFeatureFlag of iD * bool
  | DeleteUserFunctionParameter of userFunction * userFunctionParameter
  | BlankOrClick of tLID * iD * mouseEvent
  | BlankOrDoubleClick of tLID * iD * mouseEvent
  | BlankOrMouseEnter of tLID * iD * mouseEvent
  | BlankOrMouseLeave of tLID * iD * mouseEvent
  | MouseWheel of (int * int)
  | DataClick of tLID * int * mouseEvent
  | DataMouseEnter of tLID * int * mouseEvent
  | DataMouseLeave of tLID * int * mouseEvent
  | CreateRouteHandler
  | CreateDBTable
  | CreateFunction
  | ExtractFunction
  | DeleteUserFunction of tLID
  | RestoreToplevel of tLID
  | LockHandler of tLID * bool
  | ReceiveAnalysis of string
  | EnablePanning of bool
  | ShowErrororDetails of bool
  | StartMigration of tLID
  | AbandonMigration of tLID
  | DeleteColInDB of tLID * iD

type predecessor = pointerData option

type successor = pointerData option

type focus =
  | FocusNone
  | FocusExact of tLID * iD
  | FocusNext of tLID * iD option
  | FocusPageAndCursor of page * cursorState
  | FocusSame
  | FocusNoChange

type rollbackID = iD

type rollforwardID = iD

type op =
  | SetHandler of tLID * pos * handler
  | CreateDB of tLID * pos * dBName
  | AddDBCol of tLID * iD * iD
  | SetDBColName of tLID * iD * dBColName
  | SetDBColType of tLID * iD * dBColType
  | DeleteTL of tLID
  | MoveTL of tLID * pos
  | TLSavepoint of tLID
  | UndoTL of tLID
  | RedoTL of tLID
  | SetFunction of userFunction
  | DeleteFunction of tLID
  | ChangeDBColName of tLID * iD * dBColName
  | ChangeDBColType of tLID * iD * dBColType
  | DeprecatedInitDbm of
      tLID * iD * rollbackID * rollforwardID * dBMigrationKind
  | SetExpr of tLID * iD * expr
  | CreateDBMigration of tLID * rollbackID * rollforwardID * dBColumn list
  | AddDBColToDBMigration of tLID * iD * iD
  | SetDBColNameInDBMigration of tLID * iD * dBColName
  | SetDBColTypeInDBMigration of tLID * iD * dBColType
  | DeleteColInDBMigration of tLID * iD
  | AbandonDBMigration of tLID

type rPCParams = {ops: op list}

type executeFunctionRPCParams =
  {tlid: tLID; traceID: traceID; callerID: iD; args: dval list; fnName: string}

type analysisParams = tLID list

type delete404Param = fourOhFour

type autocomplete =
  { functions: function_ list
  ; admin: bool
  ; completions: autocompleteItem list list
  ; allCompletions: autocompleteItem list
  ; index: int
  ; value: string
  ; prevValue: string
  ; target: (tLID * pointerData) option
  ; tipe: tipe option
  ; isCommandMode: bool }

type stringEntryPermission = StringEntryAllowed | StringEntryNotAllowed

type stringEntryWidth = StringEntryNormalWidth | StringEntryShortWidth

type literal = string

type omniAction =
  | NewDB of dBName
  | NewHandler
  | NewFunction of string option
  | NewHTTPHandler
  | NewHTTPRoute of string
  | NewEventSpace of string

type keyword = KLet | KIf | KLambda

type command =
  { name: string
  ; action: ((model -> toplevel) -> pointerData) -> modification
  ; doc: string
  ; shortcut: string }

type autocompleteItem =
  | ACFunction of function_
  | ACField of string
  | ACVariable of varName
  | ACExtra of string
  | ACLiteral of literal
  | ACOmniAction of omniAction
  | ACKeyword of keyword
  | ACCommand of command

type target = tLID * pointerData

type autocompleteMod =
  | ACSetQuery of string
  | ACAppendQuery of string
  | ACReset
  | ACSelectDown
  | ACSelectUp
  | ACSetTarget of target option
  | ACRegenerate
  | ACEnableCommandMode

type variantTest = StubVariant

type class_ = string

type pick = PickA | PickB

type fFIsExpanded = bool

type flagsVS = fFIsExpanded Belt.Map.Int.t

type varName = string

type fnName = string

type fieldName = string

type keyName = string

type varBind = varName blankOr

type field = fieldName blankOr

type key = keyName blankOr

type lambdaParameter = varName blankOr

type expr = nExpr blankOr

type sendToRail = Rail | NoRail

type nExpr =
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

type pointerData =
  | PVarBind of varBind
  | PEventName of string blankOr
  | PEventModifier of string blankOr
  | PEventSpace of string blankOr
  | PExpr of expr
  | PField of field
  | PKey of string blankOr
  | PDBColName of string blankOr
  | PDBColType of string blankOr
  | PDarkType of darkType
  | PDarkTypeField of string blankOr
  | PFFMsg of string blankOr
  | PFnName of string blankOr
  | PParamName of string blankOr
  | PParamTipe of tipe blankOr

type pointerType =
  | VarBind
  | EventName
  | EventSpace
  | EventModifier
  | Expr
  | Field
  | Key
  | DBColName
  | DBColType
  | DarkType
  | DarkTypeField
  | FFMsg
  | FnName
  | ParamName
  | ParamTipe

type 'a blankOr = Blank of iD | F of iD * 'a

type pointerOwner = POSpecHeader | POAst | PODb | POSpecType

type darkType = nDarkType blankOr

type nDarkType =
  | DTEmpty
  | DTAny
  | DTString
  | DTInt
  | DTObj of (string blankOr * darkType) list

type specTypes = {input: darkType; output: darkType}

type handlerSpec =
  { module_: string blankOr
  ; name: string blankOr
  ; modifier: string blankOr
  ; types: specTypes }

type handlerSpace = HSHTTP | HSCron | HSOther | HSEmpty

type handler = {ast: expr; spec: handlerSpec; tlid: tLID}

type dBName = string

type dBColName = string

type dBColType = string

type dBColumn = dBColName blankOr * dBColType blankOr

type dBMigrationKind = DeprecatedMigrationKind

type dBMigrationState = DBMigrationAbandoned | DBMigrationInitialized

type dBMigration =
  { startingVersion: int
  ; version: int
  ; state: dBMigrationState
  ; rollforward: expr
  ; rollback: expr
  ; cols: dBColumn list }

type dB =
  { tlid: tLID
  ; name: dBName
  ; cols: dBColumn list
  ; version: int
  ; oldMigrations: dBMigration list
  ; activeMigration: dBMigration option }

type tLData = TLHandler of handler | TLDB of dB | TLFunc of userFunction

type toplevel = {id: tLID; pos: pos; data: tLData}

type lVDict = dval Belt.Map.Int.t

type aVDict = varName list Belt.Map.Int.t

type analysisResults = {liveValues: lVDict; availableVarnames: aVDict}

type analyses = analysisResults Belt.Map.String.t

type inputValueDict = (varName, dval) dict

type functionResult =
  {fnName: string; callerID: iD; argHash: string; value: dval}

type traceID = string

type trace =
  {id: traceID; input: inputValueDict; functionResults: functionResult list}

type traces = trace list Belt.Map.Int.t

type fourOhFour = {space: string; path: string; modifier: string}

type name = string

type page = Toplevels of pos | Fn of tLID * pos

type clipboard = pointerData option

type syncState = {inFlight: bool; ticks: int}

type urlState = {lastPos: pos}

type tLCursors = int Belt.Map.Int.t

type canvasProps = {offset: pos; fnOffset: pos; enablePan: bool}

type model =
  { error: darkError
  ; lastMsg: msg
  ; lastMod: modification
  ; tests: variantTest list
  ; complete: autocomplete
  ; userFunctions: userFunction list
  ; builtInFunctions: function_ list
  ; cursorState: cursorState
  ; currentPage: page
  ; hovering: iD list
  ; toplevels: toplevel list
  ; deletedToplevels: toplevel list
  ; traces: traces
  ; analyses: analyses
  ; globals: globalVariable list
  ; f404s: fourOhFour list
  ; unlockedDBs: tLID list
  ; integrationTestState: integrationTestState
  ; visibility: PageVisibility.visibility
  ; clipboard: clipboard
  ; syncState: syncState
  ; urlState: urlState
  ; timersEnabled: bool
  ; executingFunctions: (tLID * iD) list
  ; tlCursors: tLCursors
  ; featureFlags: flagsVS
  ; lockedHandlers: tLID list
  ; canvas: canvasProps
  ; canvasName: string
  ; userContentHost: string
  ; environment: string }

type serializableEditor =
  { clipboard: pointerData option
  ; timersEnabled: bool
  ; cursorState: cursorState
  ; lockedHandlers: tLID list }

type darkError = {message: string option; showDetails: bool}

type testResult = (string, unit) result

type integrationTestState =
  | IntegrationTestExpectation of (model -> testResult)
  | IntegrationTestFinished of testResult
  | NoIntegrationTest

type modification =
  | DisplayAndReportHttpErroror of string * Http.error
  | DisplayAndReportErroror of string
  | DisplayErroror of string
  | ClearErroror
  | Select of tLID * iD option
  | SelectCommand of tLID * iD
  | SetHover of iD
  | ClearHover of iD
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
  | SetUnlockedDBs of tLID list
  | Set404s of fourOhFour list
  | Enter of entryCursor
  | RPCFull of (rPCParams * focus)
  | RPC of (op list * focus)
  | GetAnalysisRPC
  | NoChange
  | MakeCmd of msg cmd
  | AutocompleteMod of autocompleteMod
  | Many of modification list
  | Drag of tLID * vPos * hasMoved * cursorState
  | TriggerIntegrationTest of string
  | EndIntegrationTest
  | SetCursorState of cursorState
  | SetPage of page
  | SetCenter of pos
  | CopyToClipboard of clipboard
  | SetCursor of tLID * int
  | ExecutingFunctionBegan of tLID * iD
  | ExecutingFunctionRPC of tLID * iD * string
  | ExecutingFunctionComplete of (tLID * iD) list
  | SetLockedHandlers of tLID list
  | MoveCanvasTo of canvasProps * page * pos
  | UpdateTraces of traces
  | UpdateTraceFunctionResult of
      tLID * traceID * iD * fnName * dvalArgsHash * dval
  | TweakModel of (model -> model)

type flags =
  { editorState: string option
  ; complete: flagFunction list
  ; userContentHost: string
  ; environment: string }

type parameter =
  { name: string
  ; tipe: tipe
  ; block_args: string list
  ; optional: bool
  ; description: string }

type function_ =
  { name: string
  ; parameters: parameter list
  ; description: string
  ; returnTipe: tipe
  ; previewExecutionSafe: bool
  ; deprecated: bool
  ; infix: bool }

type userFunctionParameter =
  { name: string blankOr
  ; tipe: tipe blankOr
  ; block_args: string list
  ; optional: bool
  ; description: string }

type userFunctionMetadata =
  { name: string blankOr
  ; parameters: userFunctionParameter list
  ; description: string
  ; returnTipe: tipe blankOr
  ; infix: bool }

type userFunction = {tlid: tLID; metadata: userFunctionMetadata; ast: expr}

type flagParameter =
  { name: string
  ; tipe: string
  ; block_args: string list
  ; optional: bool
  ; description: string }

type flagFunction =
  { name: string
  ; parameters: flagParameter list
  ; description: string
  ; return_type: string
  ; preview_execution_safe: bool
  ; deprecated: bool
  ; infix: bool }
