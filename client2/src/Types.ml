open Belt
open Tea
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

and dhttp = Redirect of string | Response of int * (string * string) list

and optionT = OptSome of dval | OptNone

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

and pos = {x: int; y: int}

and vPos = {vx: int; vy: int}

and mouseEvent = {pos: vPos; button: int}

and isLeftButton = bool

and tLID = TLID of int

and iD = ID of int

and darkKeyboardEvent =
  { standard: Dom.keyboardEvent
  ; selectionStart: int option
  ; selectionEnd: int option }

and entryCursor = Creating of pos | Filling of tLID * iD

and hasMoved = bool

and cursorState =
  | Selecting of tLID * iD option
  | Entering of entryCursor
  | Dragging of tLID * vPos * hasMoved * cursorState
  | SelectingCommand of tLID * iD
  | Deselected

and timerAction = RefreshAnalysis | CheckUrlHashPosition

and globalVariable = string

and rPCResult =
  toplevel list
  * toplevel list
  * traces
  * globalVariable list
  * userFunction list
  * tLID list

and dvalArgsHash = string

and executeFunctionRPCResult = dval * dvalArgsHash

and getAnalysisResult =
  traces * globalVariable list * fourOhFour list * tLID list

and initialLoadResult = rPCResult

and msg =
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
  | FocusEntry of (Dom.errorEvent, unit) result
  | FocusAutocompleteItem of (Dom.errorEvent, unit) result
  | RPCCallback of focus * rPCParams * (string Http.error, rPCResult) result
  | SaveTestRPCCallback of (string Http.error, string) result
  | GetAnalysisRPCCallback of (string Http.error, getAnalysisResult) result
  | GetDelete404RPCCallback of (string Http.error, fourOhFour list) result
  | InitialLoadRPCCallback of
      focus * modification * (string Http.error, initialLoadResult) result
  | LocationChange of Web.Location.location
  | AddRandom
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleTimers
  | ExecuteFunctionRPCCallback of
      executeFunctionRPCParams
      * (string Http.error, executeFunctionRPCResult) result
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

and predecessor = pointerData option

and successor = pointerData option

and focus =
  | FocusNone
  | FocusExact of tLID * iD
  | FocusNext of tLID * iD option
  | FocusPageAndCursor of page * cursorState
  | FocusSame
  | FocusNoChange

and rollbackID = iD

and rollforwardID = iD

and op =
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

and rPCParams = {ops: op list}

and executeFunctionRPCParams =
  {tlid: tLID; traceID: traceID; callerID: iD; args: dval list; fnName: string}

and analysisParams = tLID list

and delete404Param = fourOhFour

and autocomplete =
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
  { name: string
  ; action: ((model -> toplevel) -> pointerData) -> modification
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

and target = tLID * pointerData

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

and fFIsExpanded = bool

and flagsVS = fFIsExpanded Belt.Map.Int.t

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
  | PDarkType of darkType
  | PDarkTypeField of string blankOr
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
  | DarkType
  | DarkTypeField
  | FFMsg
  | FnName
  | ParamName
  | ParamTipe

and 'a blankOr = Blank of iD | F of iD * 'a

and pointerOwner = POSpecHeader | POAst | PODb | POSpecType

and darkType = nDarkType blankOr

and nDarkType =
  | DTEmpty
  | DTAny
  | DTString
  | DTInt
  | DTObj of (string blankOr * darkType) list

and specTypes = {input: darkType; output: darkType}

and handlerSpec =
  { module_: string blankOr
  ; name: string blankOr
  ; modifier: string blankOr
  ; types: specTypes }

and handlerSpace = HSHTTP | HSCron | HSOther | HSEmpty

and handler = {ast: expr; spec: handlerSpec; tlid: tLID}

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
  { tlid: tLID
  ; name: dBName
  ; cols: dBColumn list
  ; version: int
  ; oldMigrations: dBMigration list
  ; activeMigration: dBMigration option }

and tLData = TLHandler of handler | TLDB of dB | TLFunc of userFunction

and toplevel = {id: tLID; pos: pos; data: tLData}

and lVDict = dval Belt.Map.Int.t

and aVDict = varName list Belt.Map.Int.t

and analysisResults = {liveValues: lVDict; availableVarnames: aVDict}

and analyses = analysisResults Belt.Map.String.t

and inputValueDict = (varName, dval) dict

and functionResult =
  {fnName: string; callerID: iD; argHash: string; value: dval}

and traceID = string

and trace =
  {id: traceID; input: inputValueDict; functionResults: functionResult list}

and traces = trace list Belt.Map.Int.t

and fourOhFour = {space: string; path: string; modifier: string}

and name = string

and page = Toplevels of pos | Fn of tLID * pos

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

and serializableEditor =
  { clipboard: pointerData option
  ; timersEnabled: bool
  ; cursorState: cursorState
  ; lockedHandlers: tLID list }

and darkError = {message: string option; showDetails: bool}

and testResult = (string, unit) result

and integrationTestState =
  | IntegrationTestExpectation of (model -> testResult)
  | IntegrationTestFinished of testResult
  | NoIntegrationTest

and modification =
  | DisplayAndReportHttpErroror of string * string Http.error
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

and flags =
  { editorState: string option
  ; complete: flagFunction list
  ; userContentHost: string
  ; environment: string }

and parameter =
  { name: string
  ; tipe: tipe
  ; block_args: string list
  ; optional: bool
  ; description: string }

and function_ =
  { name: string
  ; parameters: parameter list
  ; description: string
  ; returnTipe: tipe
  ; previewExecutionSafe: bool
  ; deprecated: bool
  ; infix: bool }

and userFunctionParameter =
  { name: string blankOr
  ; tipe: tipe blankOr
  ; block_args: string list
  ; optional: bool
  ; description: string }

and userFunctionMetadata =
  { name: string blankOr
  ; parameters: userFunctionParameter list
  ; description: string
  ; returnTipe: tipe blankOr
  ; infix: bool }

and userFunction = {tlid: tLID; metadata: userFunctionMetadata; ast: expr}

and flagParameter =
  { name: string
  ; tipe: string
  ; block_args: string list
  ; optional: bool
  ; description: string }

and flagFunction =
  { name: string
  ; parameters: flagParameter list
  ; description: string
  ; return_type: string
  ; preview_execution_safe: bool
  ; deprecated: bool
  ; infix: bool }
