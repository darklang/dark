open Tc

let show_list ~(f : 'a -> string) (x : 'a list) : string =
  "[" ^ String.join ~sep:"," (List.map ~f x) ^ "]"


let opaque msg fmt _ =
  Format.pp_print_string fmt ("<opaque:" ^ msg ^ ">") ;
  ()


(* Probably deletable? *)
module PageVisibility = struct
  type visibility =
    | Hidden
    | Visible
  [@@deriving show]
end

type exception_ =
  { short : string
  ; long : string option
  ; exceptionTipe : string
  ; actual : string option
  ; actualType : string option
  ; result : string option
  ; resultType : string option
  ; expected : string option
  ; info : string StrDict.t
  ; workarounds : string list }

(* ---------------------- *)
(* Basic types *)
(* ---------------------- *)
and tlid = TLID of string

and id = ID of string

and 'a blankOr =
  | Blank of id
  | F of id * 'a

(* There are two coordinate systems. Pos is an absolute position in the *)
(* canvas. Nodes and Edges have Pos'. VPos is the viewport: clicks occur *)
(* within the viewport and we map Absolute positions back to the *)
(* viewport to display in the browser. *)
(* TODO: Can we depreciate VPos? *)
and pos =
  { x : int
  ; y : int }

and vPos =
  { vx : int
  ; vy : int }

and size =
  { w : int
  ; h : int }

and box = pos * size

(* ---------------------- *)
(* Types *)
(* ---------------------- *)
and tipe =
  | TInt
  | TStr
  | TDeprecated1
  | TCharacter
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
  | TDeprecated4
  | TDate
  | TDeprecated2
  | TDeprecated3
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TResult
  | TBelongsTo of string
  | THasMany of string
  | TDbList of tipe
  | TUserType of string * int
  | TBytes

(* ---------------------- *)
(* Exprs and AST types *)
(* ---------------------- *)
and varName = string

and fnName = string

and fieldName = string

and keyName = string

and varBind = varName blankOr

and field = fieldName blankOr

and key = keyName blankOr

and lambdaParameter = varName blankOr

and sendToRail =
  | Rail
  | NoRail

and nPattern =
  | PVariable of varName
  | PLiteral of string
  | PConstructor of string * pattern list

and pattern = nPattern blankOr

and expr = nExpr blankOr

and nExpr =
  | If of expr * expr * expr
  | FnCall of fnName blankOr * expr list * sendToRail
  | Variable of varName
  | Let of varBind * expr * expr
  | Lambda of lambdaParameter list * expr
  | Value of string
  | ObjectLiteral of (key * expr) list
  | ListLiteral of expr list
  | Thread of expr list
  | FieldAccess of expr * field
  | FeatureFlag of string blankOr * expr * expr * expr
  | Match of expr * (pattern * expr) list
  | Constructor of string blankOr * expr list
  | FluidPartial of string * expr
  | FluidRightPartial of string * expr

(* ----------------------------- *)
(* Pointers *)
(* ----------------------------- *)
and pointerData =
  | PVarBind of varBind
  | PEventName of string blankOr
  | PEventModifier of string blankOr
  | PEventSpace of string blankOr
  | PExpr of expr
  | PField of field
  | PKey of string blankOr
  | PDBName of string blankOr
  | PDBColName of string blankOr
  | PDBColType of string blankOr
  | PFFMsg of string blankOr
  | PFnName of string blankOr
  | PFnCallName of string blankOr
  | PParamName of string blankOr
  | PParamTipe of tipe blankOr
  | PPattern of pattern
  | PConstructorName of string blankOr
  | PTypeName of string blankOr
  | PTypeFieldName of string blankOr
  | PTypeFieldTipe of tipe blankOr

and pointerType =
  | VarBind
  | EventName
  | EventSpace
  | EventModifier
  | Expr
  | Field
  | Key
  | DBName
  | DBColName
  | DBColType
  | FFMsg
  | FnName
  | FnCallName
  | ParamName
  | ParamTipe
  | Pattern
  | ConstructorName
  | TypeName
  | TypeFieldName
  | TypeFieldTipe

and pointerOwner =
  | POSpecHeader
  | POAst
  | PODb

(* ---------------------- *)
(* Toplevels *)
(* ---------------------- *)
and handlerSpaceName = string

and handlerName = string

and handlerModifer = string

and inTLID = tlid

and toTLID = tlid

and usage = inTLID * toTLID * id option

and tlMeta =
  | DBMeta of dBName * dBColumn list
  | HandlerMeta of handlerSpaceName * handlerName * handlerModifer option
  | FunctionMeta of fnName * userFunctionParameter list

and usedIn =
  | InHandler of
      inTLID * handlerSpaceName * handlerName * handlerModifer option
  | InFunction of inTLID * fnName * userFunctionParameter list

and refersTo =
  | ToDB of toTLID * dBName * dBColumn list * id
  | ToEvent of toTLID * handlerSpaceName * handlerName * id

(* handlers *)
and handlerSpec =
  { space : handlerSpaceName blankOr
  ; name : handlerName blankOr
  ; modifier : handlerModifer blankOr }

and handlerSpace =
  | HSHTTP
  | HSCron
  | HSOther
  | HSEmpty

and handler =
  { ast : expr
  ; spec : handlerSpec
  ; tlid : tlid }

(* dbs *)
and dBName = string

and dBColName = string

and dBColType = string

and dBColumn = dBColName blankOr * dBColType blankOr

and dBMigrationKind = DeprecatedMigrationKind

and dBMigrationState =
  | DBMigrationAbandoned
  | DBMigrationInitialized

and dBMigration =
  { startingVersion : int
  ; version : int
  ; state : dBMigrationState
  ; rollforward : expr
  ; rollback : expr
  ; cols : dBColumn list }

and dB =
  { dbTLID : tlid
  ; dbName : dBName blankOr
  ; cols : dBColumn list
  ; version : int
  ; oldMigrations : dBMigration list
  ; activeMigration : dBMigration option }

(* userFunctions *)
and userFunctionParameter =
  { ufpName : string blankOr
  ; ufpTipe : tipe blankOr
  ; ufpBlock_args : string list
  ; ufpOptional : bool
  ; ufpDescription : string }

and userFunctionMetadata =
  { ufmName : string blankOr
  ; ufmParameters : userFunctionParameter list
  ; ufmDescription : string
  ; ufmReturnTipe : tipe blankOr
  ; ufmInfix : bool }

and userFunction =
  { ufTLID : tlid
  ; ufMetadata : userFunctionMetadata
  ; ufAST : expr }

and userRecordField =
  { urfName : string blankOr
  ; urfTipe : tipe blankOr }

and userTipeDefinition = UTRecord of userRecordField list

and userTipe =
  { utTLID : tlid
  ; utName : string blankOr
  ; utVersion : int
  ; utDefinition : userTipeDefinition }

(* toplevels *)
and tlData =
  | TLHandler of handler
  | TLDB of dB
  | TLFunc of userFunction
  | TLTipe of userTipe

and toplevel =
  { id : tlid
  ; pos : pos
  ; data : tlData }

(* ---------------------- *)
(* dvals *)
(* ---------------------- *)
and dhttp =
  | Redirect of string
  | Response of int * (string * string) list

and optionT =
  | OptJust of dval
  | OptNothing

and resultT =
  | ResOk of dval
  | ResError of dval

and dval =
  | DInt of int
  | DFloat of float
  | DBool of bool
  | DNull
  | DCharacter of string
  | DStr of string
  | DList of dval list
  | DObj of dval StrDict.t
  | DIncomplete
  | DError of string
  | DBlock
  | DErrorRail of dval
  | DResp of dhttp * dval
  | DDB of string
  | DDate of string
  | DPassword of string
  | DUuid of string
  | DOption of optionT
  | DResult of resultT
  | DBytes of bytes

(* ----------------------------- *)
(* Mouse *)
(* ----------------------------- *)
and mouseEvent =
  { mePos : vPos
  ; button : int }

and isLeftButton = bool

(* ----------------------------- *)
(* CursorState *)
(* ----------------------------- *)
and entryCursor =
  | Creating of pos
  | Filling of tlid * id

and hasMoved = bool

and cursorState =
  | Selecting of tlid * id option
  | Entering of entryCursor
  | FluidEntering of tlid
  | Dragging of tlid * vPos * hasMoved * cursorState
  | SelectingCommand of tlid * id
  | Deselected

(* ------------------- *)
(* Analysis *)
(* ------------------- *)
and timerAction =
  | RefreshAnalysis
  | RefreshAvatars
  | CheckUrlHashPosition

and lvDict = dval StrDict.t

and avDict = varName list StrDict.t

and inputValueDict = dval StrDict.t

and analysisResults = {liveValues : lvDict}

and analyses = analysisResults StrDict.t

and functionResult =
  { fnName : string
  ; callerID : id
  ; argHash : string
  ; value : dval }

and fetchRequest =
  | TraceFetch of getTraceDataRPCParams
  | DbStatsFetch of dbStatsRPCParams

(* traces/db_stats fetching *)
and fetchResult =
  | TraceFetchSuccess of getTraceDataRPCParams * getTraceDataRPCResult
  | TraceFetchFailure of getTraceDataRPCParams * string * string
  | TraceFetchMissing of getTraceDataRPCParams
  | DbStatsFetchSuccess of dbStatsRPCParams * dbStatsRPCResult
  | DbStatsFetchFailure of dbStatsRPCParams * string * string
  | DbStatsFetchMissing of dbStatsRPCParams

and fetchContext =
  { canvasName : string
  ; csrfToken : string
  ; origin : string
  ; prefix : string }

and traceID = string

and traceData =
  { input : inputValueDict
  ; timestamp : string
  ; functionResults : functionResult list }

and trace = traceID * traceData option

and traces = trace list StrDict.t

and fourOhFour =
  { space : string
  ; path : string
  ; modifier : string
  ; timestamp : string
  ; traceID : string }

and deployStatus =
  | Deploying
  | Deployed

and staticDeploy =
  { deployHash : string
  ; url : string
  ; lastUpdate : Js.Date.t [@opaque]
  ; status : deployStatus }

and dbStats =
  { count : int
  ; example : dval option }

and dbStatsStore = dbStats StrDict.t

(* ------------------- *)
(* ops *)
(* ------------------- *)
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
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * dBName
  | CreateDBWithBlankOr of tlid * pos * id * dBName
  | DeleteTLForever of tlid
  | DeleteFunctionForever of tlid
  | SetType of userTipe
  | DeleteType of tlid
  | DeleteTypeForever of tlid

(* ------------------- *)
(* RPCs *)
(* ------------------- *)
(* params *)
and sendPresenceParams = avatarModelMessage

and addOpRPCParams = {ops : op list}

and executeFunctionRPCParams =
  { efpTLID : tlid
  ; efpTraceID : traceID
  ; efpCallerID : id
  ; efpArgs : dval list
  ; efpFnName : string }

and triggerCronRPCParams = {tcpTLID : tlid}

and getTraceDataRPCParams =
  { gtdrpTlid : tlid
  ; gtdrpTraceID : traceID }

and dbStatsRPCParams = {dbStatsTlids : tlid list}

and performHandlerAnalysisParams =
  { handler : handler
  ; traceID : traceID
  ; traceData : traceData
  ; dbs : dB list
  ; userFns : userFunction list
  ; userTipes : userTipe list }

and performFunctionAnalysisParams =
  { func : userFunction
  ; traceID : traceID
  ; traceData : traceData
  ; dbs : dB list
  ; userFns : userFunction list
  ; userTipes : userTipe list }

and performAnalysisParams =
  | AnalyzeHandler of performHandlerAnalysisParams
  | AnalyzeFunction of performFunctionAnalysisParams

and analysisEnvelope = traceID * analysisResults

and analysisError =
  | AnalysisExecutionError of performAnalysisParams * string
  | AnalysisParseError of string

and performAnalysisResult = (analysisError, analysisEnvelope) Tc.Result.t

and delete404RPCParams = fourOhFour

(* results *)
and addOpRPCResult =
  { toplevels : toplevel list
  ; deletedToplevels : toplevel list
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list }

and dvalArgsHash = string

and executeFunctionRPCResult = dval * dvalArgsHash * tlid list

and triggerCronRPCResult = traceID * tlid list

and unlockedDBs = StrSet.t

and getUnlockedDBsRPCResult = unlockedDBs

and getTraceDataRPCResult = {trace : trace}

and dbStatsRPCResult = dbStatsStore

and initialLoadRPCResult =
  { toplevels : toplevel list
  ; deletedToplevels : toplevel list
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; unlockedDBs : unlockedDBs
  ; fofs : fourOhFour list
  ; staticDeploys : staticDeploy list
  ; traces : (tlid * traceID) list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list }

and saveTestRPCResult = string

(* ------------------- *)
(* Autocomplete / entry *)
(* ------------------- *)
(* functions *)
and parameter =
  { paramName : string
  ; paramTipe : tipe
  ; paramBlock_args : string list
  ; paramOptional : bool
  ; paramDescription : string }

and function_ =
  { fnName : string
  ; fnParameters : parameter list
  ; fnDescription : string
  ; fnReturnTipe : tipe
  ; fnPreviewExecutionSafe : bool
  ; fnDeprecated : bool
  ; fnInfix : bool }

(* autocomplete items *)
and literal = string

and keyword =
  | KLet
  | KIf
  | KLambda
  | KMatch
  | KThread

and command =
  { commandName : string
  ; action : model -> toplevel -> pointerData -> modification
  ; doc : string
  ; shortcut : string }

and omniAction =
  | NewDB of dBName option
  | NewHandler of string option
  | NewFunction of string option
  | NewHTTPHandler of string option
  | NewEventSpace of string
  | Goto of page * tlid * string

and autocompleteItem =
  | ACFunction of function_
  | ACConstructorName of string
  | ACField of string
  | ACVariable of varName
  | ACLiteral of literal
  | ACOmniAction of omniAction
  | ACKeyword of keyword
  | ACCommand of command
  | ACHTTPModifier of string
  | ACEventName of string
  | ACCronTiming of string
  | ACEventSpace of string
  | ACDBName of string
  | ACDBColType of string
  | ACParamTipe of tipe
  | ACExtra of string
  | ACTypeFieldTipe of tipe

and target = tlid * pointerData

and autocomplete =
  { functions : function_ list
  ; admin : bool
  ; completions : autocompleteItem list
  ; invalidCompletions : autocompleteItem list
  ; allCompletions : autocompleteItem list
  ; index : int
  ; value : string
  ; prevValue : string
  ; target : target option
  ; targetDval : dval option
  ; isCommandMode : bool
  ; visible : bool }

and autocompleteMod =
  | ACSetQuery of string
  | ACAppendQuery of string
  | ACReset
  | ACSelectDown
  | ACSelectUp
  | ACSetTarget of target option
  | ACRegenerate
  | ACEnableCommandMode
  | ACSetVisible of bool

(* | ACFilterByParamType of tipe nodeList *)

(* string entry *)
and stringEntryPermission =
  | StringEntryAllowed
  | StringEntryNotAllowed

and stringEntryWidth =
  | StringEntryNormalWidth
  | StringEntryShortWidth

(* ------------------- *)
(* Clipboard *)
(* ------------------- *)
and clipboardData =
  (< setData : string -> string -> unit [@bs.meth]
   ; getData : string -> string [@bs.meth] >
   Js.t[@opaque])

and jsEvent =
  (< preventDefault : unit -> unit [@bs.meth] ; clipboardData : clipboardData >
   Js.t[@opaque])

and clipboardCopyEvent = jsEvent

and clipboardPasteEvent = jsEvent

and clipboardCutEvent = jsEvent

(* ------------------- *)
(* Modifications *)
(* ------------------- *)
and centerPage = bool

and page =
  | Architecture
  | FocusedFn of tlid
  | FocusedHandler of tlid * centerPage
  | FocusedDB of tlid * centerPage
  | FocusedType of tlid

and focus =
  | FocusNothing
  | FocusExact of tlid * id
  | FocusNext of tlid * id option
  | FocusPageAndCursor of page * cursorState
  | FocusSame
  (* unchanged *)
  | FocusNoChange

and canvasProps =
  { offset : pos
  ; enablePan : bool
  ; lastOffset : pos option
  ; viewportSize : size
  ; panAnimation : bool }

and httpError = (string Tea.Http.error[@opaque])

and errorImportance =
  | IgnorableError
  | ImportantError

and modification =
  | DisplayAndReportHttpError of
      string * errorImportance * httpError * (Js.Json.t[@opaque])
  | DisplayAndReportError of string * string option * string option
  | DisplayError of string
  | ClearError
  | Select of tlid * id option
  | SelectCommand of tlid * id
  | SetHover of tlid * id
  | ClearHover of tlid * id
  | Deselect
  | RemoveToplevel of toplevel
  | SetToplevels of toplevel list * bool
  | UpdateToplevels of toplevel list * bool
  | SetDeletedToplevels of toplevel list
  | UpdateDeletedToplevels of toplevel list
  | UpdateAnalysis of traceID * analysisResults
  | SetUserFunctions of userFunction list * userFunction list * bool
  | SetUnlockedDBs of unlockedDBs
  | AppendUnlockedDBs of unlockedDBs
  | Append404s of fourOhFour list
  | Delete404 of fourOhFour
  | Enter of entryCursor
  | EnterWithOffset of entryCursor * int
  | RPC of (op list * focus)
  | GetUnlockedDBsRPC
  | NoChange
  | MakeCmd of msg Tea.Cmd.t [@printer opaque "MakeCmd"]
  | AutocompleteMod of autocompleteMod
  | Many of modification list
  | Drag of tlid * vPos * hasMoved * cursorState
  | TriggerIntegrationTest of string
  | TriggerCronRPC of tlid
  | EndIntegrationTest
  | SetCursorState of cursorState
  | SetPage of page
  | SetCursor of tlid * traceID
  | ExecutingFunctionBegan of tlid * id
  | ExecutingFunctionRPC of tlid * id * string
  | ExecutingFunctionComplete of (tlid * id) list
  | MoveCanvasTo of pos
  | UpdateTraces of traces
  | OverrideTraces of traces
  | UpdateTraceFunctionResult of
      tlid * traceID * id * fnName * dvalArgsHash * dval
  | AppendStaticDeploy of staticDeploy list
  (* designed for one-off small changes *)
  | TweakModel of (model -> model)
  | SetTypes of userTipe list * userTipe list * bool
  | CenterCanvasOn of tlid
  | InitIntrospect of toplevel list
  | UpdateTLMeta of tlMeta StrDict.t
  | UpdateTLUsage of usage list
  | UpdateDBStatsRPC of tlid
  | UpdateDBStats of dbStatsStore
  | FluidCommandsFor of tlid * id
  | FluidCommandsClose
  | UpdateAvatarList of avatar list
  | ExpireAvatars

(* ------------------- *)
(* Msgs *)
(* ------------------- *)
and msg =
  | GlobalClick of mouseEvent
  | IgnoreMsg
  | ToplevelMouseDown of tlid * mouseEvent
  (* we have the actual node when ToplevelMouseUp is created, *)
  (* but by the time we use it the proper node will be changed *)
  | ToplevelMouseUp of tlid * mouseEvent
  | ToplevelClick of tlid * mouseEvent
  | ToplevelDoubleClick of tlid
  | ToplevelDelete of tlid
  | ToplevelDeleteForever of tlid
  | DragToplevel of tlid * Tea.Mouse.position [@printer opaque "DragToplevel"]
  | EntryInputMsg of string
  | EntrySubmitMsg
  | GlobalKeyPress of Keyboard.keyEvent
  | FluidKeyPress of FluidKeyboard.keyEvent
  | FluidMouseClick
  | AutocompleteClick of string
  | AddOpRPCCallback of
      focus * addOpRPCParams * (addOpRPCResult, httpError) Tea.Result.t
      [@printer opaque "AddOpRPCCallback"]
  | SaveTestRPCCallback of (saveTestRPCResult, httpError) Tea.Result.t
      [@printer opaque "SavetestRPCCallback"]
  | GetUnlockedDBsRPCCallback of
      (getUnlockedDBsRPCResult, httpError) Tea.Result.t
      [@printer opaque "GetUnlockedDBsRPCCallback"]
  | NewTracePush of (traceID * tlid list)
  | New404Push of fourOhFour
  | NewStaticDeployPush of staticDeploy
  | Delete404RPCCallback of delete404RPCParams * (unit, httpError) Tea.Result.t
      [@printer opaque "Delete404RPCCallback"]
  | InitialLoadRPCCallback of
      focus * modification * (initialLoadRPCResult, httpError) Tea.Result.t
      [@printer opaque "InitialLoadRPCCallback"]
  | ExecuteFunctionRPCCallback of
      executeFunctionRPCParams
      * (executeFunctionRPCResult, httpError) Tea.Result.t
      [@printer opaque "ExecuteFunctionRPCCallback"]
  | TriggerCronRPCCallback of (unit, httpError) Tea.Result.t
      [@printer opaque "TriggerCronRPCCallback"]
  | Delete404RPC of fourOhFour
  | NewPresencePush of avatar list
  | LocationChange of Web.Location.location [@printer opaque "LocationChange"]
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleTimers
  | ExecuteFunctionButton of tlid * id * string
  | CreateHandlerFrom404 of fourOhFour
  | WindowResize of int * int
  | WindowOnLoad of int * int
  | TimerFire of timerAction * Tea.Time.t [@printer opaque "TimerFire"]
  | JSError of string
  | PageVisibilityChange of PageVisibility.visibility
  | StartFeatureFlag
  | EndFeatureFlag of id * pick
  | ToggleFeatureFlag of id * bool
  | DeleteUserFunctionParameter of userFunction * userFunctionParameter
  | DeleteUserTypeField of userTipe * userRecordField
  | BlankOrClick of tlid * id * mouseEvent
  | BlankOrDoubleClick of tlid * id * mouseEvent
  | BlankOrMouseEnter of tlid * id * mouseEvent
  | BlankOrMouseLeave of tlid * id * mouseEvent
  | MouseWheel of int * int
  | TraceClick of tlid * traceID * mouseEvent
  | TraceMouseEnter of tlid * traceID * mouseEvent
  | TraceMouseLeave of tlid * traceID * mouseEvent
  | TriggerCron of tlid
  | CreateRouteHandler of string option
  | ToggleSideBar
  | CreateFunction
  | ExtractFunction
  | CreateType
  | DeleteUserFunction of tlid
  | DeleteUserFunctionForever of tlid
  | DeleteUserType of tlid
  | DeleteUserTypeForever of tlid
  | RestoreToplevel of tlid
  | LockHandler of tlid * bool
  | ReceiveAnalysis of performAnalysisResult
  | ReceiveFetch of fetchResult
  | EnablePanning of bool
  | ShowErrorDetails of bool
  | StartMigration of tlid
  | AbandonMigration of tlid
  | DeleteColInDB of tlid * id
  | MarkRoutingTableOpen of bool * string
  | CreateDBTable
  | ClipboardCopyEvent of clipboardCopyEvent
  | ClipboardCutEvent of clipboardCutEvent
  | ClipboardPasteEvent of clipboardPasteEvent
  | ClipboardCopyLivevalue of string
  | EventDecoderError of string * string * string
  | UpdateHandlerState of tlid * handlerState
  | CanvasPanAnimationEnd
  | GoTo of page
  | SetHoveringVarName of tlid * string option
  | TriggerSendPresenceCallback of (unit, httpError) Tea.Result.t
      [@printer opaque "TriggerSendPresenceCallback"]
  | FluidCommandsFilter of string
  | FluidRunCommand of command
  | TakeOffErrorRail of tlid * id

(* ----------------------------- *)
(* AB tests *)
(* ----------------------------- *)
(* just a stub *)
and variantTest =
  | StubVariant
  | FluidVariant

(* ----------------------------- *)
(* FeatureFlags *)
(* ----------------------------- *)
and ffIsExpanded = bool

and pick =
  | PickA
  | PickB

and flagsVS = ffIsExpanded StrDict.t

(* ----------------------------- *)
(* Model *)
(* ----------------------------- *)
and syncState = StrSet.t

and handlerState =
  | HandlerExpanded
  | HandlerPrepCollapse
  | HandlerCollapsing
  | HandlerCollapsed
  | HandlerExpanding

and handlerProp =
  { handlerLock : bool
  ; handlerState : handlerState
  ; hoveringVariableName : varName option }

and tlCursors = traceID StrDict.t

(* Error Handling *)
and darkError =
  { message : string option
  ; showDetails : bool }

(* Testing *)
and testResult = (string, unit) Result.t

and integrationTestState =
  | IntegrationTestExpectation of (model -> testResult)
  | IntegrationTestFinished of testResult
  | NoIntegrationTest

(* Fluid *)
and fluidName = string

(* match id, then the pattern id. We have a pattern id cause they can be
 * nested. *)
and fluidPattern =
  | FPVariable of id * id * fluidName
  | FPConstructor of id * id * fluidName * fluidPattern list
  (* TODO: support char *)
  | FPInteger of id * id * int
  | FPBool of id * id * bool
  | FPString of id * id * string
  | FPFloat of id * id * string * string
  | FPNull of id * id
  | FPBlank of id * id
  | FPOldPattern of id * pattern

and fluidPatternAutocomplete =
  | FPAVariable of id * id * fluidName
  | FPAConstructor of id * id * fluidName * fluidPattern list
  | FPANull of id * id
  | FPABool of id * id * bool

and fluidExpr =
  (* Several of these expressions have extra IDs for roundtripping to the old expr *)
  | EInteger of id * int
  | EBool of id * bool
  | EString of id * string
  | EFloat of id * string * string
  | ENull of id
  | EBlank of id
  (* The 2nd id is extra for the LHS blank. *)
  | ELet of id * id * fluidName * fluidExpr * fluidExpr
  | EIf of id * fluidExpr * fluidExpr * fluidExpr
  | EBinOp of id * fluidName * fluidExpr * fluidExpr * sendToRail
  (* the id in the varname list is extra *)
  | ELambda of id * (id * fluidName) list * fluidExpr
  (* The 2nd ID is extra for the fieldname blank *)
  | EFieldAccess of id * fluidExpr * id * fluidName
  | EVariable of id * string
  | EFnCall of id * fluidName * fluidExpr list * sendToRail
  | EPartial of id * string * fluidExpr
  | ERightPartial of id * string * fluidExpr
  | EList of id * fluidExpr list
  (* The ID in the list is extra for the fieldname *)
  | ERecord of id * (id * fluidName * fluidExpr) list
  | EThread of id * fluidExpr list
  (* The 2nd ID is extra for the name *)
  | EConstructor of id * id * fluidName * fluidExpr list
  (* TODO: add ID for fluidPattern *)
  | EMatch of id * fluidExpr * (fluidPattern * fluidExpr) list
  (* Placeholder that indicates the target of the Thread. May be movable at
   * some point *)
  | EThreadTarget of id
  | EOldExpr of expr

and placeholder = string * string

and fluidToken =
  | TInteger of id * string
  | TString of id * string
  | TBlank of id
  | TPlaceholder of placeholder * id
  | TTrue of id
  | TFalse of id
  | TNullToken of id
  | TFloatWhole of id * string
  | TFloatPoint of id
  | TFloatFraction of id * string
  (* If you're filling in an expr, but havent finished it. Not used for
   * non-expr names. *)
  | TPartial of id * string
  (* A partial that extends out to the right. Used to create binops. *)
  | TRightPartial of id * string
  (* When a partial used to be another thing, we want to show the name of the
   * old thing in a non-interactable way *)
  | TPartialGhost of id * string
  | TSep
  | TNewline
  (* All newlines in the nested tokens start indented to this position. *)
  | TIndentToHere of fluidToken list
  (* Increase the level of indentation for all these tokens. *)
  | TIndented of fluidToken list
  (* TIndentToHere and TIndented are preprocessed to the right indentation
   * and turned into TIndents *)
  | TIndent of int
  | TLetKeyword of id
  | TLetLHS of id * string
  | TLetAssignment of id
  | TIfKeyword of id
  | TIfThenKeyword of id
  | TIfElseKeyword of id
  | TBinOp of id * string
  | TFieldOp of id
  | TFieldName of id * id * string
  | TVariable of id * string
  | TFnName of id * string * sendToRail
  | TLambdaSep of id
  | TLambdaArrow of id
  | TLambdaSymbol of id
  | TLambdaVar of id * int * string
  | TListOpen of id
  | TListClose of id
  | TListSep of id * int
  | TThreadPipe of id * int
  | TRecordOpen of id
  | TRecordField of id * int * string
  | TRecordSep of id * int
  | TMatchKeyword of id
  | TMatchSep of id
  | TPatternVariable of id * id * string
  | TPatternConstructorName of id * id * string
  | TPatternInteger of id * id * string
  | TPatternString of id * id * string
  | TPatternTrue of id * id
  | TPatternFalse of id * id
  | TPatternNullToken of id * id
  | TPatternFloatWhole of id * id * string
  | TPatternFloatPoint of id * id
  | TPatternFloatFraction of id * id * string
  | TPatternBlank of id * id
  | TRecordClose of id
  | TConstructorName of id * string

and fluidTokenInfo =
  { startRow : int
  ; startCol : int
  ; startPos : int
  ; endPos : int
  ; length : int
  ; token : fluidToken }

and fluidAutocompleteItem =
  | FACFunction of function_
  | FACConstructorName of string * int
  | FACField of string
  | FACVariable of varName
  | FACLiteral of literal
  | FACKeyword of keyword
  | FACPattern of fluidPatternAutocomplete

and fluidAutocompleteState =
  { (* ------------------------------- *)
    (* state *)
    (* ------------------------------- *)
    functions : function_ list
  ; index : int option
  ; query :
      (* We need to refer back to the previous one *)
      (tlid * fluidTokenInfo) option
      (* ------------------------------- *)
      (* Cached results *)
      (* ------------------------------- *)
  ; completions : fluidAutocompleteItem list
  ; invalidCompletions : fluidAutocompleteItem list
  ; allCompletions : fluidAutocompleteItem list }

and fluidCommandState =
  { index : int
  ; show : bool
  ; commands : command list
  ; cmdOnTL : toplevel option
  ; cmdOnID : id option
  ; filter : string option }

and fluidState =
  { error : string option
  ; actions : string list
  ; oldPos : int
  ; newPos : int
  ; upDownCol :
      int option
      (* When moving up or down, and going through whitespace, track
       * the column so we can go back to it *)
  ; lastKey : FluidKeyboard.key
  ; ac : fluidAutocompleteState
  ; cp : fluidCommandState }

(* Avatars *)
and avatar =
  { canvasId : string
  ; canvasName : string
  ; serverTime : Js.Date.t [@opaque]
  ; tlid : string option
  ; username : string
  ; email : string
  ; fullname : string option
  ; browserId : string }

and avatarModelMessage =
  { browserId : string
  ; tlid : tlid option
  ; canvasName : string
  ; timestamp : float }

and model =
  { error : darkError
  ; lastMsg : msg
  ; lastMod : modification
  ; tests : variantTest list
  ; complete : autocomplete
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; builtInFunctions : function_ list
  ; cursorState : cursorState
  ; currentPage : page
  ; hovering : (tlid * id) list
  ; toplevels :
      toplevel list
      (* These are read direct from the server. The ones that are *)
      (* analysed are in analysis *)
  ; deletedToplevels : toplevel list
  ; traces : traces
  ; analyses : analyses
  ; f404s : fourOhFour list
  ; unlockedDBs : unlockedDBs
  ; integrationTestState : integrationTestState
  ; visibility : PageVisibility.visibility
  ; syncState : syncState
  ; timersEnabled : bool
  ; executingFunctions : (tlid * id) list
  ; tlCursors :
      tlCursors
      (* This is TLID id to cursor index (the cursor being *)
      (* the input to the toplevel currently used, not to *)
      (* be confused with cursorState, which is the code *)
      (* that is currently selected.) *)
  ; featureFlags : flagsVS
  ; canvasProps : canvasProps
  ; canvasName : string
  ; userContentHost : string
  ; origin : string
  ; environment : string
  ; csrfToken : string
  ; routingTableOpenDetails : StrSet.t
  ; usedDBs : int StrDict.t
  ; usedFns : int StrDict.t
  ; usedTipes : int StrDict.t
  ; handlerProps : handlerProp StrDict.t
  ; staticDeploys : staticDeploy list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list
  ; tlUsages : usage list
  ; tlMeta : tlMeta StrDict.t
  ; fluidState : fluidState
  ; dbStats : dbStatsStore
  ; avatarsList : avatar list
  ; browserId : string
  ; sidebarOpen : bool }

(* Values that we serialize *)
and serializableEditor =
  { timersEnabled : bool
  ; cursorState : cursorState
  ; routingTableOpenDetails : StrSet.t
  ; tlCursors : tlCursors
  ; featureFlags : flagsVS
  ; handlerProps : handlerProp StrDict.t
  ; canvasPos : pos }
[@@deriving show {with_path = false}]
