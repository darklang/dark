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
  | TChar
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
  | TID
  | TDate
  | TTitle
  | TUrl
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TResult
  | TBelongsTo of string
  | THasMany of string
  | TDbList of tipe
  | TUserType of string * int

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

and referral =
  | RDBName of dBName * id
  | REmit of string * string * id

(* ---------------------- *)
(* Toplevels *)
(* ---------------------- *)
(* and direction =
  | USED_IN
  | REFERS_TO *)
and tlReference =
  | OutReferenceDB of tlid * dBName * dBColumn list * id
  | OutReferenceHandler of tlid * string * string option * string * id

(* direction tlid space method name pos*)

(* handlers *)
and handlerSpec =
  { module_ : string blankOr
  ; name : string blankOr
  ; modifier : string blankOr }

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
  | DChar of char
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
  | DID of string
  | DDate of string
  | DTitle of string
  | DUrl of string
  | DPassword of string
  | DUuid of string
  | DOption of optionT
  | DResult of resultT

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
  | Dragging of tlid * vPos * hasMoved * cursorState
  | SelectingCommand of tlid * id
  | Deselected

(* ------------------- *)
(* Analysis *)
(* ------------------- *)
and timerAction =
  | RefreshAnalysis
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

(* traces / traceFetcher *)
and traceFetchResult =
  | TraceFetchSuccess of getTraceDataRPCParams * getTraceDataRPCResult
  | TraceFetchFailure of getTraceDataRPCParams * string * string
  | TraceFetchMissing of getTraceDataRPCParams

and traceFetchContext =
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
  ; lastUpdate : string
  ; status : deployStatus }

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
and addOpRPCParams = {ops : op list}

and executeFunctionRPCParams =
  { efpTLID : tlid
  ; efpTraceID : traceID
  ; efpCallerID : id
  ; efpArgs : dval list
  ; efpFnName : string }

and getTraceDataRPCParams =
  { gtdrpTlid : tlid
  ; gtdrpTraceID : traceID }

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

and executeFunctionRPCResult = dval * dvalArgsHash

and unlockedDBs = StrSet.t

and getUnlockedDBsRPCResult = unlockedDBs

and getTraceDataRPCResult = {trace : trace}

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
and page =
  | Architecture
  | FocusedFn of tlid
  | FocusedHandler of tlid
  | FocusedDB of tlid
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

and modification =
  | DisplayAndReportHttpError of
      string * bool * httpError * (Js.Json.t[@opaque])
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
  | RPCFull of (addOpRPCParams * focus)
  | RPC of (op list * focus)
  | GetUnlockedDBsRPC
  | NoChange
  | MakeCmd of msg Tea.Cmd.t [@printer opaque "MakeCmd"]
  | AutocompleteMod of autocompleteMod
  | Many of modification list
  | Drag of tlid * vPos * hasMoved * cursorState
  | TriggerIntegrationTest of string
  | EndIntegrationTest
  | SetCursorState of cursorState
  | SetPage of page
  | SetCursor of tlid * traceID
  | ExecutingFunctionBegan of tlid * id
  | ExecutingFunctionRPC of tlid * id * string
  | ExecutingFunctionComplete of (tlid * id) list
  | MoveCanvasTo of pos
  | UpdateTraces of traces
  | UpdateTraceFunctionResult of
      tlid * traceID * id * fnName * dvalArgsHash * dval
  | AppendStaticDeploy of staticDeploy list
  (* designed for one-off small changes *)
  | TweakModel of (model -> model)
  | SetTypes of userTipe list * userTipe list * bool
  | CenterCanvasOn of tlid

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
  | Delete404RPC of fourOhFour
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
  | CreateRouteHandler of string option
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
  | ReceiveTraces of traceFetchResult
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

(* ----------------------------- *)
(* AB tests *)
(* ----------------------------- *)
(* just a stub *)
and variantTest = StubVariant

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
  ; handlerState : handlerState }

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
  ; tlReferences : tlReference list StrDict.t }

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
