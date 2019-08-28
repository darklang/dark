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
[@@deriving show {with_path = false}]

module TLID = struct
  type t = tlid

  let toString (TLID str) = str

  let fromString str = TLID str
end

module ID = struct
  type t = id

  let toString (ID str) = str

  let fromString str = ID str
end

module Pair (K1 : Key) (K2 : Key) = struct
  type t = K1.t * K2.t

  let separator = "_*_DARKPAIRSEPARATOR_%_"

  let toString ((v1, v2) : t) : string =
    K1.toString v1 ^ separator ^ K2.toString v2


  let fromString (str : string) : t =
    match String.split ~on:separator str with
    | [v1; v2] ->
        (K1.fromString v1, K2.fromString v2)
    | _ ->
        failwith
          ( "Pair cannot be separated. This probably means you're using the wrong module for this dict/set: "
          ^ str )
end

module TLIDDict = struct
  include Tc.Dict (TLID)

  (* TODO: convert the tlid key back to being called key *)
  let get ~(tlid : tlid) (dict : 'value t) : 'value option = get ~key:tlid dict

  let insert ~(tlid : tlid) ~(value : 'value) (dict : 'value t) : 'value t =
    insert ~key:tlid ~value dict


  let tlids (dict : 'value t) : tlid list = dict |> keys

  let updateIfPresent ~(tlid : tlid) ~(f : 'v -> 'v) (dict : 'value t) :
      'value t =
    updateIfPresent ~key:tlid ~f dict


  let update ~(tlid : tlid) ~(f : 'v option -> 'v option) (dict : 'value t) :
      'value t =
    update ~key:tlid ~f dict


  let remove ~(tlid : tlid) (dict : 'value t) : 'value t =
    remove ~key:tlid dict


  let removeMany ~(tlids : tlid list) (dict : 'value t) : 'value t =
    removeMany ~keys:tlids dict
end

module TLIDSet = Tc.Set (TLID)
module IDSet = Tc.Set (ID)
module IDPair = Pair (TLID) (ID)
module IDPairSet = Tc.Set (IDPair)

(* There are two coordinate systems. Pos is an absolute position in the *)
(* canvas. Nodes and Edges have Pos'. VPos is the viewport: clicks occur *)
(* within the viewport and we map Absolute positions back to the *)
(* viewport to display in the browser. *)
(* TODO: Can we depreciate VPos? *)
type pos =
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
  | TDate
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TResult
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
  | PGroupName of string blankOr

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
  | GroupName

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

(* usedIn is a TL that's refered to in the refersTo tl at id *)
(* refersTo is a TL that uses the usedIn tl at id *)
and usage =
  { usedIn : tlid
  ; refersTo : tlid
  ; id : id }

(* handlers *)
and handlerSpec =
  { space : handlerSpaceName blankOr
  ; name : handlerName blankOr
  ; modifier : handlerModifer blankOr }

and handlerSpace =
  | HSHTTP
  | HSCron
  | HSWorker
  | HSRepl
  | HSDeprecatedOther

and handler =
  { ast : expr
  ; spec : handlerSpec
  ; hTLID : tlid
  ; pos : pos }

(* groups *)
and group =
  { gName : string blankOr
  ; gTLID : tlid
  ; members : tlid list
  ; pos : pos }

(* dbs *)
and dbName = string

and dbColName = string

and dbColType = string

and dbColumn = dbColName blankOr * dbColType blankOr

and dbMigrationKind = DeprecatedMigrationKind

and dbMigrationState =
  | DBMigrationAbandoned
  | DBMigrationInitialized

and dbMigration =
  { startingVersion : int
  ; version : int
  ; state : dbMigrationState
  ; rollforward : expr
  ; rollback : expr
  ; cols : dbColumn list }

and db =
  { dbTLID : tlid
  ; dbName : dbName blankOr
  ; cols : dbColumn list
  ; version : int
  ; oldMigrations : dbMigration list
  ; activeMigration : dbMigration option
  ; pos : pos }

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
and toplevel =
  | TLHandler of handler
  | TLDB of db
  | TLFunc of userFunction
  | TLTipe of userTipe
  | TLGroup of group

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
  ; button : int
  ; altKey : bool
  ; ctrlKey : bool
  ; shiftKey : bool
  ; detail : int }

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
  ; example : (dval * string) option }

and dbStatsStore = dbStats StrDict.t

(* ------------------- *)
(* ops *)
(* ------------------- *)
and rollbackID = id

and rollforwardID = id

and op =
  | SetHandler of tlid * pos * handler
  | CreateDB of tlid * pos * dbName
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * dbColName
  | SetDBColType of tlid * id * dbColType
  | DeleteTL of tlid
  | MoveTL of tlid * pos
  | TLSavepoint of tlid
  | UndoTL of tlid
  | RedoTL of tlid
  | SetFunction of userFunction
  | DeleteFunction of tlid
  | ChangeDBColName of tlid * id * dbColName
  | ChangeDBColType of tlid * id * dbColType
  | DeprecatedInitDbm of
      tlid * id * rollbackID * rollforwardID * dbMigrationKind
  | SetExpr of tlid * id * expr
  | CreateDBMigration of tlid * rollbackID * rollforwardID * dbColumn list
  | AddDBColToDBMigration of tlid * id * id
  | SetDBColNameInDBMigration of tlid * id * dbColName
  | SetDBColTypeInDBMigration of tlid * id * dbColType
  | DeleteColInDBMigration of tlid * id
  | AbandonDBMigration of tlid
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * dbName
  | CreateDBWithBlankOr of tlid * pos * id * dbName
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

and addOpRPCParams =
  { ops : op list
  ; opCtr : int option
  ; browserId : string }

and executeFunctionRPCParams =
  { efpTLID : tlid
  ; efpTraceID : traceID
  ; efpCallerID : id
  ; efpArgs : dval list
  ; efpFnName : string }

and triggerHandlerRPCParams =
  { thTLID : tlid
  ; thTraceID : traceID
  ; thInput : inputValueDict }

and getTraceDataRPCParams =
  { gtdrpTlid : tlid
  ; gtdrpTraceID : traceID }

and dbStatsRPCParams = {dbStatsTlids : tlid list}

and performHandlerAnalysisParams =
  { handler : handler
  ; traceID : traceID
  ; traceData : traceData
  ; dbs : db list
  ; userFns : userFunction list
  ; userTipes : userTipe list }

and performFunctionAnalysisParams =
  { func : userFunction
  ; traceID : traceID
  ; traceData : traceData
  ; dbs : db list
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
  { handlers : handler list
  ; deletedHandlers : handler list
  ; dbs : db list
  ; deletedDBs : db list
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list }

and addOpStrollerMsg =
  { result : addOpRPCResult
  ; params : addOpRPCParams }

and dvalArgsHash = string

and executeFunctionRPCResult = dval * dvalArgsHash * tlid list * unlockedDBs

and triggerHandlerRPCResult = tlid list

and unlockedDBs = StrSet.t

and getUnlockedDBsRPCResult = unlockedDBs

and getTraceDataRPCResult = {trace : trace}

and dbStatsRPCResult = dbStatsStore

and initialLoadRPCResult =
  { handlers : handler list
  ; deletedHandlers : handler list
  ; dbs : db list
  ; deletedDBs : db list
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; unlockedDBs : unlockedDBs
  ; fofs : fourOhFour list
  ; staticDeploys : staticDeploy list
  ; traces : (tlid * traceID) list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list
  ; permission : permission option
  ; opCtrs : int StrDict.t
  ; groups : group list
  ; deletedGroups : group list }

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
  | NewDB of dbName option
  | NewFunction of string option
  | NewHTTPHandler of string option
  | NewWorkerHandler of string option
  | NewCronHandler of string option
  | NewReplHandler of string option
  | NewGroup of string option
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
  | ACHTTPRoute of string
  | ACWorkerName of string
  | ACReplName of string
  | ACCronName of string
  | ACCronTiming of string
  | ACEventSpace of string
  | ACDBName of string
  | ACDBColType of string
  | ACParamTipe of tipe
  | ACTypeFieldTipe of tipe
  | ACDBColName of string
  | ACExpr of string
  | ACVarBind of string
  | ACEventModifier of string
  | ACKey of string
  | ACFFMsg of string
  | ACFnName of string
  | ACParamName of string
  | ACTypeName of string
  | ACTypeFieldName of string
  | ACGroupName of string

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
  | FocusedGroup of tlid * centerPage

and focus =
  | FocusNothing
  | FocusExact of tlid * id
  | FocusNext of tlid * id option
  | FocusPageAndCursor of page * cursorState
  | FocusSame
  (* unchanged *)
  | FocusNoChange

and toast =
  { toastMessage : string option
  ; toastPos : vPos option }

and canvasProps =
  { offset : pos
  ; enablePan : bool
  ; lastOffset : pos option
  ; panAnimation : bool
  ; minimap : string option }

and httpError = (string Tea.Http.error[@opaque])

and errorImportance =
  | IgnorableError
  | ImportantError

and apiError =
  { context : string
  ; originalError : httpError (* the Tea_http error *)
  ; requestParams : (Js.Json.t[@opaque]) option
  ; importance : errorImportance }

and modification =
  | HandleAPIError of apiError
  | DisplayAndReportError of string * string option * string option
  | DisplayError of string
  | ClearError
  | Select of tlid * id option
  | SelectCommand of tlid * id
  | SetHover of tlid * id
  | ClearHover of tlid * id
  | Deselect
  | RemoveToplevel of toplevel
  | RemoveGroup of toplevel
  | SetToplevels of handler list * db list * group list * bool
  | UpdateToplevels of handler list * db list * bool
  | SetDeletedToplevels of handler list * db list
  | UpdateDeletedToplevels of handler list * db list
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
  | TriggerHandlerRPC of tlid
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
  | SetPermission of permission option
  | CenterCanvasOn of tlid
  | InitIntrospect of toplevel list
  | RefreshUsages of tlid list
  | UpdateDBStatsRPC of tlid
  | UpdateDBStats of dbStatsStore
  | FluidCommandsShow of tlid * fluidToken
  | FluidCommandsClose
  | UpdateAvatarList of avatar list
  | ExpireAvatars
  | AddGroup of group
  | AddToGroup of tlid * tlid
  | UndoGroupDelete of tlid * group
  | MoveMemberToNewGroup of tlid * tlid * model

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
  | ToplevelDelete of tlid
  | ToplevelDeleteForever of tlid
  | DragToplevel of tlid * Tea.Mouse.position [@printer opaque "DragToplevel"]
  | EntryInputMsg of string
  | EntrySubmitMsg
  | GlobalKeyPress of Keyboard.keyEvent
  | FluidKeyPress of FluidKeyboard.keyEvent
  | FluidMouseClick of tlid
  | FluidSelectStart of tlid * cursorState
  | AutocompleteClick of int
  | FluidAutocompleteClick of fluidAutocompleteItem
  | FluidCopy
  | FluidCut
  | FluidPaste
  | AddOpRPCCallback of
      focus * addOpRPCParams * (addOpStrollerMsg, httpError) Tea.Result.t
      [@printer opaque "AddOpRPCCallback"]
  | AddOpStrollerMsg of addOpStrollerMsg
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
  | TriggerHandlerRPCCallback of
      triggerHandlerRPCParams
      * (triggerHandlerRPCResult, httpError) Tea.Result.t
      [@printer opaque "TriggerHandlerRPCCallback"]
  | Delete404RPC of fourOhFour
  | NewPresencePush of avatar list
  | LocationChange of Web.Location.location [@printer opaque "LocationChange"]
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleTimers
  | ExecuteFunctionButton of tlid * id * string
  | CreateHandlerFrom404 of fourOhFour
  | TimerFire of timerAction * Tea.Time.t [@printer opaque "TimerFire"]
  | JSError of string
  | PageVisibilityChange of PageVisibility.visibility
  | StartFeatureFlag
  | EndFeatureFlag of id * pick
  | ToggleFeatureFlag of id * bool
  | DeleteUserFunctionParameter of tlid * userFunctionParameter
  | AddUserFunctionParameter of tlid
  | DeleteUserTypeField of tlid * userRecordField
  | BlankOrClick of tlid * id * mouseEvent
  | BlankOrDoubleClick of tlid * id * mouseEvent
  | BlankOrMouseEnter of tlid * id * mouseEvent
  | BlankOrMouseLeave of tlid * id * mouseEvent
  | MouseWheel of int * int
  | TraceClick of tlid * traceID * mouseEvent
  | TraceMouseEnter of tlid * traceID * mouseEvent
  | TraceMouseLeave of tlid * traceID * mouseEvent
  | TriggerHandler of tlid
  | CreateRouteHandler of omniAction
  | ToggleSideBar
  | CreateFunction
  | ExtractFunction
  | CreateType
  | DeleteUserFunction of tlid
  | DeleteUserFunctionForever of tlid
  | DeleteUserType of tlid
  | DeleteUserTypeForever of tlid
  | DeleteGroupForever of tlid
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
  | ClipboardCopyLivevalue of string * vPos
  | EventDecoderError of string * string * string
  | UpdateHandlerState of tlid * handlerState
  | CanvasPanAnimationEnd
  | GoTo of page
  | SetHoveringReferences of tlid * id list
  | TriggerSendPresenceCallback of (unit, httpError) Tea.Result.t
      [@printer opaque "TriggerSendPresenceCallback"]
  | FluidCommandsFilter of string
  | FluidCommandsClick of command
  | TakeOffErrorRail of tlid * id
  | SetHandlerExeIdle of tlid
  | CopyCurl of tlid * vPos
  | SetHandlerActionsMenu of tlid * bool
  | UpdateFluidSelection of fluidSelection option
  | ResetToast
  | UpdateMinimap of string option
  | GoToArchitecturalView
  | DeleteGroup of tlid
  | DragGroupMember of tlid * tlid * mouseEvent
  | CreateGroup

(* ----------------------------- *)
(* AB tests *)
(* ----------------------------- *)
(* just a stub *)
and variantTest =
  | StubVariant
  | FluidVariant
  (* Without this libtwitter functions aren't available *)
  | LibtwitterVariant
  | GroupVariant

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

and exeState =
  | Idle
  | Executing
  | Complete

and handlerProp =
  { handlerLock : bool
  ; handlerState : handlerState
  ; hoveringReferences :
      (* When hovering over a reference, this is the list of ids that refer to
       * the reference *)
      id list
  ; execution : exeState
  ; showActions : bool }

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
  (* The 2nd id is for the name *)
  | EFeatureFlag of id * string * id * fluidExpr * fluidExpr * fluidExpr
  | EOldExpr of expr

and placeholder = string * string

and fluidToken =
  | TInteger of id * string
  | TString of id * string
  (* multi-line strings, id, segment, full-string, offset *)
  | TStringMLStart of id * string * int * string
  | TStringMLMiddle of id * string * int * string
  | TStringMLEnd of id * string * int * string
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
  (* Newlines sometimes need to hold context. When there are many things in the
   * id with the newline, the extra context is the index of which one it is. *)
  | TNewline of (id * int option) option
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
  (* id, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'DB::getAll'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3'), sendToRail *)
  | TFnName of id * string * string * string * sendToRail
  (* id, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'v3'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3') *)
  | TFnVersion of id * string * string * string
  | TLambdaSep of id * int
  | TLambdaArrow of id
  | TLambdaSymbol of id
  | TLambdaVar of id * int * string
  | TListOpen of id
  | TListClose of id
  | TListSep of id * int
  (* 2nd int is the number of pipe segments there are *)
  | TThreadPipe of id * int * int
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
  ; commands : command list
  ; location : (tlid * fluidToken) option
  ; filter : string option }

and fluidSelection = {range : int * int}

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
  ; cp : fluidCommandState
  ; selection : fluidSelection option
  ; clipboard : fluidExpr option }

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
  ; builtInFunctions : function_ list
  ; cursorState : cursorState
  ; currentPage : page
  ; hovering : (tlid * id) list
  ; groups : group TLIDDict.t
  ; handlers : handler TLIDDict.t
  ; deletedHandlers : handler TLIDDict.t
  ; dbs : db TLIDDict.t
  ; deletedDBs : db TLIDDict.t
  ; userFunctions : userFunction TLIDDict.t
  ; deletedUserFunctions : userFunction TLIDDict.t
  ; userTipes : userTipe TLIDDict.t
  ; deletedUserTipes : userTipe TLIDDict.t
  ; deletedGroups : group TLIDDict.t
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
  ; handlerProps : handlerProp TLIDDict.t
  ; staticDeploys :
      staticDeploy list
      (* tlRefersTo : to answer the question "what TLs does this TL refer to". eg
   * if myFunc was called in Repl2 at id, then the dict would be:
   *
   *   { repl2.tlid: { (myFunc.tlid, id) } }
   *
   * which you can read as "repl2 refersTo myfunc". So a tlid points to the TLs
   * it uses. *)
  ; tlRefersTo :
      IDPairSet.t TLIDDict.t
      (* tlUsedIn: to answer the question "what TLs is this TL's name used in".  eg
   * if myFunc was called in Repl2, the dict would
   *
   *   { myfunc.tlid: { repl2.tlid }}
   *
   * which you can read as "myfunc is used in repl2".  *)
  ; tlUsedIn : TLIDSet.t TLIDDict.t
  ; fluidState : fluidState
  ; dbStats : dbStatsStore
  ; avatarsList : avatar list
  ; browserId : string
  ; sidebarOpen : bool
  ; isAdmin : bool
  ; buildHash : string
  ; lastReload : (Js.Date.t[@opaque]) option
  ; toast : toast
  ; opCtrs : int StrDict.t
  ; permission : permission option }

(* Values that we serialize *)
and serializableEditor =
  { timersEnabled : bool
  ; cursorState : cursorState
  ; routingTableOpenDetails : StrSet.t
  ; tlCursors : tlCursors
  ; featureFlags : flagsVS
  ; handlerProps : handlerProp TLIDDict.t
  ; canvasPos : pos
  ; lastReload : (Js.Date.t[@opaque]) option
  ; sidebarOpen : bool }
[@@deriving show {with_path = false}]

and permission =
  | Read
  | ReadWrite
[@@deriving show eq ord]
