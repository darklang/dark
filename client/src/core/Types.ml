open Tc
include UnsharedTypes

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


  let remove ~(tlid : tlid) (dict : 'value t) : 'value t = remove ~key:tlid dict

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
and fnName = string

and fluidExpr = FluidExpression.t

and fluidPattern = FluidPattern.t

(* ----------------------------- *)
(* Pointers *)
(* ----------------------------- *)
and blankOrData =
  | PEventName of string blankOr
  | PEventModifier of string blankOr
  | PEventSpace of string blankOr
  | PDBName of string blankOr
  | PDBColName of string blankOr
  | PDBColType of string blankOr
  | PFnName of string blankOr
  | PParamName of string blankOr
  | PParamTipe of tipe blankOr
  | PTypeName of string blankOr
  | PTypeFieldName of string blankOr
  | PTypeFieldTipe of tipe blankOr
  | PGroupName of string blankOr

and blankOrType =
  | EventName
  | EventSpace
  | EventModifier
  | DBName
  | DBColName
  | DBColType
  | FnName
  | ParamName
  | ParamTipe
  | TypeName
  | TypeFieldName
  | TypeFieldTipe
  | GroupName
[@@deriving show {with_path = false}]

(* ---------------------- *)
(* Toplevels *)
(* ---------------------- *)
type handlerSpaceName = string

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
  { ast : FluidExpression.t
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
  ; rollforward : FluidExpression.t
  ; rollback : FluidExpression.t
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
  ; ufAST : FluidExpression.t }

and userRecordField =
  { urfName : string blankOr
  ; urfTipe : tipe blankOr }

and userTipeDefinition = UTRecord of userRecordField list

and userTipe =
  { utTLID : tlid
  ; utName : string blankOr
  ; utVersion : int
  ; utDefinition : userTipeDefinition }

(* Package manager Functions *)
and packageFnParameter =
  { name : string
  ; tipe : tipe
  ; description : string }

and packageFn =
  { user : string
  ; package : string
  ; module_ : string
  ; fnname : string
  ; version : int
  ; body : fluidExpr
  ; parameters : packageFnParameter list
  ; return_type : tipe
  ; description : string
  ; author : string
  ; deprecated : bool
  ; pfTLID : tlid }

(* toplevels *)
and toplevel =
  | TLHandler of handler
  | TLDB of db
  | TLFunc of userFunction
  | TLTipe of userTipe
  | TLGroup of group

and packageFns = packageFn TLIDDict.t

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

and dval_source =
  | SourceNone
  | SourceId of id

and dblock_args =
  { symtable : dval StrDict.t
  ; params : (id * string) list
  ; body : FluidExpression.t }

and dval =
  | DInt of int
  | DFloat of float
  | DBool of bool
  | DNull
  | DCharacter of string
  | DStr of string
  | DList of dval array
  | DObj of dval StrDict.t
  | DIncomplete of dval_source
  | DError of (dval_source * string)
  | DBlock of dblock_args
  | DErrorRail of dval
  | DResp of dhttp * dval
  | DDB of string
  | DDate of string
  | DPassword of string
  | DUuid of string
  | DOption of optionT
  | DResult of resultT
  | DBytes of bytes
[@@deriving show {with_path = false}]

(* ----------------------------- *)
(* Referencing parts of an AST *)
(* at the caret level *)
(* ----------------------------- *)

(* NOTE(JULIAN): the ast*Parts below are sketches of the types; they will likely change
   based on which specific parts of the AST we actually want to represent via astRef *)

type astFloatPart =
  | FPWhole
  | FPPoint
  | FPFractional
[@@deriving show {with_path = false}]

type astStringPart = SPOpenQuote [@@deriving show {with_path = false}]

type astLetPart =
  | LPKeyword
  | LPVarName
  | LPAssignment
[@@deriving show {with_path = false}]

type astIfPart =
  | IPIfKeyword
  | IPThenKeyword
  | IPElseKeyword
[@@deriving show {with_path = false}]

type astLambdaPart =
  | LBPSymbol
  | LBPVarName of (* index of the var *) int
  | LBPComma of (* index of the var *) int
  | LBPArrow
[@@deriving show {with_path = false}]

type astFieldAccessPart =
  | FAPFieldname
  | FAPFieldOp
[@@deriving show {with_path = false}]

type astRecordPart =
  | RPOpen
  | RPFieldname of (* index of the <fieldname,value> pair *) int
  | RPFieldSep of (* index of the <fieldname,value> pair *) int
  | RPClose
[@@deriving show {with_path = false}]

type astListPart =
  | LPOpen
  | LPClose
  | LPComma of int
[@@deriving show {with_path = false}]

type astMatchPart =
  | MPKeyword
  | MPBranchArrow of (* index of the branch *) int
[@@deriving show {with_path = false}]

type astPatternPart =
  | PPVariable
  | PPConstructor
  | PPInteger
  | PPBool
  | PPString of astStringPart
  | PPFloat of astFloatPart
  | PPNull
  | PPBlank
[@@deriving show {with_path = false}]

type astFlagPart =
  | FPWhenKeyword
  | FPEnabledKeyword
[@@deriving show {with_path = false}]

(* An astRef represents a reference to a specific part of an AST node,
   such as a specific Record Fieldname rather than just the record.
   Why not use a fluidToken for this purpose?
   A single construct such as a string might map to multiple fluidTokens,
   but when describing a part of the ast (for example with caretTarget),
   we often don't want to care about the details of the tokenization;
   we can represent concepts like "the caret position at the end of this
   string" without needing to know if it is a TString relative to a combination
   of TStringMLStart, TStringMLMiddle, TStringMLEnd.

   The IDs below all refer to the AST node id

   NOTE(JULIAN): We intentionally do not have any astRefs that include
   parts that refer to an part of the AST that contains nested expressions.
   In such cases (for example the value or body of a let), it makes more sense
   to generate a more specific astRef within the nested expression.
    *)
type astRef =
  | ARInteger of id
  | ARBool of id
  | ARString of id * astStringPart
  | ARFloat of id * astFloatPart
  | ARNull of id
  | ARBlank of id
  | ARLet of id * astLetPart
  | ARIf of id * astIfPart
  | ARBinOp of id (* matches the operator *)
  | ARFieldAccess of id * astFieldAccessPart
  | ARVariable of id
  | ARFnCall of id (* Matches the fn name+version *)
  | ARPartial of id
  | ARRightPartial of id
  | ARList of id * astListPart
  | ARRecord of id * astRecordPart
  | ARPipe of id * int (* index of the pipe *)
  | ARConstructor of id (* name of the constructor *)
  | ARMatch of id * astMatchPart
  | ARLambda of id * astLambdaPart
  | ARPattern of id * astPatternPart
  | ARFlag of id * astFlagPart
  (* for use if something that should never happen happened *)
  | ARInvalid
[@@deriving show {with_path = false}]

(* A caretTarget represents a distinct caret location within the AST.
   By combining a reference to part of the AST and a caret offset
   into that part of the AST, we can uniquely represent a place
   for the caret to jump during AST transformations, even ones that
   drastically change the token stream. *)
type caretTarget =
  { astRef : astRef
  ; offset : int }
[@@deriving show {with_path = false}]

(* ----------------------------- *)
(* Mouse *)
(* ----------------------------- *)
type mouseEvent =
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
  | Creating of pos option (* If we know the position the user wants the handler to be at (presumably because they clicked there to get the omnibox), then use it. Otherwise, if there's no position, we'll pick one for them later *)
  | Filling of tlid * id

and hasMoved = bool

and cursorState =
  | Selecting of tlid * id option
  | Entering of entryCursor
  | FluidEntering of tlid
  | DraggingTL of tlid * vPos * hasMoved * cursorState
  | PanningCanvas of
      { viewportStart : vPos
      ; viewportCurr : vPos
      ; prevCursorState : cursorState }
  | Deselected

(* ------------------- *)
(* Analysis *)
(* ------------------- *)
and timerAction =
  | RefreshAnalysis
  | RefreshAvatars
  | CheckUrlHashPosition

and 'result loadable =
  | LoadableSuccess of 'result
  | LoadableNotInitialized
  | LoadableLoading of 'result option
  | LoadableError of string

and dvalDict = dval StrDict.t

and executionResult =
  | ExecutedResult of dval
  | NonExecutedResult of dval

and intermediateResultStore = executionResult StrDict.t

(* map from expression ids to symbol table, which maps from varname strings to
 * the ids of the expressions that represent their values *)
and avDict = id StrDict.t StrDict.t

and inputValueDict = dvalDict

and analysisStore = intermediateResultStore loadable

and analyses = analysisStore (* indexed by traceID *) StrDict.t

and functionResult =
  { fnName : string
  ; callerID : id
  ; argHash : string
  ; argHashVersion : int
  ; value : dval }

and fetchRequest =
  | TraceFetch of getTraceDataAPIParams
  | DbStatsFetch of dbStatsAPIParams
  | WorkerStatsFetch of workerStatsAPIParams

(* traces/db_stats fetching *)
and fetchResult =
  | TraceFetchSuccess of getTraceDataAPIParams * getTraceDataAPIResult
  | TraceFetchFailure of getTraceDataAPIParams * string * string
  | TraceFetchMissing of getTraceDataAPIParams
  | DbStatsFetchSuccess of dbStatsAPIParams * dbStatsAPIResult
  | DbStatsFetchFailure of dbStatsAPIParams * string * string
  | DbStatsFetchMissing of dbStatsAPIParams
  | WorkerStatsFetchSuccess of workerStatsAPIParams * workerStatsAPIResult
  | WorkerStatsFetchFailure of workerStatsAPIParams * string * string
  | WorkerStatsFetchMissing of workerStatsAPIParams

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

and traceOpt = traceID * traceData option

and traceError =
  (* NoneYet is a replacement for what was None when trace was a
     (traceID * traceData option) *)
  | NoneYet
  (* MaximumCallStackError is unrecoverable - don't try again *)
  | MaximumCallStackError

and trace = traceID * (traceError, traceData) Result.t

and traces = trace list (* indexed by tlid *) StrDict.t

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

and workerStats =
  { count : int
  ; schedule : string option }

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
  | SetExpr of tlid * id * FluidExpression.t
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
(* APIs *)
(* ------------------- *)
(* params *)
and sendPresenceParams = avatarModelMessage

and addOpAPIParams =
  { ops : op list
  ; opCtr : int
  ; clientOpCtrId : string }

and executeFunctionAPIParams =
  { efpTLID : tlid
  ; efpTraceID : traceID
  ; efpCallerID : id
  ; efpArgs : dval list
  ; efpFnName : string }

and uploadFnAPIParams = {uplFn : userFunction}

and triggerHandlerAPIParams =
  { thTLID : tlid
  ; thTraceID : traceID
  ; thInput : inputValueDict }

and getTraceDataAPIParams =
  { gtdrpTlid : tlid
  ; gtdrpTraceID : traceID }

and dbStatsAPIParams = {dbStatsTlids : tlid list}

and workerStatsAPIParams = {workerStatsTlid : tlid}

and updateWorkerScheduleAPIParams =
  { workerName : string
  ; schedule : string }

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

and analysisEnvelope = traceID * intermediateResultStore

and analysisError =
  | AnalysisExecutionError of performAnalysisParams * string
  | AnalysisParseError of string

and performAnalysisResult = (analysisError, analysisEnvelope) Tc.Result.t

and delete404APIParams = fourOhFour

and account =
  { name : string
  ; email : string
  ; username : string }

(* results *)
and addOpAPIResult =
  { handlers : handler list
  ; deletedHandlers : handler list
  ; dbs : db list
  ; deletedDBs : db list
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list }

and addOpAPIResponse = {result : addOpAPIResult}

and addOpStrollerMsg =
  { result : addOpAPIResult
  ; params : addOpAPIParams }

and dvalArgsHash = string

and executeFunctionAPIResult =
  dval * dvalArgsHash * int * tlid list * unlockedDBs

and uploadFnAPIResult = unit

and loadPackagesAPIResult = packageFn list

and triggerHandlerAPIResult = tlid list

and unlockedDBs = StrSet.t

and getUnlockedDBsAPIResult = unlockedDBs

and getTraceDataAPIResult = {trace : trace}

and dbStatsAPIResult = dbStatsStore

and workerStatsAPIResult = workerStats

and allTracesAPIResult = {traces : (tlid * traceID) list}

and initialLoadAPIResult =
  { handlers : handler list
  ; deletedHandlers : handler list
  ; dbs : db list
  ; deletedDBs : db list
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; unlockedDBs : unlockedDBs
  ; fofs : fourOhFour list
  ; staticDeploys : staticDeploy list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list
  ; permission : permission option
  ; opCtrs : int StrDict.t
  ; groups : group list
  ; deletedGroups : group list
  ; account : account
  ; canvas_list : string list
  ; org_list : string list
  ; worker_schedules : string StrDict.t }

and saveTestAPIResult = string

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

and displayText = string

(* Some AC items needs to be dynamically added to the list,
   while others can be filtered in and out of the list.
  For example: Goto will take you to focus on a toplevel.
  In the case of "Jump to", results are filtered by name,
    and do not need to be dynamically generated.
  But in the case of "Found in", results are dynamically generated,
    based on the content that is inside.
*)
and isDynamic = bool

and keyword =
  | KLet
  | KIf
  | KLambda
  | KMatch
  | KPipe

and command =
  { commandName : string
  ; action : model -> toplevel -> id -> modification
  ; doc : string }

and omniAction =
  | NewDB of dbName option
  | NewFunction of string option
  | NewHTTPHandler of string option
  | NewWorkerHandler of string option
  | NewCronHandler of string option
  | NewReplHandler of string option
  | NewGroup of string option
  | Goto of page * tlid * displayText * isDynamic

and autocompleteItem =
  | ACOmniAction of omniAction
  (* HTTP *)
  | ACHTTPModifier of string
  | ACHTTPRoute of string
  (* Workers *)
  | ACWorkerName of string
  | ACEventSpace of string
  | ACEventModifier of string
  (* Repl *)
  | ACReplName of string
  (* CRON *)
  | ACCronName of string
  | ACCronTiming of string
  (* DBs *)
  | ACDBName of string
  | ACDBColType of string
  | ACDBColName of string
  (* User functions *)
  | ACFnName of string
  | ACParamName of string
  | ACParamTipe of tipe
  (* User types *)
  | ACTypeFieldTipe of tipe
  | ACTypeName of string
  | ACTypeFieldName of string
  (* Groups *)
  | ACGroupName of string

and target = tlid * blankOrData

and autocomplete =
  { admin : bool
  ; completions : autocompleteItem list
  ; allCompletions : autocompleteItem list
  ; index : int
  ; value : string
  ; prevValue : string
  ; target : target option
  ; visible : bool }

and autocompleteMod =
  | ACSetQuery of string
  | ACAppendQuery of string
  | ACReset
  | ACSelectDown
  | ACSelectUp
  | ACSetTarget of target option
  | ACRegenerate
  | ACSetVisible of bool

(* ------------------- *)
(* Clipboard *)
(* ------------------- *)
and clipboardData =
  (< setData : string -> string -> unit [@bs.meth]
   ; getData : string -> string [@bs.meth] >
   Js.t
  [@opaque])

and clipboardEvent =
  (< preventDefault : unit -> unit [@bs.meth] ; clipboardData : clipboardData >
   Js.t
  [@opaque])

and clipboardContents =
  (* Clipboard supports both text and encoded FluidExpression.ts. At the moment,
   * there is always a text option - there isn't a json option if the copied
   * string wasn't a FluidExpression.t *)
  (string * Js.Json.t option
  [@opaque])

(* --------------- *)
(* Component Types *)
(* --------------- *)

(* Account View *)
and settingsTab =
  | UserSettings
  | InviteUser

and settingsViewState =
  { opened : bool
  ; tab : settingsTab
  ; canvas_list : string list
  ; org_list : string list }

and settingsMsg =
  | ToggleSettingsView of bool
  | SwitchSettingsTabs of settingsTab

(* TLMenu *)
and menuState = {isOpen : bool}

and menuMsg =
  | OpenMenu
  | CloseMenu

(* FnParams *)
and fnProps =
  { draggingParamIndex : int option
  ; dragOverSpaceIndex : int option
  ; justMovedParam : id option }

and fnpMsg =
  | ParamDragStart of int
  | ParamDragDone
  | ParamEntersSpace of int
  | ParamLeavesSpace
  | ParamDropIntoSpace of int
  | Reset

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

and isTransitionAnimated =
  | AnimateTransition
  | DontAnimateTransition

and canvasProps =
  { offset : pos
  ; enablePan : bool
  ; lastOffset : pos option
  ; panAnimation : isTransitionAnimated
  ; minimap : string option }

and httpError = (string Tea.Http.error[@opaque])

and errorImportance =
  | IgnorableError
  | ImportantError

and apiError =
  { context : string
  ; originalError : httpError (* the Tea_http error *)
  ; requestParams : (Js.Json.t[@opaque]) option
  ; reload : bool
  ; importance : errorImportance }

(* Editor settings are global settings on the editor. Initially, these are only things that admins use for debugging - in the future they could be extended to editor settings *)
and editorSettings =
  { showFluidDebugger : bool
  ; runTimers : bool }

(* tlidSelectTarget represents a target inside a TLID for use
   by the `Select` modification.

   In Fluid, we should probably use STCaret in all cases --
   knowing the id of an ast node (via STID) is insufficient
   to know where to place the caret within that node.
   In non-fluid, the concept of a caret doesn't really exist;
   we select nodes at any nesting level as a whole, so STID is
   sufficient.

   If we want to select a toplevel as a whole but don't have a
   specific id in mind, we use STTopLevelRoot. There's a few
   places where we do this as a fallback when we expected to find
   an id but couldn't (they used to use Some(id) with an implicit
   fallback to None). *)
and tlidSelectTarget =
  | STCaret of caretTarget
  | STID of id
  | STTopLevelRoot

and modification =
  (* API Calls *)
  | AddOps of (op list * focus)
  | HandleAPIError of apiError
  | GetUnlockedDBsAPICall
  | GetWorkerStatsAPICall of tlid
  | ExecutingFunctionAPICall of tlid * id * string
  | TriggerHandlerAPICall of tlid
  | UpdateDBStatsAPICall of tlid
  (* End API Calls *)
  | DisplayAndReportError of string * string option * string option
  | DisplayError of string
  | ClearError
  | Select of tlid * tlidSelectTarget
  | SetHover of tlid * id
  | ClearHover of tlid * id
  | Deselect
  | RemoveToplevel of toplevel
  | RemoveGroup of toplevel
  | SetToplevels of handler list * db list * group list * bool
  | UpdateToplevels of handler list * db list * bool
  | SetDeletedToplevels of handler list * db list
  | UpdateDeletedToplevels of handler list * db list
  | UpdateAnalysis of analysisEnvelope
  | SetUserFunctions of userFunction list * userFunction list * bool
  | SetUnlockedDBs of unlockedDBs
  | AppendUnlockedDBs of unlockedDBs
  | Append404s of fourOhFour list
  | Delete404 of fourOhFour
  | Enter of entryCursor
  | EnterWithOffset of entryCursor * int
  | UpdateWorkerStats of tlid * workerStats
  | UpdateWorkerSchedules of string StrDict.t
  | NoChange
  | MakeCmd of msg Tea.Cmd.t [@printer opaque "MakeCmd"]
  | AutocompleteMod of autocompleteMod
  | Many of modification list
  | PanCanvas of
      { viewportStart : vPos
      ; viewportCurr : vPos
      ; prevCursorState : cursorState }
  | DragTL of tlid * vPos * hasMoved * cursorState
  | TriggerIntegrationTest of string
  | EndIntegrationTest
  | SetCursorState of cursorState
  | SetPage of page
  | SetTLTraceID of tlid * traceID
  | ExecutingFunctionBegan of tlid * id
  | ExecutingFunctionComplete of (tlid * id) list
  | MoveCanvasTo of pos * isTransitionAnimated
  | UpdateTraces of traces
  | OverrideTraces of traces
  | UpdateTraceFunctionResult of
      tlid * traceID * id * fnName * dvalArgsHash * int * dval
  | AppendStaticDeploy of staticDeploy list
  (* designed for one-off small changes *)
  | TweakModel of (model -> model)
  | Apply of
      (   (* It can be tempting to call a function which returns
           * modifications. However, this can have a bug - the model
           * used to create those modifications can be wrong (if the
           * model was changed by previous modifications). Apply can
           * be used to call the functions and apply the modifications,
           * so that the latest model is use. *)
          model
       -> modification)
  | SetTypes of userTipe list * userTipe list * bool
  | SetPermission of permission option
  | CenterCanvasOn of tlid
  | InitIntrospect of toplevel list
  | RefreshUsages of tlid list
  | UpdateDBStats of dbStatsStore
  | FluidCommandsShow of tlid * id
  | FluidCommandsClose
  (* We need to track clicks so that we don't mess with the caret while a
   * click is happening. *)
  | FluidStartClick
  | FluidEndClick
  | UpdateAvatarList of avatar list
  | ExpireAvatars
  | AddGroup of group
  | AddToGroup of tlid * tlid
  | UndoGroupDelete of tlid * group
  | MoveMemberToNewGroup of tlid * tlid * model
  | ShowSaveToast
  | SetClipboardContents of clipboardContents * clipboardEvent
  | UpdateASTCache of tlid * string
  | InitASTCache of handler list * userFunction list
  | FluidSetState of fluidState
  | TLMenuUpdate of tlid * menuMsg
  | SettingsViewUpdate of settingsMsg

(* ------------------- *)
(* Msgs *)
(* ------------------- *)

(* https://rawgit.com/w3c/input-events/v1/index.html#interface-InputEvent-Attributes *)
and fluidInputEvent =
  | Keypress of FluidKeyboard.keyEvent (* Backwards compatibility. Not an InputEvent inputType.*)
  | InsertText of string
  | DeleteContentBackward
  | DeleteContentForward
  | DeleteWordBackward
  | DeleteWordForward
  | DeleteSoftLineBackward
  | DeleteSoftLineForward
  | ReplaceText of string

and fluidMouseUp =
  { tlid : tlid
  ; selection :
      (* The (int * int) here represents the selection beginning + end (the
       * selection may be left->right or right->left) If the selection is None, the
       * selection will be read from the browser rather than the browser's
       * selection being set. *)
      (int * int) option
  ; editorIdx :
      (* editorIdx tells which fluid editor was clicked on.
       * see fluidState.activeEditorPanelIdx *)
      int }

and fluidMsg =
  | FluidAutocompleteClick of fluidAutocompleteItem
  | FluidInputEvent of fluidInputEvent
  | FluidCut
  | FluidPaste of clipboardContents
  | FluidMouseDown of tlid
  | FluidMouseUp of fluidMouseUp
  | FluidCommandsFilter of string
  | FluidCommandsClick of command
  | FluidFocusOnToken of id
  | FluidClearErrorDvSrc
  | FluidUpdateAutocomplete
  (* Index of the dropdown(autocomplete or command palette) item *)
  | FluidUpdateDropdownIndex of int
  | FluidCloseCmdPalette

and segmentTrack =
  | WelcomeModal
  | OpenDocs

and msg =
  | IgnoreMsg
  | FluidMsg of fluidMsg
  | AppMouseDown of mouseEvent
  | AppMouseDrag of Tea.Mouse.position [@printer opaque "AppMouseDrag"]
  | AppMouseUp of mouseEvent
  | WindowMouseUp of mouseEvent
  | TLDragRegionMouseDown of tlid * mouseEvent
  (* we have the actual node when TLDragRegionMouseUp is created, *)
  (* but by the time we use it the proper node will be changed *)
  | TLDragRegionMouseUp of tlid * mouseEvent
  | ToplevelClick of tlid * mouseEvent
  | ToplevelDelete of tlid
  | ToplevelDeleteForever of tlid
  | DragToplevel of tlid * Tea.Mouse.position [@printer opaque "DragToplevel"]
  | EntryInputMsg of string
  | EntrySubmitMsg
  | GlobalKeyPress of Keyboard.keyEvent
  | AutocompleteClick of int
  | AddOpsAPICallback of
      focus * addOpAPIParams * (addOpAPIResponse, httpError) Tea.Result.t
      [@printer opaque "AddOpsAPICallback"]
  | AddOpsStrollerMsg of addOpStrollerMsg
  | SaveTestAPICallback of (saveTestAPIResult, httpError) Tea.Result.t
      [@printer opaque "SavetestAPICallback"]
  | GetUnlockedDBsAPICallback of
      (getUnlockedDBsAPIResult, httpError) Tea.Result.t
      [@printer opaque "GetUnlockedDBsAPICallback"]
  | NewTracePush of (traceID * tlid list)
  | New404Push of fourOhFour
  | NewStaticDeployPush of staticDeploy
  | WorkerStatePush of string StrDict.t
  | Delete404APICallback of delete404APIParams * (unit, httpError) Tea.Result.t
      [@printer opaque "Delete404APICallback"]
  | InitialLoadAPICallback of
      focus * modification * (initialLoadAPIResult, httpError) Tea.Result.t
      [@printer opaque "InitialLoadAPICallback"]
  | FetchAllTracesAPICallback of (allTracesAPIResult, httpError) Tea.Result.t
      [@printer opaque "FetchAllTracesAPICallback"]
  | ExecuteFunctionAPICallback of
      executeFunctionAPIParams
      * (executeFunctionAPIResult, httpError) Tea.Result.t
      [@printer opaque "ExecuteFunctionAPICallback"]
  | UploadFnAPICallback of
      uploadFnAPIParams * (uploadFnAPIResult, httpError) Tea.Result.t
      [@printer opaque "UploadFunctionAPICallback"]
  | TriggerHandlerAPICallback of
      triggerHandlerAPIParams
      * (triggerHandlerAPIResult, httpError) Tea.Result.t
      [@printer opaque "TriggerHandlerAPICallback"]
  | LoadPackagesAPICallback of (loadPackagesAPIResult, httpError) Tea.Result.t
      [@printer opaque "LoadPackagesAPICallback"]
  | LogoutAPICallback [@printer opaque "LogoutAPICallback"]
  | Delete404APICall of fourOhFour
  | NewPresencePush of avatar list
  | LocationChange of Web.Location.location [@printer opaque "LocationChange"]
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleEditorSetting of (editorSettings -> editorSettings)
  | ExecuteFunctionButton of tlid * id * string
  | ExecuteFunctionFromWithin of executeFunctionAPIParams
  | CreateHandlerFrom404 of fourOhFour
  | TimerFire of timerAction * Tea.Time.t [@printer opaque "TimerFire"]
  | JSError of string
  | PageVisibilityChange of PageVisibility.visibility
  | StartFeatureFlag
  | EndFeatureFlag of id * pick
  | ToggleFeatureFlag of id * bool
  | DeleteUserFunctionParameter of tlid * userFunctionParameter
  | AddUserFunctionParameter of tlid
  | UploadFn of tlid
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
  | StartMigration of tlid
  | AbandonMigration of tlid
  | DeleteColInDB of tlid * id
  | MarkRoutingTableOpen of bool * string
  | CreateDBTable
  | ClipboardCopyEvent of clipboardEvent
  | ClipboardCutEvent of clipboardEvent
  | ClipboardPasteEvent of clipboardEvent
  | ClipboardCopyLivevalue of string * vPos
  | EventDecoderError of string * string * string
  | UpdateHandlerState of tlid * handlerState
  | CanvasPanAnimationEnd
  | GoTo of page
  | SetHoveringReferences of tlid * id list
  | TriggerSendPresenceCallback of (unit, httpError) Tea.Result.t
      [@printer opaque "TriggerSendPresenceCallback"]
  | TakeOffErrorRail of tlid * id
  | SetHandlerExeIdle of tlid
  | CopyCurl of tlid * vPos
  | TLMenuMsg of tlid * menuMsg
  | ResetToast
  | UpdateMinimap of string option
  | GoToArchitecturalView
  | DeleteGroup of tlid
  | DragGroupMember of tlid * tlid * mouseEvent
  | CreateGroup
  | HideTopbar
  | LogoutOfDark
  | DismissErrorBar
  | PauseWorker of string
  | RunWorker of string
  | UpdateWorkerScheduleCallback of (string StrDict.t, httpError) Tea.Result.t
      [@printer opaque "UpdateWorkerScheduleCallback"]
  | NewTabFromTLMenu of string * tlid
  | CloseWelcomeModal
  | FnParamMsg of fnpMsg
  | UpdateSegment of segmentTrack
  | SettingsViewMsg of settingsMsg

(* ----------------------------- *)
(* AB tests *)
(* ----------------------------- *)
and variantTest =
  | (* does nothing variant just so we can leave this in place
     * if we're not testing anything else *)
      StubVariant
  | GroupVariant
  | FeatureFlagVariant

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
  ; execution : exeState }

and tlTraceIDs = traceID TLIDDict.t

(* Testing *)
and testResult = (string, unit) Result.t

and integrationTestState =
  | IntegrationTestExpectation of (model -> testResult)
  | IntegrationTestFinished of testResult
  | NoIntegrationTest

(*
 * Fluid
 *)
(* eg ("value","Int") *)
and placeholder =
  { name : string
  ; tipe : string }

and fluidToken =
  | TInteger of id * string
  | TString of id * string
  (* multi-line strings: id, segment, start offset, full-string *)
  | TStringMLStart of id * string * int * string
  | TStringMLMiddle of id * string * int * string
  | TStringMLEnd of id * string * int * string
  | TBlank of id
  | TPlaceholder of
      { blankID : id
      ; fnID : id
      ; placeholder : placeholder }
  | TTrue of id
  | TFalse of id
  | TNullToken of id
  | TFloatWhole of id * string
  | TFloatPoint of id
  | TFloatFractional of id * string
  (* If you're filling in an expr, but havent finished it. Not used for
   * non-expr names. *)
  | TPartial of id * string
  (* A partial that extends out to the right. Used to create binops. *)
  | TRightPartial of id * string
  (* When a partial used to be another thing, we want to show the name of the
   * old thing in a non-interactable way *)
  | TPartialGhost of id * string
  (* the id here disambiguates with other separators for reflow *)
  | TSep of id
  (* The first id is the id of the expression directly associated with the
   * newline. The second id is the id of that expression's parent. In an
   * expression with potentially many newlines (ie, a pipeline), the int holds
   * the relative line number (index) of this newline. *)
  | TNewline of (id * id * int option) option
  | TIndent of int
  | TLetKeyword of id * analysisID
  (* Let-expr id * rhs id * varname *)
  | TLetVarName of id * analysisID * string
  | TLetAssignment of id * analysisID
  | TIfKeyword of id
  | TIfThenKeyword of id
  | TIfElseKeyword of id
  | TBinOp of id * string
  | TFieldOp of (* fieldAccess *) id * (* lhs *) id
  | TFieldName of id (* fieldAccess *) * id (* lhs *) * string
  | TFieldPartial of
      (* Partial ID, fieldAccess ID, analysisID (lhs), name *) id
      * id
      * id
      * string
  | TVariable of id * string
  (* id, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'DB::getAll'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3'), sendToRail *)
  | TFnName of id * string * string * string * FluidExpression.sendToRail
  (* id, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'v3'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3') *)
  | TFnVersion of id * string * string * string
  | TLambdaComma of id * int
  | TLambdaArrow of id
  | TLambdaSymbol of id
  | TLambdaVar of id * analysisID * int * string
  | TListOpen of id
  | TListClose of id
  | TListComma of id * int
  (* 2nd int is the number of pipe segments there are *)
  | TPipe of id * int * int
  | TRecordOpen of id
  | TRecordFieldname of
      { recordID : id
      ; exprID : id
      ; index : int
      ; fieldName : string }
  | TRecordSep of id * int * analysisID
  | TRecordClose of id
  | TMatchKeyword of id
  | TMatchBranchArrow of
      { matchID : id
      ; patternID : id
      ; index : int }
  (* for all these TPattern* variants:
   * - the first id is the match id
   * - the second id is the pattern id
   * - the final int is the index of the (pattern -> expr) *)
  | TPatternVariable of id * id * string * int
  | TPatternConstructorName of id * id * string * int
  | TPatternInteger of id * id * string * int
  | TPatternString of
      { matchID : id
      ; patternID : id
      ; str : string
      ; branchIdx : int }
  | TPatternTrue of id * id * int
  | TPatternFalse of id * id * int
  | TPatternNullToken of id * id * int
  | TPatternFloatWhole of id * id * string * int
  | TPatternFloatPoint of id * id * int
  | TPatternFloatFractional of id * id * string * int
  | TPatternBlank of id * id * int
  | TConstructorName of id * string
  | TParenOpen of id
  | TParenClose of id
  | TFlagWhenKeyword of id
  | TFlagEnabledKeyword of id

and fluidTokenInfo =
  { startRow : int
  ; startCol : int
  ; startPos : int
  ; endPos : int
  ; length : int
  ; token : fluidToken }

and fluidPatternAutocomplete =
  | FPAVariable of id * id * string
  | FPAConstructor of id * id * string * FluidPattern.t list
  | FPANull of id * id
  | FPABool of id * id * bool

and fluidAutocompleteItem =
  | FACFunction of function_
  | FACConstructorName of string * int
  | FACField of string
  | FACVariable of string * dval option
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
  ; location : (tlid * id) option
  ; filter : string option }

and fluidState =
  { error : string option
  ; actions : string list
  ; oldPos : int
  ; newPos : int
  ; upDownCol :
      (* When moving up or down and going through whitespace,
       * track the column so we can go back to it *)
      int option
  ; lastInput : fluidInputEvent
  ; ac : fluidAutocompleteState
  ; cp : fluidCommandState
  ; selectionStart : int option (* The selection ends at newPos *)
  ; midClick :
      (* If we get a renderCallback between a mousedown and a mouseUp, we
       * lose the information we're trying to get from the click. *)
      bool
  ; errorDvSrc :
      (* The source id of an error-dval of where the cursor is on and we might
       * have recently jumped to *)
      dval_source
  ; activeEditorPanelIdx :
      (* activeEditorPanelIdx is the 0-based index of the editor that is active inside
       * the current TL. Most TLs will only have a single editor most of the
       * time, but when displaying, eg, a feature flag condition there will be
       * multiple. This is used to place the caret correctly and modify the
       * correct set of tokens. idx=0 is always the "main" editor and should
       * always exist. *)
      int }

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
  { error : string option
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
  ; packageFns : packageFns
  ; traces : traces
  ; analyses : analyses
  ; f404s : fourOhFour list
  ; unlockedDBs : unlockedDBs
  ; integrationTestState
      (* State of individual integration tests *) :
      integrationTestState
  ; visibility : PageVisibility.visibility
  ; syncState : syncState
  ; executingFunctions : (tlid * id) list
  ; tlTraceIDs : tlTraceIDs (* This is TLID id to traceID map *)
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
  ; staticDeploys : staticDeploy list
        (* tlRefersTo : to answer the question "what TLs does this TL refer to". eg
   * if myFunc was called in Repl2 at id, then the dict would be:
   *
   *   { repl2.tlid: { (myFunc.tlid, id) } }
   *
   * which you can read as "repl2 refersTo myfunc". So a tlid points to the TLs
   * it uses. *)
  ; tlRefersTo : IDPairSet.t TLIDDict.t
        (* tlUsedIn: to answer the question "what TLs is this TL's name used in".  eg
   * if myFunc was called in Repl2, the dict would
   *
   *   { myfunc.tlid: { repl2.tlid }}
   *
   * which you can read as "myfunc is used in repl2".  *)
  ; tlUsedIn : TLIDSet.t TLIDDict.t
  ; fluidState : fluidState
  ; dbStats : dbStatsStore
  ; workerStats : workerStats TLIDDict.t
  ; avatarsList : avatar list
  ; browserId : string
  ; sidebarOpen : bool
  ; isAdmin : bool
  ; buildHash : string
  ; lastReload : (Js.Date.t[@opaque]) option
  ; opCtrs : int StrDict.t
  ; clientOpCtrId : string
  ; permission : permission option
  ; showTopbar : bool
  ; toast : toast
  ; username : string
  ; account : account
  ; worker_schedules : string StrDict.t
  ; searchCache : string TLIDDict.t
  ; editorSettings : editorSettings
  ; teaDebuggerEnabled : bool
  ; unsupportedBrowser : bool
  ; tlMenus : menuState TLIDDict.t
  ; showUserWelcomeModal : bool
  ; currentUserFn : fnProps
  ; settingsView : settingsViewState }

and savedUserSettings = {showUserWelcomeModal : bool}

and savedSettings =
  { editorSettings : editorSettings
  ; cursorState : cursorState
  ; routingTableOpenDetails : StrSet.t
  ; tlTraceIDs : tlTraceIDs
  ; featureFlags : flagsVS
  ; handlerProps : handlerProp TLIDDict.t
  ; canvasPos : pos
  ; lastReload : (Js.Date.t[@opaque]) option
  ; sidebarOpen : bool
  ; showTopbar : bool }
[@@deriving show {with_path = false}]

and permission =
  | Read
  | ReadWrite
[@@deriving show eq ord]
