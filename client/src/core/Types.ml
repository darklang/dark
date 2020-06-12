open Tc

(* == legacy aliases == *)
module TLIDDict = TLID.Dict
module TLIDSet = TLID.Set

type analysisID = ID.t [@@deriving show]

type parentBlockID = ID.t [@@deriving show]

(* == end legacy aliases == *)

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
and 'a blankOr =
  | Blank of ID.t
  | F of ID.t * 'a
[@@deriving show {with_path = false}]

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

module IDPair = Pair (TLID.T) (ID.T)
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
[@@deriving show]

module TypeInformation = struct
  type t =
    { fnName : string
    ; paramName : string
    ; returnType : tipe }
  [@@deriving show]

  let default : t =
    {fnName = "Unknown"; paramName = "Unknown"; returnType = TAny}
end

(* ---------------------- *)
(* Exprs and AST types *)
(* ---------------------- *)
type fnName = string

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
  | PFnReturnTipe of tipe blankOr
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
  | FnReturnTipe
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
  { usedIn : TLID.t
  ; refersTo : TLID.t
  ; id : ID.t }

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
  { ast : FluidAST.t
  ; spec : handlerSpec
  ; hTLID : TLID.t
  ; pos : pos }

(* groups *)
and group =
  { gName : string blankOr
  ; gTLID : TLID.t
  ; members : TLID.t list
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
  { dbTLID : TLID.t
  ; dbName : dbName blankOr
  ; cols : dbColumn list
  ; version : int
  ; oldMigrations : dbMigration list
  ; activeMigration : dbMigration option
  ; pos : pos }

and functionTypes =
  | UserFunction of userFunction
  | PackageFn of packageFn

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
  { ufTLID : TLID.t
  ; ufMetadata : userFunctionMetadata
  ; ufAST : FluidAST.t }

and userRecordField =
  { urfName : string blankOr
  ; urfTipe : tipe blankOr }

and userTipeDefinition = UTRecord of userRecordField list

and userTipe =
  { utTLID : TLID.t
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
  ; pfTLID : TLID.t }

(* toplevels *)
and toplevel =
  | TLHandler of handler
  | TLDB of db
  | TLPmFunc of packageFn
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
  | SourceId of TLID.t * ID.t

and dblock_args =
  { symtable : dval StrDict.t
  ; params : (ID.t * string) list
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
  | ARInteger of ID.t
  | ARBool of ID.t
  | ARString of ID.t * astStringPart
  | ARFloat of ID.t * astFloatPart
  | ARNull of ID.t
  | ARBlank of ID.t
  | ARLet of ID.t * astLetPart
  | ARIf of ID.t * astIfPart
  | ARBinOp of ID.t (* matches the operator *)
  | ARFieldAccess of ID.t * astFieldAccessPart
  | ARVariable of ID.t
  | ARFnCall of ID.t (* Matches the fn name+version *)
  | ARPartial of ID.t
  | ARRightPartial of ID.t
  | ARLeftPartial of ID.t
  | ARList of ID.t * astListPart
  | ARRecord of ID.t * astRecordPart
  | ARPipe of ID.t * int (* index of the pipe *)
  | ARConstructor of ID.t (* name of the constructor *)
  | ARMatch of ID.t * astMatchPart
  | ARLambda of ID.t * astLambdaPart
  | ARPattern of ID.t * astPatternPart
  | ARFlag of ID.t * astFlagPart
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
(* Scroll *)
(* ----------------------------- *)
type scrollEvent = {timeStamp : float} [@@deriving show {with_path = false}]

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
  | Filling of TLID.t * ID.t

and hasMoved = bool

and cursorState =
  | Selecting of TLID.t * ID.t option
  | Entering of entryCursor
  | FluidEntering of TLID.t
  | DraggingTL of TLID.t * vPos * hasMoved * cursorState
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
and avDict = ID.t StrDict.t StrDict.t

and inputValueDict = dvalDict

and analysisStore = intermediateResultStore loadable

and analyses = analysisStore (* indexed by traceID *) StrDict.t

and functionResult =
  { fnName : string
  ; callerID : ID.t
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

and traces = trace list (* indexed by TLID.t *) StrDict.t

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
and rollbackID = ID.t

and rollforwardID = ID.t

and op =
  | SetHandler of TLID.t * pos * handler
  | CreateDB of TLID.t * pos * dbName
  | AddDBCol of TLID.t * ID.t * ID.t
  | SetDBColName of TLID.t * ID.t * dbColName
  | SetDBColType of TLID.t * ID.t * dbColType
  | DeleteTL of TLID.t
  | MoveTL of TLID.t * pos
  | TLSavepoint of TLID.t
  | UndoTL of TLID.t
  | RedoTL of TLID.t
  | SetFunction of userFunction
  | DeleteFunction of TLID.t
  | ChangeDBColName of TLID.t * ID.t * dbColName
  | ChangeDBColType of TLID.t * ID.t * dbColType
  | DeprecatedInitDbm of
      TLID.t * ID.t * rollbackID * rollforwardID * dbMigrationKind
  | SetExpr of TLID.t * ID.t * FluidExpression.t
  | CreateDBMigration of TLID.t * rollbackID * rollforwardID * dbColumn list
  | AddDBColToDBMigration of TLID.t * ID.t * ID.t
  | SetDBColNameInDBMigration of TLID.t * ID.t * dbColName
  | SetDBColTypeInDBMigration of TLID.t * ID.t * dbColType
  | DeleteColInDBMigration of TLID.t * ID.t
  | AbandonDBMigration of TLID.t
  | DeleteDBCol of TLID.t * ID.t
  | RenameDBname of TLID.t * dbName
  | CreateDBWithBlankOr of TLID.t * pos * ID.t * dbName
  | DeleteTLForever of TLID.t
  | DeleteFunctionForever of TLID.t
  | SetType of userTipe
  | DeleteType of TLID.t
  | DeleteTypeForever of TLID.t

(* ------------------- *)
(* APIs *)
(* ------------------- *)
(* params *)
and sendPresenceParams = avatarModelMessage

and sendInviteParams = SettingsViewTypes.inviteFormMessage

and addOpAPIParams =
  { ops : op list
  ; opCtr : int
  ; clientOpCtrId : string }

and executeFunctionAPIParams =
  { efpTLID : TLID.t
  ; efpTraceID : traceID
  ; efpCallerID : ID.t
  ; efpArgs : dval list
  ; efpFnName : string }

and uploadFnAPIParams = {uplFn : userFunction}

and triggerHandlerAPIParams =
  { thTLID : TLID.t
  ; thTraceID : traceID
  ; thInput : inputValueDict }

and getTraceDataAPIParams =
  { gtdrpTlid : TLID.t
  ; gtdrpTraceID : traceID }

and dbStatsAPIParams = {dbStatsTlids : TLID.t list}

and workerStatsAPIParams = {workerStatsTlid : TLID.t}

and updateWorkerScheduleAPIParams =
  { workerName : string
  ; schedule : string }

and performHandlerAnalysisParams =
  { handler : handler
  ; traceID : traceID
  ; traceData : traceData
  ; dbs : db list
  ; userFns : userFunction list
  ; userTipes : userTipe list
  ; secrets : SecretTypes.t list }

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
  dval * dvalArgsHash * int * TLID.t list * unlockedDBs

and uploadFnAPIResult = unit

and loadPackagesAPIResult = packageFn list

and triggerHandlerAPIResult = TLID.t list

and unlockedDBs = StrSet.t

and getUnlockedDBsAPIResult = unlockedDBs

and get404sAPIResult = fourOhFour list

and getTraceDataAPIResult = {trace : trace}

and dbStatsAPIResult = dbStatsStore

and workerStatsAPIResult = workerStats

and allTracesAPIResult = {traces : (TLID.t * traceID) list}

and initialLoadAPIResult =
  { handlers : handler list
  ; deletedHandlers : handler list
  ; dbs : db list
  ; deletedDBs : db list
  ; userFunctions : userFunction list
  ; deletedUserFunctions : userFunction list
  ; unlockedDBs : unlockedDBs
  ; staticDeploys : staticDeploy list
  ; userTipes : userTipe list
  ; deletedUserTipes : userTipe list
  ; permission : permission option
  ; opCtrs : int StrDict.t
  ; groups : group list
  ; deletedGroups : group list
  ; account : account
  ; canvasList : string list
  ; orgs : string list
  ; orgCanvasList : string list
  ; workerSchedules : string StrDict.t
  ; secrets : SecretTypes.t list
  ; creationDate : Js.Date.t [@opaque] }

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

and previewSafety =
  | Safe
  | Unsafe

and fnOrigin =
  | UserFunction
  | PackageManager
  | Builtin

and function_ =
  { fnName : string
  ; fnParameters : parameter list
  ; fnDescription : string
  ; fnReturnTipe : tipe
  ; fnPreviewSafety : previewSafety
  ; fnDeprecated : bool
  ; fnInfix : bool
  ; fnOrigin :
      (* This is a client-side only field to be able to give different UX to
       * different functions *)
      fnOrigin }

(* autocomplete items *)
and literal = string

and displayText = string

(* Some AC items needs to be dynamically added to the list,
   while others can be filtered in and out of the list.
  For example: Goto will take you to focus on a toplevel.
  In the case of "Jump to", results are filtered by name,
    and do not need to be dynamically generated.
  But in the case of "Found in", results are dynamically generated,
    based on the content that is insID.t *e.
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
  ; action : model -> toplevel -> ID.t -> modification
  ; doc : string
  ; shouldShow : model -> toplevel -> FluidExpression.t -> bool }

and omniAction =
  | NewDB of dbName option
  | NewFunction of string option
  | NewHTTPHandler of string option
  | NewWorkerHandler of string option
  | NewCronHandler of string option
  | NewReplHandler of string option
  | NewGroup of string option
  | Goto of page * TLID.t * displayText * isDynamic

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
  | ACReturnTipe of tipe
  (* User types *)
  | ACTypeFieldTipe of tipe
  | ACTypeName of string
  | ACTypeFieldName of string
  (* Groups *)
  | ACGroupName of string

and target = TLID.t * blankOrData

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
(* Functions.ml *)
(* ------------------- *)
and functionsType =
  { builtinFunctions : function_ list
  ; packageFunctions : packageFns
  ; previewUnsafeFunctions :
      (* We do analysis to determine which functions are safe and which are not.
       * This stores the result *)
      StrSet.t
  ; allowedFunctions : function_ list }

and functionsProps =
  { usedFns : int StrDict.t
  ; userFunctions : userFunction TLIDDict.t }

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

(* TLMenu *)
and menuState = {isOpen : bool}

and menuMsg =
  | OpenMenu
  | CloseMenu

(* FnParams *)
and fnProps =
  { draggingParamIndex : int option
  ; dragOverSpaceIndex : int option
  ; justMovedParam : ID.t option }

and fnpMsg =
  | ParamDragStart of int
  | ParamDragDone
  | ParamEntersSpace of int
  | ParamLeavesSpace
  | ParamDropIntoSpace of int
  | Reset

(* Tool tips *)
and toolTipMsg =
  | OpenTooltip of tooltipSource
  | Close
  | OpenLink of string
  | OpenFnTooltip of bool
  | UpdateTutorial of tutorialMsg

and userTutorial =
  { step : tutorialStep option
  ; tlid : TLID.t option }

and tooltipState =
  { tooltipSource : tooltipSource option
  ; fnSpace : bool
  ; userTutorial : userTutorial }

(* Tutorial *)
and tutorialMsg =
  | NextStep
  | PrevStep
  | CloseTutorial
  | ReopenTutorial

(* Sidebar state *)
and sidebarMode =
  | DetailedMode
  | AbridgedMode

and sidebarState =
  { mode : sidebarMode
  ; openedCategories : StrSet.t }

and sidebarMsg =
  | ToggleSidebarMode
  | ResetSidebar
  | MarkCategoryOpen of bool * string

(* ------------------- *)
(* Modifications *)
(* ------------------- *)
and centerPage = bool

and page =
  | Architecture
  | FocusedPackageManagerFn of TLID.t
  | FocusedFn of TLID.t * traceID option
  | FocusedHandler of TLID.t * traceID option * centerPage
  | FocusedDB of TLID.t * centerPage
  | FocusedType of TLID.t
  | FocusedGroup of TLID.t * centerPage
  | SettingsModal of SettingsViewTypes.settingsTab

and focus =
  | FocusNothing
  | FocusExact of TLID.t * ID.t
  | FocusNext of TLID.t * ID.t option
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
  ; showHandlerASTs : bool
  ; runTimers : bool }

(* tlidSelectTarget represents a target insID.t *e a TLID for use
   by the `Select` modification.

   In Fluid, we should probably use STCaret in all cases --
   knowing the ID.t *of an ast node (via STID) is insufficient
   to know where to place the caret within that node.
   In non-fluid, the concept of a caret doesn't really exist;
   we select nodes at any nesting level as a whole, so STID is
   sufficient.

   If we want to select a toplevel as a whole but don't have a
   specific ID.t *in mind, we use STTopLevelRoot. There's a few
   places where we do this as a fallback when we expected to find
   an id but couldn't (they used to use Some(id) with an implicit
   fallback to None). *)
and tlidSelectTarget =
  | STCaret of caretTarget
  | STID of ID.t
  | STTopLevelRoot

and modification =
  | ReplaceAllModificationsWithThisOne of (model -> model * msg Tea.Cmd.t)
      (** ReplaceAllModificationsWithThisOne is a migration path away from modifications. It
        * takes in a model and directly returns a (model * msg Cmd.t) just like
        * The update function.
        *
        * This modification should be used in all new code.
        *
        * The intent is to completely replace all existing modifications with
        * this one, then remove the modification type entirely, directly
        * returning the (model * Cmd.t) from the update function *)
  (* API Calls *)
  | AddOps of (op list * focus)
  | HandleAPIError of apiError
  | GetUnlockedDBsAPICall
  | Get404sAPICall
  | GetWorkerStatsAPICall of TLID.t
  | ExecutingFunctionAPICall of TLID.t * ID.t * string
  | TriggerHandlerAPICall of TLID.t
  | UpdateDBStatsAPICall of TLID.t
  (* End API Calls *)
  | Select of TLID.t * tlidSelectTarget
  | SetHover of TLID.t * ID.t
  | ClearHover of TLID.t * ID.t
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
  | UpdateWorkerSchedules of string StrDict.t
  | NoChange
  | MakeCmd of msg Tea.Cmd.t [@printer opaque "MakeCmd"]
  | AutocompleteMod of autocompleteMod
  | Many of modification list
  | PanCanvas of
      { viewportStart : vPos
      ; viewportCurr : vPos
      ; prevCursorState : cursorState }
  | DragTL of TLID.t * vPos * hasMoved * cursorState
  | TriggerIntegrationTest of string
  | EndIntegrationTest
  | SetPage of page
  | SetTLTraceID of TLID.t * traceID
  | ExecutingFunctionBegan of TLID.t * ID.t
  | ExecutingFunctionComplete of (TLID.t * ID.t) list
  | MoveCanvasTo of pos * isTransitionAnimated
  | UpdateTraces of traces
  | OverrideTraces of traces
  | UpdateTraceFunctionResult of
      TLID.t * traceID * ID.t * fnName * dvalArgsHash * int * dval
  | AppendStaticDeploy of staticDeploy list
  (* designed for one-off small changes *)
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
  | CenterCanvasOn of TLID.t
  | InitIntrospect of toplevel list
  | RefreshUsages of TLID.t list
  | FluidCommandsShow of TLID.t * ID.t
  | FluidCommandsClose
  (* We need to track clicks so that we don't mess with the caret while a
   * click is happening. *)
  | FluidStartClick
  | FluidEndClick
  | UpdateAvatarList of avatar list
  | ExpireAvatars
  | AddGroup of group
  | AddToGroup of TLID.t * TLID.t
  | UndoGroupDelete of TLID.t * group
  | MoveMemberToNewGroup of TLID.t * TLID.t * model
  | SetClipboardContents of clipboardContents * clipboardEvent
  | UpdateASTCache of TLID.t * string
  | InitASTCache of handler list * userFunction list
  | FluidSetState of fluidState
  | TLMenuUpdate of TLID.t * menuMsg
  | SettingsViewUpdate of SettingsViewTypes.settingsMsg

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

and fluidMouseUpClickType =
  | SelectText of int * int (* selection read from the DOM *)
  | ClickAt of int

and fluidMouseUp =
  { tlid : TLID.t
  ; editor : fluidEditor
        (** fluidEditor is either MainEditor or a FeatureFlagEditor *)
  ; selection : fluidMouseUpClickType }

and fluidMouseDoubleClickType =
  | SelectExpressionAt of int
  (* [selectTokenAt start end]: the two ints represent the selection when the
   * double click happened, which might represent a token or a part of the
   * token (in the case of strings, a word sometimes) *)
  | SelectTokenAt of int * int

and fluidMouseDoubleClick =
  { tlid : TLID.t
  ; editor : fluidEditor
        (** fluidEditor is either MainEditor or a FeatureFlagEditor *)
  ; selection : fluidMouseDoubleClickType }

and fluidMsg =
  | FluidAutocompleteClick of fluidAutocompleteItem
  | FluidInputEvent of fluidInputEvent
  | FluidCut
  | FluidPaste of clipboardContents
  | FluidMouseDown of TLID.t
  | FluidMouseUp of fluidMouseUp
  | (* A mouse click has happened elsewhere that might have started in fluid, so
     * let fluid know *)
      FluidMouseUpExternal
  | FluidMouseDoubleClick of fluidMouseDoubleClick
  | FluidCommandsFilter of string
  | FluidCommandsClick of command
  | FluidFocusOnToken of TLID.t * ID.t
  | FluidClearErrorDvSrc
  | FluidUpdateAutocomplete
  (* Index of the dropdown(autocomplete or command palette) item *)
  | FluidUpdateDropdownIndex of int
  | FluidCloseCmdPalette

and segmentTrack =
  | WelcomeModal
  | OpenDocs
  | InviteUser
  | MarkCanvasAsShipped of string
  | MarkCanvasAsInDevelopment of string
  | OpenFnRef
  | OpenKeyboardRef

and msg =
  | IgnoreMsg of (* debug string so you know where it came from *) string
  | IgnoreMouseUp (* for nothingMouseEvent *)
  | FluidMsg of fluidMsg
  | AppMouseDown of mouseEvent
  | AppMouseDrag of Tea.Mouse.position [@printer opaque "AppMouseDrag"]
  | AppMouseUp of mouseEvent
  | AppScroll
  | WindowMouseUp of mouseEvent
  | TLDragRegionMouseDown of TLID.t * mouseEvent
  (* we have the actual node when TLDragRegionMouseUp is created, *)
  (* but by the time we use it the proper node will be changed *)
  | TLDragRegionMouseUp of TLID.t * mouseEvent
  | ToplevelDelete of TLID.t
  | ToplevelDeleteForever of TLID.t
  | DragToplevel of TLID.t * Tea.Mouse.position [@printer opaque "DragToplevel"]
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
  | Get404sAPICallback of (get404sAPIResult, httpError) Tea.Result.t
      [@printer opaque "Get404sAPICallback"]
  | NewTracePush of (traceID * TLID.t list)
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
  | InsertSecretCallback of (SecretTypes.t list, httpError) Tea.Result.t
      [@printer opaque "InsertSecretCallback"]
  | LogoutAPICallback [@printer opaque "LogoutAPICallback"]
  | Delete404APICall of fourOhFour
  | NewPresencePush of avatar list
  | LocationChange of Web.Location.location [@printer opaque "LocationChange"]
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleEditorSetting of (editorSettings -> editorSettings)
  | ExecuteFunctionButton of TLID.t * ID.t * string
  | ExecuteFunctionFromWithin of executeFunctionAPIParams
  | CreateHandlerFrom404 of fourOhFour
  | TimerFire of timerAction * Tea.Time.t [@printer opaque "TimerFire"]
  | JSError of string
  | PageVisibilityChange of PageVisibility.visibility
  | DeleteUserFunctionParameter of TLID.t * userFunctionParameter
  | AddUserFunctionParameter of TLID.t
  | UploadFn of TLID.t
  | DeleteUserTypeField of TLID.t * userRecordField
  | BlankOrClick of TLID.t * ID.t * mouseEvent
  | BlankOrDoubleClick of TLID.t * ID.t * mouseEvent
  | BlankOrMouseEnter of TLID.t * ID.t * mouseEvent
  | BlankOrMouseLeave of TLID.t * ID.t * mouseEvent
  | MouseWheel of int * int
  | TraceClick of TLID.t * traceID * mouseEvent
  | TraceMouseEnter of TLID.t * traceID * mouseEvent
  | TraceMouseLeave of TLID.t * traceID * mouseEvent
  | TriggerHandler of TLID.t
  | CreateRouteHandler of omniAction
  | SidebarMsg of sidebarMsg
  | CreateFunction
  | ExtractFunction
  | CreateType
  | DeleteUserFunction of TLID.t
  | DeleteUserFunctionForever of TLID.t
  | DeleteUserType of TLID.t
  | DeleteUserTypeForever of TLID.t
  | DeleteGroupForever of TLID.t
  | RestoreToplevel of TLID.t
  | LockHandler of TLID.t * bool
  | ReceiveAnalysis of performAnalysisResult
  | ReceiveFetch of fetchResult
  | EnablePanning of bool
  | StartMigration of TLID.t
  | AbandonMigration of TLID.t
  | DeleteColInDB of TLID.t * ID.t
  | CreateDBTable
  | ClipboardCopyEvent of clipboardEvent
  | ClipboardCutEvent of clipboardEvent
  | ClipboardPasteEvent of clipboardEvent
  | ClipboardCopyLivevalue of string * vPos
  | EventDecoderError of string * string * string
  | UpdateHandlerState of TLID.t * handlerState
  | CanvasPanAnimationEnd
  | GoTo of page
  | SetHoveringReferences of TLID.t * ID.t list
  | TriggerSendPresenceCallback of (unit, httpError) Tea.Result.t
      [@printer opaque "TriggerSendPresenceCallback"]
  | TakeOffErrorRail of TLID.t * ID.t
  | SetHandlerExeIdle of TLID.t
  | CopyCurl of TLID.t * vPos
  | TLMenuMsg of TLID.t * menuMsg
  | ResetToast
  | UpdateMinimap of string option
  | GoToArchitecturalView
  | DeleteGroup of TLID.t
  | DragGroupMember of TLID.t * TLID.t * mouseEvent
  | CreateGroup
  | HideTopbar
  | LogoutOfDark
  | DismissErrorBar
  | PauseWorker of string
  | RunWorker of string
  | UpdateWorkerScheduleCallback of (string StrDict.t, httpError) Tea.Result.t
      [@printer opaque "UpdateWorkerScheduleCallback"]
  | NewTabFromTLMenu of string * TLID.t
  | FnParamMsg of fnpMsg
  | ToolTipMsg of toolTipMsg
  | UpdateSegment of segmentTrack
  | SettingsViewMsg of SettingsViewTypes.settingsMsg
  | SecretMsg of SecretTypes.msg

(* ----------------------------- *)
(* AB tests *)
(* ----------------------------- *)
and variantTest =
  | (* does nothing variant just so we can leave this in place
     * if we're not testing anything else *)
      StubVariant
  | GroupVariant
  | NgrokVariant
  | LeftPartialVariant

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
      (* When hovering over a reference, this is the list of ID.ts that refer to
       * the reference *)
      ID.t list
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
  | TInteger of ID.t * string * parentBlockID option
  | TString of ID.t * string * parentBlockID option
  (* multi-line strings: ID.t *, segment, start offset, full-string *)
  | TStringMLStart of ID.t * string * int * string
  | TStringMLMiddle of ID.t * string * int * string
  | TStringMLEnd of ID.t * string * int * string
  | TBlank of ID.t * parentBlockID option
  | TPlaceholder of
      { blankID : ID.t
      ; fnID : ID.t
      ; parentBlockID : parentBlockID option
      ; placeholder : placeholder }
  | TTrue of ID.t * parentBlockID option
  | TFalse of ID.t * parentBlockID option
  | TNullToken of ID.t * parentBlockID option
  | TFloatWhole of ID.t * string * parentBlockID option
  | TFloatPoint of ID.t * parentBlockID option
  | TFloatFractional of ID.t * string * parentBlockID option
  (* If you're filling in an expr, but havent finished it. Not used for
   * non-expr names. *)
  | TPartial of ID.t * string * parentBlockID option
  (* A partial that extends out to the right. Used to create binops. *)
  (* A partial that preceeds an existing expression, used to wrap things in other things *)
  | TLeftPartial of ID.t * string * parentBlockID option
  | TRightPartial of ID.t * string * parentBlockID option
  (* When a partial used to be another thing, we want to show the name of the
   * old thing in a non-interactable way *)
  | TPartialGhost of ID.t * string * parentBlockID option
  (* the ID.t *here disambiguates with other separators for reflow *)
  | TSep of ID.t * parentBlockID option
  (* The first ID.t *is the ID.t *of the expression directly associated with the
   * newline. The second ID.t *is the ID.t *of that expression's parent. In an
   * expression with potentially many newlines (ie, a pipeline), the int holds
   * the relative line number (index) of this newline. *)
  | TNewline of (ID.t * ID.t * int option) option
  | TIndent of int
  | TLetKeyword of ID.t * analysisID * parentBlockID option
  (* Let-expr id * rhs id * varname *)
  | TLetVarName of ID.t * analysisID * string * parentBlockID option
  | TLetAssignment of ID.t * analysisID * parentBlockID option
  | TIfKeyword of ID.t * parentBlockID option
  | TIfThenKeyword of ID.t * parentBlockID option
  | TIfElseKeyword of ID.t * parentBlockID option
  | TBinOp of ID.t * string * parentBlockID option
  | TFieldOp of (* fieldAccess *) ID.t * (* lhs *) ID.t * parentBlockID option
  | TFieldName of
      ID.t (* fieldAccess *) * ID.t (* lhs *) * string * parentBlockID option
  | TFieldPartial of
      (* Partial ID, fieldAccess ID, analysisID (lhs), name *) ID.t
      * ID.t
      * ID.t
      * string
      * parentBlockID option
  | TVariable of ID.t * string * parentBlockID option
  (* ID.t, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'DB::getAll'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3'), sendToRail *)
  | TFnName of ID.t * string * string * string * FluidExpression.sendToRail
  (* ID.t, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'v3'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3') *)
  | TFnVersion of ID.t * string * string * string
  | TLambdaComma of ID.t * int * parentBlockID option
  | TLambdaArrow of ID.t * parentBlockID option
  | TLambdaSymbol of ID.t * parentBlockID option
  | TLambdaVar of ID.t * analysisID * int * string * parentBlockID option
  | TListOpen of ID.t * parentBlockID option
  | TListClose of ID.t * parentBlockID option
  | TListComma of ID.t * int
  (* 2nd int is the number of pipe segments there are *)
  | TPipe of ID.t * int * int * parentBlockID option
  | TRecordOpen of ID.t * parentBlockID option
  | TRecordFieldname of
      { recordID : ID.t
      ; exprID : ID.t
      ; parentBlockID : parentBlockID option
      ; index : int
      ; fieldName : string }
  | TRecordSep of ID.t * int * analysisID
  | TRecordClose of ID.t * parentBlockID option
  | TMatchKeyword of ID.t
  | TMatchBranchArrow of
      { matchID : ID.t
      ; patternID : ID.t
      ; index : int }
  (* for all these TPattern* variants:
   * - the first ID.t *is the match ID.t *
   * - the second ID.t *is the pattern ID.t *
   * - the final int is the index of the (pattern -> expr) *)
  | TPatternVariable of ID.t * ID.t * string * int
  | TPatternConstructorName of ID.t * ID.t * string * int
  | TPatternInteger of ID.t * ID.t * string * int
  | TPatternString of
      { matchID : ID.t
      ; patternID : ID.t
      ; str : string
      ; branchIdx : int }
  | TPatternTrue of ID.t * ID.t * int
  | TPatternFalse of ID.t * ID.t * int
  | TPatternNullToken of ID.t * ID.t * int
  | TPatternFloatWhole of ID.t * ID.t * string * int
  | TPatternFloatPoint of ID.t * ID.t * int
  | TPatternFloatFractional of ID.t * ID.t * string * int
  | TPatternBlank of ID.t * ID.t * int
  | TConstructorName of ID.t * string
  | TParenOpen of ID.t
  | TParenClose of ID.t
  | TFlagWhenKeyword of ID.t
  | TFlagEnabledKeyword of ID.t

and fluidTokenInfo =
  { startRow : int
  ; startCol : int
  ; startPos : int
  ; endPos : int
  ; length : int
  ; token : fluidToken }

and fluidPatternAutocomplete =
  | FPAVariable of ID.t * ID.t * string
  | FPAConstructor of ID.t * ID.t * string * FluidPattern.t list
  | FPANull of ID.t * ID.t
  | FPABool of ID.t * ID.t * bool

and fluidAutocompleteItem =
  | FACFunction of function_
  | FACConstructorName of string * int
  | FACField of string
  | FACVariable of string * dval option
  | FACLiteral of literal
  | FACKeyword of keyword
  | FACPattern of fluidPatternAutocomplete
  | FACCreateFunction of string * TLID.t * ID.t

and fluidAutocompleteData =
  { item : fluidAutocompleteItem
  ; validity : fluidAutocompleteValidity }

and fluidAutocompleteValidity =
  | FACItemValid
  | FACItemInvalidReturnType of TypeInformation.t
  | FACItemInvalidPipedArg of tipe

and fluidAutocompleteState =
  { (* ------------------------------- *)
    (* state *)
    (* ------------------------------- *)
    index : int option
  ; query :
      (* We need to refer back to the previous one *)
      (TLID.t * fluidTokenInfo) option
        (* ------------------------------- *)
        (* Cached results *)
        (* ------------------------------- *)
  ; completions : fluidAutocompleteData list }

and fluidCommandState =
  { index : int
  ; commands : command list
  ; location : (TLID.t * ID.t) option
  ; filter : string option }

and fluidEditor =
  | NoEditor
  | MainEditor of TLID.t
  | FeatureFlagEditor of TLID.t * ID.t
[@@deriving show {with_path = false}]

and fluidProps =
  { functions : functionsType
  ; variants : variantTest list }

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
      (* The source ID.t *of an error-dval of where the cursor is on and we might
       * have recently jumped to *)
      dval_source
  ; activeEditor : fluidEditor }

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
  ; tlid : TLID.t option
  ; canvasName : string
  ; timestamp : float }

and tutorialStep =
  | Welcome
  | VerbChange
  | ReturnValue
  | OpenTab
  | GettingStarted

and tooltipSource =
  | Http
  | Worker
  | Cron
  | Repl
  | Datastore
  | Function
  | FourOhFour
  | Deleted
  | PackageManager
  | StaticAssets
  | FnParam
  | FnMiniMap
  | Secrets

and model =
  { error : Error.t
  ; lastMsg : msg
  ; tests : variantTest list
  ; functions : functionsType
  ; complete : autocomplete
  ; cursorState : cursorState
  ; currentPage : page
  ; hovering : (TLID.t * ID.t) list
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
  ; integrationTestState
      (* State of individual integration tests *) :
      integrationTestState
  ; visibility : PageVisibility.visibility
  ; syncState : syncState
  ; executingFunctions : (TLID.t * ID.t) list
  ; tlTraceIDs : tlTraceIDs (* This is TLID ID.t *to traceID map *)
  ; featureFlags : flagsVS
  ; canvasProps : canvasProps
  ; canvasName : string
  ; userContentHost : string
  ; origin : string
  ; environment : string
  ; csrfToken : string
  ; usedDBs : int StrDict.t
  ; usedFns : int StrDict.t
  ; usedTipes : int StrDict.t
  ; handlerProps : handlerProp TLIDDict.t
  ; staticDeploys : staticDeploy list
        (* tlRefersTo : to answer the question "what TLs does this TL refer to". eg
   * if myFunc was called in Repl2 at id, then the dict would be:
   *
   *   { repl2.tlid { (myFunc.tlid, id) } }
   *
   * which you can read as "repl2 refersTo myfunc". So a TLID.t points to the TLs
   * it uses. *)
  ; tlRefersTo : (TLID.t * ID.t) list TLIDDict.t
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
  ; sidebarState : sidebarState
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
  ; workerSchedules : string StrDict.t
  ; searchCache : string TLIDDict.t
  ; editorSettings : editorSettings
  ; teaDebuggerEnabled : bool
  ; unsupportedBrowser : bool
  ; tlMenus : menuState TLIDDict.t
  ; firstVisitToDark : bool
        (* indicates if it is the users first time visiting any dark canvas *)
  ; tooltipState : tooltipState
  ; currentUserFn : fnProps
  ; settingsView : SettingsViewTypes.settingsViewState
  ; firstVisitToThisCanvas : bool
        (* indicates if it is the users first time this canvas *)
  ; secrets : SecretTypes.t list
  ; insertSecretModal : SecretTypes.insertModal }

and savedUserSettings =
  { firstVisitToDark : bool
  ; recordConsent : bool option }

and savedSettings =
  { editorSettings : editorSettings
  ; cursorState : cursorState
  ; tlTraceIDs : tlTraceIDs
  ; featureFlags : flagsVS
  ; handlerProps : handlerProp TLIDDict.t
  ; canvasPos : pos
  ; lastReload : (Js.Date.t[@opaque]) option
  ; sidebarState : sidebarState
  ; showTopbar : bool
  ; firstVisitToThisCanvas : bool
  ; userTutorial : tutorialStep option
  ; userTutorialTLID : TLID.t option }
[@@deriving show {with_path = false}]

and permission =
  | Read
  | ReadWrite
[@@deriving show eq ord]
