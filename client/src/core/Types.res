open Tc

module Belt = {
  include (Belt: module type of Belt with module Map := Belt.Map)

  module Map = {
    include (Belt.Map: module type of Belt.Map with module String := Belt.Map.String)

    module String = {
      include Belt.Map.String

      let pp = (
        valueFormatter: (Format.formatter, 'value) => unit,
        fmt: Format.formatter,
        map: t<'value>,
      ) => {
        Format.pp_print_string(fmt, "{ ")
        Belt.Map.String.forEach(map, (key, value) => {
          Format.pp_print_string(fmt, key)
          Format.pp_print_string(fmt, ": ")
          valueFormatter(fmt, value)
          Format.pp_print_string(fmt, ",  ")
        })
        Format.pp_print_string(fmt, "}")
        ()
      }
    }
  }
}

// -------------------
// Clipboard
// -------------------

type clipboardSetData = @meth (string, string) => unit
type clipboardGetData = @meth (string => string)
type clipboardPreventDefault = @meth (unit => unit)

type clipboardData = {"setData": clipboardSetData, "getData": clipboardGetData}

type clipboardEventDef = {"preventDefault": clipboardPreventDefault, "clipboardData": clipboardData}

@ppx.deriving(show) @opaque
type rec clipboardContents = /* Clipboard supports both text and encoded FluidExpression.ts. At the moment,
 * there is always a text option - there isn't a json option if the copied
 * string wasn't a FluidExpression.t */
(string, @opaque option<Js.Json.t>)

@ppx.deriving(show) @opaque type rec clipboardEvent = @opaque clipboardEventDef

// -------------------
// Standard types
// -------------------

module /* == legacy aliases == */ TLIDDict = TLID.Dict
module TLIDSet = TLID.Set

@ppx.deriving(show) type rec id = ID.t

@ppx.deriving(show) type rec analysisID = ID.t

@ppx.deriving(show) type rec parentBlockID = ID.t

// == end legacy aliases ==

let show_list = (~f: 'a => string, x: list<'a>): string =>
  "[" ++ (String.join(~sep=",", List.map(~f, x)) ++ "]")

let opaque = (msg, fmt, _) => {
  Format.pp_print_string(fmt, "<opaque:" ++ (msg ++ ">"))
  ()
}

// Probably deletable?
module PageVisibility = {
  @ppx.deriving(show)
  type rec visibility =
    | Hidden
    | Visible
}

type rec exception_ = {
  short: string,
  long: option<string>,
  exceptionTipe: string,
  actual: option<string>,
  actualType: option<string>,
  result: option<string>,
  resultType: option<string>,
  expected: option<string>,
  info: Map.String.t<string>,
  workarounds: list<string>,
}

// ----------------------
// Basic types
// ----------------------
@ppx.deriving(show({with_path: false}))
and blankOr<'a> =
  | Blank(ID.t)
  | F(ID.t, 'a)

// There are two coordinate systems. Pos is an absolute position in the
// canvas. Nodes and Edges have Pos'. VPos is the viewport: clicks occur
// within the viewport and we map Absolute positions back to the
// viewport to display in the browser.
// TODO: Can we depreciate VPos?
type rec pos = {
  x: int,
  y: int,
}

and vPos = {
  vx: int,
  vy: int,
}

// ----------------------
// Types
// ----------------------
@ppx.deriving(show)
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
  | TDbList(tipe)
  | TUserType(string, int)
  | TBytes

module TypeInformation = {
  @ppx.deriving(show)
  type rec t = {
    fnName: string,
    paramName: string,
    returnType: tipe,
  }

  let default: t = {fnName: "Unknown", paramName: "Unknown", returnType: TAny}
}

// ----------------------
// Exprs and AST types
// ----------------------
type rec fnName = string

and fluidExpr = FluidExpression.t

and fluidPattern = FluidPattern.t

// -----------------------------
// Pointers
// -----------------------------
and blankOrData =
  | PEventName(blankOr<string>)
  | PEventModifier(blankOr<string>)
  | PEventSpace(blankOr<string>)
  | PDBName(blankOr<string>)
  | PDBColName(blankOr<string>)
  | PDBColType(blankOr<string>)
  | PFnName(blankOr<string>)
  | PFnReturnTipe(blankOr<tipe>)
  | PParamName(blankOr<string>)
  | PParamTipe(blankOr<tipe>)
  | PTypeName(blankOr<string>)
  | PTypeFieldName(blankOr<string>)
  | PTypeFieldTipe(blankOr<tipe>)

@ppx.deriving(show({with_path: false}))
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

// ----------------------
// Toplevels
// ----------------------
type rec handlerSpaceName = string

and handlerName = string

and handlerModifer = string

// usedIn is a TL that's refered to in the refersTo tl at id
// refersTo is a TL that uses the usedIn tl at id
and usage = {
  usedIn: TLID.t,
  refersTo: TLID.t,
  id: ID.t,
}

// handlers
and handlerSpec = {
  space: blankOr<handlerSpaceName>,
  name: blankOr<handlerName>,
  modifier: blankOr<handlerModifer>,
}

and handlerSpace =
  | HSHTTP
  | HSCron
  | HSWorker
  | HSRepl
  | HSDeprecatedOther

and handler = {
  ast: FluidAST.t,
  spec: handlerSpec,
  hTLID: TLID.t,
  pos: pos,
}

// dbs
and dbName = string

and dbColName = string

and dbColType = string

and dbColumn = (blankOr<dbColName>, blankOr<dbColType>)

and dbMigrationKind = DeprecatedMigrationKind

and dbMigrationState =
  | DBMigrationAbandoned
  | DBMigrationInitialized

and dbMigration = {
  startingVersion: int,
  version: int,
  state: dbMigrationState,
  rollforward: FluidExpression.t,
  rollback: FluidExpression.t,
  cols: list<dbColumn>,
}

and db = {
  dbTLID: TLID.t,
  dbName: blankOr<dbName>,
  cols: list<dbColumn>,
  version: int,
  oldMigrations: list<dbMigration>,
  activeMigration: option<dbMigration>,
  pos: pos,
}

and functionTypes =
  | UserFunction(userFunction)
  | PackageFn(packageFn)

// userFunctions
and userFunctionParameter = {
  ufpName: blankOr<string>,
  ufpTipe: blankOr<tipe>,
  ufpBlock_args: list<string>,
  ufpOptional: bool,
  ufpDescription: string,
}

and userFunctionMetadata = {
  ufmName: blankOr<string>,
  ufmParameters: list<userFunctionParameter>,
  ufmDescription: string,
  ufmReturnTipe: blankOr<tipe>,
  ufmInfix: bool,
}

and userFunction = {
  ufTLID: TLID.t,
  ufMetadata: userFunctionMetadata,
  ufAST: FluidAST.t,
}

and userRecordField = {
  urfName: blankOr<string>,
  urfTipe: blankOr<tipe>,
}

and userTipeDefinition = UTRecord(list<userRecordField>)

and userTipe = {
  utTLID: TLID.t,
  utName: blankOr<string>,
  utVersion: int,
  utDefinition: userTipeDefinition,
}

// Package manager Functions
and packageFnParameter = {
  name: string,
  tipe: tipe,
  description: string,
}

and packageFn = {
  user: string,
  package: string,
  module_: string,
  fnname: string,
  version: int,
  body: fluidExpr,
  parameters: list<packageFnParameter>,
  return_type: tipe,
  description: string,
  author: string,
  deprecated: bool,
  pfTLID: TLID.t,
}

// toplevels
and toplevel =
  | TLHandler(handler)
  | TLDB(db)
  | TLPmFunc(packageFn)
  | TLFunc(userFunction)
  | TLTipe(userTipe)

and packageFns = TLIDDict.t<packageFn>

// ----------------------
// dvals
// ----------------------
and dhttp =
  | Redirect(string)
  | Response(int, list<(string, string)>)

and optionT =
  | OptJust(dval)
  | OptNothing

and resultT =
  | ResOk(dval)
  | ResError(dval)

and dval_source =
  | SourceNone
  | SourceId(TLID.t, ID.t)

and dblock_args = {
  /* We use Belt.Map.String as Map.String.t has a comparator that doesn't work
   with the cloning algorithm of web workers */
  symtable: Belt.Map.String.t<dval>,
  params: list<(ID.t, string)>,
  body: FluidExpression.t,
}

@ppx.deriving(show({with_path: false}))
and dval =
  | DInt(int)
  | DFloat(float)
  | DBool(bool)
  | DNull
  | DCharacter(string)
  | DStr(string)
  | DList(array<dval>)
  /* We use Belt.Map.String as Map.String.t has a comparator that doesn't work
   with the cloning algorithm of web workers */
  | DObj(Belt.Map.String.t<dval>)
  | DIncomplete(dval_source)
  | DError((dval_source, string))
  | DBlock(dblock_args)
  | DErrorRail(dval)
  | DResp(dhttp, dval)
  | DDB(string)
  | DDate(string)
  | DPassword(string)
  | DUuid(string)
  | DOption(optionT)
  | DResult(resultT)
  | DBytes(bytes)

// -----------------------------
// Referencing parts of an AST
// at the caret level
// -----------------------------

/* NOTE(JULIAN): the ast*Parts below are sketches of the types; they will likely change
 based on which specific parts of the AST we actually want to represent via astRef */

@ppx.deriving(show({with_path: false}))
type rec astFloatPart =
  | FPWhole
  | FPPoint
  | FPFractional

@ppx.deriving(show({with_path: false})) type rec astStringPart = SPOpenQuote

@ppx.deriving(show({with_path: false}))
type rec astLetPart =
  | LPKeyword
  | LPVarName
  | LPAssignment

@ppx.deriving(show({with_path: false}))
type rec astIfPart =
  | IPIfKeyword
  | IPThenKeyword
  | IPElseKeyword

@ppx.deriving(show({with_path: false}))
type rec astLambdaPart =
  | LBPSymbol
  | LBPVarName(/* index of the var */ int)
  | LBPComma(/* index of the var */ int)
  | LBPArrow

@ppx.deriving(show({with_path: false}))
type rec astFieldAccessPart =
  | FAPFieldname
  | FAPFieldOp

@ppx.deriving(show({with_path: false}))
type rec astRecordPart =
  | RPOpen
  | RPFieldname(/* index of the <fieldname,value> pair */ int)
  | RPFieldSep(/* index of the <fieldname,value> pair */ int)
  | RPClose

@ppx.deriving(show({with_path: false}))
type rec astListPart =
  | LPOpen
  | LPClose
  | LPComma(int)

@ppx.deriving(show({with_path: false}))
type rec astMatchPart =
  | MPKeyword
  | MPBranchArrow(/* index of the branch */ int)

@ppx.deriving(show({with_path: false}))
type rec astPatternPart =
  | PPVariable
  | PPConstructor
  | PPInteger
  | PPBool
  | PPString(astStringPart)
  | PPFloat(astFloatPart)
  | PPNull
  | PPBlank

@ppx.deriving(show({with_path: false}))
type rec astFlagPart =
  | FPWhenKeyword
  | FPEnabledKeyword

/* An astRef represents a reference to a specific part of an AST node,
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
 */
@ppx.deriving(show({with_path: false}))
type rec astRef =
  | ARInteger(ID.t)
  | ARBool(ID.t)
  | ARString(ID.t, astStringPart)
  | ARFloat(ID.t, astFloatPart)
  | ARNull(ID.t)
  | ARBlank(ID.t)
  | ARLet(ID.t, astLetPart)
  | ARIf(ID.t, astIfPart)
  | ARBinOp(ID.t) // matches the operator
  | ARFieldAccess(ID.t, astFieldAccessPart)
  | ARVariable(ID.t)
  | ARFnCall(ID.t) // Matches the fn name+version
  | ARPartial(ID.t)
  | ARRightPartial(ID.t)
  | ARLeftPartial(ID.t)
  | ARList(ID.t, astListPart)
  | ARRecord(ID.t, astRecordPart)
  | ARPipe(ID.t, int) // index of the pipe
  | ARConstructor(ID.t) // name of the constructor
  | ARMatch(ID.t, astMatchPart)
  | ARLambda(ID.t, astLambdaPart)
  | ARPattern(ID.t, astPatternPart)
  | ARFlag(ID.t, astFlagPart)
  // for use if something that should never happen happened
  | ARInvalid

/* A caretTarget represents a distinct caret location within the AST.
   By combining a reference to part of the AST and a caret offset
   into that part of the AST, we can uniquely represent a place
   for the caret to jump during AST transformations, even ones that
   drastically change the token stream. */
@ppx.deriving(show({with_path: false}))
type rec caretTarget = {
  astRef: astRef,
  offset: int,
}

// -----------------------------
// Scroll
// -----------------------------
@ppx.deriving(show({with_path: false})) type rec scrollEvent = {timeStamp: float}

// -----------------------------
// Mouse
// -----------------------------
type rec mouseEvent = {
  mePos: vPos,
  button: int,
  altKey: bool,
  ctrlKey: bool,
  shiftKey: bool,
  detail: int,
}

and isLeftButton = bool

// -----------------------------
// CursorState
// -----------------------------
and hasMoved = bool

/* CursorState represents what the user is focussed on and where their actions
 * (notably keypresses, but also things like autocomplete and refactoring) are
 * intended to work on */
and cursorState =
  | /* Show the onmibox. If we know the position the user wants the handler
   * to be at (presumably because they clicked there to get the omnibox),
   * then use it. Otherwise, if there's no position, we'll pick one for them
   * later */
  Omnibox(option<pos>)
  | /* Partially deprecated. This used to indicate when you had selected a
   * blankOr, but were not "entering" it. However, this mostly only made
   * sense for code, and now that code is fluid this is just annoying and
   * weird. */
  Selecting(TLID.t, option<ID.t>)
  | // When we're editing a blankOr
  Entering(TLID.t, ID.t)
  | /* When we're editing code (in the fluid
   editor) */
  FluidEntering(TLID.t)
  | /* When we're dragging toplevels -
      the old state is stored and reset
       * after moving is done */
  DraggingTL(TLID.t, vPos, hasMoved, cursorState)
  | /* For dragging
   the canvas. The old state is stored and reset later. */
  PanningCanvas({
      viewportStart: vPos,
      viewportCurr: vPos,
      prevCursorState: cursorState,
    })
  | /* Doing nothing */ Deselected

// -------------------
// Analysis
// -------------------
and timerAction =
  | RefreshAnalysis
  | RefreshAvatars
  | CheckUrlHashPosition

and loadable<'result> =
  | LoadableSuccess('result)
  | LoadableNotInitialized
  | LoadableLoading(option<'result>)
  | LoadableError(string)

and dvalDict = Map.String.t<dval>

and executionResult =
  | ExecutedResult(dval)
  | NonExecutedResult(dval)

and intermediateResultStore = Belt.Map.String.t<executionResult>

/* map from expression ids to symbol table, which maps from varname strings to
 * the ids of the expressions that represent their values */
and avDict = Map.String.t<Map.String.t<ID.t>>

and inputValueDict = Belt.Map.String.t<dval>

and analysisStore = loadable<intermediateResultStore>

and analyses = // indexed by traceID
Map.String.t<
  analysisStore,
>

and functionResult = {
  fnName: string,
  callerID: ID.t,
  argHash: string,
  argHashVersion: int,
  value: dval,
}

and fetchRequest =
  | TraceFetch(getTraceDataAPIParams)
  | DbStatsFetch(dbStatsAPIParams)
  | WorkerStatsFetch(workerStatsAPIParams)

// traces/db_stats fetching
and fetchResult =
  | TraceFetchSuccess(getTraceDataAPIParams, getTraceDataAPIResult)
  | TraceFetchFailure(getTraceDataAPIParams, string, string)
  | TraceFetchMissing(getTraceDataAPIParams)
  | DbStatsFetchSuccess(dbStatsAPIParams, dbStatsAPIResult)
  | DbStatsFetchFailure(dbStatsAPIParams, string, string)
  | DbStatsFetchMissing(dbStatsAPIParams)
  | WorkerStatsFetchSuccess(workerStatsAPIParams, workerStatsAPIResult)
  | WorkerStatsFetchFailure(workerStatsAPIParams, string, string)
  | WorkerStatsFetchMissing(workerStatsAPIParams)

and fetchContext = {
  canvasName: string,
  csrfToken: string,
  origin: string,
}

and traceID = string

and traceData = {
  input: inputValueDict,
  timestamp: string,
  functionResults: list<functionResult>,
}

and traceOpt = (traceID, option<traceData>)

and traceError =
  /* NoneYet is a replacement for what was None when trace was a
   (traceID * traceData option) */
  | NoneYet
  // MaximumCallStackError is unrecoverable - don't try again
  | MaximumCallStackError

and trace = (traceID, Result.t<traceData, traceError>)

and traces = // indexed by TLID.t
Map.String.t<
  list<trace>,
>

and fourOhFour = {
  space: string,
  path: string,
  modifier: string,
  timestamp: string,
  traceID: string,
}

and deployStatus =
  | Deploying
  | Deployed

and staticDeploy = {
  deployHash: string,
  url: string,
  @opaque lastUpdate: Js.Date.t,
  status: deployStatus,
}

and dbStats = {
  count: int,
  example: option<(dval, string)>,
}

and dbStatsStore = Map.String.t<dbStats>

and workerStats = {
  count: int,
  schedule: option<string>,
}

// -------------------
// ops
// -------------------
and rollbackID = ID.t

and rollforwardID = ID.t

and op =
  | SetHandler(TLID.t, pos, handler)
  | CreateDB(TLID.t, pos, dbName)
  | AddDBCol(TLID.t, ID.t, ID.t)
  | SetDBColName(TLID.t, ID.t, dbColName)
  | SetDBColType(TLID.t, ID.t, dbColType)
  | DeleteTL(TLID.t)
  | MoveTL(TLID.t, pos)
  | TLSavepoint(TLID.t)
  | UndoTL(TLID.t)
  | RedoTL(TLID.t)
  | SetFunction(userFunction)
  | DeleteFunction(TLID.t)
  | ChangeDBColName(TLID.t, ID.t, dbColName)
  | ChangeDBColType(TLID.t, ID.t, dbColType)
  | DeprecatedInitDbm(TLID.t, ID.t, rollbackID, rollforwardID, dbMigrationKind)
  | SetExpr(TLID.t, ID.t, FluidExpression.t)
  | CreateDBMigration(TLID.t, rollbackID, rollforwardID, list<dbColumn>)
  | AddDBColToDBMigration(TLID.t, ID.t, ID.t)
  | SetDBColNameInDBMigration(TLID.t, ID.t, dbColName)
  | SetDBColTypeInDBMigration(TLID.t, ID.t, dbColType)
  | DeleteColInDBMigration(TLID.t, ID.t)
  | AbandonDBMigration(TLID.t)
  | DeleteDBCol(TLID.t, ID.t)
  | RenameDBname(TLID.t, dbName)
  | CreateDBWithBlankOr(TLID.t, pos, ID.t, dbName)
  | SetType(userTipe)
  | DeleteType(TLID.t)

// -------------------
// APIs
// -------------------
// params
and sendPresenceParams = avatarModelMessage

and sendInviteParams = SettingsViewTypes.inviteFormMessage

and addOpAPIParams = {
  ops: list<op>,
  opCtr: int,
  clientOpCtrId: string,
}

and executeFunctionAPIParams = {
  efpTLID: TLID.t,
  efpTraceID: traceID,
  efpCallerID: ID.t,
  efpArgs: list<dval>,
  efpFnName: string,
}

and deleteToplevelForeverAPIParams = {dtfTLID: TLID.t}

and uploadFnAPIParams = {uplFn: userFunction}

and triggerHandlerAPIParams = {
  thTLID: TLID.t,
  thTraceID: traceID,
  thInput: inputValueDict,
}

and getTraceDataAPIParams = {
  gtdrpTlid: TLID.t,
  gtdrpTraceID: traceID,
}

and dbStatsAPIParams = {dbStatsTlids: list<TLID.t>}

and workerStatsAPIParams = {workerStatsTlid: TLID.t}

and updateWorkerScheduleAPIParams = {
  workerName: string,
  schedule: string,
}

and performHandlerAnalysisParams = {
  handler: handler,
  traceID: traceID,
  traceData: traceData,
  dbs: list<db>,
  userFns: list<userFunction>,
  userTipes: list<userTipe>,
  secrets: list<SecretTypes.t>,
}

and performFunctionAnalysisParams = {
  func: userFunction,
  traceID: traceID,
  traceData: traceData,
  dbs: list<db>,
  userFns: list<userFunction>,
  userTipes: list<userTipe>,
  secrets: list<SecretTypes.t>,
}

and performAnalysisParams =
  | AnalyzeHandler(performHandlerAnalysisParams)
  | AnalyzeFunction(performFunctionAnalysisParams)

and analysisEnvelope = (traceID, intermediateResultStore)

and analysisError =
  | AnalysisExecutionError(performAnalysisParams, string)
  | AnalysisParseError(string)

and performAnalysisResult = Tc.Result.t<analysisEnvelope, analysisError>

and delete404APIParams = fourOhFour

and account = {
  name: string,
  email: string,
  username: string,
}

// results
and addOpAPIResult = {
  handlers: list<handler>,
  deletedHandlers: list<handler>,
  dbs: list<db>,
  deletedDBs: list<db>,
  userFunctions: list<userFunction>,
  deletedUserFunctions: list<userFunction>,
  userTipes: list<userTipe>,
  deletedUserTipes: list<userTipe>,
}

and addOpAPIResponse = {result: addOpAPIResult}

and addOpPusherMsg = {
  result: addOpAPIResult,
  params: addOpAPIParams,
}

and dvalArgsHash = string

and executeFunctionAPIResult = (dval, dvalArgsHash, int, list<TLID.t>, unlockedDBs)

and uploadFnAPIResult = unit

and loadPackagesAPIResult = list<packageFn>

and triggerHandlerAPIResult = list<TLID.t>

and unlockedDBs = Set.String.t

and getUnlockedDBsAPIResult = unlockedDBs

and get404sAPIResult = list<fourOhFour>

and getTraceDataAPIResult = {trace: trace}

and dbStatsAPIResult = dbStatsStore

and workerStatsAPIResult = workerStats

and allTracesAPIResult = {traces: list<(TLID.t, traceID)>}

and initialLoadAPIResult = {
  handlers: list<handler>,
  deletedHandlers: list<handler>,
  dbs: list<db>,
  deletedDBs: list<db>,
  userFunctions: list<userFunction>,
  deletedUserFunctions: list<userFunction>,
  unlockedDBs: unlockedDBs,
  staticDeploys: list<staticDeploy>,
  userTipes: list<userTipe>,
  deletedUserTipes: list<userTipe>,
  permission: option<permission>,
  opCtrs: Map.String.t<int>,
  account: account,
  canvasList: list<string>,
  orgs: list<string>,
  orgCanvasList: list<string>,
  workerSchedules: Map.String.t<string>,
  secrets: list<SecretTypes.t>,
  @opaque creationDate: Js.Date.t,
}

and saveTestAPIResult = string

// -------------------
// Autocomplete / entry
// -------------------
// functions
and parameter = {
  paramName: string,
  paramTipe: tipe,
  paramBlock_args: list<string>,
  paramOptional: bool,
  paramDescription: string,
}

and previewSafety =
  | Safe
  | Unsafe

and fnOrigin =
  | UserFunction
  | PackageManager
  | Builtin

and function_ = {
  fnName: string,
  fnParameters: list<parameter>,
  fnDescription: string,
  fnReturnTipe: tipe,
  fnPreviewSafety: previewSafety,
  fnDeprecated: bool,
  fnInfix: bool,
  fnIsSupportedInQuery: bool,
  fnOrigin: /* This is a client-side only field to be able to give different UX to
   * different functions */
  fnOrigin,
}

// autocomplete items
and literal = string

and displayText = string

/* Some AC items needs to be dynamically added to the list,
   while others can be filtered in and out of the list.
  For example: Goto will take you to focus on a toplevel.
  In the case of "Jump to", results are filtered by name,
    and do not need to be dynamically generated.
  But in the case of "Found in", results are dynamically generated,
    based on the content that is insID.t *e.
*/
and isDynamic = bool

and keyword =
  | KLet
  | KIf
  | KLambda
  | KMatch
  | KPipe

and command = {
  commandName: string,
  action: (model, toplevel, ID.t) => modification,
  doc: string,
  shouldShow: (model, toplevel, FluidExpression.t) => bool,
}

and omniAction =
  | NewDB(option<dbName>)
  | NewFunction(option<string>)
  | NewHTTPHandler(option<string>)
  | NewWorkerHandler(option<string>)
  | NewCronHandler(option<string>)
  | NewReplHandler(option<string>)
  | Goto(page, TLID.t, displayText, isDynamic)

and autocompleteItem =
  | ACOmniAction(omniAction)
  // HTTP
  | ACHTTPModifier(string)
  | ACHTTPRoute(string)
  // Workers
  | ACWorkerName(string)
  | ACEventSpace(string)
  | ACEventModifier(string)
  // Repl
  | ACReplName(string)
  // CRON
  | ACCronName(string)
  | ACCronTiming(string)
  // DBs
  | ACDBName(string)
  | ACDBColType(string)
  | ACDBColName(string)
  // User functions
  | ACFnName(string)
  | ACParamName(string)
  | ACParamTipe(tipe)
  | ACReturnTipe(tipe)
  // User types
  | ACTypeFieldTipe(tipe)
  | ACTypeName(string)
  | ACTypeFieldName(string)

and target = (TLID.t, blankOrData)

and autocomplete = {
  admin: bool,
  completions: list<autocompleteItem>,
  allCompletions: list<autocompleteItem>,
  index: int,
  value: string,
  prevValue: string,
  target: option<target>,
  visible: bool,
}

and autocompleteMod =
  | ACSetQuery(string)
  | ACAppendQuery(string)
  | ACReset
  | ACSelectDown
  | ACSelectUp
  | ACSetTarget(option<target>)
  | ACRegenerate
  | ACSetVisible(bool)

// -------------------
// Functions.ml
// -------------------
and functionsType = {
  builtinFunctions: list<function_>,
  packageFunctions: packageFns,
  previewUnsafeFunctions: /* We do analysis to determine which functions are safe and which are not.
   * This stores the result */
  Set.String.t,
  allowedFunctions: list<function_>,
}

and functionsProps = {
  usedFns: Map.String.t<int>,
  userFunctions: TLIDDict.t<userFunction>,
}

// ---------------
// Component Types
// ---------------

// TLMenu
and menuState = {isOpen: bool}

and menuMsg =
  | OpenMenu
  | CloseMenu

// FnParams
and fnProps = {
  draggingParamIndex: option<int>,
  dragOverSpaceIndex: option<int>,
  justMovedParam: option<ID.t>,
}

and fnpMsg =
  | ParamDragStart(int)
  | ParamDragDone
  | ParamEntersSpace(int)
  | ParamLeavesSpace
  | ParamDropIntoSpace(int)
  | Reset

// Tool tips
and toolTipMsg =
  | OpenTooltip(tooltipSource)
  | Close
  | OpenLink(string)
  | OpenFnTooltip(bool)
  | UpdateTutorial(tutorialMsg)

and userTutorial = {
  step: option<tutorialStep>,
  tlid: option<TLID.t>,
}

and tooltipState = {
  tooltipSource: option<tooltipSource>,
  fnSpace: bool,
  userTutorial: userTutorial,
}

// Tutorial
and tutorialMsg =
  | NextStep
  | PrevStep
  | CloseTutorial
  | ReopenTutorial

// Sidebar state
and sidebarMode =
  | DetailedMode
  | AbridgedMode

and sidebarState = {
  mode: sidebarMode,
  openedCategories: Set.String.t,
}

and sidebarMsg =
  | ToggleSidebarMode
  | ResetSidebar
  | MarkCategoryOpen(bool, string)

// -------------------
// Modifications
// -------------------
and centerPage = bool

and page =
  | Architecture
  | FocusedPackageManagerFn(TLID.t)
  | FocusedFn(TLID.t, option<traceID>)
  | FocusedHandler(TLID.t, option<traceID>, centerPage)
  | FocusedDB(TLID.t, centerPage)
  | FocusedType(TLID.t)
  | SettingsModal(SettingsViewTypes.settingsTab)

and focus =
  | FocusNothing
  | FocusExact(TLID.t, ID.t)
  | FocusNext(TLID.t, option<ID.t>)
  | FocusPageAndCursor(page, cursorState)
  | FocusSame
  // unchanged
  | FocusNoChange

and toast = {
  toastMessage: option<string>,
  toastPos: option<vPos>,
}

and isTransitionAnimated =
  | AnimateTransition
  | DontAnimateTransition

and canvasProps = {
  offset: pos,
  enablePan: bool,
  lastOffset: option<pos>,
  panAnimation: isTransitionAnimated,
}

and httpError = @opaque Tea.Http.error<string>

and errorImportance =
  | IgnorableError
  | ImportantError

and apiError = {
  context: string,
  originalError: httpError /* the Tea_http error */,
  requestParams: option<@opaque Js.Json.t>,
  reload: bool,
  importance: errorImportance,
}

// Editor settings are global settings on the editor. Initially, these are only things that admins use for debugging - in the future they could be extended to editor settings
and editorSettings = {
  showFluidDebugger: bool,
  showHandlerASTs: bool,
  runTimers: bool,
}

/* tlidSelectTarget represents a target insID.t *e a TLID for use
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
   fallback to None). */
and tlidSelectTarget =
  | STCaret(caretTarget)
  | STID(ID.t)
  | STTopLevelRoot

and modification =
  | @ocaml.doc(" ReplaceAllModificationsWithThisOne is a migration path away from modifications. It
        * takes in a model and directly returns a (model * msg Cmd.t) just like
        * The update function.
        *
        * This modification should be used in all new code.
        *
        * The intent is to completely replace all existing modifications with
        * this one, then remove the modification type entirely, directly
        * returning the (model * Cmd.t) from the update function ")
  ReplaceAllModificationsWithThisOne(model => (model, Tea.Cmd.t<msg>))

  // API Calls
  | AddOps((list<op>, focus))
  | HandleAPIError(apiError)
  | GetUnlockedDBsAPICall
  | Get404sAPICall
  | GetWorkerStatsAPICall(TLID.t)
  | ExecutingFunctionAPICall(TLID.t, ID.t, string)
  | TriggerHandlerAPICall(TLID.t)
  | UpdateDBStatsAPICall(TLID.t)
  | DeleteToplevelForeverAPICall(TLID.t)
  // End API Calls
  | Select(TLID.t, tlidSelectTarget)
  | SetHover(TLID.t, ID.t)
  | ClearHover(TLID.t, ID.t)
  | Deselect
  | RemoveToplevel(toplevel)
  | SetToplevels(list<handler>, list<db>, bool)
  | UpdateToplevels(list<handler>, list<db>, bool)
  | SetDeletedToplevels(list<handler>, list<db>)
  | UpdateDeletedToplevels(list<handler>, list<db>)
  | UpdateAnalysis(analysisEnvelope)
  | SetUserFunctions(list<userFunction>, list<userFunction>, bool)
  | SetUnlockedDBs(unlockedDBs)
  | AppendUnlockedDBs(unlockedDBs)
  | Append404s(list<fourOhFour>)
  | Delete404(fourOhFour)
  | Enter /* Enter a blankOr */(TLID.t, ID.t)
  | EnterWithOffset(
      // Entering a blankOr with a desired caret offset
      TLID.t,
      ID.t,
      int,
    )
  | OpenOmnibox /* Open the omnibox */(option<pos>)
  | UpdateWorkerSchedules(Map.String.t<string>)
  | NoChange
  | @printer(opaque("MakeCmd")) MakeCmd(Tea.Cmd.t<msg>)
  | AutocompleteMod(autocompleteMod)
  | Many(list<modification>)
  | PanCanvas({viewportStart: vPos, viewportCurr: vPos, prevCursorState: cursorState})
  | DragTL(TLID.t, vPos, hasMoved, cursorState)
  | TriggerIntegrationTest(string)
  | EndIntegrationTest
  | SetPage(page)
  | SetTLTraceID(TLID.t, traceID)
  | ExecutingFunctionBegan(TLID.t, ID.t)
  | ExecutingFunctionComplete(list<(TLID.t, ID.t)>)
  | MoveCanvasTo(pos, isTransitionAnimated)
  | UpdateTraces(traces)
  | OverrideTraces(traces)
  | UpdateTraceFunctionResult(TLID.t, traceID, ID.t, fnName, dvalArgsHash, int, dval)
  | AppendStaticDeploy(list<staticDeploy>)
  // designed for one-off small changes
  | Apply(
      /* It can be tempting to call a function which returns
       * modifications. However, this can have a bug - the model
       * used to create those modifications can be wrong (if the
       * model was changed by previous modifications). Apply can
       * be used to call the functions and apply the modifications,
       * so that the latest model is use. */
      model => modification,
    )
  | SetTypes(list<userTipe>, list<userTipe>, bool)
  | SetPermission(option<permission>)
  | CenterCanvasOn(TLID.t)
  | InitIntrospect(list<toplevel>)
  | RefreshUsages(list<TLID.t>)
  | FluidCommandsShow(TLID.t, ID.t)
  | FluidCommandsClose
  /* We need to track clicks so that we don't mess with the caret while a
   * click is happening. */
  | FluidStartClick
  | FluidEndClick
  | UpdateAvatarList(list<avatar>)
  | ExpireAvatars
  | SetClipboardContents(clipboardContents, clipboardEvent)
  | UpdateASTCache(TLID.t, string)
  | InitASTCache(list<handler>, list<userFunction>)
  | FluidSetState(fluidState)
  | TLMenuUpdate(TLID.t, menuMsg)
  | SettingsViewUpdate(SettingsViewTypes.settingsMsg)

// -------------------
// Msgs
// -------------------

// https://rawgit.com/w3c/input-events/v1/index.html#interface-InputEvent-Attributes
and fluidInputEvent =
  | Keypress(FluidKeyboard.keyEvent) // Backwards compatibility. Not an InputEvent inputType.
  | InsertText(string)
  | DeleteContentBackward
  | DeleteContentForward
  | DeleteWordBackward
  | DeleteWordForward
  | DeleteSoftLineBackward
  | DeleteSoftLineForward
  | ReplaceText(string)

and fluidMouseUpClickType =
  | SelectText(int, int) // selection read from the DOM
  | ClickAt(int)

and fluidMouseUp = {
  tlid: TLID.t,
  @ocaml.doc(" fluidEditor is either MainEditor or a FeatureFlagEditor ")
  editor: fluidEditor,
  selection: fluidMouseUpClickType,
}

and fluidMouseDoubleClickType =
  | SelectExpressionAt(int)
  /* [selectTokenAt start end]: the two ints represent the selection when the
   * double click happened, which might represent a token or a part of the
   * token (in the case of strings, a word sometimes) */
  | SelectTokenAt(int, int)

and fluidMouseDoubleClick = {
  tlid: TLID.t,
  @ocaml.doc(" fluidEditor is either MainEditor or a FeatureFlagEditor ")
  editor: fluidEditor,
  selection: fluidMouseDoubleClickType,
}

and fluidMsg =
  | FluidAutocompleteClick(fluidAutocompleteItem)
  | FluidInputEvent(fluidInputEvent)
  | FluidCut
  | FluidPaste(clipboardContents)
  | FluidMouseDown(TLID.t)
  | FluidMouseUp(fluidMouseUp)
  | /* A mouse click has happened elsewhere that might have started in fluid, so
   * let fluid know */
  FluidMouseUpExternal
  | FluidMouseDoubleClick(fluidMouseDoubleClick)
  | FluidCommandsFilter(string)
  | FluidCommandsClick(command)
  | FluidFocusOnToken(TLID.t, ID.t)
  | FluidClearErrorDvSrc
  | FluidUpdateAutocomplete
  // Index of the dropdown(autocomplete or command palette) item
  | FluidUpdateDropdownIndex(int)
  | FluidCloseCmdPalette

and heapioTrack =
  | WelcomeModal
  | OpenDocs
  | InviteUser
  | OpenFnRef
  | OpenKeyboardRef

and msg =
  | IgnoreMsg(/* debug string so you know where it came from */ string)
  | IgnoreMouseUp // for nothingMouseEvent
  | FluidMsg(fluidMsg)
  | AppMouseDown(mouseEvent)
  | @printer(opaque("AppMouseDrag")) AppMouseDrag(Tea.Mouse.position)
  | AppMouseUp(mouseEvent)
  | AppScroll
  | WindowMouseUp(mouseEvent)
  | TLDragRegionMouseDown(TLID.t, mouseEvent)
  // we have the actual node when TLDragRegionMouseUp is created,
  // but by the time we use it the proper node will be changed
  | TLDragRegionMouseUp(TLID.t, mouseEvent)
  | ToplevelDelete(TLID.t)
  | ToplevelDeleteForever(TLID.t)
  | @printer(opaque("DragToplevel")) DragToplevel(TLID.t, Tea.Mouse.position)
  | EntryInputMsg(string)
  | EntrySubmitMsg
  | GlobalKeyPress(Keyboard.keyEvent)
  | AutocompleteClick(int)
  | @printer(opaque("AddOpsAPICallback"))
  AddOpsAPICallback(focus, addOpAPIParams, Tea.Result.t<addOpAPIResponse, httpError>)
  | AddOpsPusherMsg(addOpPusherMsg)
  | @printer(opaque("SavetestAPICallback"))
  SaveTestAPICallback(Tea.Result.t<saveTestAPIResult, httpError>)
  | @printer(opaque("GetUnlockedDBsAPICallback"))
  GetUnlockedDBsAPICallback(Tea.Result.t<getUnlockedDBsAPIResult, httpError>)
  | @printer(opaque("Get404sAPICallback"))
  Get404sAPICallback(Tea.Result.t<get404sAPIResult, httpError>)
  | NewTracePush((traceID, list<TLID.t>))
  | New404Push(fourOhFour)
  | NewStaticDeployPush(staticDeploy)
  | WorkerStatePush(Map.String.t<string>)
  | @printer(opaque("Delete404APICallback"))
  Delete404APICallback(delete404APIParams, Tea.Result.t<unit, httpError>)
  | @printer(opaque("DeleteToplevelForeverAPICallback"))
  DeleteToplevelForeverAPICallback(deleteToplevelForeverAPIParams, Tea.Result.t<unit, httpError>)
  | @printer(opaque("InitialLoadAPICallback"))
  InitialLoadAPICallback(focus, modification, Tea.Result.t<initialLoadAPIResult, httpError>)
  | @printer(opaque("FetchAllTracesAPICallback"))
  FetchAllTracesAPICallback(Tea.Result.t<allTracesAPIResult, httpError>)
  | @printer(opaque("ExecuteFunctionAPICallback"))
  ExecuteFunctionAPICallback(
      executeFunctionAPIParams,
      Tea.Result.t<executeFunctionAPIResult, httpError>,
    )
  | @printer(opaque("UploadFunctionAPICallback"))
  UploadFnAPICallback(uploadFnAPIParams, Tea.Result.t<uploadFnAPIResult, httpError>)
  | @printer(opaque("TriggerHandlerAPICallback"))
  TriggerHandlerAPICallback(
      triggerHandlerAPIParams,
      Tea.Result.t<triggerHandlerAPIResult, httpError>,
    )
  | @printer(opaque("LoadPackagesAPICallback"))
  LoadPackagesAPICallback(Tea.Result.t<loadPackagesAPIResult, httpError>)
  | @printer(opaque("InsertSecretCallback"))
  InsertSecretCallback(Tea.Result.t<list<SecretTypes.t>, httpError>)
  | @printer(opaque("LogoutAPICallback")) LogoutAPICallback
  | Delete404APICall(fourOhFour)
  | NewPresencePush(list<avatar>)
  | @printer(opaque("LocationChange")) LocationChange(Web.Location.location)
  | FinishIntegrationTest
  | SaveTestButton
  | ToggleEditorSetting(editorSettings => editorSettings)
  | ExecuteFunctionButton(TLID.t, ID.t, string)
  | ExecuteFunctionFromWithin(executeFunctionAPIParams)
  | CreateHandlerFrom404(fourOhFour)
  | @printer(opaque("TimerFire")) TimerFire(timerAction, Tea.Time.t)
  | JSError(string)
  | PageVisibilityChange(PageVisibility.visibility)
  | DeleteUserFunctionParameter(TLID.t, userFunctionParameter)
  | AddUserFunctionParameter(TLID.t)
  | UploadFn(TLID.t)
  | DeleteUserTypeField(TLID.t, userRecordField)
  | BlankOrClick(TLID.t, ID.t, mouseEvent)
  | BlankOrDoubleClick(TLID.t, ID.t, mouseEvent)
  | BlankOrMouseEnter(TLID.t, ID.t, mouseEvent)
  | BlankOrMouseLeave(TLID.t, ID.t, mouseEvent)
  | MouseWheel(int, int)
  | TraceClick(TLID.t, traceID, mouseEvent)
  | TraceMouseEnter(TLID.t, traceID, mouseEvent)
  | TraceMouseLeave(TLID.t, traceID, mouseEvent)
  | TriggerHandler(TLID.t)
  | CreateRouteHandler(omniAction)
  | SidebarMsg(sidebarMsg)
  | CreateFunction
  | ExtractFunction
  | CreateType
  | DeleteUserFunction(TLID.t)
  | DeleteUserFunctionForever(TLID.t)
  | DeleteUserType(TLID.t)
  | DeleteUserTypeForever(TLID.t)
  | RestoreToplevel(TLID.t)
  | ReceiveAnalysis(performAnalysisResult)
  | ReceiveFetch(fetchResult)
  | EnablePanning(bool)
  | StartMigration(TLID.t)
  | AbandonMigration(TLID.t)
  | DeleteColInDB(TLID.t, ID.t)
  | CreateDBTable
  | ClipboardCopyEvent(clipboardEvent)
  | ClipboardCutEvent(clipboardEvent)
  | ClipboardPasteEvent(clipboardEvent)
  | ClipboardCopyLivevalue(string, vPos)
  | EventDecoderError(string, string, string)
  | CanvasPanAnimationEnd
  | GoTo(page)
  | SetHoveringReferences(TLID.t, list<ID.t>)
  | @printer(opaque("TriggerSendPresenceCallback"))
  TriggerSendPresenceCallback(Tea.Result.t<unit, httpError>)
  | TakeOffErrorRail(TLID.t, ID.t)
  | SetHandlerExeIdle(TLID.t)
  | CopyCurl(TLID.t, vPos)
  | TLMenuMsg(TLID.t, menuMsg)
  | ResetToast
  | GoToArchitecturalView
  | HideTopbar
  | LogoutOfDark
  | DismissErrorBar
  | PauseWorker(string)
  | RunWorker(string)
  | @printer(opaque("UpdateWorkerScheduleCallback"))
  UpdateWorkerScheduleCallback(Tea.Result.t<Map.String.t<string>, httpError>)
  | NewTabFromTLMenu(string, TLID.t)
  | FnParamMsg(fnpMsg)
  | ToolTipMsg(toolTipMsg)
  | UpdateHeapio(heapioTrack)
  | SettingsViewMsg(SettingsViewTypes.settingsMsg)
  | SecretMsg(SecretTypes.msg)

// -----------------------------
// AB tests
// -----------------------------
and variantTest =
  | /* does nothing variant just so we can leave this in place
   * if we're not testing anything else */
  StubVariant
  | NgrokVariant
  | LeftPartialVariant

// -----------------------------
// FeatureFlags
// -----------------------------
and ffIsExpanded = bool

and pick =
  | PickA
  | PickB

and flagsVS = Map.String.t<ffIsExpanded>

// -----------------------------
// Model
// -----------------------------
and syncState = Set.String.t

and exeState =
  | Idle
  | Executing
  | Complete

and handlerProp = {
  hoveringReferences: /* When hovering over a reference, this is the list of ID.ts that refer to
   * the reference */
  list<ID.t>,
  execution: exeState,
}

and tlTraceIDs = TLIDDict.t<traceID>

// Testing
and testResult = Result.t<unit, string>

and integrationTestState =
  | IntegrationTestExpectation(model => testResult)
  | IntegrationTestFinished(testResult)
  | NoIntegrationTest

/*
 * Fluid
 */
// eg ("value","Int")
and placeholder = {
  name: string,
  tipe: string,
}

and fluidToken =
  | TInteger(ID.t, string, option<parentBlockID>)
  | TString(ID.t, string, option<parentBlockID>)
  // multi-line strings: ID.t *, segment, start offset, full-string
  | TStringMLStart(ID.t, string, int, string)
  | TStringMLMiddle(ID.t, string, int, string)
  | TStringMLEnd(ID.t, string, int, string)
  | TBlank(ID.t, option<parentBlockID>)
  | TPlaceholder({
      blankID: ID.t,
      fnID: ID.t,
      parentBlockID: option<parentBlockID>,
      placeholder: placeholder,
    })
  | TTrue(ID.t, option<parentBlockID>)
  | TFalse(ID.t, option<parentBlockID>)
  | TNullToken(ID.t, option<parentBlockID>)
  | TFloatWhole(ID.t, string, option<parentBlockID>)
  | TFloatPoint(ID.t, option<parentBlockID>)
  | TFloatFractional(ID.t, string, option<parentBlockID>)
  /* If you're filling in an expr, but havent finished it. Not used for
   * non-expr names. */
  | TPartial(ID.t, string, option<parentBlockID>)
  // A partial that extends out to the right. Used to create binops.
  // A partial that preceeds an existing expression, used to wrap things in other things
  | TLeftPartial(ID.t, string, option<parentBlockID>)
  | TRightPartial(ID.t, string, option<parentBlockID>)
  /* When a partial used to be another thing, we want to show the name of the
   * old thing in a non-interactable way */
  | TPartialGhost(ID.t, string, option<parentBlockID>)
  // the ID.t *here disambiguates with other separators for reflow
  | TSep(ID.t, option<parentBlockID>)
  /* The first ID.t *is the ID.t *of the expression directly associated with the
   * newline. The second ID.t *is the ID.t *of that expression's parent. In an
   * expression with potentially many newlines (ie, a pipeline), the int holds
   * the relative line number (index) of this newline. */
  | TNewline(option<(ID.t, ID.t, option<int>)>)
  | TIndent(int)
  | TLetKeyword(ID.t, analysisID, option<parentBlockID>)
  // Let-expr id * rhs id * varname
  | TLetVarName(ID.t, analysisID, string, option<parentBlockID>)
  | TLetAssignment(ID.t, analysisID, option<parentBlockID>)
  | TIfKeyword(ID.t, option<parentBlockID>)
  | TIfThenKeyword(ID.t, option<parentBlockID>)
  | TIfElseKeyword(ID.t, option<parentBlockID>)
  | TBinOp(ID.t, string, option<parentBlockID>)
  | TFieldOp(/* fieldAccess */ ID.t, /* lhs */ ID.t, option<parentBlockID>)
  | TFieldName(ID.t /* fieldAccess */, ID.t /* lhs */, string, option<parentBlockID>)
  | TFieldPartial(
      /* Partial ID, fieldAccess ID, analysisID (lhs), name */ ID.t,
      ID.t,
      ID.t,
      string,
      option<parentBlockID>,
    )
  | TVariable(ID.t, string, option<parentBlockID>)
  // ID.t, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'DB::getAll'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3'), sendToRail
  | TFnName(ID.t, string, string, string, FluidExpression.sendToRail)
  // ID.t, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'v3'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3')
  | TFnVersion(ID.t, string, string, string)
  | TLambdaComma(ID.t, int, option<parentBlockID>)
  | TLambdaArrow(ID.t, option<parentBlockID>)
  | TLambdaSymbol(ID.t, option<parentBlockID>)
  | TLambdaVar(ID.t, analysisID, int, string, option<parentBlockID>)
  | TListOpen(ID.t, option<parentBlockID>)
  | TListClose(ID.t, option<parentBlockID>)
  | TListComma(ID.t, int)
  // 2nd int is the number of pipe segments there are
  | TPipe(ID.t, int, int, option<parentBlockID>)
  | TRecordOpen(ID.t, option<parentBlockID>)
  | TRecordFieldname({
      recordID: ID.t,
      exprID: ID.t,
      parentBlockID: option<parentBlockID>,
      index: int,
      fieldName: string,
    })
  | TRecordSep(ID.t, int, analysisID)
  | TRecordClose(ID.t, option<parentBlockID>)
  | TMatchKeyword(ID.t)
  | TMatchBranchArrow({matchID: ID.t, patternID: ID.t, index: int})
  /* for all these TPattern* variants:
   * - the first ID.t *is the match ID.t *
   * - the second ID.t *is the pattern ID.t *
   * - the final int is the index of the (pattern -> expr) */
  | TPatternVariable(ID.t, ID.t, string, int)
  | TPatternConstructorName(ID.t, ID.t, string, int)
  | TPatternInteger(ID.t, ID.t, string, int)
  | TPatternString({matchID: ID.t, patternID: ID.t, str: string, branchIdx: int})
  | TPatternTrue(ID.t, ID.t, int)
  | TPatternFalse(ID.t, ID.t, int)
  | TPatternNullToken(ID.t, ID.t, int)
  | TPatternFloatWhole(ID.t, ID.t, string, int)
  | TPatternFloatPoint(ID.t, ID.t, int)
  | TPatternFloatFractional(ID.t, ID.t, string, int)
  | TPatternBlank(ID.t, ID.t, int)
  | TConstructorName(ID.t, string)
  | TParenOpen(ID.t)
  | TParenClose(ID.t)
  | TFlagWhenKeyword(ID.t)
  | TFlagEnabledKeyword(ID.t)

and fluidTokenInfo = {
  startRow: int,
  startCol: int,
  startPos: int,
  endPos: int,
  length: int,
  token: fluidToken,
}

and fluidPatternAutocomplete =
  | FPAVariable(ID.t, ID.t, string)
  | FPAConstructor(ID.t, ID.t, string, list<FluidPattern.t>)
  | FPANull(ID.t, ID.t)
  | FPABool(ID.t, ID.t, bool)

and fluidAutocompleteItem =
  | FACFunction(function_)
  | FACConstructorName(string, int)
  | FACField(string)
  | FACVariable(string, option<dval>)
  | FACLiteral(literal)
  | FACKeyword(keyword)
  | FACPattern(fluidPatternAutocomplete)
  | FACCreateFunction(string, TLID.t, ID.t)

and fluidAutocompleteData = {
  item: fluidAutocompleteItem,
  validity: fluidAutocompleteValidity,
}

and fluidAutocompleteValidity =
  | FACItemValid
  | FACItemInvalidReturnType(TypeInformation.t)
  | FACItemInvalidPipedArg(tipe)

and fluidAutocompleteState = {
  // -------------------------------
  // state
  // -------------------------------
  index: option<int>,
  query: // We need to refer back to the previous one
  option<(TLID.t, fluidTokenInfo)>,
  // -------------------------------
  // Cached results
  // -------------------------------
  completions: list<fluidAutocompleteData>,
}

and fluidCommandState = {
  index: int,
  commands: list<command>,
  location: option<(TLID.t, ID.t)>,
  filter: option<string>,
}

@ppx.deriving(show({with_path: false}))
and fluidEditor =
  | NoEditor
  | MainEditor(TLID.t)
  | FeatureFlagEditor(TLID.t, ID.t)

and fluidProps = {
  functions: functionsType,
  variants: list<variantTest>,
}

and fluidState = {
  error: option<string>,
  actions: list<string>,
  oldPos: int,
  newPos: int,
  upDownCol: /* When moving up or down and going through whitespace,
   * track the column so we can go back to it */
  option<int>,
  lastInput: fluidInputEvent,
  ac: fluidAutocompleteState,
  cp: fluidCommandState,
  selectionStart: option<int> /* The selection ends at newPos */,
  midClick: /* If we get a renderCallback between a mousedown and a mouseUp, we
   * lose the information we're trying to get from the click. */
  bool,
  errorDvSrc: /* The source ID.t *of an error-dval of where the cursor is on and we might
   * have recently jumped to */
  dval_source,
  activeEditor: fluidEditor,
}

// Avatars
and avatar = {
  canvasId: string,
  canvasName: string,
  @opaque serverTime: Js.Date.t,
  tlid: option<string>,
  username: string,
  email: string,
  fullname: option<string>,
  browserId: string,
}

and avatarModelMessage = {
  browserId: string,
  tlid: option<TLID.t>,
  canvasName: string,
  timestamp: float,
}

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
  | FnBackToCanvas
  | Secrets

and model = {
  error: Error.t,
  lastMsg: msg,
  tests: list<variantTest>,
  functions: functionsType,
  complete: autocomplete,
  cursorState: cursorState,
  currentPage: page,
  hovering: list<(TLID.t, ID.t)>,
  handlers: TLIDDict.t<handler>,
  deletedHandlers: TLIDDict.t<handler>,
  dbs: TLIDDict.t<db>,
  deletedDBs: TLIDDict.t<db>,
  userFunctions: TLIDDict.t<userFunction>,
  deletedUserFunctions: TLIDDict.t<userFunction>,
  userTipes: TLIDDict.t<userTipe>,
  deletedUserTipes: TLIDDict.t<userTipe>,
  traces: traces,
  analyses: analyses,
  f404s: list<fourOhFour>,
  unlockedDBs: unlockedDBs,
  integrationTestState: // State of individual integration tests
  integrationTestState,
  visibility: PageVisibility.visibility,
  syncState: syncState,
  executingFunctions: list<(TLID.t, ID.t)>,
  tlTraceIDs: tlTraceIDs /* This is TLID ID.t *to traceID map */,
  featureFlags: flagsVS,
  canvasProps: canvasProps,
  canvasName: string,
  userContentHost: string,
  origin: string,
  environment: string,
  csrfToken: string,
  usedDBs: Map.String.t<int>,
  usedFns: Map.String.t<int>,
  usedTipes: Map.String.t<int>,
  handlerProps: TLIDDict.t<handlerProp>,
  staticDeploys: list<staticDeploy>,
  /* tlRefersTo : to answer the question "what TLs does this TL refer to". eg
   * if myFunc was called in Repl2 at id, then the dict would be:
   *
   *   { repl2.tlid { (myFunc.tlid, id) } }
   *
   * which you can read as "repl2 refersTo myfunc". So a TLID.t points to the TLs
   * it uses. */
  tlRefersTo: TLIDDict.t<list<(TLID.t, ID.t)>>,
  /* tlUsedIn: to answer the question "what TLs is this TL's name used in".  eg
   * if myFunc was called in Repl2, the dict would
   *
   *   { myfunc.tlid: { repl2.tlid }}
   *
   * which you can read as "myfunc is used in repl2". */
  tlUsedIn: TLIDDict.t<TLIDSet.t>,
  fluidState: fluidState,
  dbStats: dbStatsStore,
  workerStats: TLIDDict.t<workerStats>,
  avatarsList: list<avatar>,
  browserId: string,
  sidebarState: sidebarState,
  isAdmin: bool,
  buildHash: string,
  lastReload: option<@opaque Js.Date.t>,
  opCtrs: Map.String.t<int>,
  clientOpCtrId: string,
  permission: option<permission>,
  showTopbar: bool,
  toast: toast,
  username: string,
  account: account,
  workerSchedules: Map.String.t<string>,
  searchCache: TLIDDict.t<string>,
  editorSettings: editorSettings,
  teaDebuggerEnabled: bool,
  unsupportedBrowser: bool,
  tlMenus: TLIDDict.t<menuState>,
  firstVisitToDark: bool,
  // indicates if it is the users first time visiting any dark canvas
  tooltipState: tooltipState,
  currentUserFn: fnProps,
  settingsView: SettingsViewTypes.settingsViewState,
  firstVisitToThisCanvas: bool,
  // indicates if it is the users first time this canvas
  secrets: list<SecretTypes.t>,
  insertSecretModal: SecretTypes.insertModal,
}

and savedUserSettings = {
  firstVisitToDark: bool,
  recordConsent: option<bool>,
}

@ppx.deriving(show({with_path: false}))
and savedSettings = {
  editorSettings: editorSettings,
  cursorState: cursorState,
  tlTraceIDs: tlTraceIDs,
  featureFlags: flagsVS,
  handlerProps: TLIDDict.t<handlerProp>,
  canvasPos: pos,
  lastReload: option<@opaque Js.Date.t>,
  sidebarState: sidebarState,
  showTopbar: bool,
  firstVisitToThisCanvas: bool,
  userTutorial: option<tutorialStep>,
  userTutorialTLID: option<TLID.t>,
}

@ppx.deriving(show(eq, ord))
and permission =
  | Read
  | ReadWrite
