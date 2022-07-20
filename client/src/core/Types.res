open Tc
module PT = ProgramTypes
module RT = RuntimeTypes

open BaseTypes

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
type rec clipboardContents = /* Clipboard supports both text and encoded fluidExprs. At the moment,
 * there is always a text option - there isn't a json option if the copied
 * string wasn't a fluidExpr */
(string, @opaque option<Js.Json.t>)

@ppx.deriving(show) @opaque type rec clipboardEvent = @opaque clipboardEventDef

// -------------------
// Standard types
// -------------------

@ppx.deriving(show) type rec id = ID.t

@ppx.deriving(show) type rec analysisID = id

@ppx.deriving(show) type rec parentBlockID = id

// == end legacy aliases ==

let show_list = (~f: 'a => string, x: list<'a>): string =>
  "[" ++ (String.join(~sep=",", List.map(~f, x)) ++ "]")

let opaque = (msg, fmt, _) => {
  Format.pp_print_string(fmt, "<opaque:" ++ (msg ++ ">"))
  ()
}

@ppx.deriving(show({with_path: false}))
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

module TypeInformation = {
  @ppx.deriving(show)
  type rec t = {
    fnName: option<PT.FQFnName.t>,
    paramName: string,
    returnType: DType.t,
  }

  let default: t = {fnName: None, paramName: "Unknown", returnType: TAny}
}

// ----------------------
// Exprs and AST types
// ----------------------
type rec fnName = string

and fluidPattern = ProgramTypes.Pattern.t
and fluidExpr = ProgramTypes.Expr.t
and fluidAST = ProgramTypes.AST.t

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
  | PFnReturnTipe(blankOr<DType.t>)
  | PParamName(blankOr<string>)
  | PParamTipe(blankOr<DType.t>)
  | PTypeName(blankOr<string>)
  | PTypeFieldName(blankOr<string>)
  | PTypeFieldTipe(blankOr<DType.t>)

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

// usedIn is a TL that's refered to in the refersTo tl at id
// refersTo is a TL that uses the usedIn tl at id
and usage = {
  usedIn: TLID.t,
  refersTo: TLID.t,
  id: id,
}

and handlerSpace =
  | HSHTTP
  | HSCron
  | HSWorker
  | HSRepl
  | HSDeprecatedOther

and functionTypes =
  | UserFunction(PT.UserFunction.t)
  | PackageFn(PT.Package.Fn.t)

// toplevels
and toplevel =
  | TLHandler(PT.Handler.t)
  | TLDB(PT.DB.t)
  | TLPmFunc(PT.Package.Fn.t)
  | TLFunc(PT.UserFunction.t)
  | TLTipe(PT.UserType.t)

and packageFns = TLID.Dict.t<PT.Package.Fn.t>

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
  | SourceId(TLID.t, id)

and dblock_args = {
  /* We use Belt.Map.String as Map.String.t has a comparator that doesn't work
   with the cloning algorithm of web workers */
  symtable: Belt.Map.String.t<dval>,
  params: list<(id, string)>,
  body: fluidExpr,
}

@ppx.deriving(show({with_path: false}))
and dval =
  | DInt(int64)
  | DFloat(float)
  | DBool(bool)
  | DNull
  | DCharacter(string)
  | DStr(string)
  | DList(array<dval>)
  | DTuple(dval, dval, list<dval>)
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
// Scroll
// -----------------------------
@ppx.deriving(show({with_path: false})) type rec scrollEvent = {timeStamp: float}

// -----------------------------
// Mouse
// -----------------------------
and isLeftButton = bool

// -----------------------------
// CursorState
// -----------------------------

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

and intermediateResultStore = ID.Map.t<executionResult>

// map from expression ids to symbol table, which maps from varname strings to
// the ids of the expressions that represent their values
and avDict = ID.Map.t<Map.String.t<id>>

and inputValueDict = Belt.Map.String.t<dval>

and analysisStore = loadable<intermediateResultStore>

and analyses = // indexed by traceID
Map.String.t<
  analysisStore,
>

and functionResult = {
  fnName: string,
  callerID: id,
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

// Somehow we allowed SetHover and ClearHover to use both traceIDs and regular IDs,
// and it looks like different parts of the app rely on both. Since changing the
// types of ID to no longer be a string, we can't support just hack it in anymore, so
// this wraps around it.
and idOrTraceID =
  | AnID(ID.t)
  | ATraceID(traceID)

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

and traces = TLID.Dict.t<list<trace>>

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
// APIs
// -------------------
// params
and avatarModelMessage = {
  browserId: string,
  tlid: option<TLID.t>,
  canvasName: string,
  timestamp: float,
}

and sendPresenceParams = avatarModelMessage

and sendInviteParams = SettingsViewTypes.inviteFormMessage

and executeFunctionAPIParams = {
  efpTLID: TLID.t,
  efpTraceID: traceID,
  efpCallerID: id,
  efpArgs: list<dval>,
  efpFnName: string,
}

and deleteToplevelForeverAPIParams = {dtfTLID: TLID.t}

and uploadFnAPIParams = {uplFn: PT.UserFunction.t}

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
  handler: PT.Handler.t,
  traceID: traceID,
  traceData: traceData,
  dbs: list<PT.DB.t>,
  userFns: list<PT.UserFunction.t>,
  userTipes: list<PT.UserType.t>,
  secrets: list<SecretTypes.t>,
}

and performFunctionAnalysisParams = {
  func: PT.UserFunction.t,
  traceID: traceID,
  traceData: traceData,
  dbs: list<PT.DB.t>,
  userFns: list<PT.UserFunction.t>,
  userTipes: list<PT.UserType.t>,
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

and dvalArgsHash = string

and executeFunctionAPIResult = (dval, dvalArgsHash, int, list<TLID.t>, unlockedDBs)

and uploadFnAPIResult = unit

and loadPackagesAPIResult = list<PT.Package.Fn.t>

and triggerHandlerAPIResult = list<TLID.t>

and unlockedDBs = TLID.Set.t

and getUnlockedDBsAPIResult = unlockedDBs

and get404sAPIResult = list<fourOhFour>

and getTraceDataAPIResult = {trace: trace}

and dbStatsAPIResult = dbStatsStore

and workerStatsAPIResult = workerStats

and allTracesAPIResult = {traces: list<(TLID.t, traceID)>}

and saveTestAPIResult = string

// -------------------
// Autocomplete / entry
// -------------------
// functions
and parameter = {
  paramName: string,
  paramTipe: DType.t,
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
  fnName: PT.FQFnName.t,
  fnParameters: list<parameter>,
  fnDescription: string,
  fnReturnTipe: DType.t,
  fnPreviewSafety: previewSafety,
  fnDeprecated: bool,
  fnInfix: bool,
  fnIsSupportedInQuery: bool,
  fnOrigin: /* This is a client-side only field to be able to give different UX to
   * different functions */
  fnOrigin,
}

// autocomplete items

// -------------------
// Functions.res
// -------------------
and functionsType = {
  builtinFunctions: list<RT.BuiltInFn.t>,
  packageFunctions: packageFns,
  // We do analysis to determine which functions are safe and which are not.
  // This stores the result
  previewUnsafeFunctions: Set.String.t,
  allowedFunctions: list<function_>,
}

and functionsProps = {
  usedFns: Map.String.t<int>,
  userFunctions: TLID.Dict.t<PT.UserFunction.t>,
}

// ---------------
// Component Types
// ---------------

// -------------------
// Modifications
// -------------------

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

// -------------------
// Msgs
// -------------------
and heapioTrack =
  | WelcomeModal
  | OpenDocs
  | InviteUser
  | OpenFnRef
  | OpenKeyboardRef

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
// Model
// -----------------------------
and tlTraceIDs = TLID.Dict.t<traceID>

/*
 * Fluid
 */
and fluidProps = {
  functions: functionsType,
  variants: list<variantTest>,
}
