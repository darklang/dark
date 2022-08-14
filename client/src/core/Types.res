open Tc
module PT = ProgramTypes
module RT = RuntimeTypes

// hmmm: are these 'client-only types'?
// (versus the 'shared' or 'interop' types)
// I wonder if it'd be worth creating an `interop` folder and a `???` folder to
// make these 2 worlds more clear. Or maybe renaming this file is sufficient.

// hmmm: maybe a few fewer 'and's in this file, though it's nbd currently

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

  let default: t = {fnName: None, paramName: "Unknown", returnType: DType.any}
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
  | PEventName(BlankOr.t<string>)
  | PEventModifier(BlankOr.t<string>)
  | PEventSpace(BlankOr.t<string>)
  | PDBName(BlankOr.t<string>)
  | PDBColName(BlankOr.t<string>)
  | PDBColType(BlankOr.t<DType.t>)
  | PFnName(BlankOr.t<string>)
  | PFnReturnTipe(BlankOr.t<DType.t>) // CLEANUP rename
  | PParamName(BlankOr.t<string>)
  | PParamTipe(BlankOr.t<DType.t>)
  | PTypeName(BlankOr.t<string>)
  | PTypeFieldName(BlankOr.t<string>)
  | PTypeFieldTipe(BlankOr.t<DType.t>) // CLEANUP rename

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

// -----------------------------
// Mouse
// -----------------------------
and isLeftButton = bool

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

and traceID = string

// Somehow we allowed SetHover and ClearHover to use both traceIDs and regular IDs,
// and it looks like different parts of the app rely on both. Since changing the
// types of ID to no longer be a string, we can't support just hack it in anymore, so
// this wraps around it.
and idOrTraceID =
  | AnID(ID.t)
  | ATraceID(traceID)

// -------------------
// APIs
// -------------------
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
