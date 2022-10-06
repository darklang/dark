open Tc

module PT = ProgramTypes
module RT = RuntimeTypes

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
  exceptionType: string,
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
    fnName: option<FQFnName.t>,
    paramName: string,
    returnType: DType.t,
  }

  let default: t = {fnName: None, paramName: "Unknown", returnType: DType.any}
}

// ----------------------
// Exprs and AST types
// ----------------------
type rec fnName = string

and fluidMatchPattern = ProgramTypes.MatchPattern.t
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
  | PFnReturnType(BlankOr.t<DType.t>) // CLEANUP rename
  | PParamName(BlankOr.t<string>)
  | PParamType(BlankOr.t<DType.t>)
  | PTypeName(BlankOr.t<string>)
  | PTypeFieldName(BlankOr.t<string>)
  | PTypeFieldType(BlankOr.t<DType.t>) // CLEANUP rename

@ppx.deriving(show({with_path: false}))
and blankOrType =
  | EventName
  | EventSpace
  | EventModifier
  | DBName
  | DBColName
  | DBColType
  | FnName
  | FnReturnType
  | ParamName
  | ParamType
  | TypeName
  | TypeFieldName
  | TypeFieldType

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
  | TLType(PT.UserType.t)

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

and traceID = string

// Somehow we allowed SetHover and ClearHover to use both traceIDs and regular IDs,
// and it looks like different parts of the app rely on both. Since changing the
// types of ID to no longer be a string, we can't support just hack it in anymore, so
// this wraps around it.
and idOrTraceID =
  | AnID(ID.t)
  | ATraceID(traceID)

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
// Model
// -----------------------------
and tlTraceIDs = TLID.Dict.t<traceID>
