open Prelude
open ProgramTypes
open ProgramTypes.Expr

// 'Shortcuts' to concisely create parts of Dark programs (Exprs, Patterns)

let blank = (~id=gid(), ()): t => EBlank(id)

let str = (~id=gid(), str: string): t => EString(id, str)

let int = (~id=gid(), int: int): t => EInteger(id, Int64.of_int(int))

let int64 = (~id=gid(), int: int64): t => EInteger(id, int)

let bool = (~id=gid(), b: bool): t => EBool(id, b)

let float' = (~id=gid(), sign: Sign.t, whole: int, fraction: int): t => EFloat(
  id,
  sign,
  string_of_int(whole),
  string_of_int(fraction),
)

let floatStr = (~id=gid(), sign: Sign.t, whole: string, fraction: string): t => EFloat(
  id,
  sign,
  whole,
  fraction,
)

let null: t = ENull(gid())

let record = (~id=gid(), rows: list<(string, t)>): t => ERecord(
  id,
  List.map(rows, ~f=((k, v)) => (k, v)),
)

let list = (~id=gid(), elems: list<t>): t => EList(id, elems)

let tuple = (~id=gid(), first, second, theRest: list<t>): t => ETuple(id, first, second, theRest)

let pipeTarget = EPipeTarget(gid())

let fn = (
  ~id=gid(),
  ~ster=SendToRail.NoRail,
  ~mod="",
  function: string,
  ~version=0,
  args: list<t>,
) => EFnCall(id, Stdlib({module_: mod, function: function, version: version}), args, ster)

let binop = (~id=gid(), ~ster=SendToRail.NoRail, function: string, arg0: t, arg1: t) => EBinOp(
  id,
  {module_: None, function: function},
  arg0,
  arg1,
  ster,
)

let partial = (~id=gid(), str: string, e: t): t => EPartial(id, str, e)

let rightPartial = (~id=gid(), str: string, e: t): t => ERightPartial(id, str, e)

let leftPartial = (~id=gid(), str: string, e: t): t => ELeftPartial(id, str, e)

let var = (~id=gid(), name: string): t => EVariable(id, name)

let fieldAccess = (~id=gid(), expr: t, fieldName: string): t => EFieldAccess(id, expr, fieldName)

let if' = (~id=gid(), cond: t, then': t, else': t): t => EIf(id, cond, then', else')

let let' = (~id=gid(), varName: string, rhs: t, body: t): t => ELet(id, varName, rhs, body)

let lambda = (~id=gid(), varNames: list<string>, body: t): t => ELambda(
  id,
  List.map(varNames, ~f=name => (gid(), name)),
  body,
)

let pipe = (~id=gid(), first: t, second: t, rest: list<t>): t => EPipe(id, first, second, rest)

let constructor = (~id=gid(), name: string, args: list<t>): t => EConstructor(id, name, args)

let just = (~id=gid(), arg: t): t => EConstructor(id, "Just", list{arg})

let nothing = (~id=gid(), ()): t => EConstructor(id, "Nothing", list{})

let error = (~id=gid(), arg: t): t => EConstructor(id, "Error", list{arg})

let ok = (~id=gid(), arg: t): t => EConstructor(id, "Ok", list{arg})

let match' = (~id=gid(), cond: t, matches: list<(FluidPattern.t, t)>): t => EMatch(
  id,
  cond,
  matches,
)

let pInt = (~id=gid(), int: int): FluidPattern.t => PInteger(id, Int64.of_int(int))

let pInt64 = (~id=gid(), int: int64): FluidPattern.t => PInteger(id, int)

let pVar = (~id=gid(), name: string): FluidPattern.t => PVariable(id, name)

let pConstructor = (
  ~id=gid(),
  name: string,
  patterns: list<FluidPattern.t>,
): FluidPattern.t => PConstructor(id, name, patterns)

let pJust = (~id=gid(), arg: FluidPattern.t): FluidPattern.t => PConstructor(id, "Just", list{arg})

let pNothing = (~id=gid(), ()): FluidPattern.t => PConstructor(id, "Nothing", list{})

let pError = (~id=gid(), arg: FluidPattern.t): FluidPattern.t => PConstructor(
  id,
  "Error",
  list{arg},
)

let pOk = (~id=gid(), arg: FluidPattern.t): FluidPattern.t => PConstructor(id, "Ok", list{arg})

let pBool = (~id=gid(), b: bool): FluidPattern.t => PBool(id, b)

let pString = (~id=gid(), str: string): FluidPattern.t => PString(id, str)

let pFloatStr = (~id=gid(), sign: Sign.t, whole: string, fraction: string): FluidPattern.t => {
  assert (int_of_string(whole) > 0)
  assert (int_of_string(fraction) > 0)
  PFloat(id, sign, whole, fraction)
}

let pFloat = (~id=gid(), sign, whole: int, fraction: int): FluidPattern.t => {
  assert (whole > 0)
  assert (fraction > 0)
  PFloat(id, sign, string_of_int(whole), string_of_int(fraction))
}

let pNull = (~id=gid(), ()): FluidPattern.t => PNull(id)

let pBlank = (~id=gid(), ()): FluidPattern.t => PBlank(id)

let flag = (~id=gid(), ~name="flag-1", cond, oldCode, newCode) => EFeatureFlag(
  id,
  name,
  cond,
  oldCode,
  newCode,
)
