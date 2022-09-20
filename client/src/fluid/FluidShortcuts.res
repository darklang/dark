open Prelude
open ProgramTypes
open ProgramTypes.Expr

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

let match' = (~id=gid(), cond: t, matches: list<(FluidMatchPattern.t, t)>): t => EMatch(
  id,
  cond,
  matches,
)

let mpInt = (~id=gid(), int: int): FluidMatchPattern.t => MPInteger(id, Int64.of_int(int))

let mpInt64 = (~id=gid(), int: int64): FluidMatchPattern.t => MPInteger(id, int)

let mpVar = (~id=gid(), name: string): FluidMatchPattern.t => MPVariable(id, name)

let mpConstructor = (
  ~id=gid(),
  name: string,
  args: list<FluidMatchPattern.t>,
): FluidMatchPattern.t => MPConstructor(id, name, args)

let mpJust = (~id=gid(), arg: FluidMatchPattern.t): FluidMatchPattern.t => MPConstructor(
  id,
  "Just",
  list{arg},
)

let mpNothing = (~id=gid(), ()): FluidMatchPattern.t => MPConstructor(id, "Nothing", list{})

let mpError = (~id=gid(), arg: FluidMatchPattern.t): FluidMatchPattern.t => MPConstructor(
  id,
  "Error",
  list{arg},
)

let mpOk = (~id=gid(), arg: FluidMatchPattern.t): FluidMatchPattern.t => MPConstructor(
  id,
  "Ok",
  list{arg},
)

let mpBool = (~id=gid(), b: bool): FluidMatchPattern.t => MPBool(id, b)

let mpString = (~id=gid(), str: string): FluidMatchPattern.t => MPString(id, str)

let mpFloatStr = (
  ~id=gid(),
  sign: Sign.t,
  whole: string,
  fraction: string,
): FluidMatchPattern.t => {
  assert (int_of_string(whole) > 0)
  assert (int_of_string(fraction) > 0)
  MPFloat(id, sign, whole, fraction)
}

let mpFloat = (~id=gid(), sign, whole: int, fraction: int): FluidMatchPattern.t => {
  assert (whole > 0)
  assert (fraction > 0)
  MPFloat(id, sign, string_of_int(whole), string_of_int(fraction))
}

let mpNull = (~id=gid(), ()): FluidMatchPattern.t => MPNull(id)

let mpBlank = (~id=gid(), ()): FluidMatchPattern.t => MPBlank(id)

let mpTuple = (
  ~id=gid(),
  first: FluidMatchPattern.t,
  second: FluidMatchPattern.t,
  theRest: list<FluidMatchPattern.t>,
): FluidMatchPattern.t => MPTuple(id, first, second, theRest)

let flag = (~id=gid(), ~name="flag-1", cond, oldCode, newCode) => EFeatureFlag(
  id,
  name,
  cond,
  oldCode,
  newCode,
)
