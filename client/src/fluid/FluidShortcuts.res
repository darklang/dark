open Prelude
open ProgramTypes
open ProgramTypes.Expr

let blank = (~id=gid(), ()): t => EBlank(id)

let str = (~id=gid(), str: string): t => EString(id, str)

let int = (~id=gid(), int: int): t => EInteger(id, string_of_int(int))

let intStr = (~id=gid(), int: string): t => EInteger(id, int)

let bool = (~id=gid(), b: bool): t => EBool(id, b)

let float' = (~id=gid(), sign: sign, whole: int, fraction: int): t => EFloat(
  id,
  sign,
  string_of_int(whole),
  string_of_int(fraction),
)

let floatStr = (~id=gid(), sign: sign, whole: string, fraction: string): t => EFloat(
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

let pipeTarget = EPipeTarget(gid())

let fn = (~id=gid(), ~ster=NoRail, name: string, args: list<t>) => EFnCall(id, name, args, ster)

let binop = (~id=gid(), ~ster=NoRail, name: string, arg0: t, arg1: t) => EBinOp(
  id,
  name,
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

let pipe = (~id=gid(), first: t, rest: list<t>): t => EPipe(id, list{first, ...rest})

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

let pInt = (~mid=gid(), ~id=gid(), int: int): FluidPattern.t => PInteger(
  mid,
  id,
  string_of_int(int),
)

let pIntStr = (~mid=gid(), ~id=gid(), int: string): FluidPattern.t => PInteger(mid, id, int)

let pVar = (~mid=gid(), ~id=gid(), name: string): FluidPattern.t => PVariable(mid, id, name)

let pConstructor = (
  ~mid=gid(),
  ~id=gid(),
  name: string,
  patterns: list<FluidPattern.t>,
): FluidPattern.t => PConstructor(mid, id, name, patterns)

let pJust = (~mid=gid(), ~id=gid(), arg: FluidPattern.t): FluidPattern.t => PConstructor(
  mid,
  id,
  "Just",
  list{arg},
)

let pNothing = (~mid=gid(), ~id=gid(), ()): FluidPattern.t => PConstructor(
  mid,
  id,
  "Nothing",
  list{},
)

let pError = (~mid=gid(), ~id=gid(), arg: FluidPattern.t): FluidPattern.t => PConstructor(
  mid,
  id,
  "Error",
  list{arg},
)

let pOk = (~mid=gid(), ~id=gid(), arg: FluidPattern.t): FluidPattern.t => PConstructor(
  mid,
  id,
  "Ok",
  list{arg},
)

let pBool = (~mid=gid(), ~id=gid(), b: bool): FluidPattern.t => PBool(mid, id, b)

let pString = (~mid=gid(), ~id=gid(), str: string): FluidPattern.t => PString({
  matchID: mid,
  patternID: id,
  str: str,
})

let pFloatStr = (
  ~mid=gid(),
  ~id=gid(),
  sign: sign,
  whole: string,
  fraction: string,
): FluidPattern.t => {
  assert (int_of_string(whole) > 0)
  assert (int_of_string(fraction) > 0)
  PFloat(mid, id, sign, whole, fraction)
}

let pFloat = (~mid=gid(), ~id=gid(), sign, whole: int, fraction: int): FluidPattern.t => {
  assert (whole > 0)
  assert (fraction > 0)
  PFloat(mid, id, sign, string_of_int(whole), string_of_int(fraction))
}

let pNull = (~mid=gid(), ~id=gid(), ()): FluidPattern.t => PNull(mid, id)

let pBlank = (~mid=gid(), ~id=gid(), ()): FluidPattern.t => PBlank(mid, id)

let flag = (~id=gid(), ~name="flag-1", cond, oldCode, newCode) => EFeatureFlag(
  id,
  name,
  cond,
  oldCode,
  newCode,
)
