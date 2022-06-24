open Prelude
module T = FluidToken
module E = FluidExpression
module Pattern = FluidPattern
module Util = FluidUtil
open FluidTokenizer

type token = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

let tokensToString = (tis: list<tokenInfo>): string =>
  tis |> List.map(~f=ti => T.toText(ti.token)) |> String.join(~sep="")

let eToTestString = (e: E.t): string =>
  e |> tokenize |> List.map(~f=ti => T.toTestText(ti.token)) |> String.join(~sep="")

let eToHumanString = (e: E.t): string => e |> tokenize |> tokensToString

let eToStructure = (~includeIDs=false, e: E.t): string =>
  e
  |> tokenize
  |> List.map(~f=ti =>
    "<" ++
    (T.toTypeName(ti.token) ++
    (if includeIDs {
      "(" ++ ((T.tid(ti.token) |> ID.toString) ++ ")")
    } else {
      ""
    } ++
    (":" ++
    (T.toText(ti.token) ++ ">"))))
  )
  |> String.join(~sep="")

let pToString = (p: fluidPattern): string =>
  p
  |> FluidTokenizer.patternToToken(~idx=0)
  |> List.map(~f=t => T.toTestText(t))
  |> String.join(~sep="")

let pToStructure = (p: fluidPattern): string =>
  p
  |> FluidTokenizer.patternToToken(~idx=0)
  |> List.map(~f=t => "<" ++ (T.toTypeName(t) ++ (":" ++ (T.toText(t) ++ ">"))))
  |> String.join(~sep="")

// -----------------
// Test cases
// -----------------
/* eToTestcase constructs testcases that we can enter in our
 * test suite. They are similar to `show` except that instead of the full code,
 * they use the shortcuts from FluidTestData. */
// -----------------

let rec eToTestcase = (e: E.t): string => {
  let r = eToTestcase
  let quoted = str => "\"" ++ (str ++ "\"")
  let listed = elems => "[" ++ (String.join(~sep=";", elems) ++ "]")
  let spaced = elems => String.join(~sep=" ", elems)
  let result = switch e {
  | EBlank(_) => "b"
  | EString(_, str) => spaced(list{"str", quoted(str)})
  | EBool(_, true) => spaced(list{"bool true"})
  | EBool(_, false) => spaced(list{"bool false"})
  | EFloat(_, whole, fractional) => spaced(list{"float'", whole, fractional})
  | EInteger(_, int) => spaced(list{"int", int})
  | ENull(_) => "null"
  | EPipeTarget(_) => "pipeTarget"
  | EPartial(_, str, e) => spaced(list{"partial", quoted(str), r(e)})
  | ERightPartial(_, str, e) => spaced(list{"rightPartial", quoted(str), r(e)})
  | ELeftPartial(_, str, e) => spaced(list{"prefixPartial", quoted(str), r(e)})
  | EFnCall(_, name, exprs, _) => spaced(list{"fn", quoted(name), listed(List.map(~f=r, exprs))})
  | EBinOp(_, name, lhs, rhs, _) => spaced(list{"binop", quoted(name), r(lhs), r(rhs)})
  | EVariable(_, name) => spaced(list{"var", quoted(name)})
  | EFieldAccess(_, expr, fieldname) => spaced(list{"fieldAccess", r(expr), quoted(fieldname)})
  | EMatch(_, cond, matches) =>
    let rec pToTestcase = (p: FluidPattern.t): string => {
      let quoted = str => "\"" ++ (str ++ "\"")
      let listed = elems => "[" ++ (String.join(~sep=";", elems) ++ "]")
      let spaced = elems => String.join(~sep=" ", elems)
      switch p {
      | FPBlank(_) => "pBlank"
      | FPString({str, _}) => spaced(list{"pString", quoted(str)})
      | FPBool(_, _, true) => spaced(list{"pBool true"})
      | FPBool(_, _, false) => spaced(list{"pBool false"})
      | FPFloat(_, _, whole, fractional) => spaced(list{"pFloat'", whole, fractional})
      | FPInteger(_, _, int) => spaced(list{"pInt", int})
      | FPNull(_) => "pNull"
      | FPVariable(_, _, name) => spaced(list{"pVar", quoted(name)})
      | FPConstructor(_, _, name, args) =>
        spaced(list{"pConstructor", quoted(name), listed(List.map(args, ~f=pToTestcase))})
      }
    }

    spaced(list{
      "match'",
      r(cond),
      listed(List.map(matches, ~f=((p, e)) => "(" ++ (pToTestcase(p) ++ (", " ++ (r(e) ++ ")"))))),
    })
  | ERecord(_, pairs) =>
    spaced(list{
      "record",
      listed(List.map(pairs, ~f=((k, v)) => "(" ++ (quoted(k) ++ (", " ++ (r(v) ++ ")"))))),
    })
  | EList(_, exprs) => spaced(list{"list", listed(List.map(~f=r, exprs))})
  | EPipe(_, list{a, ...rest}) => spaced(list{"pipe", r(a), listed(List.map(~f=r, rest))})
  | EPipe(_, list{}) => "INVALID PIPE - NO ELEMENTS"
  | EConstructor(_, name, exprs) =>
    spaced(list{"constructor", quoted(name), listed(List.map(exprs, ~f=r))})
  | EIf(_, cond, thenExpr, elseExpr) => spaced(list{"if'", r(cond), r(thenExpr), r(elseExpr)})
  | ELet(_, lhs, rhs, body) => spaced(list{"let'", quoted(lhs), r(rhs), r(body)})
  | ELambda(_, names, body) =>
    let names = List.map(names, ~f=((_, name)) => quoted(name)) |> listed

    spaced(list{"lambda", names, r(body)})
  | EFeatureFlag(_, _, cond, oldCode, newCode) =>
    spaced(list{"ff", r(cond), r(oldCode), r(newCode)})
  }

  "(" ++ (result ++ ")")
}
