open Prelude
module T = FluidToken
module E = FluidExpression
module MatchPattern = FluidMatchPattern
module Util = FluidUtil
open FluidTokenizer

type token = FluidTypes.Token.t

type tokenInfo = FluidTypes.TokenInfo.t

let tokensToString = (tis: list<tokenInfo>): string =>
  tis |> List.map(~f=(ti: tokenInfo) => T.toText(ti.token)) |> String.join(~sep="")

let eToTestString = (e: E.t): string =>
  e
  |> tokenizeExpr
  |> List.map(~f=(ti: tokenInfo) => T.toTestText(ti.token))
  |> String.join(~sep="")

let eToHumanString = (e: E.t): string => e |> tokenizeExpr |> tokensToString

let eToStructure = (~includeIDs=false, e: E.t): string =>
  e
  |> tokenizeExpr
  |> List.map(~f=(ti: tokenInfo) =>
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

let mpToString = (mp: fluidMatchPattern): string =>
  mp
  |> FluidTokenizer.matchPatternToTokens(ID.fromInt(0), ~idx=0)
  |> List.map(~f=t => T.toTestText(t))
  |> String.join(~sep="")

let mpToStructure = (mp: fluidMatchPattern): string =>
  mp
  |> FluidTokenizer.matchPatternToTokens(ID.fromInt(0), ~idx=0)
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
  | ECharacter(_, str) => spaced(list{"str", quoted(str)})
  | EBool(_, true) => spaced(list{"bool true"})
  | EBool(_, false) => spaced(list{"bool false"})
  | EFloat(_, sign, whole, fractional) =>
    spaced(list{"float'", ProgramTypes.Sign.toString(sign), whole, fractional})
  | EInteger(_, int) => spaced(list{"int", Int64.to_string(int)})
  | ENull(_) => "null"
  | EPipeTarget(_) => "pipeTarget"
  | EPartial(_, str, e) => spaced(list{"partial", quoted(str), r(e)})
  | ERightPartial(_, str, e) => spaced(list{"rightPartial", quoted(str), r(e)})
  | ELeftPartial(_, str, e) => spaced(list{"prefixPartial", quoted(str), r(e)})
  | EFnCall(_, name, exprs, _) =>
    spaced(list{"fn", quoted(FQFnName.toString(name)), listed(List.map(~f=r, exprs))})
  | EInfix(_, BinOp(BinOpAnd), lhs, rhs) => spaced(list{"and'", r(lhs), r(rhs)})
  | EInfix(_, BinOp(BinOpOr), lhs, rhs) => spaced(list{"or'", r(lhs), r(rhs)})
  | EInfix(_, InfixFnCall(name, _), lhs, rhs) =>
    spaced(list{"infixFnCall", quoted(PT.InfixStdlibFnName.toString(name)), r(lhs), r(rhs)})
  | EVariable(_, name) => spaced(list{"var", quoted(name)})
  | EFieldAccess(_, expr, fieldname) => spaced(list{"fieldAccess", r(expr), quoted(fieldname)})
  | EMatch(_, cond, matches) =>
    let rec mpToTestcase = (mp: FluidMatchPattern.t): string => {
      let quoted = str => "\"" ++ (str ++ "\"")
      let listed = elems => "[" ++ (String.join(~sep=";", elems) ++ "]")
      let spaced = elems => String.join(~sep=" ", elems)
      switch mp {
      | MPBlank(_) => "mpBlank"
      | MPString(_, str) => spaced(list{"mpString", quoted(str)})
      | MPCharacter(_, str) => spaced(list{"mpChar", quoted(str)})
      | MPBool(_, true) => spaced(list{"mpBool true"})
      | MPBool(_, false) => spaced(list{"mpBool false"})
      | MPFloat(_, sign, whole, fractional) =>
        spaced(list{"mpFloat'", ProgramTypes.Sign.toString(sign), whole, fractional})
      | MPInteger(_, int) => spaced(list{"mpInt", Int64.to_string(int)})
      | MPNull(_) => "mpNull"
      | MPVariable(_, name) => spaced(list{"mpVar", quoted(name)})
      | MPConstructor(_, name, args) =>
        spaced(list{"mpConstructor", quoted(name), listed(List.map(args, ~f=mpToTestcase))})
      | MPTuple(_, first, second, theRest) =>
        let exprs = list{first, second, ...theRest} |> List.map(~f=mpToTestcase)
        spaced(list{"mpTuple", "(" ++ (String.join(~sep=",", exprs) ++ ")")})
      }
    }

    spaced(list{
      "match'",
      r(cond),
      listed(List.map(matches, ~f=((p, e)) => "(" ++ (mpToTestcase(p) ++ (", " ++ (r(e) ++ ")"))))),
    })
  | ERecord(_, pairs) =>
    spaced(list{
      "record",
      listed(List.map(pairs, ~f=((k, v)) => "(" ++ (quoted(k) ++ (", " ++ (r(v) ++ ")"))))),
    })
  | EList(_, exprs) => spaced(list{"list", listed(List.map(~f=r, exprs))})
  | ETuple(_, first, second, theRest) =>
    let exprs = list{first, second, ...theRest}
    spaced(list{"tuple", listed(List.map(~f=r, exprs))})
  | EPipe(_, e1, e2, rest) => spaced(list{"pipe", r(e1), r(e2), listed(List.map(~f=r, rest))})
  | EConstructor(_, name, exprs) =>
    spaced(list{"constructor", quoted(name), listed(List.map(exprs, ~f=r))})
  | EIf(_, cond, thenExpr, elseExpr) => spaced(list{"if'", r(cond), r(thenExpr), r(elseExpr)})
  | ELet(_, pat, rhs, body) =>
    let lpToTestcase = (mp: FluidLetPattern.t): string => {
      let quoted = str => "\"" ++ (str ++ "\"")
      let spaced = elems => String.join(~sep=" ", elems)
      switch mp {
      | LPVariable(_, name) => spaced(list{"lpVar", quoted(name)})
      }
    }
    spaced(list{"let'", quoted(lpToTestcase(pat)), r(rhs), r(body)})
  | ELambda(_, names, body) =>
    let names = List.map(names, ~f=((_, name)) => quoted(name)) |> listed

    spaced(list{"lambda", names, r(body)})
  | EFeatureFlag(_, _, cond, oldCode, newCode) =>
    spaced(list{"ff", r(cond), r(oldCode), r(newCode)})
  }

  "(" ++ (result ++ ")")
}
