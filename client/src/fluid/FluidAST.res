module E = FluidExpression
module MP = FluidMatchPattern

open Prelude

open ProgramTypes.AST

type id = Prelude.id

type t = ProgramTypes.AST.t

let show = (Root(e)) => E.show(e)

let pp = (f, Root(e)) => E.pp(f, e)

let toExpr = (Root(e)) => e

let ofExpr = e => Root(e)

let toID = (Root(e)) => E.toID(e)

let ids = (ast: t): list<id> => toExpr(ast) |> E.ids

let filter = (ast: t, ~f: E.t => bool): list<E.t> => toExpr(ast) |> E.filter(~f)

let findExpr = (target: id, ast: t): option<E.t> => toExpr(ast) |> E.findExpr(target)

let findExprParent = (target: id, ast: t): option<E.t> => toExpr(ast) |> E.findParent(target)

let findMP = (target: id, ast: t): option<MP.t> => toExpr(ast) |> E.findMP(target)

let exprAncestors = (target: id, ast: t): list<E.t> => toExpr(ast) |> E.ancestors(target)

let blanks = (ast: t): list<E.t> => toExpr(ast) |> E.blanks

let getFeatureFlags = (ast: t): list<E.t> =>
  filter(ast, ~f=x =>
    switch x {
    | EFeatureFlag(_) => true
    | _ => false
    }
  )

let testEqualIgnoringIds = (a: t, b: t): bool => E.testEqualIgnoringIds(toExpr(a), toExpr(b))

let map = (~f: E.t => E.t, ast: t): t => toExpr(ast) |> f |> ofExpr

let replace = (~replacement: E.t, target: id, ast: t): t =>
  map(ast, ~f=E.replace(~replacement, target))

let update = (~failIfMissing=true, ~f: E.t => E.t, target: id, ast: t): t =>
  map(ast, ~f=E.update(~failIfMissing, ~f, target))

let validateAndFix = (~onError: (string, E.t) => unit, ast: t): t =>
  toExpr(ast) |> E.validateAndFix(~onError) |> ofExpr

let clone = map(~f=E.clone)

let updateMatchPattern = (
  ~f: fluidMatchPattern => fluidMatchPattern,
  matchID: id,
  patID: id,
  ast: t,
): t =>
  update(matchID, ast, ~f=m =>
    switch m {
    | EMatch(matchID, expr, pairs) =>
      let rec run = p =>
        if patID == MP.toID(p) {
          f(p)
        } else {
          MP.recurseDeprecated(~f=run, p)
        }

      let newPairs = List.map(pairs, ~f=((pat, expr)) => (run(pat), expr))

      EMatch(matchID, expr, newPairs)
    | _ => m
    }
  )

let replaceMatchPattern = (~newPat: fluidMatchPattern, matchID: id, patID: id, ast: t): t =>
  updateMatchPattern(matchID, patID, ast, ~f=_ => newPat)
