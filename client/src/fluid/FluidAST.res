module E = FluidExpression
module P = FluidPattern

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

let find = (target: id, ast: t): option<E.t> => toExpr(ast) |> E.find(target)

let findParent = (target: id, ast: t): option<E.t> => toExpr(ast) |> E.findParent(target)

let ancestors = (target: id, ast: t): list<E.t> => toExpr(ast) |> E.ancestors(target)

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

let clone = map(~f=E.clone)

let updatePattern = (~f: fluidPattern => fluidPattern, matchID: id, patID: id, ast: t): t =>
  update(matchID, ast, ~f=m =>
    switch m {
    | EMatch(matchID, expr, pairs) =>
      let rec run = p =>
        if patID == P.toID(p) {
          f(p)
        } else {
          P.recurseDeprecated(~f=run, p)
        }

      let newPairs = List.map(pairs, ~f=((pat, expr)) => (run(pat), expr))

      EMatch(matchID, expr, newPairs)
    | _ => m
    }
  )

let replacePattern = (~newPat: fluidPattern, matchID: id, patID: id, ast: t): t =>
  updatePattern(matchID, patID, ast, ~f=_ => newPat)
