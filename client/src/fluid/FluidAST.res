module E = FluidExpression

open ProgramTypes.AST

type id = Prelude.id

type t = ProgramTypes.AST.t

let show = (Root(e)) => E.show(e)

let pp = (f, Root(e)) => E.pp(f, e)

let toExpr = (Root(e)) => e

let ofExpr = e => Root(e)

let toID = (Root(e)) => E.toID(e)

let map = (~f: E.t => E.t, ast: t): t => toExpr(ast) |> f |> ofExpr

let replace = (~replacement: E.t, target: id, ast: t): t =>
  map(ast, ~f=E.replace(~replacement, target))

let update = (~failIfMissing=true, ~fExpr: E.t => E.t, target: id, ast: t): t => {
  map(ast, ~f=E.update(~failIfMissing, ~f=fExpr, ~fPattern=p => p, target))
}

let filter = (ast: t, ~f: E.t => bool): list<E.t> => toExpr(ast) |> E.filter(~f)

let blanks = (ast: t): list<E.t> => toExpr(ast) |> E.blanks

let ids = (ast: t): list<id> => toExpr(ast) |> E.ids

let find = (target: id, ast: t): option<E.t> => toExpr(ast) |> E.find(target)

let findParent = (target: id, ast: t): option<E.t> => toExpr(ast) |> E.findParent(target)

let ancestors = (target: id, ast: t): list<E.t> => toExpr(ast) |> E.ancestors(target)

let getFeatureFlags = (ast: t): list<E.t> =>
  filter(ast, ~f=x =>
    switch x {
    | EFeatureFlag(_) => true
    | _ => false
    }
  )

let clone = map(~f=E.clone)

let testEqualIgnoringIds = (a: t, b: t): bool => E.testEqualIgnoringIds(toExpr(a), toExpr(b))
