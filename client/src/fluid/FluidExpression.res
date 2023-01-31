include Prelude

open PT.Expr

@ppx.deriving(show({with_path: false}))
type rec t = PT.Expr.t

@ppx.deriving(show({with_path: false}))
type rec fluidMatchPatOrExpr =
  | Expr(t)
  | MatchPat(id, fluidMatchPattern)

let newB = () => PT.Expr.EBlank(gid())

let toID = (expr: t): id =>
  switch expr {
  | EInteger(id, _)
  | EString(id, _)
  | ECharacter(id, _)
  | EBool(id, _)
  | ENull(id)
  | EFloat(id, _, _, _)
  | EVariable(id, _)
  | EFieldAccess(id, _, _)
  | EFnCall(id, _, _, _)
  | ELambda(id, _, _)
  | EBlank(id)
  | ELet(id, _, _, _)
  | EIf(id, _, _, _)
  | EPartial(id, _, _)
  | ERightPartial(id, _, _)
  | ELeftPartial(id, _, _)
  | EList(id, _)
  | ETuple(id, _, _, _)
  | ERecord(id, _)
  | EPipe(id, _, _, _)
  | EPipeTarget(id)
  | EInfix(id, _, _, _)
  | EConstructor(id, _, _)
  | EFeatureFlag(id, _, _, _, _)
  | EMatch(id, _, _) => id
  }

let rec findExprOrPat = (target: id, within: fluidMatchPatOrExpr): option<fluidMatchPatOrExpr> => {
  let (id, childPatOrExprs) = switch within {
  | Expr(expr) =>
    switch expr {
    | EInteger(id, _)
    | EBool(id, _)
    | EString(id, _)
    | ECharacter(id, _)
    | EFloat(id, _, _, _)
    | ENull(id)
    | EBlank(id)
    | EVariable(id, _)
    | EPipeTarget(id) => (id, list{})
    | ELet(id, _, e1, e2) | EInfix(id, _, e1, e2) => (
        id,
        list{Expr(e1), Expr(e2)},
      )
    | EIf(id, e1, e2, e3) | EFeatureFlag(id, _, e1, e2, e3) => (
        id,
        list{Expr(e1), Expr(e2), Expr(e3)},
      )
    | EPipe(id, expr1, expr2, exprs) => (
        id,
        List.map(list{expr1, expr2, ...exprs}, ~f=e1 => Expr(e1)),
      )
    | ELambda(id, _, e1)
    | EFieldAccess(id, e1, _)
    | EPartial(id, _, e1)
    | ERightPartial(id, _, e1)
    | ELeftPartial(id, _, e1) => (id, list{Expr(e1)})
    | EFnCall(id, _, exprs, _)
    | EList(id, exprs)
    | EConstructor(id, _, exprs) => (id, List.map(exprs, ~f=e1 => Expr(e1)))
    | ETuple(id, first, second, theRest) =>
      let childExprs = List.map(list{first, second, ...theRest}, ~f=e1 => Expr(e1))
      (id, childExprs)
    | ERecord(id, nameAndExprs) => (id, List.map(nameAndExprs, ~f=((_, e1)) => Expr(e1)))
    | EMatch(id, e1, pairs) => (
        id,
        list{
          Expr(e1),
          ...pairs |> List.map(~f=((p1, e1)) => list{MatchPat(id, p1), Expr(e1)}) |> List.flatten,
        },
      )
    }
  | MatchPat(matchID, pat) =>
    switch pat {
    | MPVariable(pid, _)
    | MPInteger(pid, _)
    | MPBool(pid, _)
    | MPNull(pid)
    | MPBlank(pid)
    | MPFloat(pid, _, _, _)
    | MPCharacter(pid, _)
    | MPString(pid, _) => (pid, list{})
    | MPConstructor(pid, _, pats) => (pid, List.map(pats, ~f=p1 => MatchPat(matchID, p1)))
    | MPTuple(pid, first, second, theRest) => (
        pid,
        List.map(list{first, second, ...theRest}, ~f=p1 => MatchPat(matchID, p1)),
      )
    }
  }

  if id == target {
    Some(within)
  } else {
    childPatOrExprs |> List.findMap(~f=pOrE => findExprOrPat(target, pOrE))
  }
}

let rec findExpr = (target: id, expr: t): option<t> => {
  let fe = findExpr(target)
  if toID(expr) == target {
    Some(expr)
  } else {
    switch expr {
    | EInteger(_)
    | EBlank(_)
    | EString(_)
    | ECharacter(_)
    | EVariable(_)
    | EBool(_)
    | ENull(_)
    | EPipeTarget(_)
    | EFloat(_) =>
      None
    | ELet(_, _, rhs, next) =>
      fe(rhs) |> Option.orElseLazy(() => fe(next))
    | EIf(_, cond, ifexpr, elseexpr) =>
      fe(cond) |> Option.orElseLazy(() => fe(ifexpr)) |> Option.orElseLazy(() => fe(elseexpr))
    | EInfix(_, _, lexpr, rexpr) => fe(lexpr) |> Option.orElseLazy(() => fe(rexpr))
    | EFieldAccess(_, expr, _) | ELambda(_, _, expr) => fe(expr)
    | ERecord(_, fields) => fields |> List.map(~f=Tuple2.second) |> List.findMap(~f=fe)
    | EMatch(_, expr, pairs) =>
      fe(expr) |> Option.orElseLazy(() =>
        pairs |> List.map(~f=Tuple2.second) |> List.findMap(~f=fe)
      )
    | EFnCall(_, _, exprs, _)
    | EList(_, exprs)
    | EConstructor(_, _, exprs) =>
      List.findMap(~f=fe, exprs)
    | ETuple(_, first, second, theRest) =>
      let exprs = list{first, second, ...theRest}
      List.findMap(~f=fe, exprs)
    | EPipe(_, expr1, expr2, exprs) => List.findMap(~f=fe, list{expr1, expr2, ...exprs})
    | EPartial(_, _, oldExpr)
    | ERightPartial(_, _, oldExpr)
    | ELeftPartial(_, _, oldExpr) =>
      fe(oldExpr)
    | EFeatureFlag(_, _, cond, casea, caseb) =>
      fe(cond) |> Option.orElseLazy(() => fe(casea)) |> Option.orElseLazy(() => fe(caseb))
    }
  }
}

let findMP = (target: id, expr: t): option<FluidMatchPattern.t> => {
  switch expr {
  | EInteger(_)
  | EString(_)
  | ECharacter(_)
  | EBool(_)
  | ENull(_)
  | EFloat(_)
  | EVariable(_)
  | EFieldAccess(_)
  | EFnCall(_)
  | ELambda(_)
  | EBlank(_)
  | ELet(_)
  | EIf(_)
  | EPartial(_)
  | ERightPartial(_)
  | ELeftPartial(_)
  | EList(_)
  | ETuple(_)
  | ERecord(_)
  | EPipe(_)
  | EPipeTarget(_)
  | EInfix(_)
  | EConstructor(_)
  | EFeatureFlag(_) =>
    None
  | EMatch(_, _, cases) =>
    cases
    ->List.map(~f=Tuple2.first)
    ->List.filterMap(~f=pat => FluidMatchPattern.findMatchPattern(target, pat))
    ->List.head
  }
}

let children = (expr: t): list<t> =>
  switch expr {
  // None
  | EInteger(_)
  | EString(_)
  | ECharacter(_)
  | EBool(_)
  | EFloat(_)
  | ENull(_)
  | EBlank(_)
  | EPipeTarget(_)
  | EVariable(_) => list{}

  // One
  | EPartial(_, _, expr)
  | ERightPartial(_, _, expr)
  | ELeftPartial(_, _, expr)
  | ELambda(_, _, expr)
  | EFieldAccess(_, expr, _) => list{expr}

  // Two
  | EInfix(_, _, c0, c1) | ELet(_, _, c0, c1) => list{c0, c1}

  // Three
  | EFeatureFlag(_, _, c0, c1, c2) | EIf(_, c0, c1, c2) => list{c0, c1, c2}

  // Multiple
  | EFnCall(_, _, exprs, _)
  | EList(_, exprs)
  | EConstructor(_, _, exprs) => exprs
  | ETuple(_, first, second, theRest) => list{first, second, ...theRest}
  | EPipe(_, expr1, expr2, exprs) => list{expr1, expr2, ...exprs}

  // Special
  | ERecord(_, pairs) => pairs |> List.map(~f=Tuple2.second)
  | EMatch(_, matchExpr, cases) =>
    let casePointers = cases |> List.map(~f=Tuple2.second)
    list{matchExpr, ...casePointers}
  }

let findParent = (target: id, expr: t): option<t> => {
  let rec findParent' = (~parent: option<t>, target: id, expr: t): option<t> =>
    if toID(expr) == target {
      parent
    } else {
      let f = findParent'(~parent=Some(expr), target)
      List.findMap(~f, children(expr))
    }

  findParent'(~parent=None, target, expr)
}

let isBlank = (expr: t) =>
  switch expr {
  | EBlank(_) => true
  | _ => false
  }

let isEmpty = (expr: t): bool =>
  switch expr {
  | EBlank(_) => true
  | ERecord(_, list{}) => true
  | ERecord(_, l) => l |> List.filter(~f=((k, v)) => k == "" && !isBlank(v)) |> List.isEmpty
  | EList(_, l) => l |> List.filter(~f=\"<<"(not, isBlank)) |> List.isEmpty
  | ETuple(_, first, second, theRest) =>
    let exprs = list{first, second, ...theRest}
    exprs |> List.filter(~f=e => !isBlank(e)) |> List.isEmpty
  | _ => false
  }

let hasEmptyWithId = (id: id, expr: t): bool =>
  switch findExpr(id, expr) {
  | Some(e) => isEmpty(e)
  | _ => false
  }

let rec preTraversal = (~f: t => t, expr: t): t => {
  let r = preTraversal(~f)
  let expr = f(expr)
  switch expr {
  | EInteger(_)
  | EBlank(_)
  | EString(_)
  | ECharacter(_)
  | EVariable(_)
  | EBool(_)
  | ENull(_)
  | EPipeTarget(_)
  | EFloat(_) => expr
  | ELet(id, pat, rhs, next) => ELet(id, pat, r(rhs), r(next))
  | EIf(id, cond, ifexpr, elseexpr) => EIf(id, r(cond), r(ifexpr), r(elseexpr))
  | EInfix(id, op, lexpr, rexpr) => EInfix(id, op, r(lexpr), r(rexpr))
  | EFieldAccess(id, expr, fieldname) => EFieldAccess(id, r(expr), fieldname)
  | EFnCall(id, name, exprs, ster) => EFnCall(id, name, List.map(~f=r, exprs), ster)
  | ELambda(id, names, expr) => ELambda(id, names, r(expr))
  | EList(id, exprs) => EList(id, List.map(~f=r, exprs))
  | ETuple(id, first, second, theRest) => ETuple(id, r(first), r(second), List.map(~f=r, theRest))
  | EMatch(id, mexpr, pairs) =>
    EMatch(id, r(mexpr), List.map(~f=((name, expr)) => (name, r(expr)), pairs))
  | ERecord(id, fields) => ERecord(id, List.map(~f=((name, expr)) => (name, r(expr)), fields))
  | EPipe(id, expr1, expr2, exprs) => EPipe(id, r(expr1), r(expr2), List.map(~f=r, exprs))
  | EConstructor(id, name, exprs) => EConstructor(id, name, List.map(~f=r, exprs))
  | EPartial(id, str, oldExpr) => EPartial(id, str, r(oldExpr))
  | ERightPartial(id, str, oldExpr) => ERightPartial(id, str, r(oldExpr))
  | ELeftPartial(id, str, oldExpr) => ELeftPartial(id, str, r(oldExpr))
  | EFeatureFlag(id, msg, cond, casea, caseb) => EFeatureFlag(id, msg, r(cond), r(casea), r(caseb))
  }
}

let rec postTraversal = (~f: t => t, expr: t): t => {
  let r = postTraversal(~f)
  let result = switch expr {
  | EInteger(_)
  | EBlank(_)
  | EString(_)
  | ECharacter(_)
  | EVariable(_)
  | EBool(_)
  | ENull(_)
  | EPipeTarget(_)
  | EFloat(_) => expr
  | ELet(id, pat, rhs, next) => ELet(id, pat, r(rhs), r(next))
  | EIf(id, cond, ifexpr, elseexpr) => EIf(id, r(cond), r(ifexpr), r(elseexpr))
  | EInfix(id, op, lexpr, rexpr) => EInfix(id, op, r(lexpr), r(rexpr))
  | EFieldAccess(id, expr, fieldname) => EFieldAccess(id, r(expr), fieldname)
  | EFnCall(id, name, exprs, ster) => EFnCall(id, name, List.map(~f=r, exprs), ster)
  | ELambda(id, names, expr) => ELambda(id, names, r(expr))
  | EList(id, exprs) => EList(id, List.map(~f=r, exprs))
  | ETuple(id, first, second, theRest) => ETuple(id, r(first), r(second), List.map(~f=r, theRest))
  | EMatch(id, mexpr, pairs) =>
    EMatch(id, r(mexpr), List.map(~f=((name, expr)) => (name, r(expr)), pairs))
  | ERecord(id, fields) => ERecord(id, List.map(~f=((name, expr)) => (name, r(expr)), fields))
  | EPipe(id, expr1, expr2, exprs) => EPipe(id, r(expr1), r(expr2), List.map(~f=r, exprs))
  | EConstructor(id, name, exprs) => EConstructor(id, name, List.map(~f=r, exprs))
  | EPartial(id, str, oldExpr) => EPartial(id, str, r(oldExpr))
  | ERightPartial(id, str, oldExpr) => ERightPartial(id, str, r(oldExpr))
  | ELeftPartial(id, str, oldExpr) => ELeftPartial(id, str, r(oldExpr))
  | EFeatureFlag(id, msg, cond, casea, caseb) => EFeatureFlag(id, msg, r(cond), r(casea), r(caseb))
  }

  f(result)
}

let deprecatedWalk = (~f: t => t, expr: t): t =>
  switch expr {
  | EInteger(_)
  | EBlank(_)
  | EString(_)
  | ECharacter(_)
  | EVariable(_)
  | EBool(_)
  | ENull(_)
  | EPipeTarget(_)
  | EFloat(_) => expr
  | ELet(id, pat, rhs, next) => ELet(id, pat, f(rhs), f(next))
  | EIf(id, cond, ifexpr, elseexpr) => EIf(id, f(cond), f(ifexpr), f(elseexpr))
  | EInfix(id, op, lexpr, rexpr) => EInfix(id, op, f(lexpr), f(rexpr))
  | EFieldAccess(id, expr, fieldname) => EFieldAccess(id, f(expr), fieldname)
  | EFnCall(id, name, exprs, ster) => EFnCall(id, name, List.map(~f, exprs), ster)
  | ELambda(id, names, expr) => ELambda(id, names, f(expr))
  | EList(id, exprs) => EList(id, List.map(~f, exprs))
  | ETuple(id, first, second, theRest) => ETuple(id, f(first), f(second), List.map(~f, theRest))
  | EMatch(id, mexpr, pairs) =>
    EMatch(id, f(mexpr), List.map(~f=((name, expr)) => (name, f(expr)), pairs))
  | ERecord(id, fields) => ERecord(id, List.map(~f=((name, expr)) => (name, f(expr)), fields))
  | EPipe(id, expr1, expr2, exprs) => EPipe(id, f(expr1), f(expr2), List.map(~f, exprs))
  | EConstructor(id, name, exprs) => EConstructor(id, name, List.map(~f, exprs))
  | EPartial(id, str, oldExpr) => EPartial(id, str, f(oldExpr))
  | ERightPartial(id, str, oldExpr) => ERightPartial(id, str, f(oldExpr))
  | ELeftPartial(id, str, oldExpr) => ELeftPartial(id, str, f(oldExpr))
  | EFeatureFlag(id, msg, cond, casea, caseb) => EFeatureFlag(id, msg, f(cond), f(casea), f(caseb))
  }

let filterMap = (~f: t => option<'a>, expr: t): list<'a> => {
  let results = ref(list{})
  let rec myWalk = (e: t): t => {
    switch f(e) {
    | Some(r) =>
      results := list{r, ...results.contents}
      ()
    | None => ()
    }
    deprecatedWalk(~f=myWalk, e)
  }

  ignore(myWalk(expr))
  List.reverse(results.contents)
}

let filter = (~f: t => bool, expr: t): list<t> => filterMap(~f=t =>
    if f(t) {
      Some(t)
    } else {
      None
    }
  , expr)

let decendants = (expr: t): list<id> => {
  let res = ref(list{})
  preTraversal(expr, ~f=e => {
    res := list{toID(e), ...res.contents}
    e
  }) |> ignore
  res.contents
}

let update = (~failIfMissing=true, ~f: t => t, target: id, ast: t): t => {
  let found = ref(false)
  let rec run = e =>
    if target == toID(e) {
      found := true
      f(e)
    } else {
      deprecatedWalk(~f=run, e)
    }

  let finished = run(ast)
  if failIfMissing {
    if !found.contents {
      // prevents the significant performance cost of show
      Recover.asserT(
        ~debug=(ID.toString(target), Js.Json.stringifyAny(ast)),
        "didn't find the id in the expression to update",
        found.contents,
      )
    }
  }
  finished
}

/* FIXME: [replace] is just [update] with a hack for EPipe.
 * It's very unclear which to use at what point and likely to cause bugs.
 * We should either hide [update] from the public interface of FluidExpression
 * or remove [replace] and put the special-case EPipe logic into the calling code. */
let replace = (~replacement: t, target: id, ast: t): t => {
  // If we're putting a pipe into another pipe, fix it up
  let (target', newExpr') = switch (findParent(target, ast), replacement) {
  | (Some(EPipe(parentID, oldExpr1, oldExpr2, oldRest)), EPipe(newID, newExpr1, newExpr2, newRest))
    if toID(oldExpr1) == target => (
      parentID,
      EPipe(newID, newExpr1, newExpr2, Belt.List.concatMany([newRest, list{oldExpr2}, oldRest])),
    )
  | (Some(EPipe(parentID, oldExpr1, oldExpr2, oldRest)), EPipe(newID, newExpr1, newExpr2, newRest))
    if toID(oldExpr2) == target => (
      parentID,
      EPipe(newID, oldExpr1, newExpr1, Belt.List.concatMany([list{newExpr2}, newRest, oldRest])),
    )
  | (
      Some(EPipe(parentID, oldExpr1, oldExpr2, oldRest)),
      EPipe(newID, newExpr1, newExpr2, newRest),
    ) =>
    let (before, elemAndAfter) = List.splitWhen(~f=nested => toID(nested) == target, oldRest)
    let after = List.tail(elemAndAfter) |> Option.unwrap(~default=list{})
    let newExprs = list{newExpr1, newExpr2, ...newRest}
    (parentID, EPipe(newID, oldExpr1, oldExpr2, Belt.List.concatMany([before, newExprs, after])))
  | _ => (target, replacement)
  }

  update(target', ast, ~f=_ => newExpr')
}

// Slightly modified version of `AST.uses` (pre-fluid code)
let rec updateVariableUses = (oldVarName: string, ~f: t => t, ast: t): t => {
  let u = updateVariableUses(oldVarName, ~f)
  switch ast {
  | EVariable(_, varName) =>
    if varName == oldVarName {
      f(ast)
    } else {
      ast
    }
  | ELet(id, pat, rhs, body) =>
    let varName =
      switch pat {
        | LPVariable(_, name) => name
      }

    if oldVarName == varName {
      ELet(id, LPVariable(ID.generate(), varName), u(rhs), body)
    } else {
      ELet(id, LPVariable(ID.generate(), varName), u(rhs), u(body))
    }
  | ELambda(id, vars, lexpr) =>
    if List.map(~f=Tuple2.second, vars) |> List.member(~value=oldVarName) {
      // if variable name is rebound
      ast
    } else {
      ELambda(id, vars, u(lexpr))
    }
  | EMatch(id, cond, pairs) =>
    let pairs = List.map(~f=((pat, expr)) =>
      if FluidMatchPattern.hasVariableNamed(oldVarName, pat) {
        (pat, expr)
      } else {
        (pat, u(expr))
      }
    , pairs)

    EMatch(id, u(cond), pairs)
  | _ => deprecatedWalk(~f=u, ast)
  }
}

let renameVariableUses = (~oldName: string, ~newName: string, ast: t): t => {
  let f = expr =>
    switch expr {
    | EVariable(id, _) => EVariable(id, newName)
    | _ => expr
    }

  updateVariableUses(oldName, ~f, ast)
}

let removeVariableUse = (oldVarName: string, ast: t): t => {
  let f = _ => EBlank(gid())
  updateVariableUses(oldVarName, ~f, ast)
}

let rec clone = (expr: t): t => {
  let c = e => clone(e)
  let cl = es => List.map(~f=c, es)
  switch expr {
  | ELet(_, pat, rhs, body) =>
    ELet(gid(), FluidLetPattern.clone(pat), c(rhs), c(body))
  | EIf(_, cond, ifbody, elsebody) => EIf(gid(), c(cond), c(ifbody), c(elsebody))
  | EFnCall(_, name, exprs, r) => EFnCall(gid(), name, cl(exprs), r)
  | EInfix(_, name, left, right) => EInfix(gid(), name, c(left), c(right))
  | ELambda(_, vars, body) => ELambda(gid(), List.map(vars, ~f=((_, var)) => (gid(), var)), c(body))
  | EPipe(_, e1, e2, rest) => EPipe(gid(), c(e1), c(e2), cl(rest))
  | EFieldAccess(_, obj, field) => EFieldAccess(gid(), c(obj), field)
  | EString(_, v) => EString(gid(), v)
  | ECharacter(_, v) => ECharacter(gid(), v)
  | EInteger(_, v) => EInteger(gid(), v)
  | EBool(_, v) => EBool(gid(), v)
  | EFloat(_, sign, whole, fraction) => EFloat(gid(), sign, whole, fraction)
  | ENull(_) => ENull(gid())
  | EBlank(_) => EBlank(gid())
  | EVariable(_, name) => EVariable(gid(), name)
  | EList(_, exprs) => EList(gid(), cl(exprs))
  | ETuple(_, first, second, theRest) => ETuple(gid(), c(first), c(second), cl(theRest))
  | ERecord(_, pairs) => ERecord(gid(), List.map(~f=((k, v)) => (k, c(v)), pairs))
  | EFeatureFlag(_, name, cond, a, b) => EFeatureFlag(gid(), name, c(cond), c(a), c(b))
  | EMatch(_, matchExpr, cases) =>
    EMatch(gid(), c(matchExpr), List.map(~f=((k, v)) => (FluidMatchPattern.clone(k), c(v)), cases))
  | EConstructor(_, name, args) => EConstructor(gid(), name, cl(args))
  | EPartial(_, str, oldExpr) => EPartial(gid(), str, c(oldExpr))
  | ERightPartial(_, str, oldExpr) => ERightPartial(gid(), str, c(oldExpr))
  | ELeftPartial(id, str, oldExpr) => ELeftPartial(id, str, c(oldExpr))
  | EPipeTarget(_) => EPipeTarget(gid())
  }
}

let blanks = filter(~f=isBlank)

let ids = (ast: t): list<id> => filter(ast, ~f=_ => true) |> List.map(~f=toID)

let ancestors = (id: id, expr: t): list<t> => {
  let rec rec_ancestors = (tofind: id, walk: list<t>, exp: t) => {
    let rec_ = (id_, e_, walk_) => rec_ancestors(id_, list{e_, ...walk_})
    let reclist = (id_, e_, walk_, exprs) =>
      exprs |> List.map(~f=rec_(id_, e_, walk_)) |> List.flatten

    if toID(exp) == tofind {
      walk
    } else {
      switch exp {
      | EInteger(_)
      | EString(_)
      | ECharacter(_)
      | EBool(_)
      | EFloat(_)
      | ENull(_)
      | EBlank(_)
      | EPipeTarget(_) => list{}
      | EVariable(_) => list{}
      | ELet(_, _, rhs, body) =>
        reclist(id, exp, walk, list{rhs, body})
      | EIf(_, cond, ifbody, elsebody) => reclist(id, exp, walk, list{cond, ifbody, elsebody})
      | EFnCall(_, _, exprs, _) => reclist(id, exp, walk, exprs)
      | EInfix(_, _, lhs, rhs) => reclist(id, exp, walk, list{lhs, rhs})
      | ELambda(_, _, lexpr) => rec_(id, exp, walk, lexpr)
      | EPipe(_, e1, e2, rest) => reclist(id, exp, walk, list{e1, e2, ...rest})
      | EFieldAccess(_, obj, _) => rec_(id, exp, walk, obj)
      | EList(_, exprs) => reclist(id, expr, walk, exprs)
      | ETuple(_, first, second, theRest) =>
        let exprs = list{first, second, ...theRest}
        reclist(id, expr, walk, exprs)
      | ERecord(_, pairs) => pairs |> List.map(~f=Tuple2.second) |> reclist(id, expr, walk)
      | EFeatureFlag(_, _, cond, a, b) => reclist(id, exp, walk, list{cond, a, b})
      | EMatch(_, matchExpr, cases) =>
        reclist(id, exp, walk, list{matchExpr, ...List.map(~f=Tuple2.second, cases)})
      | EConstructor(_, _, args) => reclist(id, exp, walk, args)
      | EPartial(_, _, oldExpr) => rec_(id, exp, walk, oldExpr)
      | ERightPartial(_, _, oldExpr) => rec_(id, exp, walk, oldExpr)
      | ELeftPartial(_, _, oldExpr) => rec_(id, exp, walk, oldExpr)
      }
    }
  }

  rec_ancestors(id, list{}, expr)
}

let validateAndFix = (~onError: (string, t) => unit, expr: t): t => {
  expr->preTraversal(~f=expr =>
    switch expr {
    | EFloat(id, sign, whole, fraction) =>
      if String.startsWith(~prefix="-", whole) {
        onError("Floats cannot have a negative sign in the whole number", expr)

        let newSign = if sign == ProgramTypes.Sign.Negative {
          ProgramTypes.Sign.Positive
        } else {
          ProgramTypes.Sign.Negative
        }
        EFloat(id, newSign, String.dropLeft(~count=1, whole), fraction)
      } else {
        expr
      }
    | EInteger(_)
    | EString(_)
    | ECharacter(_)
    | EBool(_)
    | ENull(_)
    | EBlank(_)
    | EPipeTarget(_)
    | EVariable(_)
    | EPartial(_)
    | ERightPartial(_)
    | ELeftPartial(_)
    | ELet(_, _, _, _)
    | EIf(_, _, _, _)
    | EFnCall(_, _, _, _)
    | EInfix(_, _, _, _)
    | ELambda(_, _, _)
    | EPipe(_, _, _, _)
    | EFieldAccess(_, _, _)
    | EList(_, _)
    | ETuple(_, _, _, _)
    | ERecord(_, _)
    | EFeatureFlag(_, _, _, _, _)
    | EMatch(_, _, _)
    | EConstructor(_, _, _) => expr
    }
  )
}

let rec testEqualIgnoringIds = (a: t, b: t): bool => {
  // helpers for recursive calls
  let eq = testEqualIgnoringIds
  let eq2 = ((e, e'), (f, f')) => eq(e, e') && eq(f, f')
  let eq3 = ((e, e'), (f, f'), (g, g')) => eq(e, e') && (eq(f, f') && eq(g, g'))
  let eqList = (l1, l2) =>
    List.length(l1) == List.length(l2) && List.map2(~f=eq, l1, l2) |> List.all(~f=identity)

  let rec peq = (a: FluidMatchPattern.t, b: FluidMatchPattern.t) => {
    let peqList = (l1, l2) =>
      List.length(l1) == List.length(l2) &&
        Tc.List.map2(~f=peq, l1, l2) |> Tc.List.all(~f=Tc.identity)

    switch (a, b) {
    | (MPVariable(_, name), MPVariable(_, name')) => name == name'
    | (MPConstructor(_, name, args), MPConstructor(_, name', args')) =>
      name == name' && peqList(args, args')
    | (MPString(_, str), MPString(_, str')) => str == str'
    | (MPCharacter(_, str), MPCharacter(_, str')) => str == str'
    | (MPInteger(_, l), MPInteger(_, l')) => l == l'
    | (MPFloat(_, s, w, f), MPFloat(_, s', w', f')) => (s, w, f) == (s', w', f')
    | (MPBool(_, l), MPBool(_, l')) => l == l'
    | (MPNull(_), MPNull(_)) => true
    | (MPBlank(_), MPBlank(_)) => true
    | (MPTuple(_, first, second, theRest), MPTuple(_, first', second', theRest')) =>
      peqList(list{first, second, ...theRest}, list{first', second', ...theRest'})

    // exhaust pattern matching
    | (MPVariable(_), _)
    | (MPConstructor(_), _)
    | (MPString(_), _)
    | (MPCharacter(_), _)
    | (MPInteger(_), _)
    | (MPFloat(_), _)
    | (MPBool(_), _)
    | (MPNull(_), _)
    | (MPBlank(_), _)
    | (MPTuple(_), _) => false
    }
  }

  switch (a, b) {
  // expressions with no values
  | (ENull(_), ENull(_)) | (EBlank(_), EBlank(_)) | (EPipeTarget(_), EPipeTarget(_)) => true
  // expressions with single string values
  | (EInteger(_, v), EInteger(_, v')) => v == v'
  | (EString(_, v), EString(_, v'))
  | (ECharacter(_, v), ECharacter(_, v'))
  | (EVariable(_, v), EVariable(_, v')) =>
    v == v'
  | (EBool(_, v), EBool(_, v')) => v == v'
  | (EFloat(_, sign, whole, frac), EFloat(_, sign', whole', frac')) =>
    sign == sign' && whole == whole' && frac == frac'
  | (ELet(_, _pat, rhs, body), ELet(_, _pat', rhs', body')) =>
    // todo: compare lhs
    eq2((rhs, rhs'), (body, body'))
  | (EIf(_, con, thn, els), EIf(_, con', thn', els')) => eq3((con, con'), (thn, thn'), (els, els'))
  | (EList(_, l), EList(_, l')) => eqList(l, l')
  | (ETuple(_, first, second, theRest), ETuple(_, first', second', theRest')) =>
    let exprs = list{first, second, ...theRest}
    let exprs' = list{first', second', ...theRest'}
    eqList(exprs, exprs')
  | (EFnCall(_, name, args, toRail), EFnCall(_, name', args', toRail')) =>
    name == name' && (eqList(args, args') && toRail == toRail')
  | (EInfix(_, op, lhs, rhs), EInfix(_, op', lhs', rhs')) =>
    op == op' && eq2((lhs, lhs'), (rhs, rhs'))
  | (ERecord(_, pairs), ERecord(_, pairs')) =>
    let sort = List.sortBy(~f=((k, _)) => k)
    List.map2(~f=((k, v), (k', v')) => k == k' && eq(v, v'), sort(pairs), sort(pairs')) |> List.all(
      ~f=identity,
    )
  | (EFieldAccess(_, e, f), EFieldAccess(_, e', f')) => eq(e, e') && f == f'
  | (EPipe(_, e1, e2, l), EPipe(_, e1', e2', l')) => eq(e1, e1') && eq(e2, e2') && eqList(l, l')
  | (EFeatureFlag(_, _, cond, old, knew), EFeatureFlag(_, _, cond', old', knew')) =>
    eq3((cond, cond'), (old, old'), (knew, knew'))
  | (EConstructor(_, s, ts), EConstructor(_, s', ts')) => s == s' && eqList(ts, ts')
  | (ERightPartial(_, str, e), ERightPartial(_, str', e'))
  | (ELeftPartial(_, str, e), ELeftPartial(_, str', e'))
  | (EPartial(_, str, e), EPartial(_, str', e')) =>
    str == str' && eq(e, e')
  | (ELambda(_, vars, e), ELambda(_, vars', e')) =>
    eq(e, e') && List.all(~f=identity, List.map2(vars, vars', ~f=((_, v), (_, v')) => v == v'))
  | (EMatch(_, e, branches), EMatch(_, e', branches')) =>
    eq(e, e') &&
    Tc.List.map2(
      ~f=((p, v), (p', v')) => peq(p, p') && eq(v, v'),
      branches,
      branches',
    ) |> Tc.List.all(~f=Tc.identity)
  | (ENull(_), _)
  | (EBlank(_), _)
  | (EPipeTarget(_), _)
  | (EInteger(_), _)
  | (EString(_), _)
  | (ECharacter(_), _)
  | (EVariable(_), _)
  | (EBool(_), _)
  | (EFloat(_), _)
  | (ELet(_), _)
  | (EIf(_), _)
  | (EList(_), _)
  | (ETuple(_), _)
  | (EFnCall(_), _)
  | (EInfix(_), _)
  | (ERecord(_), _)
  | (EFieldAccess(_), _)
  | (EPipe(_), _)
  | (EFeatureFlag(_), _)
  | (EConstructor(_), _)
  | (ELeftPartial(_), _)
  | (ERightPartial(_), _)
  | (EPartial(_), _)
  | (ELambda(_), _)
  | (EMatch(_), _) => // exhaustiveness check
    false
  }
}

let toHumanReadable = (expr: t, showID: bool): string => {
  let rec recurse = (~indent: int, expr: t): string => {
    let iStr = String.repeat(~count=indent, " ")
    let r = recurse(~indent)
    let rin = recurse(~indent=indent + 2)
    let newlineList = exprs => exprs |> List.map(~f=rin) |> String.join(~sep="\n")
    let quoted = str => `"${str}"`
    let id = if showID {
      `[${expr->toID->ID.toString}]`
    } else {
      ""
    }
    let eStr = switch expr {
    | EBlank(_) => `(blank${id})`
    | ECharacter(_, str) => `(char${id} '${str}')`
    | EString(_, str) =>
      let str = if String.length(str) > 20 {
        String.slice(~from=0, ~to_=20, str) ++ "..."
      } else {
        str
      }
      `(str${id} "${str}")`
    | EBool(_, true) => `(true${id})`
    | EBool(_, false) => `(false${id})`
    | EFloat(_, sign, whole, fractional) =>
      let sign = ProgramTypes.Sign.toString(sign)
      `(${sign}${whole}.${fractional}${id})`
    | EInteger(_, i) => `(${Int64.to_string(i)}${id})`
    | ENull(_) => `(null${id})`
    | EPipeTarget(_) => `(pt${id})`
    | EPartial(_, str, e) => `(partial${id} "${str}" ${r(e)})`
    | ERightPartial(_, str, e) => `(rpartial${id} "${str}" ${r(e)})`
    | ELeftPartial(_, str, e) => `(lpartial${id} "${str}" ${r(e)})`
    | EFnCall(_, name, list{}, _) => `(fn${id} "${FQFnName.toString(name)}")`
    | EFnCall(_, name, exprs, _) => `(fn${id} "${FQFnName.toString(name)}"\n${newlineList(exprs)})`
    | EInfix(_, BinOp(op), lhs, rhs) =>
      `(binop${PT.Expr.BinaryOperation.toString(op)}${id} ${r(lhs)} ${r(rhs)})`
    | EInfix(_, InfixFnCall(name, _), lhs, rhs) =>
      `(infixFn${id} "${PT.InfixStdlibFnName.toString(name)}"\n${r(lhs)}\n${r(rhs)})`
    | EVariable(_, name) => `(${name}${id})`
    | EFieldAccess(_, e, name) => `(fieldAccess${id} "${name}"\n${r(e)})`
    | EMatch(_, cond, matches) =>
      let rec pToTestcase = (p: FluidMatchPattern.t): string => {
        let listed = elems => "[" ++ (String.join(~sep=";", elems) ++ "]")
        let spaced = elems => String.join(~sep=" ", elems)
        switch p {
        | MPBlank(_) => "mpBlank"
        | MPString(_, str) => spaced(list{"mpString", quoted(str)})
        | MPCharacter(_, str) => spaced(list{"mpCharacter", quoted(str)})
        | MPBool(_, true) => spaced(list{"mpBool true"})
        | MPBool(_, false) => spaced(list{"mpBool false"})
        | MPFloat(_, sign, whole, fractional) =>
          let sign = switch sign {
          | Positive => "Positive"
          | Negative => "Negative"
          }
          spaced(list{"mpFloat'", sign, whole, fractional})
        | MPInteger(_, int) => spaced(list{"mpInt", Int64.to_string(int)})
        | MPNull(_) => "mpNull"
        | MPVariable(_, name) => spaced(list{"mpVar", quoted(name)})
        | MPConstructor(_, name, args) =>
          spaced(list{"mpConstructor", quoted(name), listed(List.map(args, ~f=pToTestcase))})
        | MPTuple(_, first, second, theRest) =>
          let exprs = list{first, second, ...theRest} |> List.map(~f=pToTestcase)
          spaced(list{"mpTuple", "(" ++ String.join(~sep=",", exprs) ++ ")"})
        }
      }

      let matchStrs = List.map(matches, ~f=((p, e)) =>
        iStr ++ "  (" ++ pToTestcase(p) ++ ", " ++ rin(e) ++ ")"
      )

      `(match${id} ${r(cond)}\n${String.join(~sep="\n", matchStrs)})`
    | ERecord(_, list{}) => `(record${id})`
    | ERecord(_, pairs) =>
      let pairStrs = List.map(pairs, ~f=((k, v)) => `${iStr}("${k}" ${String.trim(r(v))})`)
      `(record${id}\n${String.join(~sep="\n", pairStrs)})`
    | EList(_, list{}) => `(list${id})`
    | EList(_, exprs) => `(list${id}\n${newlineList(exprs)})`
    | ETuple(_, first, second, theRest) =>
      let exprs = list{first, second, ...theRest}
      `(tuple${id}\n${newlineList(exprs)})`
    | EPipe(_, e1, e2, rest) => `(pipe${id}\n${newlineList(list{e1, e2, ...rest})})`
    | EConstructor(_, name, exprs) => `(constructor${id} "${name}"\n${newlineList(exprs)})`
    | EIf(_, cond, then', else') => `(if${id} ${r(cond)}\n${rin(then')}\n${rin(else')})`
    | ELet(_, pat, rhs, body) =>
      let lpToHumanReadable = (p: FluidLetPattern.t): string => {
        // TODO: review this whole thing; it's probably wrong.
        let spaced = elems => String.join(~sep=" ", elems)
        switch p {
        | LPVariable(_, name) => spaced(list{"lpVar", quoted(name)})
        }
      }

      `(let${id} ${lpToHumanReadable(pat)}\n${rin(rhs)}\n${r(body)})`
    | ELambda(_, names, body) =>
      let names = names->List.map(~f=Tuple2.second)->List.toArray->Js.Array2.joinWith("\", \"")
      `(lambda${id} "${names}"\n${rin(body)})`
    | EFeatureFlag(_, _, cond, old, new') => `(flag${id} ${rin(cond)}\n${rin(old)}\n${rin(new')})`
    }

    iStr ++ eStr
  }

  recurse(~indent=0, expr)
}
