open Prelude

// Dark
module B = BlankOr
module E = FluidExpression
open ProgramTypes.Expr
open ProgramTypes.MatchPattern

// --------------------------------
// PointerData
// --------------------------------

let isDefinitionOf = (var: string, expr: E.t): bool =>
  switch expr {
  | ELet(_, LPVariable(_, name), _, _) => name == var && name != ""
  | ELambda(_, vars, _) =>
    vars |> List.map(~f=Tuple2.second) |> List.any(~f=v => v == var && v != "")
  | EMatch(_, _, cases) =>
    let shadowsName = p => {
      let originalNames = FluidMatchPattern.variableNames(p)
      List.member(~value=var, originalNames)
    }

    cases |> List.map(~f=Tuple2.first) |> List.any(~f=shadowsName)
  | _ => false
  }

let rec uses = (var: string, expr: E.t): list<E.t> => {
  let u = uses(var)
  if isDefinitionOf(var, expr) {
    list{}
  } else {
    switch expr {
    | EInteger(_)
    | EString(_)
    | ECharacter(_)
    | EBool(_)
    | EFloat(_)
    | ENull(_)
    | EBlank(_)
    | EPipeTarget(_) => list{}
    | EVariable(_, potential) =>
      if potential == var {
        list{expr}
      } else {
        list{}
      }
    | ELet(_, _, rhs, body) =>
      List.flatten(list{u(rhs), u(body)})
    | EIf(_, cond, ifbody, elsebody) => List.flatten(list{u(cond), u(ifbody), u(elsebody)})
    | EFnCall(_, _, exprs, _) => exprs |> List.map(~f=u) |> List.flatten
    | EInfix(_, _, lhs, rhs) => Belt.List.concat(u(lhs), u(rhs))
    | EConstructor(_, _, exprs) => exprs |> List.map(~f=u) |> List.flatten
    | ELambda(_, _, lexpr) => u(lexpr)
    | EPipe(_, e1, e2, rest) => list{e1, e2, ...rest} |> List.map(~f=u) |> List.flatten
    | EFieldAccess(_, obj, _) => u(obj)
    | EList(_, exprs) => exprs |> List.map(~f=u) |> List.flatten
    | ETuple(_, first, second, theRest) =>
      list{first, second, ...theRest} |> List.map(~f=u) |> List.flatten
    | ERecord(_, pairs) => pairs |> List.map(~f=Tuple2.second) |> List.map(~f=u) |> List.flatten
    | EFeatureFlag(_, _, cond, a, b) => List.flatten(list{u(cond), u(a), u(b)})
    | EMatch(_, matchExpr, cases) =>
      let exprs = cases |> List.map(~f=Tuple2.second)
      Belt.List.concat(u(matchExpr), exprs)
    | EPartial(_, _, oldExpr) => u(oldExpr)
    | ERightPartial(_, _, oldExpr) => u(oldExpr)
    | ELeftPartial(_, _, oldExpr) => u(oldExpr)
    }
  }
}

// -------------------------
// EPipe stuff
// -------------------------

/* If the expression at `id` is one of the expressions in a pipe, this returns
 * the previous expression in that pipe (eg, the one that is piped into this
 * one) */
let pipePrevious = (id: id, ast: FluidAST.t): option<E.t> =>
  switch FluidAST.findExprParent(id, ast) {
  | Some(EPipe(_, e1, e2, rest)) =>
    let exprs = list{e1, e2, ...rest}
    exprs
    |> List.find(~f=e => E.toID(e) == id)
    |> Option.andThen(~f=value => Util.listPrevious(~value, exprs))
  | _ => None
  }

/* If the expression at `id` is one of the expressions in a pipe, this returns
 * the next expression in that pipe (eg, the one that the expr at `id` pipes into) */
let pipeNext = (id: id, ast: FluidAST.t): option<E.t> =>
  switch FluidAST.findExprParent(id, ast) {
  | Some(EPipe(_, e1, e2, rest)) =>
    let exprs = list{e1, e2, ...rest}
    exprs
    |> List.find(~f=e => E.toID(e) == id)
    |> Option.andThen(~f=value => Util.listNext(~value, exprs))
  | _ => None
  }

// Given the ID of a function call or infixFnCall, return its arguments. Takes pipes into account.
let getArguments = (id: id, ast: FluidAST.t): list<E.t> => {
  let pipePrevious = pipePrevious(id, ast)
  let caller = FluidAST.findExpr(id, ast)
  let defaultArgs = switch caller {
  | Some(EFnCall(_, _, args, _)) => args
  | Some(EInfix(_, InfixFnCall(_), arg0, arg1)) => list{arg0, arg1}
  | _ => list{}
  }

  switch (pipePrevious, defaultArgs) {
  | (
      Some(previous),
      list{EPipeTarget(_), ...rest},
    ) => /* pipetarget should be a pipetarget, but technically we might
     * allow something invalid here, esp due to copy/paste */
    list{previous, ...rest}
  | _ => defaultArgs
  }
}

/* Search for `id`, and if it is an argument of a function (including if it is
 * being piped into afunction), return the function name and the index of the
 * parameter it corresponds to.
 *
 * eg: Int::add 4 3 => if `id` was the id of the `4` expression, then we'd
 *                     return (`Int::add`, 0)
 */
let getParamIndex = (id: id, ast: FluidAST.t): option<(string, int)> => {
  let parent = pipeNext(id, ast) |> Option.orElseLazy(() => FluidAST.findExprParent(id, ast))

  switch parent {
  | Some(EFnCall(fnID, name, _, _)) =>
    getArguments(fnID, ast)
    |> List.findIndex(~f=(_, e) => E.toID(e) == id)
    |> Option.map(~f=((index, _)) => (FQFnName.toString(name), index))
  | Some(EInfix(fnID, InfixFnCall(name, _), _, _)) =>
    getArguments(fnID, ast)
    |> List.findIndex(~f=(_, e) => E.toID(e) == id)
    |> Option.map(~f=((index, _)) => (PT.InfixStdlibFnName.toString(name), index))

  | _ => None
  }
}

// -------------------------
// Ancestors
// -------------------------

let freeVariables = (ast: E.t): list<(id, string)> => {
  /* Find all variable lookups that lookup a variable that
   * is also _defined_ in this expression. We create a set of
   * these IDs so we can filter them out later. */
  let definedAndUsed =
    ast
    |> E.filterMap(~f=x =>
      // Grab all uses of the `lhs` of a Let in its body
      switch x {
      | ELet(_, LPVariable(_, varName), _, body) => Some(uses(varName, body))
      // Grab all uses of the `vars` of a Lambda in its body
      | ELambda(_, vars, body) =>
        vars
        |> List.map(~f=Tuple2.second)
        |> List.filter(~f=\"<>"(""))
        |> List.map(~f=v => uses(v, body))
        |> List.flatten
        |> (x => Some(x))
      | EMatch(_, _, cases) =>
        cases
        |> /* Grab all uses of the variable bindings in a `pattern`
         * in the `body` of each match case */
        List.map(~f=((pattern, body)) => {
          let vars = FluidMatchPattern.variableNames(pattern)
          List.map(~f=v => uses(v, body), vars)
        })
        |> List.flatten
        |> List.flatten
        |> (x => Some(x))
      | _ => None
      }
    )
    |> List.flatten
    |> List.map(~f=\">>"(E.toID, ID.toString))
    |> Set.String.fromList

  ast
  |> E.filterMap(~f=x =>
    switch x {
    | EVariable(id, name) =>
      /* Don't include EVariable lookups that we know are looking
       * up a variable bound in this expression */
      if Set.member(~value=ID.toString(id), definedAndUsed) {
        None
      } else {
        Some(id, name)
      }
    | _ => None
    }
  )
  |> List.uniqueBy(~f=((_, name)) => name)
}

module VarDict = Map.String
module IDComparable = Belt.Id.MakeComparable({
  type t = ID.t
  let cmp = ID.cmp
})

module IDTable = Belt.MutableMap

type sym_set = VarDict.t<id>

type sym_store = IDTable.t<ID.t, sym_set, IDComparable.t>

let rec sym_exec = (~trace: (E.t, sym_set) => unit, st: sym_set, expr: E.t): unit => {
  let sexe = sym_exec(~trace)
  ignore(
    switch expr {
    | EInteger(_)
    | EString(_)
    | ECharacter(_)
    | EBool(_)
    | EFloat(_)
    | ENull(_)
    | EBlank(_)
    | EPipeTarget(_) => ()
    | EVariable(_) => ()
    | ELet(_id, LPVariable(_, varName), rhs, body) =>
      sexe(st, rhs)
      
      let bound = if varName != "" {
        Map.update(~key=varName, ~f=_v => Some(E.toID(rhs)), st)
      } else {
        st
      }

      sexe(bound, body)

    | EFnCall(_, _, exprs, _) => List.forEach(~f=sexe(st), exprs)
    | EInfix(_, _, lhs, rhs) => List.forEach(~f=sexe(st), list{lhs, rhs})
    | EIf(_, cond, ifbody, elsebody)
    | EFeatureFlag(_, _, cond, elsebody, ifbody) =>
      sexe(st, cond)
      sexe(st, ifbody)
      sexe(st, elsebody)
    | ELambda(_, vars, body) =>
      let new_st =
        vars |> List.fold(~initial=st, ~f=(d, (id, varname)) =>
          Map.update(~key=varname, ~f=_v => Some(id), d)
        )

      sexe(new_st, body)
    | EPipe(_, e1, e2, rest) => List.forEach(~f=sexe(st), list{e1, e2, ...rest})
    | EFieldAccess(_, obj, _) => sexe(st, obj)
    | EList(_, exprs) => List.forEach(~f=sexe(st), exprs)
    | ETuple(_, first, second, theRest) =>
      List.forEach(~f=sexe(st), list{first, second, ...theRest})
    | EMatch(_, matchExpr, cases) =>
      let rec variablesInMP = p =>
        switch p {
        | MPInteger(_)
        | MPNull(_)
        | MPString(_)
        | MPCharacter(_)
        | MPFloat(_)
        | MPBool(_)
        | MPBlank(_) => list{}
        | MPVariable(patternID, v) => list{(patternID, v)}
        | MPConstructor(_, _, inner) => inner |> List.map(~f=variablesInMP) |> List.flatten
        | MPTuple(_, first, second, theRest) =>
          list{first, second, ...theRest} |> List.map(~f=variablesInMP) |> List.flatten
        }

      sexe(st, matchExpr)
      List.forEach(cases, ~f=((p, caseExpr)) => {
        let new_st =
          p
          |> variablesInMP
          |> List.fold(~initial=st, ~f=(d, (id, varname)) =>
            Map.update(~key=varname, ~f=_v => Some(id), d)
          )

        sexe(new_st, caseExpr)
      })
    | ERecord(_, exprs) => exprs |> List.map(~f=Tuple2.second) |> List.forEach(~f=sexe(st))
    | EConstructor(_, _, args) => List.forEach(~f=sexe(st), args)
    | EPartial(_, _, oldExpr) => sexe(st, oldExpr)
    | ERightPartial(_, _, oldExpr) => sexe(st, oldExpr)
    | ELeftPartial(_, _, oldExpr) => sexe(st, oldExpr)
    },
  )
  trace(expr, st)
}

@ocaml.doc(" [variablesIn ast] produces a map of every expression id in the [ast] to its corresponding symbol table.
 * Each symbol table maps from every available variable name to the id of the corresponding value expression bound to that name. ")
let variablesIn = (ast: E.t): AnalysisTypes.avDict => {
  let sym_store = IDTable.make(~id=module(IDComparable))
  let trace = (expr, st) => IDTable.set(sym_store, E.toID(expr), st)
  sym_exec(~trace, VarDict.empty, ast)
  sym_store |> IDTable.toList |> ID.Map.fromList
}

let removePartials = (expr: E.t): E.t =>
  E.postTraversal(expr, ~f=x =>
    switch x {
    | EPartial(_, _, e)
    | ERightPartial(_, _, e)
    | ELeftPartial(_, _, e)
    | e => e
    }
  )

@ocaml.doc(" Reorder function calls which call fnName, moving the argument at [oldPos] to [newPos],
 * pushing the element currently at [newPos] to [newPos+1]. It then handles situations
 * where the args may be in a different position due to pipes. ")
let rec reorderFnCallArgs = (fnName: FQFnName.t, oldPos: int, newPos: int, ast: E.t): E.t => {
  let rec replaceArgs = expr =>
    switch expr {
    | EFnCall(id, name, args, sendToRail) if name == fnName =>
      let newArgs = List.moveInto(~oldPos, ~newPos, args) |> List.map(~f=replaceArgs)

      EFnCall(id, name, newArgs, sendToRail)
    | EPipe(id, first, second, rest) =>
      let newFirst = reorderFnCallArgs(fnName, oldPos, newPos, first)
      let reorder = pipeArg =>
        /* If the pipetarget is involved, we're really going to have to wrap
         * it in a lambda instead of shifting things around (we could move
         * the argument up if it's the first thing being piped into, but that
         * might be ugly. */
        if oldPos === 0 || newPos === 0 {
          switch pipeArg {
          | EFnCall(fnID, name, list{_pipeTarget, ...args}, sendToRail) if name == fnName =>
            // We replace the pipeTarget with a variable in a lambda fn
            let newArg = EVariable(gid(), "x")
            let newArgs =
              List.moveInto(~oldPos, ~newPos, list{newArg, ...args}) |> List.map(~f=replaceArgs)

            /* The fncall is no longer a piped fn. # args shown == # params.
             * For example, if we moved the first param to last param:
             * Before: a |> someFun b c d
             * After:  a |> \x -> someFun b c d x
             */
            ELambda(gid(), list{(gid(), "x")}, EFnCall(fnID, name, newArgs, sendToRail))
          | ELambda(id, args, lambdaExpr) =>
            ELambda(id, args, reorderFnCallArgs(fnName, oldPos, newPos, lambdaExpr))
          | _ => reorderFnCallArgs(fnName, oldPos, newPos, pipeArg)
          }
        } else {
          // The pipetarget isn't involved, so just do it normally.
          reorderFnCallArgs(fnName, oldPos, newPos, pipeArg)
        }
      let newSecond = reorder(second)
      let newRest = List.map(rest, ~f=reorder)

      EPipe(id, newFirst, newSecond, newRest)
    | e => E.deprecatedWalk(~f=replaceArgs, e)
    }

  replaceArgs(ast)
}
