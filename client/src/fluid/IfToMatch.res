open Prelude
module TL = Toplevel
module E = ProgramTypes.Expr
module MP = FluidMatchPattern

let findIf = (ast: FluidAST.t, e: E.t): option<E.t> =>
  switch e {
  | EIf(_) => Some(e)
  | _ =>
    FluidAST.exprAncestors(FluidExpression.toID(e), ast) |> List.find(~f=x =>
      switch x {
      | E.EIf(_) => true
      | _ => false
      }
    )
  }

let refactor = (_: AppTypes.model, tl: toplevel, id: id): AppTypes.modification => {
  let makeGenericMatch = (ifID, cond, then_, else_) => E.EMatch(
    ifID,
    cond,
    list{(MPBool(gid(), true), then_), (MPBool(gid(), false), else_)},
  )
  let binOpName = (function: string): PT.InfixStdlibFnName.t => {
    function: function,
    module_: None,
  }

  let makeInfixMatch = (ifID, binopID, lhs, rhs, rail, then_, else_) => {
    // We need to make sure that whichever side we choose for the match condition,
    // we should be able to turn the other side into a pattern. So we try to make smart
    // decision whether to choose the lhs or rhs here. We default to the eft hand side,
    // except when there's something on the rhs which cannot be turned into a pattern.
    let (matchCond, arm) = switch rhs {
    | E.ELet(_)
    | E.EIf(_)
    | E.EInfix(_)
    | E.ELambda(_)
    | E.EFieldAccess(_)
    | E.EFnCall(_)
    | E.EPartial(_)
    | E.ERightPartial(_)
    | E.ELeftPartial(_)
    | E.EList(_)
    | E.ETuple(_)
    | E.ERecord(_)
    | E.EPipe(_)
    | /* Constructor could be possible, but subexpressions would need to be
     * converted to subpatterns.
     */
    E.EConstructor(_)
    | E.EMatch(_)
    | E.EPipeTarget(_)
    | E.EFeatureFlag(_) => (rhs, lhs)
    | _ => (lhs, rhs)
    }

    let matchPattern: option<MP.t> = switch arm {
    | EInteger(pid, value) => Some(MPInteger(pid, value))
    | EBool(pid, value) => Some(MPBool(pid, value))
    | EString(pid, string) => Some(MPString(pid, string))
    | ECharacter(pid, string) => Some(MPCharacter(pid, string))
    | EFloat(pid, sign, whole, frac) => Some(MPFloat(pid, sign, whole, frac))
    | ENull(pid) => Some(MPNull(pid))
    | EBlank(pid) => Some(MPBlank(pid))
    | EVariable(pid, name) => Some(MPVariable(pid, name))
    | _ => None
    }

    // If we were unable to convert the other side to a pattern, fall back to a
    // generic match expression with true and false arms.
    switch matchPattern {
    | Some(p) => E.EMatch(ifID, matchCond, list{(p, then_), (MPVariable(gid(), "_"), else_)})
    | None =>
      makeGenericMatch(
        ifID,
        EInfix(binopID, InfixFnCall(binOpName("=="), rail), lhs, rhs),
        then_,
        else_,
      )
    }
  }

  let replaceIf = ((ast: FluidAST.t, ifexpr: E.t)): option<AppTypes.modification> => {
    let ifExprToMatchExpr: option<FluidExpression.t> = switch ifexpr {
    | E.EIf(
        ifID,
        EInfix(binopID, InfixFnCall({module_: None, function: "=="}, rail), lhs, rhs),
        then_,
        else_,
      ) =>
      Some(makeInfixMatch(ifID, binopID, lhs, rhs, rail, then_, else_))
    | E.EIf(
        ifID,
        EInfix(binopID, InfixFnCall({module_: None, function: "!="}, rail), lhs, rhs),
        then_,
        else_,
      ) =>
      Some(makeInfixMatch(ifID, binopID, lhs, rhs, rail, else_, then_))
    | E.EIf(ifID, cond, then_, else_) => Some(makeGenericMatch(ifID, cond, then_, else_))
    | _ => None
    }

    ifExprToMatchExpr |> Option.map(~f=matchExpr =>
      FluidAST.replace(~replacement=matchExpr, FluidExpression.toID(ifexpr), ast) |> TL.setASTMod(
        tl,
      )
    )
  }

  TL.getAST(tl)
  |> Option.thenAlso(~f=ast => FluidAST.findExpr(id, ast) |> Option.andThen(~f=findIf(ast)))
  |> Option.andThen(~f=replaceIf)
  |> Option.unwrap(~default=AppTypes.Modification.NoChange)
}
