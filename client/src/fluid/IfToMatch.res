open Prelude
module TL = Toplevel
module E = ProgramTypes.Expr
module P = FluidPattern

let findIf = (ast: FluidAST.t, e: E.t): option<E.t> =>
  switch e {
  | EIf(_) => Some(e)
  | _ =>
    FluidAST.ancestors(FluidExpression.toID(e), ast) |> List.find(~f=x =>
      switch x {
      | E.EIf(_) => true
      | _ => false
      }
    )
  }

let refactor = (_: model, tl: toplevel, id: id): modification => {
  let makeGenericMatch = (ifID, cond, then_, else_) => E.EMatch(
    ifID,
    cond,
    list{(PBool(gid(), true), then_), (PBool(gid(), false), else_)},
  )
  let binOpName = (function: string): PT.FQFnName.InfixStdlibFnName.t => {
    function: function,
    module_: None,
  }

  let makeBinOpMatch = (ifID, binopID, lhs, rhs, rail, then_, else_) => {
    // We need to make sure that whichever side we choose for the match condition,
    // we should be able to turn the other side into a pattern. So we try to make smart
    // decision whether to choose the lhs or rhs here. We default to the eft hand side,
    // except when there's something on the rhs which cannot be turned into a pattern.
    let (matchCond, arm) = switch rhs {
    | E.ELet(_)
    | E.EIf(_)
    | E.EBinOp(_)
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

    let pattern: option<P.t> = switch arm {
    | EInteger(pid, value) => Some(PInteger(pid, value))
    | EBool(pid, value) => Some(PBool(pid, value))
    | EString(pid, string) => Some(PString(pid, string))
    | EFloat(pid, sign, whole, frac) => Some(PFloat(pid, sign, whole, frac))
    | ENull(pid) => Some(PNull(pid))
    | EBlank(pid) => Some(PBlank(pid))
    | EVariable(pid, name) => Some(PVariable(pid, name))
    | _ => None
    }

    // If we were unable to convert the other side to a pattern, fall back to a
    // generic match expression with true and false arms.
    switch pattern {
    | Some(p) => E.EMatch(ifID, matchCond, list{(p, then_), (PVariable(gid(), "_"), else_)})
    | None => makeGenericMatch(ifID, EBinOp(binopID, binOpName("=="), lhs, rhs, rail), then_, else_)
    }
  }

  let replaceIf = ((ast: FluidAST.t, ifexpr: E.t)): option<modification> => {
    let ifExprToMatchExpr: option<FluidExpression.t> = switch ifexpr {
    | E.EIf(ifID, EBinOp(binopID, {module_: None, function: "=="}, lhs, rhs, rail), then_, else_) =>
      Some(makeBinOpMatch(ifID, binopID, lhs, rhs, rail, then_, else_))
    | E.EIf(ifID, EBinOp(binopID, {module_: None, function: "!="}, lhs, rhs, rail), then_, else_) =>
      Some(makeBinOpMatch(ifID, binopID, lhs, rhs, rail, else_, then_))
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
  |> Option.thenAlso(~f=ast => FluidAST.find(id, ast) |> Option.andThen(~f=findIf(ast)))
  |> Option.andThen(~f=replaceIf)
  |> Option.unwrap(~default=NoChange)
}
