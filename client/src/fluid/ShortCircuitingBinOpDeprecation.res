open Prelude
module TL = Toplevel
module E = ProgramTypes.Expr
module MP = FluidMatchPattern

let refactor = (_: AppTypes.model, tl: toplevel, id: id): AppTypes.modification => {
  let replaceBinOp = ((ast: FluidAST.t, expr: E.t)): option<AppTypes.modification> => {
    let newExpr = switch expr {
    | E.EInfix(_, InfixFnCall({module_: None, function: "&&"}, _), lhs, rhs) =>
      Some(E.EInfix(gid(), BinOp(E.BinaryOperation.BinOpAnd), lhs, rhs))
    | E.EInfix(_, InfixFnCall({module_: None, function: "||"}, _), lhs, rhs) =>
      Some(E.EInfix(gid(), BinOp(E.BinaryOperation.BinOpOr), lhs, rhs))
    | _ => None
    }

    newExpr |> Option.map(~f=newExpr =>
      FluidAST.replace(~replacement=newExpr, FluidExpression.toID(expr), ast) |> TL.setASTMod(tl)
    )
  }

  TL.getAST(tl)
  |> Option.thenAlso(~f=ast => FluidAST.findExpr(id, ast))
  |> Option.andThen(~f=replaceBinOp)
  |> Option.unwrap(~default=AppTypes.Modification.NoChange)
}

let shouldShow = (e: E.t): bool => {
  switch e {
  | EInfix(_, InfixFnCall({module_: None, function: "&&"}, _), _, _)
  | EInfix(_, InfixFnCall({module_: None, function: "||"}, _), _, _) => true
  | _ => false
  }
}
