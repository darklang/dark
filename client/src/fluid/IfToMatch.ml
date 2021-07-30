open Prelude
module TL = Toplevel
module E = FluidExpression
module P = FluidPattern

let findIf (ast : FluidAST.t) (e : E.t) : E.t option =
  match e with
  | EIf _ ->
      Some e
  | _ ->
      FluidAST.ancestors (E.toID e) ast
      |> List.find ~f:(function FluidExpression.EIf _ -> true | _ -> false)


let refactor (_ : model) (tl : toplevel) (id : ID.t) : modification =
  let makeGenericMatch ifID cond then_ else_ =
    E.EMatch
      ( ifID
      , cond
      , [ (FPBool (ifID, gid (), true), then_)
        ; (FPBool (ifID, gid (), false), else_) ] )
  in
  let makeBinOpMatch ifID binopID lhs rhs rail then_ else_ =
    (* We need to make sure that whichever side we choose for the match condition,
     * we should be able to turn the other side into a pattern. So we try to make smart
     * decision whether to choose the lhs or rhs here. We default to the eft hand side,
     * except when there's something on the rhs which cannot be turned into a pattern.
     *)
    let matchCond, arm =
      match rhs with
      | E.ELet _
      | E.EIf _
      | E.EBinOp _
      | E.ELambda _
      | E.EFieldAccess _
      | E.EFnCall _
      | E.EPartial _
      | E.ERightPartial _
      | E.ELeftPartial _
      | E.EList _
      | E.ERecord _
      | E.EPipe _
      (* Constructor could be possible, but subexpressions would need to be
       * converted to subpatterns.
       *)
      | E.EConstructor _
      | E.EMatch _
      | E.EPipeTarget _
      | E.EFeatureFlag _ ->
          (rhs, lhs)
      | _ ->
          (lhs, rhs)
    in
    let pattern : P.t option =
      match arm with
      | EInteger (pid, value) ->
          Some (FPInteger (ifID, pid, value))
      | EBool (pid, value) ->
          Some (FPBool (ifID, pid, value))
      | EString (pid, string) ->
          Some (FPString {matchID = ifID; patternID = pid; str = string})
      | EFloat (pid, whole, frac) ->
          Some (FPFloat (ifID, pid, whole, frac))
      | ENull pid ->
          Some (FPNull (ifID, pid))
      | EBlank pid ->
          Some (FPBlank (ifID, pid))
      | EVariable (pid, name) ->
          Some (FPVariable (ifID, pid, name))
      | _ ->
          None
    in
    (* If we were unable to convert the other side to a pattern, fall back to a
     * generic match expression with true and false arms.
     *)
    match pattern with
    | Some p ->
        E.EMatch
          ( ifID
          , matchCond
          , [(p, then_); (FPVariable (ifID, gid (), "_"), else_)] )
    | None ->
        makeGenericMatch
          ifID
          (EBinOp (binopID, "==", lhs, rhs, rail))
          then_
          else_
  in
  let replaceIf ((ast : FluidAST.t), (ifexpr : E.t)) : modification option =
    let ifExprToMatchExpr : FluidExpression.t option =
      match ifexpr with
      | E.EIf (ifID, EBinOp (binopID, "==", lhs, rhs, rail), then_, else_) ->
          Some (makeBinOpMatch ifID binopID lhs rhs rail then_ else_)
      | E.EIf (ifID, EBinOp (binopID, "!=", lhs, rhs, rail), then_, else_) ->
          Some (makeBinOpMatch ifID binopID lhs rhs rail else_ then_)
      | E.EIf (ifID, cond, then_, else_) ->
          Some (makeGenericMatch ifID cond then_ else_)
      | _ ->
          None
    in
    ifExprToMatchExpr
    |> Option.map ~f:(fun matchExpr ->
           FluidAST.replace ~replacement:matchExpr (E.toID ifexpr) ast
           |> TL.setASTMod tl)
  in
  TL.getAST tl
  |> Option.thenAlso ~f:(fun ast ->
         FluidAST.find id ast |> Option.andThen ~f:(findIf ast))
  |> Option.andThen ~f:replaceIf
  |> Option.unwrap ~default:NoChange
