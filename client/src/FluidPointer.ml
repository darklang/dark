open Types

(* Dark *)
module B = Blank

(* ------------------------ *)
(* PointerData *)
(* ------------------------ *)

let typeOf (pd : astData) : astType =
  match pd with
  | PVarBind _ ->
      VarBind
  | PExpr _ ->
      Expr
  | PField _ ->
      Field
  | PKey _ ->
      Key
  | PFFMsg _ ->
      FFMsg
  | PFnCallName _ ->
      FnCallName
  | PConstructorName _ ->
      ConstructorName
  | PPattern _ ->
      Pattern


let toID (pd : astData) : id =
  match pd with
  | PVarBind (id, _) ->
      id
  | PField (id, _) ->
      id
  | PKey (id, _) ->
      id
  | PExpr e ->
      FluidExpression.id e
  | PFFMsg (id, _) ->
      id
  | PFnCallName (id, _) ->
      id
  | PPattern p ->
      FluidPattern.id p
  | PConstructorName (id, _) ->
      id


let isBlank (pd : astData) : bool =
  match pd with
  | PVarBind (_, "")
  | PField (_, "")
  | PKey (_, "")
  | PFFMsg (_, "")
  | PFnCallName (_, "")
  | PConstructorName (_, "")
  | PExpr (EBlank _)
  | PPattern (FPBlank _) ->
      true
  | PVarBind _
  | PField _
  | PKey _
  | PFFMsg _
  | PFnCallName _
  | PConstructorName _
  | PExpr _
  | PPattern _ ->
      false
