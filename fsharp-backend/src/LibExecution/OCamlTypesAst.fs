module LibExecution.OCamlTypesAst

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open OCamlTypes.RuntimeT

// Traverse is really only meant to be used by preTraversal and postTraversal
let traverse (f : fluidExpr -> fluidExpr) (expr : fluidExpr) : fluidExpr =
  match expr with
  | EInteger _
  | EBool _
  | EString _
  | ENull _
  | EBlank _
  | EVariable _
  | EPipeTarget _
  | EFloat _ -> expr
  | ELet (id, name, rhs, next) -> ELet(id, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) -> EIf(id, f cond, f ifexpr, f elseexpr)
  | EFieldAccess (id, expr, fieldname) -> EFieldAccess(id, f expr, fieldname)
  | EBinOp (id, name, left, right, ster) -> EBinOp(id, name, f left, f right, ster)
  | EPipe (id, exprs) -> EPipe(id, List.map f exprs)
  | EFnCall (id, name, exprs, ster) -> EFnCall(id, name, List.map f exprs, ster)
  | ELambda (id, names, expr) -> ELambda(id, names, f expr)
  | EList (id, exprs) -> EList(id, List.map f exprs)
  | ETuple (id, first, second, theRest) ->
    ETuple(id, f first, second, List.map f theRest)
  | EMatch (id, mexpr, pairs) ->
    EMatch(id, f mexpr, List.map (fun (name, expr) -> (name, f expr)) pairs)
  | ERecord (id, fields) ->
    ERecord(id, List.map (fun (name, expr) -> (name, f expr)) fields)
  | EConstructor (id, name, exprs) -> EConstructor(id, name, List.map f exprs)
  | EPartial (id, str, oldExpr) -> EPartial(id, str, f oldExpr)
  | ELeftPartial (id, str, oldExpr) -> ELeftPartial(id, str, f oldExpr)
  | ERightPartial (id, str, oldExpr) -> ERightPartial(id, str, f oldExpr)
  | EFeatureFlag (id, name, cond, casea, caseb) ->
    EFeatureFlag(id, name, f cond, f casea, f caseb)



let rec preTraversal (f : fluidExpr -> fluidExpr) (expr : fluidExpr) : fluidExpr =
  traverse (preTraversal f) (f expr)


let rec postTraversal (f : fluidExpr -> fluidExpr) (expr : fluidExpr) : fluidExpr =
  f (traverse (postTraversal f) expr)
