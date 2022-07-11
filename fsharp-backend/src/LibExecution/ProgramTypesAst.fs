module LibExecution.ProgramTypesAst

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open ProgramTypes

// Traverse is really only meant to be used by preTraversal and postTraversal
let traverse (f : Expr -> Expr) (expr : Expr) : Expr =
  match expr with
  | EInteger _
  | EBool _
  | EString _
  | ECharacter _
  | ENull _
  | EBlank _
  | EVariable _
  | EPipeTarget _
  | EFloat _ -> expr
  | ELet (id, name, rhs, next) -> ELet(id, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) -> EIf(id, f cond, f ifexpr, f elseexpr)
  | EFieldAccess (id, expr, fieldname) -> EFieldAccess(id, f expr, fieldname)
  | EBinOp (id, name, left, right, ster) -> EBinOp(id, name, f left, f right, ster)
  | EPipe (id, expr1, expr2, exprs) -> EPipe(id, f expr1, f expr2, List.map f exprs)
  | EFnCall (id, name, exprs, ster) -> EFnCall(id, name, List.map f exprs, ster)
  | ELambda (id, names, expr) -> ELambda(id, names, f expr)
  | EList (id, exprs) -> EList(id, List.map f exprs)
  | ETuple (id, first, second, theRest) ->
    ETuple(id, f first, f second, List.map f theRest)
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



let rec preTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = preTraversal f in
  let expr = f expr in
  traverse r expr


let rec postTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = postTraversal f in
  let result = traverse r expr
  f result

let rec patternPreTraversal (f : Pattern -> Pattern) (pattern : Pattern) : Pattern =
  let r = patternPreTraversal f in
  let pattern = f pattern in
  match pattern with
  | PVariable _
  | PCharacter _
  | PInteger _
  | PBool _
  | PString _
  | PBlank _
  | PNull _
  | PFloat _ -> pattern
  | PConstructor (patternID, name, patterns) ->
    PConstructor(patternID, name, List.map (fun p -> r p) patterns)


let rec patternPostTraversal (f : Pattern -> Pattern) (pattern : Pattern) : Pattern =
  let r = patternPostTraversal f in
  let result =
    match pattern with
    | PVariable _
    | PCharacter _
    | PInteger _
    | PBool _
    | PString _
    | PBlank _
    | PNull _
    | PFloat _ -> pattern
    | PConstructor (patternID, name, patterns) ->
      PConstructor(patternID, name, List.map (fun p -> r p) patterns)
  f result
