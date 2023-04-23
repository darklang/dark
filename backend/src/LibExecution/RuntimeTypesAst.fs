/// Module to work with Runtime ASTs
module LibExecution.RuntimeTypesAst

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth
open RuntimeTypes

let rec preTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = preTraversal f
  let expr = f expr

  match expr with
  | EInt _
  | EString _
  | EVariable _
  | EBool _
  | EUnit _
  | EChar _
  | EFloat _ -> expr
  | ELet (id, pat, rhs, next) -> ELet(id, pat, r rhs, r next)
  | EIf (id, cond, ifexpr, elseexpr) -> EIf(id, r cond, r ifexpr, r elseexpr)
  | EFieldAccess (id, expr, fieldname) -> EFieldAccess(id, r expr, fieldname)
  | EApply (id, fn, typeArgs, exprs) -> EApply(id, fn, typeArgs, List.map r exprs)
  | ELambda (id, names, expr) -> ELambda(id, names, r expr)
  | EList (id, exprs) -> EList(id, List.map r exprs)
  | ETuple (id, first, second, theRest) ->
    ETuple(id, r first, r second, List.map r theRest)
  | EMatch (id, mexpr, pairs) ->
    EMatch(id, r mexpr, List.map (fun (name, expr) -> (name, r expr)) pairs)
  | ERecord (id, typeName, fields) ->
    ERecord(id, typeName, List.map (fun (name, expr) -> (name, r expr)) fields)
  | EAnd (id, left, right) -> EAnd(id, r left, r right)
  | EOr (id, left, right) -> EOr(id, r left, r right)
  | EConstructor (id, typeName, caseName, fields) ->
    EConstructor(id, typeName, caseName, List.map r fields)
  | EDict (id, fields) -> EDict(id, List.map (fun (k, v) -> (k, r v)) fields)

let rec postTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = postTraversal f

  let result =
    match expr with
    | EInt _
    | EString _
    | EVariable _
    | EChar _
    | EBool _
    | EUnit _
    | EFloat _ -> expr
    | ELet (id, pat, rhs, next) -> ELet(id, pat, r rhs, r next)
    | EApply (id, fn, typeArgs, exprs) -> EApply(id, fn, typeArgs, List.map r exprs)
    | EIf (id, cond, ifexpr, elseexpr) -> EIf(id, r cond, r ifexpr, r elseexpr)
    | EFieldAccess (id, expr, fieldname) -> EFieldAccess(id, r expr, fieldname)
    | ELambda (id, names, expr) -> ELambda(id, names, r expr)
    | EList (id, exprs) -> EList(id, List.map r exprs)
    | ETuple (id, first, second, theRest) ->
      ETuple(id, r first, r second, List.map r theRest)
    | EMatch (id, mexpr, pairs) ->
      EMatch(id, r mexpr, List.map (fun (name, expr) -> (name, r expr)) pairs)
    | ERecord (id, typeName, fields) ->
      ERecord(id, typeName, List.map (fun (name, expr) -> (name, r expr)) fields)
    | EAnd (id, left, right) -> EAnd(id, r left, r right)
    | EOr (id, left, right) -> EOr(id, r left, r right)
    | EConstructor (id, typeName, caseName, fields) ->
      EConstructor(id, typeName, caseName, List.map r fields)
    | EDict (id, fields) -> EDict(id, List.map (fun (k, v) -> (k, r v)) fields)


  f result
