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
  | EUnit _
  | EVariable _
  | EPipeTarget _
  | EFloat _ -> expr
  | ELet (id, pat, rhs, next) -> ELet(id, pat, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) -> EIf(id, f cond, f ifexpr, f elseexpr)
  | EFieldAccess (id, expr, fieldname) -> EFieldAccess(id, f expr, fieldname)
  | EInfix (id, op, left, right) -> EInfix(id, op, f left, f right)
  | EPipe (id, expr1, expr2, exprs) -> EPipe(id, f expr1, f expr2, List.map f exprs)
  | EFnCall (id, name, exprs) -> EFnCall(id, name, List.map f exprs)
  | ELambda (id, names, expr) -> ELambda(id, names, f expr)
  | EList (id, exprs) -> EList(id, List.map f exprs)
  | ETuple (id, first, second, theRest) ->
    ETuple(id, f first, f second, List.map f theRest)
  | EMatch (id, mexpr, pairs) ->
    EMatch(id, f mexpr, List.map (fun (name, expr) -> (name, f expr)) pairs)
  | ERecord (id, fields) ->
    ERecord(id, List.map (fun (name, expr) -> (name, f expr)) fields)
  | EFeatureFlag (id, name, cond, casea, caseb) ->
    EFeatureFlag(id, name, f cond, f casea, f caseb)
  | EConstructor (id, typeName, caseName, fields) ->
    EConstructor(id, typeName, caseName, List.map f fields)



let rec preTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = preTraversal f in
  let expr = f expr in
  traverse r expr


let rec postTraversal (f : Expr -> Expr) (expr : Expr) : Expr =
  let r = postTraversal f in
  let result = traverse r expr
  f result

let rec matchPatternPreTraversal
  (f : MatchPattern -> MatchPattern)
  (pattern : MatchPattern)
  : MatchPattern =
  let r = matchPatternPreTraversal f in
  let pattern = f pattern in
  match pattern with
  | MPVariable _
  | MPCharacter _
  | MPInteger _
  | MPBool _
  | MPString _
  | MPUnit _
  | MPFloat _ -> pattern
  | MPConstructor (patternID, name, patterns) ->
    MPConstructor(patternID, name, List.map (fun p -> r p) patterns)
  | MPTuple (patternID, first, second, theRest) ->
    MPTuple(patternID, r first, r second, List.map r theRest)


let rec matchPatternPostTraversal
  (f : MatchPattern -> MatchPattern)
  (pattern : MatchPattern)
  : MatchPattern =
  let r = matchPatternPostTraversal f in
  let result =
    match pattern with
    | MPVariable _
    | MPCharacter _
    | MPInteger _
    | MPBool _
    | MPString _
    | MPUnit _
    | MPFloat _ -> pattern
    | MPConstructor (patternID, name, patterns) ->
      MPConstructor(patternID, name, List.map r patterns)
    | MPTuple (patternID, first, second, theRest) ->
      MPTuple(patternID, r first, r second, List.map r theRest)
  f result
