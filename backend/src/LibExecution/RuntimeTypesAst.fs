/// Module to work with Runtime ASTs
module LibExecution.RuntimeTypesAst

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open VendoredTablecloth
open RuntimeTypes

let rec preTraversal
  (exprFn : Expr -> Expr)
  (typeRefFn : TypeReference -> TypeReference)
  (fqtnFn : TypeName.T -> TypeName.T)
  (fqfnFn : FnName.T -> FnName.T)
  (letPatternFn : LetPattern -> LetPattern)
  (matchPatternFn : MatchPattern -> MatchPattern)
  (expr : Expr)
  : Expr =

  let rec preTraversalLetPattern (pat : LetPattern) : LetPattern =
    let f = preTraversalLetPattern
    match letPatternFn pat with
    | LPVariable _ -> letPatternFn pat
    | LPTuple(id, p1, p2, pats) -> LPTuple(id, f p1, f p2, List.map f pats)

  let rec preTraverseMatchPattern (pat : MatchPattern) : MatchPattern =
    let f = preTraverseMatchPattern
    match matchPatternFn pat with
    | MPVariable _
    | MPInt _
    | MPBool _
    | MPString _
    | MPChar _
    | MPFloat _
    | MPUnit _ -> pat
    | MPList(id, pats) -> MPList(id, List.map f pats)
    | MPTuple(id, p1, p2, pats) -> MPTuple(id, f p1, f p2, List.map f pats)
    | MPEnum(id, name, pats) -> MPEnum(id, name, List.map f pats)
    | MPListCons(id, head, tail) -> MPListCons(id, f head, f tail)

  let rec preTraversalTypeRef (typeRef : TypeReference) : TypeReference =
    let f = preTraversalTypeRef
    match typeRefFn typeRef with
    | TInt
    | TBool
    | TUnit
    | TFloat
    | TChar
    | TUuid
    | TDateTime
    | TBytes
    | TPassword
    | TVariable _
    | TString -> typeRef
    | TList tr -> TList(f tr)
    | TTuple(tr1, tr2, trs) -> TTuple(f tr1, f tr2, List.map f trs)
    | TDB tr -> TDB(f tr)
    | TCustomType(name, trs) -> TCustomType(fqtnFn name, List.map f trs)
    | TDict(tr) -> TDict(f tr)
    | TFn(trs, tr) -> TFn(List.map f trs, f tr)

  let f = preTraversal exprFn typeRefFn fqtnFn fqfnFn letPatternFn matchPatternFn

  match exprFn expr with
  | EInt _
  | EBool _
  | EChar _
  | EUnit _
  | EVariable _
  | EFloat _ -> expr
  | EString(id, strs) ->
    EString(
      id,
      strs
      |> List.map (fun s ->
        match s with
        | StringText t -> StringText t
        | StringInterpolation e -> StringInterpolation(f e))
    )
  | ELet(id, pat, rhs, next) -> ELet(id, preTraversalLetPattern pat, f rhs, f next)
  | EIf(id, cond, ifexpr, elseexpr) -> EIf(id, f cond, f ifexpr, f elseexpr)
  | EFieldAccess(id, expr, fieldname) -> EFieldAccess(id, f expr, fieldname)
  | EApply(id, FnTargetName name, typeArgs, args) ->
    EApply(
      id,
      FnTargetName(fqfnFn name),
      List.map preTraversalTypeRef typeArgs,
      List.map f args
    )
  | EApply(id, FnTargetExpr name, typeArgs, args) ->
    EApply(
      id,
      FnTargetExpr(f name),
      List.map preTraversalTypeRef typeArgs,
      List.map f args
    )
  | EAnd(id, left, right) -> EAnd(id, f left, f right)
  | EOr(id, left, right) -> EOr(id, f left, f right)
  | ELambda(id, names, expr) -> ELambda(id, names, f expr)
  | EList(id, exprs) -> EList(id, List.map f exprs)
  | EDict(id, pairs) -> EDict(id, List.map (fun (k, v) -> (k, f v)) pairs)
  | ETuple(id, first, second, theRest) ->
    ETuple(id, f first, f second, List.map f theRest)
  | EEnum(id, typeName, caseName, fields) ->
    EEnum(id, fqtnFn typeName, caseName, List.map f fields)
  | EMatch(id, mexpr, pairs) ->
    EMatch(
      id,
      f mexpr,
      List.map
        (fun (pattern, expr) -> (preTraverseMatchPattern pattern, f expr))
        pairs
    )
  | ERecord(id, typeName, fields) ->
    ERecord(
      id,
      fqtnFn typeName,
      List.map (fun (name, expr) -> (name, f expr)) fields
    )
  | ERecordUpdate(id, record, updates) ->
    ERecordUpdate(
      id,
      f record,
      List.map (fun (name, expr) -> (name, f expr)) updates
    )
  | EError(id, msg, exprs) -> EError(id, msg, List.map f exprs)

let rec postTraversal
  (exprFn : Expr -> Expr)
  (typeRefFn : TypeReference -> TypeReference)
  (fqtnFn : TypeName.T -> TypeName.T)
  (fqfnFn : FnName.T -> FnName.T)
  (letPatternFn : LetPattern -> LetPattern)
  (matchPatternFn : MatchPattern -> MatchPattern)
  (expr : Expr)
  : Expr =

  let rec postTraversalLetPattern (pat : LetPattern) : LetPattern =
    let f = postTraversalLetPattern
    match letPatternFn pat with
    | LPVariable _ -> letPatternFn pat
    | LPTuple(id, p1, p2, pats) -> LPTuple(id, f p1, f p2, List.map f pats)

  let rec postTraverseMatchPattern (pat : MatchPattern) : MatchPattern =
    let f = postTraverseMatchPattern
    match matchPatternFn pat with
    | MPVariable _
    | MPInt _
    | MPBool _
    | MPString _
    | MPChar _
    | MPFloat _
    | MPUnit _ -> pat
    | MPList(id, pats) -> MPList(id, List.map f pats)
    | MPTuple(id, p1, p2, pats) -> MPTuple(id, f p1, f p2, List.map f pats)
    | MPEnum(id, name, pats) -> MPEnum(id, name, List.map f pats)
    | MPListCons(id, head, tail) -> MPListCons(id, f head, f tail)

  let rec postTraversalTypeRef (typeRef : TypeReference) : TypeReference =
    let f = postTraversalTypeRef
    match typeRefFn typeRef with
    | TInt
    | TBool
    | TUnit
    | TFloat
    | TChar
    | TUuid
    | TDateTime
    | TBytes
    | TPassword
    | TVariable _
    | TString -> typeRef
    | TList tr -> TList(f tr)
    | TTuple(tr1, tr2, trs) -> TTuple(f tr1, f tr2, List.map f trs)
    | TDB tr -> TDB(f tr)
    | TCustomType(name, trs) -> TCustomType(fqtnFn name, List.map f trs)
    | TDict(tr) -> TDict(f tr)
    | TFn(trs, tr) -> TFn(List.map f trs, f tr)

  let f = postTraversal exprFn typeRefFn fqtnFn fqfnFn letPatternFn matchPatternFn
  (match expr with
   | EInt _
   | EBool _
   | EChar _
   | EUnit _
   | EVariable _
   | EFloat _ -> expr
   | EString(id, strs) ->
     EString(
       id,
       strs
       |> List.map (fun s ->
         match s with
         | StringText t -> StringText t
         | StringInterpolation e -> StringInterpolation(f e))
     )
   | ELet(id, pat, rhs, next) -> ELet(id, postTraversalLetPattern pat, f rhs, f next)
   | EIf(id, cond, ifexpr, elseexpr) -> EIf(id, f cond, f ifexpr, f elseexpr)
   | EFieldAccess(id, expr, fieldname) -> EFieldAccess(id, f expr, fieldname)
   | EApply(id, FnTargetName name, typeArgs, args) ->
     EApply(
       id,
       FnTargetName(fqfnFn name),
       List.map postTraversalTypeRef typeArgs,
       List.map f args
     )
   | EApply(id, FnTargetExpr name, typeArgs, args) ->

     EApply(
       id,
       FnTargetExpr(f name),
       List.map postTraversalTypeRef typeArgs,
       List.map f args
     )
   | EAnd(id, left, right) -> EAnd(id, f left, f right)
   | EOr(id, left, right) -> EOr(id, f left, f right)
   | ELambda(id, names, expr) -> ELambda(id, names, f expr)
   | EList(id, exprs) -> EList(id, List.map f exprs)
   | EDict(id, pairs) -> EDict(id, List.map (fun (k, v) -> (k, f v)) pairs)
   | ETuple(id, first, second, theRest) ->
     ETuple(id, f first, f second, List.map f theRest)
   | EEnum(id, typeName, caseName, fields) ->

     EEnum(id, fqtnFn typeName, caseName, List.map f fields)
   | EMatch(id, mexpr, pairs) ->
     EMatch(
       id,
       f mexpr,
       List.map
         (fun (pattern, expr) -> (postTraverseMatchPattern pattern, f expr))
         pairs
     )
   | ERecord(id, typeName, fields) ->
     ERecord(
       id,
       fqtnFn typeName,
       List.map (fun (name, expr) -> (name, f expr)) fields
     )
   | ERecordUpdate(id, record, updates) ->
     ERecordUpdate(
       id,
       f record,
       List.map (fun (name, expr) -> (name, f expr)) updates
     )
   | EError(id, msg, exprs) -> EError(id, msg, List.map f exprs))
  |> exprFn
