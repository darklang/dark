module LibExecution.ProgramTypesAst

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open ProgramTypes

// Traverse is really only meant to be used by preTraversal and postTraversal
let traverse (f : Expr -> Expr) (expr : Expr) : Expr =

  let traversePipeExpr (expr : PipeExpr) : PipeExpr =
    match expr with
    | EPipeFnCall(id, name, typeArgs, args) ->
      EPipeFnCall(id, name, typeArgs, List.map f args)
    | EPipeInfix(id, name, first) -> EPipeInfix(id, name, f first)
    | EPipeLambda(id, vars, body) -> EPipeLambda(id, vars, f body)
    | EPipeEnum(id, typeName, caseName, fields) ->
      EPipeEnum(id, typeName, caseName, List.map f fields)
    | EPipeVariable(id, name) -> EPipeVariable(id, name)
    | EPipeError(id, msg, exprs) -> EPipeError(id, msg, List.map f exprs)

  match expr with
  | EInt _
  | EBool _
  | EChar _
  | EUnit _
  | EVariable _
  | EFloat _ -> expr
  | ELet(id, pat, rhs, next) -> ELet(id, pat, f rhs, f next)
  | EString(id, strs) ->
    EString(
      id,
      strs
      |> List.map (fun s ->
        match s with
        | StringText t -> StringText t
        | StringInterpolation e -> StringInterpolation(f e))
    )
  | EIf(id, cond, ifexpr, elseexpr) -> EIf(id, f cond, f ifexpr, f elseexpr)
  | EFieldAccess(id, expr, fieldname) -> EFieldAccess(id, f expr, fieldname)
  | EInfix(id, op, left, right) -> EInfix(id, op, f left, f right)
  | EPipe(id, expr1, expr2, exprs) ->
    EPipe(id, f expr1, traversePipeExpr expr2, List.map traversePipeExpr exprs)
  | EApply(id, name, typeArgs, exprs) -> EApply(id, name, typeArgs, List.map f exprs)
  | ELambda(id, names, expr) -> ELambda(id, names, f expr)
  | EList(id, exprs) -> EList(id, List.map f exprs)
  | EDict(id, pairs) -> EDict(id, List.map (fun (k, v) -> (k, f v)) pairs)
  | ETuple(id, first, second, theRest) ->
    ETuple(id, f first, f second, List.map f theRest)
  | EMatch(id, mexpr, pairs) ->
    EMatch(id, f mexpr, List.map (fun (name, expr) -> (name, f expr)) pairs)
  | ERecord(id, typeName, fields) ->
    ERecord(id, typeName, List.map (fun (name, expr) -> (name, f expr)) fields)
  | ERecordUpdate(id, record, updates) ->
    ERecordUpdate(
      id,
      f record,
      List.map (fun (name, expr) -> (name, f expr)) updates
    )
  | EEnum(id, typeName, caseName, fields) ->
    EEnum(id, typeName, caseName, List.map f fields)
  | EError(id, msg, exprs) -> EError(id, msg, List.map f exprs)

let rec preTraversal
  (exprFn : Expr -> Expr)
  (exprPipeFn : PipeExpr -> PipeExpr)
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

  let f =
    preTraversal
      exprFn
      exprPipeFn
      typeRefFn
      fqtnFn
      fqfnFn
      letPatternFn
      matchPatternFn

  let rec preTraversalPipeExpr (expr : PipeExpr) : PipeExpr =
    match exprPipeFn expr with
    | EPipeFnCall(id, name, typeArgs, args) ->
      EPipeFnCall(
        id,
        fqfnFn name,
        List.map preTraversalTypeRef typeArgs,
        List.map f args
      )
    | EPipeInfix(id, name, first) -> EPipeInfix(id, name, f first)
    | EPipeLambda(id, vars, body) -> EPipeLambda(id, vars, f body)
    | EPipeEnum(id, typeName, caseName, fields) ->
      EPipeEnum(id, typeName, caseName, List.map f fields)
    | EPipeVariable(id, name) -> EPipeVariable(id, name)
    | EPipeError(id, msg, exprs) -> EPipeError(id, msg, List.map f exprs)

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
  | EInfix(id, op, left, right) -> EInfix(id, op, f left, f right)
  | EPipe(id, expr1, expr2, exprs) ->
    EPipe(
      id,
      f expr1,
      preTraversalPipeExpr expr2,
      List.map preTraversalPipeExpr exprs
    )
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
  | MPChar _
  | MPInt _
  | MPBool _
  | MPString _
  | MPUnit _
  | MPFloat _ -> pattern
  | MPEnum(patternID, caseName, fieldPats) ->
    MPEnum(patternID, caseName, List.map (fun p -> r p) fieldPats)
  | MPTuple(patternID, first, second, theRest) ->
    MPTuple(patternID, r first, r second, List.map r theRest)
  | MPList(patternID, pats) -> MPList(patternID, List.map r pats)
  | MPListCons(patternID, head, tail) -> MPListCons(patternID, r head, r tail)

let rec matchPatternPostTraversal
  (f : MatchPattern -> MatchPattern)
  (pattern : MatchPattern)
  : MatchPattern =
  let r = matchPatternPostTraversal f in
  let result =
    match pattern with
    | MPVariable _
    | MPChar _
    | MPInt _
    | MPBool _
    | MPString _
    | MPUnit _
    | MPFloat _ -> pattern
    | MPEnum(patternID, caseName, fieldPats) ->
      MPEnum(patternID, caseName, List.map r fieldPats)
    | MPTuple(patternID, first, second, theRest) ->
      MPTuple(patternID, r first, r second, List.map r theRest)
    | MPList(patternID, pats) -> MPList(patternID, List.map r pats)
    | MPListCons(patternID, head, tail) -> MPListCons(patternID, r head, r tail)
  f result
