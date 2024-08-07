/// Module to work with Runtime ASTs
module LibExecution.RuntimeTypesAst

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open RuntimeTypes

let rec preTraversal
  (exprFn : Expr -> Expr)
  (typeRefFn : TypeReference -> TypeReference)
  (fqtnFn : FQTypeName.FQTypeName -> FQTypeName.FQTypeName)
  (fqfnFn : FQFnName.FQFnName -> FQFnName.FQFnName)
  (fqcnFn : FQConstantName.FQConstantName -> FQConstantName.FQConstantName)
  (letPatternFn : LetPattern -> LetPattern)
  (matchPatternFn : MatchPattern -> MatchPattern)
  (expr : Expr)
  : Expr =

  let rec preTraversalLetPattern (pat : LetPattern) : LetPattern =
    let f = preTraversalLetPattern
    match letPatternFn pat with
    | LPVariable _
    | LPUnit _ -> letPatternFn pat
    | LPTuple(id, p1, p2, pats) -> LPTuple(id, f p1, f p2, List.map f pats)

  let rec preTraverseMatchPattern (pat : MatchPattern) : MatchPattern =
    let f = preTraverseMatchPattern
    match matchPatternFn pat with
    | MPVariable _
    | MPInt64 _
    | MPUInt64 _
    | MPInt8 _
    | MPUInt8 _
    | MPInt16 _
    | MPUInt16 _
    | MPInt32 _
    | MPUInt32 _
    | MPInt128 _
    | MPUInt128 _
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
    | TInt64
    | TUInt64
    | TInt8
    | TUInt8
    | TInt16
    | TUInt16
    | TInt32
    | TUInt32
    | TInt128
    | TUInt128
    | TBool
    | TUnit
    | TFloat
    | TChar
    | TUuid
    | TDateTime
    | TVariable _
    | TString -> typeRef
    | TList tr -> TList(f tr)
    | TTuple(tr1, tr2, trs) -> TTuple(f tr1, f tr2, List.map f trs)
    | TDB tr -> TDB(f tr)
    | TCustomType(name, trs) -> TCustomType(Result.map fqtnFn name, List.map f trs)
    | TDict(tr) -> TDict(f tr)
    | TFn(trs, tr) -> TFn(NEList.map f trs, f tr)

  let f =
    preTraversal exprFn typeRefFn fqtnFn fqfnFn fqcnFn letPatternFn matchPatternFn

  match exprFn expr with
  | EInt64 _
  | EUInt64 _
  | EInt8 _
  | EUInt8 _
  | EInt16 _
  | EUInt16 _
  | EInt32 _
  | EUInt32 _
  | EInt128 _
  | EUInt128 _
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
  | EConstant(id, name) -> EConstant(id, fqcnFn name)
  | ELet(id, pat, rhs, next) -> ELet(id, preTraversalLetPattern pat, f rhs, f next)
  | EIf(id, cond, ifexpr, elseexpr) ->
    EIf(id, f cond, f ifexpr, Option.map f elseexpr)
  | ERecordFieldAccess(id, expr, fieldname) ->
    ERecordFieldAccess(id, f expr, fieldname)
  | EApply(id, name, typeArgs, args) ->
    EApply(id, f name, List.map preTraversalTypeRef typeArgs, NEList.map f args)
  | EFnName(id, name) -> EFnName(id, fqfnFn name)
  | EAnd(id, left, right) -> EAnd(id, f left, f right)
  | EOr(id, left, right) -> EOr(id, f left, f right)
  | ELambda(id, names, expr) -> ELambda(id, names, f expr)
  | EList(id, exprs) -> EList(id, List.map f exprs)
  | EDict(id, pairs) -> EDict(id, List.map (fun (k, v) -> (k, f v)) pairs)
  | ETuple(id, first, second, theRest) ->
    ETuple(id, f first, f second, List.map f theRest)
  | EEnum(id, typeName, caseName, fields) ->
    EEnum(id, fqtnFn typeName, caseName, List.map f fields)
  | EMatch(id, mexpr, cases) ->
    EMatch(
      id,
      f mexpr,
      NEList.map
        (fun case ->
          { pat = preTraverseMatchPattern case.pat
            whenCondition = Option.map f case.whenCondition
            rhs = f case.rhs })
        cases
    )
  | ERecord(id, typeName, fields) ->
    ERecord(
      id,
      fqtnFn typeName,
      NEList.map (fun (name, expr) -> (name, f expr)) fields
    )
  | ERecordUpdate(id, record, updates) ->
    ERecordUpdate(
      id,
      f record,
      NEList.map (fun (name, expr) -> (name, f expr)) updates
    )
  | EError(id, msg, exprs) -> EError(id, msg, List.map f exprs)

let rec postTraversal
  (exprFn : Expr -> Expr)
  (typeRefFn : TypeReference -> TypeReference)
  (fqtnFn : FQTypeName.FQTypeName -> FQTypeName.FQTypeName)
  (fqfnFn : FQFnName.FQFnName -> FQFnName.FQFnName)
  (fqcnFn : FQConstantName.FQConstantName -> FQConstantName.FQConstantName)
  (letPatternFn : LetPattern -> LetPattern)
  (matchPatternFn : MatchPattern -> MatchPattern)
  (expr : Expr)
  : Expr =

  let rec postTraversalLetPattern (pat : LetPattern) : LetPattern =
    let f = postTraversalLetPattern
    match letPatternFn pat with
    | LPVariable _
    | LPUnit _ -> letPatternFn pat
    | LPTuple(id, p1, p2, pats) -> LPTuple(id, f p1, f p2, List.map f pats)

  let rec postTraverseMatchPattern (pat : MatchPattern) : MatchPattern =
    let f = postTraverseMatchPattern
    match matchPatternFn pat with
    | MPVariable _
    | MPInt64 _
    | MPUInt64 _
    | MPInt8 _
    | MPUInt8 _
    | MPInt16 _
    | MPUInt16 _
    | MPInt32 _
    | MPUInt32 _
    | MPInt128 _
    | MPUInt128 _
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
    | TInt64
    | TUInt64
    | TInt8
    | TUInt8
    | TInt16
    | TUInt16
    | TInt32
    | TUInt32
    | TInt128
    | TUInt128
    | TBool
    | TUnit
    | TFloat
    | TChar
    | TUuid
    | TDateTime
    | TVariable _
    | TString -> typeRef
    | TList tr -> TList(f tr)
    | TTuple(tr1, tr2, trs) -> TTuple(f tr1, f tr2, List.map f trs)
    | TDB tr -> TDB(f tr)
    | TCustomType(name, trs) -> TCustomType(Result.map fqtnFn name, List.map f trs)
    | TDict(tr) -> TDict(f tr)
    | TFn(trs, tr) -> TFn(NEList.map f trs, f tr)

  let f =
    postTraversal exprFn typeRefFn fqtnFn fqfnFn fqcnFn letPatternFn matchPatternFn
  (match expr with
   | EInt64 _
   | EUInt64 _
   | EInt8 _
   | EUInt8 _
   | EInt16 _
   | EUInt16 _
   | EInt32 _
   | EUInt32 _
   | EInt128 _
   | EUInt128 _
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
   | EConstant(id, name) -> EConstant(id, fqcnFn name)
   | ELet(id, pat, rhs, next) -> ELet(id, postTraversalLetPattern pat, f rhs, f next)
   | EIf(id, cond, ifexpr, elseexpr) ->
     EIf(id, f cond, f ifexpr, Option.map f elseexpr)
   | ERecordFieldAccess(id, expr, fieldname) ->
     ERecordFieldAccess(id, f expr, fieldname)
   | EApply(id, name, typeArgs, args) ->
     EApply(id, f name, List.map postTraversalTypeRef typeArgs, NEList.map f args)
   | EFnName(id, name) -> EFnName(id, fqfnFn name)
   | EAnd(id, left, right) -> EAnd(id, f left, f right)
   | EOr(id, left, right) -> EOr(id, f left, f right)
   | ELambda(id, names, expr) -> ELambda(id, names, f expr)
   | EList(id, exprs) -> EList(id, List.map f exprs)
   | EDict(id, pairs) -> EDict(id, List.map (fun (k, v) -> (k, f v)) pairs)
   | ETuple(id, first, second, theRest) ->
     ETuple(id, f first, f second, List.map f theRest)
   | EEnum(id, typeName, caseName, fields) ->

     EEnum(id, fqtnFn typeName, caseName, List.map f fields)
   | EMatch(id, mexpr, cases) ->
     EMatch(
       id,
       f mexpr,
       NEList.map
         (fun case ->
           ({ pat = postTraverseMatchPattern case.pat
              whenCondition = Option.map f case.whenCondition
              rhs = f case.rhs }))
         cases
     )
   | ERecord(id, typeName, fields) ->
     ERecord(
       id,
       fqtnFn typeName,
       NEList.map (fun (name, expr) -> (name, f expr)) fields
     )
   | ERecordUpdate(id, record, updates) ->
     ERecordUpdate(
       id,
       f record,
       NEList.map (fun (name, expr) -> (name, f expr)) updates
     )
   | EError(id, msg, exprs) -> EError(id, msg, List.map f exprs))
  |> exprFn



let rec postTraversalAsync
  (exprFn : Expr -> Ply.Ply<Expr>)
  (typeRefFn : TypeReference -> Ply.Ply<TypeReference>)
  (fqtnFn : FQTypeName.FQTypeName -> Ply.Ply<FQTypeName.FQTypeName>)
  (fqfnFn : FQFnName.FQFnName -> Ply.Ply<FQFnName.FQFnName>)
  (fqcnFn : FQConstantName.FQConstantName -> Ply.Ply<FQConstantName.FQConstantName>)
  (letPatternFn : LetPattern -> Ply.Ply<LetPattern>)
  (matchPatternFn : MatchPattern -> Ply.Ply<MatchPattern>)
  (expr : Expr)
  : Ply.Ply<Expr> =

  let rec postTraversalLetPattern (pat : LetPattern) : Ply.Ply<LetPattern> =
    uply {
      let! pat = letPatternFn pat
      let r = postTraversalLetPattern
      match pat with
      | LPVariable _
      | LPUnit _ -> return pat
      | LPTuple(id, p1, p2, pats) ->
        let! p1 = r p1
        let! p2 = r p2
        let! pats = Ply.List.mapSequentially r pats
        return LPTuple(id, p1, p2, pats)
    }

  let rec postTraverseMatchPattern (pat : MatchPattern) : Ply.Ply<MatchPattern> =
    uply {
      let! pat = matchPatternFn pat

      let r = postTraverseMatchPattern
      match pat with
      | MPVariable _
      | MPInt64 _
      | MPUInt64 _
      | MPInt8 _
      | MPUInt8 _
      | MPInt16 _
      | MPUInt16 _
      | MPInt32 _
      | MPUInt32 _
      | MPInt128 _
      | MPUInt128 _
      | MPBool _
      | MPString _
      | MPChar _
      | MPFloat _
      | MPUnit _ -> return pat
      | MPList(id, pats) ->
        let! pats = Ply.List.mapSequentially r pats
        return MPList(id, pats)
      | MPTuple(id, p1, p2, pats) ->
        let! p1 = r p1
        let! p2 = r p2
        let! pats = Ply.List.mapSequentially r pats
        return MPTuple(id, p1, p2, pats)
      | MPEnum(id, name, pats) ->
        let! pats = Ply.List.mapSequentially r pats
        return MPEnum(id, name, pats)
      | MPListCons(id, head, tail) ->
        let! head = r head
        let! tail = r tail
        return MPListCons(id, head, tail)
    }

  let rec postTraversalTypeRef (typeRef : TypeReference) : Ply.Ply<TypeReference> =
    uply {
      let! typeRef = typeRefFn typeRef
      let r = postTraversalTypeRef
      match typeRef with
      | TInt64
      | TUInt64
      | TInt8
      | TUInt8
      | TInt16
      | TUInt16
      | TInt32
      | TUInt32
      | TInt128
      | TUInt128
      | TBool
      | TUnit
      | TFloat
      | TChar
      | TUuid
      | TDateTime
      | TVariable _
      | TString -> return typeRef
      | TList tr ->
        let! tr = r tr
        return TList(tr)
      | TTuple(tr1, tr2, trs) ->
        let! tr1 = r tr1
        let! tr2 = r tr2
        let! trs = Ply.List.mapSequentially r trs
        return TTuple(tr1, tr2, trs)
      | TDB tr ->
        let! tr = r tr
        return TDB(tr)
      | TCustomType(name, trs) ->
        let! trs = Ply.List.mapSequentially r trs
        let! name = Ply.Result.map fqtnFn name
        return TCustomType(name, trs)
      | TDict(tr) ->
        let! tr = r tr
        return TDict(tr)
      | TFn(trs, tr) ->
        let! trs = Ply.NEList.mapSequentially r trs
        let! tr = r tr
        return TFn(trs, tr)
    }

  uply {
    let r =
      postTraversalAsync
        exprFn
        typeRefFn
        fqtnFn
        fqfnFn
        fqcnFn
        letPatternFn
        matchPatternFn

    let! expr =
      match expr with
      | EInt64 _
      | EUInt64 _
      | EInt8 _
      | EUInt8 _
      | EInt16 _
      | EUInt16 _
      | EInt32 _
      | EUInt32 _
      | EInt128 _
      | EUInt128 _
      | EBool _
      | EChar _
      | EUnit _
      | EVariable _
      | EFloat _ -> Ply expr
      | EAnd(id, left, right) ->
        uply {
          let! left = r left
          let! right = r right
          return EAnd(id, left, right)
        }
      | EOr(id, left, right) ->
        uply {
          let! left = r left
          let! right = r right
          return EOr(id, left, right)
        }
      | ELambda(id, names, expr) ->
        uply {
          let! expr = r expr
          return ELambda(id, names, expr)
        }
      | ELet(id, pat, rhs, next) ->
        uply {
          let! pat = postTraversalLetPattern pat
          let! rhs = r rhs
          let! next = r next
          return ELet(id, pat, rhs, next)
        }
      | EList(id, exprs) ->
        uply {
          let! exprs = Ply.List.mapSequentially r exprs
          return EList(id, exprs)
        }
      | ETuple(id, first, second, theRest) ->
        uply {
          let! first = r first
          let! second = r second
          let! theRest = Ply.List.mapSequentially r theRest
          return ETuple(id, first, second, theRest)
        }
      | EIf(id, cond, ifexpr, elseexpr) ->
        uply {
          let! cond = r cond
          let! ifexpr = r ifexpr
          let! elseexpr = Ply.Option.map r elseexpr
          return EIf(id, cond, ifexpr, elseexpr)
        }
      | EMatch(id, mexpr, cases) ->
        uply {
          let! mexpr = r mexpr
          let! cases =
            Ply.NEList.mapSequentially
              (fun case ->
                uply {
                  let! pattern = postTraverseMatchPattern case.pat
                  let! whenCondition = Ply.Option.map r case.whenCondition
                  let! expr = r case.rhs
                  return
                    { pat = pattern; whenCondition = whenCondition; rhs = expr }
                })
              cases
          return EMatch(id, mexpr, cases)
        }

      | ERecord(id, typeName, fields) ->
        uply {
          let! fields =
            Ply.NEList.mapSequentially
              (fun (name, expr) ->
                uply {
                  let! expr = r expr
                  return (name, expr)
                })
              fields
          return ERecord(id, typeName, fields)
        }
      | ERecordUpdate(id, record, updates) ->
        uply {
          let! record = r record
          let! updates =
            Ply.NEList.mapSequentially
              (fun (name, expr) ->
                uply {
                  let! expr = r expr
                  return (name, expr)
                })
              updates
          return ERecordUpdate(id, record, updates)
        }
      | EApply(id, name, typeArgs, args) ->
        uply {
          let! name = r name
          let! typeArgs = Ply.List.mapSequentially postTraversalTypeRef typeArgs
          let! args = Ply.NEList.mapSequentially r args
          return EApply(id, name, typeArgs, args)
        }
      | EError(id, msg, exprs) ->
        uply {
          let! exprs = Ply.List.mapSequentially r exprs
          return EError(id, msg, exprs)
        }
      | EDict(id, pairs) ->
        uply {
          let! pairs =
            Ply.List.mapSequentially
              (fun (k, v) ->
                uply {
                  let! v = r v
                  return (k, v)
                })
              pairs
          return EDict(id, pairs)
        }
      | EFnName(id, name) ->
        uply {
          let! name = fqfnFn name
          return EFnName(id, name)
        }
      | EConstant(id, name) ->
        uply {
          let! name = fqcnFn name
          return EConstant(id, name)
        }
      | EEnum(id, typeName, caseName, fields) ->
        uply {
          let! typeName = fqtnFn typeName
          let! fields = Ply.List.mapSequentially r fields
          return EEnum(id, typeName, caseName, fields)
        }
      | ERecordFieldAccess(id, expr, fieldname) ->
        uply {
          let! expr = r expr
          return ERecordFieldAccess(id, expr, fieldname)
        }


      | EString(id, strs) ->
        uply {
          let! strs =
            Ply.List.mapSequentially
              (fun s ->
                uply {
                  match s with
                  | StringText t -> return (StringText t)
                  | StringInterpolation e ->
                    let! e = r e
                    return (StringInterpolation(e))
                })
              strs
          return EString(id, strs)
        }

    return! exprFn expr
  }
