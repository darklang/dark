/// Runtime Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// See `RuntimeTypes.fs` for documentation of these types.
module ClientTypes2ExecutionTypes.Runtime

open Prelude
open Tablecloth

open ClientTypes.Runtime

module RT = LibExecution.RuntimeTypes

module FQTypeName =
  module UserTypeName =
    let fromCT (u : FQTypeName.UserTypeName) : RT.FQTypeName.UserTypeName =
      { typ = u.typ; version = u.version }

    let toCT (u : RT.FQTypeName.UserTypeName) : FQTypeName.UserTypeName =
      { typ = u.typ; version = u.version }

  module StdlibTypeName =
    let fromCT (s : FQTypeName.StdlibTypeName) : RT.FQTypeName.StdlibTypeName =
      { module_ = s.module_; typ = s.typ; version = s.version }

    let toCT (s : RT.FQTypeName.StdlibTypeName) : FQTypeName.StdlibTypeName =
      { module_ = s.module_; typ = s.typ; version = s.version }

  module PackageTypeName =
    let fromCT (p : FQTypeName.PackageTypeName) : RT.FQTypeName.PackageTypeName =
      { owner = p.owner
        package = p.package
        module_ = p.module_
        typ = p.typ
        version = p.version }

    let toCT (p : RT.FQTypeName.PackageTypeName) : FQTypeName.PackageTypeName =
      { owner = p.owner
        package = p.package
        module_ = p.module_
        typ = p.typ
        version = p.version }

  let fromCT (t : FQTypeName.T) : RT.FQTypeName.T =
    match t with
    | FQTypeName.Stdlib s -> RT.FQTypeName.Stdlib(StdlibTypeName.fromCT s)
    | FQTypeName.User u -> RT.FQTypeName.User(UserTypeName.fromCT u)
    | FQTypeName.Package p -> RT.FQTypeName.Package(PackageTypeName.fromCT p)

  let toCT (t : RT.FQTypeName.T) : FQTypeName.T =
    match t with
    | RT.FQTypeName.Stdlib s -> FQTypeName.Stdlib(StdlibTypeName.toCT s)
    | RT.FQTypeName.User u -> FQTypeName.User(UserTypeName.toCT u)
    | RT.FQTypeName.Package p -> FQTypeName.Package(PackageTypeName.toCT p)


module FQFnName =
  type UserFnName = string

  module PackageFnName =
    let fromCT (name : FQFnName.PackageFnName) : RT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

    let toCT (name : RT.FQFnName.PackageFnName) : FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    let fromCT (name : FQFnName.StdlibFnName) : RT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

    let toCT (name : RT.FQFnName.StdlibFnName) : FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

  let fromCT (fqfn : FQFnName.T) : RT.FQFnName.T =
    match fqfn with
    | FQFnName.User u -> RT.FQFnName.User u
    | FQFnName.Stdlib fn -> RT.FQFnName.Stdlib(StdlibFnName.fromCT fn)
    | FQFnName.Package p -> RT.FQFnName.Package(PackageFnName.fromCT p)

  let toCT (fqfn : RT.FQFnName.T) : FQFnName.T =
    match fqfn with
    | RT.FQFnName.User u -> FQFnName.User u
    | RT.FQFnName.Stdlib fn -> FQFnName.Stdlib(StdlibFnName.toCT fn)
    | RT.FQFnName.Package p -> FQFnName.Package(PackageFnName.toCT p)


module TypeReference =
  let rec toCT (t : RT.TypeReference) : TypeReference =
    let r = toCT
    let rl = List.map toCT

    match t with
    | RT.TInt -> TInt
    | RT.TFloat -> TFloat
    | RT.TBool -> TBool
    | RT.TUnit -> TUnit
    | RT.TString -> TString
    | RT.TList t -> TList(r t)
    | RT.TTuple (t1, t2, ts) -> TTuple(r t1, r t2, rl ts)
    | RT.TDict t -> TDict(r t)
    | RT.THttpResponse t -> THttpResponse(r t)
    | RT.TDB t -> TDB(r t)
    | RT.TDateTime -> TDateTime
    | RT.TChar -> TChar
    | RT.TPassword -> TPassword
    | RT.TUuid -> TUuid
    | RT.TOption t -> TOption(r t)
    | RT.TCustomType (typeName, typeArgs) ->
      TCustomType(FQTypeName.toCT typeName, List.map r typeArgs)
    | RT.TBytes -> TBytes
    | RT.TResult (ok, error) -> TResult(r ok, r error)
    | RT.TVariable (name) -> TVariable(name)
    | RT.TFn (ts, returnType) -> TFn(rl ts, r returnType)

  let rec fromCT (t : TypeReference) : RT.TypeReference =
    let r = fromCT
    let rl = List.map fromCT

    match t with
    | TInt -> RT.TInt
    | TFloat -> RT.TFloat
    | TBool -> RT.TBool
    | TUnit -> RT.TUnit
    | TString -> RT.TString
    | TList t -> RT.TList(r t)
    | TTuple (t1, t2, ts) -> RT.TTuple(r t1, r t2, rl ts)
    | TDict t -> RT.TDict(r t)
    | THttpResponse t -> RT.THttpResponse(r t)
    | TDB t -> RT.TDB(r t)
    | TDateTime -> RT.TDateTime
    | TChar -> RT.TChar
    | TPassword -> RT.TPassword
    | TUuid -> RT.TUuid
    | TOption t -> RT.TOption(r t)
    | TCustomType (t, typeArgs) ->
      RT.TCustomType(FQTypeName.fromCT t, List.map r typeArgs)
    | TBytes -> RT.TBytes
    | TResult (ok, error) -> RT.TResult(r ok, r error)
    | TVariable (name) -> RT.TVariable(name)
    | TFn (ts, returnType) -> RT.TFn(rl ts, r returnType)

module LetPattern =
  let rec fromCT (p : LetPattern) : RT.LetPattern =
    match p with
    | LPVariable (id, str) -> RT.LPVariable(id, str)

  let rec toCT (p : RT.LetPattern) : LetPattern =
    match p with
    | RT.LPVariable (id, str) -> LPVariable(id, str)

module MatchPattern =
  let rec fromCT (p : MatchPattern) : RT.MatchPattern =
    let r = fromCT
    match p with
    | MPVariable (id, str) -> RT.MPVariable(id, str)
    | MPConstructor (id, caseName, fieldPats) ->
      RT.MPConstructor(id, caseName, List.map r fieldPats)
    | MPInt (id, i) -> RT.MPInt(id, i)
    | MPBool (id, b) -> RT.MPBool(id, b)
    | MPChar (id, c) -> RT.MPChar(id, c)
    | MPString (id, s) -> RT.MPString(id, s)
    | MPFloat (id, f) -> RT.MPFloat(id, f)
    | MPUnit id -> RT.MPUnit id
    | MPTuple (id, first, second, theRest) ->
      RT.MPTuple(id, r first, r second, List.map r theRest)
    | MPList (id, pats) -> RT.MPList(id, List.map r pats)

  let rec toCT (p : RT.MatchPattern) : MatchPattern =
    let r = toCT
    match p with
    | RT.MPVariable (id, str) -> MPVariable(id, str)
    | RT.MPConstructor (id, caseName, fieldPats) ->
      MPConstructor(id, caseName, List.map r fieldPats)
    | RT.MPInt (id, i) -> MPInt(id, i)
    | RT.MPBool (id, b) -> MPBool(id, b)
    | RT.MPChar (id, c) -> MPChar(id, c)
    | RT.MPString (id, s) -> MPString(id, s)
    | RT.MPFloat (id, f) -> MPFloat(id, f)
    | RT.MPUnit id -> MPUnit id
    | RT.MPTuple (id, first, second, theRest) ->
      MPTuple(id, r first, r second, List.map r theRest)
    | RT.MPList (id, pats) -> MPList(id, List.map r pats)

module Expr =

  let rec fromCT (e : Expr.T) : RT.Expr =
    let r = fromCT

    match e with
    | Expr.EChar (id, char) -> RT.EChar(id, char)
    | Expr.EInt (id, num) -> RT.EInt(id, num)
    | Expr.EString (id, segment) ->
      RT.EString(id, List.map stringSegmentToRT segment)
    | Expr.EFloat (id, f) -> RT.EFloat(id, f)

    | Expr.EBool (id, b) -> RT.EBool(id, b)
    | Expr.EUnit id -> RT.EUnit id
    | Expr.EVariable (id, var) -> RT.EVariable(id, var)
    | Expr.EFieldAccess (id, obj, fieldname) -> RT.EFieldAccess(id, r obj, fieldname)
    | Expr.ELambda (id, vars, body) -> RT.ELambda(id, vars, r body)
    | Expr.ELet (id, pat, rhs, body) ->
      RT.ELet(id, LetPattern.fromCT pat, r rhs, r body)
    | Expr.EIf (id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, r cond, r thenExpr, r elseExpr)
    | Expr.EApply (id, target, typeArgs, exprs) ->
      RT.EApply(
        id,
        fnTargetFromCT target,
        List.map TypeReference.fromCT typeArgs,
        List.map r exprs
      )
    | Expr.EList (id, exprs) -> RT.EList(id, List.map r exprs)
    | Expr.ETuple (id, first, second, theRest) ->
      RT.ETuple(id, r first, r second, List.map r theRest)
    | Expr.ERecord (id, typeName, fields) ->
      RT.ERecord(
        id,
        Option.map FQTypeName.fromCT typeName,
        List.map (Tuple2.mapSecond r) fields
      )
    | Expr.EConstructor (id, typeName, caseName, fields) ->
      RT.EConstructor(
        id,
        Option.map FQTypeName.fromCT typeName,
        caseName,
        List.map r fields
      )
    | Expr.EMatch (id, mexpr, pairs) ->
      RT.EMatch(
        id,
        r mexpr,
        List.map (Tuple2.mapFirst MatchPattern.fromCT << Tuple2.mapSecond r) pairs
      )
    | Expr.EFeatureFlag (id, cond, caseA, caseB) ->
      RT.EFeatureFlag(id, r cond, r caseA, r caseB)
    | Expr.EAnd (id, left, right) -> RT.EAnd(id, r left, r right)
    | Expr.EOr (id, left, right) -> RT.EOr(id, r left, r right)

  and stringSegmentToRT (segment : Expr.StringSegment) : RT.StringSegment =
    match segment with
    | Expr.StringText text -> RT.StringText text
    | Expr.StringInterpolation expr -> RT.StringInterpolation(fromCT expr)

  and fnTargetFromCT (target : Expr.FnTarget) : RT.FnTarget =
    match target with
    | Expr.FnName name -> RT.FnName(FQFnName.fromCT name)
    | Expr.FnTargetExpr expr -> RT.FnTargetExpr(fromCT expr)




  let rec toCT (e : RT.Expr) : Expr.T =
    let r = toCT

    match e with
    | RT.EChar (id, char) -> Expr.EChar(id, char)
    | RT.EInt (id, num) -> Expr.EInt(id, num)
    | RT.EString (id, str) -> Expr.EString(id, List.map stringSegmentToCT str)
    | RT.EFloat (id, f) -> Expr.EFloat(id, f)

    | RT.EBool (id, b) -> Expr.EBool(id, b)
    | RT.EUnit id -> Expr.EUnit id
    | RT.EVariable (id, var) -> Expr.EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> Expr.EFieldAccess(id, r obj, fieldname)
    | RT.ELambda (id, vars, body) -> Expr.ELambda(id, vars, r body)
    | RT.ELet (id, pat, rhs, body) ->
      Expr.ELet(id, LetPattern.toCT pat, r rhs, r body)
    | RT.EIf (id, cond, thenExpr, elseExpr) ->
      Expr.EIf(id, r cond, r thenExpr, r elseExpr)
    | RT.EApply (id, target, typeArgs, exprs) ->
      Expr.EApply(
        id,
        fnTargetToCT target,
        List.map TypeReference.toCT typeArgs,
        List.map r exprs
      )
    | RT.EList (id, exprs) -> Expr.EList(id, List.map r exprs)
    | RT.ETuple (id, first, second, theRest) ->
      Expr.ETuple(id, r first, r second, List.map r theRest)
    | RT.ERecord (id, typeName, fields) ->
      Expr.ERecord(
        id,
        Option.map FQTypeName.toCT typeName,
        List.map (Tuple2.mapSecond r) fields
      )
    | RT.EConstructor (id, typeName, caseName, fields) ->
      Expr.EConstructor(
        id,
        Option.map FQTypeName.toCT typeName,
        caseName,
        List.map r fields
      )
    | RT.EMatch (id, mexpr, pairs) ->
      Expr.EMatch(
        id,
        r mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toCT << Tuple2.mapSecond r) pairs
      )
    | RT.EFeatureFlag (id, cond, caseA, caseB) ->
      Expr.EFeatureFlag(id, r cond, r caseA, r caseB)
    | RT.EAnd (id, left, right) -> Expr.EAnd(id, r left, r right)
    | RT.EOr (id, left, right) -> Expr.EOr(id, r left, r right)

  and stringSegmentToCT (segment : RT.StringSegment) : Expr.StringSegment =
    match segment with
    | RT.StringText text -> Expr.StringText text
    | RT.StringInterpolation expr -> Expr.StringInterpolation(toCT expr)

  and fnTargetToCT (target : RT.FnTarget) : Expr.FnTarget =
    match target with
    | RT.FnName name -> Expr.FnName(FQFnName.toCT name)
    | RT.FnTargetExpr expr -> Expr.FnTargetExpr(toCT expr)

module Dval =
  module DvalSource =
    let fromCT (s : Dval.DvalSource) : RT.DvalSource =
      match s with
      | Dval.SourceNone -> RT.SourceNone
      | Dval.SourceID (tlid, id) -> RT.SourceID(tlid, id)

    let toCT (s : RT.DvalSource) : Dval.DvalSource =
      match s with
      | RT.SourceNone -> Dval.SourceNone
      | RT.SourceID (tlid, id) -> Dval.SourceID(tlid, id)

  let rec fromCT (dv : Dval.T) : RT.Dval =
    let r = fromCT

    match dv with
    | Dval.DString s -> RT.DString s
    | Dval.DChar c -> RT.DChar c
    | Dval.DInt i -> RT.DInt i
    | Dval.DBool b -> RT.DBool b
    | Dval.DFloat f -> RT.DFloat f
    | Dval.DUnit -> RT.DUnit
    | Dval.DFnVal (Dval.Lambda (impl)) ->
      let symtable = Map.map r impl.symtable
      RT.DFnVal(
        RT.Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.fromCT impl.body }
      )
    | Dval.DIncomplete (source) -> RT.DIncomplete(DvalSource.fromCT source)
    | Dval.DError (source, msg) -> RT.DError(DvalSource.fromCT source, msg)
    | Dval.DDateTime d -> RT.DDateTime d
    | Dval.DDB name -> RT.DDB name
    | Dval.DUuid uuid -> RT.DUuid uuid
    | Dval.DPassword pw -> RT.DPassword(pw)
    | Dval.DHttpResponse (id, pairs, dval) -> RT.DHttpResponse(id, pairs, r dval)
    | Dval.DList list -> RT.DList(List.map r list)
    | Dval.DTuple (first, second, theRest) ->
      RT.DTuple(r first, r second, List.map r theRest)
    | Dval.DDict o -> RT.DDict(Map.map r o)
    | Dval.DRecord o -> RT.DRecord(Map.map r o)
    | Dval.DOption None -> RT.DOption None
    | Dval.DOption (Some dv) -> RT.DOption(Some(r dv))
    | Dval.DResult (Ok dv) -> RT.DResult(Ok(r dv))
    | Dval.DResult (Error dv) -> RT.DResult(Error(r dv))
    | Dval.DBytes bytes -> RT.DBytes bytes
    | Dval.DConstructor (typeName, caseName, fields) ->
      RT.DConstructor(
        Option.map FQTypeName.fromCT typeName,
        caseName,
        List.map r fields
      )

  and toCT (dv : RT.Dval) : Dval.T =
    let r = toCT

    match dv with
    | RT.DString s -> Dval.DString s
    | RT.DChar c -> Dval.DChar c
    | RT.DInt i -> Dval.DInt i
    | RT.DBool b -> Dval.DBool b
    | RT.DFloat f -> Dval.DFloat f
    | RT.DUnit -> Dval.DUnit
    | RT.DFnVal (RT.Lambda (impl)) ->
      let symtable = Map.map r impl.symtable
      Dval.DFnVal(
        Dval.Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.toCT impl.body }
      )
    | RT.DIncomplete (source) -> Dval.DIncomplete(DvalSource.toCT source)
    | RT.DError (source, msg) -> Dval.DError(DvalSource.toCT source, msg)
    | RT.DDateTime d -> Dval.DDateTime d
    | RT.DDB name -> Dval.DDB name
    | RT.DUuid uuid -> Dval.DUuid uuid
    | RT.DPassword (Password pw) -> Dval.DPassword(Password pw)
    | RT.DHttpResponse (code, headers, dval) ->
      Dval.DHttpResponse(code, headers, toCT dval)
    | RT.DList l -> Dval.DList(List.map r l)
    | RT.DTuple (first, second, theRest) ->
      Dval.DTuple(r first, r second, List.map r theRest)
    | RT.DDict o -> Dval.DDict(Map.map r o)
    | RT.DRecord o -> Dval.DRecord(Map.map r o)
    | RT.DOption None -> Dval.DOption None
    | RT.DOption (Some dv) -> Dval.DOption(Some(r dv))
    | RT.DResult (Ok dv) -> Dval.DResult(Ok(r dv))
    | RT.DResult (Error dv) -> Dval.DResult(Error(r dv))
    | RT.DBytes bytes -> Dval.DBytes bytes
    | RT.DConstructor (typeName, caseName, fields) -> Dval.DUnit // todo
