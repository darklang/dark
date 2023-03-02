/// Runtime Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// These should all directly match `RuntimeTypes.res` in `client`.
/// See `RuntimeTypes.fs` for documentation of these types.
module ClientTypes2ExecutionTypes.Runtime

open Prelude
open Tablecloth

open ClientTypes.Runtime

module RT = LibExecution.RuntimeTypes

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


module DType =
  let rec toCT (t : RT.DType) : DType =
    let r = toCT
    let rl = List.map toCT
    match t with
    | RT.TInt -> TInt
    | RT.TFloat -> TFloat
    | RT.TBool -> TBool
    | RT.TUnit -> TUnit
    | RT.TStr -> TStr
    | RT.TList t -> TList(r t)
    | RT.TTuple (t1, t2, ts) -> TTuple(r t1, r t2, rl ts)
    | RT.TDict t -> TDict(r t)
    | RT.TIncomplete -> TIncomplete
    | RT.TError -> TError
    | RT.THttpResponse t -> THttpResponse(r t)
    | RT.TDB t -> TDB(r t)
    | RT.TDateTime -> TDateTime
    | RT.TChar -> TChar
    | RT.TPassword -> TPassword
    | RT.TUuid -> TUuid
    | RT.TOption t -> TOption(r t)
    | RT.TUserType (str, version) -> TUserType(str, version)
    | RT.TBytes -> TBytes
    | RT.TResult (ok, error) -> TResult(r ok, r error)
    | RT.TVariable (name) -> TVariable(name)
    | RT.TFn (ts, returnType) -> TFn(rl ts, r returnType)
    | RT.TRecord (pairs) -> TRecord(List.map (fun (k, t) -> (k, r t)) pairs)

  let rec fromCT (t : DType) : RT.DType =
    let r = fromCT
    let rl = List.map fromCT

    match t with
    | TInt -> RT.TInt
    | TFloat -> RT.TFloat
    | TBool -> RT.TBool
    | TUnit -> RT.TUnit
    | TStr -> RT.TStr
    | TList t -> RT.TList(r t)
    | TTuple (t1, t2, ts) -> RT.TTuple(r t1, r t2, rl ts)
    | TDict t -> RT.TDict(r t)
    | TIncomplete -> RT.TIncomplete
    | TError -> RT.TError
    | THttpResponse t -> RT.THttpResponse(r t)
    | TDB t -> RT.TDB(r t)
    | TDateTime -> RT.TDateTime
    | TChar -> RT.TChar
    | TPassword -> RT.TPassword
    | TUuid -> RT.TUuid
    | TOption t -> RT.TOption(r t)
    | TUserType (str, version) -> RT.TUserType(str, version)
    | TBytes -> RT.TBytes
    | TResult (ok, error) -> RT.TResult(r ok, r error)
    | TVariable (name) -> RT.TVariable(name)
    | TFn (ts, returnType) -> RT.TFn(rl ts, r returnType)
    | TRecord (pairs) -> RT.TRecord(List.map (fun (k, t) -> (k, r t)) pairs)

module MatchPattern =
  let rec fromCT (p : MatchPattern) : RT.MatchPattern =
    let r = fromCT
    match p with
    | MPVariable (id, str) -> RT.MPVariable(id, str)
    | MPConstructor (id, name, pats) -> RT.MPConstructor(id, name, List.map r pats)
    | MPInteger (id, i) -> RT.MPInteger(id, i)
    | MPBool (id, b) -> RT.MPBool(id, b)
    | MPCharacter (id, c) -> RT.MPCharacter(id, c)
    | MPString (id, s) -> RT.MPString(id, s)
    | MPFloat (id, f) -> RT.MPFloat(id, f)
    | MPUnit id -> RT.MPUnit id
    | MPTuple (id, first, second, theRest) ->
      RT.MPTuple(id, r first, r second, List.map r theRest)

  let rec toCT (p : RT.MatchPattern) : MatchPattern =
    let r = toCT
    match p with
    | RT.MPVariable (id, str) -> MPVariable(id, str)
    | RT.MPConstructor (id, name, pats) -> MPConstructor(id, name, List.map r pats)
    | RT.MPInteger (id, i) -> MPInteger(id, i)
    | RT.MPBool (id, b) -> MPBool(id, b)
    | RT.MPCharacter (id, c) -> MPCharacter(id, c)
    | RT.MPString (id, s) -> MPString(id, s)
    | RT.MPFloat (id, f) -> MPFloat(id, f)
    | RT.MPUnit id -> MPUnit id
    | RT.MPTuple (id, first, second, theRest) ->
      MPTuple(id, r first, r second, List.map r theRest)

module Expr =
  let pipeToRT (pipe : Expr.IsInPipe) : RT.IsInPipe =
    match pipe with
    | Expr.InPipe (id) -> RT.InPipe(id)
    | Expr.NotInPipe -> RT.NotInPipe

  let pipeFromRT (pipe : RT.IsInPipe) : Expr.IsInPipe =
    match pipe with
    | RT.InPipe (id) -> Expr.InPipe(id)
    | RT.NotInPipe -> Expr.NotInPipe

  let rec fromCT (e : Expr.T) : RT.Expr =
    let r = fromCT

    match e with
    | Expr.ECharacter (id, char) -> RT.ECharacter(id, char)
    | Expr.EInteger (id, num) -> RT.EInteger(id, num)
    | Expr.EString (id, str) -> RT.EString(id, str)
    | Expr.EFloat (id, f) -> RT.EFloat(id, f)

    | Expr.EBool (id, b) -> RT.EBool(id, b)
    | Expr.EUnit id -> RT.EUnit id
    | Expr.EVariable (id, var) -> RT.EVariable(id, var)
    | Expr.EFieldAccess (id, obj, fieldname) -> RT.EFieldAccess(id, r obj, fieldname)
    | Expr.ELambda (id, vars, body) -> RT.ELambda(id, vars, r body)
    | Expr.ELet (id, lhs, rhs, body) -> RT.ELet(id, lhs, r rhs, r body)
    | Expr.EIf (id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, r cond, r thenExpr, r elseExpr)
    | Expr.EApply (id, expr, exprs, pipe) ->
      RT.EApply(id, r expr, List.map r exprs, pipeToRT pipe)
    | Expr.EFQFnValue (id, name) -> RT.EFQFnValue(id, FQFnName.fromCT name)
    | Expr.EList (id, exprs) -> RT.EList(id, List.map r exprs)
    | Expr.ETuple (id, first, second, theRest) ->
      RT.ETuple(id, r first, r second, List.map r theRest)
    | Expr.ERecord (id, pairs) -> RT.ERecord(id, List.map (Tuple2.mapSecond r) pairs)
    | Expr.EConstructor (id, name, exprs) ->
      RT.EConstructor(id, name, List.map r exprs)
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


  let rec toCT (e : RT.Expr) : Expr.T =
    let r = toCT

    match e with
    | RT.ECharacter (id, char) -> Expr.ECharacter(id, char)
    | RT.EInteger (id, num) -> Expr.EInteger(id, num)
    | RT.EString (id, str) -> Expr.EString(id, str)
    | RT.EFloat (id, f) -> Expr.EFloat(id, f)

    | RT.EBool (id, b) -> Expr.EBool(id, b)
    | RT.EUnit id -> Expr.EUnit id
    | RT.EVariable (id, var) -> Expr.EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> Expr.EFieldAccess(id, r obj, fieldname)
    | RT.ELambda (id, vars, body) -> Expr.ELambda(id, vars, r body)
    | RT.ELet (id, lhs, rhs, body) -> Expr.ELet(id, lhs, r rhs, r body)
    | RT.EIf (id, cond, thenExpr, elseExpr) ->
      Expr.EIf(id, r cond, r thenExpr, r elseExpr)
    | RT.EApply (id, expr, exprs, pipe) ->
      Expr.EApply(id, r expr, List.map r exprs, pipeFromRT pipe)
    | RT.EFQFnValue (id, name) -> Expr.EFQFnValue(id, FQFnName.toCT name)
    | RT.EList (id, exprs) -> Expr.EList(id, List.map r exprs)
    | RT.ETuple (id, first, second, theRest) ->
      Expr.ETuple(id, r first, r second, List.map r theRest)
    | RT.ERecord (id, pairs) -> Expr.ERecord(id, List.map (Tuple2.mapSecond r) pairs)
    | RT.EConstructor (id, name, exprs) ->
      Expr.EConstructor(id, name, List.map r exprs)
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

  let rec httpResponseFromCT (s : Dval.DHTTP) : RT.DHTTP =
    match s with
    | Dval.Redirect (uri) -> RT.Redirect(uri)
    | Dval.Response (id, pairs, dval) -> RT.Response(id, pairs, fromCT dval)

  and httpResponseToCT (s : RT.DHTTP) : Dval.DHTTP =
    match s with
    | RT.Redirect (uri) -> Dval.Redirect(uri)
    | RT.Response (code, headers, dval) -> Dval.Response(code, headers, toCT dval)

  and fromCT (dv : Dval.T) : RT.Dval =
    let r = fromCT

    match dv with
    | Dval.DStr s -> RT.DStr s
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
    | Dval.DFnVal (Dval.FnName (name)) -> RT.DFnVal(RT.FnName(FQFnName.fromCT name))
    | Dval.DIncomplete (source) -> RT.DIncomplete(DvalSource.fromCT source)
    | Dval.DError (source, msg) -> RT.DError(DvalSource.fromCT source, msg)
    | Dval.DDate d -> RT.DDate d
    | Dval.DDB name -> RT.DDB name
    | Dval.DUuid uuid -> RT.DUuid uuid
    | Dval.DPassword pw -> RT.DPassword(pw)
    | Dval.DHttpResponse (response) -> RT.DHttpResponse(httpResponseFromCT response)
    | Dval.DList l -> RT.DList(List.map r l)
    | Dval.DTuple (first, second, theRest) ->
      RT.DTuple(r first, r second, List.map r theRest)
    | Dval.DObj o -> RT.DObj(Map.map r o)
    | Dval.DOption None -> RT.DOption None
    | Dval.DOption (Some dv) -> RT.DOption(Some(r dv))
    | Dval.DResult (Ok dv) -> RT.DResult(Ok(r dv))
    | Dval.DResult (Error dv) -> RT.DResult(Error(r dv))
    | Dval.DBytes bytes -> RT.DBytes bytes

  and toCT (dv : RT.Dval) : Dval.T =
    let r = toCT

    match dv with
    | RT.DStr s -> Dval.DStr s
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
    | RT.DFnVal (RT.FnName (name)) -> Dval.DFnVal(Dval.FnName(FQFnName.toCT name))
    | RT.DIncomplete (source) -> Dval.DIncomplete(DvalSource.toCT source)
    | RT.DError (source, msg) -> Dval.DError(DvalSource.toCT source, msg)
    | RT.DDate d -> Dval.DDate d
    | RT.DDB name -> Dval.DDB name
    | RT.DUuid uuid -> Dval.DUuid uuid
    | RT.DPassword (Password pw) -> Dval.DPassword(Password pw)
    | RT.DHttpResponse (response) -> Dval.DHttpResponse(httpResponseToCT response)
    | RT.DList l -> Dval.DList(List.map r l)
    | RT.DTuple (first, second, theRest) ->
      Dval.DTuple(r first, r second, List.map r theRest)
    | RT.DObj o -> Dval.DObj(Map.map r o)
    | RT.DOption None -> Dval.DOption None
    | RT.DOption (Some dv) -> Dval.DOption(Some(r dv))
    | RT.DResult (Ok dv) -> Dval.DResult(Ok(r dv))
    | RT.DResult (Error dv) -> Dval.DResult(Error(r dv))
    | RT.DBytes bytes -> Dval.DBytes bytes
