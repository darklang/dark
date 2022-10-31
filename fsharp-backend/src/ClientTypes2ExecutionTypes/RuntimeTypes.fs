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
    | RT.TNull -> TNull
    | RT.TStr -> TStr
    | RT.TList t -> TList(r t)
    | RT.TTuple (t1, t2, ts) -> TTuple(r t1, r t2, rl ts)
    | RT.TDict t -> TDict(r t)
    | RT.TIncomplete -> TIncomplete
    | RT.TError -> TError
    | RT.THttpResponse t -> THttpResponse(r t)
    | RT.TDB t -> TDB(r t)
    | RT.TDate -> TDate
    | RT.TChar -> TChar
    | RT.TPassword -> TPassword
    | RT.TUuid -> TUuid
    | RT.TOption t -> TOption(r t)
    | RT.TErrorRail -> TErrorRail
    | RT.TUserType (str, version) -> TUserType(str, version)
    | RT.TBytes -> TBytes
    | RT.TResult (ok, error) -> TResult(r ok, r error)
    | RT.TVariable (name) -> TVariable(name)
    | RT.TFn (ts, returnType) -> TFn(rl ts, r returnType)
    | RT.TRecord (pairs) -> TRecord(List.map (fun (k, t) -> (k, r t)) pairs)

module Pattern =
  let rec fromCT (p : Pattern) : RT.Pattern =
    let r = fromCT
    match p with
    | PVariable (id, str) -> RT.PVariable(id, str)
    | PConstructor (id, name, pats) -> RT.PConstructor(id, name, List.map r pats)
    | PInteger (id, i) -> RT.PInteger(id, i)
    | PBool (id, b) -> RT.PBool(id, b)
    | PCharacter (id, c) -> RT.PCharacter(id, c)
    | PString (id, s) -> RT.PString(id, s)
    | PFloat (id, f) -> RT.PFloat(id, f)
    | PNull id -> RT.PNull id
    | PBlank id -> RT.PBlank id
    | PTuple (id, first, second, theRest) ->
      RT.PTuple(id, r first, r second, List.map r theRest)

  let rec toCT (p : RT.Pattern) : Pattern =
    let r = toCT
    match p with
    | RT.PVariable (id, str) -> PVariable(id, str)
    | RT.PConstructor (id, name, pats) -> PConstructor(id, name, List.map toCT pats)
    | RT.PInteger (id, i) -> PInteger(id, i)
    | RT.PBool (id, b) -> PBool(id, b)
    | RT.PCharacter (id, c) -> PCharacter(id, c)
    | RT.PString (id, s) -> PString(id, s)
    | RT.PFloat (id, f) -> PFloat(id, f)
    | RT.PNull id -> PNull id
    | RT.PBlank id -> PBlank id
    | RT.PTuple (id, first, second, theRest) ->
      PTuple(id, r first, r second, List.map r theRest)

module Expr =
  let pipeToRT (pipe : Expr.IsInPipe) : RT.IsInPipe =
    match pipe with
    | Expr.InPipe (id) -> RT.InPipe(id)
    | Expr.NotInPipe -> RT.NotInPipe

  let pipeFromRT (pipe : RT.IsInPipe) : Expr.IsInPipe =
    match pipe with
    | RT.InPipe (id) -> Expr.InPipe(id)
    | RT.NotInPipe -> Expr.NotInPipe

  let sterToRT (ster : Expr.SendToRail) : RT.SendToRail =
    match ster with
    | Expr.Rail -> RT.Rail
    | Expr.NoRail -> RT.NoRail

  let sterFromRT (ster : RT.SendToRail) : Expr.SendToRail =
    match ster with
    | RT.Rail -> Expr.Rail
    | RT.NoRail -> Expr.NoRail

  let rec fromCT (e : Expr.T) : RT.Expr =
    let r = fromCT

    match e with
    | Expr.EBlank id -> RT.EBlank id
    | Expr.ECharacter (id, char) -> RT.ECharacter(id, char)
    | Expr.EInteger (id, num) -> RT.EInteger(id, num)
    | Expr.EString (id, str) -> RT.EString(id, str)
    | Expr.EFloat (id, f) -> RT.EFloat(id, f)

    | Expr.EBool (id, b) -> RT.EBool(id, b)
    | Expr.ENull id -> RT.ENull id
    | Expr.EVariable (id, var) -> RT.EVariable(id, var)
    | Expr.EFieldAccess (id, obj, fieldname) -> RT.EFieldAccess(id, r obj, fieldname)
    | Expr.ELambda (id, vars, body) -> RT.ELambda(id, vars, r body)
    | Expr.ELet (id, lhs, rhs, body) -> RT.ELet(id, lhs, r rhs, r body)
    | Expr.EIf (id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, r cond, r thenExpr, r elseExpr)
    | Expr.EApply (id, expr, exprs, pipe, ster) ->
      RT.EApply(id, r expr, List.map r exprs, pipeToRT pipe, sterToRT ster)
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
        List.map (Tuple2.mapFirst Pattern.fromCT << Tuple2.mapSecond r) pairs
      )
    | Expr.EFeatureFlag (id, cond, caseA, caseB) ->
      RT.EFeatureFlag(id, r cond, r caseA, r caseB)


  let rec toCT (e : RT.Expr) : Expr.T =
    let r = toCT

    match e with
    | RT.EBlank id -> Expr.EBlank id
    | RT.ECharacter (id, char) -> Expr.ECharacter(id, char)
    | RT.EInteger (id, num) -> Expr.EInteger(id, num)
    | RT.EString (id, str) -> Expr.EString(id, str)
    | RT.EFloat (id, f) -> Expr.EFloat(id, f)

    | RT.EBool (id, b) -> Expr.EBool(id, b)
    | RT.ENull id -> Expr.ENull id
    | RT.EVariable (id, var) -> Expr.EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> Expr.EFieldAccess(id, r obj, fieldname)
    | RT.ELambda (id, vars, body) -> Expr.ELambda(id, vars, r body)
    | RT.ELet (id, lhs, rhs, body) -> Expr.ELet(id, lhs, r rhs, r body)
    | RT.EIf (id, cond, thenExpr, elseExpr) ->
      Expr.EIf(id, r cond, r thenExpr, r elseExpr)
    | RT.EApply (id, expr, exprs, pipe, ster) ->
      Expr.EApply(id, r expr, List.map r exprs, pipeFromRT pipe, sterFromRT ster)
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
        List.map (Tuple2.mapFirst Pattern.toCT << Tuple2.mapSecond r) pairs
      )
    | RT.EFeatureFlag (id, cond, caseA, caseB) ->
      Expr.EFeatureFlag(id, r cond, r caseA, r caseB)

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
    | Dval.DStr s -> RT.DStr s
    | Dval.DChar c -> RT.DChar c
    | Dval.DInt i -> RT.DInt i
    | Dval.DBool b -> RT.DBool b
    | Dval.DFloat f -> RT.DFloat f
    | Dval.DNull -> RT.DNull
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
    | Dval.DHttpResponse (Dval.Redirect url) -> RT.DHttpResponse(RT.Redirect url)
    | Dval.DHttpResponse (Dval.Response (code, headers, hdv)) ->
      RT.DHttpResponse(RT.Response(code, headers, r hdv))
    | Dval.DList l -> RT.DList(List.map r l)
    | Dval.DTuple (first, second, theRest) ->
      RT.DTuple(r first, r second, List.map r theRest)
    | Dval.DObj o -> RT.DObj(Map.map r o)
    | Dval.DOption None -> RT.DOption None
    | Dval.DOption (Some dv) -> RT.DOption(Some(r dv))
    | Dval.DResult (Ok dv) -> RT.DResult(Ok(r dv))
    | Dval.DResult (Error dv) -> RT.DResult(Error(r dv))
    | Dval.DErrorRail dv -> RT.DErrorRail(r dv)
    | Dval.DBytes bytes -> RT.DBytes bytes


  let rec toCT (dv : RT.Dval) : Dval.T =
    let r = toCT

    match dv with
    | RT.DStr s -> Dval.DStr s
    | RT.DChar c -> Dval.DChar c
    | RT.DInt i -> Dval.DInt i
    | RT.DBool b -> Dval.DBool b
    | RT.DFloat f -> Dval.DFloat f
    | RT.DNull -> Dval.DNull
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
    | RT.DHttpResponse (RT.Redirect url) -> Dval.DHttpResponse(Dval.Redirect url)
    | RT.DHttpResponse (RT.Response (code, headers, hdv)) ->
      Dval.DHttpResponse(Dval.Response(code, headers, r hdv))
    | RT.DList l -> Dval.DList(List.map r l)
    | RT.DTuple (first, second, theRest) ->
      Dval.DTuple(r first, r second, List.map r theRest)
    | RT.DObj o -> Dval.DObj(Map.map r o)
    | RT.DOption None -> Dval.DOption None
    | RT.DOption (Some dv) -> Dval.DOption(Some(r dv))
    | RT.DResult (Ok dv) -> Dval.DResult(Ok(r dv))
    | RT.DResult (Error dv) -> Dval.DResult(Error(r dv))
    | RT.DErrorRail dv -> Dval.DErrorRail(r dv)
    | RT.DBytes bytes -> Dval.DBytes bytes
