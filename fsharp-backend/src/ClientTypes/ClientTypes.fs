/// Types used in many APIs
module ClientTypes

open Prelude
open Tablecloth

// ProgramTypes can be used in APIs, because that's kinda what they do. But
// RuntimeTypes are designed for the interpreter, and it's important that we're able
// to change the types without affecting APIs. So we add types here that we can
// standardize in the API without affecting the RuntimeTypes.

module RT = LibExecution.RuntimeTypes

module FQFnName =
  type UserFnName = string

  module PackageFnName =
    type T =
      { owner : string
        package : string
        module_ : string
        function_ : string
        version : int }

    let toRT (name : T) : RT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

    let fromRT (name : RT.FQFnName.PackageFnName) : T =
      { owner = name.owner
        package = name.package
        module_ = name.module_
        function_ = name.function_
        version = name.version }

  module StdlibFnName =
    type T = { module_ : string; function_ : string; version : int }

    let toRT (name : T) : RT.FQFnName.StdlibFnName =
      { module_ = name.module_; function_ = name.function_; version = name.version }

    let fromRT (name : RT.FQFnName.StdlibFnName) : T =
      { module_ = name.module_; function_ = name.function_; version = name.version }

  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName.T
    | Package of PackageFnName.T

  let toRT (fqfn : T) : RT.FQFnName.T =
    match fqfn with
    | User u -> RT.FQFnName.User u
    | Stdlib fn -> RT.FQFnName.Stdlib(StdlibFnName.toRT fn)
    | Package p -> RT.FQFnName.Package(PackageFnName.toRT p)

  let fromRT (fqfn : RT.FQFnName.T) : T =
    match fqfn with
    | RT.FQFnName.User u -> User u
    | RT.FQFnName.Stdlib fn -> Stdlib(StdlibFnName.fromRT fn)
    | RT.FQFnName.Package p -> Package(PackageFnName.fromRT p)


module DType =
  type T =
    | TInt
    | TFloat
    | TBool
    | TNull
    | TStr
    | TList of T
    | TTuple of T * T * List<T>
    | TDict of T
    | TIncomplete
    | TError
    | THttpResponse of T
    | TDB of T
    | TDate
    | TChar
    | TPassword
    | TUuid
    | TOption of T
    | TErrorRail
    | TUserType of string * int
    | TBytes
    | TResult of T * T
    | TVariable of string
    | TFn of List<T> * T
    | TRecord of List<string * T>

  let rec fromRT (t : RT.DType) =
    let r = fromRT
    let rl = List.map fromRT
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

module Expr =
  type T =
    | EInteger of id * int64
    | EBool of id * bool
    | EString of id * string
    | ECharacter of id * string
    | EFloat of id * double
    | ENull of id
    | EBlank of id
    | ELet of id * string * T * T
    | EIf of id * T * T * T
    | ELambda of id * List<id * string> * T
    | EFieldAccess of id * T * string
    | EVariable of id * string
    | EApply of id * T * List<T> * IsInPipe * SendToRail
    | EFQFnValue of id * FQFnName.T
    | EList of id * List<T>
    | ETuple of id * T * T * List<T>
    | ERecord of id * List<string * T>
    | EConstructor of id * string * List<T>
    | EMatch of id * T * List<Pattern * T>
    | EFeatureFlag of id * T * T * T

  and SendToRail =
    | Rail
    | NoRail

  and IsInPipe =
    | InPipe of id
    | NotInPipe

  and Pattern =
    | PVariable of id * string
    | PConstructor of id * string * List<Pattern>
    | PInteger of id * int64
    | PBool of id * bool
    | PCharacter of id * string
    | PString of id * string
    | PFloat of id * double
    | PNull of id
    | PBlank of id

  let pipeToRT (pipe : IsInPipe) : RT.IsInPipe =
    match pipe with
    | InPipe (id) -> RT.InPipe(id)
    | NotInPipe -> RT.NotInPipe

  let pipeFromRT (pipe : RT.IsInPipe) : IsInPipe =
    match pipe with
    | RT.InPipe (id) -> InPipe(id)
    | RT.NotInPipe -> NotInPipe


  let sterToRT (ster : SendToRail) : RT.SendToRail =
    match ster with
    | Rail -> RT.Rail
    | NoRail -> RT.NoRail

  let sterFromRT (ster : RT.SendToRail) : SendToRail =
    match ster with
    | RT.Rail -> Rail
    | RT.NoRail -> NoRail

  let rec patternToRT (p : Pattern) : RT.Pattern =
    match p with
    | PVariable (id, str) -> RT.PVariable(id, str)
    | PConstructor (id, name, pats) ->
      RT.PConstructor(id, name, List.map patternToRT pats)
    | PInteger (id, i) -> RT.PInteger(id, i)
    | PBool (id, b) -> RT.PBool(id, b)
    | PCharacter (id, c) -> RT.PCharacter(id, c)
    | PString (id, s) -> RT.PString(id, s)
    | PFloat (id, f) -> RT.PFloat(id, f)
    | PNull id -> RT.PNull id
    | PBlank id -> RT.PBlank id

  let rec patternFromRT (p : RT.Pattern) : Pattern =
    match p with
    | RT.PVariable (id, str) -> PVariable(id, str)
    | RT.PConstructor (id, name, pats) ->
      PConstructor(id, name, List.map patternFromRT pats)
    | RT.PInteger (id, i) -> PInteger(id, i)
    | RT.PBool (id, b) -> PBool(id, b)
    | RT.PCharacter (id, c) -> PCharacter(id, c)
    | RT.PString (id, s) -> PString(id, s)
    | RT.PFloat (id, f) -> PFloat(id, f)
    | RT.PNull id -> PNull id
    | RT.PBlank id -> PBlank id

  let rec toRT (e : T) : RT.Expr =
    match e with
    | EBlank id -> RT.EBlank id
    | ECharacter (id, char) -> RT.ECharacter(id, char)
    | EInteger (id, num) -> RT.EInteger(id, num)
    | EString (id, str) -> RT.EString(id, str)
    | EFloat (id, f) -> RT.EFloat(id, f)

    | EBool (id, b) -> RT.EBool(id, b)
    | ENull id -> RT.ENull id
    | EVariable (id, var) -> RT.EVariable(id, var)
    | EFieldAccess (id, obj, fieldname) -> RT.EFieldAccess(id, toRT obj, fieldname)
    | ELambda (id, vars, body) -> RT.ELambda(id, vars, toRT body)
    | ELet (id, lhs, rhs, body) -> RT.ELet(id, lhs, toRT rhs, toRT body)
    | EIf (id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, toRT cond, toRT thenExpr, toRT elseExpr)
    | EApply (id, expr, exprs, pipe, ster) ->
      RT.EApply(id, toRT expr, List.map toRT exprs, pipeToRT pipe, sterToRT ster)
    | EFQFnValue (id, name) -> RT.EFQFnValue(id, FQFnName.toRT name)
    | EList (id, exprs) -> RT.EList(id, List.map toRT exprs)
    | ETuple (id, first, second, theRest) ->
      RT.ETuple(id, toRT first, toRT second, List.map toRT theRest)
    | ERecord (id, pairs) -> RT.ERecord(id, List.map (Tuple2.mapSecond toRT) pairs)
    | EConstructor (id, name, exprs) ->
      RT.EConstructor(id, name, List.map toRT exprs)
    | EMatch (id, mexpr, pairs) ->
      RT.EMatch(
        id,
        toRT mexpr,
        List.map (Tuple2.mapFirst patternToRT << Tuple2.mapSecond toRT) pairs
      )
    | EFeatureFlag (id, cond, caseA, caseB) ->
      RT.EFeatureFlag(id, toRT cond, toRT caseA, toRT caseB)


  let rec fromRT (e : RT.Expr) : T =
    match e with
    | RT.EBlank id -> EBlank id
    | RT.ECharacter (id, char) -> ECharacter(id, char)
    | RT.EInteger (id, num) -> EInteger(id, num)
    | RT.EString (id, str) -> EString(id, str)
    | RT.EFloat (id, f) -> EFloat(id, f)

    | RT.EBool (id, b) -> EBool(id, b)
    | RT.ENull id -> ENull id
    | RT.EVariable (id, var) -> EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> EFieldAccess(id, fromRT obj, fieldname)
    | RT.ELambda (id, vars, body) -> ELambda(id, vars, fromRT body)
    | RT.ELet (id, lhs, rhs, body) -> ELet(id, lhs, fromRT rhs, fromRT body)
    | RT.EIf (id, cond, thenExpr, elseExpr) ->
      EIf(id, fromRT cond, fromRT thenExpr, fromRT elseExpr)
    | RT.EApply (id, expr, exprs, pipe, ster) ->
      EApply(
        id,
        fromRT expr,
        List.map fromRT exprs,
        pipeFromRT pipe,
        sterFromRT ster
      )
    | RT.EFQFnValue (id, name) -> EFQFnValue(id, FQFnName.fromRT name)
    | RT.EList (id, exprs) -> EList(id, List.map fromRT exprs)
    | RT.ETuple (id, first, second, theRest) ->
      ETuple(id, fromRT first, fromRT second, List.map fromRT theRest)
    | RT.ERecord (id, pairs) -> ERecord(id, List.map (Tuple2.mapSecond fromRT) pairs)
    | RT.EConstructor (id, name, exprs) ->
      EConstructor(id, name, List.map fromRT exprs)
    | RT.EMatch (id, mexpr, pairs) ->
      EMatch(
        id,
        fromRT mexpr,
        List.map (Tuple2.mapFirst patternFromRT << Tuple2.mapSecond fromRT) pairs
      )
    | RT.EFeatureFlag (id, cond, caseA, caseB) ->
      EFeatureFlag(id, fromRT cond, fromRT caseA, fromRT caseB)

module Dval =

  type DvalSource =
    | SourceNone
    | SourceID of tlid * id

  and Symtable = Map<string, T>

  and LambdaImpl =
    { parameters : List<id * string>
      symtable : Symtable
      body : Expr.T }

  and FnValImpl =
    | Lambda of LambdaImpl
    | FnName of FQFnName.T

  and DHTTP =
    | Redirect of string
    | Response of int64 * List<string * string> * T

  and T =
    | DInt of int64
    | DFloat of double
    | DBool of bool
    | DNull
    | DStr of string
    | DChar of string
    | DList of List<T>
    | DTuple of T * T * List<T>
    | DFnVal of FnValImpl // See docs/dblock-serialization.md
    | DObj of Map<string, T>
    | DError of DvalSource * string
    | DIncomplete of DvalSource
    | DErrorRail of T
    | DHttpResponse of DHTTP
    | DDB of string
    | DDate of NodaTime.LocalDateTime
    | DPassword of byte array // We are allowed serialize this here, so don't use the Password type which doesn't deserialize
    | DUuid of System.Guid
    | DOption of Option<T>
    | DResult of Result<T, T>
    | DBytes of byte array

  let rec toRT (dv : T) : RT.Dval =
    match dv with
    | DStr s -> RT.DStr s
    | DChar c -> RT.DChar c
    | DInt i -> RT.DInt i
    | DBool b -> RT.DBool b
    | DFloat f -> RT.DFloat f
    | DNull -> RT.DNull
    | DFnVal (Lambda (impl)) ->
      let symtable = Map.map toRT impl.symtable
      RT.DFnVal(
        RT.Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.toRT impl.body }
      )
    | DFnVal (FnName (name)) -> RT.DFnVal(RT.FnName(FQFnName.toRT name))
    | DIncomplete SourceNone -> RT.DIncomplete RT.SourceNone
    | DIncomplete (SourceID (tlid, id)) -> RT.DIncomplete(RT.SourceID(tlid, id))
    | DError (SourceNone, msg) -> RT.DError(RT.SourceNone, msg)
    | DError (SourceID (tlid, id), msg) -> RT.DError(RT.SourceID(tlid, id), msg)
    | DDate d -> RT.DDate d
    | DDB name -> RT.DDB name
    | DUuid uuid -> RT.DUuid uuid
    | DPassword pw -> RT.DPassword(Password pw)
    | DHttpResponse (Redirect url) -> RT.DHttpResponse(RT.Redirect url)
    | DHttpResponse (Response (code, headers, hdv)) ->
      RT.DHttpResponse(RT.Response(code, headers, toRT hdv))
    | DList l -> RT.DList(List.map toRT l)
    | DTuple (first, second, theRest) ->
      RT.DTuple(toRT first, toRT second, List.map toRT theRest)
    | DObj o -> RT.DObj(Map.map toRT o)
    | DOption None -> RT.DOption None
    | DOption (Some dv) -> RT.DOption(Some(toRT dv))
    | DResult (Ok dv) -> RT.DResult(Ok(toRT dv))
    | DResult (Error dv) -> RT.DResult(Error(toRT dv))
    | DErrorRail dv -> RT.DErrorRail(toRT dv)
    | DBytes bytes -> RT.DBytes bytes


  let rec fromRT (dv : RT.Dval) : T =
    match dv with
    | RT.DStr s -> DStr s
    | RT.DChar c -> DChar c
    | RT.DInt i -> DInt i
    | RT.DBool b -> DBool b
    | RT.DFloat f -> DFloat f
    | RT.DNull -> DNull
    | RT.DFnVal (RT.Lambda (impl)) ->
      let symtable = Map.map fromRT impl.symtable
      DFnVal(
        Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.fromRT impl.body }
      )
    | RT.DFnVal (RT.FnName (name)) -> DFnVal(FnName(FQFnName.fromRT name))
    | RT.DIncomplete RT.SourceNone -> DIncomplete SourceNone
    | RT.DIncomplete (RT.SourceID (tlid, id)) -> DIncomplete(SourceID(tlid, id))
    | RT.DError (RT.SourceNone, msg) -> DError(SourceNone, msg)
    | RT.DError (RT.SourceID (tlid, id), msg) -> DError(SourceID(tlid, id), msg)
    | RT.DDate d -> DDate d
    | RT.DDB name -> DDB name
    | RT.DUuid uuid -> DUuid uuid
    | RT.DPassword (Password pw) -> DPassword pw
    | RT.DHttpResponse (RT.Redirect url) -> DHttpResponse(Redirect url)
    | RT.DHttpResponse (RT.Response (code, headers, hdv)) ->
      DHttpResponse(Response(code, headers, fromRT hdv))
    | RT.DList l -> DList(List.map fromRT l)
    | RT.DTuple (first, second, theRest) ->
      DTuple(fromRT first, fromRT second, List.map fromRT theRest)
    | RT.DObj o -> DObj(Map.map fromRT o)
    | RT.DOption None -> DOption None
    | RT.DOption (Some dv) -> DOption(Some(fromRT dv))
    | RT.DResult (Ok dv) -> DResult(Ok(fromRT dv))
    | RT.DResult (Error dv) -> DResult(Error(fromRT dv))
    | RT.DErrorRail dv -> DErrorRail(fromRT dv)
    | RT.DBytes bytes -> DBytes bytes

/// All these types correspond to AnalysisTypes.fs in the backend, but should
/// directly match AnalysisTypes in the client
module Analysis =
  module PT = LibExecution.ProgramTypes
  module AT = LibExecution.AnalysisTypes

  module ExecutionResult =
    type T =
      | ExecutedResult of Dval.T
      | NonExecutedResult of Dval.T

    let fromAT (er : AT.ExecutionResult) : T =
      match er with
      | AT.ExecutedResult (dv) -> ExecutedResult(Dval.fromRT dv)
      | AT.NonExecutedResult (dv) -> NonExecutedResult(Dval.fromRT dv)

  module AnalysisResults =
    type T = Dictionary.T<id, ExecutionResult.T>

    let fromAT (ar : AT.AnalysisResults) : T =
      ar
      |> Dictionary.toList
      |> List.map (fun (k, v) -> (k, ExecutionResult.fromAT v))
      |> Dictionary.fromList


  type InputVars = List<string * Dval.T>

  type FunctionArgHash = string
  type HashVersion = int
  type FnName = string
  type FunctionResult = FnName * id * FunctionArgHash * HashVersion * Dval.T

  type TraceID = System.Guid

  module TraceData =
    type T =
      { input : InputVars
        timestamp : NodaTime.Instant
        function_results : List<FunctionResult> }

    let toAT (td : T) : AT.TraceData =
      { input = List.map (fun (k, v) -> (k, Dval.toRT v)) td.input
        timestamp = td.timestamp
        function_results =
          List.map
            (fun (name, id, hash, version, dval) ->
              (name, id, hash, version, Dval.toRT dval))
            td.function_results }


  type Trace = TraceID * TraceData.T

  type HandlerAnalysisParam =
    { handler : PT.Handler.T
      traceID : TraceID
      traceData : TraceData.T
      dbs : List<PT.DB.T>
      userFns : list<PT.UserFunction.T>
      userTypes : list<PT.UserType.T>
      secrets : list<PT.Secret.T> }

  type FunctionAnalysisParam =
    { func : PT.UserFunction.T
      traceID : TraceID
      traceData : TraceData.T
      dbs : List<PT.DB.T>
      userFns : list<PT.UserFunction.T>
      userTypes : list<PT.UserType.T>
      secrets : list<PT.Secret.T> }

  type PerformAnalysisParams =
    | AnalyzeHandler of HandlerAnalysisParam
    | AnalyzeFunction of FunctionAnalysisParam

  type AnalysisEnvelope = TraceID * AnalysisResults.T
