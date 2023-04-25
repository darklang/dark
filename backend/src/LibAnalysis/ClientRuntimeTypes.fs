/// Runtime Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// See `RuntimeTypes.fs` for documentation of these types.
module LibAnalysis.ClientRuntimeTypes

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes


module FQTypeName =
  type StdlibTypeName = { modules : List<string>; typ : string; version : int }

  module StdlibTypeName =
    let fromCT (s : StdlibTypeName) : RT.FQTypeName.StdlibTypeName =
      { modules = s.modules; typ = s.typ; version = s.version }

    let toCT (s : RT.FQTypeName.StdlibTypeName) : StdlibTypeName =
      { modules = s.modules; typ = s.typ; version = s.version }


  type UserTypeName = { modules : List<string>; typ : string; version : int }

  module UserTypeName =
    let fromCT (u : UserTypeName) : RT.FQTypeName.UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }


    let toCT (u : RT.FQTypeName.UserTypeName) : UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }


  type PackageTypeName =
    { owner : string
      package : string
      modules : NonEmptyList<string>
      typ : string
      version : int }

  module PackageTypeName =
    let fromCT (p : PackageTypeName) : RT.FQTypeName.PackageTypeName =
      { owner = p.owner
        package = p.package
        modules = p.modules
        typ = p.typ
        version = p.version }

    let toCT (p : RT.FQTypeName.PackageTypeName) : PackageTypeName =
      { owner = p.owner
        package = p.package
        modules = p.modules
        typ = p.typ
        version = p.version }

  type T =
    | Stdlib of StdlibTypeName
    | User of UserTypeName
    | Package of PackageTypeName

  let fromCT (t : T) : RT.FQTypeName.T =
    match t with
    | Stdlib s -> RT.FQTypeName.Stdlib(StdlibTypeName.fromCT s)
    | User u -> RT.FQTypeName.User(UserTypeName.fromCT u)
    | Package p -> RT.FQTypeName.Package(PackageTypeName.fromCT p)

  let toCT (t : RT.FQTypeName.T) : T =
    match t with
    | RT.FQTypeName.Stdlib s -> Stdlib(StdlibTypeName.toCT s)
    | RT.FQTypeName.User u -> User(UserTypeName.toCT u)
    | RT.FQTypeName.Package p -> Package(PackageTypeName.toCT p)


module FQFnName =
  type UserFnName = { modules : List<string>; function_ : string; version : int }

  module UserFnName =
    let fromCT (name : UserFnName) : RT.FQFnName.UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

    let toCT (name : RT.FQFnName.UserFnName) : UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

  type PackageFnName =
    { owner : string
      package : string
      modules : NonEmptyList<string>
      function_ : string
      version : int }

  module PackageFnName =
    let fromCT (name : PackageFnName) : RT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        modules = name.modules
        function_ = name.function_
        version = name.version }

    let toCT (name : RT.FQFnName.PackageFnName) : PackageFnName =
      { owner = name.owner
        package = name.package
        modules = name.modules
        function_ = name.function_
        version = name.version }


  type StdlibFnName = { modules : List<string>; function_ : string; version : int }

  module StdlibFnName =
    let fromCT (name : StdlibFnName) : RT.FQFnName.StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

    let toCT (name : RT.FQFnName.StdlibFnName) : StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }


  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

  let fromCT (fqfn : T) : RT.FQFnName.T =
    match fqfn with
    | Stdlib fn -> RT.FQFnName.Stdlib(StdlibFnName.fromCT fn)
    | User u -> RT.FQFnName.User(UserFnName.fromCT u)
    | Package p -> RT.FQFnName.Package(PackageFnName.fromCT p)

  let toCT (fqfn : RT.FQFnName.T) : T =
    match fqfn with
    | RT.FQFnName.Stdlib fn -> Stdlib(StdlibFnName.toCT fn)
    | RT.FQFnName.User u -> User(UserFnName.toCT u)
    | RT.FQFnName.Package p -> Package(PackageFnName.toCT p)


type TypeReference =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TString
  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TDict of TypeReference
  | THttpResponse of TypeReference
  | TDB of TypeReference
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TOption of TypeReference
  | TCustomType of FQTypeName.T * typeArgs : List<TypeReference>
  | TBytes
  | TResult of TypeReference * TypeReference
  | TVariable of string
  | TFn of List<TypeReference> * TypeReference

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



type LetPattern =
  | LPVariable of id * name : string
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

module LetPattern =
  let rec fromCT (p : LetPattern) : RT.LetPattern =
    match p with
    | LPVariable (id, str) -> RT.LPVariable(id, str)
    | LPTuple (id, first, second, theRest) ->
      RT.LPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)

  let rec toCT (p : RT.LetPattern) : LetPattern =
    match p with
    | RT.LPVariable (id, str) -> LPVariable(id, str)
    | RT.LPTuple (id, first, second, theRest) ->
      LPTuple(id, toCT first, toCT second, List.map toCT theRest)


type MatchPattern =
  | MPVariable of id * string
  | MPEnum of id * caseName : string * fieldPatterns : List<MatchPattern>
  | MPInt of id * int64
  | MPBool of id * bool
  | MPChar of id * string
  | MPString of id * string
  | MPFloat of id * double
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>
  | MPList of id * List<MatchPattern>

module MatchPattern =
  let rec fromCT (p : MatchPattern) : RT.MatchPattern =
    let r = fromCT
    match p with
    | MPVariable (id, str) -> RT.MPVariable(id, str)
    | MPEnum (id, caseName, fieldPats) ->
      RT.MPEnum(id, caseName, List.map r fieldPats)
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
    | RT.MPEnum (id, caseName, fieldPats) ->
      MPEnum(id, caseName, List.map r fieldPats)
    | RT.MPInt (id, i) -> MPInt(id, i)
    | RT.MPBool (id, b) -> MPBool(id, b)
    | RT.MPChar (id, c) -> MPChar(id, c)
    | RT.MPString (id, s) -> MPString(id, s)
    | RT.MPFloat (id, f) -> MPFloat(id, f)
    | RT.MPUnit id -> MPUnit id
    | RT.MPTuple (id, first, second, theRest) ->
      MPTuple(id, r first, r second, List.map r theRest)
    | RT.MPList (id, pats) -> MPList(id, List.map r pats)

module CustomType =
  // TYPESCLEANUP support type parameters
  type RecordField = { name : string; typ : TypeReference }

  module RecordField =
    let fromCT (f : RecordField) : RT.CustomType.RecordField =
      { name = f.name; typ = TypeReference.fromCT f.typ }

    let toCT (f : RT.CustomType.RecordField) : RecordField =
      { name = f.name; typ = TypeReference.toCT f.typ }

  type EnumField = { typ : TypeReference; label : Option<string> }

  module EnumField =
    let fromCT (f : EnumField) : RT.CustomType.EnumField =
      { typ = TypeReference.fromCT f.typ; label = f.label }

    let toCT (f : RT.CustomType.EnumField) : EnumField =
      { typ = TypeReference.toCT f.typ; label = f.label }

  type EnumCase = { name : string; fields : List<EnumField> }

  module EnumCase =
    let fromCT (c : EnumCase) : RT.CustomType.EnumCase =
      { name = c.name; fields = List.map EnumField.fromCT c.fields }

    let toCT (c : RT.CustomType.EnumCase) : EnumCase =
      { name = c.name; fields = List.map EnumField.toCT c.fields }

  type T =
    | Record of firstField : RecordField * additionalFields : List<RecordField>
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>

  let fromCT (t : T) : RT.CustomType.T =
    match t with
    | Record (firstField, additionalFields) ->
      RT.CustomType.Record(
        RecordField.fromCT firstField,
        List.map RecordField.fromCT additionalFields
      )
    | Enum (firstCase, additionalCases) ->
      RT.CustomType.Enum(
        EnumCase.fromCT firstCase,
        List.map EnumCase.fromCT additionalCases
      )

  let toCT (t : RT.CustomType.T) : T =
    match t with
    | RT.CustomType.Record (firstField, additionalFields) ->
      Record(RecordField.toCT firstField, List.map RecordField.toCT additionalFields)
    | RT.CustomType.Enum (firstCase, additionalCases) ->
      Enum(EnumCase.toCT firstCase, List.map EnumCase.toCT additionalCases)



module Expr =
  type T =
    | EInt of id * int64
    | EBool of id * bool
    | EString of id * List<StringSegment>
    | EChar of id * string
    | EFloat of id * double
    | EUnit of id
    | ELet of id * LetPattern * T * T
    | EIf of id * T * T * T
    | ELambda of id * List<id * string> * T
    | EFieldAccess of id * T * string
    | EVariable of id * string
    | EApply of id * FnTarget * List<TypeReference> * List<T>
    | EList of id * List<T>
    | ETuple of id * T * T * List<T>
    | ERecord of id * typeName : FQTypeName.T * fields : List<string * T>
    | EDict of id * List<string * T>
    | EEnum of id * typeName : FQTypeName.T * caseName : string * fields : List<T>
    | EMatch of id * T * List<MatchPattern * T>
    | EAnd of id * T * T
    | EOr of id * T * T

  and StringSegment =
    | StringText of string
    | StringInterpolation of T

  and FnTarget =
    | FnName of FQFnName.T
    | FnTargetExpr of T

  let rec fromCT (e : T) : RT.Expr =
    let r = fromCT

    match e with
    | EChar (id, char) -> RT.EChar(id, char)
    | EInt (id, num) -> RT.EInt(id, num)
    | EString (id, segment) -> RT.EString(id, List.map stringSegmentToRT segment)
    | EFloat (id, f) -> RT.EFloat(id, f)

    | EBool (id, b) -> RT.EBool(id, b)
    | EUnit id -> RT.EUnit id
    | EVariable (id, var) -> RT.EVariable(id, var)
    | EFieldAccess (id, obj, fieldname) -> RT.EFieldAccess(id, r obj, fieldname)
    | ELambda (id, vars, body) -> RT.ELambda(id, vars, r body)
    | ELet (id, pat, rhs, body) -> RT.ELet(id, LetPattern.fromCT pat, r rhs, r body)
    | EIf (id, cond, thenExpr, elseExpr) ->
      RT.EIf(id, r cond, r thenExpr, r elseExpr)
    | EApply (id, target, typeArgs, exprs) ->
      RT.EApply(
        id,
        fnTargetFromCT target,
        List.map TypeReference.fromCT typeArgs,
        List.map r exprs
      )
    | EList (id, exprs) -> RT.EList(id, List.map r exprs)
    | ETuple (id, first, second, theRest) ->
      RT.ETuple(id, r first, r second, List.map r theRest)
    | ERecord (id, typeName, fields) ->
      RT.ERecord(
        id,
        FQTypeName.fromCT typeName,
        List.map (Tuple2.mapSecond r) fields
      )
    | EEnum (id, typeName, caseName, fields) ->
      RT.EEnum(id, FQTypeName.fromCT typeName, caseName, List.map r fields)
    | EMatch (id, mexpr, pairs) ->
      RT.EMatch(
        id,
        r mexpr,
        List.map (Tuple2.mapFirst MatchPattern.fromCT << Tuple2.mapSecond r) pairs
      )
    | EAnd (id, left, right) -> RT.EAnd(id, r left, r right)
    | EOr (id, left, right) -> RT.EOr(id, r left, r right)
    | EDict (id, pairs) -> RT.EDict(id, List.map (Tuple2.mapSecond r) pairs)

  and stringSegmentToRT (segment : StringSegment) : RT.StringSegment =
    match segment with
    | StringText text -> RT.StringText text
    | StringInterpolation expr -> RT.StringInterpolation(fromCT expr)

  and fnTargetFromCT (target : FnTarget) : RT.FnTarget =
    match target with
    | FnName name -> RT.FnName(FQFnName.fromCT name)
    | FnTargetExpr expr -> RT.FnTargetExpr(fromCT expr)


  let rec toCT (e : RT.Expr) : T =
    let r = toCT

    match e with
    | RT.EChar (id, char) -> EChar(id, char)
    | RT.EInt (id, num) -> EInt(id, num)
    | RT.EString (id, str) -> EString(id, List.map stringSegmentToCT str)
    | RT.EFloat (id, f) -> EFloat(id, f)

    | RT.EBool (id, b) -> EBool(id, b)
    | RT.EUnit id -> EUnit id
    | RT.EVariable (id, var) -> EVariable(id, var)
    | RT.EFieldAccess (id, obj, fieldname) -> EFieldAccess(id, r obj, fieldname)
    | RT.ELambda (id, vars, body) -> ELambda(id, vars, r body)
    | RT.ELet (id, pat, rhs, body) -> ELet(id, LetPattern.toCT pat, r rhs, r body)
    | RT.EIf (id, cond, thenExpr, elseExpr) ->
      EIf(id, r cond, r thenExpr, r elseExpr)
    | RT.EApply (id, target, typeArgs, exprs) ->
      EApply(
        id,
        fnTargetToCT target,
        List.map TypeReference.toCT typeArgs,
        List.map r exprs
      )
    | RT.EList (id, exprs) -> EList(id, List.map r exprs)
    | RT.ETuple (id, first, second, theRest) ->
      ETuple(id, r first, r second, List.map r theRest)
    | RT.ERecord (id, typeName, fields) ->
      ERecord(id, FQTypeName.toCT typeName, List.map (Tuple2.mapSecond r) fields)
    | RT.EEnum (id, typeName, caseName, fields) ->
      EEnum(id, FQTypeName.toCT typeName, caseName, List.map r fields)
    | RT.EMatch (id, mexpr, pairs) ->
      EMatch(
        id,
        r mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toCT << Tuple2.mapSecond r) pairs
      )
    | RT.EAnd (id, left, right) -> EAnd(id, r left, r right)
    | RT.EOr (id, left, right) -> EOr(id, r left, r right)
    | RT.EDict (id, pairs) -> EDict(id, List.map (Tuple2.mapSecond r) pairs)

  and stringSegmentToCT (segment : RT.StringSegment) : StringSegment =
    match segment with
    | RT.StringText text -> StringText text
    | RT.StringInterpolation expr -> StringInterpolation(toCT expr)

  and fnTargetToCT (target : RT.FnTarget) : FnTarget =
    match target with
    | RT.FnName name -> FnName(FQFnName.toCT name)
    | RT.FnTargetExpr expr -> FnTargetExpr(toCT expr)




module Dval =
  type DvalSource =
    | SourceNone
    | SourceID of tlid * id

  module DvalSource =
    let fromCT (s : DvalSource) : RT.DvalSource =
      match s with
      | SourceNone -> RT.SourceNone
      | SourceID (tlid, id) -> RT.SourceID(tlid, id)

    let toCT (s : RT.DvalSource) : DvalSource =
      match s with
      | RT.SourceNone -> SourceNone
      | RT.SourceID (tlid, id) -> SourceID(tlid, id)


  type Symtable = Map<string, T>

  and LambdaImpl =
    { parameters : List<id * string>
      symtable : Symtable
      body : Expr.T }

  and FnValImpl = Lambda of LambdaImpl

  and T =
    | DInt of int64
    | DFloat of double
    | DBool of bool
    | DUnit
    | DString of string
    | DChar of string
    | DList of List<T>
    | DTuple of T * T * List<T>
    | DFnVal of FnValImpl // See docs/dblock-serialization.md
    | DDict of Map<string, T>
    | DRecord of Map<string, T>
    | DError of DvalSource * string
    | DIncomplete of DvalSource
    | DHttpResponse of int64 * List<string * string> * T
    | DDB of string
    | DDateTime of NodaTime.LocalDateTime
    | DPassword of Password
    | DUuid of System.Guid
    | DOption of Option<T>
    | DResult of Result<T, T>
    | DBytes of byte array
    | DEnum of typeName : FQTypeName.T * caseName : string * fields : List<T>

  let rec fromCT (dv : T) : RT.Dval =
    let r = fromCT

    match dv with
    | DString s -> RT.DString s
    | DChar c -> RT.DChar c
    | DInt i -> RT.DInt i
    | DBool b -> RT.DBool b
    | DFloat f -> RT.DFloat f
    | DUnit -> RT.DUnit
    | DFnVal (Lambda (impl)) ->
      let symtable = Map.map r impl.symtable
      RT.DFnVal(
        RT.Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.fromCT impl.body }
      )
    | DIncomplete (source) -> RT.DIncomplete(DvalSource.fromCT source)
    | DError (source, msg) -> RT.DError(DvalSource.fromCT source, msg)
    | DDateTime d -> RT.DDateTime d
    | DDB name -> RT.DDB name
    | DUuid uuid -> RT.DUuid uuid
    | DPassword pw -> RT.DPassword(pw)
    | DHttpResponse (id, pairs, dval) -> RT.DHttpResponse(id, pairs, r dval)
    | DList list -> RT.DList(List.map r list)
    | DTuple (first, second, theRest) ->
      RT.DTuple(r first, r second, List.map r theRest)
    | DDict o -> RT.DDict(Map.map r o)
    | DRecord o -> RT.DRecord(Map.map r o)
    | DOption None -> RT.DOption None
    | DOption (Some dv) -> RT.DOption(Some(r dv))
    | DResult (Ok dv) -> RT.DResult(Ok(r dv))
    | DResult (Error dv) -> RT.DResult(Error(r dv))
    | DBytes bytes -> RT.DBytes bytes
    | DEnum (typeName, caseName, fields) ->
      RT.DEnum(FQTypeName.fromCT typeName, caseName, List.map r fields)

  and toCT (dv : RT.Dval) : T =
    let r = toCT

    match dv with
    | RT.DString s -> DString s
    | RT.DChar c -> DChar c
    | RT.DInt i -> DInt i
    | RT.DBool b -> DBool b
    | RT.DFloat f -> DFloat f
    | RT.DUnit -> DUnit
    | RT.DFnVal (RT.Lambda (impl)) ->
      let symtable = Map.map r impl.symtable
      DFnVal(
        Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.toCT impl.body }
      )
    | RT.DIncomplete (source) -> DIncomplete(DvalSource.toCT source)
    | RT.DError (source, msg) -> DError(DvalSource.toCT source, msg)
    | RT.DDateTime d -> DDateTime d
    | RT.DDB name -> DDB name
    | RT.DUuid uuid -> DUuid uuid
    | RT.DPassword (Password pw) -> DPassword(Password pw)
    | RT.DHttpResponse (code, headers, dval) ->
      DHttpResponse(code, headers, toCT dval)
    | RT.DList l -> DList(List.map r l)
    | RT.DTuple (first, second, theRest) ->
      DTuple(r first, r second, List.map r theRest)
    | RT.DDict o -> DDict(Map.map r o)
    | RT.DRecord o -> DRecord(Map.map r o)
    | RT.DOption None -> DOption None
    | RT.DOption (Some dv) -> DOption(Some(r dv))
    | RT.DResult (Ok dv) -> DResult(Ok(r dv))
    | RT.DResult (Error dv) -> DResult(Error(r dv))
    | RT.DBytes bytes -> DBytes bytes
    | RT.DEnum (typeName, caseName, fields) -> DUnit // todo


module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  module CronInterval =
    let fromCT (interval : CronInterval) : RT.Handler.CronInterval =
      match interval with
      | EveryDay -> RT.Handler.EveryDay
      | EveryWeek -> RT.Handler.EveryWeek
      | EveryFortnight -> RT.Handler.EveryFortnight
      | EveryHour -> RT.Handler.EveryHour
      | Every12Hours -> RT.Handler.Every12Hours
      | EveryMinute -> RT.Handler.EveryMinute

    let toCT (interval : RT.Handler.CronInterval) : CronInterval =
      match interval with
      | RT.Handler.EveryDay -> EveryDay
      | RT.Handler.EveryWeek -> EveryWeek
      | RT.Handler.EveryFortnight -> EveryFortnight
      | RT.Handler.EveryHour -> EveryHour
      | RT.Handler.Every12Hours -> Every12Hours
      | RT.Handler.EveryMinute -> EveryMinute


  type Spec =
    | HTTP of path : string * method : string
    | Worker of name : string
    | Cron of name : string * interval : CronInterval
    | REPL of name : string

  module Spec =
    let fromCT (spec : Spec) : RT.Handler.Spec =
      match spec with
      | HTTP (path, method) -> RT.Handler.HTTP(path, method)
      | Worker name -> RT.Handler.Worker name
      | Cron (name, interval) -> RT.Handler.Cron(name, CronInterval.fromCT interval)
      | REPL name -> RT.Handler.REPL name

    let toCT (spec : RT.Handler.Spec) : Spec =
      match spec with
      | RT.Handler.HTTP (path, method) -> HTTP(path, method)
      | RT.Handler.Worker name -> Worker name
      | RT.Handler.Cron (name, interval) -> Cron(name, CronInterval.toCT interval)
      | RT.Handler.REPL name -> REPL name

  type T = { tlid : tlid; ast : Expr.T; spec : Spec }

  let fromCT (handler : T) : RT.Handler.T =
    { tlid = handler.tlid
      ast = Expr.fromCT handler.ast
      spec = Spec.fromCT handler.spec }

  let toCT (handler : RT.Handler.T) : T =
    { tlid = handler.tlid
      ast = Expr.toCT handler.ast
      spec = Spec.toCT handler.spec }

module DB =
  type T = { tlid : tlid; name : string; typ : TypeReference; version : int }

  let fromCT (db : T) : RT.DB.T =
    { tlid = db.tlid
      name = db.name
      typ = TypeReference.fromCT db.typ
      version = db.version }

  let toCT (db : RT.DB.T) : T =
    { tlid = db.tlid
      name = db.name
      typ = TypeReference.toCT db.typ
      version = db.version }

module UserType =
  type T = { tlid : tlid; name : FQTypeName.UserTypeName; definition : CustomType.T }

  let fromCT (userType : T) : RT.UserType.T =
    { tlid = userType.tlid
      name = FQTypeName.UserTypeName.fromCT userType.name
      definition = CustomType.fromCT userType.definition }

  let toCT (userType : RT.UserType.T) : T =
    { tlid = userType.tlid
      name = FQTypeName.UserTypeName.toCT userType.name
      definition = CustomType.toCT userType.definition }

module UserFunction =
  type Parameter = { name : string; typ : TypeReference; description : string }

  module Parameter =
    let fromCT (param : Parameter) : RT.UserFunction.Parameter =
      { name = param.name
        typ = TypeReference.fromCT param.typ
        description = param.description }

    let toCT (param : RT.UserFunction.Parameter) : Parameter =
      { name = param.name
        typ = TypeReference.toCT param.typ
        description = param.description }

  type T =
    { tlid : tlid
      name : FQFnName.UserFnName
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      infix : bool
      body : Expr.T }

  let fromCT (userFn : T) : RT.UserFunction.T =
    { tlid = userFn.tlid
      name = FQFnName.UserFnName.fromCT userFn.name
      typeParams = userFn.typeParams
      parameters = List.map Parameter.fromCT userFn.parameters
      returnType = TypeReference.fromCT userFn.returnType
      description = userFn.description
      infix = userFn.infix
      body = Expr.fromCT userFn.body }

  let toCT (userFn : RT.UserFunction.T) : T =
    { tlid = userFn.tlid
      name = FQFnName.UserFnName.toCT userFn.name
      typeParams = userFn.typeParams
      parameters = List.map Parameter.toCT userFn.parameters
      returnType = TypeReference.toCT userFn.returnType
      description = userFn.description
      infix = userFn.infix
      body = Expr.toCT userFn.body }


type Secret = { name : string; value : string }

module Secret =
  let fromCT (s : Secret) : RT.Secret.T = { name = s.name; value = s.value }

  let toCT (s : RT.Secret.T) : Secret = { name = s.name; value = s.value }


module Package =
  type Parameter = { name : string; typ : TypeReference; description : string }

  module Parameter =
    let fromCT (p : Parameter) : RT.Package.Parameter =
      { name = p.name
        typ = TypeReference.fromCT p.typ
        description = p.description }

    let toCT (p : RT.Package.Parameter) : Parameter =
      { name = p.name; typ = TypeReference.toCT p.typ; description = p.description }

  type Fn =
    { name : FQFnName.PackageFnName
      body : Expr.T
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      deprecated : bool }

  module Fn =
    let fromCT (fn : Fn) : RT.Package.Fn =
      { name = FQFnName.PackageFnName.fromCT fn.name
        body = Expr.fromCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.fromCT fn.parameters
        returnType = TypeReference.fromCT fn.returnType
        description = fn.description
        deprecated = fn.deprecated }

    let toCT (fn : RT.Package.Fn) : Fn =
      { name = FQFnName.PackageFnName.toCT fn.name
        body = Expr.toCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.toCT fn.parameters
        returnType = TypeReference.toCT fn.returnType
        description = fn.description
        deprecated = fn.deprecated }
