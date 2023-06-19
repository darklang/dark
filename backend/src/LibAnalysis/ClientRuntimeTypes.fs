/// Runtime Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// See `RuntimeTypes.fs` for documentation of these types.
module LibAnalysis.ClientRuntimeTypes

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes

module FQName =

  type CTBuiltIn<'name> = { modules : List<string>; name : 'name; version : int }
  type CTUserProgram<'name> = { modules : List<string>; name : 'name; version : int }
  type CTPackage<'name> =
    { owner : string; modules : NonEmptyList<string>; name : 'name; version : int }

  // Use the CT prefix to avoid name clashes with the final T enum
  module CTBuiltIn =
    let toCT
      (f : 'ptName -> 'ctName)
      (name : RT.FQName.BuiltIn<'ptName>)
      : CTBuiltIn<'ctName> =
      { modules = name.modules; name = f name.name; version = name.version }

    let fromCT
      (f : 'ctName -> 'ptName)
      (name : CTBuiltIn<'ctName>)
      : RT.FQName.BuiltIn<'ptName> =
      { modules = name.modules; name = f name.name; version = name.version }

  module CTUserProgram =
    let toCT
      (f : 'ptName -> 'ctName)
      (name : RT.FQName.UserProgram<'ptName>)
      : CTUserProgram<'ctName> =
      { modules = name.modules; name = f name.name; version = name.version }

    let fromCT
      (f : 'ctName -> 'ptName)
      (name : CTUserProgram<'ctName>)
      : RT.FQName.UserProgram<'ptName> =
      { modules = name.modules; name = f name.name; version = name.version }

  module CTPackage =
    let toCT
      (f : 'ptName -> 'ctName)
      (name : RT.FQName.Package<'ptName>)
      : CTPackage<'ctName> =
      { owner = name.owner
        modules = name.modules
        name = f name.name
        version = name.version }

    let fromCT
      (f : 'ctName -> 'ptName)
      (name : CTPackage<'ctName>)
      : RT.FQName.Package<'ptName> =
      { owner = name.owner
        modules = name.modules
        name = f name.name
        version = name.version }

  type T<'name> =
    | BuiltIn of CTBuiltIn<'name>
    | UserProgram of CTUserProgram<'name>
    | Package of CTPackage<'name>



  let toCT (f : 'ptName -> 'ctName) (name : RT.FQName.T<'ptName>) : T<'ctName> =
    match name with
    | RT.FQName.BuiltIn name -> BuiltIn(CTBuiltIn.toCT f name)
    | RT.FQName.UserProgram name -> UserProgram(CTUserProgram.toCT f name)
    | RT.FQName.Package name -> Package(CTPackage.toCT f name)

  let fromCT (f : 'ctName -> 'ptName) (name : T<'ctName>) : RT.FQName.T<'ptName> =
    match name with
    | BuiltIn name -> RT.FQName.BuiltIn(CTBuiltIn.fromCT f name)
    | UserProgram name -> RT.FQName.UserProgram(CTUserProgram.fromCT f name)
    | Package name -> RT.FQName.Package(CTPackage.fromCT f name)

module TypeName =
  type Name = TypeName of string
  type T = FQName.T<Name>
  type BuiltIn = FQName.CTBuiltIn<Name>
  type UserProgram = FQName.CTUserProgram<Name>
  type Package = FQName.CTPackage<Name>

  module CTBuiltIn =
    let toCT (name : RT.TypeName.BuiltIn) : BuiltIn =
      FQName.CTBuiltIn.toCT (fun (RT.TypeName.TypeName name) -> TypeName name) name

    let fromCT (name : BuiltIn) : RT.TypeName.BuiltIn =
      FQName.CTBuiltIn.fromCT (fun (TypeName name) -> RT.TypeName.TypeName name) name

  module CTUserProgram =
    let toCT (name : RT.TypeName.UserProgram) : UserProgram =
      FQName.CTUserProgram.toCT
        (fun (RT.TypeName.TypeName name) -> TypeName name)
        name

    let fromCT (name : UserProgram) : RT.TypeName.UserProgram =
      FQName.CTUserProgram.fromCT
        (fun (TypeName name) -> RT.TypeName.TypeName name)
        name

  module CTPackage =
    let toCT (name : RT.TypeName.Package) : Package =
      FQName.CTPackage.toCT (fun (RT.TypeName.TypeName name) -> TypeName name) name

    let fromCT (name : Package) : RT.TypeName.Package =
      FQName.CTPackage.fromCT (fun (TypeName name) -> RT.TypeName.TypeName name) name

  let toCT (name : RT.TypeName.T) : T =
    FQName.toCT (fun (RT.TypeName.TypeName name) -> TypeName name) name

  let fromCT (name : T) : RT.TypeName.T =
    FQName.fromCT (fun (TypeName name) -> RT.TypeName.TypeName name) name


module FnName =
  type Name = FnName of string
  type T = FQName.T<Name>
  type BuiltIn = FQName.CTBuiltIn<Name>
  type UserProgram = FQName.CTUserProgram<Name>
  type Package = FQName.CTPackage<Name>


  module CTBuiltIn =
    let toCT (name : RT.FnName.BuiltIn) : BuiltIn =
      FQName.CTBuiltIn.toCT (fun (RT.FnName.FnName name) -> FnName name) name

    let fromCT (name : BuiltIn) : RT.FnName.BuiltIn =
      FQName.CTBuiltIn.fromCT (fun (FnName name) -> RT.FnName.FnName name) name

  module CTUserProgram =
    let toCT (name : RT.FnName.UserProgram) : UserProgram =
      FQName.CTUserProgram.toCT (fun (RT.FnName.FnName name) -> FnName name) name

    let fromCT (name : UserProgram) : RT.FnName.UserProgram =
      FQName.CTUserProgram.fromCT (fun (FnName name) -> RT.FnName.FnName name) name

  module CTPackage =
    let toCT (name : RT.FnName.Package) : Package =
      FQName.CTPackage.toCT (fun (RT.FnName.FnName name) -> FnName name) name

    let fromCT (name : Package) : RT.FnName.Package =
      FQName.CTPackage.fromCT (fun (FnName name) -> RT.FnName.FnName name) name


  let toCT (name : RT.FnName.T) : T =
    FQName.toCT (fun (RT.FnName.FnName name) -> FnName name) name

  let fromCT (name : T) : RT.FnName.T =
    FQName.fromCT (fun (FnName name) -> RT.FnName.FnName name) name


type TypeReference =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TString
  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TDict of TypeReference
  | TDB of TypeReference
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TOption of TypeReference
  | TCustomType of TypeName.T * typeArgs : List<TypeReference>
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
    | RT.TTuple(t1, t2, ts) -> TTuple(r t1, r t2, rl ts)
    | RT.TDict t -> TDict(r t)
    | RT.TDB t -> TDB(r t)
    | RT.TDateTime -> TDateTime
    | RT.TChar -> TChar
    | RT.TPassword -> TPassword
    | RT.TUuid -> TUuid
    | RT.TOption t -> TOption(r t)
    | RT.TCustomType(typeName, typeArgs) ->
      TCustomType(TypeName.toCT typeName, List.map r typeArgs)
    | RT.TBytes -> TBytes
    | RT.TResult(ok, error) -> TResult(r ok, r error)
    | RT.TVariable(name) -> TVariable(name)
    | RT.TFn(ts, returnType) -> TFn(rl ts, r returnType)

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
    | TTuple(t1, t2, ts) -> RT.TTuple(r t1, r t2, rl ts)
    | TDict t -> RT.TDict(r t)
    | TDB t -> RT.TDB(r t)
    | TDateTime -> RT.TDateTime
    | TChar -> RT.TChar
    | TPassword -> RT.TPassword
    | TUuid -> RT.TUuid
    | TOption t -> RT.TOption(r t)
    | TCustomType(t, typeArgs) ->
      RT.TCustomType(TypeName.fromCT t, List.map r typeArgs)
    | TBytes -> RT.TBytes
    | TResult(ok, error) -> RT.TResult(r ok, r error)
    | TVariable(name) -> RT.TVariable(name)
    | TFn(ts, returnType) -> RT.TFn(rl ts, r returnType)



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
    | LPVariable(id, str) -> RT.LPVariable(id, str)
    | LPTuple(id, first, second, theRest) ->
      RT.LPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)

  let rec toCT (p : RT.LetPattern) : LetPattern =
    match p with
    | RT.LPVariable(id, str) -> LPVariable(id, str)
    | RT.LPTuple(id, first, second, theRest) ->
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
  | MPListCons of id * head : MatchPattern * tail : MatchPattern

module MatchPattern =
  let rec fromCT (p : MatchPattern) : RT.MatchPattern =
    let r = fromCT
    match p with
    | MPVariable(id, str) -> RT.MPVariable(id, str)
    | MPEnum(id, caseName, fieldPats) ->
      RT.MPEnum(id, caseName, List.map r fieldPats)
    | MPInt(id, i) -> RT.MPInt(id, i)
    | MPBool(id, b) -> RT.MPBool(id, b)
    | MPChar(id, c) -> RT.MPChar(id, c)
    | MPString(id, s) -> RT.MPString(id, s)
    | MPFloat(id, f) -> RT.MPFloat(id, f)
    | MPUnit id -> RT.MPUnit id
    | MPTuple(id, first, second, theRest) ->
      RT.MPTuple(id, r first, r second, List.map r theRest)
    | MPList(id, pats) -> RT.MPList(id, List.map r pats)
    | MPListCons(id, head, tail) -> RT.MPListCons(id, r head, r tail)

  let rec toCT (p : RT.MatchPattern) : MatchPattern =
    let r = toCT
    match p with
    | RT.MPVariable(id, str) -> MPVariable(id, str)
    | RT.MPEnum(id, caseName, fieldPats) ->
      MPEnum(id, caseName, List.map r fieldPats)
    | RT.MPInt(id, i) -> MPInt(id, i)
    | RT.MPBool(id, b) -> MPBool(id, b)
    | RT.MPChar(id, c) -> MPChar(id, c)
    | RT.MPString(id, s) -> MPString(id, s)
    | RT.MPFloat(id, f) -> MPFloat(id, f)
    | RT.MPUnit id -> MPUnit id
    | RT.MPTuple(id, first, second, theRest) ->
      MPTuple(id, r first, r second, List.map r theRest)
    | RT.MPList(id, pats) -> MPList(id, List.map r pats)
    | RT.MPListCons(id, head, tail) -> MPListCons(id, r head, r tail)

module CustomType =
  // TYPESCLEANUP support type parameters
  type RecordField = { name : string; typ : TypeReference; description : string }

  module RecordField =
    let fromCT (f : RecordField) : RT.CustomType.RecordField =
      { name = f.name
        typ = TypeReference.fromCT f.typ
        description = f.description }

    let toCT (f : RT.CustomType.RecordField) : RecordField =
      { name = f.name; typ = TypeReference.toCT f.typ; description = f.description }

  type EnumField =
    { typ : TypeReference; label : Option<string>; description : string }

  module EnumField =
    let fromCT (f : EnumField) : RT.CustomType.EnumField =
      { typ = TypeReference.fromCT f.typ
        label = f.label
        description = f.description }

    let toCT (f : RT.CustomType.EnumField) : EnumField =
      { typ = TypeReference.toCT f.typ
        label = f.label
        description = f.description }

  type EnumCase = { name : string; fields : List<EnumField>; description : string }

  module EnumCase =
    let fromCT (c : EnumCase) : RT.CustomType.EnumCase =
      { name = c.name
        fields = List.map EnumField.fromCT c.fields
        description = c.description }

    let toCT (c : RT.CustomType.EnumCase) : EnumCase =
      { name = c.name
        fields = List.map EnumField.toCT c.fields
        description = c.description }

  type T =
    | Alias of TypeReference
    | Record of firstField : RecordField * additionalFields : List<RecordField>
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>

  let fromCT (t : T) : RT.CustomType.T =
    match t with
    | Alias typ -> RT.CustomType.Alias(TypeReference.fromCT typ)
    | Record(firstField, additionalFields) ->
      RT.CustomType.Record(
        RecordField.fromCT firstField,
        List.map RecordField.fromCT additionalFields
      )
    | Enum(firstCase, additionalCases) ->
      RT.CustomType.Enum(
        EnumCase.fromCT firstCase,
        List.map EnumCase.fromCT additionalCases
      )

  let toCT (t : RT.CustomType.T) : T =
    match t with
    | RT.CustomType.Alias typ -> Alias(TypeReference.toCT typ)
    | RT.CustomType.Record(firstField, additionalFields) ->
      Record(RecordField.toCT firstField, List.map RecordField.toCT additionalFields)
    | RT.CustomType.Enum(firstCase, additionalCases) ->
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
    | ERecord of id * typeName : TypeName.T * fields : List<string * T>
    | ERecordUpdate of id * record : T * updates : List<string * T>
    | EDict of id * List<string * T>
    | EEnum of id * typeName : TypeName.T * caseName : string * fields : List<T>
    | EMatch of id * T * List<MatchPattern * T>
    | EAnd of id * T * T
    | EOr of id * T * T

  and StringSegment =
    | StringText of string
    | StringInterpolation of T

  and FnTarget =
    | FnTargetName of FnName.T
    | FnTargetExpr of T

  let rec fromCT (e : T) : RT.Expr =
    let r = fromCT

    match e with
    | EChar(id, char) -> RT.EChar(id, char)
    | EInt(id, num) -> RT.EInt(id, num)
    | EString(id, segment) -> RT.EString(id, List.map stringSegmentToRT segment)
    | EFloat(id, f) -> RT.EFloat(id, f)

    | EBool(id, b) -> RT.EBool(id, b)
    | EUnit id -> RT.EUnit id
    | EVariable(id, var) -> RT.EVariable(id, var)
    | EFieldAccess(id, obj, fieldname) -> RT.EFieldAccess(id, r obj, fieldname)
    | ELambda(id, vars, body) -> RT.ELambda(id, vars, r body)
    | ELet(id, pat, rhs, body) -> RT.ELet(id, LetPattern.fromCT pat, r rhs, r body)
    | EIf(id, cond, thenExpr, elseExpr) -> RT.EIf(id, r cond, r thenExpr, r elseExpr)
    | EApply(id, target, typeArgs, exprs) ->
      RT.EApply(
        id,
        fnTargetFromCT target,
        List.map TypeReference.fromCT typeArgs,
        List.map r exprs
      )
    | EList(id, exprs) -> RT.EList(id, List.map r exprs)
    | ETuple(id, first, second, theRest) ->
      RT.ETuple(id, r first, r second, List.map r theRest)
    | ERecord(id, typeName, fields) ->
      RT.ERecord(id, TypeName.fromCT typeName, List.map (Tuple2.mapSecond r) fields)
    | ERecordUpdate(id, record, updates) ->
      RT.ERecordUpdate(id, r record, List.map (Tuple2.mapSecond r) updates)
    | EEnum(id, typeName, caseName, fields) ->
      RT.EEnum(id, TypeName.fromCT typeName, caseName, List.map r fields)
    | EMatch(id, mexpr, pairs) ->
      RT.EMatch(
        id,
        r mexpr,
        List.map (Tuple2.mapFirst MatchPattern.fromCT << Tuple2.mapSecond r) pairs
      )
    | EAnd(id, left, right) -> RT.EAnd(id, r left, r right)
    | EOr(id, left, right) -> RT.EOr(id, r left, r right)
    | EDict(id, pairs) -> RT.EDict(id, List.map (Tuple2.mapSecond r) pairs)

  and stringSegmentToRT (segment : StringSegment) : RT.StringSegment =
    match segment with
    | StringText text -> RT.StringText text
    | StringInterpolation expr -> RT.StringInterpolation(fromCT expr)

  and fnTargetFromCT (target : FnTarget) : RT.FnTarget =
    match target with
    | FnTargetName name -> RT.FnTargetName(FnName.fromCT name)
    | FnTargetExpr expr -> RT.FnTargetExpr(fromCT expr)


  let rec toCT (e : RT.Expr) : T =
    let r = toCT

    match e with
    | RT.EChar(id, char) -> EChar(id, char)
    | RT.EInt(id, num) -> EInt(id, num)
    | RT.EString(id, str) -> EString(id, List.map stringSegmentToCT str)
    | RT.EFloat(id, f) -> EFloat(id, f)

    | RT.EBool(id, b) -> EBool(id, b)
    | RT.EUnit id -> EUnit id
    | RT.EVariable(id, var) -> EVariable(id, var)
    | RT.EFieldAccess(id, obj, fieldname) -> EFieldAccess(id, r obj, fieldname)
    | RT.ELambda(id, vars, body) -> ELambda(id, vars, r body)
    | RT.ELet(id, pat, rhs, body) -> ELet(id, LetPattern.toCT pat, r rhs, r body)
    | RT.EIf(id, cond, thenExpr, elseExpr) -> EIf(id, r cond, r thenExpr, r elseExpr)
    | RT.EApply(id, target, typeArgs, exprs) ->
      EApply(
        id,
        fnTargetToCT target,
        List.map TypeReference.toCT typeArgs,
        List.map r exprs
      )
    | RT.EList(id, exprs) -> EList(id, List.map r exprs)
    | RT.ETuple(id, first, second, theRest) ->
      ETuple(id, r first, r second, List.map r theRest)
    | RT.ERecord(id, typeName, fields) ->
      ERecord(id, TypeName.toCT typeName, List.map (Tuple2.mapSecond r) fields)
    | RT.ERecordUpdate(id, record, updates) ->
      ERecordUpdate(id, r record, List.map (Tuple2.mapSecond r) updates)
    | RT.EEnum(id, typeName, caseName, fields) ->
      EEnum(id, TypeName.toCT typeName, caseName, List.map r fields)
    | RT.EMatch(id, mexpr, pairs) ->
      EMatch(
        id,
        r mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toCT << Tuple2.mapSecond r) pairs
      )
    | RT.EAnd(id, left, right) -> EAnd(id, r left, r right)
    | RT.EOr(id, left, right) -> EOr(id, r left, r right)
    | RT.EDict(id, pairs) -> EDict(id, List.map (Tuple2.mapSecond r) pairs)

  and stringSegmentToCT (segment : RT.StringSegment) : StringSegment =
    match segment with
    | RT.StringText text -> StringText text
    | RT.StringInterpolation expr -> StringInterpolation(toCT expr)

  and fnTargetToCT (target : RT.FnTarget) : FnTarget =
    match target with
    | RT.FnTargetName name -> FnTargetName(FnName.toCT name)
    | RT.FnTargetExpr expr -> FnTargetExpr(toCT expr)




module Dval =
  type DvalSource =
    | SourceNone
    | SourceID of tlid * id

  module DvalSource =
    let fromCT (s : DvalSource) : RT.DvalSource =
      match s with
      | SourceNone -> RT.SourceNone
      | SourceID(tlid, id) -> RT.SourceID(tlid, id)

    let toCT (s : RT.DvalSource) : DvalSource =
      match s with
      | RT.SourceNone -> SourceNone
      | RT.SourceID(tlid, id) -> SourceID(tlid, id)


  type Symtable = Map<string, T>

  and LambdaImpl =
    { parameters : List<id * string>; symtable : Symtable; body : Expr.T }

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
    | DError of DvalSource * string
    | DIncomplete of DvalSource
    | DDB of string
    | DDateTime of NodaTime.LocalDateTime
    | DPassword of Password
    | DUuid of System.Guid
    | DOption of Option<T>
    | DResult of Result<T, T>
    | DBytes of byte array
    | DRecord of TypeName.T * Map<string, T>
    | DEnum of TypeName.T * caseName : string * List<T>

  let rec fromCT (dv : T) : RT.Dval =
    let r = fromCT

    match dv with
    | DString s -> RT.DString s
    | DChar c -> RT.DChar c
    | DInt i -> RT.DInt i
    | DBool b -> RT.DBool b
    | DFloat f -> RT.DFloat f
    | DUnit -> RT.DUnit
    | DFnVal(Lambda(impl)) ->
      let symtable = Map.map r impl.symtable
      RT.DFnVal(
        RT.Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.fromCT impl.body }
      )
    | DIncomplete(source) -> RT.DIncomplete(DvalSource.fromCT source)
    | DError(source, msg) -> RT.DError(DvalSource.fromCT source, msg)
    | DDateTime d -> RT.DDateTime d
    | DDB name -> RT.DDB name
    | DUuid uuid -> RT.DUuid uuid
    | DPassword pw -> RT.DPassword(pw)
    | DList list -> RT.DList(List.map r list)
    | DTuple(first, second, theRest) ->
      RT.DTuple(r first, r second, List.map r theRest)
    | DDict o -> RT.DDict(Map.map r o)
    | DRecord(typeName, o) -> RT.DRecord(TypeName.fromCT typeName, Map.map r o)
    | DOption None -> RT.DOption None
    | DOption(Some dv) -> RT.DOption(Some(r dv))
    | DResult(Ok dv) -> RT.DResult(Ok(r dv))
    | DResult(Error dv) -> RT.DResult(Error(r dv))
    | DBytes bytes -> RT.DBytes bytes
    | DEnum(typeName, caseName, fields) ->
      RT.DEnum(TypeName.fromCT typeName, caseName, List.map r fields)

  and toCT (dv : RT.Dval) : T =
    let r = toCT

    match dv with
    | RT.DString s -> DString s
    | RT.DChar c -> DChar c
    | RT.DInt i -> DInt i
    | RT.DBool b -> DBool b
    | RT.DFloat f -> DFloat f
    | RT.DUnit -> DUnit
    | RT.DFnVal(RT.Lambda(impl)) ->
      let symtable = Map.map r impl.symtable
      DFnVal(
        Lambda
          { parameters = impl.parameters
            symtable = symtable
            body = Expr.toCT impl.body }
      )
    | RT.DIncomplete(source) -> DIncomplete(DvalSource.toCT source)
    | RT.DError(source, msg) -> DError(DvalSource.toCT source, msg)
    | RT.DDateTime d -> DDateTime d
    | RT.DDB name -> DDB name
    | RT.DUuid uuid -> DUuid uuid
    | RT.DPassword(Password pw) -> DPassword(Password pw)
    | RT.DList l -> DList(List.map r l)
    | RT.DTuple(first, second, theRest) ->
      DTuple(r first, r second, List.map r theRest)
    | RT.DDict o -> DDict(Map.map r o)
    | RT.DRecord(typeName, o) -> DRecord(TypeName.toCT typeName, Map.map r o)
    | RT.DOption None -> DOption None
    | RT.DOption(Some dv) -> DOption(Some(r dv))
    | RT.DResult(Ok dv) -> DResult(Ok(r dv))
    | RT.DResult(Error dv) -> DResult(Error(r dv))
    | RT.DBytes bytes -> DBytes bytes
    | RT.DEnum(typeName, caseName, fields) -> DUnit // todo


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
      | HTTP(path, method) -> RT.Handler.HTTP(path, method)
      | Worker name -> RT.Handler.Worker name
      | Cron(name, interval) -> RT.Handler.Cron(name, CronInterval.fromCT interval)
      | REPL name -> RT.Handler.REPL name

    let toCT (spec : RT.Handler.Spec) : Spec =
      match spec with
      | RT.Handler.HTTP(path, method) -> HTTP(path, method)
      | RT.Handler.Worker name -> Worker name
      | RT.Handler.Cron(name, interval) -> Cron(name, CronInterval.toCT interval)
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
  type T =
    { tlid : tlid
      name : TypeName.UserProgram
      typeParams : List<string>
      definition : CustomType.T }

  let fromCT (userType : T) : RT.UserType.T =
    { tlid = userType.tlid
      name = TypeName.CTUserProgram.fromCT userType.name
      typeParams = userType.typeParams
      definition = CustomType.fromCT userType.definition }

  let toCT (userType : RT.UserType.T) : T =
    { tlid = userType.tlid
      name = TypeName.CTUserProgram.toCT userType.name
      typeParams = userType.typeParams
      definition = CustomType.toCT userType.definition }

module UserFunction =
  type Parameter = { name : string; typ : TypeReference }

  module Parameter =
    let fromCT (param : Parameter) : RT.UserFunction.Parameter =
      { name = param.name; typ = TypeReference.fromCT param.typ }

    let toCT (param : RT.UserFunction.Parameter) : Parameter =
      { name = param.name; typ = TypeReference.toCT param.typ }

  type T =
    { tlid : tlid
      name : FnName.UserProgram
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      body : Expr.T }

  let fromCT (userFn : T) : RT.UserFunction.T =
    { tlid = userFn.tlid
      name = FnName.CTUserProgram.fromCT userFn.name
      typeParams = userFn.typeParams
      parameters = List.map Parameter.fromCT userFn.parameters
      returnType = TypeReference.fromCT userFn.returnType
      body = Expr.fromCT userFn.body }

  let toCT (userFn : RT.UserFunction.T) : T =
    { tlid = userFn.tlid
      name = FnName.CTUserProgram.toCT userFn.name
      typeParams = userFn.typeParams
      parameters = List.map Parameter.toCT userFn.parameters
      returnType = TypeReference.toCT userFn.returnType
      body = Expr.toCT userFn.body }


type Secret = { name : string; value : string }

module Secret =
  let fromCT (s : Secret) : RT.Secret.T = { name = s.name; value = s.value }

  let toCT (s : RT.Secret.T) : Secret = { name = s.name; value = s.value }


module Package =
  type Parameter = { name : string; typ : TypeReference }

  module Parameter =
    let fromCT (p : Parameter) : RT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.fromCT p.typ }

    let toCT (p : RT.PackageFn.Parameter) : Parameter =
      { name = p.name; typ = TypeReference.toCT p.typ }

  type Fn =
    { name : FnName.Package
      tlid : tlid
      body : Expr.T
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference }

  module Fn =
    let fromCT (fn : Fn) : RT.PackageFn.T =
      { name = FnName.CTPackage.fromCT fn.name
        tlid = fn.tlid
        body = Expr.fromCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.fromCT fn.parameters
        returnType = TypeReference.fromCT fn.returnType }

    let toCT (fn : RT.PackageFn.T) : Fn =
      { name = FnName.CTPackage.toCT fn.name
        tlid = fn.tlid
        body = Expr.toCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.toCT fn.parameters
        returnType = TypeReference.toCT fn.returnType }
