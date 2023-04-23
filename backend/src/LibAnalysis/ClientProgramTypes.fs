/// Program Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// These should all directly match ProgramTypes in the client
/// See ProgramTypes.fs for documentation of these types
module LibAnalysis.ClientProgramTypes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

module FQTypeName =

  type StdlibTypeName = { modules : List<string>; typ : string; version : int }

  module StdlibTypeName =
    let fromCT (t : StdlibTypeName) : PT.FQTypeName.StdlibTypeName =
      { modules = t.modules; typ = t.typ; version = t.version }

    let toCT (t : PT.FQTypeName.StdlibTypeName) : StdlibTypeName =
      { modules = t.modules; typ = t.typ; version = t.version }


  type UserTypeName = { modules : List<string>; typ : string; version : int }

  module UserTypeName =
    let fromCT (u : UserTypeName) : PT.FQTypeName.UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }

    let toCT (u : PT.FQTypeName.UserTypeName) : UserTypeName =
      { modules = u.modules; typ = u.typ; version = u.version }


  type PackageTypeName =
    { owner : string
      package : string
      modules : NonEmptyList<string>
      typ : string
      version : int }

  module PackageTypeName =
    let fromCT (t : PackageTypeName) : PT.FQTypeName.PackageTypeName =
      { owner = t.owner
        package = t.package
        modules = t.modules
        typ = t.typ
        version = t.version }

    let toCT (t : PT.FQTypeName.PackageTypeName) : PackageTypeName =
      { owner = t.owner
        package = t.package
        modules = t.modules
        typ = t.typ
        version = t.version }


  type T =
    | Stdlib of StdlibTypeName
    | User of UserTypeName
    | Package of PackageTypeName

  let fromCT (t : T) : PT.FQTypeName.T =
    match t with
    | Stdlib t -> PT.FQTypeName.Stdlib(StdlibTypeName.fromCT t)
    | User u -> PT.FQTypeName.User(UserTypeName.fromCT u)
    | Package p -> PT.FQTypeName.Package(PackageTypeName.fromCT p)

  let toCT (t : PT.FQTypeName.T) : T =
    match t with
    | PT.FQTypeName.Stdlib t -> Stdlib(StdlibTypeName.toCT t)
    | PT.FQTypeName.User u -> User(UserTypeName.toCT u)
    | PT.FQTypeName.Package p -> Package(PackageTypeName.toCT p)


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
  let rec fromCT (dtype : TypeReference) : PT.TypeReference =
    match dtype with
    | TInt -> PT.TInt
    | TFloat -> PT.TFloat
    | TBool -> PT.TBool
    | TUnit -> PT.TUnit
    | TString -> PT.TString
    | TList (t) -> PT.TList(fromCT t)
    | TTuple (first, second, theRest) ->
      PT.TTuple(fromCT first, fromCT second, List.map fromCT theRest)
    | TDict (t) -> PT.TDict(fromCT t)
    | THttpResponse (t) -> PT.THttpResponse(fromCT t)
    | TDB (t) -> PT.TDB(fromCT t)
    | TDateTime -> PT.TDateTime
    | TChar -> PT.TChar
    | TPassword -> PT.TPassword
    | TUuid -> PT.TUuid
    | TOption (t) -> PT.TOption(fromCT t)
    | TCustomType (t, typeArgs) ->
      PT.TCustomType(FQTypeName.fromCT t, List.map fromCT typeArgs)
    | TBytes -> PT.TBytes
    | TResult (ok, err) -> PT.TResult(fromCT ok, fromCT err)
    | TVariable (name) -> PT.TVariable(name)
    | TFn (args, body) -> PT.TFn(List.map fromCT args, fromCT body)

  let rec toCT (dtype : PT.TypeReference) : TypeReference =
    match dtype with
    | PT.TInt -> TInt
    | PT.TFloat -> TFloat
    | PT.TBool -> TBool
    | PT.TUnit -> TUnit
    | PT.TString -> TString
    | PT.TList (t) -> TList(toCT t)
    | PT.TTuple (first, second, theRest) ->
      TTuple(toCT first, toCT second, List.map toCT theRest)
    | PT.TDict (t) -> TDict(toCT t)
    | PT.THttpResponse (t) -> THttpResponse(toCT t)
    | PT.TDB (t) -> TDB(toCT t)
    | PT.TDateTime -> TDateTime
    | PT.TChar -> TChar
    | PT.TPassword -> TPassword
    | PT.TUuid -> TUuid
    | PT.TOption (t) -> TOption(toCT t)
    | PT.TCustomType (t, typeArgs) ->
      TCustomType(FQTypeName.toCT t, List.map toCT typeArgs)
    | PT.TBytes -> TBytes
    | PT.TResult (ok, err) -> TResult(toCT ok, toCT err)
    | PT.TVariable (name) -> TVariable(name)
    | PT.TFn (args, body) -> TFn(List.map toCT args, toCT body)


module FQFnName =
  type StdlibFnName = { modules : List<string>; function_ : string; version : int }

  module StdlibFnName =
    let fromCT (name : StdlibFnName) : PT.FQFnName.StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

    let toCT (name : PT.FQFnName.StdlibFnName) : StdlibFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }


  type UserFnName = { modules : List<string>; function_ : string; version : int }

  module UserFnName =
    let fromCT (name : UserFnName) : PT.FQFnName.UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }

    let toCT (name : PT.FQFnName.UserFnName) : UserFnName =
      { modules = name.modules; function_ = name.function_; version = name.version }


  type PackageFnName =
    { owner : string
      package : string
      modules : NonEmptyList<string>
      function_ : string
      version : int }

  module PackageFnName =
    let fromCT (name : PackageFnName) : PT.FQFnName.PackageFnName =
      { owner = name.owner
        package = name.package
        modules = name.modules
        function_ = name.function_
        version = name.version }

    let toCT (name : PT.FQFnName.PackageFnName) : PackageFnName =
      { owner = name.owner
        package = name.package
        modules = name.modules
        function_ = name.function_
        version = name.version }

  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

  let fromCT (fqfn : T) : PT.FQFnName.T =
    match fqfn with
    | User u -> PT.FQFnName.User(UserFnName.fromCT u)
    | Stdlib fn -> PT.FQFnName.Stdlib(StdlibFnName.fromCT fn)
    | Package p -> PT.FQFnName.Package(PackageFnName.fromCT p)

  let toCT (fqfn : PT.FQFnName.T) : T =
    match fqfn with
    | PT.FQFnName.User u -> User(UserFnName.toCT u)
    | PT.FQFnName.Stdlib fn -> Stdlib(StdlibFnName.toCT fn)
    | PT.FQFnName.Package p -> Package(PackageFnName.toCT p)


type InfixFnName =
  | ArithmeticPlus
  | ArithmeticMinus
  | ArithmeticMultiply
  | ArithmeticDivide
  | ArithmeticModulo
  | ArithmeticPower
  | ComparisonGreaterThan
  | ComparisonGreaterThanOrEqual
  | ComparisonLessThan
  | ComparisonLessThanOrEqual
  | ComparisonEquals
  | ComparisonNotEquals
  | StringConcat

module InfixFnName =
  let fromCT (name : InfixFnName) : PT.InfixFnName =
    match name with
    | ArithmeticPlus -> PT.ArithmeticPlus
    | ArithmeticMinus -> PT.ArithmeticMinus
    | ArithmeticMultiply -> PT.ArithmeticMultiply
    | ArithmeticDivide -> PT.ArithmeticDivide
    | ArithmeticModulo -> PT.ArithmeticModulo
    | ArithmeticPower -> PT.ArithmeticPower
    | ComparisonGreaterThan -> PT.ComparisonGreaterThan
    | ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
    | ComparisonLessThan -> PT.ComparisonLessThan
    | ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
    | ComparisonEquals -> PT.ComparisonEquals
    | ComparisonNotEquals -> PT.ComparisonNotEquals
    | StringConcat -> PT.StringConcat

  let toCT (name : PT.InfixFnName) : InfixFnName =
    match name with
    | PT.ArithmeticPlus -> ArithmeticPlus
    | PT.ArithmeticMinus -> ArithmeticMinus
    | PT.ArithmeticMultiply -> ArithmeticMultiply
    | PT.ArithmeticDivide -> ArithmeticDivide
    | PT.ArithmeticModulo -> ArithmeticModulo
    | PT.ArithmeticPower -> ArithmeticPower
    | PT.ComparisonGreaterThan -> ComparisonGreaterThan
    | PT.ComparisonGreaterThanOrEqual -> ComparisonGreaterThanOrEqual
    | PT.ComparisonLessThan -> ComparisonLessThan
    | PT.ComparisonLessThanOrEqual -> ComparisonLessThanOrEqual
    | PT.ComparisonEquals -> ComparisonEquals
    | PT.ComparisonNotEquals -> ComparisonNotEquals
    | PT.StringConcat -> StringConcat


type LetPattern =
  | LPVariable of id * name : string
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

module LetPattern =
  let rec fromCT (p : LetPattern) : PT.LetPattern =
    match p with
    | LPVariable (id, str) -> PT.LPVariable(id, str)
    | LPTuple (id, first, second, theRest) ->
      PT.LPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)

  let rec toCT (p : PT.LetPattern) : LetPattern =
    match p with
    | PT.LPVariable (id, str) -> LPVariable(id, str)
    | PT.LPTuple (id, first, second, theRest) ->
      LPTuple(id, toCT first, toCT second, List.map toCT theRest)


type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * caseName : string * fieldPatterns : List<MatchPattern>
  | MPInt of id * int64
  | MPBool of id * bool
  | MPChar of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>
  | MPList of id * List<MatchPattern>

module MatchPattern =
  let rec fromCT (pat : MatchPattern) : PT.MatchPattern =
    match pat with
    | MPVariable (id, str) -> PT.MPVariable(id, str)
    | MPConstructor (id, caseName, fieldPats) ->
      PT.MPConstructor(id, caseName, List.map fromCT fieldPats)
    | MPInt (id, i) -> PT.MPInt(id, i)
    | MPBool (id, b) -> PT.MPBool(id, b)
    | MPChar (id, str) -> PT.MPChar(id, str)
    | MPString (id, str) -> PT.MPString(id, str)
    | MPFloat (id, sign, whole, frac) -> PT.MPFloat(id, sign, whole, frac)
    | MPUnit (id) -> PT.MPUnit(id)
    | MPTuple (id, first, second, theRest) ->
      PT.MPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)
    | MPList (id, pats) -> PT.MPList(id, List.map fromCT pats)

  let rec toCT (pat : PT.MatchPattern) : MatchPattern =
    match pat with
    | PT.MPVariable (id, str) -> MPVariable(id, str)
    | PT.MPConstructor (id, caseName, fieldPats) ->
      MPConstructor(id, caseName, List.map toCT fieldPats)
    | PT.MPInt (id, i) -> MPInt(id, i)
    | PT.MPBool (id, b) -> MPBool(id, b)
    | PT.MPChar (id, str) -> MPChar(id, str)
    | PT.MPString (id, str) -> MPString(id, str)
    | PT.MPFloat (id, sign, whole, frac) -> MPFloat(id, sign, whole, frac)
    | PT.MPUnit (id) -> MPUnit(id)
    | PT.MPTuple (id, first, second, theRest) ->
      MPTuple(id, toCT first, toCT second, List.map toCT theRest)
    | PT.MPList (id, pats) -> MPList(id, List.map toCT pats)


type BinaryOperation =
  | BinOpAnd
  | BinOpOr

module BinaryOperation =
  let fromCT (op : BinaryOperation) : PT.BinaryOperation =
    match op with
    | BinOpAnd -> PT.BinOpAnd
    | BinOpOr -> PT.BinOpOr

  let toCT (op : PT.BinaryOperation) : BinaryOperation =
    match op with
    | PT.BinOpAnd -> BinOpAnd
    | PT.BinOpOr -> BinOpOr


type Infix =
  | InfixFnCall of InfixFnName
  | BinOp of BinaryOperation

module Infix =
  let fromCT (infix : Infix) : PT.Infix =
    match infix with
    | InfixFnCall (name) -> PT.InfixFnCall(InfixFnName.fromCT name)
    | BinOp (op) -> PT.BinOp(BinaryOperation.fromCT op)

  let toCT (infix : PT.Infix) : Infix =
    match infix with
    | PT.InfixFnCall (name) -> InfixFnCall(InfixFnName.toCT name)
    | PT.BinOp (op) -> BinOp(BinaryOperation.toCT op)


type Expr =
  | EInt of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | EChar of id * string
  | EFloat of id * Sign * string * string
  | EUnit of id
  | ELet of id * LetPattern * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EInfix of id * Infix * Expr * Expr
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * typeArgs : List<TypeReference> * args : List<Expr>
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | ERecord of id * Option<FQTypeName.T> * List<string * Expr>
  | EDict of id * List<string * Expr>
  | EPipe of id * Expr * Expr * List<Expr>
  | EConstructor of
    id *
    typeName : Option<FQTypeName.T> *
    caseName : string *
    fields : List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>
  | EPipeTarget of id

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

module Expr =
  let rec fromCT (expr : Expr) : PT.Expr =
    match expr with
    | EInt (id, i) -> PT.EInt(id, i)
    | EBool (id, b) -> PT.EBool(id, b)
    | EString (id, segment) -> PT.EString(id, List.map stringSegmentFromCTPT segment)
    | EChar (id, c) -> PT.EChar(id, c)
    | EFloat (id, sign, whole, frac) -> PT.EFloat(id, sign, whole, frac)
    | EUnit (id) -> PT.EUnit(id)
    | ELet (id, pat, expr, body) ->
      PT.ELet(id, LetPattern.fromCT pat, fromCT expr, fromCT body)
    | EIf (id, cond, ifExpr, thenExpr) ->
      PT.EIf(id, fromCT cond, fromCT ifExpr, fromCT thenExpr)
    | EInfix (id, infix, first, second) ->
      PT.EInfix(id, Infix.fromCT (infix), fromCT first, fromCT second)
    | ELambda (id, args, body) -> PT.ELambda(id, args, fromCT body)
    | EFieldAccess (id, expr, fieldName) ->
      PT.EFieldAccess(id, fromCT expr, fieldName)
    | EVariable (id, name) -> PT.EVariable(id, name)
    | EFnCall (id, fnName, typeArgs, args) ->
      PT.EFnCall(
        id,
        FQFnName.fromCT fnName,
        List.map TypeReference.fromCT typeArgs,
        List.map fromCT args
      )
    | EList (id, exprs) -> PT.EList(id, List.map fromCT exprs)
    | ETuple (id, first, second, theRest) ->
      PT.ETuple(id, fromCT first, fromCT second, List.map fromCT theRest)
    | ERecord (id, typeName, fields) ->
      PT.ERecord(
        id,
        Option.map FQTypeName.fromCT typeName,
        fields |> List.map (fun (name, expr) -> (name, fromCT expr))
      )
    | EPipe (id, expr1, expr2, exprs) ->
      PT.EPipe(id, fromCT expr1, fromCT expr2, List.map fromCT exprs)
    | EMatch (id, matchExpr, cases) ->
      PT.EMatch(
        id,
        fromCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.fromCT pat, fromCT expr))
      )
    | EPipeTarget (id) -> PT.EPipeTarget(id)
    | EConstructor (id, typeName, caseName, fields) ->
      PT.Expr.EConstructor(
        id,
        Option.map FQTypeName.fromCT typeName,
        caseName,
        List.map fromCT fields
      )
    | EDict (id, fields) ->
      PT.Expr.EDict(id, fields |> List.map (fun (key, value) -> (key, fromCT value)))

  and stringSegmentFromCTPT (segment : StringSegment) : PT.StringSegment =
    match segment with
    | StringText text -> PT.StringText text
    | StringInterpolation expr -> PT.StringInterpolation(fromCT expr)

  let rec toCT (expr : PT.Expr) : Expr =
    match expr with
    | PT.EInt (id, i) -> EInt(id, i)
    | PT.EBool (id, b) -> EBool(id, b)
    | PT.EString (id, s) -> EString(id, List.map stringSegmentToCT s)
    | PT.EChar (id, c) -> EChar(id, c)
    | PT.EFloat (id, sign, whole, frac) -> EFloat(id, sign, whole, frac)
    | PT.EUnit (id) -> EUnit(id)
    | PT.ELet (id, pat, expr, body) ->
      ELet(id, LetPattern.toCT pat, toCT expr, toCT body)
    | PT.EIf (id, cond, ifExpr, thenExpr) ->
      EIf(id, toCT cond, toCT ifExpr, toCT thenExpr)
    | PT.EInfix (id, PT.InfixFnCall (name), first, second) ->
      EInfix(id, InfixFnCall(InfixFnName.toCT name), toCT first, toCT second)
    | PT.EInfix (id, PT.BinOp op, first, second) ->
      EInfix(id, BinOp(BinaryOperation.toCT op), toCT first, toCT second)
    | PT.ELambda (id, args, body) -> ELambda(id, args, toCT body)
    | PT.EFieldAccess (id, expr, fieldName) -> EFieldAccess(id, toCT expr, fieldName)
    | PT.EVariable (id, name) -> EVariable(id, name)
    | PT.EFnCall (id, fnName, typeArgs, args) ->
      EFnCall(
        id,
        FQFnName.toCT fnName,
        List.map TypeReference.toCT typeArgs,
        List.map toCT args
      )
    | PT.EList (id, exprs) -> EList(id, List.map toCT exprs)
    | PT.ETuple (id, first, second, theRest) ->
      ETuple(id, toCT first, toCT second, List.map toCT theRest)
    | PT.ERecord (id, typeName, fields) ->
      ERecord(
        id,
        Option.map FQTypeName.toCT typeName,
        fields |> List.map (fun (name, expr) -> (name, toCT expr))
      )
    | PT.EPipe (id, expr1, expr2, exprs) ->
      EPipe(id, toCT expr1, toCT expr2, List.map toCT exprs)
    | PT.EConstructor (id, typeName, caseName, fields) ->
      EConstructor(
        id,
        Option.map FQTypeName.toCT typeName,
        caseName,
        List.map toCT fields
      )
    | PT.EMatch (id, matchExpr, cases) ->
      EMatch(
        id,
        toCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.toCT pat, toCT expr))
      )
    | PT.EPipeTarget (id) -> EPipeTarget(id)
    | PT.EDict (id, fields) ->
      EDict(id, fields |> List.map (fun (key, value) -> (key, toCT value)))


  and stringSegmentToCT (segment : PT.StringSegment) : StringSegment =
    match segment with
    | PT.StringText text -> StringText text
    | PT.StringInterpolation expr -> StringInterpolation(toCT expr)


module CustomType =
  type RecordField = { id : id; name : string; typ : TypeReference }

  module RecordField =
    let fromCT (rf : RecordField) : PT.CustomType.RecordField =
      { id = rf.id; name = rf.name; typ = TypeReference.fromCT rf.typ }

    let toCT (rf : PT.CustomType.RecordField) : RecordField =
      { id = rf.id; name = rf.name; typ = TypeReference.toCT rf.typ }

  type EnumField = { id : id; typ : TypeReference; label : Option<string> }

  module EnumField =
    let fromCT (ef : EnumField) : PT.CustomType.EnumField =
      { id = ef.id; typ = TypeReference.fromCT ef.typ; label = ef.label }

    let toCT (ef : PT.CustomType.EnumField) : EnumField =
      { id = ef.id; typ = TypeReference.toCT ef.typ; label = ef.label }

  type EnumCase = { id : id; name : string; fields : List<EnumField> }

  module EnumCase =
    let fromCT (ec : EnumCase) : PT.CustomType.EnumCase =
      { id = ec.id; name = ec.name; fields = List.map EnumField.fromCT ec.fields }

    let toCT (ec : PT.CustomType.EnumCase) : EnumCase =
      { id = ec.id; name = ec.name; fields = List.map EnumField.toCT ec.fields }

  type T =
    | Record of firstField : RecordField * additionalFields : List<RecordField>
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>

  let fromCT (def : T) : PT.CustomType.T =
    match def with
    | Record (firstField, additionalFields) ->
      PT.CustomType.Record(
        RecordField.fromCT firstField,
        List.map RecordField.fromCT additionalFields
      )
    | Enum (firstCase, additionalCases) ->
      PT.CustomType.Enum(
        EnumCase.fromCT firstCase,
        List.map EnumCase.fromCT additionalCases
      )

  let toCT (def : PT.CustomType.T) : T =
    match def with
    | PT.CustomType.Record (firstField, additionalFields) ->
      Record(RecordField.toCT firstField, List.map RecordField.toCT additionalFields)
    | PT.CustomType.Enum (firstCase, additionalCases) ->
      Enum(EnumCase.toCT firstCase, List.map EnumCase.toCT additionalCases)


module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  module CronInterval =
    let fromCT (ci : CronInterval) : PT.Handler.CronInterval =
      match ci with
      | CronInterval.EveryDay -> PT.Handler.EveryDay
      | CronInterval.EveryWeek -> PT.Handler.EveryWeek
      | CronInterval.EveryFortnight -> PT.Handler.EveryFortnight
      | CronInterval.EveryHour -> PT.Handler.EveryHour
      | CronInterval.Every12Hours -> PT.Handler.Every12Hours
      | CronInterval.EveryMinute -> PT.Handler.EveryMinute

    let toCT (ci : PT.Handler.CronInterval) : CronInterval =
      match ci with
      | PT.Handler.EveryDay -> CronInterval.EveryDay
      | PT.Handler.EveryWeek -> CronInterval.EveryWeek
      | PT.Handler.EveryFortnight -> CronInterval.EveryFortnight
      | PT.Handler.EveryHour -> CronInterval.EveryHour
      | PT.Handler.Every12Hours -> CronInterval.Every12Hours
      | PT.Handler.EveryMinute -> CronInterval.EveryMinute

  type ids = { moduleID : id; nameID : id; modifierID : id }

  module ids =
    let fromCT (ids : ids) : PT.Handler.ids =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }

    let toCT (ids : PT.Handler.ids) : ids =
      { moduleID = ids.moduleID; nameID = ids.nameID; modifierID = ids.modifierID }


  type Spec =
    | HTTP of route : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids

  module Spec =
    let fromCT (spec : Spec) : PT.Handler.Spec =
      match spec with
      | Spec.HTTP (route, method, i) -> PT.Handler.HTTP(route, method, ids.fromCT i)
      | Spec.Worker (name, i) -> PT.Handler.Worker(name, ids.fromCT i)
      | Spec.Cron (name, interval, i) ->
        PT.Handler.Cron(name, Option.map CronInterval.fromCT interval, ids.fromCT i)
      | Spec.REPL (name, i) -> PT.Handler.REPL(name, ids.fromCT i)

    let toCT (spec : PT.Handler.Spec) : Spec =
      match spec with
      | PT.Handler.HTTP (route, method, i) -> Spec.HTTP(route, method, ids.toCT i)
      | PT.Handler.Worker (name, i) -> Spec.Worker(name, ids.toCT i)
      | PT.Handler.Cron (name, interval, i) ->
        Spec.Cron(name, Option.map CronInterval.toCT interval, ids.toCT i)
      | PT.Handler.REPL (name, i) -> Spec.REPL(name, ids.toCT i)

  type T = { tlid : tlid; ast : Expr; spec : Spec }

  let fromCT (h : T) : PT.Handler.T =
    { tlid = h.tlid; ast = Expr.fromCT h.ast; spec = Spec.fromCT h.spec }

  let toCT (h : PT.Handler.T) : T =
    { tlid = h.tlid; ast = Expr.toCT h.ast; spec = Spec.toCT h.spec }


module DB =
  type T = { tlid : tlid; name : string; version : int; typ : TypeReference }

  let fromCT (db : T) : PT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.fromCT db.typ }

  let toCT (db : PT.DB.T) : T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toCT db.typ }

module UserType =
  type Definition = CustomType.T
  type T = { tlid : tlid; name : FQTypeName.UserTypeName; definition : Definition }

  let fromCT (ut : T) : PT.UserType.T =
    { tlid = ut.tlid
      name = FQTypeName.UserTypeName.fromCT ut.name
      definition = CustomType.fromCT ut.definition }

  let toCT (ut : PT.UserType.T) : T =
    { tlid = ut.tlid
      name = FQTypeName.UserTypeName.toCT ut.name
      definition = CustomType.toCT ut.definition }


module UserFunction =
  type Parameter =
    { id : id
      name : string
      typ : TypeReference
      description : string }

  module Parameter =
    let fromCT (p : Parameter) : PT.UserFunction.Parameter =
      { id = p.id
        name = p.name
        typ = TypeReference.fromCT p.typ
        description = p.description }

    let toCT (p : PT.UserFunction.Parameter) : Parameter =
      { id = p.id
        name = p.name
        typ = TypeReference.toCT p.typ
        description = p.description }

  type T =
    { tlid : tlid
      name : FQFnName.UserFnName
      returnType : TypeReference
      typeParams : List<string>
      parameters : List<Parameter>
      description : string
      infix : bool
      body : Expr }

  let fromCT (uf : T) : PT.UserFunction.T =
    { tlid = uf.tlid
      name = FQFnName.UserFnName.fromCT uf.name
      typeParams = uf.typeParams
      parameters = List.map Parameter.fromCT uf.parameters
      returnType = TypeReference.fromCT uf.returnType
      description = uf.description
      infix = uf.infix
      body = Expr.fromCT uf.body }

  let toCT (uf : PT.UserFunction.T) : T =
    { tlid = uf.tlid
      name = FQFnName.UserFnName.toCT uf.name
      typeParams = uf.typeParams
      parameters = List.map Parameter.toCT uf.parameters
      returnType = TypeReference.toCT uf.returnType
      description = uf.description
      infix = uf.infix
      body = Expr.toCT uf.body }


type Toplevel =
  | TLHandler of Handler.T
  | TLDB of DB.T
  | TLFunction of UserFunction.T
  | TLType of UserType.T

module Toplevel =
  let fromCT (tl : Toplevel) : PT.Toplevel.T =
    match tl with
    | TLHandler handler -> PT.Toplevel.TLHandler(Handler.fromCT handler)
    | TLDB db -> PT.Toplevel.TLDB(DB.fromCT db)
    | TLFunction uf -> PT.Toplevel.TLFunction(UserFunction.fromCT uf)
    | TLType ut -> PT.Toplevel.TLType(UserType.fromCT ut)

  let toCT (tl : PT.Toplevel.T) : Toplevel =
    match tl with
    | PT.Toplevel.TLHandler handler -> TLHandler(Handler.toCT handler)
    | PT.Toplevel.TLDB db -> TLDB(DB.toCT db)
    | PT.Toplevel.TLFunction uf -> TLFunction(UserFunction.toCT uf)
    | PT.Toplevel.TLType ut -> TLType(UserType.toCT ut)


type Secret = { name : string; value : string; version : int }

module Secret =
  let fromCT (s : Secret) : PT.Secret.T =
    { name = s.name; value = s.value; version = s.version }

  let toCT (s : PT.Secret.T) : Secret =
    { name = s.name; value = s.value; version = s.version }


module Package =
  type Parameter = { name : string; typ : TypeReference; description : string }

  module Parameter =
    let fromCT (p : Parameter) : PT.Package.Parameter =
      { name = p.name
        typ = TypeReference.fromCT p.typ
        description = p.description }

    let toCT (p : PT.Package.Parameter) : Parameter =
      { name = p.name; typ = TypeReference.toCT p.typ; description = p.description }

  type Fn =
    { name : FQFnName.PackageFnName
      body : Expr
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      author : string
      deprecated : bool
      tlid : tlid }

  module Fn =
    let fromCT (fn : Fn) : PT.Package.Fn =
      { name = FQFnName.PackageFnName.fromCT fn.name
        body = Expr.fromCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.fromCT fn.parameters
        returnType = TypeReference.fromCT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }

    let toCT (fn : PT.Package.Fn) : Fn =
      { name = FQFnName.PackageFnName.toCT fn.name
        body = Expr.toCT fn.body
        typeParams = fn.typeParams
        parameters = List.map Parameter.toCT fn.parameters
        returnType = TypeReference.toCT fn.returnType
        description = fn.description
        author = fn.author
        deprecated = fn.deprecated
        tlid = fn.tlid }
