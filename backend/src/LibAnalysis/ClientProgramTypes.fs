/// Program Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// These should all directly match ProgramTypes in the client
/// See ProgramTypes.fs for documentation of these types
module LibAnalysis.ClientProgramTypes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes

module FQName =

  type CTBuiltIn<'name> = { modules : List<string>; name : 'name; version : int }
  type CTUserProgram<'name> = { modules : List<string>; name : 'name; version : int }
  type CTPackage<'name> =
    { owner : string; modules : NonEmptyList<string>; name : 'name; version : int }

  // Use the CT prefix to avoid name clashes with the final T enum
  module CTBuiltIn =
    let toCT
      (f : 'ptName -> 'ctName)
      (name : PT.FQName.BuiltIn<'ptName>)
      : CTBuiltIn<'ctName> =
      { modules = name.modules; name = f name.name; version = name.version }

    let fromCT
      (f : 'ctName -> 'ptName)
      (name : CTBuiltIn<'ctName>)
      : PT.FQName.BuiltIn<'ptName> =
      { modules = name.modules; name = f name.name; version = name.version }

  module CTUserProgram =
    let toCT
      (f : 'ptName -> 'ctName)
      (name : PT.FQName.UserProgram<'ptName>)
      : CTUserProgram<'ctName> =
      { modules = name.modules; name = f name.name; version = name.version }

    let fromCT
      (f : 'ctName -> 'ptName)
      (name : CTUserProgram<'ctName>)
      : PT.FQName.UserProgram<'ptName> =
      { modules = name.modules; name = f name.name; version = name.version }

  module CTPackage =
    let toCT
      (f : 'ptName -> 'ctName)
      (name : PT.FQName.Package<'ptName>)
      : CTPackage<'ctName> =
      { owner = name.owner
        modules = name.modules
        name = f name.name
        version = name.version }

    let fromCT
      (f : 'ctName -> 'ptName)
      (name : CTPackage<'ctName>)
      : PT.FQName.Package<'ptName> =
      { owner = name.owner
        modules = name.modules
        name = f name.name
        version = name.version }

  type T<'name> =
    | BuiltIn of CTBuiltIn<'name>
    | UserProgram of CTUserProgram<'name>
    | Package of CTPackage<'name>



  let toCT (f : 'ptName -> 'ctName) (name : PT.FQName.T<'ptName>) : T<'ctName> =
    match name with
    | PT.FQName.BuiltIn name -> BuiltIn(CTBuiltIn.toCT f name)
    | PT.FQName.UserProgram name -> UserProgram(CTUserProgram.toCT f name)
    | PT.FQName.Package name -> Package(CTPackage.toCT f name)

  let fromCT (f : 'ctName -> 'ptName) (name : T<'ctName>) : PT.FQName.T<'ptName> =
    match name with
    | BuiltIn name -> PT.FQName.BuiltIn(CTBuiltIn.fromCT f name)
    | UserProgram name -> PT.FQName.UserProgram(CTUserProgram.fromCT f name)
    | Package name -> PT.FQName.Package(CTPackage.fromCT f name)

module TypeName =
  type Name = TypeName of string
  type T = FQName.T<Name>
  type BuiltIn = FQName.CTBuiltIn<Name>
  type UserProgram = FQName.CTUserProgram<Name>
  type Package = FQName.CTPackage<Name>

  module CTBuiltIn =
    let toCT (name : PT.TypeName.BuiltIn) : BuiltIn =
      FQName.CTBuiltIn.toCT (fun (PT.TypeName.TypeName name) -> TypeName name) name

    let fromCT (name : BuiltIn) : PT.TypeName.BuiltIn =
      FQName.CTBuiltIn.fromCT (fun (TypeName name) -> PT.TypeName.TypeName name) name

  module CTUserProgram =
    let toCT (name : PT.TypeName.UserProgram) : UserProgram =
      FQName.CTUserProgram.toCT
        (fun (PT.TypeName.TypeName name) -> TypeName name)
        name

    let fromCT (name : UserProgram) : PT.TypeName.UserProgram =
      FQName.CTUserProgram.fromCT
        (fun (TypeName name) -> PT.TypeName.TypeName name)
        name

  module CTPackage =
    let toCT (name : PT.TypeName.Package) : Package =
      FQName.CTPackage.toCT (fun (PT.TypeName.TypeName name) -> TypeName name) name

    let fromCT (name : Package) : PT.TypeName.Package =
      FQName.CTPackage.fromCT (fun (TypeName name) -> PT.TypeName.TypeName name) name

  let toCT (name : PT.TypeName.T) : T =
    FQName.toCT (fun (PT.TypeName.TypeName name) -> TypeName name) name

  let fromCT (name : T) : PT.TypeName.T =
    FQName.fromCT (fun (TypeName name) -> PT.TypeName.TypeName name) name


module FnName =
  type Name = FnName of string
  type T = FQName.T<Name>
  type BuiltIn = FQName.CTBuiltIn<Name>
  type UserProgram = FQName.CTUserProgram<Name>
  type Package = FQName.CTPackage<Name>


  module CTBuiltIn =
    let toCT (name : PT.FnName.BuiltIn) : BuiltIn =
      FQName.CTBuiltIn.toCT (fun (PT.FnName.FnName name) -> FnName name) name

    let fromCT (name : BuiltIn) : PT.FnName.BuiltIn =
      FQName.CTBuiltIn.fromCT (fun (FnName name) -> PT.FnName.FnName name) name

  module CTUserProgram =
    let toCT (name : PT.FnName.UserProgram) : UserProgram =
      FQName.CTUserProgram.toCT (fun (PT.FnName.FnName name) -> FnName name) name

    let fromCT (name : UserProgram) : PT.FnName.UserProgram =
      FQName.CTUserProgram.fromCT (fun (FnName name) -> PT.FnName.FnName name) name

  module CTPackage =
    let toCT (name : PT.FnName.Package) : Package =
      FQName.CTPackage.toCT (fun (PT.FnName.FnName name) -> FnName name) name

    let fromCT (name : Package) : PT.FnName.Package =
      FQName.CTPackage.fromCT (fun (FnName name) -> PT.FnName.FnName name) name


  let toCT (name : PT.FnName.T) : T =
    FQName.toCT (fun (PT.FnName.FnName name) -> FnName name) name

  let fromCT (name : T) : PT.FnName.T =
    FQName.fromCT (fun (FnName name) -> PT.FnName.FnName name) name



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
  let rec fromCT (dtype : TypeReference) : PT.TypeReference =
    match dtype with
    | TInt -> PT.TInt
    | TFloat -> PT.TFloat
    | TBool -> PT.TBool
    | TUnit -> PT.TUnit
    | TString -> PT.TString
    | TList(t) -> PT.TList(fromCT t)
    | TTuple(first, second, theRest) ->
      PT.TTuple(fromCT first, fromCT second, List.map fromCT theRest)
    | TDict(t) -> PT.TDict(fromCT t)
    | TDB(t) -> PT.TDB(fromCT t)
    | TDateTime -> PT.TDateTime
    | TChar -> PT.TChar
    | TPassword -> PT.TPassword
    | TUuid -> PT.TUuid
    | TOption(t) -> PT.TOption(fromCT t)
    | TCustomType(t, typeArgs) ->
      PT.TCustomType(TypeName.fromCT t, List.map fromCT typeArgs)
    | TBytes -> PT.TBytes
    | TResult(ok, err) -> PT.TResult(fromCT ok, fromCT err)
    | TVariable(name) -> PT.TVariable(name)
    | TFn(args, body) -> PT.TFn(List.map fromCT args, fromCT body)

  let rec toCT (dtype : PT.TypeReference) : TypeReference =
    match dtype with
    | PT.TInt -> TInt
    | PT.TFloat -> TFloat
    | PT.TBool -> TBool
    | PT.TUnit -> TUnit
    | PT.TString -> TString
    | PT.TList(t) -> TList(toCT t)
    | PT.TTuple(first, second, theRest) ->
      TTuple(toCT first, toCT second, List.map toCT theRest)
    | PT.TDict(t) -> TDict(toCT t)
    | PT.TDB(t) -> TDB(toCT t)
    | PT.TDateTime -> TDateTime
    | PT.TChar -> TChar
    | PT.TPassword -> TPassword
    | PT.TUuid -> TUuid
    | PT.TOption(t) -> TOption(toCT t)
    | PT.TCustomType(t, typeArgs) ->
      TCustomType(TypeName.toCT t, List.map toCT typeArgs)
    | PT.TBytes -> TBytes
    | PT.TResult(ok, err) -> TResult(toCT ok, toCT err)
    | PT.TVariable(name) -> TVariable(name)
    | PT.TFn(args, body) -> TFn(List.map toCT args, toCT body)


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
    | LPVariable(id, str) -> PT.LPVariable(id, str)
    | LPTuple(id, first, second, theRest) ->
      PT.LPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)

  let rec toCT (p : PT.LetPattern) : LetPattern =
    match p with
    | PT.LPVariable(id, str) -> LPVariable(id, str)
    | PT.LPTuple(id, first, second, theRest) ->
      LPTuple(id, toCT first, toCT second, List.map toCT theRest)


type MatchPattern =
  | MPVariable of id * string
  | MPEnum of id * caseName : string * fieldPatterns : List<MatchPattern>
  | MPInt of id * int64
  | MPBool of id * bool
  | MPChar of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>
  | MPList of id * List<MatchPattern>
  | MPListCons of id * head : MatchPattern * tail : MatchPattern

module MatchPattern =
  let rec fromCT (pat : MatchPattern) : PT.MatchPattern =
    match pat with
    | MPVariable(id, str) -> PT.MPVariable(id, str)
    | MPEnum(id, caseName, fieldPats) ->
      PT.MPEnum(id, caseName, List.map fromCT fieldPats)
    | MPInt(id, i) -> PT.MPInt(id, i)
    | MPBool(id, b) -> PT.MPBool(id, b)
    | MPChar(id, str) -> PT.MPChar(id, str)
    | MPString(id, str) -> PT.MPString(id, str)
    | MPFloat(id, sign, whole, frac) -> PT.MPFloat(id, sign, whole, frac)
    | MPUnit(id) -> PT.MPUnit(id)
    | MPTuple(id, first, second, theRest) ->
      PT.MPTuple(id, fromCT first, fromCT second, List.map fromCT theRest)
    | MPList(id, pats) -> PT.MPList(id, List.map fromCT pats)
    | MPListCons(id, head, tail) -> PT.MPListCons(id, fromCT head, fromCT tail)

  let rec toCT (pat : PT.MatchPattern) : MatchPattern =
    match pat with
    | PT.MPVariable(id, str) -> MPVariable(id, str)
    | PT.MPEnum(id, caseName, fieldPats) ->
      MPEnum(id, caseName, List.map toCT fieldPats)
    | PT.MPInt(id, i) -> MPInt(id, i)
    | PT.MPBool(id, b) -> MPBool(id, b)
    | PT.MPChar(id, str) -> MPChar(id, str)
    | PT.MPString(id, str) -> MPString(id, str)
    | PT.MPFloat(id, sign, whole, frac) -> MPFloat(id, sign, whole, frac)
    | PT.MPUnit(id) -> MPUnit(id)
    | PT.MPTuple(id, first, second, theRest) ->
      MPTuple(id, toCT first, toCT second, List.map toCT theRest)
    | PT.MPList(id, pats) -> MPList(id, List.map toCT pats)
    | PT.MPListCons(id, head, tail) -> MPListCons(id, toCT head, toCT tail)

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
    | InfixFnCall(name) -> PT.InfixFnCall(InfixFnName.fromCT name)
    | BinOp(op) -> PT.BinOp(BinaryOperation.fromCT op)

  let toCT (infix : PT.Infix) : Infix =
    match infix with
    | PT.InfixFnCall(name) -> InfixFnCall(InfixFnName.toCT name)
    | PT.BinOp(op) -> BinOp(BinaryOperation.toCT op)


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
  | EFnCall of id * FnName.T * typeArgs : List<TypeReference> * args : List<Expr>
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | ERecord of id * TypeName.T * List<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : List<string * Expr>
  | EDict of id * List<string * Expr>
  | EPipe of id * Expr * PipeExpr * List<PipeExpr>
  | EEnum of id * typeName : TypeName.T * caseName : string * fields : List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and PipeExpr =
  | EPipeVariable of id * string
  | EPipeLambda of id * List<id * string> * Expr
  | EPipeInfix of id * Infix * Expr
  | EPipeFnCall of id * FnName.T * typeArgs : List<TypeReference> * args : List<Expr>
  | EPipeEnum of id * typeName : TypeName.T * caseName : string * fields : List<Expr>

module Expr =
  let rec fromCT (expr : Expr) : PT.Expr =
    match expr with
    | EInt(id, i) -> PT.EInt(id, i)
    | EBool(id, b) -> PT.EBool(id, b)
    | EString(id, segment) -> PT.EString(id, List.map stringSegmentFromCTPT segment)
    | EChar(id, c) -> PT.EChar(id, c)
    | EFloat(id, sign, whole, frac) -> PT.EFloat(id, sign, whole, frac)
    | EUnit(id) -> PT.EUnit(id)
    | ELet(id, pat, expr, body) ->
      PT.ELet(id, LetPattern.fromCT pat, fromCT expr, fromCT body)
    | EIf(id, cond, ifExpr, thenExpr) ->
      PT.EIf(id, fromCT cond, fromCT ifExpr, fromCT thenExpr)
    | EInfix(id, infix, first, second) ->
      PT.EInfix(id, Infix.fromCT (infix), fromCT first, fromCT second)
    | ELambda(id, args, body) -> PT.ELambda(id, args, fromCT body)
    | EFieldAccess(id, expr, fieldName) ->
      PT.EFieldAccess(id, fromCT expr, fieldName)
    | EVariable(id, name) -> PT.EVariable(id, name)
    | EFnCall(id, fnName, typeArgs, args) ->
      PT.EFnCall(
        id,
        FnName.fromCT fnName,
        List.map TypeReference.fromCT typeArgs,
        List.map fromCT args
      )
    | EList(id, exprs) -> PT.EList(id, List.map fromCT exprs)
    | ETuple(id, first, second, theRest) ->
      PT.ETuple(id, fromCT first, fromCT second, List.map fromCT theRest)
    | ERecord(id, typeName, fields) ->
      PT.ERecord(
        id,
        TypeName.fromCT typeName,
        fields |> List.map (fun (name, expr) -> (name, fromCT expr))
      )
    | ERecordUpdate(id, record, updates) ->
      PT.ERecordUpdate(
        id,
        fromCT record,
        updates |> List.map (fun (name, expr) -> (name, fromCT expr))
      )
    | EPipe(id, expr1, expr2, exprs) ->
      PT.EPipe(
        id,
        fromCT expr1,
        pipeExprFromCTPT expr2,
        List.map pipeExprFromCTPT exprs
      )
    | EMatch(id, matchExpr, cases) ->
      PT.EMatch(
        id,
        fromCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.fromCT pat, fromCT expr))
      )
    | EEnum(id, typeName, caseName, fields) ->
      PT.Expr.EEnum(id, TypeName.fromCT typeName, caseName, List.map fromCT fields)
    | EDict(id, fields) ->
      PT.Expr.EDict(id, fields |> List.map (fun (key, value) -> (key, fromCT value)))

  and stringSegmentFromCTPT (segment : StringSegment) : PT.StringSegment =
    match segment with
    | StringText text -> PT.StringText text
    | StringInterpolation expr -> PT.StringInterpolation(fromCT expr)

  and pipeExprFromCTPT (pipeExpr : PipeExpr) : PT.PipeExpr =
    match pipeExpr with
    | EPipeVariable(id, name) -> PT.EPipeVariable(id, name)
    | EPipeLambda(id, args, body) -> PT.EPipeLambda(id, args, fromCT body)
    | EPipeInfix(id, infix, first) ->
      PT.EPipeInfix(id, Infix.fromCT (infix), fromCT first)
    | EPipeFnCall(id, fnName, typeArgs, args) ->
      PT.EPipeFnCall(
        id,
        FnName.fromCT fnName,
        List.map TypeReference.fromCT typeArgs,
        List.map fromCT args
      )
    | EPipeEnum(id, typeName, caseName, fields) ->
      PT.EPipeEnum(id, TypeName.fromCT typeName, caseName, List.map fromCT fields)

  let rec toCT (expr : PT.Expr) : Expr =
    match expr with
    | PT.EInt(id, i) -> EInt(id, i)
    | PT.EBool(id, b) -> EBool(id, b)
    | PT.EString(id, s) -> EString(id, List.map stringSegmentToCT s)
    | PT.EChar(id, c) -> EChar(id, c)
    | PT.EFloat(id, sign, whole, frac) -> EFloat(id, sign, whole, frac)
    | PT.EUnit(id) -> EUnit(id)
    | PT.ELet(id, pat, expr, body) ->
      ELet(id, LetPattern.toCT pat, toCT expr, toCT body)
    | PT.EIf(id, cond, ifExpr, thenExpr) ->
      EIf(id, toCT cond, toCT ifExpr, toCT thenExpr)
    | PT.EInfix(id, PT.InfixFnCall(name), first, second) ->
      EInfix(id, InfixFnCall(InfixFnName.toCT name), toCT first, toCT second)
    | PT.EInfix(id, PT.BinOp op, first, second) ->
      EInfix(id, BinOp(BinaryOperation.toCT op), toCT first, toCT second)
    | PT.ELambda(id, args, body) -> ELambda(id, args, toCT body)
    | PT.EFieldAccess(id, expr, fieldName) -> EFieldAccess(id, toCT expr, fieldName)
    | PT.EVariable(id, name) -> EVariable(id, name)
    | PT.EFnCall(id, fnName, typeArgs, args) ->
      EFnCall(
        id,
        FnName.toCT fnName,
        List.map TypeReference.toCT typeArgs,
        List.map toCT args
      )
    | PT.EList(id, exprs) -> EList(id, List.map toCT exprs)
    | PT.ETuple(id, first, second, theRest) ->
      ETuple(id, toCT first, toCT second, List.map toCT theRest)
    | PT.ERecord(id, typeName, fields) ->
      ERecord(
        id,
        TypeName.toCT typeName,
        fields |> List.map (fun (name, expr) -> (name, toCT expr))
      )
    | PT.ERecordUpdate(id, record, updates) ->
      ERecordUpdate(
        id,
        toCT record,
        updates |> List.map (fun (name, expr) -> (name, toCT expr))
      )
    | PT.EPipe(id, expr1, expr2, exprs) ->
      EPipe(id, toCT expr1, pipeExprToCT expr2, List.map pipeExprToCT exprs)
    | PT.EEnum(id, typeName, caseName, fields) ->
      EEnum(id, TypeName.toCT typeName, caseName, List.map toCT fields)
    | PT.EMatch(id, matchExpr, cases) ->
      EMatch(
        id,
        toCT matchExpr,
        cases |> List.map (fun (pat, expr) -> (MatchPattern.toCT pat, toCT expr))
      )
    | PT.EDict(id, fields) ->
      EDict(id, fields |> List.map (fun (key, value) -> (key, toCT value)))


  and stringSegmentToCT (segment : PT.StringSegment) : StringSegment =
    match segment with
    | PT.StringText text -> StringText text
    | PT.StringInterpolation expr -> StringInterpolation(toCT expr)

  and pipeExprToCT (pipeExpr : PT.PipeExpr) : PipeExpr =
    match pipeExpr with
    | PT.EPipeVariable(id, name) -> EPipeVariable(id, name)
    | PT.EPipeLambda(id, args, body) -> EPipeLambda(id, args, toCT body)
    | PT.EPipeInfix(id, PT.InfixFnCall(name), first) ->
      EPipeInfix(id, InfixFnCall(InfixFnName.toCT name), toCT first)
    | PT.EPipeInfix(id, PT.BinOp op, first) ->
      EPipeInfix(id, BinOp(BinaryOperation.toCT op), toCT first)
    | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
      EPipeFnCall(
        id,
        FnName.toCT fnName,
        List.map TypeReference.toCT typeArgs,
        List.map toCT args
      )
    | PT.EPipeEnum(id, nameOpt, caseName, fields) ->
      EPipeEnum(id, TypeName.toCT nameOpt, caseName, List.map toCT fields)

module CustomType =
  type RecordField = { name : string; typ : TypeReference; description : string }

  module RecordField =
    let fromCT (rf : RecordField) : PT.CustomType.RecordField =
      { name = rf.name
        typ = TypeReference.fromCT rf.typ
        description = rf.description }

    let toCT (rf : PT.CustomType.RecordField) : RecordField =
      { name = rf.name
        typ = TypeReference.toCT rf.typ
        description = rf.description }

  type EnumField =
    { typ : TypeReference; label : Option<string>; description : string }

  module EnumField =
    let fromCT (ef : EnumField) : PT.CustomType.EnumField =
      { typ = TypeReference.fromCT ef.typ
        label = ef.label
        description = ef.description }

    let toCT (ef : PT.CustomType.EnumField) : EnumField =
      { typ = TypeReference.toCT ef.typ
        label = ef.label
        description = ef.description }

  type EnumCase = { name : string; fields : List<EnumField>; description : string }

  module EnumCase =
    let fromCT (ec : EnumCase) : PT.CustomType.EnumCase =
      { name = ec.name
        fields = List.map EnumField.fromCT ec.fields
        description = ec.description }

    let toCT (ec : PT.CustomType.EnumCase) : EnumCase =
      { name = ec.name
        fields = List.map EnumField.toCT ec.fields
        description = ec.description }

  type T =
    | Alias of TypeReference
    | Record of firstField : RecordField * additionalFields : List<RecordField>
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>

  let fromCT (def : T) : PT.CustomType.T =
    match def with
    | Alias typ -> PT.CustomType.Alias(TypeReference.fromCT typ)
    | Record(firstField, additionalFields) ->
      PT.CustomType.Record(
        RecordField.fromCT firstField,
        List.map RecordField.fromCT additionalFields
      )
    | Enum(firstCase, additionalCases) ->
      PT.CustomType.Enum(
        EnumCase.fromCT firstCase,
        List.map EnumCase.fromCT additionalCases
      )

  let toCT (def : PT.CustomType.T) : T =
    match def with
    | PT.CustomType.Alias typ -> Alias(TypeReference.toCT typ)
    | PT.CustomType.Record(firstField, additionalFields) ->
      Record(RecordField.toCT firstField, List.map RecordField.toCT additionalFields)
    | PT.CustomType.Enum(firstCase, additionalCases) ->
      Enum(EnumCase.toCT firstCase, List.map EnumCase.toCT additionalCases)


type Deprecation<'name> =
  | NotDeprecated
  | RenamedTo of 'name
  | ReplacedBy of 'name
  | DeprecatedBecause of string

module Deprecation =
  let fromCT
    (f : 'name1 -> 'name2)
    (d : Deprecation<'name1>)
    : PT.Deprecation<'name2> =
    match d with
    | NotDeprecated -> PT.NotDeprecated
    | RenamedTo name -> PT.RenamedTo(f name)
    | ReplacedBy name -> PT.ReplacedBy(f name)
    | DeprecatedBecause reason -> PT.DeprecatedBecause reason

  let toCT
    (f : 'name1 -> 'name2)
    (d : PT.Deprecation<'name1>)
    : Deprecation<'name2> =
    match d with
    | PT.NotDeprecated -> NotDeprecated
    | PT.RenamedTo name -> RenamedTo(f name)
    | PT.ReplacedBy name -> ReplacedBy(f name)
    | PT.DeprecatedBecause reason -> DeprecatedBecause reason



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

  type Spec =
    | HTTP of route : string * method : string
    | Worker of name : string
    | Cron of name : string * interval : CronInterval
    | REPL of name : string

  module Spec =
    let fromCT (spec : Spec) : PT.Handler.Spec =
      match spec with
      | Spec.HTTP(route, method) -> PT.Handler.HTTP(route, method)
      | Spec.Worker name -> PT.Handler.Worker name
      | Spec.Cron(name, interval) ->
        PT.Handler.Cron(name, CronInterval.fromCT interval)
      | Spec.REPL name -> PT.Handler.REPL name

    let toCT (spec : PT.Handler.Spec) : Spec =
      match spec with
      | PT.Handler.HTTP(route, method) -> Spec.HTTP(route, method)
      | PT.Handler.Worker name -> Spec.Worker name
      | PT.Handler.Cron(name, interval) ->
        Spec.Cron(name, CronInterval.toCT interval)
      | PT.Handler.REPL name -> Spec.REPL name

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
  type T =
    { tlid : tlid
      name : TypeName.UserProgram
      typeParams : List<string>
      definition : CustomType.T }

  let fromCT (ut : T) : PT.UserType.T =
    { tlid = ut.tlid
      name = TypeName.CTUserProgram.fromCT ut.name
      typeParams = ut.typeParams
      definition = CustomType.fromCT ut.definition }

  let toCT (ut : PT.UserType.T) : T =
    { tlid = ut.tlid
      name = TypeName.CTUserProgram.toCT ut.name
      typeParams = ut.typeParams
      definition = CustomType.toCT ut.definition }


module UserFunction =
  type Parameter = { name : string; typ : TypeReference; description : string }

  module Parameter =
    let fromCT (p : Parameter) : PT.UserFunction.Parameter =
      { name = p.name
        typ = TypeReference.fromCT p.typ
        description = p.description }

    let toCT (p : PT.UserFunction.Parameter) : Parameter =
      { name = p.name; typ = TypeReference.toCT p.typ; description = p.description }

  type T =
    { tlid : tlid
      name : FnName.UserProgram
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      deprecated : Deprecation<FnName.T>
      body : Expr }

  let fromCT (uf : T) : PT.UserFunction.T =
    { tlid = uf.tlid
      name = FnName.CTUserProgram.fromCT uf.name
      typeParams = uf.typeParams
      parameters = List.map Parameter.fromCT uf.parameters
      returnType = TypeReference.fromCT uf.returnType
      description = uf.description
      deprecated = Deprecation.fromCT FnName.fromCT uf.deprecated
      body = Expr.fromCT uf.body }

  let toCT (uf : PT.UserFunction.T) : T =
    { tlid = uf.tlid
      name = FnName.CTUserProgram.toCT uf.name
      typeParams = uf.typeParams
      parameters = List.map Parameter.toCT uf.parameters
      returnType = TypeReference.toCT uf.returnType
      description = uf.description
      deprecated = Deprecation.toCT FnName.toCT uf.deprecated
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


module PackageFn =
  type Parameter = { name : string; typ : TypeReference; description : string }

  module Parameter =
    let fromCT (p : Parameter) : PT.PackageFn.Parameter =
      { name = p.name
        typ = TypeReference.fromCT p.typ
        description = p.description }

    let toCT (p : PT.PackageFn.Parameter) : Parameter =
      { name = p.name; typ = TypeReference.toCT p.typ; description = p.description }

  type T =
    { name : FnName.Package
      id : System.Guid
      body : Expr
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      deprecated : Deprecation<FnName.T>
      tlid : tlid }

  let fromCT (fn : T) : PT.PackageFn.T =
    { name = FnName.CTPackage.fromCT fn.name
      id = fn.id
      body = Expr.fromCT fn.body
      typeParams = fn.typeParams
      parameters = List.map Parameter.fromCT fn.parameters
      returnType = TypeReference.fromCT fn.returnType
      description = fn.description
      deprecated = Deprecation.fromCT FnName.fromCT fn.deprecated
      tlid = fn.tlid }

  let toCT (fn : PT.PackageFn.T) : T =
    { name = FnName.CTPackage.toCT fn.name
      id = fn.id
      body = Expr.toCT fn.body
      typeParams = fn.typeParams
      parameters = List.map Parameter.toCT fn.parameters
      returnType = TypeReference.toCT fn.returnType
      description = fn.description
      deprecated = Deprecation.toCT FnName.toCT fn.deprecated
      tlid = fn.tlid }

module PackageType =
  type T =
    { tlid : tlid
      id : System.Guid
      name : TypeName.Package
      typeParams : List<string>
      definition : CustomType.T
      description : string
      deprecated : Deprecation<TypeName.T> }

  let fromCT (pt : T) : PT.PackageType.T =
    { tlid = pt.tlid
      id = pt.id
      name = TypeName.CTPackage.fromCT pt.name
      typeParams = pt.typeParams
      definition = CustomType.fromCT pt.definition
      description = pt.description
      deprecated = Deprecation.fromCT TypeName.fromCT pt.deprecated }

  let toCT (pt : PT.PackageType.T) : T =
    { tlid = pt.tlid
      id = pt.id
      name = TypeName.CTPackage.toCT pt.name
      typeParams = pt.typeParams
      definition = CustomType.toCT pt.definition
      description = pt.description
      deprecated = Deprecation.toCT TypeName.toCT pt.deprecated }
