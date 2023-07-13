/// Conversion functions from ProgramTypes to SerializedTypes and back
module LibBinarySerialization.ProgramTypesToSerializedTypes

open Prelude
open Tablecloth

// Used for conversion functions
module ST = SerializedTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

module TypeName =

  module BuiltIn =
    let toST (b : PT.TypeName.BuiltIn) : ST.TypeName.BuiltIn =
      let (PT.TypeName.TypeName name) = b.name
      { modules = b.modules; name = name; version = b.version }

    let toPT (b : ST.TypeName.BuiltIn) : PT.TypeName.BuiltIn =
      { modules = b.modules
        name = PT.TypeName.TypeName b.name
        version = b.version }

  module UserProgram =
    let toST (u : PT.TypeName.UserProgram) : ST.TypeName.UserProgram =
      let (PT.TypeName.TypeName name) = u.name
      { modules = u.modules; name = name; version = u.version }

    let toPT (u : ST.TypeName.UserProgram) : PT.TypeName.UserProgram =
      { modules = u.modules
        name = PT.TypeName.TypeName u.name
        version = u.version }

  module Package =
    let toST (p : PT.TypeName.Package) : ST.TypeName.Package =
      let (PT.TypeName.TypeName name) = p.name
      { owner = p.owner
        modules = { head = p.modules.Head; tail = p.modules.Tail }
        name = name
        version = p.version }

    let toPT (p : ST.TypeName.Package) : PT.TypeName.Package =
      { owner = p.owner
        modules = { Head = p.modules.head; Tail = p.modules.tail }
        name = PT.TypeName.TypeName p.name
        version = p.version }

  let toST (fqtn : PT.TypeName.T) : ST.TypeName.T =
    match fqtn with
    | PT.FQName.BuiltIn s -> ST.TypeName.BuiltIn(BuiltIn.toST s)
    | PT.FQName.UserProgram u -> ST.TypeName.UserProgram(UserProgram.toST u)
    | PT.FQName.Package p -> ST.TypeName.Package(Package.toST p)

  let toPT (fqfn : ST.TypeName.T) : PT.TypeName.T =
    match fqfn with
    | ST.TypeName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.toPT s)
    | ST.TypeName.UserProgram u -> PT.FQName.UserProgram(UserProgram.toPT u)
    | ST.TypeName.Package p -> PT.FQName.Package(Package.toPT p)



module FnName =

  module BuiltIn =
    let toST (b : PT.FnName.BuiltIn) : ST.FnName.BuiltIn =
      let (PT.FnName.FnName name) = b.name
      { modules = b.modules; name = name; version = b.version }

    let toPT (b : ST.FnName.BuiltIn) : PT.FnName.BuiltIn =
      { modules = b.modules; name = PT.FnName.FnName b.name; version = b.version }

  module UserProgram =
    let toST (u : PT.FnName.UserProgram) : ST.FnName.UserProgram =
      let (PT.FnName.FnName name) = u.name
      { modules = u.modules; name = name; version = u.version }

    let toPT (u : ST.FnName.UserProgram) : PT.FnName.UserProgram =
      { modules = u.modules; name = PT.FnName.FnName u.name; version = u.version }

  module Package =
    let toST (p : PT.FnName.Package) : ST.FnName.Package =
      let (PT.FnName.FnName name) = p.name
      { owner = p.owner
        modules = { head = p.modules.Head; tail = p.modules.Tail }
        name = name
        version = p.version }

    let toPT (p : ST.FnName.Package) : PT.FnName.Package =
      { owner = p.owner
        modules = { Head = p.modules.head; Tail = p.modules.tail }
        name = PT.FnName.FnName p.name
        version = p.version }

  let toST (fqfn : PT.FnName.T) : ST.FnName.T =
    match fqfn with
    | PT.FQName.BuiltIn s -> ST.FnName.BuiltIn(BuiltIn.toST s)
    | PT.FQName.UserProgram u -> ST.FnName.UserProgram(UserProgram.toST u)
    | PT.FQName.Package p -> ST.FnName.Package(Package.toST p)

  let toPT (fqfn : ST.FnName.T) : PT.FnName.T =
    match fqfn with
    | ST.FnName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.toPT s)
    | ST.FnName.UserProgram u -> PT.FQName.UserProgram(UserProgram.toPT u)
    | ST.FnName.Package p -> PT.FQName.Package(Package.toPT p)

// type NameResolutionError = NotFound of List<string>
module NameResolution =
  module NameResultionError =
    let toST
      (err : LibExecution.Errors.NameResolutionError)
      : ST.NameResolutionError =
      match err with
      | LibExecution.Errors.NotFound modules ->
        ST.NameResolutionError.NotFound modules
      | LibExecution.Errors.MissingModuleName moduleName ->
        ST.NameResolutionError.MissingModuleName moduleName
      | LibExecution.Errors.InvalidPackageName packageName ->
        ST.NameResolutionError.InvalidPackageName packageName

    let toPT
      (err : ST.NameResolutionError)
      : LibExecution.Errors.NameResolutionError =
      match err with
      | ST.NameResolutionError.NotFound modules ->
        LibExecution.Errors.NotFound modules
      | ST.NameResolutionError.MissingModuleName moduleName ->
        LibExecution.Errors.MissingModuleName moduleName
      | ST.NameResolutionError.InvalidPackageName packageName ->
        LibExecution.Errors.InvalidPackageName packageName

  // type NameResolution = Result<x, NameResolutionError>
  let toST (f : 'p -> 's) (result : PT.NameResolution<'p>) : ST.NameResolution<'s> =
    match result with
    | Ok name -> Ok(f name)
    | Error err -> Error(NameResultionError.toST err)

  let toPT (f : 's -> 'p) (result : ST.NameResolution<'s>) : PT.NameResolution<'p> =
    match result with
    | Ok name -> Ok(f name)
    | Error err -> Error(NameResultionError.toPT err)


module InfixFnName =
  let toST (name : PT.InfixFnName) : ST.InfixFnName =
    match name with
    | PT.ArithmeticPlus -> ST.ArithmeticPlus
    | PT.ArithmeticMinus -> ST.ArithmeticMinus
    | PT.ArithmeticMultiply -> ST.ArithmeticMultiply
    | PT.ArithmeticDivide -> ST.ArithmeticDivide
    | PT.ArithmeticModulo -> ST.ArithmeticModulo
    | PT.ArithmeticPower -> ST.ArithmeticPower
    | PT.ComparisonGreaterThan -> ST.ComparisonGreaterThan
    | PT.ComparisonGreaterThanOrEqual -> ST.ComparisonGreaterThanOrEqual
    | PT.ComparisonLessThan -> ST.ComparisonLessThan
    | PT.ComparisonLessThanOrEqual -> ST.ComparisonLessThanOrEqual
    | PT.ComparisonEquals -> ST.ComparisonEquals
    | PT.ComparisonNotEquals -> ST.ComparisonNotEquals
    | PT.StringConcat -> ST.StringConcat

  let toPT (name : ST.InfixFnName) : PT.InfixFnName =
    match name with
    | ST.ArithmeticPlus -> PT.ArithmeticPlus
    | ST.ArithmeticMinus -> PT.ArithmeticMinus
    | ST.ArithmeticMultiply -> PT.ArithmeticMultiply
    | ST.ArithmeticDivide -> PT.ArithmeticDivide
    | ST.ArithmeticModulo -> PT.ArithmeticModulo
    | ST.ArithmeticPower -> PT.ArithmeticPower
    | ST.ComparisonGreaterThan -> PT.ComparisonGreaterThan
    | ST.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
    | ST.ComparisonLessThan -> PT.ComparisonLessThan
    | ST.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
    | ST.ComparisonEquals -> PT.ComparisonEquals
    | ST.ComparisonNotEquals -> PT.ComparisonNotEquals
    | ST.StringConcat -> PT.StringConcat

module TypeReference =
  let rec toST (t : PT.TypeReference) : ST.TypeReference =
    match t with
    | PT.TInt -> ST.TInt
    | PT.TFloat -> ST.TFloat
    | PT.TBool -> ST.TBool
    | PT.TUnit -> ST.TUnit
    | PT.TString -> ST.TString
    | PT.TList typ -> ST.TList(toST typ)
    | PT.TTuple(first, second, theRest) ->
      ST.TTuple(toST first, toST second, List.map toST theRest)
    | PT.TDict typ -> ST.TDict(toST typ)
    | PT.TDB typ -> ST.TDB(toST typ)
    | PT.TDateTime -> ST.TDateTime
    | PT.TChar -> ST.TChar
    | PT.TPassword -> ST.TPassword
    | PT.TUuid -> ST.TUuid
    | PT.TCustomType(t, typeArgs) ->
      ST.TCustomType(TypeName.toST t, List.map toST typeArgs)
    | PT.TBytes -> ST.TBytes
    | PT.TVariable(name) -> ST.TVariable(name)
    | PT.TFn(paramTypes, returnType) ->
      ST.TFn(List.map toST paramTypes, toST returnType)

  let rec toPT (t : ST.TypeReference) : PT.TypeReference =
    match t with
    | ST.TInt -> PT.TInt
    | ST.TFloat -> PT.TFloat
    | ST.TBool -> PT.TBool
    | ST.TUnit -> PT.TUnit
    | ST.TString -> PT.TString
    | ST.TList typ -> PT.TList(toPT typ)
    | ST.TTuple(firstType, secondType, otherTypes) ->
      PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
    | ST.TDict typ -> PT.TDict(toPT typ)
    | ST.TDB typ -> PT.TDB(toPT typ)
    | ST.TDateTime -> PT.TDateTime
    | ST.TChar -> PT.TChar
    | ST.TPassword -> PT.TPassword
    | ST.TUuid -> PT.TUuid
    | ST.TCustomType(t, typeArgs) ->
      PT.TCustomType(TypeName.toPT t, List.map toPT typeArgs)
    | ST.TBytes -> PT.TBytes
    | ST.TVariable(name) -> PT.TVariable(name)
    | ST.TFn(paramTypes, returnType) ->
      PT.TFn(List.map toPT paramTypes, toPT returnType)

module BinaryOperation =
  let toST (op : PT.BinaryOperation) : ST.BinaryOperation =
    match op with
    | PT.BinOpAnd -> ST.BinOpAnd
    | PT.BinOpOr -> ST.BinOpOr

  let toPT (binop : ST.BinaryOperation) : PT.BinaryOperation =
    match binop with
    | ST.BinOpAnd -> PT.BinOpAnd
    | ST.BinOpOr -> PT.BinOpOr

module Infix =
  let toPT (infix : ST.Infix) : PT.Infix =
    match infix with
    | ST.InfixFnCall(fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
    | ST.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

module LetPattern =
  let rec toST (p : PT.LetPattern) : ST.LetPattern =
    match p with
    | PT.LPVariable(id, str) -> ST.LPVariable(id, str)
    | PT.LPTuple(id, first, second, theRest) ->
      ST.LPTuple(id, toST first, toST second, List.map toST theRest)

  let rec toPT (p : ST.LetPattern) : PT.LetPattern =
    match p with
    | ST.LPVariable(id, str) -> PT.LPVariable(id, str)
    | ST.LPTuple(id, first, second, theRest) ->
      PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)

module MatchPattern =
  let rec toST (p : PT.MatchPattern) : ST.MatchPattern =
    match p with
    | PT.MPVariable(id, str) -> ST.MPVariable(id, str)
    | PT.MPEnum(id, caseName, fieldPats) ->
      ST.MPEnum(id, caseName, List.map toST fieldPats)
    | PT.MPInt(id, i) -> ST.MPInt(id, i)
    | PT.MPBool(id, b) -> ST.MPBool(id, b)
    | PT.MPChar(id, c) -> ST.MPChar(id, c)
    | PT.MPString(id, s) -> ST.MPString(id, s)
    | PT.MPFloat(id, s, w, f) -> ST.MPFloat(id, s, w, f)
    | PT.MPUnit id -> ST.MPUnit id
    | PT.MPTuple(id, first, second, theRest) ->
      ST.MPTuple(id, toST first, toST second, List.map toST theRest)
    | PT.MPList(id, pats) -> ST.MPList(id, List.map toST pats)
    | PT.MPListCons(id, head, tail) -> ST.MPListCons(id, toST head, toST tail)


  let rec toPT (p : ST.MatchPattern) : PT.MatchPattern =
    match p with
    | ST.MPVariable(id, str) -> PT.MPVariable(id, str)
    | ST.MPEnum(id, caseName, fieldPats) ->
      PT.MPEnum(id, caseName, List.map toPT fieldPats)
    | ST.MPInt(id, i) -> PT.MPInt(id, i)
    | ST.MPBool(id, b) -> PT.MPBool(id, b)
    | ST.MPChar(id, c) -> PT.MPChar(id, c)
    | ST.MPString(id, s) -> PT.MPString(id, s)
    | ST.MPFloat(id, s, w, f) -> PT.MPFloat(id, s, w, f)
    | ST.MPUnit id -> PT.MPUnit id
    | ST.MPTuple(id, first, second, theRest) ->
      PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)
    | ST.MPList(id, pats) -> PT.MPList(id, List.map toPT pats)
    | ST.MPListCons(id, head, tail) -> PT.MPListCons(id, toPT head, toPT tail)


module Expr =
  let rec toST (e : PT.Expr) : ST.Expr =
    match e with
    | PT.EChar(id, char) -> ST.EChar(id, char)
    | PT.EInt(id, num) -> ST.EInt(id, num)
    | PT.EString(id, segments) -> ST.EString(id, List.map stringSegmentToST segments)
    | PT.EFloat(id, sign, whole, fraction) -> ST.EFloat(id, sign, whole, fraction)
    | PT.EBool(id, b) -> ST.EBool(id, b)
    | PT.EUnit id -> ST.EUnit id
    | PT.EVariable(id, var) -> ST.EVariable(id, var)
    | PT.EFieldAccess(id, obj, fieldname) -> ST.EFieldAccess(id, toST obj, fieldname)
    | PT.EApply(id, PT.FnTargetName name, typeArgs, args) ->
      let name = NameResolution.toST FnName.toST name
      ST.EApply(
        id,
        ST.FnTargetName name,
        List.map TypeReference.toST typeArgs,
        List.map toST args
      )
    | PT.EApply(id, PT.FnTargetExpr name, typeArgs, args) ->
      ST.EApply(
        id,
        ST.FnTargetExpr(toST name),
        List.map TypeReference.toST typeArgs,
        List.map toST args
      )
    | PT.EInfix(id, PT.InfixFnCall name, arg1, arg2) ->
      ST.EInfix(id, ST.InfixFnCall(InfixFnName.toST name), toST arg1, toST arg2)
    | PT.EInfix(id, PT.BinOp(op), arg1, arg2) ->
      ST.EInfix(id, ST.BinOp(BinaryOperation.toST (op)), toST arg1, toST arg2)
    | PT.ELambda(id, vars, body) -> ST.ELambda(id, vars, toST body)
    | PT.ELet(id, pat, rhs, body) ->
      ST.ELet(id, LetPattern.toST pat, toST rhs, toST body)
    | PT.EIf(id, cond, thenExpr, elseExpr) ->
      ST.EIf(id, toST cond, toST thenExpr, toST elseExpr)
    | PT.EList(id, exprs) -> ST.EList(id, List.map toST exprs)
    | PT.ETuple(id, first, second, theRest) ->
      ST.ETuple(id, toST first, toST second, List.map toST theRest)
    | PT.ERecord(id, typeName, fields) ->
      ST.ERecord(
        id,
        NameResolution.toST TypeName.toST typeName,
        List.map (Tuple2.mapSecond toST) fields
      )
    | PT.ERecordUpdate(id, record, updates) ->
      ST.ERecordUpdate(
        id,
        toST record,
        updates |> List.map (fun (name, expr) -> (name, toST expr))
      )
    | PT.EPipe(pipeID, expr1, expr2, rest) ->
      ST.EPipe(pipeID, toST expr1, pipeExprToST expr2, List.map pipeExprToST rest)
    | PT.EEnum(id, typeName, caseName, fields) ->
      ST.EEnum(
        id,
        NameResolution.toST TypeName.toST typeName,
        caseName,
        List.map toST fields
      )
    | PT.EMatch(id, mexpr, cases) ->
      ST.EMatch(
        id,
        toST mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toST << Tuple2.mapSecond toST) cases
      )
    | PT.EDict(id, fields) -> ST.EDict(id, List.map (Tuple2.mapSecond toST) fields)

  and stringSegmentToST (segment : PT.StringSegment) : ST.StringSegment =
    match segment with
    | PT.StringText text -> ST.StringText text
    | PT.StringInterpolation expr -> ST.StringInterpolation(toST expr)

  and pipeExprToST (pipeExpr : PT.PipeExpr) : ST.PipeExpr =
    match pipeExpr with
    | PT.EPipeVariable(id, name) -> ST.EPipeVariable(id, name)
    | PT.EPipeLambda(id, args, body) -> ST.EPipeLambda(id, args, toST body)
    | PT.EPipeInfix(id, PT.InfixFnCall name, first) ->
      ST.EPipeInfix(id, ST.InfixFnCall(InfixFnName.toST name), toST first)
    | PT.EPipeInfix(id, PT.BinOp(op), first) ->
      ST.EPipeInfix(id, ST.BinOp(BinaryOperation.toST (op)), toST first)
    | PT.EPipeFnCall(id, fnName, typeArgs, args) ->
      ST.EPipeFnCall(
        id,
        NameResolution.toST FnName.toST fnName,
        List.map TypeReference.toST typeArgs,
        List.map toST args
      )
    | PT.EPipeEnum(id, typeName, caseName, fields) ->
      ST.EPipeEnum(
        id,
        NameResolution.toST TypeName.toST typeName,
        caseName,
        List.map toST fields
      )

  let rec toPT (e : ST.Expr) : PT.Expr =
    match e with
    | ST.EChar(id, char) -> PT.EChar(id, char)
    | ST.EInt(id, num) -> PT.EInt(id, num)
    | ST.EString(id, segment) -> PT.EString(id, List.map stringSegmentToPT segment)
    | ST.EFloat(id, sign, whole, fraction) -> PT.EFloat(id, sign, whole, fraction)
    | ST.EBool(id, b) -> PT.EBool(id, b)
    | ST.EUnit id -> PT.EUnit id
    | ST.EVariable(id, var) -> PT.EVariable(id, var)
    | ST.EFieldAccess(id, obj, fieldname) -> PT.EFieldAccess(id, toPT obj, fieldname)
    | ST.EApply(id, ST.FnTargetName name, typeArgs, args) ->
      PT.EApply(
        id,
        PT.FnTargetName(NameResolution.toPT FnName.toPT name),
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | ST.EApply(id, ST.FnTargetExpr name, typeArgs, args) ->
      PT.EApply(
        id,
        PT.FnTargetExpr(toPT name),
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | ST.ELambda(id, vars, body) -> PT.ELambda(id, vars, toPT body)
    | ST.ELet(id, pat, rhs, body) ->
      PT.ELet(id, LetPattern.toPT pat, toPT rhs, toPT body)
    | ST.EIf(id, cond, thenExpr, elseExpr) ->
      PT.EIf(id, toPT cond, toPT thenExpr, toPT elseExpr)
    | ST.EList(id, exprs) -> PT.EList(id, List.map toPT exprs)
    | ST.ETuple(id, first, second, theRest) ->
      PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
    | ST.ERecord(id, typeName, fields) ->
      PT.ERecord(
        id,
        NameResolution.toPT TypeName.toPT typeName,
        List.map (Tuple2.mapSecond toPT) fields
      )
    | ST.ERecordUpdate(id, record, updates) ->
      PT.ERecordUpdate(
        id,
        toPT record,
        updates |> List.map (fun (name, expr) -> (name, toPT expr))
      )
    | ST.EPipe(pipeID, expr1, expr2, rest) ->
      PT.EPipe(pipeID, toPT expr1, pipeExprToPT expr2, List.map pipeExprToPT rest)
    | ST.EEnum(id, typeName, caseName, exprs) ->
      PT.EEnum(
        id,
        NameResolution.toPT TypeName.toPT typeName,
        caseName,
        List.map toPT exprs
      )
    | ST.EMatch(id, mexpr, pairs) ->
      PT.EMatch(
        id,
        toPT mexpr,
        List.map (Tuple2.mapFirst MatchPattern.toPT << Tuple2.mapSecond toPT) pairs
      )
    | ST.EInfix(id, infix, arg1, arg2) ->
      PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)
    | ST.EDict(id, pairs) -> PT.EDict(id, List.map (Tuple2.mapSecond toPT) pairs)

  and stringSegmentToPT (segment : ST.StringSegment) : PT.StringSegment =
    match segment with
    | ST.StringText text -> PT.StringText text
    | ST.StringInterpolation expr -> PT.StringInterpolation(toPT expr)

  and pipeExprToPT (pipeExpr : ST.PipeExpr) : PT.PipeExpr =
    match pipeExpr with
    | ST.EPipeVariable(id, name) -> PT.EPipeVariable(id, name)
    | ST.EPipeLambda(id, args, body) -> PT.EPipeLambda(id, args, toPT body)
    | ST.EPipeInfix(id, infix, first) ->
      PT.EPipeInfix(id, Infix.toPT infix, toPT first)
    | ST.EPipeFnCall(id, fnName, typeArgs, args) ->
      PT.EPipeFnCall(
        id,
        NameResolution.toPT FnName.toPT fnName,
        List.map TypeReference.toPT typeArgs,
        List.map toPT args
      )
    | ST.EPipeEnum(id, typeName, caseName, fields) ->
      PT.EPipeEnum(
        id,
        NameResolution.toPT TypeName.toPT typeName,
        caseName,
        List.map toPT fields
      )

module Deprecation =
  type Deprecation<'name> =
    | NotDeprecated
    | RenamedTo of 'name
    | ReplacedBy of 'name
    | DeprecatedBecause of string

  let toST
    (f : 'name1 -> 'name2)
    (d : PT.Deprecation<'name1>)
    : ST.Deprecation<'name2> =
    match d with
    | PT.DeprecatedBecause str -> ST.DeprecatedBecause str
    | PT.RenamedTo name -> ST.RenamedTo(f name)
    | PT.ReplacedBy name -> ST.ReplacedBy(f name)
    | PT.NotDeprecated -> ST.NotDeprecated

  let toPT
    (f : 'name1 -> 'name2)
    (d : ST.Deprecation<'name1>)
    : PT.Deprecation<'name2> =
    match d with
    | ST.NotDeprecated -> PT.NotDeprecated
    | ST.RenamedTo name -> PT.RenamedTo(f name)
    | ST.ReplacedBy name -> PT.ReplacedBy(f name)
    | ST.DeprecatedBecause reason -> PT.DeprecatedBecause reason



module TypeDeclaration =
  module RecordField =
    let toST (f : PT.TypeDeclaration.RecordField) : ST.TypeDeclaration.RecordField =
      { name = f.name; typ = TypeReference.toST f.typ; description = f.description }

    let toPT (f : ST.TypeDeclaration.RecordField) : PT.TypeDeclaration.RecordField =
      { name = f.name; typ = TypeReference.toPT f.typ; description = f.description }

  module EnumField =
    let toST (f : PT.TypeDeclaration.EnumField) : ST.TypeDeclaration.EnumField =
      { typ = TypeReference.toST f.typ
        label = f.label
        description = f.description }

    let toPT (f : ST.TypeDeclaration.EnumField) : PT.TypeDeclaration.EnumField =
      { typ = TypeReference.toPT f.typ
        label = f.label
        description = f.description }

  module EnumCase =
    let toST (c : PT.TypeDeclaration.EnumCase) : ST.TypeDeclaration.EnumCase =
      { name = c.name
        fields = List.map EnumField.toST c.fields
        description = c.description }

    let toPT (c : ST.TypeDeclaration.EnumCase) : PT.TypeDeclaration.EnumCase =
      { name = c.name
        fields = List.map EnumField.toPT c.fields
        description = c.description }

  module Definition =
    let toST (d : PT.TypeDeclaration.Definition) : ST.TypeDeclaration.Definition =
      match d with
      | PT.TypeDeclaration.Alias typ ->
        ST.TypeDeclaration.Alias(TypeReference.toST typ)
      | PT.TypeDeclaration.Record(firstField, additionalFields) ->
        ST.TypeDeclaration.Record(
          RecordField.toST firstField,
          List.map RecordField.toST additionalFields
        )
      | PT.TypeDeclaration.Enum(firstCase, additionalCases) ->
        ST.TypeDeclaration.Enum(
          EnumCase.toST firstCase,
          List.map EnumCase.toST additionalCases
        )

    let toPT (d : ST.TypeDeclaration.Definition) : PT.TypeDeclaration.Definition =
      match d with
      | ST.TypeDeclaration.Alias typ ->
        PT.TypeDeclaration.Alias(TypeReference.toPT typ)
      | ST.TypeDeclaration.Record(firstField, additionalFields) ->
        PT.TypeDeclaration.Record(
          RecordField.toPT firstField,
          List.map RecordField.toPT additionalFields
        )
      | ST.TypeDeclaration.Enum(firstCase, additionalCases) ->
        PT.TypeDeclaration.Enum(
          EnumCase.toPT firstCase,
          List.map EnumCase.toPT additionalCases
        )

  let toST (d : PT.TypeDeclaration.T) : ST.TypeDeclaration.T =
    { typeParams = d.typeParams; definition = Definition.toST d.definition }

  let toPT (d : ST.TypeDeclaration.T) : PT.TypeDeclaration.T =
    { typeParams = d.typeParams; definition = Definition.toPT d.definition }


module Handler =
  module CronInterval =
    let toST (ci : PT.Handler.CronInterval) : ST.Handler.CronInterval =
      match ci with
      | PT.Handler.EveryDay -> ST.Handler.EveryDay
      | PT.Handler.EveryWeek -> ST.Handler.EveryWeek
      | PT.Handler.EveryFortnight -> ST.Handler.EveryFortnight
      | PT.Handler.EveryHour -> ST.Handler.EveryHour
      | PT.Handler.Every12Hours -> ST.Handler.Every12Hours
      | PT.Handler.EveryMinute -> ST.Handler.EveryMinute

    let toPT (ci : ST.Handler.CronInterval) : PT.Handler.CronInterval =
      match ci with
      | ST.Handler.EveryDay -> PT.Handler.EveryDay
      | ST.Handler.EveryWeek -> PT.Handler.EveryWeek
      | ST.Handler.EveryFortnight -> PT.Handler.EveryFortnight
      | ST.Handler.EveryHour -> PT.Handler.EveryHour
      | ST.Handler.Every12Hours -> PT.Handler.Every12Hours
      | ST.Handler.EveryMinute -> PT.Handler.EveryMinute

  module Spec =
    let toST (s : PT.Handler.Spec) : ST.Handler.Spec =
      match s with
      | PT.Handler.HTTP(route, method) -> ST.Handler.HTTP(route, method)
      | PT.Handler.Worker name -> ST.Handler.Worker name
      | PT.Handler.Cron(name, interval) ->
        ST.Handler.Cron(name, CronInterval.toST interval)
      | PT.Handler.REPL name -> ST.Handler.REPL name

    let toPT (s : ST.Handler.Spec) : PT.Handler.Spec =
      match s with
      | ST.Handler.HTTP(route, method) -> PT.Handler.HTTP(route, method)
      | ST.Handler.Worker name -> PT.Handler.Worker name
      | ST.Handler.Cron(name, interval) ->
        PT.Handler.Cron(name, CronInterval.toPT interval)
      | ST.Handler.REPL name -> PT.Handler.REPL name

  let toST (h : PT.Handler.T) : ST.Handler.T =
    { tlid = h.tlid; ast = Expr.toST h.ast; spec = Spec.toST h.spec }

  let toPT (h : ST.Handler.T) : PT.Handler.T =
    { tlid = h.tlid; ast = Expr.toPT h.ast; spec = Spec.toPT h.spec }

module DB =
  let toST (db : PT.DB.T) : ST.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toST db.typ }

  let toPT (db : ST.DB.T) : PT.DB.T =
    { tlid = db.tlid
      name = db.name
      version = db.version
      typ = TypeReference.toPT db.typ }

module UserType =
  let toST (t : PT.UserType.T) : ST.UserType.T =
    { tlid = t.tlid
      name = TypeName.UserProgram.toST t.name
      declaration = TypeDeclaration.toST t.declaration
      description = t.description
      deprecated = Deprecation.toST TypeName.toST t.deprecated }

  let toPT (t : ST.UserType.T) : PT.UserType.T =
    { tlid = t.tlid
      name = TypeName.UserProgram.toPT t.name
      declaration = TypeDeclaration.toPT t.declaration
      description = t.description
      deprecated = Deprecation.toPT TypeName.toPT t.deprecated }


module UserFunction =
  module Parameter =
    let toST (p : PT.UserFunction.Parameter) : ST.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toST p.typ; description = p.description }

    let toPT (p : ST.UserFunction.Parameter) : PT.UserFunction.Parameter =
      { name = p.name; typ = TypeReference.toPT p.typ; description = p.description }

  let toST (f : PT.UserFunction.T) : ST.UserFunction.T =
    { tlid = f.tlid
      name = FnName.UserProgram.toST f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toST f.parameters
      returnType = TypeReference.toST f.returnType
      description = f.description
      deprecated = Deprecation.toST FnName.toST f.deprecated
      body = Expr.toST f.body }

  let toPT (f : ST.UserFunction.T) : PT.UserFunction.T =
    { tlid = f.tlid
      name = FnName.UserProgram.toPT f.name
      typeParams = f.typeParams
      parameters = List.map Parameter.toPT f.parameters
      returnType = TypeReference.toPT f.returnType
      description = f.description
      deprecated = Deprecation.toPT FnName.toPT f.deprecated
      body = Expr.toPT f.body }

module Toplevel =
  let toST (tl : PT.Toplevel.T) : ST.Toplevel.T =
    match tl with
    | PT.Toplevel.TLHandler h -> ST.Toplevel.TLHandler(Handler.toST h)
    | PT.Toplevel.TLDB db -> ST.Toplevel.TLDB(DB.toST db)
    | PT.Toplevel.TLFunction f -> ST.Toplevel.TLFunction(UserFunction.toST f)
    | PT.Toplevel.TLType ut -> ST.Toplevel.TLType(UserType.toST ut)

  let toPT (tl : ST.Toplevel.T) : PT.Toplevel.T =
    match tl with
    | ST.Toplevel.TLHandler h -> PT.Toplevel.TLHandler(Handler.toPT h)
    | ST.Toplevel.TLDB db -> PT.Toplevel.TLDB(DB.toPT db)
    | ST.Toplevel.TLFunction f -> PT.Toplevel.TLFunction(UserFunction.toPT f)
    | ST.Toplevel.TLType ut -> PT.Toplevel.TLType(UserType.toPT ut)

module PackageFn =
  module Parameter =
    let toST (p : PT.PackageFn.Parameter) : ST.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toST p.typ; description = p.description }

    let toPT (p : ST.PackageFn.Parameter) : PT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toPT p.typ; description = p.description }

  let toST (fn : PT.PackageFn.T) : ST.PackageFn.T =
    { name = FnName.Package.toST fn.name
      parameters = List.map Parameter.toST fn.parameters
      returnType = TypeReference.toST fn.returnType
      description = fn.description
      deprecated = Deprecation.toST FnName.toST fn.deprecated
      body = Expr.toST fn.body
      typeParams = fn.typeParams
      id = fn.id
      tlid = fn.tlid }

  let toPT (fn : ST.PackageFn.T) : PT.PackageFn.T =
    { name = FnName.Package.toPT fn.name
      parameters = List.map Parameter.toPT fn.parameters
      returnType = TypeReference.toPT fn.returnType
      description = fn.description
      deprecated = Deprecation.toPT FnName.toPT fn.deprecated
      body = Expr.toPT fn.body
      typeParams = fn.typeParams
      id = fn.id
      tlid = fn.tlid }

module PackageType =
  let toST (pt : PT.PackageType.T) : ST.PackageType.T =
    { name = TypeName.Package.toST pt.name
      description = pt.description
      declaration = TypeDeclaration.toST pt.declaration
      deprecated = Deprecation.toST TypeName.toST pt.deprecated
      id = pt.id
      tlid = pt.tlid }

  let toPT (pt : ST.PackageType.T) : PT.PackageType.T =
    { name = TypeName.Package.toPT pt.name
      description = pt.description
      declaration = TypeDeclaration.toPT pt.declaration
      deprecated = Deprecation.toPT TypeName.toPT pt.deprecated
      id = pt.id
      tlid = pt.tlid }
