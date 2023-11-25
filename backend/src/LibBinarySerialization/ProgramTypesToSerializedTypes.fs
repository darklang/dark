/// Conversion functions from ProgramTypes to SerializedTypes and back
module LibBinarySerialization.ProgramTypesToSerializedTypes

open Prelude

// Used for conversion functions
module ST = SerializedTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser

module NEList =
  let toST (l : NEList<'a>) : ST.NEList<'a> = { head = l.head; tail = l.tail }

  let toPT (l : ST.NEList<'a>) : NEList<'a> = { head = l.head; tail = l.tail }

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
      { owner = p.owner; modules = p.modules; name = name; version = p.version }

    let toPT (p : ST.TypeName.Package) : PT.TypeName.Package =
      { owner = p.owner
        modules = p.modules
        name = PT.TypeName.TypeName p.name
        version = p.version }

  let toST (fqtn : PT.TypeName.TypeName) : ST.TypeName.TypeName =
    match fqtn with
    | PT.FQName.BuiltIn s -> ST.TypeName.BuiltIn(BuiltIn.toST s)
    | PT.FQName.UserProgram u -> ST.TypeName.UserProgram(UserProgram.toST u)
    | PT.FQName.Package p -> ST.TypeName.Package(Package.toST p)

  let toPT (fqfn : ST.TypeName.TypeName) : PT.TypeName.TypeName =
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
      { owner = p.owner; modules = p.modules; name = name; version = p.version }

    let toPT (p : ST.FnName.Package) : PT.FnName.Package =
      { owner = p.owner
        modules = p.modules
        name = PT.FnName.FnName p.name
        version = p.version }

  let toST (fqfn : PT.FnName.FnName) : ST.FnName.FnName =
    match fqfn with
    | PT.FQName.BuiltIn s -> ST.FnName.BuiltIn(BuiltIn.toST s)
    | PT.FQName.UserProgram u -> ST.FnName.UserProgram(UserProgram.toST u)
    | PT.FQName.Package p -> ST.FnName.Package(Package.toST p)

  let toPT (fqfn : ST.FnName.FnName) : PT.FnName.FnName =
    match fqfn with
    | ST.FnName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.toPT s)
    | ST.FnName.UserProgram u -> PT.FQName.UserProgram(UserProgram.toPT u)
    | ST.FnName.Package p -> PT.FQName.Package(Package.toPT p)


module NameResolutionError =

  module NameType =
    let toST
      (nameType : LibExecution.NameResolutionError.NameType)
      : ST.NameResolutionError.NameType =
      match nameType with
      | LibExecution.NameResolutionError.Type -> ST.NameResolutionError.Type
      | LibExecution.NameResolutionError.Function -> ST.NameResolutionError.Function
      | LibExecution.NameResolutionError.Constant -> ST.NameResolutionError.Constant

    let toPT
      (nameType : ST.NameResolutionError.NameType)
      : LibExecution.NameResolutionError.NameType =
      match nameType with
      | ST.NameResolutionError.Type -> LibExecution.NameResolutionError.Type
      | ST.NameResolutionError.Function -> LibExecution.NameResolutionError.Function
      | ST.NameResolutionError.Constant -> LibExecution.NameResolutionError.Constant


  module ErrorType =
    let toST
      (err : LibExecution.NameResolutionError.ErrorType)
      : ST.NameResolutionError.ErrorType =
      match err with
      | LibExecution.NameResolutionError.NotFound -> ST.NameResolutionError.NotFound
      | LibExecution.NameResolutionError.MissingEnumModuleName caseName ->
        ST.NameResolutionError.MissingEnumModuleName caseName
      | LibExecution.NameResolutionError.InvalidPackageName ->
        ST.NameResolutionError.InvalidPackageName
      | LibExecution.NameResolutionError.ExpectedEnumButNot ->
        ST.NameResolutionError.ExpectedEnumButNot
      | LibExecution.NameResolutionError.ExpectedRecordButNot ->
        ST.NameResolutionError.ExpectedRecordButNot


    let toPT
      (err : ST.NameResolutionError.ErrorType)
      : LibExecution.NameResolutionError.ErrorType =
      match err with
      | ST.NameResolutionError.ErrorType.NotFound ->
        LibExecution.NameResolutionError.NotFound
      | ST.NameResolutionError.MissingEnumModuleName caseName ->
        LibExecution.NameResolutionError.MissingEnumModuleName caseName
      | ST.NameResolutionError.InvalidPackageName ->
        LibExecution.NameResolutionError.InvalidPackageName
      | ST.NameResolutionError.ExpectedEnumButNot ->
        LibExecution.NameResolutionError.ExpectedEnumButNot
      | ST.NameResolutionError.ExpectedRecordButNot ->
        LibExecution.NameResolutionError.ExpectedRecordButNot

  module Error =
    let toST
      (err : LibExecution.NameResolutionError.Error)
      : ST.NameResolutionError.Error =
      { nameType = NameType.toST err.nameType
        errorType = ErrorType.toST err.errorType
        names = err.names }

    let toPT
      (err : ST.NameResolutionError.Error)
      : LibExecution.NameResolutionError.Error =
      { errorType = ErrorType.toPT err.errorType
        nameType = NameType.toPT err.nameType
        names = err.names }


module NameResolution =
  let toST (f : 'p -> 's) (result : PT.NameResolution<'p>) : ST.NameResolution<'s> =
    match result with
    | Ok name -> Ok(f name)
    | Error err -> Error(NameResolutionError.Error.toST err)

  let toPT (f : 's -> 'p) (result : ST.NameResolution<'s>) : PT.NameResolution<'p> =
    match result with
    | Ok name -> Ok(f name)
    | Error err -> Error(NameResolutionError.Error.toPT err)



module ConstantName =
  module BuiltIn =
    let toST (b : PT.ConstantName.BuiltIn) : ST.ConstantName.BuiltIn =
      let (PT.ConstantName.ConstantName name) = b.name
      { modules = b.modules; name = name; version = b.version }

    let toPT (b : ST.ConstantName.BuiltIn) : PT.ConstantName.BuiltIn =
      { modules = b.modules
        name = PT.ConstantName.ConstantName b.name
        version = b.version }

  module UserProgram =
    let toST (u : PT.ConstantName.UserProgram) : ST.ConstantName.UserProgram =
      let (PT.ConstantName.ConstantName name) = u.name
      { modules = u.modules; name = name; version = u.version }

    let toPT (u : ST.ConstantName.UserProgram) : PT.ConstantName.UserProgram =
      { modules = u.modules
        name = PT.ConstantName.ConstantName u.name
        version = u.version }

  module Package =
    let toST (p : PT.ConstantName.Package) : ST.ConstantName.Package =
      let (PT.ConstantName.ConstantName name) = p.name
      { owner = p.owner; modules = p.modules; name = name; version = p.version }

    let toPT (p : ST.ConstantName.Package) : PT.ConstantName.Package =
      { owner = p.owner
        modules = p.modules
        name = PT.ConstantName.ConstantName p.name
        version = p.version }

  let toST
    (fqConstant : PT.ConstantName.ConstantName)
    : ST.ConstantName.ConstantName =
    match fqConstant with
    | PT.FQName.BuiltIn s -> ST.ConstantName.BuiltIn(BuiltIn.toST s) // ConstantName.BuiltIn(BuiltIn.toST s)
    | PT.FQName.UserProgram u -> ST.ConstantName.UserProgram(UserProgram.toST u)
    | PT.FQName.Package p -> ST.ConstantName.Package(Package.toST p)

  let toPT
    (fqConstant : ST.ConstantName.ConstantName)
    : PT.ConstantName.ConstantName =
    match fqConstant with
    | ST.ConstantName.BuiltIn s -> PT.FQName.BuiltIn(BuiltIn.toPT s)
    | ST.ConstantName.UserProgram u -> PT.FQName.UserProgram(UserProgram.toPT u)
    | ST.ConstantName.Package p -> PT.FQName.Package(Package.toPT p)


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
    | PT.TInt64 -> ST.TInt64
    | PT.TUInt64 -> ST.TUInt64
    | PT.TInt8 -> ST.TInt8
    | PT.TUInt8 -> ST.TUInt8
    | PT.TInt16 -> ST.TInt16
    | PT.TUInt16 -> ST.TUInt16
    | PT.TInt32 -> ST.TInt32
    | PT.TUInt32 -> ST.TUInt32
    | PT.TInt128 -> ST.TInt128
    | PT.TUInt128 -> ST.TUInt128
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
    | PT.TUuid -> ST.TUuid
    | PT.TCustomType(t, typeArgs) ->
      ST.TCustomType(NameResolution.toST TypeName.toST t, List.map toST typeArgs)
    | PT.TVariable(name) -> ST.TVariable(name)
    | PT.TFn(paramTypes, returnType) ->
      ST.TFn(paramTypes |> NEList.map toST |> NEList.toST, toST returnType)

  let rec toPT (t : ST.TypeReference) : PT.TypeReference =
    match t with
    | ST.TInt64 -> PT.TInt64
    | ST.TUInt64 -> PT.TUInt64
    | ST.TInt8 -> PT.TInt8
    | ST.TUInt8 -> PT.TUInt8
    | ST.TInt16 -> PT.TInt16
    | ST.TUInt16 -> PT.TUInt16
    | ST.TInt32 -> PT.TInt32
    | ST.TUInt32 -> PT.TUInt32
    | ST.TInt128 -> PT.TInt128
    | ST.TUInt128 -> PT.TUInt128
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
    | ST.TUuid -> PT.TUuid
    | ST.TCustomType(t, typeArgs) ->
      PT.TCustomType(NameResolution.toPT TypeName.toPT t, List.map toPT typeArgs)
    | ST.TVariable(name) -> PT.TVariable(name)
    | ST.TFn(paramTypes, returnType) ->
      PT.TFn(paramTypes |> NEList.toPT |> NEList.map toPT, toPT returnType)

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
    | PT.LPUnit id -> ST.LPUnit id
    | PT.LPTuple(id, first, second, theRest) ->
      ST.LPTuple(id, toST first, toST second, List.map toST theRest)

  let rec toPT (p : ST.LetPattern) : PT.LetPattern =
    match p with
    | ST.LPVariable(id, str) -> PT.LPVariable(id, str)
    | ST.LPUnit id -> PT.LPUnit id
    | ST.LPTuple(id, first, second, theRest) ->
      PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)

module MatchPattern =
  let rec toST (p : PT.MatchPattern) : ST.MatchPattern =
    match p with
    | PT.MPVariable(id, str) -> ST.MPVariable(id, str)
    | PT.MPEnum(id, caseName, fieldPats) ->
      ST.MPEnum(id, caseName, List.map toST fieldPats)
    | PT.MPInt64(id, i) -> ST.MPInt64(id, i)
    | PT.MPUInt64(id, i) -> ST.MPUInt64(id, i)
    | PT.MPInt8(id, i) -> ST.MPInt8(id, i)
    | PT.MPUInt8(id, i) -> ST.MPUInt8(id, i)
    | PT.MPInt16(id, i) -> ST.MPInt16(id, i)
    | PT.MPUInt16(id, i) -> ST.MPUInt16(id, i)
    | PT.MPInt32(id, i) -> ST.MPInt32(id, i)
    | PT.MPUInt32(id, i) -> ST.MPUInt32(id, i)
    | PT.MPInt128(id, i) -> ST.MPInt128(id, System.Numerics.BigInteger.op_Implicit i)
    | PT.MPUInt128(id, i) ->
      ST.MPUInt128(id, System.Numerics.BigInteger.op_Implicit i)
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
    | ST.MPInt64(id, i) -> PT.MPInt64(id, i)
    | ST.MPUInt64(id, i) -> PT.MPUInt64(id, i)
    | ST.MPInt8(id, i) -> PT.MPInt8(id, i)
    | ST.MPUInt8(id, i) -> PT.MPUInt8(id, i)
    | ST.MPInt16(id, i) -> PT.MPInt16(id, i)
    | ST.MPUInt16(id, i) -> PT.MPUInt16(id, i)
    | ST.MPInt32(id, i) -> PT.MPInt32(id, i)
    | ST.MPUInt32(id, i) -> PT.MPUInt32(id, i)
    | ST.MPInt128(id, i) -> PT.MPInt128(id, System.Numerics.BigInteger.op_Explicit i)
    | ST.MPUInt128(id, i) ->
      PT.MPUInt128(id, System.Numerics.BigInteger.op_Explicit i)
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
    | PT.EInt64(id, num) -> ST.EInt64(id, num)
    | PT.EUInt64(id, num) -> ST.EUInt64(id, num)
    | PT.EInt8(id, num) -> ST.EInt8(id, num)
    | PT.EUInt8(id, num) -> ST.EUInt8(id, num)
    | PT.EInt16(id, num) -> ST.EInt16(id, num)
    | PT.EUInt16(id, num) -> ST.EUInt16(id, num)
    | PT.EInt128(id, num) ->
      ST.EInt128(id, System.Numerics.BigInteger.op_Implicit num)
    | PT.EUInt128(id, num) ->
      ST.EUInt128(id, System.Numerics.BigInteger.op_Implicit num)
    | PT.EInt32(id, num) -> ST.EInt32(id, num)
    | PT.EUInt32(id, num) -> ST.EUInt32(id, num)
    | PT.EString(id, segments) -> ST.EString(id, List.map stringSegmentToST segments)
    | PT.EFloat(id, sign, whole, fraction) -> ST.EFloat(id, sign, whole, fraction)
    | PT.EBool(id, b) -> ST.EBool(id, b)
    | PT.EUnit id -> ST.EUnit id
    | PT.EConstant(id, name) ->
      ST.EConstant(id, NameResolution.toST ConstantName.toST name)
    | PT.EVariable(id, var) -> ST.EVariable(id, var)
    | PT.EFieldAccess(id, obj, fieldname) -> ST.EFieldAccess(id, toST obj, fieldname)
    | PT.EApply(id, fn, typeArgs, args) ->
      ST.EApply(
        id,
        toST fn,
        List.map TypeReference.toST typeArgs,
        args |> NEList.map toST |> NEList.toST
      )
    | PT.EInfix(id, PT.InfixFnCall name, arg1, arg2) ->
      ST.EInfix(id, ST.InfixFnCall(InfixFnName.toST name), toST arg1, toST arg2)
    | PT.EInfix(id, PT.BinOp(op), arg1, arg2) ->
      ST.EInfix(id, ST.BinOp(BinaryOperation.toST (op)), toST arg1, toST arg2)
    | PT.ELambda(id, pats, body) ->
      ST.ELambda(id, NEList.map LetPattern.toST pats |> NEList.toST, toST body)
    | PT.ELet(id, pat, rhs, body) ->
      ST.ELet(id, LetPattern.toST pat, toST rhs, toST body)
    | PT.EIf(id, cond, thenExpr, elseExpr) ->
      ST.EIf(id, toST cond, toST thenExpr, Option.map toST elseExpr)
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
        updates |> NEList.map (fun (name, expr) -> (name, toST expr)) |> NEList.toST
      )
    | PT.EPipe(pipeID, expr1, rest) ->
      ST.EPipe(pipeID, toST expr1, List.map pipeExprToST rest)
    | PT.EEnum(id, typeName, caseName, fields) ->
      ST.EEnum(
        id,
        NameResolution.toST TypeName.toST typeName,
        caseName,
        List.map toST fields
      )
    | PT.EMatch(id, mexpr, cases) ->
      ST.EMatch(id, toST mexpr, List.map matchCaseToST cases)
    | PT.EDict(id, fields) -> ST.EDict(id, List.map (Tuple2.mapSecond toST) fields)
    | PT.EFnName(id, fnName) ->
      ST.EFnName(id, NameResolution.toST FnName.toST fnName)

  and stringSegmentToST (segment : PT.StringSegment) : ST.StringSegment =
    match segment with
    | PT.StringText text -> ST.StringText text
    | PT.StringInterpolation expr -> ST.StringInterpolation(toST expr)

  and pipeExprToST (pipeExpr : PT.PipeExpr) : ST.PipeExpr =
    match pipeExpr with
    | PT.EPipeVariable(id, name, exprs) ->
      ST.EPipeVariable(id, name, List.map toST exprs)
    | PT.EPipeLambda(id, pats, body) ->
      ST.EPipeLambda(id, NEList.map LetPattern.toST pats |> NEList.toST, toST body)
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
  and matchCaseToST (case : PT.MatchCase) : ST.MatchCase =
    { pat = MatchPattern.toST case.pat
      whenCondition = Option.map toST case.whenCondition
      rhs = toST case.rhs }

  let rec toPT (e : ST.Expr) : PT.Expr =
    match e with
    | ST.EChar(id, char) -> PT.EChar(id, char)
    | ST.EInt64(id, num) -> PT.EInt64(id, num)
    | ST.EUInt64(id, num) -> PT.EUInt64(id, num)
    | ST.EInt8(id, num) -> PT.EInt8(id, num)
    | ST.EUInt8(id, num) -> PT.EUInt8(id, num)
    | ST.EInt16(id, num) -> PT.EInt16(id, num)
    | ST.EUInt16(id, num) -> PT.EUInt16(id, num)
    | ST.EInt32(id, num) -> PT.EInt32(id, num)
    | ST.EUInt32(id, num) -> PT.EUInt32(id, num)
    | ST.EInt128(id, num) ->
      PT.EInt128(id, System.Numerics.BigInteger.op_Explicit num)
    | ST.EUInt128(id, num) ->
      PT.EUInt128(id, System.Numerics.BigInteger.op_Explicit num)
    | ST.EString(id, segment) -> PT.EString(id, List.map stringSegmentToPT segment)
    | ST.EFloat(id, sign, whole, fraction) -> PT.EFloat(id, sign, whole, fraction)
    | ST.EBool(id, b) -> PT.EBool(id, b)
    | ST.EUnit id -> PT.EUnit id
    | ST.EConstant(id, name) ->
      PT.EConstant(id, NameResolution.toPT ConstantName.toPT name)
    | ST.EVariable(id, var) -> PT.EVariable(id, var)
    | ST.EFieldAccess(id, obj, fieldname) -> PT.EFieldAccess(id, toPT obj, fieldname)
    | ST.EApply(id, fn, typeArgs, args) ->
      PT.EApply(
        id,
        toPT fn,
        List.map TypeReference.toPT typeArgs,
        args |> NEList.toPT |> NEList.map toPT
      )
    | ST.ELambda(id, pats, body) ->
      PT.ELambda(id, NEList.toPT pats |> NEList.map LetPattern.toPT, toPT body)
    | ST.ELet(id, pat, rhs, body) ->
      PT.ELet(id, LetPattern.toPT pat, toPT rhs, toPT body)
    | ST.EIf(id, cond, thenExpr, elseExpr) ->
      PT.EIf(id, toPT cond, toPT thenExpr, Option.map toPT elseExpr)
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
        updates |> NEList.toPT |> NEList.map (fun (name, expr) -> (name, toPT expr))
      )
    | ST.EPipe(pipeID, expr1, rest) ->
      PT.EPipe(pipeID, toPT expr1, List.map pipeExprToPT rest)
    | ST.EEnum(id, typeName, caseName, exprs) ->
      PT.EEnum(
        id,
        NameResolution.toPT TypeName.toPT typeName,
        caseName,
        List.map toPT exprs
      )
    | ST.EMatch(id, mexpr, cases) ->
      PT.EMatch(id, toPT mexpr, List.map matchCaseToPT cases)
    | ST.EInfix(id, infix, arg1, arg2) ->
      PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)
    | ST.EDict(id, pairs) -> PT.EDict(id, List.map (Tuple2.mapSecond toPT) pairs)
    | ST.EFnName(id, fnName) ->
      PT.EFnName(id, NameResolution.toPT FnName.toPT fnName)

  and stringSegmentToPT (segment : ST.StringSegment) : PT.StringSegment =
    match segment with
    | ST.StringText text -> PT.StringText text
    | ST.StringInterpolation expr -> PT.StringInterpolation(toPT expr)

  and pipeExprToPT (pipeExpr : ST.PipeExpr) : PT.PipeExpr =
    match pipeExpr with
    | ST.EPipeVariable(id, name, exprs) ->
      PT.EPipeVariable(id, name, List.map toPT exprs)
    | ST.EPipeLambda(id, pats, body) ->
      PT.EPipeLambda(id, NEList.toPT pats |> NEList.map LetPattern.toPT, toPT body)
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

  and matchCaseToPT (case : ST.MatchCase) : PT.MatchCase =
    { pat = MatchPattern.toPT case.pat
      whenCondition = Option.map toPT case.whenCondition
      rhs = toPT case.rhs }


module Const =
  let rec toST (c : PT.Const) : ST.Const =
    match c with
    | PT.Const.CInt64 i -> ST.Const.CInt64 i
    | PT.Const.CUInt64 i -> ST.Const.CUInt64 i
    | PT.Const.CInt8 i -> ST.Const.CInt8 i
    | PT.Const.CUInt8 i -> ST.Const.CUInt8 i
    | PT.Const.CInt16 i -> ST.Const.CInt16 i
    | PT.Const.CUInt16 i -> ST.Const.CUInt16 i
    | PT.Const.CInt32 i -> ST.Const.CInt32 i
    | PT.Const.CUInt32 i -> ST.Const.CUInt32 i
    | PT.Const.CInt128 i ->
      ST.Const.CInt128(System.Numerics.BigInteger.op_Implicit i)
    | PT.Const.CUInt128 i ->
      ST.Const.CUInt128(System.Numerics.BigInteger.op_Implicit i)
    | PT.Const.CBool b -> ST.Const.CBool b
    | PT.Const.CString s -> ST.Const.CString s
    | PT.Const.CChar c -> ST.Const.CChar c
    | PT.Const.CFloat(sign, w, f) -> ST.Const.CFloat(sign, w, f)
    | PT.Const.CUnit -> ST.Const.CUnit
    | PT.Const.CTuple(first, second, rest) ->
      ST.Const.CTuple(toST first, toST second, List.map toST rest)
    | PT.Const.CEnum(typeName, caseName, fields) ->
      ST.Const.CEnum(
        NameResolution.toST TypeName.toST typeName,
        caseName,
        List.map toST fields
      )
    | PT.Const.CList items -> ST.Const.CList(List.map toST items)
    | PT.Const.CDict items -> ST.Const.CDict(List.map (Tuple2.mapSecond toST) items)

  let rec toPT (c : ST.Const) : PT.Const =
    match c with
    | ST.Const.CInt64 i -> PT.Const.CInt64 i
    | ST.Const.CUInt64 i -> PT.Const.CUInt64 i
    | ST.Const.CInt8 i -> PT.Const.CInt8 i
    | ST.Const.CUInt8 i -> PT.Const.CUInt8 i
    | ST.Const.CInt16 i -> PT.Const.CInt16 i
    | ST.Const.CUInt16 i -> PT.Const.CUInt16 i
    | ST.Const.CInt32 i -> PT.Const.CInt32 i
    | ST.Const.CUInt32 i -> PT.Const.CUInt32 i
    | ST.Const.CInt128 i ->
      PT.Const.CInt128(System.Numerics.BigInteger.op_Explicit i)
    | ST.Const.CUInt128 i ->
      PT.Const.CUInt128(System.Numerics.BigInteger.op_Explicit i)
    | ST.Const.CBool b -> PT.Const.CBool b
    | ST.Const.CString s -> PT.Const.CString s
    | ST.Const.CChar c -> PT.Const.CChar c
    | ST.Const.CFloat(sign, w, f) -> PT.Const.CFloat(sign, w, f)
    | ST.Const.CUnit -> PT.Const.CUnit
    | ST.Const.CTuple(first, second, rest) ->
      PT.Const.CTuple(toPT first, toPT second, List.map toPT rest)
    | ST.Const.CEnum(typeName, caseName, fields) ->
      PT.Const.CEnum(
        NameResolution.toPT TypeName.toPT typeName,
        caseName,
        List.map toPT fields
      )
    | ST.Const.CList items -> PT.Const.CList(List.map toPT items)
    | ST.Const.CDict items -> PT.Const.CDict(List.map (Tuple2.mapSecond toPT) items)


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
      | PT.TypeDeclaration.Record fields ->
        ST.TypeDeclaration.Record(NEList.map RecordField.toST fields |> NEList.toST)
      | PT.TypeDeclaration.Enum cases ->
        ST.TypeDeclaration.Enum(NEList.map EnumCase.toST cases |> NEList.toST)

    let toPT (d : ST.TypeDeclaration.Definition) : PT.TypeDeclaration.Definition =
      match d with
      | ST.TypeDeclaration.Alias typ ->
        PT.TypeDeclaration.Alias(TypeReference.toPT typ)
      | ST.TypeDeclaration.Record fields ->
        PT.TypeDeclaration.Record(
          fields |> NEList.toPT |> NEList.map RecordField.toPT
        )
      | ST.TypeDeclaration.Enum cases ->
        PT.TypeDeclaration.Enum(cases |> NEList.toPT |> NEList.map EnumCase.toPT)

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
      parameters = NEList.map Parameter.toST f.parameters |> NEList.toST
      returnType = TypeReference.toST f.returnType
      description = f.description
      deprecated = Deprecation.toST FnName.toST f.deprecated
      body = Expr.toST f.body }

  let toPT (f : ST.UserFunction.T) : PT.UserFunction.T =
    { tlid = f.tlid
      name = FnName.UserProgram.toPT f.name
      typeParams = f.typeParams
      parameters = f.parameters |> NEList.toPT |> NEList.map Parameter.toPT
      returnType = TypeReference.toPT f.returnType
      description = f.description
      deprecated = Deprecation.toPT FnName.toPT f.deprecated
      body = Expr.toPT f.body }

module UserConstant =
  let toST (c : PT.UserConstant.T) : ST.UserConstant.T =
    { tlid = c.tlid
      name = ConstantName.UserProgram.toST c.name
      description = c.description
      deprecated = Deprecation.toST ConstantName.toST c.deprecated
      body = Const.toST c.body }

  let toPT (c : ST.UserConstant.T) : PT.UserConstant.T =
    { tlid = c.tlid
      name = ConstantName.UserProgram.toPT c.name
      description = c.description
      deprecated = Deprecation.toPT ConstantName.toPT c.deprecated
      body = Const.toPT c.body }

module Toplevel =
  let toST (tl : PT.Toplevel.T) : ST.Toplevel.T =
    match tl with
    | PT.Toplevel.TLHandler h -> ST.Toplevel.TLHandler(Handler.toST h)
    | PT.Toplevel.TLDB db -> ST.Toplevel.TLDB(DB.toST db)
    | PT.Toplevel.TLFunction f -> ST.Toplevel.TLFunction(UserFunction.toST f)
    | PT.Toplevel.TLType ut -> ST.Toplevel.TLType(UserType.toST ut)
    | PT.Toplevel.TLConstant c -> ST.Toplevel.TLConstant(UserConstant.toST c)

  let toPT (tl : ST.Toplevel.T) : PT.Toplevel.T =
    match tl with
    | ST.Toplevel.TLHandler h -> PT.Toplevel.TLHandler(Handler.toPT h)
    | ST.Toplevel.TLDB db -> PT.Toplevel.TLDB(DB.toPT db)
    | ST.Toplevel.TLFunction f -> PT.Toplevel.TLFunction(UserFunction.toPT f)
    | ST.Toplevel.TLType ut -> PT.Toplevel.TLType(UserType.toPT ut)
    | ST.Toplevel.TLConstant c -> PT.Toplevel.TLConstant(UserConstant.toPT c)

module PackageFn =
  module Parameter =
    let toST (p : PT.PackageFn.Parameter) : ST.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toST p.typ; description = p.description }

    let toPT (p : ST.PackageFn.Parameter) : PT.PackageFn.Parameter =
      { name = p.name; typ = TypeReference.toPT p.typ; description = p.description }

  let toST (fn : PT.PackageFn.T) : ST.PackageFn.T =
    { name = FnName.Package.toST fn.name
      parameters = NEList.map Parameter.toST fn.parameters |> NEList.toST
      returnType = TypeReference.toST fn.returnType
      description = fn.description
      deprecated = Deprecation.toST FnName.toST fn.deprecated
      body = Expr.toST fn.body
      typeParams = fn.typeParams
      id = fn.id
      tlid = fn.tlid }

  let toPT (fn : ST.PackageFn.T) : PT.PackageFn.T =
    { name = FnName.Package.toPT fn.name
      parameters = fn.parameters |> NEList.toPT |> NEList.map Parameter.toPT
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

module PackageConstant =
  let toST (pc : PT.PackageConstant.T) : ST.PackageConstant.T =
    { tlid = pc.tlid
      id = pc.id
      name = ConstantName.Package.toST pc.name
      body = Const.toST pc.body
      description = pc.description
      deprecated = Deprecation.toST ConstantName.toST pc.deprecated }

  let toPT (pc : ST.PackageConstant.T) : PT.PackageConstant.T =
    { tlid = pc.tlid
      id = pc.id
      name = ConstantName.Package.toPT pc.name
      body = Const.toPT pc.body
      description = pc.description
      deprecated = Deprecation.toPT ConstantName.toPT pc.deprecated }
