// Please see the README.
module rec LibPackageManager.JsonDeserialization

open SimpleJson
open Types

#nowarn "40"

module ID =
  let decoder : JsonDecoder<ID> = Decoders.uint64

module TLID =
  let decoder : JsonDecoder<TLID> = Decoders.uint64

module Sign =
  let decoder : JsonDecoder<Sign> =
    [ ("Positive", Decoders.enum0Fields Sign.Positive)
      ("Negative", Decoders.enum0Fields Sign.Negative) ]
    |> Map.ofList
    |> Decoders.du


module NameResolutionError =
  module ErrorType =
    type DU = NameResolutionError.ErrorType

    let decoder : JsonDecoder<DU> =
      [ ("NotFound", Decoders.enum1Field (Decoders.list Decoders.string) DU.NotFound)
        ("ExpectedEnumButNot",
         Decoders.enum1Field Decoders.uuid DU.ExpectedEnumButNot)
        ("ExpectedRecordButNot",
         Decoders.enum1Field Decoders.uuid DU.ExpectedRecordButNot)
        ("MissingEnumModuleName",
         Decoders.enum1Field Decoders.string DU.MissingEnumModuleName)
        ("InvalidPackageName",
         Decoders.enum1Field (Decoders.list Decoders.string) DU.InvalidPackageName) ]
      |> Map.ofList
      |> Decoders.du


  module NameType =
    type DU = NameResolutionError.NameType

    let decoder : JsonDecoder<DU> =
      [ ("Type", Decoders.enum0Fields DU.Type)
        ("Constant", Decoders.enum0Fields DU.Constant)
        ("Function", Decoders.enum0Fields DU.Function) ]
      |> Map.ofList
      |> Decoders.du


  module Error =
    let decoder : JsonDecoder<NameResolutionError.Error> =
      Decoders.obj2Fields
        "NameResolution"
        ("errorType", ErrorType.decoder)
        ("nameType", NameType.decoder)
        (fun errType nameType -> { errorType = errType; nameType = nameType })


module ProgramTypes =
  module NameResolution =
    let decoder
      (inner : JsonDecoder<'TInner>)
      : JsonDecoder<ProgramTypes.NameResolution<'TInner>> =
      [ ("Ok", Decoders.enum1Field inner Ok)
        ("Error", Decoders.enum1Field NameResolutionError.Error.decoder Error) ]
      |> Map.ofList
      |> Decoders.du


  module FQTypeName =
    module Package =
      let decoder : JsonDecoder<ProgramTypes.FQTypeName.Package> = Decoders.uuid

    module FQTypeName =
      type DU = ProgramTypes.FQTypeName.FQTypeName

      let decoder : JsonDecoder<DU> =
        [ ("Package", Decoders.enum1Field Package.decoder DU.Package) ]
        |> Map.ofList
        |> Decoders.du


  module FQFnName =
    module Builtin =
      let decoder : JsonDecoder<ProgramTypes.FQFnName.Builtin> =
        Decoders.obj2Fields
          "FQFnName.Builtin"
          ("name", Decoders.string)
          ("version", Decoders.int32)
          (fun name version -> { name = name; version = version })

    module Package =
      let decoder : JsonDecoder<ProgramTypes.FQFnName.Package> = Decoders.uuid

    module FQFnName =
      type DU = ProgramTypes.FQFnName.FQFnName

      let decoder : JsonDecoder<DU> =
        [ ("Builtin", Decoders.enum1Field Builtin.decoder DU.Builtin)
          ("Package", Decoders.enum1Field Package.decoder DU.Package) ]
        |> Map.ofList
        |> Decoders.du


  module FQConstantName =
    module Builtin =
      let decoder : JsonDecoder<ProgramTypes.FQConstantName.Builtin> =
        Decoders.obj2Fields
          "FQConstantName.Builtin"
          ("name", Decoders.string)
          ("version", Decoders.int32)
          (fun name version -> { name = name; version = version })

    module Package =
      let decoder : JsonDecoder<ProgramTypes.FQConstantName.Package> = Decoders.uuid

    module FQConstantName =
      type DU = ProgramTypes.FQConstantName.FQConstantName

      let decoder : JsonDecoder<DU> =
        [ ("Builtin", Decoders.enum1Field Builtin.decoder DU.Builtin)
          ("Package", Decoders.enum1Field Package.decoder DU.Package) ]
        |> Map.ofList
        |> Decoders.du

  module TypeReference =
    type DU = ProgramTypes.TypeReference

    let rec decoder : JsonDecoder<DU> =
      [ ("TVariable", Decoders.enum1Field Decoders.string DU.TVariable)
        ("TUnit", Decoders.enum0Fields DU.TUnit)
        ("TBool", Decoders.enum0Fields DU.TBool)
        ("TInt64", Decoders.enum0Fields DU.TInt64)
        ("TUInt64", Decoders.enum0Fields DU.TUInt64)
        ("TInt8", Decoders.enum0Fields DU.TInt8)
        ("TUInt8", Decoders.enum0Fields DU.TUInt8)
        ("TInt16", Decoders.enum0Fields DU.TInt16)
        ("TUInt16", Decoders.enum0Fields DU.TUInt16)
        ("TInt32", Decoders.enum0Fields DU.TInt32)
        ("TUInt32", Decoders.enum0Fields DU.TUInt32)
        ("TInt128", Decoders.enum0Fields DU.TInt128)
        ("TUInt128", Decoders.enum0Fields DU.TUInt128)
        ("TFloat", Decoders.enum0Fields DU.TFloat)
        ("TChar", Decoders.enum0Fields DU.TChar)
        ("TString", Decoders.enum0Fields DU.TString)
        ("TDateTime", Decoders.enum0Fields DU.TDateTime)
        ("TUuid", Decoders.enum0Fields DU.TUuid)
        ("TList", Decoders.enum1Field (fun ctx -> decoder ctx) DU.TList)
        ("TTuple",
         Decoders.enum3Fields
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (Decoders.list (fun ctx -> decoder ctx))
           (fun first second rest -> DU.TTuple(first, second, rest)))
        ("TDict", Decoders.enum1Field (fun ctx -> decoder ctx) DU.TDict)
        ("TCustomType",
         Decoders.enum2Fields
           (NameResolution.decoder FQTypeName.FQTypeName.decoder)
           (Decoders.list (fun ctx -> decoder ctx))
           (fun name typeArgs -> DU.TCustomType(name, typeArgs)))
        ("TDB", Decoders.enum1Field (fun ctx -> decoder ctx) DU.TDB)
        ("TFn",
         Decoders.enum2Fields
           (Decoders.list (fun ctx -> decoder ctx))
           (fun ctx -> decoder ctx)
           (fun typeArgs returnType ->
             DU.TFn(
               NEList.ofListUnsafe
                 "TODO: PT.TypeRef.TFn should have an NEList in Dark"
                 []
                 typeArgs,
               returnType
             ))) ]
      |> Map.ofList
      |> Decoders.du


  module LetPattern =
    type DU = ProgramTypes.LetPattern

    let rec decoder : JsonDecoder<DU> =
      [ ("LPVariable",
         Decoders.enum2Fields ID.decoder Decoders.string (fun id name ->
           DU.LPVariable(id, name)))
        ("LPTuple",
         Decoders.enum4Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id first second rest -> DU.LPTuple(id, first, second, rest))) ]
      |> Map.ofList
      |> Decoders.du

  module MatchPattern =
    type DU = ProgramTypes.MatchPattern

    let rec decoder : JsonDecoder<DU> =
      [ ("MPVariable",
         Decoders.enum2Fields ID.decoder Decoders.string (fun id name ->
           DU.MPVariable(id, name)))
        ("MPUnit", Decoders.enum1Field ID.decoder DU.MPUnit)
        ("MPBool",
         Decoders.enum2Fields ID.decoder Decoders.bool (fun id value ->
           DU.MPBool(id, value)))
        ("MPInt64",
         Decoders.enum2Fields ID.decoder Decoders.int64 (fun id value ->
           DU.MPInt64(id, value)))
        ("MPUInt64",
         Decoders.enum2Fields ID.decoder Decoders.uint64 (fun id value ->
           DU.MPUInt64(id, value)))
        ("MPInt8",
         Decoders.enum2Fields ID.decoder Decoders.int8 (fun id value ->
           DU.MPInt8(id, value)))
        ("MPUInt8",
         Decoders.enum2Fields ID.decoder Decoders.uint8 (fun id value ->
           DU.MPUInt8(id, value)))
        ("MPInt16",
         Decoders.enum2Fields ID.decoder Decoders.int16 (fun id value ->
           DU.MPInt16(id, value)))
        ("MPUInt16",
         Decoders.enum2Fields ID.decoder Decoders.uint16 (fun id value ->
           DU.MPUInt16(id, value)))
        ("MPInt32",
         Decoders.enum2Fields ID.decoder Decoders.int32 (fun id value ->
           DU.MPInt32(id, value)))
        ("MPUInt32",
         Decoders.enum2Fields ID.decoder Decoders.uint32 (fun id value ->
           DU.MPUInt32(id, value)))
        ("MPInt128",
         Decoders.enum2Fields ID.decoder Decoders.int128 (fun id value ->
           DU.MPInt128(id, value)))
        ("MPUInt128",
         Decoders.enum2Fields ID.decoder Decoders.uint128 (fun id value ->
           DU.MPUInt128(id, value)))
        ("MPFloat",
         Decoders.enum4Fields
           ID.decoder
           Sign.decoder
           Decoders.string
           Decoders.string
           (fun id sign mantissa exponent ->
             DU.MPFloat(id, sign, mantissa, exponent)))
        ("MPChar",
         Decoders.enum2Fields ID.decoder Decoders.string (fun id value ->
           DU.MPChar(id, value)))
        ("MPString",
         Decoders.enum2Fields ID.decoder Decoders.string (fun id value ->
           DU.MPString(id, value)))
        ("MPList",
         Decoders.enum2Fields
           ID.decoder
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id value -> DU.MPList(id, value)))
        ("MPListCons",
         Decoders.enum3Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (fun id head tail -> DU.MPListCons(id, head, tail)))
        ("MPTuple",
         Decoders.enum4Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id first second rest -> DU.MPTuple(id, first, second, rest)))
        ("MPEnum",
         Decoders.enum3Fields
           ID.decoder
           Decoders.string
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id caseName fieldPats -> DU.MPEnum(id, caseName, fieldPats))) ]
      |> Map.ofList
      |> Decoders.du


  module BinaryOperation =
    type DU = ProgramTypes.BinaryOperation

    let decoder : JsonDecoder<DU> =
      [ ("BinOpAnd", Decoders.enum0Fields DU.BinOpAnd)
        ("BinOpOr", Decoders.enum0Fields DU.BinOpOr) ]
      |> Map.ofList
      |> Decoders.du

  module InfixFnName =
    type DU = ProgramTypes.InfixFnName

    let decoder : JsonDecoder<DU> =
      [ ("ArithmeticPlus", Decoders.enum0Fields DU.ArithmeticPlus)
        ("ArithmeticMinus", Decoders.enum0Fields DU.ArithmeticMinus)
        ("ArithmeticMultiply", Decoders.enum0Fields DU.ArithmeticMultiply)
        ("ArithmeticDivide", Decoders.enum0Fields DU.ArithmeticDivide)
        ("ArithmeticModulo", Decoders.enum0Fields DU.ArithmeticModulo)
        ("ArithmeticPower", Decoders.enum0Fields DU.ArithmeticPower)
        ("ComparisonGreaterThan", Decoders.enum0Fields DU.ComparisonGreaterThan)
        ("ComparisonGreaterThanOrEqual",
         Decoders.enum0Fields DU.ComparisonGreaterThanOrEqual)
        ("ComparisonLessThan", Decoders.enum0Fields DU.ComparisonLessThan)
        ("ComparisonLessThanOrEqual",
         Decoders.enum0Fields DU.ComparisonLessThanOrEqual)
        ("ComparisonEquals", Decoders.enum0Fields DU.ComparisonEquals)
        ("ComparisonNotEquals", Decoders.enum0Fields DU.ComparisonNotEquals)
        ("StringConcat", Decoders.enum0Fields DU.StringConcat) ]
      |> Map.ofList
      |> Decoders.du


  module Infix =
    type DU = ProgramTypes.Infix

    let decoder : JsonDecoder<DU> =
      [ ("InfixFnCall", Decoders.enum1Field InfixFnName.decoder DU.InfixFnCall)
        ("BinOp", Decoders.enum1Field BinaryOperation.decoder DU.BinOp) ]
      |> Map.ofList
      |> Decoders.du


  module StringSegment =
    type DU = ProgramTypes.StringSegment

    let decoder : JsonDecoder<DU> =
      [ ("StringText", Decoders.enum1Field Decoders.string DU.StringText)
        ("StringInterpolation",
         Decoders.enum1Field (fun ctx -> Expr.decoder ctx) DU.StringInterpolation) ]
      |> Map.ofList
      |> Decoders.du


  module PipeExpr =
    type DU = ProgramTypes.PipeExpr

    let rec decoder : JsonDecoder<DU> =
      [ ("EPipeVariable",
         Decoders.enum3Fields
           ID.decoder
           Decoders.string
           (Decoders.list (fun ctx -> Expr.decoder ctx))
           (fun id name args -> DU.EPipeVariable(id, name, args)))
        ("EPipeLambda",
         Decoders.enum3Fields
           ID.decoder
           (Decoders.list (LetPattern.decoder))
           (fun ctx -> Expr.decoder ctx)
           (fun id pats body ->
             DU.EPipeLambda(
               id,
               pats
               |> NEList.ofListUnsafe
                 "TODO: Expr.EPipeLambda should have an NEList in Dark"
                 [],
               body
             )))
        ("EPipeInfix",
         Decoders.enum3Fields
           ID.decoder
           Infix.decoder
           (fun ctx -> Expr.decoder ctx)
           (fun id infix expr -> DU.EPipeInfix(id, infix, expr)))
        ("EPipeFnCall",
         Decoders.enum4Fields
           ID.decoder
           (NameResolution.decoder FQFnName.FQFnName.decoder)
           (Decoders.list (fun ctx -> TypeReference.decoder ctx))
           (Decoders.list (fun ctx -> Expr.decoder ctx))
           (fun id name typeArgs args -> DU.EPipeFnCall(id, name, typeArgs, args)))
        ("EPipeEnum",
         Decoders.enum4Fields
           ID.decoder
           (NameResolution.decoder FQTypeName.FQTypeName.decoder)
           Decoders.string
           (Decoders.list (fun ctx -> Expr.decoder ctx))
           (fun id typeName caseName fields ->
             DU.EPipeEnum(id, typeName, caseName, fields))) ]
      |> Map.ofList
      |> Decoders.du


  module Expr =
    type DU = ProgramTypes.Expr

    let rec decoder : JsonDecoder<DU> =
      [ ("EUnit", Decoders.enum1Field ID.decoder DU.EUnit)
        ("EBool",
         Decoders.enum2Fields ID.decoder Decoders.bool (fun id value ->
           DU.EBool(id, value)))
        ("EInt64",
         Decoders.enum2Fields ID.decoder Decoders.int64 (fun id value ->
           DU.EInt64(id, value)))
        ("EUInt64",
         Decoders.enum2Fields ID.decoder Decoders.uint64 (fun id value ->
           DU.EUInt64(id, value)))
        ("EInt8",
         Decoders.enum2Fields ID.decoder Decoders.int8 (fun id value ->
           DU.EInt8(id, value)))
        ("EUInt8",
         Decoders.enum2Fields ID.decoder Decoders.uint8 (fun id value ->
           DU.EUInt8(id, value)))
        ("EInt16",
         Decoders.enum2Fields ID.decoder Decoders.int16 (fun id value ->
           DU.EInt16(id, value)))
        ("EUInt16",
         Decoders.enum2Fields ID.decoder Decoders.uint16 (fun id value ->
           DU.EUInt16(id, value)))
        ("EInt32",
         Decoders.enum2Fields ID.decoder Decoders.int32 (fun id value ->
           DU.EInt32(id, value)))
        ("EUInt32",
         Decoders.enum2Fields ID.decoder Decoders.uint32 (fun id value ->
           DU.EUInt32(id, value)))
        ("EInt128",
         Decoders.enum2Fields ID.decoder Decoders.int128 (fun id value ->
           DU.EInt128(id, value)))
        ("EUInt128",
         Decoders.enum2Fields ID.decoder Decoders.uint128 (fun id value ->
           DU.EUInt128(id, value)))
        ("EFloat",
         Decoders.enum4Fields
           ID.decoder
           Sign.decoder
           Decoders.string
           Decoders.string
           (fun id sign mantissa exponent -> DU.EFloat(id, sign, mantissa, exponent)))
        ("EChar",
         Decoders.enum2Fields ID.decoder Decoders.string (fun id value ->
           DU.EChar(id, value)))
        ("EString",
         Decoders.enum2Fields
           ID.decoder
           (Decoders.list StringSegment.decoder)
           (fun id segments -> DU.EString(id, segments)))
        ("EConstant",
         Decoders.enum2Fields
           ID.decoder
           (NameResolution.decoder FQConstantName.FQConstantName.decoder)
           (fun id name -> DU.EConstant(id, name)))
        ("EList",
         Decoders.enum2Fields
           ID.decoder
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id value -> DU.EList(id, value)))
        ("EDict",
         Decoders.enum2Fields
           ID.decoder
           (Decoders.list (Decoders.pair Decoders.string (fun ctx -> decoder ctx)))
           (fun id value -> DU.EDict(id, value)))
        ("ETuple",
         Decoders.enum4Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id first second rest -> DU.ETuple(id, first, second, rest)))
        ("ERecord",
         Decoders.enum3Fields
           ID.decoder
           (NameResolution.decoder FQTypeName.FQTypeName.decoder)
           (Decoders.list (Decoders.pair Decoders.string (fun ctx -> decoder ctx)))
           (fun id name fields -> DU.ERecord(id, name, fields)))
        ("EEnum",
         Decoders.enum4Fields
           ID.decoder
           (NameResolution.decoder FQTypeName.FQTypeName.decoder)
           Decoders.string
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id typeName caseName fields ->
             DU.EEnum(id, typeName, caseName, fields)))
        ("ELet",
         Decoders.enum4Fields
           ID.decoder
           (LetPattern.decoder)
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (fun id pattern value body -> DU.ELet(id, pattern, value, body)))
        ("ERecordFieldAccess",
         Decoders.enum3Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           Decoders.string
           (fun id expr fieldName -> DU.ERecordFieldAccess(id, expr, fieldName)))
        ("EVariable",
         Decoders.enum2Fields ID.decoder Decoders.string (fun id name ->
           DU.EVariable(id, name)))
        ("EIf",
         Decoders.enum4Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (Decoders.option (fun ctx -> decoder ctx))
           (fun id cond thenExpr elseExpr -> DU.EIf(id, cond, thenExpr, elseExpr)))
        ("EMatch",
         Decoders.enum3Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (Decoders.list MatchCase.decoder)
           (fun id arg cases -> DU.EMatch(id, arg, cases)))
        ("EPipe",
         Decoders.enum3Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (Decoders.list (fun ctx -> PipeExpr.decoder ctx))
           (fun id expr pipeExprs -> DU.EPipe(id, expr, pipeExprs)))
        ("EInfix",
         Decoders.enum4Fields
           ID.decoder
           Infix.decoder
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (fun id infix left right -> DU.EInfix(id, infix, left, right)))
        ("ELambda",
         Decoders.enum3Fields
           ID.decoder
           (Decoders.list (LetPattern.decoder))
           (fun ctx -> decoder ctx)
           (fun id pats body ->
             DU.ELambda(
               id,
               pats
               |> NEList.ofListUnsafe
                 "TODO: Expr.ELambda should have an NEList in Dark"
                 [],
               body
             )))
        ("EApply",
         Decoders.enum4Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (Decoders.list (fun ctx -> TypeReference.decoder ctx))
           (Decoders.list (fun ctx -> decoder ctx))
           (fun id expr typeArgs args ->
             DU.EApply(
               id,
               expr,
               typeArgs,
               args
               |> NEList.ofListUnsafe
                 "TODO: Expr.EApply should have an NEList in Dark"
                 []
             )))
        ("EFnName",
         Decoders.enum2Fields
           ID.decoder
           (NameResolution.decoder FQFnName.FQFnName.decoder)
           (fun id name -> DU.EFnName(id, name)))
        ("ERecordUpdate",
         Decoders.enum3Fields
           ID.decoder
           (fun ctx -> decoder ctx)
           (Decoders.list (Decoders.pair Decoders.string (fun ctx -> decoder ctx)))
           (fun id record updates ->
             DU.ERecordUpdate(
               id,
               record,
               updates
               |> NEList.ofListUnsafe
                 "TODO: Expr.ERecordUpdate should have an NEList in Dark"
                 []
             ))) ]
      |> Map.ofList
      |> Decoders.du



  module MatchCase =
    let decoder : JsonDecoder<ProgramTypes.MatchCase> =
      Decoders.obj3Fields
        "MatchCase"
        ("pat", (fun ctx -> MatchPattern.decoder ctx))
        ("whenCondition", Decoders.option (fun ctx -> Expr.decoder ctx))
        ("rhs", (fun ctx -> Expr.decoder ctx))
        (fun pat whenCondition rhs ->
          { pat = pat; whenCondition = whenCondition; rhs = rhs })


  module Deprecation =
    type DU<'name> = ProgramTypes.Deprecation<'name>

    let decoder (nameDecoder : JsonDecoder<'name>) : JsonDecoder<DU<'name>> =
      [ ("NotDeprecated", Decoders.enum0Fields DU.NotDeprecated)
        ("RenamedTo", Decoders.enum1Field nameDecoder DU.RenamedTo)
        ("ReplacedBy", Decoders.enum1Field nameDecoder DU.ReplacedBy)
        ("DeprecatedBecause",
         Decoders.enum1Field Decoders.string DU.DeprecatedBecause) ]
      |> Map.ofList
      |> Decoders.du



  module TypeDeclaration =
    module RecordField =
      let decoder : JsonDecoder<ProgramTypes.TypeDeclaration.RecordField> =
        Decoders.obj3Fields
          "TypeDeclaration.RecordField"
          ("name", Decoders.string)
          ("typ", (fun ctx -> TypeReference.decoder ctx))
          ("description", Decoders.string)
          (fun name typ description ->
            { name = name; typ = typ; description = description })


    module EnumField =
      let decoder : JsonDecoder<ProgramTypes.TypeDeclaration.EnumField> =
        Decoders.obj3Fields
          "TypeDeclaration.EnumField"
          ("typ", (fun ctx -> TypeReference.decoder ctx))
          ("label", Decoders.option Decoders.string)
          ("description", Decoders.string)
          (fun typ label description ->
            { typ = typ; label = label; description = description })

    module EnumCase =
      let decoder : JsonDecoder<ProgramTypes.TypeDeclaration.EnumCase> =
        Decoders.obj3Fields
          "TypeDeclaration.EnumCase"
          ("name", Decoders.string)
          ("fields", Decoders.list EnumField.decoder)
          ("description", Decoders.string)
          (fun name fields description ->
            { name = name; fields = fields; description = description })


    module Definition =
      type DU = ProgramTypes.TypeDeclaration.Definition

      let decoder : JsonDecoder<DU> =
        [ ("Alias",
           Decoders.enum1Field (fun ctx -> TypeReference.decoder ctx) DU.Alias)
          ("Record",
           Decoders.enum1Field (Decoders.list RecordField.decoder) (fun fields ->
             fields
             |> NEList.ofListUnsafe
               "TODO: PT.Definition.Record should have an NEList in Dark"
               []
             |> DU.Record))
          ("Enum",
           Decoders.enum1Field (Decoders.list EnumCase.decoder) (fun cases ->
             cases
             |> NEList.ofListUnsafe
               "TODO: PT.Definition.Enum should have an NEList in Dark"
               []
             |> DU.Enum)) ]
        |> Map.ofList
        |> Decoders.du


    module TypeDeclaration =
      let decoder : JsonDecoder<ProgramTypes.TypeDeclaration.TypeDeclaration> =
        Decoders.obj2Fields
          "TypeDeclaration.TypeDeclaration"
          ("typeParams", Decoders.list Decoders.string)
          ("definition", Definition.decoder)
          (fun typeParams definition ->
            { typeParams = typeParams; definition = definition })

  module PackageType =
    module Name =
      let decoder : JsonDecoder<ProgramTypes.PackageType.Name> =
        Decoders.obj3Fields
          "PackageType.Name"
          ("owner", Decoders.string)
          ("modules", Decoders.list Decoders.string)
          ("name", Decoders.string)
          (fun owner modules name ->
            { owner = owner; modules = modules; name = name })

    let decoder : JsonDecoder<ProgramTypes.PackageType.PackageType> =
      Decoders.obj5Fields
        "PackageType"
        ("id", Decoders.uuid)
        ("name", Name.decoder)
        ("declaration", TypeDeclaration.TypeDeclaration.decoder)
        ("description", Decoders.string)
        ("deprecated", Deprecation.decoder FQTypeName.FQTypeName.decoder)
        (fun id name declaration description deprecated ->
          { id = id
            name = name
            declaration = declaration
            description = description
            deprecated = deprecated })


  module PackageFn =
    module Parameter =
      let decoder : JsonDecoder<ProgramTypes.PackageFn.Parameter> =
        Decoders.obj3Fields
          "PackageFn.Parameter"
          ("name", Decoders.string)
          ("typ", (fun ctx -> TypeReference.decoder ctx))
          ("description", Decoders.string)
          (fun name typ description ->
            { name = name; typ = typ; description = description })

    module PackageFn =
      module Name =
        let decoder : JsonDecoder<ProgramTypes.PackageFn.Name> =
          Decoders.obj3Fields
            "PackageFn.Name"
            ("owner", Decoders.string)
            ("modules", Decoders.list Decoders.string)
            ("name", Decoders.string)
            (fun owner modules name ->
              { owner = owner; modules = modules; name = name })

      let decoder : JsonDecoder<ProgramTypes.PackageFn.PackageFn> =
        Decoders.obj8Fields
          "PackageFn.PackageFn"
          ("id", Decoders.uuid)
          ("name", Name.decoder)
          ("body", (fun ctx -> Expr.decoder ctx))
          ("typeParams", Decoders.list Decoders.string)
          ("parameters", Decoders.list Parameter.decoder)
          ("returnType", (fun ctx -> TypeReference.decoder ctx))
          ("description", Decoders.string)
          ("deprecated", Deprecation.decoder FQFnName.FQFnName.decoder)
          (fun id name body typeParams parameters returnType description deprecated ->
            { id = id
              name = name
              body = body
              typeParams = typeParams
              parameters =
                parameters
                |> NEList.ofListUnsafe
                  "TODO: PT.PackageFn.PackageFn should have an NEList in Dark"
                  []
              returnType = returnType
              description = description
              deprecated = deprecated })

  module Const =
    type DU = ProgramTypes.Const

    let rec decoder : JsonDecoder<DU> =
      [ ("CInt64", Decoders.enum1Field Decoders.int64 DU.CInt64)
        ("CUInt64", Decoders.enum1Field Decoders.uint64 DU.CUInt64)
        ("CInt8", Decoders.enum1Field Decoders.int8 DU.CInt8)
        ("CUInt8", Decoders.enum1Field Decoders.uint8 DU.CUInt8)
        ("CInt16", Decoders.enum1Field Decoders.int16 DU.CInt16)
        ("CUInt16", Decoders.enum1Field Decoders.uint16 DU.CUInt16)
        ("CInt32", Decoders.enum1Field Decoders.int32 DU.CInt32)
        ("CUInt32", Decoders.enum1Field Decoders.uint32 DU.CUInt32)
        ("CInt128", Decoders.enum1Field Decoders.int128 DU.CInt128)
        ("CUInt128", Decoders.enum1Field Decoders.uint128 DU.CUInt128)
        ("CBool", Decoders.enum1Field Decoders.bool DU.CBool)
        ("CString", Decoders.enum1Field Decoders.string DU.CString)
        ("CChar", Decoders.enum1Field Decoders.string DU.CChar)
        ("CFloat",
         Decoders.enum3Fields
           Sign.decoder
           Decoders.string
           Decoders.string
           (fun sign mantissa exponent -> DU.CFloat(sign, mantissa, exponent)))
        ("CUnit", Decoders.enum0Fields DU.CUnit)
        ("CTuple",
         Decoders.enum3Fields
           (fun ctx -> decoder ctx)
           (fun ctx -> decoder ctx)
           (Decoders.list (fun ctx -> decoder ctx))
           (fun first second rest -> DU.CTuple(first, second, rest)))
        ("CEnum",
         Decoders.enum3Fields
           (NameResolution.decoder FQTypeName.FQTypeName.decoder)
           Decoders.string
           (Decoders.list (fun ctx -> decoder ctx))
           (fun typeName caseName fields -> DU.CEnum(typeName, caseName, fields)))
        ("CList",
         Decoders.enum1Field (Decoders.list (fun ctx -> decoder ctx)) DU.CList)
        ("CDict",
         Decoders.enum1Field
           (Decoders.list (Decoders.pair Decoders.string (fun ctx -> decoder ctx)))
           DU.CDict) ]
      |> Map.ofList
      |> Decoders.du


  module PackageConstant =
    module Name =
      let decoder : JsonDecoder<ProgramTypes.PackageConstant.Name> =
        Decoders.obj3Fields
          "PackageConstant.Name"
          ("owner", Decoders.string)
          ("modules", Decoders.list Decoders.string)
          ("name", Decoders.string)
          (fun owner modules name ->
            { owner = owner; modules = modules; name = name })

    let decoder : JsonDecoder<ProgramTypes.PackageConstant.PackageConstant> =
      Decoders.obj5Fields
        "PackageConstant"
        ("id", Decoders.uuid)
        ("name", Name.decoder)
        ("description", Decoders.string)
        ("deprecated", Deprecation.decoder FQConstantName.FQConstantName.decoder)
        ("body", (fun ctx -> Const.decoder ctx))
        (fun id name description deprecated body ->
          { id = id
            name = name
            description = description
            deprecated = deprecated
            body = body })
