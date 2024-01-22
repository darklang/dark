/// Fetches package items from a web-based package manager
/// hosted in a `dark-packages` canvas.
module LibCliExecution.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

type ID = uint64
type TLID = uint64

type Sign =
  | Positive
  | Negative


module NameResolutionError =
  type ErrorType =
    | NotFound
    | ExpectedEnumButNot
    | ExpectedRecordButNot
    | MissingEnumModuleName of caseName : string
    | InvalidPackageName

  type NameType =
    | Function
    | Type
    | Constant

  type Error = { errorType : ErrorType; nameType : NameType; names : List<string> }


module ProgramTypes =
  type NameResolution<'a> = Result<'a, NameResolutionError.Error>

  module FQTypeName =
    type Package =
      { owner : string; modules : List<string>; name : string; version : int }

    type FQTypeName = Package of Package


  module FQFnName =
    type Builtin = { modules : List<string>; name : string; version : int }
    type Package =
      { owner : string; modules : List<string>; name : string; version : int }

    type FQFnName =
      | Builtin of Builtin
      | Package of Package


  module FQConstantName =
    type Builtin = { modules : List<string>; name : string; version : int }
    type Package =
      { owner : string; modules : List<string>; name : string; version : int }

    type FQConstantName =
      | Builtin of Builtin
      | Package of Package


  type TypeReference =
    | TVariable of string
    | TUnit
    | TBool
    | TInt64
    | TUInt64
    | TInt8
    | TUInt8
    | TInt16
    | TUInt16
    | TInt32
    | TUInt32
    | TInt128
    | TUInt128
    | TFloat
    | TChar
    | TString
    | TDateTime
    | TUuid
    | TList of TypeReference
    | TTuple of TypeReference * TypeReference * List<TypeReference>
    | TDict of TypeReference
    | TCustomType of
      NameResolution<FQTypeName.FQTypeName> *
      typeArgs : List<TypeReference>
    | TDB of TypeReference
    | TFn of NEList<TypeReference> * TypeReference

  type LetPattern =
    | LPVariable of ID * name : string
    | LPTuple of ID * LetPattern * LetPattern * List<LetPattern>

  type MatchPattern =
    | MPVariable of ID * string
    | MPUnit of ID
    | MPBool of ID * bool
    | MPInt64 of ID * int64
    | MPUInt64 of ID * uint64
    | MPInt8 of ID * int8
    | MPUInt8 of ID * uint8
    | MPInt16 of ID * int16
    | MPUInt16 of ID * uint16
    | MPInt32 of ID * int32
    | MPUInt32 of ID * uint32
    | MPInt128 of ID * System.Int128
    | MPUInt128 of ID * System.UInt128
    | MPFloat of ID * Sign * string * string
    | MPChar of ID * string
    | MPString of ID * string
    | MPList of ID * List<MatchPattern>
    | MPListCons of ID * head : MatchPattern * tail : MatchPattern
    | MPTuple of ID * MatchPattern * MatchPattern * List<MatchPattern>
    | MPEnum of ID * caseName : string * fieldPats : List<MatchPattern>

  type BinaryOperation =
    | BinOpAnd
    | BinOpOr

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

  type Infix =
    | InfixFnCall of InfixFnName
    | BinOp of BinaryOperation

  type StringSegment =
    | StringText of string
    | StringInterpolation of Expr

  and PipeExpr =
    | EPipeVariable of ID * string * List<Expr>
    | EPipeLambda of ID * pats : NEList<LetPattern> * body : Expr
    | EPipeInfix of ID * Infix * Expr
    | EPipeFnCall of
      ID *
      NameResolution<FQFnName.FQFnName> *
      typeArgs : List<TypeReference> *
      args : List<Expr>
    | EPipeEnum of
      ID *
      typeName : NameResolution<FQTypeName.FQTypeName> *
      caseName : string *
      fields : List<Expr>


  and Expr =
    | EUnit of ID

    | EBool of ID * bool
    | EInt64 of ID * int64
    | EUInt64 of ID * uint64
    | EInt8 of ID * int8
    | EUInt8 of ID * uint8
    | EInt16 of ID * int16
    | EUInt16 of ID * uint16
    | EInt32 of ID * int32
    | EUInt32 of ID * uint32
    | EInt128 of ID * System.Int128
    | EUInt128 of ID * System.UInt128
    | EFloat of ID * Sign * string * string
    | EChar of ID * string
    | EString of ID * List<StringSegment>

    | EConstant of ID * NameResolution<FQConstantName.FQConstantName>

    | EList of ID * List<Expr>
    | EDict of ID * List<string * Expr>
    | ETuple of ID * Expr * Expr * List<Expr>
    | ERecord of ID * NameResolution<FQTypeName.FQTypeName> * List<string * Expr>
    | EEnum of
      ID *
      typeName : NameResolution<FQTypeName.FQTypeName> *
      caseName : string *
      fields : List<Expr>

    | ELet of ID * LetPattern * Expr * Expr
    | EFieldAccess of ID * Expr * string
    | EVariable of ID * string

    | EIf of ID * cond : Expr * thenExpr : Expr * elseExpr : Option<Expr>
    | EMatch of ID * arg : Expr * cases : List<MatchCase>
    | EPipe of ID * Expr * List<PipeExpr>

    | EInfix of ID * Infix * Expr * Expr
    | ELambda of ID * pats : NEList<LetPattern> * body : Expr
    | EApply of ID * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>
    | EFnName of ID * NameResolution<FQFnName.FQFnName>
    | ERecordUpdate of ID * record : Expr * updates : NEList<string * Expr>

  and MatchCase = { pat : MatchPattern; whenCondition : Option<Expr>; rhs : Expr }


  type Deprecation<'name> =
    | NotDeprecated
    | RenamedTo of 'name
    | ReplacedBy of 'name
    | DeprecatedBecause of string


  module TypeDeclaration =
    type RecordField = { name : string; typ : TypeReference; description : string }

    type EnumField =
      { typ : TypeReference; label : Option<string>; description : string }

    type EnumCase = { name : string; fields : List<EnumField>; description : string }

    type Definition =
      | Alias of TypeReference
      | Record of NEList<RecordField>
      | Enum of NEList<EnumCase>

    type TypeDeclaration = { typeParams : List<string>; definition : Definition }


  type PackageType =
    { tlid : TLID
      id : System.Guid
      name : FQTypeName.Package
      declaration : TypeDeclaration.TypeDeclaration
      description : string
      deprecated : Deprecation<FQTypeName.FQTypeName> }


  module PackageFn =
    type Parameter = { name : string; typ : TypeReference; description : string }

    type PackageFn =
      { tlid : TLID
        id : System.Guid
        name : FQFnName.Package
        body : Expr
        typeParams : List<string>
        parameters : NEList<Parameter>
        returnType : TypeReference
        description : string
        deprecated : Deprecation<FQFnName.FQFnName> }

  type Const =
    | CInt64 of int64
    | CUInt64 of uint64
    | CInt8 of int8
    | CUInt8 of uint8
    | CInt16 of int16
    | CUInt16 of uint16
    | CInt32 of int32
    | CUInt32 of uint32
    | CInt128 of System.Int128
    | CUInt128 of System.UInt128
    | CBool of bool
    | CString of string
    | CChar of string
    | CFloat of Sign * string * string
    | CUnit
    | CTuple of first : Const * second : Const * rest : List<Const>
    | CEnum of
      NameResolution<FQTypeName.FQTypeName> *
      caseName : string *
      List<Const>
    | CList of List<Const>
    | CDict of List<string * Const>


  type PackageConstant =
    { tlid : TLID
      id : System.Guid
      name : FQConstantName.Package
      description : string
      deprecated : Deprecation<FQConstantName.FQConstantName>
      body : Const }

module EPT = ProgramTypes

module ExternalTypesToProgramTypes =
  module NameResolutionError =
    module NameType =
      let toPT
        (nameType : NameResolutionError.NameType)
        : LibExecution.NameResolutionError.NameType =
        match nameType with
        | NameResolutionError.Type -> LibExecution.NameResolutionError.Type
        | NameResolutionError.Function -> LibExecution.NameResolutionError.Function
        | NameResolutionError.Constant -> LibExecution.NameResolutionError.Constant

    module ErrorType =
      let toPT
        (err : NameResolutionError.ErrorType)
        : LibExecution.NameResolutionError.ErrorType =
        match err with
        | NameResolutionError.ErrorType.NotFound ->
          LibExecution.NameResolutionError.NotFound
        | NameResolutionError.MissingEnumModuleName caseName ->
          LibExecution.NameResolutionError.MissingEnumModuleName caseName
        | NameResolutionError.InvalidPackageName ->
          LibExecution.NameResolutionError.InvalidPackageName
        | NameResolutionError.ExpectedEnumButNot ->
          LibExecution.NameResolutionError.ExpectedEnumButNot
        | NameResolutionError.ExpectedRecordButNot ->
          LibExecution.NameResolutionError.ExpectedRecordButNot

    module Error =
      let toPT
        (err : NameResolutionError.Error)
        : LibExecution.NameResolutionError.Error =
        { errorType = ErrorType.toPT err.errorType
          nameType = NameType.toPT err.nameType
          names = err.names }

  module NameResolution =
    let toPT
      (f : 's -> 'p)
      (result : EPT.NameResolution<'s>)
      : PT.NameResolution<'p> =
      match result with
      | Ok name -> Ok(f name)
      | Error err -> Error(NameResolutionError.Error.toPT err)


  module Sign =
    let toPT (s : Sign) : Prelude.Sign =
      match s with
      | Positive -> Prelude.Positive
      | Negative -> Prelude.Negative

  module TypeName =
    module Package =
      let toPT (p : EPT.FQTypeName.Package) : PT.FQTypeName.Package =
        { owner = p.owner; modules = p.modules; name = p.name; version = p.version }

    let toPT (fqfn : EPT.FQTypeName.FQTypeName) : PT.FQTypeName.FQTypeName =
      match fqfn with
      | EPT.FQTypeName.Package p -> PT.FQTypeName.Package(Package.toPT p)


  module FnName =
    module Builtin =
      let toPT (b : EPT.FQFnName.Builtin) : PT.FQFnName.Builtin =
        { modules = b.modules; name = b.name; version = b.version }

    module Package =
      let toPT (p : EPT.FQFnName.Package) : PT.FQFnName.Package =
        { owner = p.owner; modules = p.modules; name = p.name; version = p.version }

    let toPT (fqfn : EPT.FQFnName.FQFnName) : PT.FQFnName.FQFnName =
      match fqfn with
      | EPT.FQFnName.Builtin s -> PT.FQFnName.Builtin(Builtin.toPT s)
      | EPT.FQFnName.Package p -> PT.FQFnName.Package(Package.toPT p)

  module ConstantName =
    module Builtin =
      let toPT (b : EPT.FQConstantName.Builtin) : PT.FQConstantName.Builtin =
        { modules = b.modules; name = b.name; version = b.version }

    module Package =
      let toPT (p : EPT.FQConstantName.Package) : PT.FQConstantName.Package =
        { owner = p.owner; modules = p.modules; name = p.name; version = p.version }

    let toPT
      (fqfn : EPT.FQConstantName.FQConstantName)
      : PT.FQConstantName.FQConstantName =
      match fqfn with
      | EPT.FQConstantName.Builtin s -> PT.FQConstantName.Builtin(Builtin.toPT s)
      | EPT.FQConstantName.Package p -> PT.FQConstantName.Package(Package.toPT p)



  module InfixFnName =
    let toPT (name : ProgramTypes.InfixFnName) : PT.InfixFnName =
      match name with
      | EPT.ArithmeticPlus -> PT.ArithmeticPlus
      | EPT.ArithmeticMinus -> PT.ArithmeticMinus
      | EPT.ArithmeticMultiply -> PT.ArithmeticMultiply
      | EPT.ArithmeticDivide -> PT.ArithmeticDivide
      | EPT.ArithmeticModulo -> PT.ArithmeticModulo
      | EPT.ArithmeticPower -> PT.ArithmeticPower
      | EPT.ComparisonGreaterThan -> PT.ComparisonGreaterThan
      | EPT.ComparisonGreaterThanOrEqual -> PT.ComparisonGreaterThanOrEqual
      | EPT.ComparisonLessThan -> PT.ComparisonLessThan
      | EPT.ComparisonLessThanOrEqual -> PT.ComparisonLessThanOrEqual
      | EPT.ComparisonEquals -> PT.ComparisonEquals
      | EPT.ComparisonNotEquals -> PT.ComparisonNotEquals
      | EPT.StringConcat -> PT.StringConcat

  module TypeReference =
    let rec toPT (t : EPT.TypeReference) : PT.TypeReference =
      match t with
      | EPT.TInt64 -> PT.TInt64
      | EPT.TUInt64 -> PT.TUInt64
      | EPT.TInt8 -> PT.TInt8
      | EPT.TUInt8 -> PT.TUInt8
      | EPT.TInt16 -> PT.TInt16
      | EPT.TUInt16 -> PT.TUInt16
      | EPT.TInt32 -> PT.TInt32
      | EPT.TUInt32 -> PT.TUInt32
      | EPT.TInt128 -> PT.TInt128
      | EPT.TUInt128 -> PT.TUInt128
      | EPT.TFloat -> PT.TFloat
      | EPT.TBool -> PT.TBool
      | EPT.TUnit -> PT.TUnit
      | EPT.TString -> PT.TString
      | EPT.TList typ -> PT.TList(toPT typ)
      | EPT.TTuple(firstType, secondType, otherTypes) ->
        PT.TTuple(toPT firstType, toPT secondType, List.map toPT otherTypes)
      | EPT.TDict typ -> PT.TDict(toPT typ)
      | EPT.TDB typ -> PT.TDB(toPT typ)
      | EPT.TDateTime -> PT.TDateTime
      | EPT.TChar -> PT.TChar
      | EPT.TUuid -> PT.TUuid
      | EPT.TCustomType(t, typeArgs) ->
        PT.TCustomType(NameResolution.toPT TypeName.toPT t, List.map toPT typeArgs)
      | EPT.TVariable(name) -> PT.TVariable(name)
      | EPT.TFn(paramTypes, returnType) ->
        PT.TFn(NEList.map toPT paramTypes, toPT returnType)

  module BinaryOperation =
    let toPT (binop : EPT.BinaryOperation) : PT.BinaryOperation =
      match binop with
      | EPT.BinOpAnd -> PT.BinOpAnd
      | EPT.BinOpOr -> PT.BinOpOr

  module Infix =
    let toPT (infix : EPT.Infix) : PT.Infix =
      match infix with
      | EPT.InfixFnCall(fn) -> PT.InfixFnCall(InfixFnName.toPT fn)
      | EPT.BinOp binop -> PT.BinOp(BinaryOperation.toPT binop)

  module LetPattern =
    let rec toPT (p : EPT.LetPattern) : PT.LetPattern =
      match p with
      | EPT.LPVariable(id, str) -> PT.LPVariable(id, str)
      | EPT.LPTuple(id, first, second, theRest) ->
        PT.LPTuple(id, toPT first, toPT second, List.map toPT theRest)

  module MatchPattern =
    let rec toPT (p : EPT.MatchPattern) : PT.MatchPattern =
      match p with
      | EPT.MPVariable(id, str) -> PT.MPVariable(id, str)
      | EPT.MPEnum(id, caseName, fieldPats) ->
        PT.MPEnum(id, caseName, List.map toPT fieldPats)
      | EPT.MPInt64(id, i) -> PT.MPInt64(id, i)
      | EPT.MPUInt64(id, i) -> PT.MPUInt64(id, i)
      | EPT.MPInt8(id, i) -> PT.MPInt8(id, i)
      | EPT.MPUInt8(id, i) -> PT.MPUInt8(id, i)
      | EPT.MPInt16(id, i) -> PT.MPInt16(id, i)
      | EPT.MPUInt16(id, i) -> PT.MPUInt16(id, i)
      | EPT.MPInt32(id, i) -> PT.MPInt32(id, i)
      | EPT.MPUInt32(id, i) -> PT.MPUInt32(id, i)
      | EPT.MPInt128(id, i) -> PT.MPInt128(id, i)
      | EPT.MPUInt128(id, i) -> PT.MPUInt128(id, i)
      | EPT.MPBool(id, b) -> PT.MPBool(id, b)
      | EPT.MPChar(id, c) -> PT.MPChar(id, c)
      | EPT.MPString(id, s) -> PT.MPString(id, s)
      | EPT.MPFloat(id, s, w, f) -> PT.MPFloat(id, Sign.toPT s, w, f)
      | EPT.MPUnit id -> PT.MPUnit id
      | EPT.MPTuple(id, first, second, theRest) ->
        PT.MPTuple(id, toPT first, toPT second, List.map toPT theRest)
      | EPT.MPList(id, pats) -> PT.MPList(id, List.map toPT pats)
      | EPT.MPListCons(id, head, tail) -> PT.MPListCons(id, toPT head, toPT tail)


  module Expr =
    let rec toPT (e : EPT.Expr) : PT.Expr =
      match e with
      | EPT.EChar(id, char) -> PT.EChar(id, char)
      | EPT.EInt64(id, num) -> PT.EInt64(id, num)
      | EPT.EUInt64(id, num) -> PT.EUInt64(id, num)
      | EPT.EInt8(id, num) -> PT.EInt8(id, num)
      | EPT.EUInt8(id, num) -> PT.EUInt8(id, num)
      | EPT.EInt16(id, num) -> PT.EInt16(id, num)
      | EPT.EUInt16(id, num) -> PT.EUInt16(id, num)
      | EPT.EInt32(id, num) -> PT.EInt32(id, num)
      | EPT.EUInt32(id, num) -> PT.EUInt32(id, num)
      | EPT.EInt128(id, num) -> PT.EInt128(id, num)
      | EPT.EUInt128(id, num) -> PT.EUInt128(id, num)
      | EPT.EString(id, segment) ->
        PT.EString(id, List.map stringSegmentToPT segment)
      | EPT.EFloat(id, sign, whole, fraction) ->
        PT.EFloat(id, Sign.toPT sign, whole, fraction)
      | EPT.EBool(id, b) -> PT.EBool(id, b)
      | EPT.EUnit id -> PT.EUnit id
      | EPT.EConstant(id, name) ->
        PT.EConstant(id, NameResolution.toPT ConstantName.toPT name)
      | EPT.EVariable(id, var) -> PT.EVariable(id, var)
      | EPT.EFieldAccess(id, obj, fieldname) ->
        PT.EFieldAccess(id, toPT obj, fieldname)
      | EPT.EApply(id, name, typeArgs, args) ->
        PT.EApply(
          id,
          toPT name,
          List.map TypeReference.toPT typeArgs,
          NEList.map toPT args
        )
      | EPT.ELambda(id, pats, body) ->
        PT.ELambda(id, NEList.map LetPattern.toPT pats, toPT body)
      | EPT.ELet(id, pat, rhs, body) ->
        PT.ELet(id, LetPattern.toPT pat, toPT rhs, toPT body)
      | EPT.EIf(id, cond, thenExpr, elseExpr) ->
        PT.EIf(id, toPT cond, toPT thenExpr, Option.map toPT elseExpr)
      | EPT.EList(id, exprs) -> PT.EList(id, List.map toPT exprs)
      | EPT.ETuple(id, first, second, theRest) ->
        PT.ETuple(id, toPT first, toPT second, List.map toPT theRest)
      | EPT.ERecord(id, typeName, fields) ->
        PT.ERecord(
          id,
          NameResolution.toPT TypeName.toPT typeName,
          List.map (Tuple2.mapSecond toPT) fields
        )
      | EPT.ERecordUpdate(id, record, updates) ->
        PT.ERecordUpdate(
          id,
          toPT record,
          updates |> NEList.map (fun (name, expr) -> (name, toPT expr))
        )
      | EPT.EPipe(pipeID, expr1, rest) ->
        PT.EPipe(pipeID, toPT expr1, List.map pipeExprToPT rest)
      | EPT.EEnum(id, typeName, caseName, exprs) ->
        PT.EEnum(
          id,
          NameResolution.toPT TypeName.toPT typeName,
          caseName,
          List.map toPT exprs
        )
      | EPT.EMatch(id, mexpr, cases) ->
        PT.EMatch(id, toPT mexpr, List.map matchCaseToPT cases)
      | EPT.EInfix(id, infix, arg1, arg2) ->
        PT.EInfix(id, Infix.toPT infix, toPT arg1, toPT arg2)
      | EPT.EDict(id, pairs) -> PT.EDict(id, List.map (Tuple2.mapSecond toPT) pairs)
      | EPT.EFnName(id, name) -> PT.EFnName(id, NameResolution.toPT FnName.toPT name)

    and stringSegmentToPT (segment : EPT.StringSegment) : PT.StringSegment =
      match segment with
      | EPT.StringText text -> PT.StringText text
      | EPT.StringInterpolation expr -> PT.StringInterpolation(toPT expr)

    and pipeExprToPT (pipeExpr : EPT.PipeExpr) : PT.PipeExpr =
      match pipeExpr with
      | EPT.EPipeVariable(id, name, exprs) ->
        PT.EPipeVariable(id, name, List.map toPT exprs)
      | EPT.EPipeLambda(id, pats, body) ->
        PT.EPipeLambda(id, NEList.map LetPattern.toPT pats, toPT body)
      | EPT.EPipeInfix(id, infix, first) ->
        PT.EPipeInfix(id, Infix.toPT infix, toPT first)
      | EPT.EPipeFnCall(id, fnName, typeArgs, args) ->
        PT.EPipeFnCall(
          id,
          NameResolution.toPT FnName.toPT fnName,
          List.map TypeReference.toPT typeArgs,
          List.map toPT args
        )
      | EPT.EPipeEnum(id, typeName, caseName, fields) ->
        PT.EPipeEnum(
          id,
          NameResolution.toPT TypeName.toPT typeName,
          caseName,
          List.map toPT fields
        )

    and matchCaseToPT (case : EPT.MatchCase) : PT.MatchCase =
      { pat = MatchPattern.toPT case.pat
        whenCondition = Option.map toPT case.whenCondition
        rhs = toPT case.rhs }

  module Deprecation =
    let toPT
      (f : 'name1 -> 'name2)
      (d : EPT.Deprecation<'name1>)
      : PT.Deprecation<'name2> =
      match d with
      | EPT.NotDeprecated -> PT.NotDeprecated
      | EPT.RenamedTo name -> PT.RenamedTo(f name)
      | EPT.ReplacedBy name -> PT.ReplacedBy(f name)
      | EPT.DeprecatedBecause reason -> PT.DeprecatedBecause reason

  module TypeDeclaration =
    module RecordField =
      let toPT
        (f : EPT.TypeDeclaration.RecordField)
        : PT.TypeDeclaration.RecordField =
        { name = f.name
          typ = TypeReference.toPT f.typ
          description = f.description }

    module EnumField =
      let toPT (f : EPT.TypeDeclaration.EnumField) : PT.TypeDeclaration.EnumField =
        { typ = TypeReference.toPT f.typ
          label = f.label
          description = f.description }

    module EnumCase =
      let toPT (c : EPT.TypeDeclaration.EnumCase) : PT.TypeDeclaration.EnumCase =
        { name = c.name
          fields = List.map EnumField.toPT c.fields
          description = c.description }

    module Definition =
      let toPT (d : EPT.TypeDeclaration.Definition) : PT.TypeDeclaration.Definition =
        match d with
        | EPT.TypeDeclaration.Alias typ ->
          PT.TypeDeclaration.Alias(TypeReference.toPT typ)
        | EPT.TypeDeclaration.Record fields ->
          PT.TypeDeclaration.Record(NEList.map RecordField.toPT fields)
        | EPT.TypeDeclaration.Enum cases ->
          PT.TypeDeclaration.Enum(NEList.map EnumCase.toPT cases)

    let toPT (d : EPT.TypeDeclaration.TypeDeclaration) : PT.TypeDeclaration.T =
      { typeParams = d.typeParams; definition = Definition.toPT d.definition }

  module PackageFn =
    module Parameter =
      let toPT (p : EPT.PackageFn.Parameter) : PT.PackageFn.Parameter =
        { name = p.name
          typ = TypeReference.toPT p.typ
          description = p.description }

    let toPT (fn : EPT.PackageFn.PackageFn) : PT.PackageFn.T =
      { name = FnName.Package.toPT fn.name
        parameters = NEList.map Parameter.toPT fn.parameters
        returnType = TypeReference.toPT fn.returnType
        description = fn.description
        deprecated = Deprecation.toPT FnName.toPT fn.deprecated
        body = Expr.toPT fn.body
        typeParams = fn.typeParams
        id = fn.id
        tlid = fn.tlid }

  module PackageType =
    let toPT (pt : EPT.PackageType) : PT.PackageType.T =
      { name = TypeName.Package.toPT pt.name
        description = pt.description
        declaration = TypeDeclaration.toPT pt.declaration
        deprecated = Deprecation.toPT TypeName.toPT pt.deprecated
        id = pt.id
        tlid = pt.tlid }

  module Const =
    let rec toPT (c : EPT.Const) : PT.Const =
      match c with
      | EPT.CInt64 i -> PT.CInt64 i
      | EPT.CUInt64 i -> PT.CUInt64 i
      | EPT.CInt8 i -> PT.CInt8 i
      | EPT.CUInt8 i -> PT.CUInt8 i
      | EPT.CInt16 i -> PT.CInt16 i
      | EPT.CUInt16 i -> PT.CUInt16 i
      | EPT.CInt32 i -> PT.CInt32 i
      | EPT.CUInt32 i -> PT.CUInt32 i
      | EPT.CInt128 i -> PT.CInt128 i
      | EPT.CUInt128 i -> PT.CUInt128 i
      | EPT.CBool b -> PT.CBool b
      | EPT.CString s -> PT.CString s
      | EPT.CChar c -> PT.CChar c
      | EPT.CFloat(s, w, f) -> PT.CFloat(Sign.toPT s, w, f)
      | EPT.CUnit -> PT.CUnit
      | EPT.CTuple(first, second, rest) ->
        PT.CTuple(toPT first, toPT second, List.map toPT rest)
      | EPT.CEnum(typeName, caseName, fields) ->
        PT.CEnum(
          NameResolution.toPT TypeName.toPT typeName,
          caseName,
          List.map toPT fields
        )
      | EPT.CList l -> PT.CList(List.map toPT l)
      | EPT.CDict pairs -> PT.CDict(List.map (Tuple2.mapSecond toPT) pairs)


  module PackageConstant =
    let toPT (c : EPT.PackageConstant) : PT.PackageConstant.T =
      { name = ConstantName.Package.toPT c.name
        description = c.description
        deprecated = Deprecation.toPT ConstantName.toPT c.deprecated
        id = c.id
        tlid = c.tlid
        body = Const.toPT c.body }



module ET2PT = ExternalTypesToProgramTypes


// TODO: copy back to LibCloud/LibCloudExecution, or relocate somewhere central
// TODO: what should we do when the shape of types at the corresponding endpoints change?

/// The baseUrl is expected to be something like
/// - https://dark-packages.darklang.io normally
/// - http://dark-packages.dlio.localhost:11001 for local dev
let baseUrl =
  match
    System.Environment.GetEnvironmentVariable "DARK_CONFIG_PACKAGE_MANAGER_BASE_URL"
  with
  | null -> "https://packages.darklang.com"
  | var -> var

let packageManager : RT.PackageManager =
  let httpClient = new System.Net.Http.HttpClient() // CLEANUP pass this in as param? or mutate it externally?

  let withCache (f : 'name -> Ply<Option<'value>>) =
    let cache = System.Collections.Concurrent.ConcurrentDictionary<'name, 'value>()
    fun (name : 'name) ->
      uply {
        let mutable cached = Unchecked.defaultof<'value>
        let inCache = cache.TryGetValue(name, &cached)
        if inCache then
          return Some cached
        else
          let! result = f name
          match result with
          | Some v -> cache.TryAdd(name, v) |> ignore<bool>
          | None -> ()
          return result
      }

  let fetch
    (url : string)
    (f : 'serverType -> 'cachedType)
    : Ply<Option<'cachedType>> =
    uply {
      let! response = url |> httpClient.GetAsync

      let! responseStr = response.Content.ReadAsStringAsync()
      try
        if response.StatusCode = System.Net.HttpStatusCode.OK then
          let deserialized = responseStr |> Json.Vanilla.deserialize<'serverType>
          let cached = f deserialized
          return Some cached
        else if response.StatusCode = System.Net.HttpStatusCode.NotFound then
          return None
        else
          return
            Exception.raiseInternal
              "Failed to fetch package"
              [ "responseStr", responseStr ]
              null
      with e ->
        return
          Exception.raiseInternal
            "Failed to deserialize package"
            [ "responseStr", responseStr ]
            e
    }

  let fetchByName
    (kind : string)
    (owner : string)
    (modules : List<string>)
    (name : string)
    (version : int)
    (f : 'serverType -> 'cachedType)
    : Ply<Option<'cachedType>> =
    let modules = modules |> String.concat "."
    let namestring = $"{owner}.{modules}.{name}_v{version}"
    let url = $"{baseUrl}/{kind}/by-name/{namestring}"
    fetch url f

  let fetchById
    (kind : string)
    (id : tlid)
    (f : 'serverType -> 'cachedType)
    : Ply<Option<'cachedType>> =
    let url = $"{baseUrl}/{kind}/by-id/{id}"
    fetch url f


  { getType =
      withCache (fun name ->
        let conversionFn (parsed : EPT.PackageType) : RT.PackageType.T =
          parsed |> ET2PT.PackageType.toPT |> PT2RT.PackageType.toRT
        fetchByName
          "type"
          name.owner
          name.modules
          name.name
          name.version
          conversionFn)

    getFn =
      withCache (fun name ->
        let conversionFn (parsed : EPT.PackageFn.PackageFn) : RT.PackageFn.T =
          parsed |> ET2PT.PackageFn.toPT |> PT2RT.PackageFn.toRT
        fetchByName
          "function"
          name.owner
          name.modules
          name.name
          name.version
          conversionFn)

    getFnByTLID =
      withCache (fun tlid ->
        let conversionFn (parsed : EPT.PackageFn.PackageFn) : RT.PackageFn.T =
          parsed |> ET2PT.PackageFn.toPT |> PT2RT.PackageFn.toRT
        fetchById "function" tlid conversionFn)

    getConstant =
      withCache (fun name ->
        let conversionFn (parsed : EPT.PackageConstant) : RT.PackageConstant.T =
          parsed |> ET2PT.PackageConstant.toPT |> PT2RT.PackageConstant.toRT
        fetchByName
          "constant"
          name.owner
          name.modules
          name.name
          name.version
          conversionFn)

    init = uply { return () } }
