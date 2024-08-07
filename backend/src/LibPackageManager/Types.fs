// See README in `LibPackageManager`

/// This is basically a clone of the `@Darklang.LanguageTools.ProgramTypes`
/// module of Darklang types.
module LibPackageManager.Types

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

type ID = uint64
type TLID = uint64

type Sign =
  | Positive
  | Negative


module NameResolutionError =
  type ErrorType =
    | NotFound of names : List<string>
    | ExpectedEnumButNot of packageTypeID : uuid
    | ExpectedRecordButNot of packageTypeID : uuid
    | MissingEnumModuleName of caseName : string
    | InvalidPackageName of names : List<string>

  type NameType =
    | Function
    | Type
    | Constant

  type Error = { errorType : ErrorType; nameType : NameType }


module ProgramTypes =
  type NameResolution<'a> = Result<'a, NameResolutionError.Error>

  module FQTypeName =
    type Package = uuid
    type FQTypeName = Package of Package


  module FQFnName =
    type Builtin = { name : string; version : int }
    type Package = uuid

    type FQFnName =
      | Builtin of Builtin
      | Package of Package


  module FQConstantName =
    type Builtin = { name : string; version : int }
    type Package = uuid

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
    | ERecordFieldAccess of ID * Expr * string
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


  module PackageType =
    type Name = { owner : string; modules : List<string>; name : string }

    type PackageType =
      { id : uuid
        name : Name
        declaration : TypeDeclaration.TypeDeclaration
        description : string
        deprecated : Deprecation<FQTypeName.FQTypeName> }


  module PackageFn =
    type Name = { owner : string; modules : List<string>; name : string }

    type Parameter = { name : string; typ : TypeReference; description : string }

    type PackageFn =
      { id : uuid
        name : Name
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


  module PackageConstant =
    type Name = { owner : string; modules : List<string>; name : string }

    type PackageConstant =
      { id : uuid
        name : Name
        description : string
        deprecated : Deprecation<FQConstantName.FQConstantName>
        body : Const }
