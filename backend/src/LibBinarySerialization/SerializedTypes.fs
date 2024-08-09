/// The types that are serialized for the program. This type is only for
/// binary serialization to the DB, use ProgramTypes for anything else.
module LibBinarySerialization.SerializedTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign
type uuid = Prelude.uuid

// The types in this files are serialized using MessagePack.
//
// https://github.com/neuecc/MessagePack-CSharp
// https://github.com/pocketberserker/MessagePack.FSharpExtensions
//
// All types should be annotated with `[<MessagePack.MessagePackObject>]`, and each
// field in the record should be annotated with `[<MessagePack.Key 0>]` (the zero
// should be replaced with a unique sequential index):
//
// [<MessagePack.MessagePackObject>] type X = { [<MessagePack.Key 0>] x : int }
//
// If you forget to annotate all parts of a type (or a type referred to by that type)
// the serializer will raise an exception. (It seems to be OK to not annotate some
// variants but not others; since it's unclear we annotate them all)
//
// All "code" in Dark is serialized using these types and stored in the DB, and we
// need to be very careful about changes to the types. "Safe" changes allow data
// saved in files in the old format to continue to be read by the serializers for the
// new format.
//
// The following changes appear to be safe:
// - removing a variant at the end of an Enum (so long as that variant is not used in saved data)
// - renaming a variant in an Enum (even if that variant is used)
// - rename a field in a record (does not have to be the last field, don't change the keys of other fields)
// - remove a field from a record (do not change the index of the other fields)
// - adding a new variant at the end of an Enum
//
// The following changes appear to be unsafe (and would require migrating data):
// - adding a new variant to an Enum that is not at the end
// - removing a variant in an Enum that is not at the end
// - reorder variants in an Enum
//
// The following changes have not been tested but are assumed to be unsafe:
// - adding a field to variant (eg add b to X(a,b))
// - add a field to a record
// - change the type of a field in a variant
// - change the type of a field in a record
// - removing a field from a variant (eg remove b to X(a,b))

[<MessagePack.MessagePackObject>]
type NEList<'a> =
  { [<MessagePack.Key 0>]
    head : 'a
    [<MessagePack.Key 1>]
    tail : List<'a> }


module FQTypeName =
  [<MessagePack.MessagePackObject>]
  type Package = uuid

  [<MessagePack.MessagePackObject>]
  type FQTypeName = Package of Package


module FQConstantName =
  [<MessagePack.MessagePackObject>]
  type Builtin =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      version : int }

  [<MessagePack.MessagePackObject>]
  type Package = uuid

  [<MessagePack.MessagePackObject>]
  type FQConstantName =
    | Builtin of Builtin
    | Package of Package


module FQFnName =
  [<MessagePack.MessagePackObject>]
  type Builtin =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      version : int }

  [<MessagePack.MessagePackObject>]
  type Package = uuid

  [<MessagePack.MessagePackObject>]
  type FQFnName =
    | Builtin of Builtin
    | Package of Package


module NameResolutionError =
  [<MessagePack.MessagePackObject>]
  type ErrorType =
    | NotFound of names : List<string>
    | ExpectedEnumButNot of packageTypeID : uuid
    | ExpectedRecordButNot of packageTypeID : uuid
    | MissingEnumModuleName of caseName : string
    | InvalidPackageName of names : List<string>

  [<MessagePack.MessagePackObject>]
  type NameType =
    | Function
    | Type
    | Constant

  [<MessagePack.MessagePackObject>]
  type Error =
    { [<MessagePack.Key 0>]
      errorType : ErrorType
      [<MessagePack.Key 1>]
      nameType : NameType }

[<MessagePack.MessagePackObject>]
type NameResolution<'a> = Result<'a, NameResolutionError.Error>



[<MessagePack.MessagePackObject>]
type TypeReference =
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
  | TBool
  | TUnit
  | TString
  | TList of TypeReference
  | TDict of TypeReference
  | TDB of TypeReference
  | TDateTime
  | TChar
  | TUuid
  | TCustomType of
    typeName : NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference>
  | TVariable of string
  | TFn of NEList<TypeReference> * TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>

[<MessagePack.MessagePackObject>]
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

[<MessagePack.MessagePackObject>]
type LetPattern =
  | LPVariable of id * name : string
  | LPUnit of id
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>


// We use bigint for serializing Int128 and UInt128 in MessagePack because
// BigInteger is a built-in supported type that supports handling large integers beyond the range of Int128 and UInt128.
// Creating custom formatters for Int128 and UInt128 would introduce complexity,
// especially when dealing with endianness and serialization/deserialization processes.

[<MessagePack.MessagePackObject>]
type MatchPattern =
  | MPVariable of id * string
  | MPEnum of id * caseName : string * fieldPats : List<MatchPattern>
  | MPInt64 of id * int64
  | MPUInt64 of id * uint64
  | MPInt8 of id * int8
  | MPUInt8 of id * uint8
  | MPInt16 of id * int16
  | MPUInt16 of id * uint16
  | MPInt32 of id * int32
  | MPUInt32 of id * uint32
  | MPInt128 of id * bigint
  | MPUInt128 of id * bigint
  | MPBool of id * bool
  | MPChar of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>
  | MPList of id * List<MatchPattern>
  | MPListCons of id * head : MatchPattern * tail : MatchPattern

[<MessagePack.MessagePackObject>]
type BinaryOperation =
  | BinOpAnd
  | BinOpOr

[<MessagePack.MessagePackObject>]
type Infix =
  | InfixFnCall of InfixFnName
  | BinOp of BinaryOperation

[<MessagePack.MessagePackObject>]
type Expr =
  | EInt64 of id * int64
  | EUInt64 of id * uint64
  | EInt8 of id * int8
  | EUInt8 of id * uint8
  | EInt16 of id * int16
  | EUInt16 of id * uint16
  | EInt32 of id * int32
  | EUInt32 of id * uint32
  | EInt128 of id * bigint
  | EUInt128 of id * bigint
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | EChar of id * string
  | EFloat of id * Sign * string * string
  | EUnit of id
  | EConstant of id * NameResolution<FQConstantName.FQConstantName>
  | ELet of id * LetPattern * Expr * Expr
  | EIf of id * cond : Expr * thenExpr : Expr * elseExpr : Option<Expr>
  | ELambda of id * pats : NEList<LetPattern> * body : Expr
  | ERecordFieldAccess of id * Expr * string
  | EVariable of id * string
  | EApply of id * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>
  | EList of id * List<Expr>
  | ERecord of
    id *
    typeName : NameResolution<FQTypeName.FQTypeName> *
    fields : List<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : NEList<string * Expr>
  | EPipe of id * Expr * List<PipeExpr>
  | EEnum of
    id *
    typeName : NameResolution<FQTypeName.FQTypeName> *
    caseName : string *
    fields : List<Expr>
  | EMatch of id * Expr * List<MatchCase>
  | ETuple of id * Expr * Expr * List<Expr>
  | EInfix of id * Infix * Expr * Expr
  | EDict of id * List<string * Expr>
  | EFnName of id * NameResolution<FQFnName.FQFnName>

and [<MessagePack.MessagePackObject>] MatchCase =
  { [<MessagePack.Key 0>]
    pat : MatchPattern
    [<MessagePack.Key 1>]
    whenCondition : Option<Expr>
    [<MessagePack.Key 2>]
    rhs : Expr }

and [<MessagePack.MessagePackObject>] StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and [<MessagePack.MessagePackObject>] PipeExpr =
  | EPipeVariable of id * string * List<Expr>
  | EPipeLambda of id * pats : NEList<LetPattern> * body : Expr
  | EPipeInfix of id * Infix * Expr
  | EPipeFnCall of
    id *
    NameResolution<FQFnName.FQFnName> *
    typeArgs : List<TypeReference> *
    args : List<Expr>
  | EPipeEnum of
    id *
    typeName : NameResolution<FQTypeName.FQTypeName> *
    caseName : string *
    fields : List<Expr>

[<MessagePack.MessagePackObject>]
type Deprecation<'name> =
  | NotDeprecated
  | RenamedTo of 'name
  | ReplacedBy of 'name
  | DeprecatedBecause of string


module TypeDeclaration =
  [<MessagePack.MessagePackObject>]
  type RecordField =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      typ : TypeReference
      [<MessagePack.Key 2>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type EnumField =
    { [<MessagePack.Key 0>]
      typ : TypeReference
      [<MessagePack.Key 1>]
      label : Option<string>
      [<MessagePack.Key 2>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type EnumCase =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      fields : List<EnumField>
      [<MessagePack.Key 2>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type Definition =
    | Alias of TypeReference
    | Record of NEList<RecordField>
    | Enum of NEList<EnumCase>

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      typeParams : List<string>
      [<MessagePack.Key 1>]
      definition : Definition }


[<MessagePack.MessagePackObject>]
type Const =
  | CInt64 of int64
  | CUInt64 of uint64
  | CInt8 of int8
  | CUInt8 of uint8
  | CInt16 of int16
  | CUInt16 of uint16
  | CInt32 of int32
  | CUInt32 of uint32
  | CInt128 of bigint
  | CUInt128 of bigint
  | CBool of bool
  | CString of string
  | CChar of string
  | CFloat of Sign * string * string
  | CUnit
  | CTuple of first : Const * second : Const * rest : List<Const>
  | CEnum of NameResolution<FQTypeName.FQTypeName> * caseName : string * List<Const>
  | CList of List<Const>
  | CDict of List<string * Const>



module PackageType =
  [<MessagePack.MessagePackObject>]
  type Name =
    { [<MessagePack.Key 0>]
      owner : string
      [<MessagePack.Key 1>]
      modules : List<string>
      [<MessagePack.Key 2>]
      name : string }

  [<MessagePack.MessagePackObject>]
  type PackageType =
    { [<MessagePack.Key 0>]
      id : System.Guid
      [<MessagePack.Key 1>]
      name : Name
      [<MessagePack.Key 2>]
      declaration : TypeDeclaration.T
      [<MessagePack.Key 3>]
      description : string
      [<MessagePack.Key 4>]
      deprecated : Deprecation<FQTypeName.FQTypeName> }

module PackageConstant =
  [<MessagePack.MessagePackObject>]
  type Name =
    { [<MessagePack.Key 0>]
      owner : string
      [<MessagePack.Key 1>]
      modules : List<string>
      [<MessagePack.Key 2>]
      name : string }

  [<MessagePack.MessagePackObject>]
  type PackageConstant =
    { [<MessagePack.Key 0>]
      id : System.Guid
      [<MessagePack.Key 1>]
      name : Name
      [<MessagePack.Key 2>]
      body : Const
      [<MessagePack.Key 3>]
      description : string
      [<MessagePack.Key 4>]
      deprecated : Deprecation<FQConstantName.FQConstantName> }


module PackageFn =
  [<MessagePack.MessagePackObject>]
  type Name =
    { [<MessagePack.Key 0>]
      owner : string
      [<MessagePack.Key 1>]
      modules : List<string>
      [<MessagePack.Key 2>]
      name : string }

  [<MessagePack.MessagePackObject>]
  type Parameter =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      typ : TypeReference
      [<MessagePack.Key 2>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type PackageFn =
    { [<MessagePack.Key 0>]
      id : System.Guid
      [<MessagePack.Key 1>]
      name : Name
      [<MessagePack.Key 2>]
      body : Expr
      [<MessagePack.Key 3>]
      typeParams : List<string>
      [<MessagePack.Key 4>]
      parameters : NEList<Parameter>
      [<MessagePack.Key 5>]
      returnType : TypeReference
      [<MessagePack.Key 6>]
      description : string
      [<MessagePack.Key 7>]
      deprecated : Deprecation<FQFnName.FQFnName> }


// -- User stuff



module Handler =
  [<MessagePack.MessagePackObject>]
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  [<MessagePack.MessagePackObject>]
  type Spec =
    | Worker of name : string
    | Cron of name : string * interval : CronInterval
    | REPL of name : string
    | HTTP of route : string * method : string

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      ast : Expr
      [<MessagePack.Key 2>]
      spec : Spec }


module DB =
  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : string
      [<MessagePack.Key 2>]
      version : int
      [<MessagePack.Key 3>]
      typ : TypeReference }



module Toplevel =
  [<MessagePack.MessagePackObject>]
  type T =
    | TLDB of DB.T
    | TLHandler of Handler.T
