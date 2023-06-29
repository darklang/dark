/// The types that are serialized for the program. This type is only for
/// binary serialization to the DB, use ProgramTypes for anything else.
module LibBinarySerialization.SerializedTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

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
// The follow changes appear to be safe:
// - removing a variant at the end of an Enum (so long as that variant is not used in saved data)
// - renaming a variant in an Enum (even if that variant is used)
// - rename a field in a record (does not have the be the last field, don't change the keys of other fields)
// - remove a field from a record (keep the other fields in the right place)
// - adding a variant at the end of an Enum
//
// The following changes appear to be unsafe (and will require migrating data):
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
type NonEmptyList<'a> =
  { [<MessagePack.Key 0>]
    head : 'a
    [<MessagePack.Key 1>]
    tail : List<'a> }


/// Used to reference a type defined by a User, Standard Library module, or Package
module FQTypeName =
  [<MessagePack.MessagePackObject>]
  type StdlibTypeName =
    { [<MessagePack.Key 0>]
      modules : List<string>
      [<MessagePack.Key 1>]
      typ : string
      [<MessagePack.Key 2>]
      version : int }

  /// A type written by a Developer in their canvas
  [<MessagePack.MessagePackObject>]
  type UserTypeName =
    { [<MessagePack.Key 0>]
      modules : List<string>
      [<MessagePack.Key 1>]
      typ : string
      [<MessagePack.Key 2>]
      version : int }

  /// The name of a type in the package manager
  [<MessagePack.MessagePackObject>]
  type PackageTypeName =
    { [<MessagePack.Key 0>]
      owner : string
      [<MessagePack.Key 1>]
      modules : NonEmptyList<string>
      [<MessagePack.Key 2>]
      typ : string
      [<MessagePack.Key 3>]
      version : int }

  [<MessagePack.MessagePackObject>]
  type T =
    | Stdlib of StdlibTypeName
    | User of UserTypeName
    | Package of PackageTypeName

/// A Fully-Qualified Function Name
/// Includes package, module, and version information where relevant.
module FQFnName =

  /// Standard Library Function Name
  [<MessagePack.MessagePackObject>]
  type StdlibFnName =
    { [<MessagePack.Key 0>]
      modules : List<string>
      [<MessagePack.Key 1>]
      function_ : string
      [<MessagePack.Key 2>]
      version : int }

  /// A UserFunction is a function written by a Developer in their canvas
  [<MessagePack.MessagePackObject>]
  type UserFnName =
    { [<MessagePack.Key 0>]
      modules : List<string>
      [<MessagePack.Key 1>]
      function_ : string
      [<MessagePack.Key 2>]
      version : int }


  /// The name of a function in the package manager
  [<MessagePack.MessagePackObject>]
  type PackageFnName =
    { [<MessagePack.Key 0>]
      owner : string
      [<MessagePack.Key 1>]
      modules : NonEmptyList<string>
      [<MessagePack.Key 2>]
      function_ : string
      [<MessagePack.Key 3>]
      version : int }

  [<MessagePack.MessagePackObject>]
  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

/// A Fully-Qualified Constant Name
/// Includes package, module, and version information where relevant.
module FQConstantName =
  /// Standard Library Constant Name
  [<MessagePack.MessagePackObject>]
  type StdlibConstantName =
    { [<MessagePack.Key 0>]
      modules : List<string>
      [<MessagePack.Key 1>]
      constant : string
      [<MessagePack.Key 2>]
      version : int }

  /// A UserConstan is a constant written by a Developer in their canvas
  [<MessagePack.MessagePackObject>]
  type UserConstantName =
    { [<MessagePack.Key 0>]
      modules : List<string>
      [<MessagePack.Key 1>]
      constant : string
      [<MessagePack.Key 2>]
      version : int }

  /// The name of a constant in the package manager
  [<MessagePack.MessagePackObject>]
  type PackageConstantName =
    { [<MessagePack.Key 0>]
      owner : string
      [<MessagePack.Key 1>]
      modules : NonEmptyList<string>
      [<MessagePack.Key 2>]
      constant : string
      [<MessagePack.Key 3>]
      version : int }

  [<MessagePack.MessagePackObject>]
  type T =
    | User of UserConstantName // stub
    | Stdlib of StdlibConstantName
    | Package of PackageConstantName // stub



[<MessagePack.MessagePackObject>]
type TypeReference =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TString
  | TList of TypeReference
  | TDict of TypeReference
  | TDB of TypeReference
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TOption of TypeReference
  | TCustomType of typeName : FQTypeName.T * typeArgs : List<TypeReference>
  | TBytes
  | TResult of TypeReference * TypeReference
  | TVariable of string
  | TFn of List<TypeReference> * TypeReference
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
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

[<MessagePack.MessagePackObject>]
type MatchPattern =
  | MPVariable of id * string
  | MPEnum of id * caseName : string * fieldPats : List<MatchPattern>
  | MPInt of id * int64
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
  | EInt of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | EChar of id * string
  | EFloat of id * Sign * string * string
  | EUnit of id
  | EConstant of id * FQConstantName.T
  | ELet of id * LetPattern * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * typeArgs : List<TypeReference> * args : List<Expr>
  | EList of id * List<Expr>
  | ERecord of id * typeName : FQTypeName.T * fields : List<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : List<string * Expr>
  | EPipe of id * Expr * PipeExpr * List<PipeExpr>
  | EEnum of id * typeName : FQTypeName.T * caseName : string * fields : List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | EInfix of id * Infix * Expr * Expr
  | EDict of id * List<string * Expr>

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and [<MessagePack.MessagePackObject>] PipeExpr =
  | EPipeVariable of id * string
  | EPipeLambda of id * List<id * string> * Expr
  | EPipeInfix of id * Infix * Expr
  | EPipeFnCall of
    id *
    FQFnName.T *
    typeArgs : List<TypeReference> *
    args : List<Expr>
  | EPipeEnum of
    id *
    typeName : FQTypeName.T *
    caseName : string *
    fields : List<Expr>

[<MessagePack.MessagePackObject>]
type Deprecation<'name> =
  | NotDeprecated
  | RenamedTo of 'name
  | ReplacedBy of 'name
  | DeprecatedBecause of string


module CustomType =
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
  type T =
    | Alias of TypeReference
    | Record of firstField : RecordField * additionalFields : List<RecordField>
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>


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


module UserType =
  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : FQTypeName.UserTypeName
      [<MessagePack.Key 2>]
      definition : CustomType.T }

module UserConstant =
  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : FQConstantName.UserConstantName
      [<MessagePack.Key 2>]
      typ : TypeReference
      [<MessagePack.Key 3>]
      description : string
      [<MessagePack.Key 4>]
      deprecated : Deprecation<FQConstantName.T>
      [<MessagePack.Key 5>]
      body : Expr }

module UserFunction =
  [<MessagePack.MessagePackObject>]
  type Parameter =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      typ : TypeReference
      [<MessagePack.Key 2>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : FQFnName.UserFnName
      [<MessagePack.Key 2>]
      typeParams : List<string>
      [<MessagePack.Key 3>]
      parameters : List<Parameter>
      [<MessagePack.Key 4>]
      returnType : TypeReference
      [<MessagePack.Key 5>]
      description : string
      [<MessagePack.Key 6>]
      deprecated : Deprecation<FQFnName.T>
      [<MessagePack.Key 7>]
      body : Expr }

module PackageFn =
  [<MessagePack.MessagePackObject>]
  type Parameter =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      typ : TypeReference
      [<MessagePack.Key 2>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      id : System.Guid
      [<MessagePack.Key 2>]
      name : FQFnName.PackageFnName
      [<MessagePack.Key 3>]
      body : Expr
      [<MessagePack.Key 4>]
      typeParams : List<string>
      [<MessagePack.Key 5>]
      parameters : List<Parameter>
      [<MessagePack.Key 6>]
      returnType : TypeReference
      [<MessagePack.Key 7>]
      description : string
      [<MessagePack.Key 8>]
      deprecated : Deprecation<FQFnName.T> }

module PackageType =
  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      id : System.Guid
      [<MessagePack.Key 2>]
      name : FQTypeName.PackageTypeName
      // CLEANUP add type params
      [<MessagePack.Key 3>]
      definition : CustomType.T
      [<MessagePack.Key 4>]
      description : string
      [<MessagePack.Key 5>]
      deprecated : Deprecation<FQTypeName.T> }

module PackageConstant =
  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      id : System.Guid
      [<MessagePack.Key 2>]
      name : FQConstantName.PackageConstantName
      [<MessagePack.Key 3>]
      body : Expr
      [<MessagePack.Key 4>]
      typ : TypeReference
      [<MessagePack.Key 5>]
      description : string
      [<MessagePack.Key 6>]
      deprecated : Deprecation<FQConstantName.T> }

module Toplevel =
  [<MessagePack.MessagePackObject>]
  type T =
    | TLHandler of Handler.T
    | TLDB of DB.T
    | TLFunction of UserFunction.T
    | TLType of UserType.T
    | TLConstant of UserConstant.T

/// An Operation on a Canvas
///
/// "Op" is an abbreviation for Operation,
/// and is preferred throughout code and documentation.
[<MessagePack.MessagePackObject>]
type Op =
  | SetExpr of tlid * id * Expr
  | SetHandler of Handler.T
  | SetFunction of UserFunction.T
  | SetType of UserType.T
  | CreateDB of tlid * string * TypeReference
  | RenameDB of tlid * string

  | DeleteTL of tlid // CLEANUP move Deletes to API calls instead of Ops
  | DeleteFunction of tlid // CLEANUP move Deletes to API calls instead of Ops
  | DeleteType of tlid // CLEANUP move Deletes to API calls instead of Ops

  // CLEANUP this way of doing undo/redo is bad, should be per-user
  | TLSavepoint of tlid
  | UndoTL of tlid
  | RedoTL of tlid


[<MessagePack.MessagePackObject>]
type Oplist = List<Op>
