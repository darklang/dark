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


/// Used to reference a type defined by a User, Standard Library module, or Package
module FQTypeName =
  [<MessagePack.MessagePackObject>]
  type StdlibTypeName =
    { [<MessagePack.Key 0>]
      typ : string }

  /// A type written by a Developer in their canvas
  [<MessagePack.MessagePackObject>]
  type UserTypeName =
    { [<MessagePack.Key 0>]
      typ : string

      [<MessagePack.Key 1>]
      version : int }

  [<MessagePack.MessagePackObject>]
  type T =
    | Stdlib of StdlibTypeName
    | User of UserTypeName

/// A Fully-Qualified Function Name
/// Includes package, module, and version information where relevant.
module FQFnName =

  /// Standard Library Function Name
  [<MessagePack.MessagePackObject>]
  type StdlibFnName =
    { [<MessagePack.Key 0>]
      module_ : string
      [<MessagePack.Key 1>]
      function_ : string
      [<MessagePack.Key 2>]
      version : int }

  /// A UserFunction is a function written by a Developer in their canvas
  [<MessagePack.MessagePackObject>]
  type UserFnName = string

  /// The name of a function in the package manager
  [<MessagePack.MessagePackObject>]
  type PackageFnName =
    { [<MessagePack.Key 0>]
      owner : string
      [<MessagePack.Key 1>]
      package : string
      [<MessagePack.Key 2>]
      module_ : string
      [<MessagePack.Key 3>]
      function_ : string
      [<MessagePack.Key 4>]
      version : int }

  [<MessagePack.MessagePackObject>]
  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

[<MessagePack.MessagePackObject>]
type DType =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TStr
  | TList of DType
  | TDict of DType
  | TIncomplete
  | TError
  | THttpResponse of DType
  | TDB of DType
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TOption of DType
  | TCustomType of typeName : FQTypeName.T * typeArgs : List<DType>
  | TBytes
  | TResult of DType * DType
  | TVariable of string
  | TFn of List<DType> * DType
  | TRecord of List<string * DType>
  | TDbList of DType // TODO: cleanup and remove
  | TTuple of DType * DType * List<DType>

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
type LetPattern = LPVariable of id * name : string

[<MessagePack.MessagePackObject>]
type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * caseName : string * fieldPats : List<MatchPattern>
  | MPInteger of id * int64
  | MPBool of id * bool
  | MPCharacter of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

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
  | EInteger of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | ECharacter of id * string
  | EFloat of id * Sign * string * string
  | EUnit of id
  | ELet of id * LetPattern * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * typeArgs : List<DType> * args : List<Expr>
  | EList of id * List<Expr>
  | ERecord of id * typeName : Option<FQTypeName.T> * fields : List<string * Expr>
  | EPipe of id * Expr * PipeExpr * List<PipeExpr>
  | EConstructor of
    id *
    typeName : Option<FQTypeName.T> *
    caseName : string *
    fields : List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr
  | ETuple of id * Expr * Expr * List<Expr>
  | EInfix of id * Infix * Expr * Expr

and PipeExpr =
  | EPipeLambda of id * List<id * string> * Expr
  | EPipeInfix of id * Infix * Expr * Expr
  | EPipeFnCall of id *
    FQFnName.T *
    typeArgs : List<DType> *
    args : List<Expr>
  | EPipeConstructor of id *
    typeName : Option<FQTypeName.T> *
    caseName : string *
    fields : List<Expr>
  | EPipeForbiddenExpr of Expr

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

module Pipe =
  /// Convert Regular Expr to PipeExpr
  let toPipeExpr (expr: Expr): PipeExpr  =
    match expr with
    | ELambda(id, lid, expr) -> EPipeLambda (id, lid, expr)
    | EInfix (id, infix, expr1, expr2) -> EPipeInfix (id, infix, expr1, expr2)
    | EFnCall (id, fQFnName, ltypeArgs, args) -> EPipeFnCall (id, fQFnName, ltypeArgs, args)
    | EConstructor (id,typeName,caseName,fields) -> EPipeConstructor (id,typeName,caseName,fields)
    | exp -> EPipeForbiddenExpr exp


  /// Convert PipeExpr to Regular Expr
  let toExpr (expr: PipeExpr): Expr =
    match expr with
    | EPipeLambda(id, lid, expr) -> ELambda (id, lid, expr)
    | EPipeInfix (id, infix, expr1, expr2) -> EInfix (id, infix, expr1, expr2)
    | EPipeFnCall (id, fQFnName, ltypeArgs, args) -> EFnCall (id, fQFnName, ltypeArgs, args)
    | EPipeConstructor (id,typeName,caseName,fields) -> EConstructor (id,typeName,caseName,fields)
    | EPipeForbiddenExpr exp -> exp

module CustomType =
  [<MessagePack.MessagePackObject>]
  type RecordField =
    { [<MessagePack.Key 0>]
      id : id
      [<MessagePack.Key 1>]
      name : string
      [<MessagePack.Key 2>]
      typ : DType }

  [<MessagePack.MessagePackObject>]
  type EnumField =
    { [<MessagePack.Key 0>]
      id : id
      [<MessagePack.Key 1>]
      typ : DType
      [<MessagePack.Key 2>]
      label : Option<string> }

  [<MessagePack.MessagePackObject>]
  type EnumCase =
    { [<MessagePack.Key 0>]
      id : id
      [<MessagePack.Key 1>]
      name : string
      [<MessagePack.Key 2>]
      fields : List<EnumField> }

  [<MessagePack.MessagePackObject>]
  type T =
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

  // We need to keep the IDs around until we get rid of them on the client
  [<MessagePack.MessagePackObject>]
  type ids =
    { [<MessagePack.Key 0>]
      moduleID : id
      [<MessagePack.Key 1>]
      nameID : id
      [<MessagePack.Key 2>]
      modifierID : id }

  [<MessagePack.MessagePackObject>]
  type Spec =
    | Worker of name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids
    | HTTP of route : string * method : string * ids : ids

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
  type Col =
    { [<MessagePack.Key 0>]
      name : Option<string>
      [<MessagePack.Key 1>]
      typ : Option<DType>
      [<MessagePack.Key 2>]
      nameID : id
      [<MessagePack.Key 3>]
      typeID : id }

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid

      [<MessagePack.Key 1>]
      nameID : id

      [<MessagePack.Key 2>]
      name : string

      [<MessagePack.Key 3>]
      version : int

      [<MessagePack.Key 4>]
      cols : List<Col> }


module UserType =
  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : FQTypeName.UserTypeName
      [<MessagePack.Key 2>]
      definition : CustomType.T }


module UserFunction =
  [<MessagePack.MessagePackObject>]
  type Parameter =
    { [<MessagePack.Key 0>]
      id : id
      [<MessagePack.Key 1>]
      name : string
      [<MessagePack.Key 2>]
      typ : DType
      [<MessagePack.Key 3>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : string
      [<MessagePack.Key 2>]
      typeParams : List<string>
      [<MessagePack.Key 3>]
      parameters : List<Parameter>
      [<MessagePack.Key 4>]
      returnType : DType
      [<MessagePack.Key 5>]
      description : string
      [<MessagePack.Key 6>]
      infix : bool
      [<MessagePack.Key 7>]
      body : Expr }

module Toplevel =
  [<MessagePack.MessagePackObject>]
  type T =
    | TLHandler of Handler.T
    | TLDB of DB.T
    | TLFunction of UserFunction.T
    | TLType of UserType.T

/// An Operation on a Canvas
///
/// "Op" is an abbreviation for Operation,
/// and is preferred throughout code and documentation.
[<MessagePack.MessagePackObject>]
type Op =
  | SetHandler of tlid * Handler.T
  | CreateDB of tlid * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid // CLEANUP move Deletes to API calls instead of Ops
  | SetFunction of UserFunction.T
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | SetExpr of tlid * id * Expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid // CLEANUP move Deletes to API calls instead of Ops
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * id * string
  | DeleteTLForever of tlid // CLEANUP not used, can be removed (carefully)
  | DeleteFunctionForever of tlid // CLEANUP not used, can be removed (carefully)
  | SetType of UserType.T
  | DeleteType of tlid // CLEANUP move Deletes to API calls instead of Ops
  | DeleteTypeForever of tlid // CLEANUP not used, can be removed (carefully)

[<MessagePack.MessagePackObject>]
type Oplist = List<Op>
