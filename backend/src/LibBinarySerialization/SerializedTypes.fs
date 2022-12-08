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
type Position =
  { [<MessagePack.Key 0>]
    x : int
    [<MessagePack.Key 1>]
    y : int }

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
type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * string * List<MatchPattern>
  | MPInteger of id * int64
  | MPBool of id * bool
  | MPCharacter of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPNull of id
  | MPBlank of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

[<MessagePack.MessagePackObject>]
type SendToRail =
  | Rail
  | NoRail

[<MessagePack.MessagePackObject>]
type Expr =
  | EInteger of id * int64
  | EBool of id * bool
  | EString of id * string
  | ECharacter of id * string
  | EFloat of id * Sign * string * string
  | ENull of id
  | EBlank of id
  | ELet of id * string * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EBinOp of id * FQFnName.T * Expr * Expr * SendToRail
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * List<Expr> * SendToRail
  | EPartial of id * string * Expr
  | ERightPartial of id * string * Expr
  | ELeftPartial of id * string * Expr
  | EList of id * List<Expr>
  | ERecord of id * List<string * Expr>
  | EPipe of id * Expr * Expr * List<Expr>
  | EConstructor of id * string * List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr
  | ETuple of id * Expr * Expr * List<Expr>
  | EAnd of id * Expr * Expr
  | EOr of id * Expr * Expr

[<MessagePack.MessagePackObject>]
type DType =
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TList of DType
  | TDict of DType
  | TIncomplete
  | TError
  | THttpResponse of DType
  | TDB of DType
  | TDate
  | TChar
  | TPassword
  | TUuid
  | TOption of DType
  | TErrorRail
  | TUserType of string * int
  | TBytes
  | TResult of DType * DType
  | TVariable of string
  | TFn of List<DType> * DType
  | TRecord of List<string * DType>
  | TDbList of DType // TODO: cleanup and remove
  | TTuple of DType * DType * List<DType>

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
    | HTTP of route : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    // Deprecated but still supported form
    // CLEANUP: convert these into regular workers (change module name to WORKER,
    // check if they're unique first though)
    | OldWorker of modulename : string * name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids
    // If there's no module
    // CLEANUP: convert these into repl and get rid of this case
    | UnknownHandler of string * string * ids
    | HTTPBasic of route : string * method : string * ids : ids

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      pos : Position
      [<MessagePack.Key 2>]
      ast : Expr
      [<MessagePack.Key 3>]
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
      pos : Position
      [<MessagePack.Key 2>]
      nameID : id
      [<MessagePack.Key 3>]
      name : string
      [<MessagePack.Key 4>]
      version : int
      [<MessagePack.Key 5>]
      cols : List<Col> }

module UserType =
  [<MessagePack.MessagePackObject>]
  type RecordField =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      typ : Option<DType>
      [<MessagePack.Key 2>]
      nameID : id
      [<MessagePack.Key 3>]
      typeID : id }

  [<MessagePack.MessagePackObject>]
  type Definition = Record of List<RecordField>

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : string
      [<MessagePack.Key 2>]
      nameID : id
      [<MessagePack.Key 3>]
      version : int
      [<MessagePack.Key 4>]
      definition : Definition }

module UserFunction =
  [<MessagePack.MessagePackObject>]
  type Parameter =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      nameID : id
      [<MessagePack.Key 2>]
      typ : Option<DType>
      [<MessagePack.Key 3>]
      typeID : id
      [<MessagePack.Key 4>]
      description : string }

  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      tlid : tlid
      [<MessagePack.Key 1>]
      name : string
      [<MessagePack.Key 2>]
      nameID : id
      [<MessagePack.Key 3>]
      parameters : List<Parameter>
      [<MessagePack.Key 4>]
      returnType : DType
      [<MessagePack.Key 5>]
      returnTypeID : id
      [<MessagePack.Key 6>]
      description : string
      [<MessagePack.Key 7>]
      infix : bool
      [<MessagePack.Key 8>]
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
  | SetHandler of tlid * Position * Handler.T
  | CreateDB of tlid * Position * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid // CLEANUP move Deletes to API calls instead of Ops
  | MoveTL of tlid * Position
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
  | CreateDBWithBlankOr of tlid * Position * id * string
  | DeleteTLForever of tlid // CLEANUP not used, can be removed (carefully)
  | DeleteFunctionForever of tlid // CLEANUP not used, can be removed (carefully)
  | SetType of UserType.T
  | DeleteType of tlid // CLEANUP move Deletes to API calls instead of Ops
  | DeleteTypeForever of tlid // CLEANUP not used, can be removed (carefully)

[<MessagePack.MessagePackObject>]
type Oplist = List<Op>
