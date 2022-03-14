/// The types that are serialized for the program. This file only contains things
/// needed for binary serialization to store in the DB, and nearly all other
/// functionality is in other modules.
module LibExecution.ProgramTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

// The types in this files are serialized using MessagePack.
//
// https://github.com/neuecc/MessagePack-CSharp
// https://github.com/pocketberserker/MessagePack.FSharpExtensions
//
// Records should be annotated with `[<MessagePack.MessagePackObject>]`, and each
// field in the record should be annotated with `[<MessagePack.Key 0>]` (the zero
// should be replaced with a unique sequential index):
//
// [<MessagePack.MessagePackObject>] type X = { [<MessagePack.Key 0>] x : int
//
// If you forget to annotate all parts of a type (or a type referred to by that type)
// the serialized size will be much bigger and the serialization/deserialization time
// will be higher.
//
// To check this, check the file backend/serialization/oplist-format.json for "Item"
// -- if it contains the string then MessagePack couldn't use the optimized format
// and is falling back to less-optimized format.
//

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

/// Patterns - used for pattern matching in a match statement
[<MessagePack.MessagePackObject>]
type Pattern =
  | PVariable of id * string
  | PConstructor of id * string * List<Pattern>
  | PInteger of id * int64
  | PBool of id * bool
  | PCharacter of id * string
  | PString of id * string
  | PFloat of id * Sign * string * string
  | PNull of id
  | PBlank of id

/// Whether a function's result is unwrapped automatically (and, in the case of
/// Error/Nothing, sent to the error rail). NoRail functions are not unwrapped.
[<MessagePack.MessagePackObject>]
type SendToRail =
  | Rail
  | NoRail

/// Expressions - the main part of the language.
[<MessagePack.MessagePackObject>]
type Expr =
  | EInteger of id * int64
  | EBool of id * bool
  | EString of id * string
  /// A character is an Extended Grapheme Cluster (hence why we use a string). This
  /// is equivalent to one screen-visible "character" in Unicode.
  | ECharacter of id * string
  // Allow the user to have arbitrarily big numbers, even if they don't make sense as
  // floats. The float is split as we want to preserve what the user entered.
  // Strings are used as numbers lose the leading zeros (eg 7.00007)
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
  | EMatch of id * Expr * List<Pattern * Expr>
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr

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
  // A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string // replaces TAny
  | TFn of List<DType> * DType // replaces TLambda
  | TRecord of List<string * DType>
  | TDbList of DType // TODO: cleanup and remove
// This allows you to build up a record to eventually be the right shape.
// | TRecordWithFields of List<string * DType>
// | TRecordPlusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType
// | TRecordMinusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType


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

  let toTLID (tl : T) : tlid =
    match tl with
    | TLHandler h -> h.tlid
    | TLDB db -> db.tlid
    | TLFunction f -> f.tlid
    | TLType t -> t.tlid

module Secret =
  [<MessagePack.MessagePackObject>]
  type T =
    { [<MessagePack.Key 0>]
      name : string
      [<MessagePack.Key 1>]
      value : string }

[<MessagePack.MessagePackObject>]
type DeprecatedMigrationKind = | DeprecatedMigrationKind

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
  | DeleteTL of tlid
  | MoveTL of tlid * Position
  | SetFunction of UserFunction.T
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | SetExpr of tlid * id * Expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid
  | CreateDBMigration of tlid * id * id * (string * id * string * id) list
  | AddDBColToDBMigration of tlid * id * id
  | SetDBColNameInDBMigration of tlid * id * string
  | SetDBColTypeInDBMigration of tlid * id * string
  | AbandonDBMigration of tlid
  | DeleteColInDBMigration of tlid * id
  | DeleteDBCol of tlid * id
  | DeprecatedInitDBm of tlid * id * id * id * DeprecatedMigrationKind
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * Position * id * string
  | DeleteTLForever of tlid
  | DeleteFunctionForever of tlid
  | SetType of UserType.T
  | DeleteType of tlid
  | DeleteTypeForever of tlid

[<MessagePack.MessagePackObject>]
type Oplist = List<Op>

type TLIDOplists = List<tlid * Oplist>

// Not actually serialized
module Package =
  type Parameter = { name : string; typ : DType; description : string }

  type Fn =
    { name : FQFnName.PackageFnName
      body : Expr
      parameters : List<Parameter>
      returnType : DType
      description : string
      author : string
      deprecated : bool
      tlid : tlid }
