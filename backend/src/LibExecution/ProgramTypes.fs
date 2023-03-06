/// The types that the user sees
module LibExecution.ProgramTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

/// A Fully-Qualified Type Name
/// Includes package, module, and version information where relevant.
/// TODO: maybe this should be FQCustomTypeName or CustomTypeName or something?
module FQTypeName =

  /// Standard Library Type Name
  type StdlibTypeName =
    { module_ : string
      type_ : string
      // TODO: does this really need a version?
      // (Do we plan on releasing a v2 of any stdlib type?)
      version : int }

  /// A UserType is a type written by a Developer in their canvas
  type UserTypeName = { type_: string; version: int}

  /// The name of a type in the package manager
  type PackageTypeName =
    { owner : string
      package : string
      module_ : string
      type_ : string
      version : int }

  type T =
    | User of UserTypeName
    | Stdlib of StdlibTypeName
    | Package of PackageTypeName

/// A Fully-Qualified Function Name
/// Includes package, module, and version information where relevant.
module FQFnName =

  /// Standard Library Function Name
  type StdlibFnName = { module_ : string; function_ : string; version : int }

  /// Standard Library Infix Function Name
  // CLEANUP The module is only there for a few functions in the Date module, such as
  // Date::<. Making these infix wasn't a great idea, and we should remove them.
  type InfixStdlibFnName = { module_ : Option<string>; function_ : string }

  /// A UserFunction is a function written by a Developer in their canvas
  type UserFnName = string

  /// The name of a function in the package manager
  type PackageFnName =
    { owner : string
      package : string
      module_ : string
      function_ : string
      version : int }

  // We don't include InfixStdlibFnName here as that is used directly by EInfix
  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

/// Used for pattern matching in a match statement
type MatchPattern =
  | MPVariable of id * string
  // TODO: rebrand to MPEnum?
  | MPConstructor of id * string * List<MatchPattern>
  | MPInteger of id * int64
  | MPBool of id * bool
  | MPCharacter of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

type BinaryOperation =
  | BinOpAnd
  | BinOpOr

type Infix =
  | InfixFnCall of FQFnName.InfixStdlibFnName
  | BinOp of BinaryOperation

/// Expressions - the main part of the language.
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
  | EUnit of id
  | ELet of id * string * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EInfix of id * Infix * Expr * Expr
  // the id in the varname list is the analysis id, used to get a livevalue
  // from the analysis engine
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * List<Expr>
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | EAnonRecord of id * List<string * Expr>
  | EPipe of id * Expr * Expr * List<Expr>
  // Constructors include `Just`, `Nothing`, `Error`, `Ok`.  In practice the
  // expr list is currently always length 1 (for `Just`, `Error`, and `Ok`)
  // or length 0 (for `Nothing`).
  | EConstructor of id * string * List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>
  // Placeholder that indicates the target of the Thread. May be movable at
  // some point
  | EPipeTarget of id
  // EFeatureFlag: id, flagName, condExpr, caseAExpr, caseBExpr
  | EFeatureFlag of id * string * Expr * Expr * Expr
  | ECustomRecord of id * FQTypeName.T * List<string * Expr>
  | ECustomEnum of id * FQTypeName.T * caseName: string * fields: List<Expr>
  // TODO: alias/abbreviation


type DType =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TStr
  | TList of T
  | TTuple of T * T * List<T>
  | TDict of T
  | TIncomplete
  | TError
  | THttpResponse of T
  | TDB of T
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TOption of T
  | TBytes
  | TResult of T * T
  // A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string // replaces TAny
  | TFn of List<T> * T // replaces TLambda
  | TAnonRecord of List<string * T>
  | TDbList of T // TODO: cleanup and remove
  // This allows you to build up a record to eventually be the right shape.
  // | TRecordWithFields of List<string * T>
  // | TRecordPlusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType
  // | TRecordMinusField of string (* polymorphic type name, like TVariable *)  * string (* record field name *)  * DType
  | TCustomType of FQTypeName.T


/// Complex/custom types
/// defined by a standard library, user/canvas, or package
module CustomType =
  /// Model types like this:
  /// type A = { field: DInt; field2: DRecord }
  module Record =
    // Q: why does it need a typeID? Why does it need a nameID?
    type Field = { nameID : id; name : string; type_ : DType; typeID : id }
    type T = { name: string; fields: List<Field>}

  // Goal: support enums like this
  /// type X =
  ///   | A
  ///   | B of int
  ///   | C of int * string
  ///   | D of i: int * string
  ///
  /// - one or more cases (no upper limit)
  /// - each case may have 0 or more fields (no upper limit)
  /// - each field has a type
  /// - a field may have an optional label/name
  ///
  /// TODO: or should this be called a Union, or DU?
  module Enum =
    type Field = { type_: T; label: Option<string> }
    type Case = { nameID: id; name: string; cases: List<Field> }
    type T = { name: string; firstCase: Case; additionalCases: List<Case> }

  type T =
    // TODO: alias/abbreviation?
    // e.g. `type SomePair = int * string`

    | Record of Record.T

    | Enum of Enum.T


module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  // We need to keep the IDs around until we get rid of them on the client
  type ids = { moduleID : id; nameID : id; modifierID : id }

  type Spec =
    | HTTP of route : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids

  type T = { tlid : tlid; ast : Expr; spec : Spec }


module DB =
  type Col = { name : Option<string>; typ : Option<DType.T>; nameID : id; typeID : id }

  type T =
    { tlid : tlid
      name : string
      nameID : id
      version : int
      cols : List<Col> }


// kill usertype? Really
type UserType =
  // type RecordField = { name : string; typ : Option<DType.T>; nameID : id; typeID : id }
  // type Definition = Record of List<RecordField>

    { tlid : tlid
      name : string
      nameID : id
      version : int
      definition : CustomType.T }

module UserFunction =
  type Parameter =
    { name : string
      nameID : id
      typ : Option<DType.T>
      typeID : id
      description : string }

  type T =
    { tlid : tlid
      name : string // why isn't this FQFnName.UserFunction?
      nameID : id // I don't understand why we need this
      parameters : List<Parameter>
      returnType : DType.T
      returnTypeID : id
      description : string
      infix : bool
      body : Expr }

module Toplevel =
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


/// An Operation on a Canvas
///
/// "Op" is an abbreviation for Operation,
/// and is preferred throughout code and documentation.
type Op =
  | SetHandler of tlid * Handler.T
  | CreateDB of tlid * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid // CLEANUP move Deletes to API calls instead of Ops
  | SetFunction of UserFunction.T
  | SetType of UserType.T
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | SetExpr of tlid * id * Expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid // CLEANUP move Deletes to API calls instead of Ops
  // TODO: DeleteType? Unclear what to do here. When should something be an Op or not?
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * id * string
  | SetType of UserType.T
  | DeleteType of tlid // CLEANUP move Deletes to API calls instead of Ops

type Oplist = List<Op>

type TLIDOplists = List<tlid * Oplist>

module Secret =
  type T = { name : string; value : string }

module Package =
  // module Type =
  //   type

  module Fn =
    type Parameter = { name : string; typ : DType.T; description : string }

    type T =
      { name : FQFnName.PackageFnName
        body : Expr
        parameters : List<Parameter>
        returnType : DType.T
        description : string
        author : string
        deprecated : bool
        tlid : tlid }
