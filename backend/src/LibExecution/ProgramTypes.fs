/// The types that the user sees
module LibExecution.ProgramTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

type Position = { x : int; y : int }

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

  // We don't include InfixStdlibFnName here as that is used directly by EBinOp
  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

/// Used for pattern matching in a match statement
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

/// Whether a function's result is unwrapped automatically (and, in the case of
/// Error/Nothing, sent to the error rail). NoRail functions are not unwrapped.
type SendToRail =
  | Rail
  | NoRail

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
  | ENull of id
  | EBlank of id
  | ELet of id * string * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EBinOp of id * FQFnName.InfixStdlibFnName * Expr * Expr * SendToRail
  // the id in the varname list is the analysis id, used to get a livevalue
  // from the analysis engine
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * List<Expr> * SendToRail
  // An EPartial holds the intermediate state of user-input when changing from
  // one expression to another. The [string] is the exact text that has been
  // entered and the [t] is the old expression that is being changed.
  //
  // Examples:
  // - When filling in an EBlank by typing `Str` an EPartial (id, "Str", EBlank (...)) is used.
  // - When changing the EFnCall of "String::append" by deleting a character
  //   from the end, an EPartial (id, "String::appen", EFnCall _) would
  //   be created.
  //
  // EPartial is usually rendered as just the string part, but sometimes when
  // wrapping certain kinds of expressions will be rendered in unique ways.
  // Eg, an EPartial wrapping an EFnCall will render the arguments of the old
  // EFnCall expression after the string. See FluidPrinter for specifics.
  | EPartial of id * string * Expr
  // An ERightPartial is used while in the process of adding an EBinOp,
  // allowing for typing multiple characters as operators (eg, "++") after an
  // expression. The [string] holds the typed characters while the [t] holds
  // the LHS of the binop.
  //
  // Example:
  // Typing `"foo" ++` creates ERightPartial (id, "++", EString (_, "foo"))
  // until the autocomplete of "++" is accepted, transforming the ERightPartial
  // into a proper EBinOp.
  //
  // ERightPartial is rendered as the old expression followed by the string.
  | ERightPartial of id * string * Expr
  // ELeftPartial allows typing to prepend a construct to an existing
  // expression. The [string] holds the typed text, while the [t] holds the
  // existing expression to the right.
  //
  // Example:
  // On an existing line with `String::append "a" "b"` (a EFnCall), typing `if` at the beginning of the line
  // will create a ELeftPartial (id, "if", EFnCall _). Accepting autocomplete
  // of `if` would wrap the EFnCall into an EIf.
  //
  // ELeftPartial is rendered as the string followed by the normal rendering of the old expression.
  | ELeftPartial of id * string * Expr
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | ERecord of id * List<string * Expr>
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
  // Short-circuiting operators
  | EAnd of id * Expr * Expr
  | EOr of id * Expr * Expr

type DType =
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TList of DType
  | TTuple of DType * DType * List<DType>
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
    | HTTPBasic of route : string * method : string * ids : ids
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

  type T = { tlid : tlid; pos : Position; ast : Expr; spec : Spec }


module DB =
  type Col = { name : Option<string>; typ : Option<DType>; nameID : id; typeID : id }

  type T =
    { tlid : tlid
      pos : Position
      name : string
      nameID : id
      version : int
      cols : List<Col> }

module UserType =
  type RecordField = { name : string; typ : Option<DType>; nameID : id; typeID : id }
  type Definition = Record of List<RecordField>

  type T =
    { tlid : tlid
      name : string
      nameID : id
      version : int
      definition : Definition }

module UserFunction =
  type Parameter =
    { name : string
      nameID : id
      typ : Option<DType>
      typeID : id
      description : string }

  type T =
    { tlid : tlid
      name : string
      nameID : id
      parameters : List<Parameter>
      returnType : DType
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
  | SetType of UserType.T
  | DeleteType of tlid // CLEANUP move Deletes to API calls instead of Ops

type Oplist = List<Op>

type TLIDOplists = List<tlid * Oplist>

module Secret =
  type T = { name : string; value : string }

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
