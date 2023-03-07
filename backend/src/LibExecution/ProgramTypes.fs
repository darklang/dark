/// The types that the user sees
module LibExecution.ProgramTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

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

  let oneWordFunctions =
    Set [ "equals"; "notEquals"; "equals_v0"; "notEquals_v0"; "emit_v1" ]

  let infixFnNames =
    Set [ "+"; "-"; "*"; ">"; ">="; "<="; "<"; "^"; "%"; "/"; "++"; "=="; "!=" ]

  // CLEANUP Packages should just have a uuid
  let namePat = @"^[a-z][a-z0-9_]*$"
  let modNamePat = @"^[A-Z][a-z0-9A-Z_]*$"
  let fnnamePat = @"^([a-z][a-z0-9A-Z_]*)$"
  let userFnNamePat = @"^([a-z][a-z0-9A-Z_]*)$"

  let packageFnName
    (owner : string)
    (package : string)
    (module_ : string)
    (function_ : string)
    (version : int)
    : PackageFnName =
    Prelude.assertRe "owner must match" namePat owner
    Prelude.assertRe "package must match" namePat package
    Prelude.assertRe "modName name must match" modNamePat module_
    Prelude.assertRe "package function name must match" fnnamePat function_
    Prelude.assert_ "version can't be negative" [ "version", version ] (version >= 0)
    { owner = owner
      package = package
      module_ = module_
      function_ = function_
      version = version }

  let packageFqName
    (owner : string)
    (package : string)
    (module_ : string)
    (function_ : string)
    (version : int)
    : T =
    Package(packageFnName owner package module_ function_ version)

  let userFnName (fnName : string) : UserFnName =
    Prelude.assertRe "user function name must match" userFnNamePat fnName
    fnName


  let userFqName (fnName : string) = User(userFnName fnName)

  let stdlibFnName
    (module_ : string)
    (function_ : string)
    (version : int)
    : StdlibFnName =
    if module_ = "" then
      Prelude.assert_
        "stdlib function name must match"
        [ "function", function_ ]
        (Set.contains function_ oneWordFunctions
         || Set.contains function_ infixFnNames)
    else
      Prelude.assertRe "modName name must match" modNamePat module_
      Prelude.assertRe "stdlib function name must match" fnnamePat function_
    Prelude.assert_
      "version can't be negative"
      [ "function", function_; "version", version ]
      (version >= 0)
    { module_ = module_; function_ = function_; version = version }

  let stdlibFqName (module_ : string) (function_ : string) (version : int) : T =
    Stdlib(stdlibFnName module_ function_ version)

type LetPattern = LPVariable of id * name : string

/// Used for pattern matching in a match statement
type MatchPattern =
  | MPVariable of id * string
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
  | EString of id * List<StringSegment>
  /// A character is an Extended Grapheme Cluster (hence why we use a string). This
  /// is equivalent to one screen-visible "character" in Unicode.
  | ECharacter of id * string
  // Allow the user to have arbitrarily big numbers, even if they don't make sense as
  // floats. The float is split as we want to preserve what the user entered.
  // Strings are used as numbers lose the leading zeros (eg 7.00007)
  | EFloat of id * Sign * string * string
  | EUnit of id
  | ELet of id * LetPattern * Expr * Expr
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

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

type DType =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TStr
  | TList of DType
  | TTuple of DType * DType * List<DType>
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
    | Worker of name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids

  type T = { tlid : tlid; ast : Expr; spec : Spec }


module DB =
  type Col = { name : Option<string>; typ : Option<DType>; nameID : id; typeID : id }

  type T =
    { tlid : tlid
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
      typ : DType
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
