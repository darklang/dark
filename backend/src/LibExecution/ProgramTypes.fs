/// The types that the user sees
module LibExecution.ProgramTypes

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

/// Used to reference a type defined by a User, Standard Library module, or Package
module FQTypeName =
  type StdlibTypeName = { typ : string }

  /// A type written by a Developer in their canvas
  type UserTypeName = { typ : string; version : int }

  /// if we come up with a better name for this, that'd be great
  /// ideas: Source
  type T =
    // TODO:
    | Stdlib of StdlibTypeName
    // | Package of PackageTypeName
    | User of UserTypeName

  let stdlibTypeNamePat = @"^[A-Z][a-z0-9A-Z_]*$"

  let stdlibTypeName (typ : string) : StdlibTypeName =
    Prelude.assertRe "stdlib type name must match" stdlibTypeNamePat typ

    { typ = typ }


// I'm not sure when we'll need this, but probably soon
// Some of the FQTypeName references might need to be replaced with FQResolvedTypeName ones
//
/// A FQTypeName with any relevant named type arguments "applied"
/// The StdLib-defined type "Option" is an example of a type with type arguments
/// - the Stdlib defines `Option<a>`, whic has a type argument `a`
///   This would look like `{ typ: Option, args: ["a"]}
/// - Map.map is defined as `Map.map<a, b>(f: a -> b, map: Map<a>): Map<b>`
type FQResolvedTypeName = { typ : FQTypeName.T; args : List<string> }


/// A Fully-Qualified Function Name
/// Includes package, module, and version information where relevant.
module FQFnName =

  /// Standard Library Function Name
  type StdlibFnName = { module_ : string; function_ : string; version : int }

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
    if module_ <> "" then
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
  // TODO: do we need typeName here for anything?
  // feels like it could ensure extra type-safety, but also maybe excessive.
  | MPConstructor of id * caseName : string * fieldPats : List<MatchPattern>
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
  | EPipe of id * Expr * Expr * List<Expr>

  | ERecord of id * Option<FQTypeName.T> * List<string * Expr>

  // Constructors include `Just`, `Nothing`, `Error`, `Ok`, as well
  // as user-defined enums.
  // TODO:
  //
  /// Given an Enum type of:
  ///   `type MyEnum = A | B of int | C of int * (label: string) | D of MyEnum`
  /// , this is the expression
  ///   `C (1, "title")`
  /// represented as
  ///   `EConstructor(Some UserType.MyEnum, "C", [EInteger(1), EString("title")]`
  /// TODO: the UserTypeName should eventually be a non-optional FQTypeName.
  | EConstructor of
    id *

    Option<FQTypeName.T> *  // TODO: this shouldn't be an Option

    // we need something (either on this line or as part of the previous)
    // to represent the generic type parameters of the defined type referenced.
    // or do we? maybe not,

    caseName : string *
    fields : List<Expr>

  /// Supports `match` expressions
  /// ```fsharp
  /// match x + 2 with // arg
  /// // cases
  /// | pattern -> expr
  /// | pattern -> expr
  /// | ...
  /// ```
  | EMatch of id * arg : Expr * cases : List<MatchPattern * Expr>

  // Placeholder that indicates the target of the Thread. May be movable at
  // some point
  | EPipeTarget of id

  /// Like an if statement, but with a label
  /// TODO: continue describing
  | EFeatureFlag of
    id *
    flagName : string *
    cond : Expr *
    caseA : Expr *
    caseB : Expr

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr


/// Darklang's available types
/// - `int`
/// - `List<T>`
/// - user-defined enums
/// - etc.
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
  | TBytes
  // A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string // replaces TAny
  | TFn of List<DType> * DType // replaces TLambda

  | TDbList of DType // TODO: cleanup and remove


  /// A type defined by a standard library module, a canvas/user, or a package
  /// e.g. `Result<Int, String>` is represented as `TCustomType("Result", [TInt, TStr])`
  /// `genArgs` is the list of type arguments, if any
  | TCustomType of FQTypeName.T * genArgs : List<DType>

  // TODO: collapse into TCustomType once Stdlib-defined types are supported in FQTypeName
  // and the Option module defines the custom `Option` type
  | TOption of DType

  // TODO: collapse into TCustomType once Stdlib-defined types are supported in FQTypeName
  // and the Result module defines the custom `Result` type
  | TResult of DType * DType

  // TODO: remove in favor of `TCustomType` referring to defined `CustomType.Record`s
  | TRecord of List<string * DType>





/// A type defined by a standard library module, a canvas/user, or a package
module CustomType =
  type RecordField = { id : id; name : string; typ : DType }

  type EnumField = { id : id; typ : DType; label : Option<string> }
  type EnumCase = { id : id; name : string; fields : List<EnumField> }

  type T =
    // TODO: | Abbreviation/Alias of DType
    | Record of firstField : RecordField * additionalFields : List<RecordField>
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>

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
  // TODO: consider flattening this (just type UserType = { ... }, without the module level)
  type Definition = CustomType.T
  type T = { tlid : tlid; name : FQTypeName.UserTypeName; definition : Definition }

module UserFunction =
  type Parameter = { id : id; name : string; typ : DType; description : string }

  type T =
    { tlid : tlid
      name : string
      parameters : List<Parameter>
      returnType : DType
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


/// A built-in standard library type
type BuiltInType =
  { name : FQTypeName.StdlibTypeName
    typeArgs : List<string>
    definition : CustomType.T
    description : string }
