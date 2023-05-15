/// The types that the user sees
module LibExecution.ProgramTypes

open Prelude

/// Used to reference a type defined by a User, Standard Library module, or Package
module FQTypeName =

  /// A type written in F# and shipped in the executable. Module required for all but a few cases.
  type StdlibTypeName = { modules : List<string>; typ : string; version : int }

  /// A type written by a Developer in their canvas. Module not required
  type UserTypeName = { modules : List<string>; typ : string; version : int }

  /// The name of a type in the package manager
  type PackageTypeName =
    { owner : string
      modules : NonEmptyList<string>
      typ : string
      version : int }

  type T =
    | Stdlib of StdlibTypeName
    | User of UserTypeName
    | Package of PackageTypeName

  let toString (fqtn : T) : string =
    match fqtn with
    | Stdlib s ->
      RuntimeTypes.FQTypeName.toString (
        RuntimeTypes.FQTypeName.Stdlib
          { modules = s.modules; typ = s.typ; version = s.version }
      )
    | User u ->
      RuntimeTypes.FQTypeName.toString (
        RuntimeTypes.FQTypeName.User
          { modules = u.modules; typ = u.typ; version = u.version }
      )
    | Package p ->
      RuntimeTypes.FQTypeName.toString (
        RuntimeTypes.FQTypeName.Package
          { owner = p.owner; modules = p.modules; typ = p.typ; version = p.version }
      )

  let modNamePat = @"^[A-Z][a-z0-9A-Z_]*$"
  let typeNamePat = @"^[A-Z][a-z0-9A-Z_]*$"


  let stdlibTypeName
    (modul : string)
    (typ : string)
    (version : int)
    : StdlibTypeName =
    if modul <> "" then assertRe "modName name must match" modNamePat modul
    assertRe "stdlib type name must match" typeNamePat typ
    assert_ "version can't be negative" [ "version", version ] (version >= 0)
    { modules = [ modul ]; typ = typ; version = version }

  let userTypeName (modul : string) (typ : string) (version : int) : UserTypeName =
    if modul <> "" then assertRe "modName name must match" modNamePat modul
    assertRe "stdlib type name must match" typeNamePat typ
    assert_ "version can't be negative" [ "version", version ] (version >= 0)
    { modules = [ modul ]; typ = typ; version = version }




/// A Fully-Qualified Function Name
/// Includes package, module, and version information where relevant.
module FQFnName =

  /// Standard Library Function Name
  type StdlibFnName = { modules : List<string>; function_ : string; version : int }

  /// A UserFunction is a function written by a Developer in their canvas
  type UserFnName = { modules : List<string>; function_ : string; version : int }

  /// The name of a function in the package manager
  type PackageFnName =
    { owner : string
      modules : NonEmptyList<string>
      function_ : string
      version : int }

  // We don't include InfixStdlibFnName here as that is used directly by EInfix
  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

  let oneWordFunctions =
    Set [ "equals"
          "notEquals"
          "equals_v0"
          "notEquals_v0"
          "emit_v1"
          "print"
          "print_v0" ]

  // CLEANUP Packages should just have a uuid
  let namePat = @"^[a-z][a-z0-9_]*$"
  let modNamePat = @"^[A-Z][a-z0-9A-Z_]*$"
  let fnnamePat = @"^([a-z][a-z0-9A-Z_]*)$"
  let userFnNamePat = @"^([a-z][a-z0-9A-Z_]*[']?)$"

  let packageFnName
    (owner : string)
    (modules : NonEmptyList<string>)
    (function_ : string)
    (version : int)
    : PackageFnName =
    assertRe "owner must match" namePat owner
    NonEmptyList.iter (assertRe "modName name must match" modNamePat) modules
    assertRe "package function name must match" fnnamePat function_
    assert_ "version can't be negative" [ "version", version ] (version >= 0)
    { owner = owner; modules = modules; function_ = function_; version = version }

  let packageFqName
    (owner : string)
    (modules : NonEmptyList<string>)
    (function_ : string)
    (version : int)
    : T =
    Package(packageFnName owner modules function_ version)

  let stdlibFnName
    (modules : List<string>)
    (function_ : string)
    (version : int)
    : StdlibFnName =
    List.iter (assertRe "modName name must match" modNamePat) modules
    assertRe "stdlib function name must match" fnnamePat function_
    assert_
      "version can't be negative"
      [ "function", function_; "version", version ]
      (version >= 0)
    { modules = modules; function_ = function_; version = version }

  let stdlibFqName
    (modules : List<string>)
    (function_ : string)
    (version : int)
    : T =
    Stdlib(stdlibFnName modules function_ version)

  let userFnName
    (modules : List<string>)
    (function_ : string)
    (version : int)
    : UserFnName =
    List.iter (assertRe "modName name must match" modNamePat) modules
    assertRe "user function name must match" userFnNamePat function_
    assert_
      "version can't be negative"
      [ "function", function_; "version", version ]
      (version >= 0)
    { modules = modules; function_ = function_; version = version }

  let userFqName (modules : List<string>) (function_ : string) (version : int) : T =
    User(userFnName modules function_ version)


type LetPattern =
  | LPVariable of id * name : string
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

/// Used for pattern matching in a match statement
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
  | MPListCons of id * heads : List<MatchPattern> * tail : MatchPattern

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

/// Darklang's available types
/// - `int`
/// - `List<T>`
/// - user-defined enums
/// - etc.
type TypeReference =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TString
  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TDict of TypeReference
  | THttpResponse of TypeReference
  | TDB of TypeReference
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TBytes
  // A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string // replaces TAny
  | TFn of List<TypeReference> * TypeReference // replaces TLambda

  /// A type defined by a standard library module, a canvas/user, or a package
  /// e.g. `Result<Int, String>` is represented as `TCustomType("Result", [TInt, TString])`
  /// `typeArgs` is the list of type arguments, if any
  | TCustomType of FQTypeName.T * typeArgs : List<TypeReference>

  // TODO: collapse into TCustomType once Stdlib-defined types are supported in FQTypeName
  // and the Option module defines the custom `Option` type
  | TOption of TypeReference

  // TODO: collapse into TCustomType once Stdlib-defined types are supported in FQTypeName
  // and the Result module defines the custom `Result` type
  | TResult of TypeReference * TypeReference


/// Expressions - the main part of the language.
type Expr =
  | EInt of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  /// A character is an Extended Grapheme Cluster (hence why we use a string). This
  /// is equivalent to one screen-visible "character" in Unicode.
  | EChar of id * string
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
  | EFnCall of id * FQFnName.T * typeArgs : List<TypeReference> * args : List<Expr>
  | EList of id * List<Expr>
  | EDict of id * List<string * Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | EPipe of id * Expr * PipeExpr * List<PipeExpr>

  | ERecord of id * FQTypeName.T * List<string * Expr>

  // Enums include `Just`, `Nothing`, `Error`, `Ok`, as well
  // as user-defined enums.
  //
  /// Given an Enum type of:
  ///   `type MyEnum = A | B of int | C of int * (label: string) | D of MyEnum`
  /// , this is the expression
  ///   `C (1, "title")`
  /// represented as
  ///   `EEnum(Some UserType.MyEnum, "C", [EInt(1), EString("title")]`
  /// TODO: the UserTypeName should eventually be a non-optional FQTypeName.
  | EEnum of id * typeName : FQTypeName.T * caseName : string * fields : List<Expr>

  /// Supports `match` expressions
  /// ```fsharp
  /// match x + 2 with // arg
  /// // cases
  /// | pattern -> expr
  /// | pattern -> expr
  /// | ...
  /// ```
  | EMatch of id * arg : Expr * cases : List<MatchPattern * Expr>

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and PipeExpr =
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
// Used to mark whether a function/type has been deprecated, and if so,
// details about possible replacements/alternatives, and reasoning
type Deprecation<'name> =
  | NotDeprecated

  // The exact same thing is available under a new, preferred name
  | RenamedTo of 'name

  /// This has been deprecated and has a replacement we can suggest
  | ReplacedBy of 'name

  /// This has been deprecated and not replaced, provide a message for the user
  | DeprecatedBecause of string


/// A type defined by a standard library module, a canvas/user, or a package
module CustomType =
  type RecordField = { name : string; typ : TypeReference; description : string }

  type EnumField =
    { typ : TypeReference
      label : Option<string>
      description : string }

  type EnumCase = { name : string; fields : List<EnumField>; description : string }

  type T =
    // TODO: //| Abbreviation/Alias of TypeReference

    /// `type MyRecord = { a : int; b : string }`
    | Record of firstField : RecordField * additionalFields : List<RecordField>

    /// `type MyEnum = A | B of int | C of int * (label: string)`
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>

module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  type Spec =
    | HTTP of route : string * method : string
    | Worker of name : string
    | Cron of name : string * interval : CronInterval
    | REPL of name : string

  type T = { tlid : tlid; ast : Expr; spec : Spec }


module DB =
  type T = { tlid : tlid; name : string; version : int; typ : TypeReference }

module UserType =
  // CLEANUP: needs type arguments
  type T = { tlid : tlid; name : FQTypeName.UserTypeName; definition : CustomType.T }

module UserFunction =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { tlid : tlid
      name : FQFnName.UserFnName
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      deprecated : Deprecation<FQFnName.T>
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

type Oplist = List<Op>

type TLIDOplists = List<tlid * Oplist>

module Secret =
  type T = { name : string; value : string; version : int }

module Package =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type Fn =
    { tlid : tlid
      id : System.Guid
      name : FQFnName.PackageFnName
      body : Expr
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
      deprecated : Deprecation<FQFnName.T> }
