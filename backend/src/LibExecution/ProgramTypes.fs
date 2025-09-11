/// The types that the user sees
module LibExecution.ProgramTypes

open Prelude


type NameValidator<'name> = 'name -> unit
type NamePrinter<'name> = 'name -> string

// Lowercase starting letter for modules and users
let modulePattern = @"^[A-Z][a-z0-9A-Z_]*$"
//let typeNamePattern = @"^[A-Z][a-z0-9A-Z_]*$"
let fnNamePattern = @"^[a-z][a-z0-9A-Z_']*$"
let builtinNamePattern = @"^(__|[a-z])[a-z0-9A-Z_]\w*$"
let valueNamePattern = @"^[a-z][a-z0-9A-Z_']*$"

let assertBuiltin
  (name : string)
  (version : int)
  (nameValidator : string -> unit)
  : unit =
  nameValidator name
  assert_ "version can't be negative" [ "version", version ] (version >= 0)


/// Fully-Qualified Type Name
///
/// Used to reference a type defined in a Package or by a User
module FQTypeName =
  /// The hash of a type in the package manager
  type Package = Hash

  type FQTypeName = Package of Package

  let package (hash : Hash) : Package = hash

  let fqPackage (hash : Hash) : FQTypeName = Package hash



/// A Fully-Qualified Value Name
///
/// Used to reference a value defined by the runtime, in a Package, or by a User
module FQValueName =
  /// A value built into the runtime
  type Builtin = { name : string; version : int }

  /// The hash of a value in the package manager
  type Package = Hash

  type FQValueName =
    | Builtin of Builtin
    | Package of Package


  let assertValueName (name : string) : unit =
    assertRe "Value name must match" valueNamePattern name

  let builtIn (name : string) (version : int) : Builtin =
    assertBuiltin name version assertValueName
    { name = name; version = version }

  let fqBuiltIn (name : string) (version : int) : FQValueName =
    Builtin(builtIn name version)

  let package (hash : Hash) : Package = hash

  let fqPackage (hash : Hash) : FQValueName = Package hash




/// A Fully-Qualified Function Name
///
/// Used to reference a function defined by the runtime, in a Package, or by a User
module FQFnName =
  /// A function built into the runtime
  type Builtin = { name : string; version : int }

  /// The hash of a function in the package manager
  type Package = Hash

  type FQFnName =
    | Builtin of Builtin
    | Package of Package

  let assertFnName (name : string) : unit =
    assertRe $"Fn name must match" fnNamePattern name

  let builtIn (name : string) (version : int) : Builtin =
    assertBuiltin name version assertFnName
    { name = name; version = version }

  let fqBuiltIn (name : string) (version : int) : FQFnName =
    Builtin(builtIn name version)

  let package (hash : Hash) : Package = hash

  let fqPackage (hash : Hash) : FQFnName = Package hash


// In ProgramTypes, names (FnNames, TypeNames, ValueNames) have already been
// resolved. The user wrote them in WrittenTypes, and the WrittenTypesToProgramTypes
// pass looked them up and specified them exactly in ProgramTypes.
//
// However, sometimes the name/fn/type/value could not be found, which means the
// user specified a name that doesn't exist (it shouldn't be for any other reason -
// things like "the internet was down" should error differently).
//
// When there is an error, we still want to keep the rest of the expression around,
// as ProgramTypes's job is to keep the program as it was written by the user. We
// also have a goal of running invalid programs as much as possible. As such, an
// incorrectly specified name shouldn't cause a compile-time/parse-time error, nor
// should it lose information that was specified by the user.
//
// As a result, we model those cases as a Result type, where the Ok case is the
// resolved name, and the Error case models the text name of the type and some error
// information.

type NameResolutionError =
  | NotFound of List<string>
  | InvalidName of List<string>

type NameResolution<'a> = Result<'a, NameResolutionError>


type LetPattern =
  /// `let x = 1`
  | LPVariable of id * name : string

  // /// `let _ignored = 1`
  // | LPIgnored

  // /// let (x) = 1
  //| LPParens of inner : LetPattern

  /// `let (x, _) = (1, 2)`
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

  /// `let () = ()`
  | LPUnit of id

module LetPattern =
  let rec symbolsUsed (pattern : LetPattern) : Set<string> =
    match pattern with
    | LPVariable(_, name) -> Set.singleton name
    | LPTuple(_, first, second, rest) ->
      Set.unionMany
        [ symbolsUsed first
          symbolsUsed second
          rest |> List.map symbolsUsed |> Set.unionMany ]
    | LPUnit _ -> Set.empty

  let toID (pattern : LetPattern) : id =
    match pattern with
    | LPVariable(id, _)
    | LPTuple(id, _, _, _)
    | LPUnit id -> id


/// Used for pattern matching in a match statement
type MatchPattern =
  | MPUnit of id

  | MPBool of id * bool

  | MPInt8 of id * int8
  | MPUInt8 of id * uint8
  | MPInt16 of id * int16
  | MPUInt16 of id * uint16
  | MPInt32 of id * int32
  | MPUInt32 of id * uint32
  | MPInt64 of id * int64
  | MPUInt64 of id * uint64
  | MPInt128 of id * System.Int128
  | MPUInt128 of id * System.UInt128

  | MPFloat of id * Sign * string * string

  | MPChar of id * string
  | MPString of id * string

  | MPList of id * List<MatchPattern>
  | MPListCons of id * head : MatchPattern * tail : MatchPattern
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

  | MPEnum of id * caseName : string * fieldPats : List<MatchPattern>

  | MPVariable of id * string

  | MPOr of id * NEList<MatchPattern>

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
/// - `Int64`
/// - `List<T>`
/// - user-defined enums
/// - etc.
type TypeReference =
  | TUnit

  | TBool

  | TInt8
  | TUInt8
  | TInt16
  | TUInt16
  | TInt32
  | TUInt32
  | TInt64
  | TUInt64
  | TInt128
  | TUInt128

  | TFloat

  | TChar
  | TString

  | TUuid
  | TDateTime

  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TDict of TypeReference

  /// A type defined by a standard library module, a canvas/user, or a package
  /// e.g. `Result<Int64, String>` is represented as `TCustomType("Result", [TInt64, TString])`
  /// `typeArgs` is the list of type arguments, if any
  | TCustomType of
    // TODO: this reference should be by-hash
    NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference>

  | TFn of arguments : NEList<TypeReference> * ret : TypeReference

  /// A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string

  | TDB of TypeReference


/// Expressions - the main part of the language.
type Expr =
  // -- Simple exprs --
  | EUnit of id
  | EBool of id * bool

  | EInt8 of id * int8
  | EUInt8 of id * uint8
  | EInt16 of id * int16
  | EUInt16 of id * uint16
  | EInt32 of id * int32
  | EUInt32 of id * uint32
  | EInt64 of id * int64
  | EUInt64 of id * uint64
  | EInt128 of id * System.Int128
  | EUInt128 of id * System.UInt128

  // Allow the user to have arbitrarily big numbers, even if they don't make sense as
  // floats. The float is split as we want to preserve what the user entered.
  // Strings are used as numbers lose the leading zeros (eg 7.00007)
  | EFloat of id * Sign * whole : string * part : string

  /// A character is an Extended Grapheme Cluster (hence why we use a string). This
  /// is equivalent to one screen-visible "character" in Unicode.
  | EChar of id * string
  | EString of id * List<StringSegment>


  // -- Flow control --
  /// `if cond then thenExpr else elseExpr`
  | EIf of id * cond : Expr * thenExpr : Expr * elseExpr : Option<Expr>

  /// `(1 + 2) |> fnName |> (+) 3`
  | EPipe of id * lhs : Expr * parts : List<PipeExpr>

  /// Supports `match` expressions
  /// ```fsharp
  /// match x + 2 with // arg
  /// | pattern -> expr // cases[0]
  /// | pattern -> expr
  /// | ...
  /// ```
  // cases is a list to represent when a user starts typing but doesn't complete it
  | EMatch of id * arg : Expr * cases : List<MatchCase>

  // <summary>
  // Composed of binding pattern, the expression to create bindings for,
  // and the expression that follows, where the bound values are available
  // </summary>
  //
  // <code>
  // let str = expr1
  // expr2
  // </code>
  | ELet of id * LetPattern * Expr * Expr

  // Reference some local variable by name
  //
  // i.e. after a `let binding = value`, any use of `binding`
  | EVariable of id * string


  // -- Basic structures --
  | EList of id * List<Expr>
  | EDict of id * List<string * Expr>
  | ETuple of id * Expr * Expr * List<Expr>

  // -- "Applying" args to things, such as fns and lambdas --
  /// This is a function call, the first expression is the value of the function.
  /// - `expr (args[0])`
  /// - `expr (args[0]) (args[1])`
  /// - `expr<typeArg[0]> (args[0])`
  | EApply of id * expr : Expr * typeArgs : List<TypeReference> * args : NEList<Expr>

  /// Reference a function name, _usually_ so we can _apply_ it with args
  | EFnName of id * NameResolution<FQFnName.FQFnName>

  // Composed of a parameters * the expression itself
  // The id in the varname list is the analysis id, used to get a livevalue
  // from the analysis engine
  | ELambda of id * pats : NEList<LetPattern> * body : Expr

  /// Calls upon an infix function
  | EInfix of id * Infix * lhs : Expr * rhs : Expr


  // -- References to custom types and data --

  /// Construct a record
  /// `SomeRecord { field1: value; field2: value }`
  | ERecord of
    id *
    // TODO: this reference should be by-hash
    typeName : NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference> *
    // User is allowed type `Name {}` even if that's an error
    fields : List<string * Expr>

  /// Access a field of some record (e.g. `someExpr.fieldName`)
  | ERecordFieldAccess of id * record : Expr * fieldName : string

  /// Clone a record, and update some of its values
  /// `{ r with key = value }`
  | ERecordUpdate of id * record : Expr * updates : NEList<string * Expr>


  // Enums include `Some`, `None`, `Error`, `Ok`, as well
  // as user-defined enums.
  //
  /// Given an Enum type of:
  ///   `type MyEnum = A | B of int | C of int * (label: string) | D of MyEnum`
  /// , this is the expression
  ///   `C (1, "title")`
  /// represented as
  ///   `EEnum(Some UserType.MyEnum, "C", [EInt64(1), EString("title")]`
  | EEnum of
    id *
    // TODO: this reference should be by-hash
    typeName : NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference> *
    caseName : string *
    fields : List<Expr>

  | EValue of
    id *
    // TODO: this reference should be by-hash
    NameResolution<FQValueName.FQValueName>

  | EStatement of id * first : Expr * next : Expr

and MatchCase = { pat : MatchPattern; whenCondition : Option<Expr>; rhs : Expr }

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and PipeExpr =
  /// `1 |> fun x -> x + 1`
  | EPipeLambda of id * pats : NEList<LetPattern> * body : Expr

  /// `1 |> (+) 1`
  | EPipeInfix of id * Infix * Expr

  /// `1 |> Json.serialize<Int64>`
  | EPipeFnCall of
    id *
    NameResolution<FQFnName.FQFnName> *
    typeArgs : List<TypeReference> *
    args : List<Expr>

  /// `1 |> Option.Some`
  | EPipeEnum of
    id *
    // TODO: this reference should be by-hash
    typeName : NameResolution<FQTypeName.FQTypeName> *
    caseName : string *
    fields : List<Expr>

  /// ```fsharp
  /// let myLambda = fun x -> x + 1
  /// 1 |> myLambda
  /// ```
  | EPipeVariable of id * varContainingPipeable : string * args : List<Expr>


module Expr =
  let toID (expr : Expr) : id =
    match expr with
    | EUnit id
    | EBool(id, _)
    | EInt8(id, _)
    | EUInt8(id, _)
    | EInt16(id, _)
    | EUInt16(id, _)
    | EInt32(id, _)
    | EUInt32(id, _)
    | EInt64(id, _)
    | EUInt64(id, _)
    | EInt128(id, _)
    | EUInt128(id, _)
    | EChar(id, _)
    | EString(id, _)
    | EFloat(id, _, _, _)
    | EValue(id, _)
    | ELet(id, _, _, _)
    | EIf(id, _, _, _)
    | EInfix(id, _, _, _)
    | ELambda(id, _, _)
    | EFnName(id, _)
    | EVariable(id, _)
    | EApply(id, _, _, _)
    | EList(id, _)
    | EDict(id, _)
    | ETuple(id, _, _, _)
    | EPipe(id, _, _)
    | ERecord(id, _, _, _)
    | ERecordUpdate(id, _, _)
    | ERecordFieldAccess(id, _, _)
    | EEnum(id, _, _, _, _)
    | EMatch(id, _, _)
    | EStatement(id, _, _) -> id



/// A type defined by a package or canvas/user
module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference; description : string }

  type EnumField =
    { typ : TypeReference; label : Option<string>; description : string }

  type EnumCase = { name : string; fields : List<EnumField>; description : string }

  /// The right-hand-side of the declaration: eg List<'a>
  type Definition =
    /// `type MyAlias = Int64`
    | Alias of TypeReference

    /// `type MyRecord = { a : int; b : string }`
    | Record of NEList<RecordField>

    /// `type MyEnum = A | B of int | C of int * (label: string)`
    | Enum of NEList<EnumCase>

  /// Combined the RHS definition, with the list of type parameters. Eg type
  /// MyType<'a> = List<'a>
  type T = { typeParams : List<string>; definition : Definition }



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


// --
// Package things
// --

// TODO: bring these back in some form.
// let assertPackage
//   (modules : List<string>)
//   (name : string)
//   (nameValidator : string -> unit)
//   : unit =
//   List.iter (assertRe "modules name must match" modulePattern) modules
//   nameValidator name


// let packageName (owner : string) (modules : List<string>) (name : string) : string =
//   let nameParts =
//     match owner with
//     | "Tests" -> modules @ [ name ]
//   nameParts |> String.concat "."


module PackageType =
  type Name = { owner : string; modules : List<string>; name : string }

  let name (owner : string) (modules : List<string>) (name : string) : Name =
    // TODO: assert OK
    { owner = owner; modules = modules; name = name }

  type PackageType =
    { hash : Hash
      name : Name
      declaration : TypeDeclaration.T
      description : string
      deprecated : Deprecation<FQTypeName.FQTypeName> }

module PackageValue =
  type Name = { owner : string; modules : List<string>; name : string }

  let name (owner : string) (modules : List<string>) (name : string) : Name =
    // TODO: assert OK
    { owner = owner; modules = modules; name = name }

  type PackageValue =
    { hash : Hash
      name : Name
      description : string
      deprecated : Deprecation<FQValueName.FQValueName>
      body : Expr }

module PackageFn =
  type Name = { owner : string; modules : List<string>; name : string }

  let name (owner : string) (modules : List<string>) (name : string) : Name =
    // TODO: assert OK
    { owner = owner; modules = modules; name = name }

  type Parameter = { name : string; typ : TypeReference; description : string }

  type PackageFn =
    { hash : Hash
      name : Name
      body : Expr
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string
      deprecated : Deprecation<FQFnName.FQFnName> }

type Packages =
  { types : List<PackageType.PackageType>
    values : List<PackageValue.PackageValue>
    fns : List<PackageFn.PackageFn> }

  static member combine(packages : List<Packages>) : Packages =
    { types = packages |> List.collect _.types
      values = packages |> List.collect _.values
      fns = packages |> List.collect _.fns }

module Search =
  /// The type of entity to search for
  type EntityType =
    | Type
    | Module
    | Fn
    | Value

  /// How deep to search in the module hierarchy
  type SearchDepth = | OnlyDirectDescendants
  // TODO: support this. | AllDescendants

  /// Query parameters for searching packages
  type SearchQuery =
    {
      /// i.e. "Darklang.Stdlib"
      currentModule : List<string>

      /// i.e. "List" or "map"
      text : string

      searchDepth : SearchDepth

      /// empty list implies 'any'
      entityTypes : List<EntityType>

      /// if true, require exact matches for names and modules instead of fuzzy matching
      exactMatch : bool
    }

  /// Results from a package search
  type SearchResults =
    { submodules : List<List<string>> // [ [ "List"]; ["String"; "List"] ]
      types : List<PackageType.PackageType>
      values : List<PackageValue.PackageValue>
      fns : List<PackageFn.PackageFn> }

/// Functionality written in Dark stored and managed outside of user space
///
/// Note: It may be tempting to think the `getX` fns shouldn't return Options,
/// but there's a chance of Local <-> Cloud not being fully in sync,
/// for whatever reasons.
type PackageManager =
  { findType : PackageType.Name -> Ply<Option<FQTypeName.Package>>
    findValue : PackageValue.Name -> Ply<Option<FQValueName.Package>>
    findFn : PackageFn.Name -> Ply<Option<FQFnName.Package>>

    getType : FQTypeName.Package -> Ply<Option<PackageType.PackageType>>
    getValue : FQValueName.Package -> Ply<Option<PackageValue.PackageValue>>
    getFn : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>>

    search : Search.SearchQuery -> Ply<Search.SearchResults>

    init : Ply<unit> }

  static member empty =
    { findType = fun _ -> Ply None
      findValue = fun _ -> Ply None
      findFn = fun _ -> Ply None

      getType = fun _ -> Ply None
      getValue = fun _ -> Ply None
      getFn = fun _ -> Ply None

      search =
        fun _ ->
          uply { return { submodules = []; types = []; values = []; fns = [] } }

      init = uply { return () } }


  /// Allows you to side-load a few 'extras' in-memory, along
  /// the normal fetching functionality. (Mostly helpful for tests)
  static member withExtras
    (types : List<PackageType.PackageType>)
    (values : List<PackageValue.PackageValue>)
    (fns : List<PackageFn.PackageFn>)
    (pm : PackageManager)
    : PackageManager =
    { findType =
        fun name ->
          match types |> List.tryFind (fun t -> t.name = name) with
          | Some t -> Some t.hash |> Ply
          | None -> pm.findType name
      findValue =
        fun name ->
          match values |> List.tryFind (fun v -> v.name = name) with
          | Some v -> Some v.hash |> Ply
          | None -> pm.findValue name
      findFn =
        fun name ->
          match fns |> List.tryFind (fun f -> f.name = name) with
          | Some f -> Some f.hash |> Ply
          | None -> pm.findFn name

      getType =
        fun hash ->
          match types |> List.tryFind (fun t -> t.hash = hash) with
          | Some t -> Ply(Some t)
          | None -> pm.getType hash
      getValue =
        fun hash ->
          match values |> List.tryFind (fun v -> v.hash = hash) with
          | Some v -> Ply(Some v)
          | None -> pm.getValue hash
      getFn =
        fun hash ->
          match fns |> List.tryFind (fun f -> f.hash = hash) with
          | Some f -> Ply(Some f)
          | None -> pm.getFn hash

      search = fun query -> pm.search query

      init = pm.init }



// --
// User things
// --
module DB =
  type T = { tlid : tlid; name : string; version : int; typ : TypeReference }

module Secret =
  type T = { name : string; value : string; version : int }

module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  /// User to represent handlers in their lowest-level form: a triple of space * name * modifier
  /// "Space" is "HTTP", "WORKER", "REPL", etc.
  ///
  /// "Modifier" options differ based on space.
  /// e.g. HTTP handler may have "GET" modifier.
  ///
  /// Handlers which don't have modifiers (e.g. repl, worker) nearly
  /// always (but not actually always) have `_` as their modifier.
  type HandlerDesc = (string * string * string)

  type Spec =
    | HTTP of route : string * method : string
    | Worker of name : string
    | Cron of name : string * interval : CronInterval
    | REPL of name : string

  type T = { tlid : tlid; ast : Expr; spec : Spec }

module Toplevel =
  type T =
    | TLDB of DB.T
    | TLHandler of Handler.T

  let toTLID (tl : T) : tlid =
    match tl with
    | TLDB db -> db.tlid
    | TLHandler h -> h.tlid
