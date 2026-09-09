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



// TODO: consider grouping SCM types (BranchId, Branch, MergeError, Commit) into a
// SourceControl module to match the Dark package structure (Darklang.SCM.*)
/// Structural hash of a package item's content (shape, not name/location).
type Hash =
  | Hash of string
  // Explicit ToString — F# unions' default override goes through
  // StructuredPrintfImpl reflection, which is broken under AOT trimming.
  override this.ToString() = let (Hash s) = this in s

module Hash =
  let empty : Hash = Hash ""
  let toHexString (Hash h) : string = h

  /// The hash of a piece of PROSE, which is not content and so is not hashed by `Canonical`.
  ///
  /// `UpdateDoc.previous` names the text it replaced, and naming it needs a way to say which text
  /// without carrying it. Same construction as an item hash (SHA-256, lowercase hex) so that the two
  /// are indistinguishable in a column, and deliberately NOT the same function: an item hash is over
  /// a canonical serialization and this is over the bytes of a string.
  let ofText (text : string) : Hash =
    text
    |> System.Text.Encoding.UTF8.GetBytes
    |> System.Security.Cryptography.SHA256.HashData
    |> System.Convert.ToHexString
    |> fun s -> Hash(s.ToLowerInvariant())

// Branch identity lives in `Branching` because `RuntimeTypes` needs it too and compiles first. This
// abbreviation is what lets everything keep saying `PT.BranchId`.
type BranchId = Branching.BranchId


/// Fully-Qualified Type Name
///
/// Used to reference a type defined in a Package or by a User
module FQTypeName =
  type Package = Hash

  type FQTypeName = Package of Package

  let package (h : string) : Package = Hash h

  let fqPackage (h : string) : FQTypeName = Package(Hash h)



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

  let package (h : string) : Package = Hash h

  let fqPackage (h : string) : FQValueName = Package(Hash h)




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

  let package (h : string) : Package = Hash h

  let fqPackage (h : string) : FQFnName = Package(Hash h)


type PackageLocation =
  // CLEANUP this doesn't really account for when you're referring to a root 'owner'
  { owner : string
    modules : List<string>
    name : string }


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
  | NotFound
  | InvalidName

/// A successfully resolved name and (where applicable) the package
/// location that resolved it.
///
/// `location` is the matched fully-qualified location after `namesToTry`
///   expansion — `Some` for resolved package items, `None` for builtins
///   (and for resolved package items where no location was captured).
///   Carrying it alongside the resolved hash lets downstream consumers
///   skip a post-hoc lookup: dep-edge inserts, propagation rewrites
///   (AstTransformer's byLocation substitution), SCC hash substitution
///   (Canonical), and deferred refresh after a package moves.
type ResolvedName<'a> = { name : 'a; location : Option<PackageLocation> }

/// `originalName` is the user-typed name (a list of qualifiers).
/// `resolved` is the resolved name (or the resolution error). The Ok
///   payload bundles the resolved name with its package location so
///   "location without resolution" is unrepresentable.
type NameResolution<'a> =
  { originalName : List<string>
    resolved : Result<ResolvedName<'a>, NameResolutionError> }

module NameResolution =
  let ok (value : 'a) : NameResolution<'a> =
    { originalName = []; resolved = Ok { name = value; location = None } }


type LetPattern =
  /// `let x = 1`
  | LPVariable of id * name : string

  /// `let _ = 1`
  | LPWildcard of id

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
    | LPWildcard _ -> Set.empty
    | LPTuple(_, first, second, rest) ->
      Set.unionMany
        [ symbolsUsed first
          symbolsUsed second
          rest |> List.map symbolsUsed |> Set.unionMany ]
    | LPUnit _ -> Set.empty

  let toID (pattern : LetPattern) : id =
    match pattern with
    | LPVariable(id, _)
    | LPWildcard id
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
  | MPInt of id * bigint

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
  | TInt

  | TFloat

  | TChar
  | TString

  | TUuid
  | TDateTime

  /// Immutable byte sequence. At runtime, represented as a reference
  /// to bytes held either in-process (ephemeral) or in the
  /// content-addressed `package_blobs` table (persistent).
  | TBlob

  /// Lazy, single-consumer, non-persistable sequence of values of
  /// the given element type.
  | TStream of TypeReference

  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TDict of key : TypeReference * value : TypeReference

  /// A type defined by a standard library module or a package
  /// e.g. `Result<Int64, String>` is represented as `TCustomType("Result", [TInt64, TString])`
  /// `typeArgs` is the list of type arguments, if any
  | TCustomType of
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
  | EInt of id * bigint

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

  // Reference a function argument by its position index
  //
  // i.e. in `let fn (x: Int64) (y: String) = ...`,
  // references to `x` become `EArg(id, 0)` and `y` becomes `EArg(id, 1)`
  | EArg of id * index : int


  // -- Basic structures --
  | EList of id * List<Expr>
  | EDict of id * List<Expr * Expr>
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
    typeName : NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference> *
    caseName : string *
    fields : List<Expr>

  | EValue of id * NameResolution<FQValueName.FQValueName>

  | EStatement of id * first : Expr * next : Expr

  | ESelf of id

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
    typeName : NameResolution<FQTypeName.FQTypeName> *
    caseName : string *
    fields : List<Expr>

  /// ```fsharp
  /// let myLambda = fun x -> x + 1
  /// 1 |> myLambda
  /// ```
  | EPipeVariable of id * varContainingPipeable : string * args : List<Expr>


module InfixFnName =
  /// The builtin each infix operator lowers to. Lowering and static checking
  /// share this table.
  let toBuiltinName (name : InfixFnName) : string =
    match name with
    | ArithmeticPlus -> "add"
    | ArithmeticMinus -> "subtract"
    | ArithmeticMultiply -> "multiply"
    | ArithmeticDivide -> "divide"
    | ArithmeticModulo -> "modulo"
    | ArithmeticPower -> "power"
    | ComparisonGreaterThan -> "greaterThan"
    | ComparisonGreaterThanOrEqual -> "greaterThanOrEqualTo"
    | ComparisonLessThan -> "lessThan"
    | ComparisonLessThanOrEqual -> "lessThanOrEqualTo"
    | StringConcat -> "stringAppend"
    | ComparisonEquals -> "equals"
    | ComparisonNotEquals -> "notEquals"

  // Keep in step with the type; `toBuiltinName` is the exhaustive mapping.
  let private all : List<InfixFnName> =
    [ ArithmeticPlus
      ArithmeticMinus
      ArithmeticMultiply
      ArithmeticDivide
      ArithmeticModulo
      ArithmeticPower
      ComparisonGreaterThan
      ComparisonGreaterThanOrEqual
      ComparisonLessThan
      ComparisonLessThanOrEqual
      StringConcat
      ComparisonEquals
      ComparisonNotEquals ]

  /// Recognize the infix operator implemented by a builtin.
  let tryFromBuiltinName (builtinName : string) : Option<InfixFnName> =
    all |> List.tryFind (fun name -> toBuiltinName name = builtinName)

  /// The unary-minus builtin the parser lowers `-x` (non-literal) to.
  let negateBuiltinName = "negate"


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
    | EInt(id, _)
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
    | EArg(id, _)
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
    | ESelf id -> id



/// A type defined by a package
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



/// Used to mark whether a function/type has been deprecated, and if so,
/// details about possible replacements/alternatives, and reasoning
///
/// Our use of this is sort of minimal currently.
/// I'm not sure if it's still an appropriate model going forward.
/// TODO reconsider
/// TODO has this changed at all since -classic? Check the old source.
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

module PackageType =
  type PackageType =
    { hash : FQTypeName.Package
      declaration : TypeDeclaration.T
      description : string }


module PackageValue =
  type PackageValue =
    { hash : FQValueName.Package; description : string; body : Expr }


module PackageFn =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type PackageFn =
    {
      hash : FQFnName.Package
      body : Expr
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string

      /// The author's ceiling on what this function and everything it calls
      /// may do at runtime, from the `:{…}` row on the declaration. It only
      /// restricts; granting stays with the instance, run and package layers.
      ///
      ///   None            no row was written: no function layer is added
      ///   Some Set.empty  `:{}`: effect-free, every host effect inside is denied
      ///   Some {Http; …}  `:{Http, …}`: only these; anything else inside is denied
      ///
      /// Part of the content hash and of the upgrade contract, so changing the
      /// row is a new version that `permissions update` asks about.
      permissionCeiling : Option<Set<Effects.Effect>>
    }


/// What happened to a branch. A CLOSED set, so a type rather than a string. Both cases are MONOTONIC:
/// applying one twice, or out of order, lands in the same place, so they need no stamp to arbitrate and
/// can travel between machines with nothing to compare against. A rename would be last-writer-wins and
/// there is no stamp on `branches` to arbitrate it, so there is no case for one.
type BranchEventKind =
  /// Its work is in its parent. Carries the ids of the ops the merge moved, because a peer holding the
  /// branch may hold MORE of it than the merger saw (its own unpushed edits), and the event must not
  /// take those with it.
  | Merged of ops : List<System.Guid>
  /// Put away, not deleted.
  | Archived

/// Operations on packages: the canonical, append-only unit of change.
type PackageOp =
  // Content operations - add definitions
  | AddType of typ : PackageType.PackageType
  | AddValue of value : PackageValue.PackageValue
  | AddFn of fn : PackageFn.PackageFn

  // Location operations - bind a name to a piece of content.
  // Content is identified by a Reference (hash + kind); the location is a
  // branch-scoped FQ path.
  /// `previous` is the hash this binding REPLACED at that location, or None when the location was empty.
  ///
  /// It lets two machines tell an ordinary edit from an independent creation, which otherwise look
  /// identical: one local binding, one incoming, no shared base. A SetName naming the other side's hash
  /// descends from it, so taking it is collaborating; NEITHER side naming anything means both invented the
  /// name, which is a real conflict.
  ///
  /// Filled in where the store is known -- authoring, propagation, a rename -- and None from the parsers,
  /// which read source files and have no store to ask. So None means "no predecessor known", and the
  /// detector only concludes anything from BOTH sides being None.
  | SetName of
    location : PackageLocation *
    target : Reference *
    previous : Option<Hash>

  /// A name stops existing. The fold unlists whatever is live at the location and nothing else: the
  /// content stays reachable by hash for anything that still references it, and a caller that does is
  /// what `constraints` is for. `previous` is the hash it unbound, for the same reason `SetName`
  /// carries one: a merge wants to know what was replaced.
  ///
  /// Not a `Deprecate`: deprecation is a statement about CONTENT and keys on the hash, so it would also
  /// mark every other name bound to the same body, and a deprecated name still resolves. A location's
  /// identity is (owner, modules, name), so no kind: whatever kind holds the name is what goes.
  | Unbind of location : PackageLocation * previous : Option<Hash>

  // Deprecation: author-initiated annotation on a specific content hash.
  //
  // Future: implicit deprecations as Constraints.
  //
  // Some SCM events raise an implicit deprecation signal — e.g. a
  // propagation leaves an item with no inbound refs, or a newly-bound fn
  // shadows an existing one with an identical signature. Surface these as
  // Constraints alongside merge and propagation conflicts, routed through
  // the same `status` / `review` / LSP flow the other conflict types use.
  //
  // Auto-resolution defaults to "ignore": the item stays live, no op is
  // emitted, the signal is informational. The Constraint stays visible to
  // the author, who can then pick from:
  //   (a) commit the auto-ignore ("reviewed, intentional") — records the
  //       acknowledgement so the signal doesn't re-fire on every future op,
  //   (b) emit an explicit resolution: a Deprecate op (usually Obsolete
  //       with a message, or SupersededBy pointing at the new item), a
  //       SetName rebinding, or an Unbind.
  //
  // The point is a prompt-for-committed-resolution loop that parallels
  // merge-conflict resolution — system surfaces what it noticed, author
  // commits their intent, op log carries both.
  | Deprecate of target : Reference * kind : DeprecationKind * message : string

  // Clear any prior deprecation on a target.
  // TODO: merge-into-main needs a permission axis once ACLs land (a branch
  //   shouldn't silently un-Harmful on merge).
  | Undeprecate of target : Reference

  /// What an item, or one named part of it, SAYS about itself: prose said ABOUT content rather
  /// than being part of it.
  ///
  /// A doc comment is not behaviour, so it is not in the identity hash (see `Canonical`): editing
  /// one leaves the hash alone, and every caller keeps resolving to the same item. That is only
  /// possible with an op of its own -- ops are content-addressed, so an `AddFn` that differs only
  /// in its docs IS the earlier `AddFn` and folds to nothing, which is exactly how a doc edit came
  /// to be reported as saved and then dropped.
  ///
  /// Keyed on content, like `Deprecate`: every name bound to this body describes the same thing.
  ///
  /// `previous` is the hash of the text this one replaces, or None when there was nothing there.
  /// It plays the role `SetName.previous` plays: without it, two people editing one doc comment are
  /// indistinguishable from one of them editing after seeing the other, and the loser's words
  /// vanish with nothing recorded. With it, the fold can tell an edit that DESCENDS from what this
  /// store holds (apply it) from one that does not (apply the newer, and record a conflict).
  ///
  /// TODO: the shape this wants to become is a package VALUE of a broadly-known type, roughly
  /// `{ text: String; reference: PackageThing }`, so that examples, deprecation notes and a third
  /// party's annotations of code they do not own are all the same mechanism. This op is the same
  /// idea with the vocabulary we have.
  ///
  /// `restating` is empty except on one path, and is never read by the fold: it is what makes a
  /// RESTATEMENT a distinct op. Ops are content-addressed, so "put this doc back to what it said
  /// before" produces the op that already exists, which dedupes and folds nothing -- the same hole
  /// `Decision.id` exists to fill for a name. `Inserts` fills it with a stamp when it sees a doc op
  /// that is already held and does not match what the target says now.
  | UpdateDoc of
    target : DocTarget *
    text : string *
    previous : Option<Hash> *
    restating : Option<string>

  /// A human's judgment about a NAME, recorded so that it travels.
  ///
  /// One op rather than two because overriding a binding and acking a finding are the same class of act:
  /// someone decided something about a location, and the decision has to hold on every machine rather
  /// than only the one they typed it on. They differ in what they DO, which is `kind`, so no case carries
  /// a field it cannot honour. Both write a status into `conflicts`; only `Override` also binds a name.
  ///
  /// `id` is what makes each decision a DISTINCT op, and idempotence is the CALLER's choice of what to
  /// put in it. Ops are content-addressed, so a decision that re-states an earlier one byte for byte IS
  /// that earlier op and folds nothing. A conflict override uses the conflict id, because resolving #7
  /// the same way twice is one decision stated twice. A propagation policy stamps the id with a time,
  /// because pin -> unset -> pin is three decisions and the third must not dedup into the first and so
  /// lose to the second under LWW. The fold never LOOKS UP by `id`: it is provenance and uniqueness.
  ///
  /// `reason` is the author's words, carried and never interpreted.
  | Decision of
    id : string *
    location : PackageLocation *
    reason : string *
    kind : DecisionKind

  /// Something that happened to a BRANCH, as opposed to something that happened to a name.
  ///
  /// Merging is otherwise local: the merged ops travel (they are main ops now) and
  /// the two mains converge on identical hashes, but the FACT of the merge does not,
  /// so a colleague's branch still lists as live work and they can keep authoring on
  /// something that already landed.
  ///
  /// An event for a branch this store has never heard of folds to nothing. That is
  /// not a failure: branch ids travel with a bundle, so the ones you share match,
  /// and the ones you do not share are none of your business.
  | BranchEvent of branchId : BranchId * event : BranchEventKind * at : string

// There is deliberately no op for a propagation or its undo. "These changes belong together" is
// what a COMMIT says, and "this version lost" is what a recorded conflict says; a cascade is just
// the `AddFn`/`SetName` pairs it produced.

//   | MoveItem of item: uuid * from : Location * to_: Location
//   // we can punt this for now, I think
//   //| MoveModule of from: Location * to_: Location // hmm what about the _timing_ of this?
//   // maybe this isn't supported, and we instead need _many_ moveItem


// prob belongs in LibMatter
// type BranchMergeConflict =
//   | TypeIntroducedButNotReferenced of FQTypeName.Package
//   | ...IntroducedButNotReferenced of ...


/// The kind of package item (function, type, or value)
and ItemKind =
  | Fn
  | Type
  | Value

  /// Convert from database string representation
  static member fromString(s : string) : ItemKind =
    match s with
    | "fn" -> Fn
    | "type" -> Type
    | "value" -> Value
    | _ -> Exception.raiseInternal $"Unknown item kind: {s}" []

  /// Convert to database string representation
  /// CLEANUP might be appropriate to either migrate these fns to LibSerialization,
  // or replace them w/ _binary_ serializer equivs (but, then DB is less queryable by humans directly)
  member this.toString() : string =
    match this with
    | Fn -> "fn"
    | Type -> "type"
    | Value -> "value"


/// A reference to a specific package item by content hash.
/// Collapses the pervasive (Hash * ItemKind) pairs into one shape, and leaves
/// room for future kinds (RefBuiltin, RefExternal, ...).
and Reference =
  | PackageType of Hash
  | PackageValue of Hash
  | PackageFn of Hash

  /// Extract the ItemKind (display helper).
  member this.kind : ItemKind =
    match this with
    | PackageType _ -> ItemKind.Type
    | PackageValue _ -> ItemKind.Value
    | PackageFn _ -> ItemKind.Fn

  /// Extract the content Hash.
  member this.hash : Hash =
    match this with
    | PackageType h
    | PackageValue h
    | PackageFn h -> h

  /// Build a Reference from a hash + item kind (common SQL-boundary need).
  static member fromHashAndKind(h : Hash, k : ItemKind) : Reference =
    match k with
    | ItemKind.Type -> PackageType h
    | ItemKind.Value -> PackageValue h
    | ItemKind.Fn -> PackageFn h


/// WHICH piece of prose an `UpdateDoc` sets.
///
/// One op with a target rather than a family of ops: the fold, the serializer, the LWW register and
/// the conflict path are identical for every case, and only the reach into the declaration differs.
///
/// The nested cases are why this exists at all. A doc comment on a FIELD, a case or a parameter has
/// never been part of identity either, so before this there was no op that could carry an edit to
/// one: the save reported success, the hash did not move, and the words were gone by the next read.
and DocTarget =
  /// The item's own doc comment.
  | ItemDoc of target : Reference

  /// One record field's, by field name.
  | RecordFieldDoc of target : Reference * fieldName : string

  /// One enum case's, by case name.
  | EnumCaseDoc of target : Reference * caseName : string

  /// One function parameter's, BY POSITION.
  ///
  /// Not by name, unlike the other two: a record field's name and an enum case's name are part of
  /// the identity hash, and a parameter's is not (a body references parameters by position, so the
  /// name is cosmetic). Two functions differing only in their parameter names are therefore ONE
  /// item, and a name-keyed target would name a parameter the stored declaration does not have.
  | ParameterDoc of target : Reference * parameterIndex : int

  /// The content this prose is about.
  member this.reference : Reference =
    match this with
    | ItemDoc r
    | RecordFieldDoc(r, _)
    | EnumCaseDoc(r, _)
    | ParameterDoc(r, _) -> r

  /// Which KIND of part, as the register stores it.
  member this.part : string =
    match this with
    | ItemDoc _ -> "item"
    | RecordFieldDoc _ -> "record-field"
    | EnumCaseDoc _ -> "enum-case"
    | ParameterDoc _ -> "parameter"

  /// WHICH part, by the name it has in the declaration; "" for the item itself. With `part`, the
  /// key one doc is stored under.
  member this.within : string =
    match this with
    | ItemDoc _ -> ""
    | RecordFieldDoc(_, n)
    | EnumCaseDoc(_, n) -> n
    | ParameterDoc(_, i) -> string i

  /// How it reads in a listing: `Some.Fn` or `Some.Type.fieldName`.
  member this.describe : string =
    match this with
    | ItemDoc _ -> "the item"
    | RecordFieldDoc(_, n) -> $"field {n}"
    | EnumCaseDoc(_, n) -> $"case {n}"
    | ParameterDoc(_, i) -> $"parameter {i + 1}"


/// What a `Decision` DID. The shared part of a decision (who, where, why) lives on the op; this is the
/// part that varies, so that a decision which binds a name and one which only closes a finding cannot be
/// confused for each other.
and DecisionKind =
  /// Bind this name to THIS version, the one the fold did not pick. Its own case rather than another
  /// `SetName` because ops are content-addressed: re-authoring the losing SetName produces the op that
  /// already exists, which INSERT-OR-IGNOREs and folds nothing. Binds with source='resolution', so
  /// `discard` will not wipe it, and closes the conflict as `overridden`.
  | Override of target : Reference

  /// A standing finding has been seen and accepted. Closes it as `acked`. Binds nothing: an ack changes
  /// what you are asked about, never what a name resolves to.
  | Ack of findingId : string

  /// Whether this name follows its dependencies when they move, or stays where it is.
  | Propagation of policy : PropagationPolicy

/// Pin and follow are the two standing answers; `Unset` is the absence of one, which the fold deletes
/// rather than stores, so "no policy" has a single spelling.
and PropagationPolicy =
  | Pin
  | Follow
  | Unset

  /// The spelling stored in `propagation_policy.policy`. Defined once because TWO folds write that
  /// column -- main's in `PackageOpPlayback` and a branch's in `Branches` -- and a policy that spells
  /// itself differently depending on which path folded it stops matching on read.
  member this.ToText : string =
    match this with
    | Pin -> "pin"
    | Follow -> "follow"
    | Unset -> "unset"

/// Why a package item has been deprecated. Author-supplied metadata on the
/// Deprecate op; consumers (LSP, CLI, runtime) decide how loud to be.
/// TODO: `Harmful` is the only kind SCM can't already express via rebinding;
///   if usage confirms `SupersededBy`/`Obsolete` overlap, fold into one.
and DeprecationKind =
  /// A different item (different hash) should be used instead.
  | SupersededBy of replacement : Reference

  /// Actively dangerous (security, correctness, data loss).
  /// Runtime halts on invocation by default; `--allow-harmful` overrides.
  /// Nothing currently prevents a Type from being marked Harmful, which
  /// seems silly — maybe address somehow, probably ignore. (A value can
  /// legitimately be Harmful if it holds a secret accidentally.)
  | Harmful

  /// Don't use this anymore (catch-all; no halt, no replacement pointer).
  | Obsolete


// A single repoint: what propagation moved, reported back to the caller so it can say so.
// NOT an op -- the actual state change is the accompanying SetName.
// `fromRef`/`toRef` carry both the hash and the item kind.
and PropagateRepoint =
  { location : PackageLocation; fromRef : Reference; toRef : Reference }


/// Reading and writing the prose a `DocTarget` names, inside a declaration.
///
/// One implementation, shared by the fold (which reaches through a serialized blob) and by the
/// branch overlay (which reaches through a loaded item). Two would drift, and it is the READ that
/// decides whether an incoming edit descends from what a store holds or diverges from it, so a
/// disagreement between them would be a disagreement about what is a conflict.
///
/// Every read answers `None` when the target does not fit -- a record field on a function, a
/// parameter the signature does not have. That is what makes an op for a part that no longer exists
/// fold to nothing rather than raise: such an op can only arrive from another instance, and one bad
/// op must not refuse the batch it came in.
module DocTarget =
  let private named
    (name : string)
    (nameOf : 'a -> string)
    (docOf : 'a -> string)
    (items : NEList<'a>)
    : Option<string> =
    items |> NEList.find (fun i -> nameOf i = name) |> Option.map docOf

  let private redoc
    (name : string)
    (nameOf : 'a -> string)
    (setDoc : 'a -> 'a)
    (items : NEList<'a>)
    : NEList<'a> =
    items |> NEList.map (fun i -> if nameOf i = name then setDoc i else i)

  let inFn (target : DocTarget) (fn : PackageFn.PackageFn) : Option<string> =
    match target with
    | ItemDoc _ -> Some fn.description
    | ParameterDoc(_, index) ->
      fn.parameters
      |> NEList.toList
      |> List.tryItem index
      |> Option.map (fun p -> p.description)
    | RecordFieldDoc _
    | EnumCaseDoc _ -> None

  let onFn
    (target : DocTarget)
    (text : string)
    (fn : PackageFn.PackageFn)
    : PackageFn.PackageFn =
    match target with
    | ItemDoc _ -> { fn with description = text }
    | ParameterDoc(_, index) ->
      { fn with
          parameters =
            fn.parameters
            |> NEList.mapWithIndex (fun i p ->
              if i = index then { p with description = text } else p) }
    | RecordFieldDoc _
    | EnumCaseDoc _ -> fn

  let inType (target : DocTarget) (t : PackageType.PackageType) : Option<string> =
    match target, t.declaration.definition with
    | ItemDoc _, _ -> Some t.description
    | RecordFieldDoc(_, name), TypeDeclaration.Record fields ->
      named
        name
        (fun (f : TypeDeclaration.RecordField) -> f.name)
        (fun f -> f.description)
        fields
    | EnumCaseDoc(_, name), TypeDeclaration.Enum cases ->
      named
        name
        (fun (c : TypeDeclaration.EnumCase) -> c.name)
        (fun c -> c.description)
        cases
    | _ -> None

  let onType
    (target : DocTarget)
    (text : string)
    (t : PackageType.PackageType)
    : PackageType.PackageType =
    let redefine definition =
      { t with declaration = { t.declaration with definition = definition } }

    match target, t.declaration.definition with
    | ItemDoc _, _ -> { t with description = text }
    | RecordFieldDoc(_, name), TypeDeclaration.Record fields ->
      redoc
        name
        (fun (f : TypeDeclaration.RecordField) -> f.name)
        (fun f -> { f with description = text })
        fields
      |> TypeDeclaration.Record
      |> redefine
    | EnumCaseDoc(_, name), TypeDeclaration.Enum cases ->
      redoc
        name
        (fun (c : TypeDeclaration.EnumCase) -> c.name)
        (fun c -> { c with description = text })
        cases
      |> TypeDeclaration.Enum
      |> redefine
    | _ -> t

  let inValue (target : DocTarget) (v : PackageValue.PackageValue) : Option<string> =
    match target with
    | ItemDoc _ -> Some v.description
    | _ -> None

  let onValue
    (target : DocTarget)
    (text : string)
    (v : PackageValue.PackageValue)
    : PackageValue.PackageValue =
    match target with
    | ItemDoc _ -> { v with description = text }
    | _ -> v


/// A package entity paired with its location
/// The op that says a binding AGAIN, or `None` for an op that is not a binding.
///
/// Ops are content-addressed, so `SetName(name -> H)` for a name that once held H IS the op that
/// bound it the first time: it dedupes, and when it does land it carries that op's stamp and loses
/// to anything bound since. A revert is unsayable as a `SetName`. `Decision`/`Override` can say it,
/// because it carries an id -- the reason it exists on the conflict path too. The id carries the
/// stamp, because 611 -> 622 -> 611 -> 622 is four decisions and the fourth must not dedupe into
/// the second.
///
/// Here rather than beside a writer, because main authoring and a branch's must say it identically
/// or a restatement syncs as a different op than the one that was made.
///
/// A doc restatement is the same shape one level down -- putting a doc back to text it held before
/// produces the op that set it then -- and is said by stamping the op's own `restating` field,
/// since prose is about CONTENT and there is no location for a `Decision` to answer about.
let restating (ts : string) (op : PackageOp) : Option<PackageOp> =
  match op with
  | PackageOp.SetName(location, target, _) ->
    let (Hash h) = target.hash
    let where =
      String.concat "." (location.owner :: (location.modules @ [ location.name ]))
    Some(
      PackageOp.Decision(
        $"restate:{where}:{h}:{ts}",
        location,
        "bound this name to a version it held before",
        DecisionKind.Override target
      )
    )
  | PackageOp.UpdateDoc(target, text, previous, _) ->
    Some(PackageOp.UpdateDoc(target, text, previous, Some ts))
  | _ -> None


type LocatedItem<'T> = { entity : 'T; location : PackageLocation }

module Search =
  /// The type of entity to search for
  type EntityType =
    | Type
    | Module
    | Fn
    | Value

  /// How deep to search in the module hierarchy
  type SearchDepth =
    | OnlyDirectDescendants
    | AllDescendants

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
      types : List<LocatedItem<PackageType.PackageType>>
      values : List<LocatedItem<PackageValue.PackageValue>>
      fns : List<LocatedItem<PackageFn.PackageFn>> }

/// Functionality written in Dark stored and managed outside of user space
///
/// Note: It may be tempting to think the `getX` fns shouldn't return Options,
/// but there's a chance of Local <-> Cloud not being fully in sync,
/// for whatever reasons.
type PackageManager =
  { findType : PackageLocation -> Ply<Option<FQTypeName.Package>>
    findValue : PackageLocation -> Ply<Option<FQValueName.Package>>
    findFn : PackageLocation -> Ply<Option<FQFnName.Package>>

    search : Search.SearchQuery -> Ply<Search.SearchResults>

    // CLEANUP why does the PT one even need these?
    getType : FQTypeName.Package -> Ply<Option<PackageType.PackageType>>
    getValue : FQValueName.Package -> Ply<Option<PackageValue.PackageValue>>
    getFn : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>>

    // Reverse lookups — returns ALL locations for a hash
    getTypeLocations : FQTypeName.Package -> Ply<List<PackageLocation>>
    getValueLocations : FQValueName.Package -> Ply<List<PackageLocation>>
    getFnLocations : FQFnName.Package -> Ply<List<PackageLocation>>

    init : Ply<unit> }


  static member empty =
    { findType = fun _ -> Ply None
      findFn = fun _ -> Ply None
      findValue = fun _ -> Ply None

      search = fun _ -> Ply { submodules = []; types = []; values = []; fns = [] }

      getType = fun _ -> Ply None
      getFn = fun _ -> Ply None
      getValue = fun _ -> Ply None

      getTypeLocations = fun _ -> Ply []
      getValueLocations = fun _ -> Ply []
      getFnLocations = fun _ -> Ply []

      init = uply { return () } }


  /// Allows you to side-load a few 'extras' in-memory, along
  /// the normal fetching functionality. (Mostly helpful for tests)
  static member withExtras
    (types : List<PackageType.PackageType * PackageLocation>)
    (values : List<PackageValue.PackageValue * PackageLocation>)
    (fns : List<PackageFn.PackageFn * PackageLocation>)
    (pm : PackageManager)
    : PackageManager =

    let typeLocationToHash =
      types |> List.map (fun (t, loc) -> loc, t.hash) |> Map.ofList
    let typeHashToLocations =
      types
      |> List.fold
        (fun acc (t, loc) ->
          let existing = Map.tryFind t.hash acc |> Option.defaultValue []
          Map.add t.hash (existing @ [ loc ]) acc)
        Map.empty
    let typeHashToType = types |> List.map (fun (t, _) -> t.hash, t) |> Map.ofList

    let valueLocationToHash =
      values |> List.map (fun (v, loc) -> loc, v.hash) |> Map.ofList
    let valueHashToLocations =
      values
      |> List.fold
        (fun acc (v, loc) ->
          let existing = Map.tryFind v.hash acc |> Option.defaultValue []
          Map.add v.hash (existing @ [ loc ]) acc)
        Map.empty
    let valueHashToValue = values |> List.map (fun (v, _) -> v.hash, v) |> Map.ofList

    let fnLocationToHash =
      fns |> List.map (fun (f, loc) -> loc, f.hash) |> Map.ofList
    let fnHashToLocations =
      fns
      |> List.fold
        (fun acc (f, loc) ->
          let existing = Map.tryFind f.hash acc |> Option.defaultValue []
          Map.add f.hash (existing @ [ loc ]) acc)
        Map.empty
    let fnHashToFn = fns |> List.map (fun (f, _) -> f.hash, f) |> Map.ofList

    { findType =
        fun location ->
          match Map.tryFind location typeLocationToHash with
          | Some hash -> Ply(Some hash)
          | None -> pm.findType location

      findValue =
        fun location ->
          match Map.tryFind location valueLocationToHash with
          | Some hash -> Ply(Some hash)
          | None -> pm.findValue location

      findFn =
        fun location ->
          match Map.tryFind location fnLocationToHash with
          | Some hash -> Ply(Some hash)
          | None -> pm.findFn location

      search = fun query -> pm.search query

      getType =
        fun hash ->
          match Map.tryFind hash typeHashToType with
          | Some t -> Ply(Some t)
          | None -> pm.getType hash

      getValue =
        fun hash ->
          match Map.tryFind hash valueHashToValue with
          | Some v -> Ply(Some v)
          | None -> pm.getValue hash

      getFn =
        fun hash ->
          match Map.tryFind hash fnHashToFn with
          | Some f -> Ply(Some f)
          | None -> pm.getFn hash

      getTypeLocations =
        fun hash ->
          uply {
            let local =
              Map.tryFind hash typeHashToLocations |> Option.defaultValue []
            let! fallback = pm.getTypeLocations hash
            return local @ fallback
          }

      getValueLocations =
        fun hash ->
          uply {
            let local =
              Map.tryFind hash valueHashToLocations |> Option.defaultValue []
            let! fallback = pm.getValueLocations hash
            return local @ fallback
          }

      getFnLocations =
        fun hash ->
          uply {
            let local = Map.tryFind hash fnHashToLocations |> Option.defaultValue []
            let! fallback = pm.getFnLocations hash
            return local @ fallback
          }

      init = pm.init }





(*
the source of truth is our core tables, which sync:
  package_ops, branches, instances
  should branch operations be separate from package ops? hmm idk.
  we should sync all ops that you have permissions to...
  oh, how _should_ we do permissioning?
  iI guess there's an SetName thing and later an ApproveName thing? Not sure I actually worked that out...
  | AddBranch? hmm.
  what if an Op referring to a branch is received before the AddBranch op? Prob ignore that for now, right?
  we really need to timestamp these ops in a super-safe way
  I guess working internationally helps us test this a bit...
  what about timezone switches and ... probably need NodaTime if we don't already have it

the package stuff is all a projection of that
  package types, values, fns
  locations, and how they map to those package items
*)





// /// Atomic operations that can be tracked and validated
// module Op =
//   type T =
//     // Content Operations - create new immutable content
//     | AddFunctionContent of hash: string * content: PackageFn.PackageFn
//     | AddTypeContent of hash: string * content: PackageType.PackageType
//     | AddValueContent of hash: string * content: PackageValue.PackageValue

//     // Name Operations - manage name pointers
//     | CreateName of location: PackageLocation.T * hash: string * contentType: string
//     | UpdateNamePointer of location: PackageLocation.T * oldHash: string * newHash: string
//     | MoveName of oldLocation: PackageLocation.T * newLocation: PackageLocation.T
//     | UnassignName of location: PackageLocation.T

//     // Content Operations - deprecate content (by hash)
//     | DeprecateContent of hash: string * reason: string * replacement: string option

// /// Types of conflicts that can occur when we try to apply an Op
// type Conflict =
//   | TODO




// /// A development session
// /// informally a 'branch'
// module Session =
//   type State =
//     | Active
//     | Abandoned
//     | Merged

//   type T = {
//     id: uuid
//     title: string
//     ops: List<uuid>
//     createdAt: System.DateTime
//     lastActiveAt: System.DateTime
//     state: SessionState.T
//     workspace: WorkspaceState.T
//   }



// /// Darklang instance definition -- what can we sync against
// module Instance =
//   type Location =
//     | LocalCLI of pathToExe: string // or maybe this should be path to dir? prob not.
//     | HttpServer of url: string

//   type T = {
//     id: uuid
//     name: string
//     location: Location
//   }



// --
// User things
// --
module DB =
  type T = { tlid : tlid; name : string; version : int; typ : TypeReference }


/// Compatibility shim: callers used to wrap a `DB.T` in `Toplevel.TLDB`
/// and read tlids via `Toplevel.toTLID`. Handler / TLHandler are gone
/// (Worker / Cron / REPL had no live consumers; HTTP went earlier with
/// the BwdServer rewrite). `DB.T` IS the toplevel now — keep the
/// `Toplevel.toTLID` accessor as a one-line shim so the noisier
/// callsites don't all churn shape simultaneously.
module Toplevel =
  let toTLID (db : DB.T) : tlid = db.tlid
