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
/// SCM branch identifier
/// Structural hash of a package item's content (shape, not name/location).
type Hash =
  | Hash of string
  // Explicit ToString — F# unions' default override goes through
  // StructuredPrintfImpl reflection, which is broken under AOT trimming.
  override this.ToString() = let (Hash s) = this in s

module Hash =
  let empty : Hash = Hash ""
  let toHexString (Hash h) : string = h

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
  | TDict of TypeReference

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
    { hash : FQFnName.Package
      body : Expr
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string }


/// Operations on packages
/// What happened to a branch. A CLOSED set, unlike `Decide`'s open-ended `kind`, so
/// it is a type rather than a string: there are only so many things that can happen
/// to a branch, and each carries its own payload or none. Both cases are MONOTONIC
/// on purpose: applying one twice, or out of order, lands in the same place, so they
/// need no stamp to arbitrate and can travel between machines with nothing to
/// compare against.
///
/// A rename is deliberately absent. It is last-writer-wins, so folding one needs a
/// stamp on `branches` to compare against and there is none. Defining the case
/// before it can be honoured would put a state in the format that the fold silently
/// ignores. Adding it later is free: op tags are per-case, so a new case leaves
/// every existing op's bytes, and therefore every existing op id, untouched.
type BranchEventKind =
  /// Its work is in its parent.
  | Merged
  /// Put away, not deleted.
  | Archived

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

  // Deprecation: author-initiated annotation on a specific content hash. Always explicit; implicit
  // deprecation signals raised as Constraints are a later design, not this.
  | Deprecate of target : Reference * kind : DeprecationKind * message : string

  // Clear any prior deprecation on a target.
  // TODO: merge-into-main needs a permission axis once ACLs land (a branch
  //   shouldn't silently un-Harmful on merge).
  | Undeprecate of target : Reference

  // A human OVERRIDING an automatic binding: bind this name to THIS version, the one
  // the machine did not pick. Two things use it, and they are the same act:
  //   - resolving a conflict: take the version the deterministic fold rejected.
  //     decisionId = conflict id
  //   - pinning: put this back on the version it had before propagation moved it.
  //     decisionId = the pin
  //
  // Its own op rather than another SetName, structurally: ops are content-addressed, so re-authoring
  // `SetName(name -> that hash)` produces the op that ALREADY exists -- the one being reinstated -- which
  // INSERT-OR-IGNOREs and folds nothing. `decisionId` is what makes it distinct, and records WHY the
  // binding was forced.
  //
  // Idempotence is the CALLER's choice, via what it puts in `decisionId`: a conflict resolution uses the
  // conflict id, so re-resolving #7 the same way is one decision stated twice; a pin stamps the id with a
  // time, because pin -> follow -> pin is three decisions.
  //
  // The fold binds it with source='resolution' so `discard` will not wipe it, and gives it a fresh stamp
  // so re-folding the ops that caused the situation cannot quietly undo it. A genuinely later edit still
  // wins. The fold IGNORES `decisionId`: it is provenance and uniqueness, never a lookup key.
  | Resolve of decisionId : string * location : PackageLocation * target : Reference

  // A person's standing DECISION about a name, as opposed to a binding of one.
  //
  // Two things use it today and they're the same act -- someone deciding something
  // about a location, and needing that decision to hold on every machine rather than
  // only the one they typed it on:
  //   - propagation policy: `kind = "propagation"`, `value` = "pin" | "follow"
  //   - a Constraint ack:   `kind = "constraint-ack"`, `value` = the finding id
  //
  // An OP rather than a row because a decision that does not travel is half a decision: an ack held only
  // locally re-reports forever, and differently on each machine.
  //
  // The fold writes it to whichever projection owns that kind (`propagation_policy`, `conflicts`), keeping
  // those tables derived rather than a second source of truth. Last-write-wins on
  // (kind, location, value-key) by origin_ts. `reason` is the author's words, carried and never
  // interpreted.
  //
  // `decidedAt` is what makes each decision a DISTINCT op. Without it, pin -> unset -> pin produces a third
  // op byte-identical to the first, which dedups to the first op's origin_ts and so loses to the unset
  // under LWW -- the second pin silently does nothing.
  | Decide of
    kind : string *
    location : PackageLocation *
    value : string *
    reason : string *
    decidedAt : string

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
  | BranchEvent of branchId : string * event : BranchEventKind * at : string

// Deliberately no PropagateUpdate / RevertPropagation op. "These changes belong
// together" is the COMMIT, and "this version lost" is a recorded conflict.

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
  // or replace them w/ _binary_ serializer equivs (but, then DB is less queryable by
  // humans directly)
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


/// A package entity paired with its location
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
