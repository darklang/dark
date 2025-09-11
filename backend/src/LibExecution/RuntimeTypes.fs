/// The core types and functions used by the Dark language's runtime.
///
/// This format is lossy, relative to the ProgramTypes; use IDs to refer back.
/// CLEANUP we could realistically expand upon this a bit,
///   excluding things like enum field names, fn param names, etc.
///   (referring back to PT by index or something)
///
/// CLEANUP there's some useful "reference things by hash" work to be done.
module LibExecution.RuntimeTypes

open Prelude

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
/// Used to reference a type defined in a Package
module FQTypeName =
  /// The hash of a type in the package manager
  type Package = Hash

  type FQTypeName = Package of Package

  let package (hash : Hash) : Package = hash

  let fqPackage (hash : Hash) : FQTypeName = Package hash


/// A Fully-Qualified Value Name
///
/// Used to reference a value defined by the runtime or in a Package
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

  let builtin (name : string) (version : int) : Builtin =
    assertBuiltin name version assertValueName
    { name = name; version = version }

  let package (hash : Hash) : Package = hash

  let fqPackage (hash : Hash) : FQValueName = Package hash


/// A Fully-Qualified Function Name
///
/// Used to reference a function defined by the runtime or in a Package
module FQFnName =
  /// A function built into the runtime
  type Builtin = { name : string; version : int }

  type Package = Hash

  type FQFnName =
    | Builtin of Builtin
    | Package of Package

  let assertBuiltinFnName (name : string) : unit =
    assertRe $"Fn name must match" builtinNamePattern name

  let builtin (name : string) (version : int) : Builtin =
    assertBuiltin name version assertBuiltinFnName
    { name = name; version = version }

  let package (hash : Hash) = hash

  let fqBuiltin (name : string) (version : int) : FQFnName =
    Builtin { name = name; version = version }

  let fqPackage (hash : Hash) : FQFnName = Package hash


  let isInternalFn (fnName : Builtin) : bool = fnName.name.Contains "darkInternal"


/// TODO include "ParseTime" in name (requires a lot of boring work in many files)
type NameResolutionError =
  | NotFound of List<string>
  | InvalidName of List<string>

type NameResolution<'a> = Result<'a, NameResolutionError>


/// A KnownType represents the type of a dval.
///
/// Many KnownTypes (such as lists and records) have nested types. Often, these
/// nested types are unknown (such as the contents of an empty list, or the
/// `Result.Error` type for `Ok 5`). As such, KnownTypes always nest ValueTypes
/// (an optional form of KnownType).
type KnownType =
  | KTUnit
  | KTBool
  | KTInt8
  | KTUInt8
  | KTInt16
  | KTUInt16
  | KTInt32
  | KTUInt32
  | KTInt64
  | KTUInt64
  | KTInt128
  | KTUInt128
  | KTFloat
  | KTChar
  | KTString
  | KTUuid
  | KTDateTime

  /// `let empty =    []` // KTList Unknown
  /// `let intList = [1]` // KTList (ValueType.Known KTInt64)
  | KTList of ValueType

  /// Intuitively, since `Dval`s generate `KnownType`s, you would think that we can
  /// use `KnownType`s in a `KTTuple`.
  ///
  /// However, we sometimes construct a KTTuple to repesent the type of a Tuple
  /// which doesn't exist. For example, in `List.zip [] []`, we create the result
  /// from the types of the two lists, which themselves might be (and likely are)
  /// `Unknown`.
  | KTTuple of ValueType * ValueType * List<ValueType>

  /// let f = (fun x -> x)        // KTFn([Unknown], Unknown)
  /// let intF = (fun (x: Int) -> x) // KTFn([Known KTInt64], Unknown)
  ///
  /// Note that we could theoretically know some return types by analyzing the
  /// code or type signatures of functions. We don't do this yet as it's
  /// complicated. When we do decide to do this, some incorrect programs may stop
  /// functioning (see example). Our goal is for correctly typed functions to
  /// stay working so this might be ok.
  ///
  /// For example:
  ///   let z1 = (fun x -> 5)
  ///   let z2 = (fun x -> "str")
  /// `[z1, z2]` is allowed now but might not be allowed later
  | KTFn of args : NEList<ValueType> * ret : ValueType

  /// At time of writing, all DBs are of a specific type, and DBs may only be
  /// referenced directly, but we expect to eventually allow references to DBs
  /// where the type may be unknown
  /// List.head ([]: List<DB<'a>>) // KTDB (Unknown)
  | KTDB of ValueType

  /// let n = None          // type args: [Unknown]
  /// let s = Some(5)       // type args: [Known KTInt64]
  /// let o = Ok (5)        // type args: [Known KTInt64, Unknown]
  /// let e = Error ("str") // type args: [Unknown, Known KTString]
  | KTCustomType of FQTypeName.FQTypeName * typeArgs : List<ValueType>

  /// let myDict = {} // KTDict Unknown
  | KTDict of ValueType

/// Represents the actual type of a Dval
///
/// "Unknown" represents the concept of "bottom" in
///   type system / data flow analysis / lattices
and [<RequireQualifiedAccess>] ValueType =
  | Unknown
  | Known of KnownType




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
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TList of TypeReference
  | TDict of TypeReference // CLEANUP add key type
  | TFn of NEList<TypeReference> * TypeReference
  | TCustomType of
    NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference>
  | TVariable of string
  | TDB of TypeReference


  member this.isFn() : bool =
    match this with
    | TFn _ -> true
    | _ -> false

  member this.isConcrete() : bool =
    let rec isConcrete (t : TypeReference) : bool =
      match t with
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
      | TDateTime -> true

      | TTuple(t1, t2, ts) ->
        isConcrete t1 && isConcrete t2 && List.forall isConcrete ts
      | TList t -> isConcrete t
      | TDict t -> isConcrete t

      | TCustomType(_, ts) -> List.forall isConcrete ts

      | TFn(ts, t) -> NEList.forall isConcrete ts && isConcrete t

      | TDB t -> isConcrete t

      | TVariable _ -> false

    isConcrete this


/// Our record/tracking of any type arguments in scope
///
/// i.e. within the execution of
///   `let serialize<'a> (x : 'a) : string = ...`,
/// called with inputs
///   `serialize<int> 1`,
/// we would have a TypeSymbolTable of
///  { "a" => TInt64 }
type TypeSymbolTable = Map<string, ValueType>



// ------------
// Instructions ("bytecode")
// ------------
[<Measure>]
type register

type Register = int //<register> // TODO: unit of measure

/// The LHS pattern in
/// - a `let` binding (in `let x = 1`, the `x`)
/// - a lambda (in `fn (x, y) -> x + y`, the `(x, y)`
type LetPattern =
  /// `let x = 1`
  | LPVariable of extractTo : Register

  // /// `let _ = 1`
  // | LPIgnored

  /// `let (x, y) = (1, 2)`
  | LPTuple of first : LetPattern * second : LetPattern * theRest : List<LetPattern>

  /// `let () = ()`
  | LPUnit


type MatchPattern =
  | MPUnit
  | MPBool of bool
  | MPInt8 of int8
  | MPUInt8 of uint8
  | MPInt16 of int16
  | MPUInt16 of uint16
  | MPInt32 of int32
  | MPUInt32 of uint32
  | MPInt64 of int64
  | MPUInt64 of uint64
  | MPInt128 of System.Int128
  | MPUInt128 of System.UInt128
  | MPFloat of float
  | MPChar of string
  | MPString of string
  | MPList of List<MatchPattern>
  | MPListCons of head : MatchPattern * tail : MatchPattern
  | MPTuple of
    first : MatchPattern *
    second : MatchPattern *
    theRest : List<MatchPattern>
  | MPEnum of caseName : string * fields : List<MatchPattern>
  | MPVariable of Register
  | MPOr of NEList<MatchPattern>


type StringSegment =
  | Text of string
  | Interpolated of Register


type Instruction =
  // == Simple register operations ==
  /// Push a value into a register
  | LoadVal of loadTo : Register * Dval

  | CopyVal of copyTo : Register * copyFrom : Register

  // TODO: update both of these to take a _single_ arg,
  // and replace the 'rhs' component with a 'jumpIfFalse' component
  // hmm or maybe jumpIfTrue.
  // the point here is to allow for short-circuiting, allowing the RHS instructions to be skipped
  // if the first argument resolves the condition.
  // So I guess Or needs jumpIfTrue, and And needs jumpIfFalse.
  // and the jumpIfFalse/jumpIfTrue might have a 0-instr skip for the RHS.
  | Or of createTo : Register * lhs : Register * rhs : Register
  | And of createTo : Register * lhs : Register * rhs : Register

  // == Working with Basic Types ==
  | CreateString of createTo : Register * segments : List<StringSegment>

  // == Working with Variables ==
  /// Extract values in a Register to 0 or more registers, per the pattern.
  /// (e.g. `let (x, y) = (1, 2)`)
  ///
  /// Errors if the pattern doesn't match the value.
  | CheckLetPatternAndExtractVars of valueReg : Register * pat : LetPattern


  // == Flow Control ==

  // -- Jumps --
  /// Go `n` instructions forward, if the value in the register is `false`
  | JumpByIfFalse of instrsToJump : int * conditionReg : Register

  /// Go `n` instructions forward, unconditionally
  | JumpBy of instrsToJump : int


  // -- Match --
  /// Check if the value in the noted register the noted pattern,
  /// and extract values to registers per the nested patterns.
  | CheckMatchPatternAndExtractVars of
    /// what we're matching against
    valueReg : Register *
    pat : MatchPattern *
    /// jump over the current `match` expr's instructions if it doesn't match
    /// (to the next case, or to the "unmatched" instruction)
    failJump : int

  /// Could not find matching case in a match expression
  /// CLEANUP we probably need a way to reference back to PT so we can get useful RTEs
  /// TODO probably better as a usage of a broader "Fail" error case.
  | MatchUnmatched of valueReg : Register


  // == Working with Collections ==
  | CreateTuple of
    createTo : Register *
    first : Register *
    second : Register *
    theRest : List<Register>

  /// Create a list, and type-check to ensure the items are of a consistent type
  | CreateList of createTo : Register * itemsToAdd : List<Register>

  /// Create a dict, and type-check to ensure the entries are of a consistent type
  | CreateDict of createTo : Register * entries : List<string * Register>


  // == Working with Custom Data ==
  // -- Records --
  | CreateRecord of
    createTo : Register *
    typeName : FQTypeName.FQTypeName *
    typeArgs : List<TypeReference> *
    fields : List<string * Register>

  | CloneRecordWithUpdates of
    createTo : Register *
    originalRecordReg : Register *
    updates : List<string * Register>

  | GetRecordField of
    targetReg : Register *
    recordReg : Register *
    fieldName : string

  // -- Enums --
  | CreateEnum of
    createTo : Register *
    typeName : FQTypeName.FQTypeName *
    typeArgs : List<TypeReference> *
    caseName : string *
    fields : List<Register>


  | LoadValue of createTo : Register * FQValueName.FQValueName

  // == Working with things that Apply ==

  | CreateLambda of createTo : Register * lambda : LambdaImpl

  /// Apply some args (and maybe type args) to something
  /// (a named function, or lambda, etc)
  | Apply of
    createTo : Register *
    thingToApply : Register *
    typeArgs : List<TypeReference> *
    args : NEList<Register>

  // == Errors ==
  | RaiseNRE of NameResolutionError

  | VarNotFound of targetRegIfSecretOrDB : Register * name : string

  | CheckIfFirstExprIsUnit of Register

and Instructions =
  {
    /// How many registers are used in evaluating these instructions
    registerCount : int

    /// The instructions themselves
    instructions : List<Instruction>

    /// The register that will hold the result of the instructions
    resultIn : Register
  }


and DvalMap = Map<string, Dval>


/// Lambdas are a bit special:
/// they have to close over variables, and have their own set of instructions, not embedded in the main set
///
/// Note to self: trying to remove typeSymbolTable here
/// causes all sorts of scoping issues. Beware.
and LambdaImpl =
  {
    // -- Things we know as soon as we create the lambda --
    // maybe we need the TL ID as well?
    // CLEANUP maybe incldue word 'source' or something in this field name?
    exprId : id

    /// How should the arguments be deconstructed?
    patterns : NEList<LetPattern>

    /// When the lambda is defined,
    /// we need to "close over" any symbols 'above' that are referenced.
    ///
    /// e.g. in
    /// ```fsharp
    /// let a = 1
    /// let incr = fn x -> x + a
    /// incr 2
    /// ```
    /// , the lambda `fn x -> x + a` closes over `a`,
    /// which we record as `[(1, 2)]`
    /// (copy from register '1' above into register '2' in this CF)
    ///
    /// PT2RT has the duty of creating and passing in (PT2RT-only)
    /// symtable for the evaluation of the expr on the RHS
    registersToCloseOver : List<Register * Register>

    instructions : Instructions
  }


and ApplicableNamedFn =
  { name : FQFnName.FQFnName

    typeSymbolTable : TypeSymbolTable

    // CLEANUP maybe this could be List<ValueType>?
    typeArgs : List<TypeReference>

    argsSoFar : List<Dval> }

and ApplicableLambda =
  {
    /// The lambda's ID, corresponding to the PT.Expr
    /// (the actual implementation is stored in the VMState)
    ///
    /// CLEANUP including the TLID of the expr here would be useful
    /// (exprId alone isn't enough to perform equality checks, etc.)
    exprId : id

    /// We _could_ have this be List<Register * Register>
    /// , but we run some risk of the register's value changing
    /// between the time we create the lambda and the time we apply it.
    /// (even though, at time of writing, this seems impossible.)
    closedRegisters : List<Register * Dval>

    /// A cache/copy of the type symbol table[1] when the lambda was created.
    ///
    /// [1] the `name: String -> Type` lookup of resolved generics
    /// for e.g. `Option<'a>`
    typeSymbolTable : TypeSymbolTable

    argsSoFar : List<Dval>
  }


/// Any thing that can be applied,
/// along with anything needed within their application closure
/// TODO: follow up with typeSymbols
and Applicable =
  | AppLambda of ApplicableLambda
  | AppNamedFn of ApplicableNamedFn




// We use NoComparison here to avoid accidentally using structural comparison
and [<NoComparison>] Dval =
  | DUnit

  // Simple types
  | DBool of bool

  | DInt8 of int8
  | DUInt8 of uint8
  | DInt16 of int16
  | DUInt16 of uint16
  | DInt32 of int32
  | DUInt32 of uint32
  | DInt64 of int64
  | DUInt64 of uint64
  | DInt128 of System.Int128
  | DUInt128 of System.UInt128

  | DFloat of double

  | DChar of string // TextElements (extended grapheme clusters) are provided as strings
  | DString of string

  | DDateTime of DarkDateTime.T
  | DUuid of System.Guid

  // Compound types
  | DList of ValueType * List<Dval>
  | DTuple of first : Dval * second : Dval * theRest : List<Dval>
  | DDict of
    // This is the type of the _values_, not the keys. Once users can specify the
    // key type, we likely will need to add a `keyType: ValueType` field here. TODO
    valueType : ValueType *
    entries : DvalMap

  // TODO: go through all instances of DRecord and DEnum
  // and make sure the typeNames are in the correct order

  // -- custom types --
  | DRecord of
    sourceTypeName : FQTypeName.FQTypeName *
    runtimeTypeName : FQTypeName.FQTypeName *
    // CLEANUP
    // Do we need to split this into sourceTypeArgs and runtimeTypeArgs?
    // What are we even using the source stuff for? error-reporting?
    // Could source stuff be erased in PT2RT, if we dealt with alias-resolution there?
    typeArgs : List<ValueType> *
    fields : DvalMap

  | DEnum of
    sourceTypeName : FQTypeName.FQTypeName *
    runtimeTypeName : FQTypeName.FQTypeName *
    typeArgs : List<ValueType> *
    caseName : string *
    fields : List<Dval>

  | DApplicable of Applicable

  // References
  | DDB of name : string


and DvalTask = Ply<Dval>



and ThreadID = uuid

and BuiltInParam =
  { name : string
    typ : TypeReference
    blockArgs : List<string>
    description : string }

  static member make
    (name : string)
    (typ : TypeReference)
    (description : string)
    : BuiltInParam =
    assert_ "make called on TFn" [ "name", name ] (not (typ.isFn ()))
    { name = name; typ = typ; description = description; blockArgs = [] }

  static member makeWithArgs
    (name : string)
    (typ : TypeReference)
    (description : string)
    (blockArgs : List<string>)
    : BuiltInParam =
    assert_ "makeWithArgs not called on TFn" [ "name", name ] (typ.isFn ())
    { name = name; typ = typ; description = description; blockArgs = blockArgs }



module RuntimeError =
  module TypeChecking =
    type TypeCheckPathPart =
      | ListType

      | DictValueType
      // CLEANUP add DictKeyType here, once Dicts support non-string keys

      | TupleLength of expected : int * actual : int
      | TupleAtIndex of int

      | TypeArgLength of
        typeName : FQTypeName.FQTypeName *
        expected : int *
        actual : int
      | TypeArg of
        typeName : FQTypeName.FQTypeName *
        typeArgIndex : int *
        typeArgCount : int


    type ReverseTypeCheckPath = List<TypeCheckPathPart>


  module Bools =
    type Error =
      | AndOnlySupportsBooleans of gotLeft : ValueType * gotRight : ValueType
      | OrOnlySupportsBooleans of gotRight : ValueType * gotLeft : ValueType
      | ConditionRequiresBool of actualValueType : ValueType * actualValue : Dval

  module Ints =
    type Error =
      | DivideByZeroError
      | OutOfRange // CLEANUP consider including the out-of-range value
      | NegativeExponent
      | NegativeModulus
      | ZeroModulus

  module Strings =
    type Error = NonStringInInterpolation of vt : ValueType * dv : Dval


  module Lists =
    type Error =
      | TriedToAddMismatchedData of
        index : int *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

  module Dicts =
    type Error =
      | TriedToAddKeyAfterAlreadyPresent of key : string

      | TriedToAddMismatchedData of
        key : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval


  module Lets =
    // TODO consider some kinda _path_ thing like with JSON errors:
    // type Details =
    //   /// Unit pattern does not match
    //   | UnitPatternDoesNotMatch

    //   /// Tuple pattern does not match
    //   | TuplePatternDoesNotMatch

    //   /// Tuple pattern has wrong number of elements
    //   | TuplePatternWrongLength of expected: Int * actual: Int

    // maybe it'd be better to present:
    // - top-level path we're matching against
    // - the path to failure
    // - (?) ??

    type Error =
      /// Could not decompose `{someFn dval}` with pattern `{someFn pat}` in `let` expression
      | PatternDoesNotMatch of dval : Dval * pat : LetPattern

  module Matches =
    // TODO "When condition should be a boolean" -- this could warn _or_ error -- which do we want?
    // CLEANUP "Match must have at least one case"
    type Error =
      /// Could not find matching case for the given value
      | MatchUnmatched of unmatchedValue : Dval

  module Enums =
    type Error =
      | ConstructionWrongNumberOfFields of
        typeName : FQTypeName.FQTypeName *
        caseName : string *
        expectedFieldCount : int64 *
        actualFieldCount : int64

      | ConstructionCaseNotFound of
        typeName : FQTypeName.FQTypeName *
        caseName : string

      | ConstructionFieldOfWrongType of
        caseName : string *
        fieldIndex : int64 *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval


  module Records =
    // CLEANUP _maybe_ "Record must have at least one field" (Q: for defs, or instances?)
    // I'm not totally convinced, though - `type WIP = {}` seems useful.
    // Later note -- this^ should be in some separate error tree for _dev-time_ errors

    type Error =
      // -- Creation --
      | CreationTypeNotRecord of name : FQTypeName.FQTypeName
      | CreationEmptyKey // I'm not quite sure how this can be reached(?)
      | CreationMissingField of fieldName : string
      | CreationDuplicateField of fieldName : string
      | CreationFieldNotExpected of fieldName : string
      | CreationFieldOfWrongType of
        fieldName : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      // -- Update --
      | UpdateNotRecord of actualType : ValueType
      | UpdateEmptyKey
      | UpdateDuplicateField of fieldName : string
      | UpdateFieldNotExpected of fieldName : string
      | UpdateFieldOfWrongType of
        fieldName : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      // -- Field Access --
      | FieldAccessEmptyFieldName
      | FieldAccessFieldNotFound of fieldName : string
      | FieldAccessNotRecord of actualType : ValueType


  /// Errors that occur when trying to apply a function or lambda
  module Applications =
    type Error =
      | ExpectedApplicableButNot of actualTyp : ValueType * actualValue : Dval

      // specific to fns
      | WrongNumberOfTypeArgsForFn of
        fn : FQFnName.FQFnName *
        expected : int64 *
        actual : int64

      | CannotApplyTypeArgsMoreThanOnce

      | TooManyArgsForFn of
        fn : FQFnName.FQFnName *
        expected : int64 *
        actual : int64

      | FnParameterNotExpectedType of
        fnName : FQFnName.FQFnName *
        paramIndex : int64 *
        paramName : string *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      | FnResultNotExpectedType of
        fnName : FQFnName.FQFnName *
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

      // specific to lambdas
      | CannotApplyTypeArgsToLambda
      | TooManyArgsForLambda of lambdaExprId : id * expected : int64 * actual : int64

  module Statements =
    type Error =
      | FirstExpressionMustBeUnit of
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval


  module Unwraps =
    type Error =
      | GotNone
      | GotError of err : Dval
      | NonOptionOrResult of actual : Dval
      | MultipleArgs of args : List<Dval>

  module Jsons =
    type Error =
      | UnsupportedType of TypeReference
      | CannotSerializeValue of Dval

  module CLIs =
    type Error =
      | NoExpressionsToExecute
      | NonIntReturned of actuallyReturned : Dval



  /// RuntimeError is the major way of representing errors that occur at runtime.
  /// Most are focused on user errors, such as trying to put an Int in a list of Bools.
  /// Some cases represent internal failures, not at the fault of a user.
  ///
  /// These are not to be confused with Results, which should be used
  /// in functions to represent _expected_ cases of failure.
  ///
  /// See `docs/errors.md` for more discussion.
  type Error =
    | Bool of Bools.Error
    | Int of Ints.Error
    | String of Strings.Error

    | List of Lists.Error
    | Dict of Dicts.Error

    | Let of Lets.Error
    | VariableNotFound of attemptedVarName : string

    | EqualityCheckOnIncompatibleTypes of left : ValueType * right : ValueType

    | IfConditionNotBool of actualValue : Dval * actualValueType : ValueType

    | Match of Matches.Error

    | ParseTimeNameResolution of NameResolutionError

    | TypeNotFound of name : FQTypeName.FQTypeName
    | FnNotFound of name : FQFnName.FQFnName
    | ValueNotFound of name : FQValueName.FQValueName

    | WrongNumberOfTypeArgsForType of
      fn : FQTypeName.FQTypeName *
      expected : int64 *
      actual : int64

    | Enum of Enums.Error
    | Record of Records.Error

    | Apply of Applications.Error

    | Unwrap of Unwraps.Error

    | Json of Jsons.Error


    // stuff that isn't _quite _ "core", and maybe should belong elsewhere
    // , once RTEs are (somehow) more extensible

    | CLI of CLIs.Error

    | DBSetOfWrongType of expected : TypeReference * actual : ValueType

    | Statement of Statements.Error

    // punting these until DBs are supported again
    // - bring back this RTE where/when relevant "Attempting to access field '{fieldName}' of a Datastore (use `DB.*` standard library functions to interact with Datastores. Field access only work with records)"
    // - in backend/src/LibCloud/SqlCompiler.fs:
    //   - 1223: | SqlCompilerException errStr -> return Error(RuntimeError.oldError errStr)
    //   - 1224: // return Error(RuntimeError.oldError (errStr + $"\n\nIn body: {body}"))
    //   - | SqlCompiler of SqlCompiler.Error // -- or maybe this should happen during PT2RT? hmm.


    /// Sometimes, very-unexpected things happen. This is a catch-all for those.
    ///
    /// For local/private runtimes+hosting, allow users to see the details,
    /// but (TODO) for _our_ hosting, users shouldn't see the whole call stack or
    /// whatever, for (our) safety. Perhaps we can provide an opaque ID refer to
    /// the error in a support ticket.
    | UncaughtException of msg : string * metadata : List<string * Dval>


// CLEANUP the ThreadID isn't useful yet -- consider abandoning for now.
exception RuntimeErrorException of Option<ThreadID> * rte : RuntimeError.Error


let raiseRTE (threadId : ThreadID) (rte : RuntimeError.Error) : 'a =
  raise (RuntimeErrorException(Some threadId, rte))

let raiseUntargetedRTE (rte : RuntimeError.Error) : 'a =
  raise (RuntimeErrorException(None, rte))



type ExecutionPoint =
  /// User is executing some "arbitrary" expression, passed in by a user.
  /// This should only be at the `entrypoint` of a CallStack.
  ///
  /// Executing some top-level handler,
  /// such as a saved Script, an HTTP handler, or a Cron.
  | Source

  // Executing some function
  | Function of FQFnName.FQFnName

  /// Executing some lambda
  | Lambda of parent : ExecutionPoint * lambdaExprId : id


/// Not: in reverse order
type CallStack = List<ExecutionPoint>

module CallStack =
  let entrypoint (cs : CallStack) : Option<ExecutionPoint> = List.last cs

  let last (cs : CallStack) : Option<ExecutionPoint> = List.head cs


/// Internally in the runtime, we allow throwing RuntimeErrorExceptions. At the
/// boundary, typically in Execution.fs, we will catch the exception, and return
/// this type.
/// CLEANUP return a call stack or vmstate, or something, here
type ExecutionResult = Result<Dval, RuntimeError.Error * CallStack>

/// IncorrectArgs should never happen, as all functions are type-checked before
/// calling. If it does happen, it means that the type parameters in the Fn structure
/// do not match the args expected in the Builtin function definition.
/// CLEANUP should this take more args, so we can find the error? Maybe just the fn name?
let incorrectArgs () = Exception.raiseInternal "IncorrectArgs" []




// Used to mark whether a function/type has been deprecated, and if so,
// details about possible replacements/alternatives, and reasoning
type Deprecation<'name> =
  | NotDeprecated

  // The exact same thing is available under a new, preferred name
  | RenamedTo of 'name

  /// This has been deprecated and has a replacement we can suggest
  | ReplacedBy of 'name

  /// This has been deprecated and not replaced, provide a message for the user
  | DeprecatedBecause of reason : string


module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference }

  type EnumCase = { name : string; fields : List<TypeReference> }

  type Definition =
    | Alias of TypeReference
    | Record of NEList<RecordField>
    | Enum of NEList<EnumCase>

  type T = { typeParams : List<string>; definition : Definition }



// Functions for working with Dark runtime values
module Dval =
  let rec toValueType (dv : Dval) : ValueType =
    match dv with
    | DUnit -> ValueType.Known KTUnit

    | DBool _ -> ValueType.Known KTBool

    | DInt8 _ -> ValueType.Known KTInt8
    | DUInt8 _ -> ValueType.Known KTUInt8
    | DInt16 _ -> ValueType.Known KTInt16
    | DUInt16 _ -> ValueType.Known KTUInt16
    | DInt32 _ -> ValueType.Known KTInt32
    | DUInt32 _ -> ValueType.Known KTUInt32
    | DInt64 _ -> ValueType.Known KTInt64
    | DUInt64 _ -> ValueType.Known KTUInt64
    | DInt128 _ -> ValueType.Known KTInt128
    | DUInt128 _ -> ValueType.Known KTUInt128
    | DFloat _ -> ValueType.Known KTFloat
    | DChar _ -> ValueType.Known KTChar
    | DString _ -> ValueType.Known KTString
    | DDateTime _ -> ValueType.Known KTDateTime
    | DUuid _ -> ValueType.Known KTUuid

    | DList(t, _) -> ValueType.Known(KTList t)
    | DDict(t, _) -> ValueType.Known(KTDict t)
    | DTuple(first, second, theRest) ->
      ValueType.Known(
        KTTuple(toValueType first, toValueType second, List.map toValueType theRest)
      )

    | DRecord(_, typeName, typeArgs, _) ->
      KTCustomType(typeName, typeArgs) |> ValueType.Known

    | DEnum(_, typeName, typeArgs, _, _) ->
      KTCustomType(typeName, typeArgs) |> ValueType.Known

    | DApplicable applicable ->
      match applicable with
      | AppLambda _lambda ->
        // TODO something
        //   KTFn(
        //     NEList.map (fun _ -> ValueType.Unknown) lambda.parameters,
        //     ValueType.Unknown
        //   )
        //   |> ValueType.Known
        ValueType.Unknown

      // TODO look up type, etc
      // (probably forces us to make this fn async?)
      | AppNamedFn _named -> ValueType.Unknown

    // CLEANUP follow up when DDB has a typeReference
    | DDB _ -> ValueType.Unknown




// ------------
// Package-Space
// ------------
module PackageType =
  type PackageType = { hash : Hash; declaration : TypeDeclaration.T }

module PackageValue =
  type PackageValue = { hash : Hash; body : Dval }

module PackageFn =
  type Parameter = { name : string; typ : TypeReference }

  type PackageFn =
    { hash : Hash
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference

      // CLEANUP consider renaming - just `instructions` maybe?
      body : Instructions }


/// Functionality written in Dark stored and managed outside of user space
///
/// Note: it may be tempting to think these shouldn't return Options,
/// but if/when Package items may live (for some time) only on local systems,
/// there's a chance some code will be committed, referencing something
/// not yet in the Cloud PM.
/// (though, we'll likely demand deps. in the PM before committing something upstream...)
type PackageManager =
  { getType : FQTypeName.Package -> Ply<Option<PackageType.PackageType>>
    getValue : FQValueName.Package -> Ply<Option<PackageValue.PackageValue>>
    getFn : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>>

    init : Ply<unit> }

  static member empty =
    { getType = (fun _ -> Ply None)
      getFn = (fun _ -> Ply None)
      getValue = (fun _ -> Ply None)

      init = uply { return () } }

  /// Allows you to side-load a few 'extras' in-memory, along
  /// the normal fetching functionality. (Mostly helpful for tests)
  static member withExtras
    (types : List<PackageType.PackageType>)
    (values : List<PackageValue.PackageValue>)
    (fns : List<PackageFn.PackageFn>)
    (pm : PackageManager)
    : PackageManager =
    { getType =
        fun hash ->
          match types |> List.tryFind (fun t -> t.hash = hash) with
          | Some t -> Some t |> Ply
          | None -> pm.getType hash
      getValue =
        fun hash ->
          match values |> List.tryFind (fun v -> v.hash = hash) with
          | Some v -> Some v |> Ply
          | None -> pm.getValue hash
      getFn =
        fun hash ->
          match fns |> List.tryFind (fun f -> f.hash = hash) with
          | Some f -> Some f |> Ply
          | None -> pm.getFn hash
      init = pm.init }


// ------------
// User-/Canvas- Space
// ------------
module DB =
  // CLEANUP consider making typ a ValueType instead
  type T = { tlid : tlid; name : string; typ : TypeReference; version : int }

module Secret =
  type T = { name : string; value : string; version : int }



// ------------
// Builtins, Execution State, Package Manager
// A bunch of tangled things we need to `and` together
// ------------

/// <summary>
/// Used to mark whether a function can be run on the client rather than backend.
/// </summary>
/// <remarks>
/// The runtime needs to know whether to save a function's results when it
/// runs. Pure functions that can be run on the client do not need to have
/// their results saved.
/// In addition, some functions can be run without side-effects; to give
/// the user a good experience, we can run them as soon as they are added.
/// this includes DateTime.now and Int.random.
/// </remarks>
type Previewable =
  /// The same inputs will always yield the same outputs,
  /// so we don't need to save results. e.g. `DateTime.addSeconds`
  | Pure

  /// Output may vary with the same inputs, though we can safely preview.
  /// e.g. `DateTime.now`. We should save the results.
  | ImpurePreviewable

  /// Can only be run on the server. e.g. `DB.update`
  /// We should save the results.
  | Impure


/// Used to mark whether a function has an equivalent that can be
/// used within a Sqlite query.
type SqlSpec =
  /// Can be implemented, but we haven't yet
  | NotYetImplemented

  /// This is not a function which can be queried
  | NotQueryable

  /// A query function (it can't be called inside a query, but its argument can be a query)
  | QueryFunction

  /// Can be implemented by a given builtin operator with 1 arg (eg `@ x`)
  | SqlUnaryOp of string

  /// Can be implemented by a given builtin operator with 2 args (eg `x + y`)
  | SqlBinOp of string

  /// Can be implemented by a given builtin function
  | SqlFunction of string

  /// Can be implemented by a given builtin function with extra arguments that go first
  | SqlFunctionWithPrefixArgs of string * List<string>

  /// Can be implemented by a given builtin function with extra arguments that go last
  | SqlFunctionWithSuffixArgs of string * List<string>

  /// Can be implemented by given callback that receives 1 SQLified-string argument
  /// | SqlCallback of (string -> string)
  /// Can be implemented by given callback that receives 2 SQLified-string argument
  | SqlCallback2 of (string -> string -> string)

  member this.isQueryable() : bool =
    match this with
    | NotYetImplemented
    | NotQueryable
    | QueryFunction -> false
    | SqlUnaryOp _
    | SqlBinOp _
    | SqlFunction _
    | SqlFunctionWithPrefixArgs _
    | SqlFunctionWithSuffixArgs _
    | SqlCallback2 _ -> true


module Tracing =
  /// Record the source expression of an error.
  /// This is to show the code that was responsible for it.
  /// TODO maybe rename to ExprLocation
  type Source = ExecutionPoint * Option<id>

  type FunctionRecord = Source * FQFnName.FQFnName

  type TraceDval = id -> Dval -> unit

  type TraceExecutionPoint = ExecutionPoint -> unit

  type LoadFnResult =
    FunctionRecord -> NEList<Dval> -> Option<Dval * NodaTime.Instant>

  type StoreFnResult = FunctionRecord -> NEList<Dval> -> Dval -> unit

  /// Set of callbacks used to trace the interpreter, and other context needed to run code
  type Tracing =
    { traceDval : TraceDval
      traceExecutionPoint : TraceExecutionPoint
      loadFnResult : LoadFnResult
      storeFnResult : StoreFnResult }


// -- The VM --
type Registers = Dval array

type CallFrame =
  {
    id : uuid

    /// (Id * where to put result in parent * pc of parent to return to)
    parent : Option<uuid * Register * int>

    // The instructions and resultReg are not in the CallFrame itself.
    // Multiple CFs may be operating on the same fn/lambda/etc.,
    // so we keep only one copy of such, in the root of the VMState
    executionPoint : ExecutionPoint

    /// What instruction index we are currently 'at'
    mutable programCounter : int

    mutable typeSymbolTable : TypeSymbolTable

    registers : Registers
  }

type InstrData =
  {
    instructions : Instruction array

    /// The register that the result of the block will be in
    resultReg : Register
  }

type VMState =
  { mutable threadID : uuid

    mutable callFrames : Map<uuid, CallFrame>
    mutable currentFrameID : uuid

    // The inst data for each fn/lambda/etc. is stored here, so that
    // it doesn't have to be copied into each CallFrame.
    rootInstrData : Option<tlid> * InstrData
    mutable lambdaInstrCache : Map<ExecutionPoint * id, LambdaImpl>
    mutable packageFnInstrCache : Map<FQFnName.Package, InstrData> }

  static member create(instrs : Option<tlid> * Instructions) : VMState =
    let tlid, instrs = instrs

    let rootCallFrameID = System.Guid.NewGuid()

    let rootCallFrame : CallFrame =
      { id = rootCallFrameID
        executionPoint = Source
        programCounter = 0
        registers = Array.zeroCreate instrs.registerCount
        typeSymbolTable = Map.empty
        parent = None }

    { threadID = System.Guid.NewGuid()
      currentFrameID = rootCallFrameID
      callFrames = Map [ rootCallFrameID, rootCallFrame ]
      rootInstrData =
        let instrs =
          { instructions = List.toArray instrs.instructions
            resultReg = instrs.resultIn }
        (tlid, instrs)
      lambdaInstrCache = Map.empty
      packageFnInstrCache = Map.empty }

  static member createWithoutTLID(instrs : Instructions) : VMState =
    VMState.create (None, instrs)

  static member creatWithTLID (tlid : tlid) (instrs : Instructions) : VMState =
    VMState.create (Some tlid, instrs)



// -- Builtins --
type BuiltInValue =
  { name : FQValueName.Builtin
    typ : TypeReference
    description : string
    deprecated : Deprecation<FQValueName.FQValueName>
    body : Dval }

/// A built-in standard library function
///
/// (Generally shouldn't be accessed directly,
/// except by a single stdlib Package fn that wraps it)
type BuiltInFn =
  { name : FQFnName.Builtin
    typeParams : List<string>
    parameters : List<BuiltInParam> // TODO: should be NEList but there's so much to change!
    returnType : TypeReference
    description : string
    previewable : Previewable
    deprecated : Deprecation<FQFnName.FQFnName>
    sqlSpec : SqlSpec
    fn : BuiltInFnSig }

and BuiltInFnSig =
  // (exeState * vmState * typeArgs * fnArgs) -> result
  (ExecutionState * VMState * List<TypeReference> * List<Dval>) -> DvalTask


/// Functionally written in F# and shipped with the executable
and Builtins =
  { values : Map<FQValueName.Builtin, BuiltInValue>
    fns : Map<FQFnName.Builtin, BuiltInFn> }





/// Every part of a user's program
/// CLEANUP rename to 'app' or 'canvas' or something
and Program =
  { canvasID : CanvasID
    internalFnsAllowed : bool
    dbs : Map<string, DB.T>
    secrets : List<Secret.T> }


// Used for testing
// CLEANUP maybe this belongs in Execution rather than RuntimeTypes?
// and taken out of ExecutionState, where it's not really used?
and TestContext =
  { mutable sideEffectCount : int

    mutable exceptionReports : List<string * string * Metadata>
    mutable expectedExceptionCount : int
    postTestExecutionHook : TestContext -> unit }


and ExceptionReporter = ExecutionState -> VMState -> Metadata -> exn -> Ply<unit>

and Notifier = ExecutionState -> VMState -> string -> Metadata -> Ply<unit>

/// All state set when starting an execution; non-changing
/// (as opposed to the VMState, which changes as the execution progresses)
and ExecutionState =
  { // -- Set consistently across a runtime --
    tracing : Tracing.Tracing
    test : TestContext

    /// Called to report exceptions
    reportException : ExceptionReporter

    /// Called to notify that something of interest (that isn't an exception)
    /// has happened.
    ///
    /// Useful for tracking behaviour we want to deprecate, understanding what
    /// users are doing, etc.
    notify : Notifier


    // -- Set per-execution --
    program : Program

    types : Types
    fns : Functions
    values : Values
  }


and Types = { package : FQTypeName.Package -> Ply<Option<PackageType.PackageType>> }

and Values =
  { builtIn : Map<FQValueName.Builtin, BuiltInValue>
    package : FQValueName.Package -> Ply<Option<PackageValue.PackageValue>> }

and Functions =
  { builtIn : Map<FQFnName.Builtin, BuiltInFn>
    package : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>> }



module Types =
  let empty = { package = (fun _ -> Ply None) }

  let find
    (types : Types)
    (name : FQTypeName.FQTypeName)
    : Ply<Option<TypeDeclaration.T>> =
    match name with
    | FQTypeName.Package pkg ->
      types.package pkg |> Ply.map (Option.map _.declaration)

  /// Swap concrete types for type parameters
  /// CLEANUP consider accepting a pre-zipped list instead
  let rec substitute
    (typeParams : List<string>)
    (typeArguments : List<TypeReference>)
    (typ : TypeReference)
    : TypeReference =
    let r = substitute typeParams typeArguments
    match typ with
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
    | TDateTime -> typ

    | TTuple(t1, t2, rest) -> TTuple(r t1, r t2, List.map r rest)
    | TList t -> TList(r t)
    | TDict t -> TDict(r t)

    | TCustomType(typ, args) -> TCustomType(typ, List.map r args)

    | TFn(args, ret) -> TFn(NEList.map r args, r ret)
    | TDB inner -> TDB(r inner)

    | TVariable v ->
      if typeParams.Length = typeArguments.Length then
        List.zip typeParams typeArguments
        |> List.find (fun (param, _) -> param = v)
        |> Option.map snd
        |> Exception.unwrapOptionInternal
          "No type argument found for type parameter"
          []
      else
        Exception.raiseInternal
          $"typeParams and typeArguments have different lengths"
          [ "typeParams", typeParams; "typeArguments", typeArguments ]


module TypeReference =
  let result (t1 : TypeReference) (t2 : TypeReference) : TypeReference =
    TCustomType(Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.result), [ t1; t2 ])

  let option (t : TypeReference) : TypeReference =
    TCustomType(Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.option), [ t ])

  let rec unwrapAlias (types : Types) (typ : TypeReference) : Ply<TypeReference> =
    match typ with
    | TCustomType(Ok outerTypeName, outerTypeArgs) ->
      uply {
        match! Types.find types outerTypeName with
        | Some { definition = TypeDeclaration.Alias typ; typeParams = typeParams } ->
          let typ = Types.substitute typeParams outerTypeArgs typ
          return! unwrapAlias types typ
        | _ -> return typ
      }
    | _ -> Ply typ


  let rec toVT
    (types : Types)
    (tst : TypeSymbolTable)
    (typeRef : TypeReference)
    : Ply<ValueType> =
    let r = toVT types tst

    uply {
      match! unwrapAlias types typeRef with
      | TUnit -> return ValueType.Known KTUnit
      | TBool -> return ValueType.Known KTBool
      | TInt8 -> return ValueType.Known KTInt8
      | TUInt8 -> return ValueType.Known KTUInt8
      | TInt16 -> return ValueType.Known KTInt16
      | TUInt16 -> return ValueType.Known KTUInt16
      | TInt32 -> return ValueType.Known KTInt32
      | TUInt32 -> return ValueType.Known KTUInt32
      | TInt64 -> return ValueType.Known KTInt64
      | TUInt64 -> return ValueType.Known KTUInt64
      | TInt128 -> return ValueType.Known KTInt128
      | TUInt128 -> return ValueType.Known KTUInt128
      | TFloat -> return ValueType.Known KTFloat
      | TChar -> return ValueType.Known KTChar
      | TString -> return ValueType.Known KTString
      | TUuid -> return ValueType.Known KTUuid
      | TDateTime -> return ValueType.Known KTDateTime

      | TTuple(first, second, theRest) ->
        let! first = r first
        let! second = r second
        let! theRest = theRest |> Ply.List.mapSequentially r
        return KTTuple(first, second, theRest) |> ValueType.Known
      | TList inner ->
        let! inner = r inner
        return ValueType.Known(KTList inner)
      | TDict inner ->
        let! inner = r inner
        return ValueType.Known(KTDict inner)

      | TCustomType(Ok typeName, typeArgs) ->
        let! typeArgs = typeArgs |> Ply.List.mapSequentially r
        return KTCustomType(typeName, typeArgs) |> ValueType.Known

      | TCustomType(Error nre, _) ->
        return raiseUntargetedRTE (RuntimeError.ParseTimeNameResolution nre)

      | TVariable name ->
        return tst |> Map.get name |> Option.defaultValue ValueType.Unknown

      | TFn(args, result) ->
        let! args = args |> Ply.NEList.mapSequentially r
        let! result = r result
        return KTFn(args, result) |> ValueType.Known

      | TDB inner ->
        let! inner = r inner
        return ValueType.Known(KTDB inner)
    }



let consoleReporter : ExceptionReporter =
  fun _state _vm (metadata : Metadata) (exn : exn) ->
    uply { printException "runtime-error" metadata exn }

let consoleNotifier : Notifier =
  fun _state _vm msg tags ->
    uply { print $"A notification happened in the runtime:\n  {msg}\n  {tags}\n\n" }
