/// The core types and functions used by the Dark language's runtime.
///
/// If you need to save data of this format, create a set of new
/// types and convert this type into them. (even if they are identical).
///
/// This format is lossy, relative to the serialized types; use IDs to refer back.
module LibExecution.RuntimeTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let builtinNamePattern = @"^(__|[a-z])[a-z0-9A-Z_]\w*$"
let constantNamePattern = @"^[a-z][a-z0-9A-Z_']*$"

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
  /// The id of a type in the package manager
  type Package = uuid

  type FQTypeName = Package of Package

  let package (id : uuid) : Package = id

  let fqPackage (id : uuid) : FQTypeName = Package id


/// A Fully-Qualified Constant Name
///
/// Used to reference a constant defined by the runtime or in a Package
module FQConstantName =
  /// A constant built into the runtime
  type Builtin = { name : string; version : int }

  /// The id of a constant in the package manager
  type Package = uuid

  type FQConstantName =
    | Builtin of Builtin
    | Package of Package

  let assertConstantName (name : string) : unit =
    assertRe "Constant name must match" constantNamePattern name

  let builtin (name : string) (version : int) : Builtin =
    assertBuiltin name version assertConstantName
    { name = name; version = version }

  let package (id : uuid) : Package = id

  let fqPackage (id : uuid) : FQConstantName = Package id

  let builtinToString (s : Builtin) : string =
    let name = s.name
    if s.version = 0 then name else $"{name}_v{s.version}"


/// A Fully-Qualified Function Name
///
/// Used to reference a function defined by the runtime or in a Package
module FQFnName =
  /// A function built into the runtime
  type Builtin = { name : string; version : int }

  type Package = uuid

  type FQFnName =
    | Builtin of Builtin
    | Package of Package

  let assertBuiltinFnName (name : string) : unit =
    assertRe $"Fn name must match" builtinNamePattern name

  let builtin (name : string) (version : int) : Builtin =
    assertBuiltin name version assertBuiltinFnName
    { name = name; version = version }

  let package (id : uuid) = id

  let fqBuiltin (name : string) (version : int) : FQFnName =
    Builtin { name = name; version = version }

  let fqPackage (id : uuid) : FQFnName = Package id


  let isInternalFn (fnName : Builtin) : bool = fnName.name.Contains "darkInternal"


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

  // /// let f = (fun x -> x)        // KTFn([Unknown], Unknown)
  // /// let intF = (fun (x: Int) -> x) // KTFn([Known KTInt64], Unknown)
  // ///
  // /// Note that we could theoretically know some return types by analyzing the
  // /// code or type signatures of functions. We don't do this yet as it's
  // /// complicated. When we do decide to do this, some incorrect programs may stop
  // /// functioning (see example). Our goal is for correctly typed functions to
  // /// stay working so this might be ok.
  // ///
  // /// For example:
  // ///   let z1 = (fun x -> 5)
  // ///   let z2 = (fun x -> "str")
  // /// `[z1, z2]` is allowed now but might not be allowed later
  // | KTFn of args : NEList<ValueType> * ret : ValueType

  // /// At time of writing, all DBs are of a specific type, and DBs may only be
  // /// referenced directly, but we expect to eventually allow references to DBs
  // /// where the type may be unknown
  // /// List.head ([]: List<DB<'a>>) // KTDB (Unknown)
  // | KTDB of ValueType

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
  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TFn of NEList<TypeReference> * TypeReference
  // | TDB of TypeReference
  | TVariable of string
  | TCustomType of
    NameResolution<FQTypeName.FQTypeName> *
    typeArgs : List<TypeReference>
  | TDict of TypeReference // CLEANUP add key type

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

      | TList t -> isConcrete t
      | TTuple(t1, t2, ts) ->
        isConcrete t1 && isConcrete t2 && List.forall isConcrete ts
      | TFn(ts, t) -> NEList.forall isConcrete ts && isConcrete t
      // | TDB t -> isConcrete t
      | TCustomType(_, ts) -> List.forall isConcrete ts
      | TDict t -> isConcrete t

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
type TypeSymbolTable = Map<string, TypeReference>



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
  | MPVariable of Register


type StringSegment =
  | Text of string
  | Interpolated of Register


type Instruction =
  // == Simple register operations ==
  /// Push a ("constant") value into a register
  | LoadVal of loadTo : Register * Dval

  | CopyVal of copyTo : Register * copyFrom : Register

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
  /// TODO maybe make this a special case of Fail
  | MatchUnmatched


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

  // | CloneRecordWithUpdates of
  //   createTo : Register *
  //   originalRecordReg : Register *
  //   updates : List<string * Register>

  | GetRecordField of
    // todo: rename to "lhs"? Look into this.
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


  // == Working with things that Apply ==

  // | CreateLambda of createTo : Register * lambda : LambdaImpl

  /// Apply some args (and maybe type args) to something
  /// (a named function, or lambda, etc)
  | Apply of
    createTo : Register *
    thingToApply : Register *
    typeArgs : List<TypeReference> *
    args : NEList<Register>

  // == Errors ==
  | RaiseNRE of NameResolutionError

  | VarNotFound of name : string


/// (rc, instructions, result register)
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


(*
let

*)

/// Lambdas are a bit special:
/// they have to close over variables, and have their own set of instructions, not embedded in the main set
///
/// Note to self: trying to remove typeSymbolTable here
/// causes all sorts of scoping issues. Beware.
and LambdaImpl =
  {
    // -- Things we know as soon as we create the lambda --
    // maybe we need the TL ID as well?
    exprId : id

    /// How should the arguments be deconstructed?
    ///
    /// When we've received as many args as there are patterns,
    /// we should either apply the lambda, or error.
    patterns : NEList<LetPattern> // LPVar 1

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
    /// symbtable for the evaluation of the expr on the RHS
    registersToClose : List<Register * Register>

    // Hmm do these actually belong here, or somewhere else? idk how we get this to work.
    // do we need to call eval within eval or something? would love to avoid that.
    // if so, we might need a pc or something to keep track of where we are in the 'above' instructions
    instructions : Instructions
  }



and ApplicableNamedFn =
  {
    name : FQFnName.FQFnName

    // /// We need these around, even for a partially-applied fn
    // /// , to make sure we can fail on type errors appropriately
    // /// e.g. `Int64.add true` should fail before a second arg is provided.
    // parameters : NEList<Param>

    /// CLEANUP should this be a list of registers instead?
    argsSoFar : List<Dval>
  }

// if we're just evaluating a "raw expr," I suppose that's InputClosure?
// eval probably handles whichever of these,
// with a fn above that to coordinate things?
and ApplicableLambda =
  {
    /// The lambda's ID, corresponding to the PT.Expr
    /// (the actual implementation is stored in the VMState)
    exprId : id

    // we need registers here, right?


    // /// The symtable at the time of creation (only copy what's noted in `symbolsToClose`)
    // /// , along with anything created throughout processing so far
    // symtable : Symtable

    // TODO: typeSymbolTable : TypeSymbolTable

    argsSoFar : List<Dval>
  }

// // Is this a _kind_ of closure? I think so!
// // So do we need
// //
// and CallFrameReference =
//   | InputExpr
//   | TopLevel of tlid
//   | NamedFn of FQFnName.FQFnName
//   | Lambda of id

// /// TODO VMState holds a Map of these? and we fetch by ID?
// /// specifically `Map<CallFrameReference, CallFrame>`? idk.
// ///
// /// Any of these things can be applied, and _somewhere_ have a set of instructions to evaluate
// and CallFrame =
//   /// for raw exprs, e.g. tests, one-off scripts, etc
//   ///
//   /// Thinking...
//   /// I think this one actually needs its Instructions -- and maybe registers and such?
//   /// This might be where more of VMState gets moved to.
//   | InputExpr of parent : Option<CallFrameReference> * Instructions

//   /// TODO probably good to 'migrate' some usages from "raw" expr evaluation to these.
//   | TopLevel of parent : Option<CallFrameReference> * tlid

//   | NamedFn of parent : Option<CallFrameReference> * ApplicableNamedFn

//   /// Note: the impl details are stored "centrally" in the VMState
//   /// in a LambdaImpl object, after being loaded by a LoadLambda instruction
//   | Lambda of parent : CallFrameReference * ApplicableLambda


/// Any thing that can be applied,
/// along with anything needed within their application closure
/// TODO: follow up with typeSymbols
/// TODO needs a better name, clearly.
and Applicable =
  //| Lambda of ApplicableLambda

  | NamedFn of ApplicableNamedFn




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
    // CLEANUP we may need a sourceTypeArgs here as well
    sourceTypeName : FQTypeName.FQTypeName *
    runtimeTypeName : FQTypeName.FQTypeName *
    // do we need to split this into sourceTypeArgs and runtimeTypeArgs?
    // What are we even using the source stuff for? error-reporting?
    typeArgs : List<ValueType> *
    fields : DvalMap // would a list be better? We can do the type-check fun _after_
  // field access would be a tad slower, but there usually aren't that many fields
  // and it's probably more convenient?
  // Hmm for dicts, we could consider the same thing, but field-access perf is
  // more important there.

  | DEnum of
    // CLEANUP we may need a sourceTypeArgs here as well
    sourceTypeName : FQTypeName.FQTypeName *
    runtimeTypeName : FQTypeName.FQTypeName *
    typeArgs : List<ValueType> *  // same q here - split into sourceTypeArgs and runtimeTypeArgs?
    caseName : string *
    fields : List<Dval>

  | DApplicable of Applicable

// // References
// | DDB of name : string



and DvalTask = Ply<Dval>

// /// Our record/tracking of any variable bindings in scope
// ///
// /// i.e. within the execution of `x+y` in
// ///  `let x = 1; let y = 2; x + y`
// /// , we would have a Symtable of
// ///   `{ "x" => DInt64 1; "y" => DInt64 2 }`
// and Symtable = Map<string, Dval>


and ExecutionPoint =
  /// User is executing some "arbitrary" expression, passed in by a user.
  ///
  /// This should only be at the `entrypoint` of a CallStack.
  ///   (CLEANUP consider enforcing this via types)
  | Script //TODO of name: string

  /// Executing some top-level handler,
  /// such as a saved Script, an HTTP handler, or a Cron
  | Toplevel of tlid

  // Executing some function
  | Function of FQFnName.FQFnName

// Executing some lambda
//| Lambda of parent: ExecutionPoint * exprId: id

/// Record the source expression of an error.
/// This is to show the code that was responsible for it.
/// TODO maybe rename to ExprLocation
and Source = ExecutionPoint * Option<id>

// and CallStack =
//   {
//     /// The entrypoint of the execution
//     /// (whatever we're executing for a user)
//     entrypoint : ExecutionPoint

//     // TODO: bring this back and do something with it,
//     // and improve it to be more useful
//     // (i.e. maintain order of calls, deal with recursions, etc.)
//     // See https://chatgpt.com/share/087935f9-44be-4686-8209-66e701e887b1
//     // /// All of the fns that have been called in this execution
//     // calledFns : Set<FQFnName.FQFnName>

//     /// The last-called thing (roughly)
//     ///
//     /// If we've encountered an exception, this should be where things failed
//     lastCalled : Source
//   }

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

and Param = { name : string; typ : TypeReference }


// TODO really consider making this extensible without requiring a rebuild
// (maybe reframe this as an `Exception`, maybe some new TL that users can add to)
module RuntimeError =
  module TypeChecker =
    // TODO: move this somewhere..
    type Context =
      | FunctionCallParameter of
        fnName : FQFnName.FQFnName *
        parameter : Param *
        paramIndex : int
      | FunctionCallResult of fnName : FQFnName.FQFnName * returnType : TypeReference
      | RecordField of
        recordTypeName : FQTypeName.FQTypeName *
        fieldName : string *
        fieldType : TypeReference
      | DictKey of key : string * typ : TypeReference
      | EnumField of
        enumTypeName : FQTypeName.FQTypeName *
        caseName : string *
        fieldIndex : int *
        fieldCount : int *
        fieldType : TypeReference
      | DBQueryVariable of varName : string * expected : TypeReference
      | DBSchemaType of name : string * expectedType : TypeReference
      | ListIndex of index : int * listTyp : TypeReference * parent : Context
      | TupleIndex of index : int * elementType : TypeReference * parent : Context
      | FnValResult of returnType : TypeReference


  // module Cli =
  //   type Error =
  //     | NoExpressionsToExecute
  //     | UncaughtException of String * List<String * String>
  //     | NonIntReturned of actuallyReturned: Dval.Dval


  // module Json =
  //   type Error = UnsupportedType of RuntimeTypes.TypeReference


  module Ints =
    type Error =
      | DivideByZeroError
      | OutOfRange
      | NegativeExponent
      | NegativeModulus
      | ZeroModulus



  // module Execution =
  //   type Error =
  //     | MatchExprUnmatched of RuntimeTypes.Dval.Dval
  //     | NonStringInStringInterpolation of RuntimeTypes.Dval.Dval
  //     | ConstDoesntExist of RuntimeTypes.FQConstantName.FQConstantName
  //     | EnumConstructionCaseNotFound of typeName: RuntimeTypes.FQTypeName * caseName: String
  //

  //     // TODO: Record submodule
  //     | RecordConstructionFieldDoesntExist of typeName: RuntimeTypes.FQTypeName * fieldName: String
  //     | RecordConstructionMissingField of RuntimeTypes.FQTypeName * missingFieldName: String
  //     | RecordConstructionDuplicateField of RuntimeTypes.FQTypeName * duplicateFieldName: String
  //     | FieldAccessFieldDoesntExist of typeName: RuntimeTypes.FQTypeName * invalidFieldName: String
  //     | FieldAccessNotRecord of RuntimeTypes.ValueType * String

  // module Unwrap =
  //   type Error =
  //     | GotNone
  //     | GotError of Dval
  //     | NonOptionOrResult of Dval


  module Lets =
    // CLEANUP consider some kinda _path_ thing like with JSON errors
    // type Details =
    //   /// Unit pattern does not match
    //   | UnitPatternDoesNotMatch

    //   /// Tuple pattern does not match
    //   | TuplePatternDoesNotMatch

    //   /// Tuple pattern has wrong number of elements
    //   | TuplePatternWrongLength of expected: Int * actual: Int

    type Error =
      /// Could not decompose `{someFn dval}` with pattern `{someFn pat}` in `let` expression
      | PatternDoesNotMatch of dval : Dval * pat : LetPattern

  // module Enum =
  //   type Error =
  //     /// $"When constructing enum value `typeName`.`{caseName}`,
  //     /// expected {expectedFieldCount} fields but got {actualFieldCount}"
  //     | WrongNumberOfFields of typeName * FQTypeName * caseName: String * expectedFieldCount: Int * actualFieldCount: Int

  module Bools =
    type Error =
      // | AndOnlySupportsBooleans of gotInstead: Dval
      // | OrOnlySupportsBooleans of gotInstead: Dval
      | ConditionRequiresBool of actualValueType : ValueType * actualValue : Dval

  module Strings =
    type Error =
      // "Error: Invalid string-append attempt"
      | InvalidStringAppend


  module Lists =
    type Error =
      /// Cannot add a {} ({}) to a list of {}
      | TriedToAddMismatchedData of
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

  // CLEANUP same here^
  module Dicts =
    type Error =
      | TriedToAddKeyAfterAlreadyPresent of key : string

      /// Cannot add a {} ({}) to a dict of {}
      | TriedToAddMismatchedData of
        expectedType : ValueType *
        actualType : ValueType *
        actualValue : Dval

  module Records =
    type Error =
      | CreationEmptyKey
      | CreationMissingField of fieldName : string
      | CreationDuplicateField of fieldName : string
      | CreationFieldOfWrongType of
        fieldName : string *
        expectedType : TypeReference *
        actualType : ValueType

      | FieldAccessFieldNotFound of fieldName : string
      | FieldAccessNotRecord of actualType : ValueType



  /// RuntimeError is the major way of representing errors in the runtime. These are
  /// primarily used for things where the user made an error, such as a type error,
  /// as opposed to a place where the runtime is flawed (use Exception.raiseInternal
  /// for those). See docs/errors.md for detailed discussion.
  /// CLEANUP rewrite this^
  ///
  ///
  /// TODO: this needs a way to be extensible
  /// users should have _some_ way to add their own RuntimeErrors
  /// and we don't want to have to rebuild everything to add a new RTE
  type Error =
    | TypeDoesntExist of FQTypeName.FQTypeName

    | NameResolution of NameResolutionError

    | Bool of Bools.Error
    | Int of Ints.Error
    //| Json of Json.Error
    | String of Strings.Error
    | List of Lists.Error
    | Dict of Dicts.Error
    | Record of Records.Error
    | Let of Lets.Error
    // | Enum of Enum.Error

    | MatchUnmatched

    // TODO consider currying instead, at which point a `Int.add 0 1 2` would result in "can't apply 2 to 1"
    | TooManyArgs of
      fn : FQFnName.FQFnName *
      expectedTypeArgs : int64 *
      expectedArgs : int64 *
      actualTypeArgs : int64 *
      actualArgs : int64


    // /// "The condition for an `if` expression must be a `Bool`,
    // /// but is here a `{someFn actualValueType}` (`{someFn actualValue}`)"
    // | IfConditionNotBool of actualValue: Dval * actualValueType: ValueType

    // | Unwrap of Unwrap.Error

    | EqualityCheckOnIncompatibleTypes of left : ValueType * right : ValueType

    | ValueNotExpectedType of
      actualValue : Dval *
      expectedType : TypeReference *
      context : TypeChecker.Context

    // | ExpectedBoolInCondition of Dval
    | VariableNotFound of attemptedVarName : string


    // //| SqlCompiler of SqlCompiler.Error // -- or maybe this should happen during PT2RT? hmm.

    // // lol aren't they all execution errors?
    // // remove this level...
    // | Execution of Execution.Error


    // /// || only supports Booleans
    // | OrOnlySupportsBooleans of gotInstead: Dval

    // /// && only supports Booleans
    // | AndOnlySupportsBooleans of gotInstead: Dval


    // //| Cli of Cli.Error


    // TODO

    // backend/src/BuiltinExecution/Libs/NoModule.fs:
    // - $"unwrap called with multiple arguments: {multipleArgs}"


    // backend/src/LibCloud/SqlCompiler.fs:
    // 1223: | SqlCompilerException errStr -> return Error(RuntimeError.oldError errStr)
    // 1224: // return Error(RuntimeError.oldError (errStr + $"\n\nIn body: {body}"))


    // backend/src/LibExecution/Interpreter.fs:
    // - "TODO"

    /// $"Function {FQFnName.toString fnName} is not found"
    | FnNotFound of fnName : FQFnName.FQFnName

    // backend/src/LibExecution/Interpreter.Old.fs:
    // - "TODO"
    // - $"Invalid const name: {msg}"
    // - $"Expected {expectedLength} arguments, got {actualLength}"
    // - $"Function {FQFnName.toString fnToCall} is not found")
    // - "Unknown error"


    // backend/src/LibExecution/NameResolutionError.fs:
    // - "TODO"


    // backend/src/LibExecution/ProgramTypesToRuntimeTypes.fs:
    // - "Couldn't find fn"
    // - "Record must have at least one field"
    // - "Match must have at least one case"


    // backend/src/LibExecution/TypeChecker.fs:

    //$"Could not merge types {ValueType.toString (VT.customType typeName [ innerType ])} and {ValueType.toString (VT.customType typeName [ dvalType ])}"
    | CannotMergeValues of left : ValueType * right : ValueType
    // - $"Could not merge types {ValueType.toString (VT.list typ)} and {ValueType.toString (VT.list dvalType)}"
    // - $"Could not merge types {ValueType.toString (VT.customType typeName [ innerType ])} and {ValueType.toString (VT.customType typeName [ dvalType ])}"
    // - $"Could not merge types {ValueType.toString (VT.customType Dval.resultType [ okType; errorType ])} and {ValueType.toString (VT.customType Dval.resultType [ dvalType; errorType ])}"
    // - $"Could not merge types {ValueType.toString (VT.customType Dval.resultType [ okType; errorType ])} and {ValueType.toString (VT.customType Dval.resultType [ okType; dvalType ])}"
    // - "Empty key"
    // - $"Duplicate key: {k}"


    // backend/tests/TestUtils/LibTest.fs:
    // - update `Builtin.testRuntimeError` to take an `RTE` value instead of a string
    // - update all usages


    // backend/src/LibCloud/SqlCompiler.fs:
    // 1223: | SqlCompilerException errStr -> return Error(RuntimeError.oldError errStr)
    // 1224: // return Error(RuntimeError.oldError (errStr + $"\n\nIn body: {body}"))


    // /home/dark/app/backend/src/LibExecution/Interpreter.Old.fs
    // - $"Empty key for value `{dv}`"
    // - "Expected a record in record update"
    // - "Field name is empty"
    // - "When condition should be a boolean" -- this _could_ warn _or_ error. which?
    // - $"Expected a record but {typeStr} is something else"

    /// "Expected a function value, got something else: {DvalReprDeveloper.toRepr other}"
    | ExpectedApplicableButNot of actualTyp : ValueType * actualValue : Dval

    // - "Attempting to access field '{fieldName}' of a Datastore
    // (use `DB.*` standard library functions to interact with Datastores. Field access only work with records)"

    // incorrectArgs



    /// Sometimes, very-unexpected things happen. This is a catch-all for those.
    /// For local/private runtimes+hosting, allow users to see the details,
    /// but for _our_ hosting, users shouldn't see the whole call stack or
    /// whatever, for (our) safety. But, they can use the error ID to refer to
    /// the error in a support ticket.
    | UncaughtException of reference : uuid



// module CallStack =
//   let fromEntryPoint (entrypoint : ExecutionPoint) : CallStack =
//     { entrypoint = entrypoint; lastCalled = (entrypoint, None) }

module TypeReference =
  let result (t1 : TypeReference) (t2 : TypeReference) : TypeReference =
    TCustomType(Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.result), [ t1; t2 ])

  let option (t : TypeReference) : TypeReference =
    TCustomType(Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.option), [ t ])




/// Note: in cases where it's awkward to niclude a CallStack,
/// the Interpreter should try to inject it where it can
///
/// CLEANUP: ideally, the CallStack isn't required as part of the exception.
/// This would clean up a _lot_ of code.
/// The tricky part is that we do want the CallStack around, to report on,
/// and to use for debugging, but the way the Interpreter+Execution is set up,
/// there's no great single place to `try/with` to supply the call stack.
exception RuntimeErrorException of ThreadID * rte : RuntimeError.Error


let raiseRTE (threadId : ThreadID) (rte : RuntimeError.Error) : 'a =
  raise (RuntimeErrorException(threadId, rte))

// let raiseRTE (callStack : CallStack) (rte : RuntimeError) : 'a =
//   raise (RuntimeErrorException(Some callStack, rte))

// // (only?) OK in builtins because we "fill in" the callstack in the Interpreter for such failures
// // CLEANUP maybe (somehow) restrict to only Builtins
// let raiseUntargetedRTE (rte : RuntimeError) : 'a =
//   raise (RuntimeErrorException(None, rte))


/// Internally in the runtime, we allow throwing RuntimeErrorExceptions. At the
/// boundary, typically in Execution.fs, we will catch the exception, and return
/// this type.
/// TODO return a call stack or vmstate, or something, here
type ExecutionResult = Result<Dval, RuntimeError.Error>

/// IncorrectArgs should never happen, as all functions are type-checked before
/// calling. If it does happen, it means that the type parameters in the Fn structure
/// do not match the args expected in the F# function definition.
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
  | DeprecatedBecause of string


module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference }

  //type EnumCase = { name : string; fields : List<TypeReference> }

  type Definition =
    | Alias of TypeReference
    | Record of NEList<RecordField>
  //| Enum of NEList<EnumCase>

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

    | DRecord(typeName, _, typeArgs, _) ->
      KTCustomType(typeName, typeArgs) |> ValueType.Known

    | DEnum(typeName, _, typeArgs, _, _) ->
      KTCustomType(typeName, typeArgs) |> ValueType.Known

    | DApplicable applicable ->
      match applicable with
      // | Lambda _lambda ->
      //   // KTFn(
      //   //   NEList.map (fun _ -> ValueType.Unknown) lambda.parameters,
      //   //   ValueType.Unknown
      //   // )
      //   // |> ValueType.Known
      //   ValueType.Unknown

      // VTTODO look up type, etc
      | NamedFn _named -> ValueType.Unknown

// // CLEANUP follow up when DDB has a typeReference
// | DDB _ -> ValueType.Unknown




type Const =
  | CUnit
  | CBool of bool

  | CInt8 of int8
  | CUInt8 of uint8
  | CInt16 of int16
  | CUInt16 of uint16
  | CInt32 of int32
  | CUInt32 of uint32
  | CInt64 of int64
  | CUInt64 of uint64
  | CInt128 of System.Int128
  | CUInt128 of System.UInt128

  | CFloat of Sign * string * string

  | CChar of string
  | CString of string

  | CList of List<Const>
  | CTuple of first : Const * second : Const * rest : List<Const>
  | CDict of List<string * Const>

  | CEnum of NameResolution<FQTypeName.FQTypeName> * caseName : string * List<Const>



// ------------
// Package-Space
// ------------
module PackageType =
  // TODO: hash
  type PackageType = { id : uuid; declaration : TypeDeclaration.T }

module PackageConstant =
  // TODO: hash
  type PackageConstant = { id : uuid; body : Const }

module PackageFn =
  type Parameter = { name : string; typ : TypeReference }

  // TODO: hash
  type PackageFn =
    { id : uuid
      typeParams : List<string>

      // CLEANUP I have an odd suspicion we might not need this field
      // Maybe we just need a paramCount, and the Instructinos in PT2RT ????
      parameters : NEList<Parameter>
      returnType : TypeReference

      // CLEANUP consider renaming - just `instructions` maybe?
      body : Instructions }


// // ------------
// // User-/Canvas- Space
// // ------------
// module DB =
//   type T = { tlid : tlid; name : string; typ : TypeReference; version : int }

// module Secret =
//   type T = { name : string; value : string; version : int }



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
/// used within a Postgres query.
type SqlSpec =
  /// Can be implemented, but we haven't yet
  | NotYetImplemented

  /// This is not a function which can be queried
  | NotQueryable

  /// A query function (it can't be called inside a query, but its argument can be a query)
  | QueryFunction

  /// Can be implemented by a given builtin postgres 9.6 operator with 1 arg (eg `@ x`)
  | SqlUnaryOp of string

  /// Can be implemented by a given builtin postgres 9.6 operator with 2 args (eg `x + y`)
  | SqlBinOp of string

  /// Can be implemented by a given builtin postgres 9.6 function
  | SqlFunction of string

  /// Can be implemented by a given builtin postgres 9.6 function with extra arguments that go first
  | SqlFunctionWithPrefixArgs of string * List<string>

  /// Can be implemented by a given builtin postgres 9.6 function with extra arguments that go last
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


type BuiltInConstant =
  { name : FQConstantName.Builtin
    typ : TypeReference
    description : string
    deprecated : Deprecation<FQConstantName.FQConstantName>
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

and Fn =
  {
    name : FQFnName.FQFnName
    typeParams : List<string>
    parameters : NEList<Param>
    returnType : TypeReference
    previewable : Previewable
    sqlSpec : SqlSpec

    /// <remarks>
    /// May throw an exception, though we're trying to get them to never throw exceptions.
    /// </remarks>
    fn : FnImpl
  }

and BuiltInFnSig =
  // exeState * vmState * typeArgs * fnArgs -> result
  // CLEANUP this is sort of a _lot_ to pass into every builtin fn call - reduce?
  (ExecutionState * VMState * List<TypeReference> * List<Dval>) -> DvalTask

and FnImpl =
  | BuiltInFunction of BuiltInFnSig
  | PackageFunction of FQFnName.Package * Instructions //* localCount: int


and FunctionRecord = Source * FQFnName.FQFnName

and TraceDval = id -> Dval -> unit

and TraceExecutionPoint = ExecutionPoint -> unit

// why do we need the Dvals here? those are the args, right - do we really need them?
// ah, because we could call the same fn twice, from the same place, but with different args. hmm.
and LoadFnResult = FunctionRecord -> NEList<Dval> -> Option<Dval * NodaTime.Instant>

and StoreFnResult = FunctionRecord -> NEList<Dval> -> Dval -> unit

/// Every part of a user's program
/// CLEANUP rename to 'app'?
and Program =
  { canvasID : CanvasID
    internalFnsAllowed : bool
  //dbs : Map<string, DB.T>
  //secrets : List<Secret.T>
  }

/// Set of callbacks used to trace the interpreter, and other context needed to run code
and Tracing =
  { traceDval : TraceDval
    traceExecutionPoint : TraceExecutionPoint
    loadFnResult : LoadFnResult
    storeFnResult : StoreFnResult }

// Used for testing
// TODO: maybe this belongs in Execution rather than RuntimeTypes?
// and taken out of ExecutionState, where it's not really used?
and TestContext =
  { mutable sideEffectCount : int

    mutable exceptionReports : List<string * string * Metadata>
    mutable expectedExceptionCount : int
    postTestExecutionHook : TestContext -> unit }

/// Functionally written in F# and shipped with the executable
and Builtins =
  { constants : Map<FQConstantName.Builtin, BuiltInConstant>
    fns : Map<FQFnName.Builtin, BuiltInFn> }

/// Functionality written in Dark stored and managed outside of user space
///
/// Note: it may be tempting to think these shouldn't return Options,
/// but if/when Package items may live (for some time) only on local systems,
/// there's a chance some code will be committed, referencing something
/// not yet in the Cloud PM.
/// (though, we'll likely demand deps. in the PM before committing something upstream...)
and PackageManager =
  { getType : FQTypeName.Package -> Ply<Option<PackageType.PackageType>>
    getConstant :
      FQConstantName.Package -> Ply<Option<PackageConstant.PackageConstant>>
    getFn : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>>

    init : Ply<unit> }

  static member empty =
    { getType = (fun _ -> Ply None)
      getFn = (fun _ -> Ply None)
      getConstant = (fun _ -> Ply None)

      init = uply { return () } }

  /// Allows you to side-load a few 'extras' in-memory, along
  /// the normal fetching functionality. (Mostly helpful for tests)
  static member withExtras
    (types : List<PackageType.PackageType>)
    (constants : List<PackageConstant.PackageConstant>)
    (fns : List<PackageFn.PackageFn>)
    (pm : PackageManager)
    : PackageManager =
    { getType =
        fun id ->
          match types |> List.tryFind (fun t -> t.id = id) with
          | Some t -> Some t |> Ply
          | None -> pm.getType id
      getConstant =
        fun id ->
          match constants |> List.tryFind (fun c -> c.id = id) with
          | Some c -> Some c |> Ply
          | None -> pm.getConstant id
      getFn =
        fun id ->
          match fns |> List.tryFind (fun f -> f.id = id) with
          | Some f -> Some f |> Ply
          | None -> pm.getFn id
      init = pm.init }

and ExceptionReporter = ExecutionState -> Metadata -> exn -> unit

and Notifier = ExecutionState -> string -> Metadata -> unit

/// All state used while running a program
and ExecutionState =
  { // -- Set consistently across a runtime --
    tracing : Tracing
    test : TestContext

    /// Called to report exceptions
    reportException : ExceptionReporter

    /// Called to notify that something of interest (that isn't an exception)
    /// has happened.
    ///
    /// Useful for tracking behaviour we want to deprecate, understanding what
    /// users are doing, etc.
    notify : Notifier

    // -- Set at the start of an execution --
    program : Program // TODO: rename to Canvas?


    // -- Can change over time during execution --
    // (probably move these things to VMState)

    // // Maybe replace this and `builtins` with availTypes, availConsts, availFns?
    // // We're doing some ExecutionState -> (those) mappings at runtime on occasion,
    // // probably a lot more than we need
    // packageManager : PackageManager
    // builtins : Builtins

    types : Types
    fns : Functions
    constants : Constants
  }

and Registers = Dval array

and CallFrameContext =
  | Source // from raw expr (for test) or TopLevel
  | PackageFn of FQFnName.Package
//| Lambda of parent : CallFrameContext * id

// all package fn applications (reaching into the interpreter)
// - should be done with eApply with w/e args
//   `eApply (fn, ???)`
//

(*
let incr a =
  Int.add 1 a

let add3 a =
  fun b -> add a b
*)

and CallFrame =
  {
    id : uuid

    parent : Option<uuid>

    // TODO the instructions and resultReg are not in the CallFrame itself
    // -- multiple CFs may be operating on the same fn or w/e
    // so we keep only one copy of such, in the root of the VMState
    context : CallFrameContext

    /// Program counter (what instruction index we are currently 'at')
    mutable pc : int

    registers : Registers // mutable because array?
  }

and Something =
  {
    instructions : Instruction array

    /// The register that the result of the block will be in
    resultReg : Register
  }


and VMState =
  { mutable threadID : uuid

    callFrames : Map<uuid, CallFrame>
    currentFrameID : uuid

    sourceInfo : Something
    //mutable lambdas : Map<id, LambdaImpl>
    mutable packageFns : Map<FQFnName.Package, Something> }

  static member fromExpr(expr : Instructions) : VMState =
    let callFrameId = System.Guid.NewGuid()

    let callFrame : CallFrame =
      { id = callFrameId
        context = Source
        pc = 0
        registers = Array.zeroCreate expr.registerCount
        parent = None }

    { threadID = System.Guid.NewGuid()
      currentFrameID = callFrameId
      callFrames = Map [ callFrameId, callFrame ]
      sourceInfo =
        { instructions = List.toArray expr.instructions; resultReg = expr.resultIn }
      packageFns = Map.empty }


and Types =
  { typeSymbolTable : TypeSymbolTable
    package : FQTypeName.Package -> Ply<Option<PackageType.PackageType>> }

and Constants =
  { builtIn : Map<FQConstantName.Builtin, BuiltInConstant>
    package : FQConstantName.Package -> Ply<Option<PackageConstant.PackageConstant>> }

and Functions =
  { builtIn : Map<FQFnName.Builtin, BuiltInFn>
    package : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>> }



// module ExecutionState =
//   let availableTypes (state : ExecutionState) : Types =
//     { typeSymbolTable = state.typeSymbolTable
//     //package = state.packageManager.getType
//     }

// let availableConstants (state : ExecutionState) : Constants =
//   { builtIn = state.builtins.constants
//     package = state.packageManager.getConstant }

// let availableFunctions (state : ExecutionState) : Functions =
//   { builtIn = state.builtins.fns; package = state.packageManager.getFn }



module Types =
  let empty = { typeSymbolTable = Map.empty; package = (fun _ -> Ply None) }

  let find
    (types : Types)
    (name : FQTypeName.FQTypeName)
    : Ply<Option<TypeDeclaration.T>> =
    match name with
    | FQTypeName.Package pkg ->
      types.package pkg |> Ply.map (Option.map _.declaration)

//   /// Swap concrete types for type parameters
//   let rec substitute
//     (typeParams : List<string>)
//     (typeArguments : List<TypeReference>)
//     (typ : TypeReference)
//     : TypeReference =
//     let substitute = substitute typeParams typeArguments
//     match typ with
//     | TVariable v ->
//       if typeParams.Length = typeArguments.Length then
//         List.zip typeParams typeArguments
//         |> List.find (fun (param, _) -> param = v)
//         |> Option.map snd
//         |> Exception.unwrapOptionInternal
//           "No type argument found for type parameter"
//           []
//       else
//         Exception.raiseInternal
//           $"typeParams and typeArguments have different lengths"
//           [ "typeParams", typeParams; "typeArguments", typeArguments ]


//     | TUnit
//     | TBool
//     | TInt8
//     | TUInt8
//     | TInt16
//     | TUInt16
//     | TInt32
//     | TUInt32
//     | TInt64
//     | TUInt64
//     | TInt128
//     | TUInt128
//     | TFloat
//     | TChar
//     | TString
//     | TUuid
//     | TDateTime -> typ

//     | TList t -> TList(substitute t)
//     | TTuple(t1, t2, rest) ->
//       TTuple(substitute t1, substitute t2, List.map substitute rest)
//     | TFn _ -> typ // TYPESTODO
//     | TDB _ -> typ // TYPESTODO
//     | TCustomType(typeName, typeArgs) ->
//       TCustomType(typeName, List.map substitute typeArgs)
//     | TDict t -> TDict(substitute t)




let consoleReporter : ExceptionReporter =
  fun _state (metadata : Metadata) (exn : exn) ->
    printException "runtime-error" metadata exn

let consoleNotifier : Notifier =
  fun _state msg tags ->
    print $"A notification happened in the runtime:\n  {msg}\n  {tags}\n\n"


// let builtInParamToParam (p : BuiltInParam) : Param = { name = p.name; typ = p.typ }

// let builtInFnToFn (fn : BuiltInFn) : Fn =
//   { name = FQFnName.Builtin fn.name
//     typeParams = fn.typeParams
//     parameters =
//       fn.parameters
//       |> List.map builtInParamToParam
//       // We'd like to remove this and use NELists, but it's much too annoying to put
//       // this in every builtin fn definition
//       |> NEList.ofListUnsafe "builtInFnToFn" [ "name", fn.name ]
//     returnType = fn.returnType
//     previewable = fn.previewable
//     sqlSpec = fn.sqlSpec
//     fn = BuiltInFunction fn.fn }

// let packageFnToFn (fn : PackageFn.PackageFn) : Fn =
//   let toParam (p : PackageFn.Parameter) : Param = { name = p.name; typ = p.typ }

//   { name = FQFnName.Package fn.id
//     typeParams = fn.typeParams
//     parameters = fn.parameters |> NEList.map toParam
//     returnType = fn.returnType
//     previewable = Impure
//     sqlSpec = NotQueryable
//     fn = PackageFunction(fn.id, fn.body) }
