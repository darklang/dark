/// The core types and functions used by the Dark language's runtime. These
/// are not identical to the serialized types or the types used in the Editor,
/// as those have unique constraints (typically, backward compatibility or
/// continuous delivery).
module LibExecution.RuntimeTypes

// The design of these types is intended to accomodate the unique design of
// Dark, that it's being run sometimes in an editor and sometimes in
// production, etc.

// This typically represents our most accurate representation of the language
// as it is today, however, slight variations of these types are expected to
// exist in other places representing different constraints, such as how
// we've put something in some kind of storage, sending it to some API, etc.
// Those types will always be converted to these types for execution.
//
// The reason these are distinct formats from the serialized types is that
// those types are very difficult to change, while we want this to be
// straightforward to change. So we transform any serialized formats into
// this one for running. We remove any "syntactic sugar" (editor/display only
// features).
//
// These formats should never be serialized/deserialized, that defeats the
// purpose. If you need to save data of this format, create a set of new
// types and convert this type into them. (even if they are identical).
//
// This format is lossy, relative to the serialized types. Use IDs to refer
// back.

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let modulePattern = @"^[A-Z][a-z0-9A-Z_]*$"
let fnNamePattern = @"^[a-z][a-z0-9A-Z_']*$"
let builtinNamePattern = @"^(__|[a-z])[a-z0-9A-Z_]\w*$"
//let constantNamePattern = @"^[a-z][a-z0-9A-Z_']*$"


let assertBuiltin
  (name : string)
  (version : int)
  (nameValidator : string -> unit)
  : unit =
  nameValidator name
  assert_ "version can't be negative" [ "version", version ] (version >= 0)


// /// Fully-Qualified Type Name
// ///
// /// Used to reference a type defined in a Package
// module FQTypeName =
//   /// The id of a type in the package manager
//   type Package = uuid

//   type FQTypeName = Package of Package

//   let package (id : uuid) : Package = id

//   let fqPackage (id : uuid) : FQTypeName = Package id

//   let toString (name : FQTypeName) : string =
//     match name with
//     | Package p -> string p // TODO: better


// /// A Fully-Qualified Constant Name
// ///
// /// Used to reference a constant defined by the runtime or in a Package
// module FQConstantName =
//   /// A constant built into the runtime
//   type Builtin = { name : string; version : int }

//   /// The id of a constant in the package manager
//   type Package = uuid

//   type FQConstantName =
//     | Builtin of Builtin
//     | Package of Package

//   let assertConstantName (name : string) : unit =
//     assertRe "Constant name must match" constantNamePattern name

//   let builtin (name : string) (version : int) : Builtin =
//     assertBuiltin name version assertConstantName
//     { name = name; version = version }

//   let package (id : uuid) : Package = id

//   let fqPackage (id : uuid) : FQConstantName = Package id

//   let builtinToString (s : Builtin) : string =
//     let name = s.name
//     if s.version = 0 then name else $"{name}_v{s.version}"

//   let toString (name : FQConstantName) : string =
//     match name with
//     | Builtin b -> builtinToString b
//     | Package p -> string p // TODO: better


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

  let fqPackage (id : uuid) : FQFnName = Package id

  let builtinToString (s : Builtin) : string =
    let name = s.name
    if s.version = 0 then name else $"{name}_v{s.version}"

  let packageToString (s : Package) : string = string s // TODO: better

  let toString (name : FQFnName) : string =
    match name with
    | Builtin b -> builtinToString b
    | Package pkg -> packageToString pkg

  let isInternalFn (fnName : Builtin) : bool = fnName.name.Contains("darkInternal")



/// A KnownType represents the type of a dval.
///
/// Many KnownTypes (such as lists and records) have nested types. Often, these
/// nested types are unknown (such as the contents of an empty list, or the
/// `Result.Error` type for `Ok 5`). As such, KnownTypes always nest ValueTypes
/// (an optional form of KnownType).
type KnownType =
  | KTUnit
  | KTBool
  // | KTInt8
  // | KTUInt8
  // | KTInt16
  // | KTUInt16
  // | KTInt32
  // | KTUInt32
  | KTInt64
  // | KTUInt64
  // | KTInt128
  // | KTUInt128
  // | KTFloat
  // | KTChar
  | KTString
  // | KTUuid
  // | KTDateTime

  /// let empty =    [] // KTList Unknown
  /// let intList = [1] // KTList (ValueType.Known KTInt64)
  | KTList of ValueType

  // /// Intuitively, since Dvals generate KnownTypes, you would think that we can
  // /// use KnownTypes in a KTTuple.
  // ///
  // /// However, we sometimes construct a KTTuple to repesent the type of a Tuple
  // /// which doesn't exist. For example, in `List.zip [] []`, we create the result
  // /// from the types of the two lists, which themselves might be (and likely are)
  // /// `Unknown`.
  // | KTTuple of ValueType * ValueType * List<ValueType>

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

// /// At time of writing, all DBs are of a specific type, and DBs may only be
// /// referenced directly, but we expect to eventually allow references to DBs
// /// where the type may be unknown
// /// List.head ([]: List<DB<'a>>) // KTDB (Unknown)
// | KTDB of ValueType

// /// let n = None          // type args: [Unknown]
// /// let s = Some(5)       // type args: [Known KTInt64]
// /// let o = Ok (5)        // type args: [Known KTInt64, Unknown]
// /// let e = Error ("str") // type args: [Unknown, Known KTString]
// | KTCustomType of FQTypeName.FQTypeName * typeArgs : List<ValueType>

// /// let myDict = {} // KTDict Unknown
// | KTDict of ValueType

/// Represents the actual type of a Dval
///
/// "Unknown" represents the concept of "bottom" in
///   type system / data flow analysis / lattices
and [<RequireQualifiedAccess>] ValueType =
  | Unknown
  | Known of KnownType

[<RequireQualifiedAccess>]
module ValueType =
  // some helpers to reduce typing elsewhere
  let unknown = ValueType.Unknown
  let unknownTODO = ValueType.Unknown
  let unknownDbTODO = ValueType.Unknown
  let typeArgsTODO = []

  let known inner = ValueType.Known inner

  let unit = known KTUnit
  let bool = known KTBool
  // let int8 = known KTInt8
  // let uint8 = known KTUInt8
  // let int16 = known KTInt16
  // let uint16 = known KTUInt16
  // let int32 = known KTInt32
  // let uint32 = known KTUInt32
  let int64 = known KTInt64
  // let uint64 = known KTUInt64
  // let int128 = known KTInt128
  // let uint128 = known KTUInt128
  // let float = known KTFloat
  // let char = known KTChar
  let string = known KTString
  // let dateTime = known KTDateTime
  // let uuid = known KTUuid

  // let list (inner : ValueType) : ValueType = known (KTList inner)
  // let dict (inner : ValueType) : ValueType = known (KTDict inner)
  // let tuple
  //   (first : ValueType)
  //   (second : ValueType)
  //   (theRest : List<ValueType>)
  //   : ValueType =
  //   KTTuple(first, second, theRest) |> known

  // let customType
  //   (typeName : FQTypeName.FQTypeName)
  //   (typeArgs : List<ValueType>)
  //   : ValueType =
  //   KTCustomType(typeName, typeArgs) |> known

  let rec toString (vt : ValueType) : string =
    match vt with
    | ValueType.Unknown -> "_"
    | ValueType.Known kt ->
      match kt with
      | KTUnit -> "Unit"
      | KTBool -> "Bool"
      // | KTInt8 -> "Int8"
      // | KTUInt8 -> "UInt8"
      // | KTInt16 -> "Int16"
      // | KTUInt16 -> "UInt16"
      // | KTInt32 -> "Int32"
      // | KTUInt32 -> "UInt32"
      | KTInt64 -> "Int64"
      // | KTUInt64 -> "UInt64"
      // | KTInt128 -> "Int128"
      // | KTUInt128 -> "UInt128"
      // | KTFloat -> "Float"
      // | KTChar -> "Char"
      | KTString -> "String"
      // | KTUuid -> "Uuid"
      // | KTDateTime -> "DateTime"

      | KTList inner -> $"List<{toString inner}>"
      // | KTDict inner -> $"Dict<{toString inner}>"
      // | KTTuple(first, second, theRest) ->
      //   first :: second :: theRest
      //   |> List.map toString
      //   |> String.concat " * "
      //   |> fun inner -> $"({inner})"
      // | KTCustomType(typeName, typeArgs) ->
      //   let typeArgsPart =
      //     match typeArgs with
      //     | [] -> ""
      //     | _ ->
      //       typeArgs
      //       |> List.map toString
      //       |> String.concat ", "
      //       |> fun inner -> $"<{inner}>"

      //   $"{FQTypeName.toString typeName}{typeArgsPart}"

      | KTFn(args, ret) ->
        NEList.toList args @ [ ret ] |> List.map toString |> String.concat " -> "

  //| KTDB inner -> $"DB<{toString inner}>"


  let rec private mergeKnownTypes
    (left : KnownType)
    (right : KnownType)
    : Result<KnownType, unit> =
    let r = merge
    match left, right with
    | KTUnit, KTUnit -> KTUnit |> Ok
    | KTBool, KTBool -> KTBool |> Ok
    // | KTInt8, KTInt8 -> KTInt8 |> Ok
    // | KTUInt8, KTUInt8 -> KTUInt8 |> Ok
    // | KTInt16, KTInt16 -> KTInt16 |> Ok
    // | KTUInt16, KTUInt16 -> KTUInt16 |> Ok
    // | KTInt32, KTInt32 -> KTInt32 |> Ok
    // | KTUInt32, KTUInt32 -> KTUInt32 |> Ok
    | KTInt64, KTInt64 -> KTInt64 |> Ok
    // | KTUInt64, KTUInt64 -> KTUInt64 |> Ok
    // | KTInt128, KTInt128 -> KTInt128 |> Ok
    // | KTUInt128, KTUInt128 -> KTUInt128 |> Ok
    // | KTFloat, KTFloat -> KTFloat |> Ok
    // | KTChar, KTChar -> KTChar |> Ok
    | KTString, KTString -> KTString |> Ok
    // | KTUuid, KTUuid -> KTUuid |> Ok
    // | KTDateTime, KTDateTime -> KTDateTime |> Ok

    // | KTList left, KTList right -> r left right |> Result.map KTList
    // | KTDict left, KTDict right -> r left right |> Result.map KTDict
    // | KTTuple(l1, l2, ls), KTTuple(r1, r2, rs) ->
    //   let firstMerged = r l1 r1
    //   let secondMerged = r l2 r2
    //   let restMerged = List.map2 r ls rs |> Result.collect

    //   match firstMerged, secondMerged, restMerged with
    //   | Ok first, Ok second, Ok rest -> Ok(KTTuple(first, second, rest))
    //   | _ -> Error()

    // | KTCustomType(lName, lArgs), KTCustomType(rName, rArgs) ->
    //   if lName <> rName then
    //     Error()
    //   else if List.length lArgs <> List.length rArgs then
    //     Error()
    //   else
    //     List.map2 r lArgs rArgs
    //     |> Result.collect
    //     |> Result.map (fun args -> KTCustomType(lName, args))

    | KTFn(lArgs, lRet), KTFn(rArgs, rRet) ->
      let argsMerged = NEList.map2 r lArgs rArgs |> Result.collectNE
      let retMerged = r lRet rRet

      match argsMerged, retMerged with
      | Ok args, Ok ret -> Ok(KTFn(args, ret))
      | _ -> Error()

    | _ -> Error()

  and merge (left : ValueType) (right : ValueType) : Result<ValueType, unit> =
    match left, right with
    | ValueType.Unknown, v
    | v, ValueType.Unknown -> Ok v

    | ValueType.Known left, ValueType.Known right ->
      mergeKnownTypes left right |> Result.map ValueType.Known



// ------------
// Exprs
// ------------

// /// The LHS pattern in
// /// - a `let` binding (in `let x = 1`, the `x`)
// /// - a lambda (in `fn (x, y) -> x + y`, the `(x, y)`
// type LetPattern =
//   | LPUnit of id
//   | LPTuple of
//     id *
//     first : LetPattern *
//     second : LetPattern *
//     theRest : List<LetPattern>
//   | LPVariable of id * name : string


// /// The LHS of a `match` case
// ///
// /// i.e. the `true` (`MPBool true`) in
// /// ```fsharp
// /// match x with
// /// | true -> "some text"
// /// ```
// type MatchPattern =
//   | MPUnit of id

//   | MPBool of id * bool
//   | MPInt8 of id * int8
//   | MPUInt8 of id * uint8
//   | MPInt16 of id * int16
//   | MPUInt16 of id * uint16
//   | MPInt32 of id * int32
//   | MPUInt32 of id * uint32
//   | MPInt64 of id * int64
//   | MPUInt64 of id * uint64
//   | MPInt128 of id * System.Int128
//   | MPUInt128 of id * System.UInt128

//   | MPFloat of id * double

//   | MPChar of id * string
//   | MPString of id * string

//   | MPList of id * List<MatchPattern>
//   | MPListCons of id * head : MatchPattern * tail : MatchPattern
//   | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

//   | MPEnum of id * caseName : string * fieldPatterns : List<MatchPattern>

//   | MPVariable of id * string


// ------------
// Instructions ("bytecode")
// ------------
type NameResolution<'a> = Result<'a, RuntimeError>

and TypeReference =
  | TUnit
  | TBool
  // | TInt8
  // | TUInt8
  // | TInt16
  // | TUInt16
  // | TInt32
  // | TUInt32
  | TInt64
  // | TUInt64
  // | TInt128
  // | TUInt128
  // | TFloat
  // | TChar
  | TString
  // | TUuid
  // | TDateTime
  | TList of TypeReference
  // | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TFn of NEList<TypeReference> * TypeReference
  // | TDB of TypeReference
  // | TVariable of string
  // | TCustomType of
  //   NameResolution<FQTypeName.FQTypeName> *
  //   typeArgs : List<TypeReference>
  // | TDict of TypeReference // CLEANUP add key type

  member this.isFn() : bool =
    match this with
    | TFn _ -> true
    | _ -> false

  member this.isConcrete() : bool =
    let rec isConcrete (t : TypeReference) : bool =
      match t with
      | TUnit
      | TBool
      // | TInt8
      // | TUInt8
      // | TInt16
      // | TUInt16
      // | TInt32
      // | TUInt32
      | TInt64
      // | TUInt64
      // | TInt128
      // | TUInt128
      // | TFloat
      // | TChar
      | TString
      // | TUuid
      // | TDateTime
       -> true

      | TList t -> isConcrete t
      // | TTuple(t1, t2, ts) ->
      //   isConcrete t1 && isConcrete t2 && List.forall isConcrete ts
      | TFn(ts, t) -> NEList.forall isConcrete ts && isConcrete t
    // | TDB t -> isConcrete t
    // | TCustomType(_, ts) -> List.forall isConcrete ts
    // | TDict t -> isConcrete t

    //| TVariable _-> false

    isConcrete this

and Register = int // TODO: unit of measure

// TODO: consider if each of these should include the Expr ID that they came from
//
// Would Expr ID be enough?
// I don't _think_ we'd have to note the fn ID or TL ID or script name, but maybe?)
and Instruction =
  /// Push a ("constant") value into a register
  | LoadVal of loadTo : Register * Dval

  /// Apply some args (and maybe type args) to something
  /// (a named function, or lambda, etc)
  | Apply of
    putResultIn : Register *
    thingToApply : Register *
    typeArgs : List<TypeReference> *
    args : NEList<Register>

  /// Loads the value of a register into a variable
  | SetVar of varName : string * loadFrom : Register

  /// Stores the value of a variable to a register
  | GetVar of loadTo : Register * varName : string

  // | Jump of jumpTo: Register
  // | JumpIfFalse of condition: Register * jumpTo: Register

  /// Add an item to an existing list
  /// , and type-check to make sure it matches the ValueType of that list
  ///
  /// Note: lists are _created_ with `LoadVal`
  /// (always an empty list of unknown type, to ensure type safety)
  | AddItemToList of listRegister : Register * itemToAdd : Register

  /// Return whatever's in the noted register
  /// (usually relevant only for branching logic like `if`, `match`)
  | Return of from : Register

  /// Fail if this is hit (basically "raise an exception")
  | Fail of RuntimeError


and Instructions = List<Instruction>
and InstructionsWithContext =
  // (rc, instructions, result register)
  (int * Instructions * Register)

// // Expressions here are runtime variants of the AST in ProgramTypes, having had
// // superfluous information removed.
// and Expr =
//   | EUnit of id

//   | EBool of id * bool

//   // | EInt8 of id * int8
//   // | EUInt8 of id * uint8
//   // | EInt16 of id * int16
//   // | EUInt16 of id * uint16
//   // | EInt32 of id * int32
//   // | EUInt32 of id * uint32
//   | EInt64 of id * int64
//   // | EUInt64 of id * uint64
//   // | EInt128 of id * System.Int128
//   // | EUInt128 of id * System.UInt128

//   // | EFloat of id * double

//   // | EChar of id * string
//   | EString of id * List<StringSegment>

//   // // flow control
//   // | EIf of id * cond : Expr * thenExpr : Expr * elseExpr : Option<Expr>
//   // | EMatch of id * Expr * NEList<MatchCase>
//   // | EAnd of id * lhs : Expr * rhs : Expr
//   // | EOr of id * lhs : Expr * rhs : Expr

//   // // declaring and referencing vars
//   // | ELet of id * LetPattern * Expr * Expr
//   // | EVariable of id * string
//   // | EFieldAccess of id * Expr * string

//   // calling fns and other things
//   | EFnName of id * FQFnName.FQFnName
//   | EApply of id * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>
//   //| ELambda of id * pats : NEList<LetPattern> * body : Expr

//   // // structures
//   // | EList of id * List<Expr>
//   // | ETuple of id * Expr * Expr * List<Expr>
//   // | EDict of id * List<string * Expr>

//   // // working with custom types
//   // | EConstant of id * FQConstantName.FQConstantName
//   // | ERecord of id * FQTypeName.FQTypeName * NEList<string * Expr>
//   // | ERecordUpdate of id * record : Expr * updates : NEList<string * Expr>
//   // | EEnum of id * FQTypeName.FQTypeName * caseName : string * fields : List<Expr>

//   // A runtime error. This is included so that we can allow the program to run in the
//   // presence of compile-time errors (which are converted to this error). We may
//   // adapt this to include more information as we go. This list of exprs is the
//   // subexpressions to evaluate before evaluating the error.
//   | EError of id * RuntimeError * List<Expr>

// // and MatchCase = { pat : MatchPattern; whenCondition : Option<Expr>; rhs : Expr }

// and StringSegment =
//   | StringText of string
//   | StringInterpolation of Expr


and DvalMap = Map<string, Dval>

// // Note to self: trying to remove symTable and typeSymbolTable here
// // causes all sorts of scoping issues. Beware.
// // (that said, typeSymbolTable seems the less-risky to remove...)
// and LambdaImpl =
//   { typeSymbolTable : TypeSymbolTable
//     symtable : Symtable
//     parameters : NEList<LetPattern>
//     body : Expr }

and FnValImpl =
  //| Lambda of LambdaImpl
  | NamedFn of FQFnName.FQFnName

/// RuntimeError is the major way of representing errors in the runtime. These are
/// primarily used for things where the user made an error, such as a type error, as
/// opposed to a place where the runtime is flawed (use Exception.raiseInternal for those).
/// See docs/errors.md for detailed discussion.
and RuntimeError = private RuntimeError of string //Dval

// We use NoComparison here to avoid accidentally using structural comparison
and [<NoComparison>] Dval =
  | DUnit

  // Simple types
  | DBool of bool

  // | DInt8 of int8
  // | DUInt8 of uint8
  // | DInt16 of int16
  // | DUInt16 of uint16
  // | DInt32 of int32
  // | DUInt32 of uint32
  | DInt64 of int64
  // | DUInt64 of uint64
  // | DInt128 of System.Int128
  // | DUInt128 of System.UInt128

  // | DFloat of double

  // | DChar of string // TextElements (extended grapheme clusters) are provided as strings
  | DString of string

  // | DDateTime of DarkDateTime.T
  // | DUuid of System.Guid

  // Compound types
  | DList of ValueType * List<Dval>
  // | DTuple of first : Dval * second : Dval * theRest : List<Dval>
  // | DDict of
  //   // This is the type of the _values_, not the keys. Once users can specify the
  //   // key type, we likely will need to add a `keyType: ValueType` field here.
  //   valueType : ValueType *
  //   entries : DvalMap

  // // custom types
  // | DRecord of
  //   // CLEANUP nitpick: maybe move sourceTypeName before runtimeTypeName?
  //   // CLEANUP we may need a sourceTypeArgs here as well
  //   runtimeTypeName : FQTypeName.FQTypeName *
  //   sourceTypeName : FQTypeName.FQTypeName *
  //   typeArgs : List<ValueType> *
  //   fields : DvalMap

  // | DEnum of
  //   // CLEANUP nitpick: maybe move sourceTypeName before runtimeTypeName?
  //   // CLEANUP we may need a sourceTypeArgs here as well
  //   runtimeTypeName : FQTypeName.FQTypeName *
  //   sourceTypeName : FQTypeName.FQTypeName *
  //   typeArgs : List<ValueType> *
  //   caseName : string *
  //   fields : List<Dval>

  // Functions
  | DFnVal of FnValImpl // VTTODO I'm not sure how ValueType fits in here

// // References
// | DDB of name : string


and DvalTask = Ply<Dval>

/// Our record/tracking of any variable bindings in scope
///
/// i.e. within the execution of `x+y` in
///  `let x = 1; let y = 2; x + y`
/// , we would have a Symtable of
///   `{ "x" => DInt64 1; "y" => DInt64 2 }`
and Symtable = Map<string, Dval>

/// Our record/tracking of any type arguments in scope
///
/// i.e. within the execution of
///   `let serialize<'a> (x : 'a) : string = ...`,
/// called with inputs
///   `serialize<int> 1`,
/// we would have a TypeSymbolTable of
///  { "a" => TInt64 }
and TypeSymbolTable = Map<string, TypeReference>

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

/// Record the source expression of an error.
/// This is to show the code that was responsible for it.
/// TODO maybe rename to ExprLocation
and Source = ExecutionPoint * Option<id>

and CallStack =
  {
    /// The entrypoint of the execution
    /// (whatever we're executing for a user)
    entrypoint : ExecutionPoint

    // TODO: bring this back and do something with it,
    // and improve it to be more useful
    // (i.e. maintain order of calls, deal with recursions, etc.)
    // See https://chatgpt.com/share/087935f9-44be-4686-8209-66e701e887b1
    // /// All of the fns that have been called in this execution
    // calledFns : Set<FQFnName.FQFnName>

    /// The last-called thing (roughly)
    ///
    /// If we've encountered an exception, this should be where things failed
    lastCalled : Source
  }

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


module CallStack =
  let fromEntryPoint (entrypoint : ExecutionPoint) : CallStack =
    { entrypoint = entrypoint; lastCalled = (entrypoint, None) }

// module TypeReference =
//   let result (t1 : TypeReference) (t2 : TypeReference) : TypeReference =
//     TCustomType(Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.result), [ t1; t2 ])

//   let option (t : TypeReference) : TypeReference =
//     TCustomType(Ok(FQTypeName.fqPackage PackageIDs.Type.Stdlib.option), [ t ])


module RuntimeError =
  //   let typeName =
  //     FQTypeName.fqPackage PackageIDs.Type.LanguageTools.RuntimeError.error

  //   let toDT (RuntimeError e : RuntimeError) : Dval = e

  //   let fromDT (dv : Dval) : RuntimeError = RuntimeError dv

  //   let case (caseName : string) (fields : List<Dval>) : RuntimeError =
  //     DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError


  //   let cliError field = case "CliError" [ field ]

  //   let nameResolutionError field = case "NameResolutionError" [ field ]

  //   let typeCheckerError field = case "TypeCheckerError" [ field ]

  //   let jsonError field = case "JsonError" [ field ]

  //   let sqlCompilerRuntimeError (internalError : RuntimeError) =
  //     case "SqlCompilerRuntimeError" [ toDT internalError ]

  //   let executionError field = case "ExecutionError" [ field ]

  //   let intError field = case "IntError" [ field ]


  // TODO remove all usages of this in favor of better error cases
  let oldError (msg : string) : RuntimeError =
    //case "OldStringErrorTODO" [ DString msg ]
    RuntimeError msg


/// Note: in cases where it's awkward to niclude a CallStack,
/// the Interpreter should try to inject it where it can
///
/// CLEANUP: ideally, the CallStack isn't required as part of the exception.
/// This would clean up a _lot_ of code.
/// The tricky part is that we do want the CallStack around, to report on,
/// and to use for debugging, but the way the Interpreter+Execution is set up,
/// there's no great single place to `try/with` to supply the call stack.
exception RuntimeErrorException of Option<CallStack> * RuntimeError

let raiseRTE (callStack : CallStack) (rte : RuntimeError) : 'a =
  raise (RuntimeErrorException(Some callStack, rte))

// (only?) OK in builtins because we "fill in" the callstack in the Interpreter for such failures
// CLEANUP maybe (somehow) restrict to only Builtins
let raiseUntargetedRTE (rte : RuntimeError) : 'a =
  raise (RuntimeErrorException(None, rte))


// TODO remove all usages of this in favor of better error cases
let raiseUntargetedString (s : string) : 'a =
  raiseUntargetedRTE (RuntimeError.oldError s)

/// Internally in the runtime, we allow throwing RuntimeErrorExceptions. At the
/// boundary, typically in Execution.fs, we will catch the exception, and return this
/// type.
type ExecutionResult = Result<Dval, Option<CallStack> * RuntimeError>

/// IncorrectArgs should never happen, as all functions are type-checked before
/// calling. If it does happen, it means that the type parameters in the Fn structure
/// do not match the args expected in the F# function definition.
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


// module TypeDeclaration =
//   type RecordField = { name : string; typ : TypeReference }

//   type EnumCase = { name : string; fields : List<TypeReference> }

//   type Definition =
//     | Alias of TypeReference
//     | Record of NEList<RecordField>
//     | Enum of NEList<EnumCase>

//   type T = { typeParams : List<string>; definition : Definition }


// // Functions for working with Dark runtime expressions
// module Expr =
//   let toID (expr : Expr) : id =
//     match expr with
//     | EUnit id

//     | EBool(id, _)

//     // | EInt8(id, _)
//     // | EUInt8(id, _)
//     // | EInt16(id, _)
//     // | EUInt16(id, _)
//     // | EInt32(id, _)
//     // | EUInt32(id, _)
//     | EInt64(id, _)
//     // | EUInt64(id, _)
//     // | EInt128(id, _)
//     // | EUInt128(id, _)

//     // | EFloat(id, _)

//     // | EChar(id, _)
//     | EString(id, _)

//     // | EConstant(id, _)
//     // | EVariable(id, _)
//     // | EFieldAccess(id, _, _)
//     // | ELambda(id, _, _)
//     // | ELet(id, _, _, _)
//     // | EIf(id, _, _, _)
//     | EApply(id, _, _, _)
//     | EFnName(id, _)
//     // | EList(id, _)
//     // | ETuple(id, _, _, _)
//     // | ERecord(id, _, _)
//     // | ERecordUpdate(id, _, _)
//     // | EDict(id, _)
//     // | EEnum(id, _, _, _)
//     // | EMatch(id, _, _)
//     | EError(id, _, _)
//     // | EAnd(id, _, _)
//     // | EOr(id, _, _)
//       -> id

// // Functions for working with Dark Let patterns
// module LetPattern =
//   let toID (pat : LetPattern) : id =
//     match pat with
//     | LPVariable(id, _) -> id
//     | LPUnit id -> id
//     | LPTuple(id, _, _, _) -> id

// // Functions for working with Dark match patterns
// module MatchPattern =
//   let toID (pat : MatchPattern) : id =
//     match pat with
//     | MPInt64(id, _)
//     | MPUInt64(id, _)
//     | MPInt8(id, _)
//     | MPUInt8(id, _)
//     | MPInt16(id, _)
//     | MPUInt16(id, _)
//     | MPInt32(id, _)
//     | MPUInt32(id, _)
//     | MPInt128(id, _)
//     | MPUInt128(id, _)
//     | MPString(id, _)
//     | MPChar(id, _)
//     | MPBool(id, _)
//     | MPUnit id
//     | MPFloat(id, _)
//     | MPVariable(id, _)
//     | MPTuple(id, _, _, _)
//     | MPEnum(id, _, _)
//     | MPListCons(id, _, _)
//     | MPList(id, _) -> id

// Functions for working with Dark runtime values
module Dval =
  // <summary>
  // Checks if a runtime's value matches a given type
  // </summary>
  // <remarks>
  // We have nested types so they need to be checked deeper. CLEANUP:
  // there is also "real" type checking elsewhere - this should be unified.
  // Note, this is primarily used to figure out which argument has ALREADY not
  // matched the actual runtime parameter type of the called function. So more
  // accuracy is better, as the runtime is perfectly accurate.
  // </summary>
  let rec typeMatches (typ : TypeReference) (dv : Dval) : bool =
    let r = typeMatches

    match (dv, typ) with
    //| _, TVariable _ -> true

    | DUnit, TUnit
    | DBool _, TBool

    // | DInt8 _, TInt8
    // | DUInt8 _, TUInt8
    // | DInt16 _, TInt16
    // | DUInt16 _, TUInt16
    // | DInt32 _, TInt32
    // | DUInt32 _, TUInt32
    | DInt64 _, TInt64
    // | DUInt64 _, TUInt64
    // | DInt128 _, TInt128
    // | DUInt128 _, TUInt128

    // | DFloat _, TFloat

    // | DChar _, TChar
    | DString _, TString

    // | DDateTime _, TDateTime
    // | DUuid _, TUuid

    // | DDB _, TDB _
     -> true
    // | DTuple(first, second, theRest), TTuple(firstType, secondType, otherTypes) ->
    //   let pairs =
    //     [ (first, firstType); (second, secondType) ] @ List.zip theRest otherTypes

    //   pairs |> List.all (fun (v, subtype) -> r subtype v)
    | DList(_vtTODO, l), TList t -> List.all (r t) l
    // | DDict(_vtTODO, m), TDict t -> Map.all (r t) m
    // | DFnVal(Lambda l), TFn(parameters, _) ->
    //   NEList.length parameters = NEList.length l.parameters

    // | DRecord(typeName, _, _typeArgsTODO, _fields),
    //   TCustomType(Ok typeName', _typeArgs) ->
    //   // TYPESCLEANUP: should load type by name
    //   // TYPESCLEANUP: are we handling type arguments here?
    //   // TYPESCLEANUP: do we need to check fields?
    //   typeName = typeName'

    // | DEnum(typeName, _, _typeArgsDEnumTODO, _casename, _fields),
    //   TCustomType(Ok typeName', _typeArgsExpected) ->
    //   // TYPESCLEANUP: should load type by name
    //   // TYPESCLEANUP: convert TCustomType's typeArgs to valueTypes, and compare
    //   // against the typeArgs in the DEnum - their zipped values should merge OK
    //   typeName = typeName'

    // exhaustiveness checking
    | DUnit, _
    | DBool _, _
    // | DInt8 _, _
    // | DUInt8 _, _
    // | DInt16 _, _
    // | DUInt16 _, _
    // | DInt32 _, _
    // | DUInt32 _, _
    | DInt64 _, _
    // | DUInt64 _, _
    // | DInt128 _, _
    // | DUInt128 _, _
    // | DFloat _, _
    | DString _, _
    // | DDateTime _, _
    // | DUuid _, _
    // | DChar _, _
    // | DDB _, _
    | DList _, _
    // | DTuple _, _
    // | DDict _, _
    // | DRecord _, _
    | DFnVal _, _
    //| DEnum _, _
     -> false


  let rec toValueType (dv : Dval) : ValueType =
    match dv with
    | DUnit -> ValueType.Known KTUnit

    | DBool _ -> ValueType.Known KTBool

    // | DInt8 _ -> ValueType.Known KTInt8
    // | DUInt8 _ -> ValueType.Known KTUInt8
    // | DInt16 _ -> ValueType.Known KTInt16
    // | DUInt16 _ -> ValueType.Known KTUInt16
    // | DInt32 _ -> ValueType.Known KTInt32
    // | DUInt32 _ -> ValueType.Known KTUInt32
    | DInt64 _ -> ValueType.Known KTInt64
    // | DUInt64 _ -> ValueType.Known KTUInt64
    // | DInt128 _ -> ValueType.Known KTInt128
    // | DUInt128 _ -> ValueType.Known KTUInt128
    // | DFloat _ -> ValueType.Known KTFloat
    // | DChar _ -> ValueType.Known KTChar
    | DString _ -> ValueType.Known KTString
    // | DDateTime _ -> ValueType.Known KTDateTime
    // | DUuid _ -> ValueType.Known KTUuid

    | DList(t, _) -> ValueType.Known(KTList t)
    // | DDict(t, _) -> ValueType.Known(KTDict t)
    // | DTuple(first, second, theRest) ->
    //   ValueType.Known(
    //     KTTuple(toValueType first, toValueType second, List.map toValueType theRest)
    //   )

    // | DRecord(typeName, _, typeArgs, _) ->
    //   KTCustomType(typeName, typeArgs) |> ValueType.Known

    // | DEnum(typeName, _, typeArgs, _, _) ->
    //   KTCustomType(typeName, typeArgs) |> ValueType.Known

    | DFnVal fnImpl ->
      match fnImpl with
      // | Lambda lambda ->
      //   KTFn(
      //     NEList.map (fun _ -> ValueType.Unknown) lambda.parameters,
      //     ValueType.Unknown
      //   )
      //   |> ValueType.Known

      // VTTODO look up type, etc
      | NamedFn _named -> ValueType.Unknown

  // // CLEANUP follow up when DDB has a typeReference
  // | DDB _ -> ValueType.Unknown


  // let asList (dv : Dval) : Option<List<Dval>> =
  //   match dv with
  //   | DList(_, l) -> Some l
  //   | _ -> None

  // let asDict (dv : Dval) : Option<Map<string, Dval>> =
  //   match dv with
  //   | DDict(_, d) -> Some d
  //   | _ -> None

  // let asTuple2 (dv : Dval) : Option<Dval * Dval> =
  //   match dv with
  //   | DTuple(first, second, _) -> Some(first, second)
  //   | _ -> None

  // let asTuple3 (dv : Dval) : Option<Dval * Dval * Dval> =
  //   match dv with
  //   | DTuple(first, second, [ third ]) -> Some(first, second, third)
  //   | _ -> None

  let asString (dv : Dval) : Option<string> =
    match dv with
    | DString s -> Some s
    | _ -> None

  // let asInt8 (dv : Dval) : Option<int8> =
  //   match dv with
  //   | DInt8 i -> Some i
  //   | _ -> None

  // let asUInt8 (dv : Dval) : Option<uint8> =
  //   match dv with
  //   | DUInt8 i -> Some i
  //   | _ -> None

  // let asInt16 (dv : Dval) : Option<int16> =
  //   match dv with
  //   | DInt16 i -> Some i
  //   | _ -> None

  // let asUInt16 (dv : Dval) : Option<uint16> =
  //   match dv with
  //   | DUInt16 i -> Some i
  //   | _ -> None

  // let asInt32 (dv : Dval) : Option<int32> =
  //   match dv with
  //   | DInt32 i -> Some i
  //   | _ -> None

  // let asUInt32 (dv : Dval) : Option<uint32> =
  //   match dv with
  //   | DUInt32 i -> Some i
  //   | _ -> None

  let asInt64 (dv : Dval) : Option<int64> =
    match dv with
    | DInt64 i -> Some i
    | _ -> None

  // let asUInt64 (dv : Dval) : Option<uint64> =
  //   match dv with
  //   | DUInt64 i -> Some i
  //   | _ -> None

  // let asInt128 (dv : Dval) : Option<System.Int128> =
  //   match dv with
  //   | DInt128 i -> Some i
  //   | _ -> None

  // let asUInt128 (dv : Dval) : Option<System.UInt128> =
  //   match dv with
  //   | DUInt128 i -> Some i
  //   | _ -> None

  // let asFloat (dv : Dval) : Option<double> =
  //   match dv with
  //   | DFloat f -> Some f
  //   | _ -> None

  let asBool (dv : Dval) : Option<bool> =
    match dv with
    | DBool b -> Some b
    | _ -> None

// let asUuid (dv : Dval) : Option<System.Guid> =
//   match dv with
//   | DUuid u -> Some u
//   | _ -> None


// type Const =
//   | CUnit
//   | CBool of bool

//   | CInt8 of int8
//   | CUInt8 of uint8
//   | CInt16 of int16
//   | CUInt16 of uint16
//   | CInt32 of int32
//   | CUInt32 of uint32
//   | CInt64 of int64
//   | CUInt64 of uint64
//   | CInt128 of System.Int128
//   | CUInt128 of System.UInt128

//   | CFloat of Sign * string * string

//   | CChar of string
//   | CString of string

//   | CList of List<Const>
//   | CTuple of first : Const * second : Const * rest : List<Const>
//   | CDict of List<string * Const>

//   | CEnum of NameResolution<FQTypeName.FQTypeName> * caseName : string * List<Const>



// // ------------
// // Package-Space
// // ------------
// module PackageType =
//   // TODO: hash
//   type PackageType = { id : uuid; declaration : TypeDeclaration.T }

// module PackageConstant =
//   // TODO: hash
//   type PackageConstant = { id : uuid; body : Const }

module PackageFn =
  type Parameter = { name : string; typ : TypeReference }

  // TODO: hash
  type PackageFn =
    { id : uuid
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      body : InstructionsWithContext }


// // ------------
// // User-/Canvas- Space
// // ------------
// module DB =
//   type T = { tlid : tlid; name : string; typ : TypeReference; version : int }

// module Secret =
//   type T = { name : string; value : string; version : int }

// module Handler =
//   type CronInterval =
//     | EveryDay
//     | EveryWeek
//     | EveryFortnight
//     | EveryHour
//     | Every12Hours
//     | EveryMinute

//   type Spec =
//     | HTTP of path : string * method : string
//     | Worker of name : string
//     | Cron of name : string * interval : CronInterval
//     | REPL of name : string

//   type T = { tlid : tlid; ast : Expr; spec : Spec }

// module Toplevel =
//   type T =
//     | TLHandler of Handler.T
//     | TLDB of DB.T

//   let toTLID (tl : T) : tlid =
//     match tl with
//     | TLHandler h -> h.tlid
//     | TLDB db -> db.tlid



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


// /// Used to mark whether a function has an equivalent that can be
// /// used within a Postgres query.
// type SqlSpec =
//   /// Can be implemented, but we haven't yet
//   | NotYetImplemented

//   /// This is not a function which can be queried
//   | NotQueryable

//   /// A query function (it can't be called inside a query, but its argument can be a query)
//   | QueryFunction

//   /// Can be implemented by a given builtin postgres 9.6 operator with 1 arg (eg `@ x`)
//   | SqlUnaryOp of string

//   /// Can be implemented by a given builtin postgres 9.6 operator with 2 args (eg `x + y`)
//   | SqlBinOp of string

//   /// Can be implemented by a given builtin postgres 9.6 function
//   | SqlFunction of string

//   /// Can be implemented by a given builtin postgres 9.6 function with extra arguments that go first
//   | SqlFunctionWithPrefixArgs of string * List<string>

//   /// Can be implemented by a given builtin postgres 9.6 function with extra arguments that go last
//   | SqlFunctionWithSuffixArgs of string * List<string>

//   /// Can be implemented by given callback that receives 1 SQLified-string argument
//   /// | SqlCallback of (string -> string)
//   /// Can be implemented by given callback that receives 2 SQLified-string argument
//   | SqlCallback2 of (string -> string -> string)

//   member this.isQueryable() : bool =
//     match this with
//     | NotYetImplemented
//     | NotQueryable
//     | QueryFunction -> false
//     | SqlUnaryOp _
//     | SqlBinOp _
//     | SqlFunction _
//     | SqlFunctionWithPrefixArgs _
//     | SqlFunctionWithSuffixArgs _
//     | SqlCallback2 _ -> true


// type BuiltInConstant =
//   { name : FQConstantName.Builtin
//     typ : TypeReference
//     description : string
//     deprecated : Deprecation<FQConstantName.FQConstantName>
//     body : Dval }


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
    //sqlSpec : SqlSpec
    fn : BuiltInFnSig }

and Fn =
  {
    name : FQFnName.FQFnName
    typeParams : List<string>
    parameters : NEList<Param>
    returnType : TypeReference
    previewable : Previewable
    //sqlSpec : SqlSpec

    /// <remarks>
    /// May throw an exception, though we're trying to get them to never throw exceptions.
    /// </remarks>
    fn : FnImpl
  }

and BuiltInFnSig =
  // state * typeArgs * fnArgs -> result
  (ExecutionState * List<TypeReference> * List<Dval>) -> DvalTask

and FnImpl =
  | BuiltInFunction of BuiltInFnSig
  | PackageFunction of FQFnName.Package * InstructionsWithContext //* localCount: int


and FunctionRecord = Source * FQFnName.FQFnName

and TraceDval = id -> Dval -> unit

and TraceExecutionPoint = ExecutionPoint -> unit

// why do we need the Dvals here? those are the args, right - do we really need them?
// ah, because we could call the same fn twice, from the same place, but with different args. hmm.
and LoadFnResult = FunctionRecord -> NEList<Dval> -> Option<Dval * NodaTime.Instant>

and StoreFnResult = FunctionRecord -> NEList<Dval> -> Dval -> unit

/// Every part of a user's program
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
    storeFnResult : StoreFnResult

    callStack : CallStack }

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
  { //constants : Map<FQConstantName.Builtin, BuiltInConstant>
    fns : Map<FQFnName.Builtin, BuiltInFn> }

/// Functionality written in Dark stored and managed outside of user space
///
/// Note: it may be tempting to think these shouldn't return Options,
/// but if/when Package items may live (for some time) only on local systems,
/// there's a chance some code will be committed, referencing something
/// not yet in the Cloud PM.
/// (though, we'll likely demand deps. in the PM before committing something upstream...)
and PackageManager =
  { //getType : FQTypeName.Package -> Ply<Option<PackageType.PackageType>>
    //getConstant :
    //  FQConstantName.Package -> Ply<Option<PackageConstant.PackageConstant>>
    getFn : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>>

    init : Ply<unit> }

  static member empty =
    { //getType = (fun _ -> Ply None)
      getFn = (fun _ -> Ply None)
      //getConstant = (fun _ -> Ply None)

      init = uply { return () } }

  /// Allows you to side-load a few 'extras' in-memory, along
  /// the normal fetching functionality. (Mostly helpful for tests)
  static member withExtras
    (pm : PackageManager)
    //(types : List<PackageType.PackageType>)
    //(constants : List<PackageConstant.PackageConstant>)
    (fns : List<PackageFn.PackageFn>)
    : PackageManager =
    {
      // getType =
      //   fun id ->
      //     match types |> List.tryFind (fun t -> t.id = id) with
      //     | Some t -> Some t |> Ply
      //     | None -> pm.getType id
      // getConstant =
      //   fun id ->
      //     match constants |> List.tryFind (fun c -> c.id = id) with
      //     | Some c -> Some c |> Ply
      //     | None -> pm.getConstant id
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
    builtins : Builtins
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

    // Maybe replace this and `builtins` with availTypes, availConsts, availFns?
    // We're doing some ExecutionState -> (those) mappings at runtime on occasion,
    // probably a lot more than we need
    packageManager : PackageManager

    // Is anything actually referencing this right now?
    symbolTable : Symtable
    typeSymbolTable : TypeSymbolTable
  }

and Types =
  { typeSymbolTable : TypeSymbolTable
  //package : FQTypeName.Package -> Ply<Option<PackageType.PackageType>>
  }

// and Constants =
//   { builtIn : Map<FQConstantName.Builtin, BuiltInConstant>
//     package : FQConstantName.Package -> Ply<Option<PackageConstant.PackageConstant>> }

and Functions =
  { builtIn : Map<FQFnName.Builtin, BuiltInFn>
    package : FQFnName.Package -> Ply<Option<PackageFn.PackageFn>> }



module ExecutionState =
  let availableTypes (state : ExecutionState) : Types =
    { typeSymbolTable = state.typeSymbolTable
    //package = state.packageManager.getType
    }

  // let availableConstants (state : ExecutionState) : Constants =
  //   { builtIn = state.builtins.constants
  //     package = state.packageManager.getConstant }

  let availableFunctions (state : ExecutionState) : Functions =
    { builtIn = state.builtins.fns; package = state.packageManager.getFn }



// module Types =
//   let empty = { typeSymbolTable = Map.empty; package = (fun _ -> Ply None) }

//   let find
//     // TODO: swap these args
//     (name : FQTypeName.FQTypeName)
//     (types : Types)
//     : Ply<Option<TypeDeclaration.T>> =
//     match name with
//     | FQTypeName.Package pkg ->
//       types.package pkg |> Ply.map (Option.map _.declaration)

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



let rec getTypeReferenceFromAlias
  (_types : Types)
  (typ : TypeReference)
  : Ply<Result<TypeReference, RuntimeError>> =
  match typ with
  // | TCustomType(Ok outerTypeName, outerTypeArgs) ->
  //   uply {
  //     match! Types.find outerTypeName types with
  //     | Some { definition = TypeDeclaration.Alias typ; typeParams = typeParams } ->
  //       let typ = Types.substitute typeParams outerTypeArgs typ
  //       return! getTypeReferenceFromAlias types typ
  //     | _ -> return Ok typ
  //   }

  // | TCustomType(Error err, _) -> Ply(Error err)

  | _ -> Ply(Ok typ)


let consoleReporter : ExceptionReporter =
  fun _state (metadata : Metadata) (exn : exn) ->
    printException "runtime-error" metadata exn

let consoleNotifier : Notifier =
  fun _state msg tags ->
    print $"A notification happened in the runtime:\n  {msg}\n  {tags}\n\n"


let builtInParamToParam (p : BuiltInParam) : Param = { name = p.name; typ = p.typ }

let builtInFnToFn (fn : BuiltInFn) : Fn =
  { name = FQFnName.Builtin fn.name
    typeParams = fn.typeParams
    parameters =
      fn.parameters
      |> List.map builtInParamToParam
      // We'd like to remove this and use NELists, but it's much too annoying to put
      // this in every builtin fn definition
      |> NEList.ofListUnsafe "builtInFnToFn" [ "name", fn.name ]
    returnType = fn.returnType
    previewable = fn.previewable
    //sqlSpec = fn.sqlSpec
    fn = BuiltInFunction fn.fn }

let packageFnToFn (fn : PackageFn.PackageFn) : Fn =
  let toParam (p : PackageFn.Parameter) : Param = { name = p.name; typ = p.typ }

  { name = FQFnName.Package fn.id
    typeParams = fn.typeParams
    parameters = fn.parameters |> NEList.map toParam
    returnType = fn.returnType
    previewable = Impure
    //sqlSpec = NotQueryable
    fn = PackageFunction(fn.id, fn.body) }
