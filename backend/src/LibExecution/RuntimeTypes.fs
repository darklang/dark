/// The core types and functions used by the Dark language's runtime. These
/// are not idential to the serialized types or the types used in the Editor,
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


/// Used to name where type/function/etc lives, eg a BuiltIn module, a User module,
/// or a Package module.
module FQName =

  /// A name that is built into the runtime
  type BuiltIn<'name> = { modules : List<string>; name : 'name; version : int }

  /// Part of the user's program (eg canvas or cli)
  type UserProgram<'name> = { modules : List<string>; name : 'name; version : int }

  /// The name of a thing in the package manager
  // TODO: We plan to use UUIDs for this, but this is a placeholder
  type Package<'name> =
    { owner : string; modules : List<string>; name : 'name; version : int }

  type FQName<'name> =
    | BuiltIn of BuiltIn<'name>
    | UserProgram of UserProgram<'name>
    | Package of Package<'name>


  type NameValidator<'name> = 'name -> unit
  type NamePrinter<'name> = 'name -> string

  // Lowercase starting letter for modules and users
  let modulePattern = @"^[A-Z][a-z0-9A-Z_]*$"
  let assert'
    (modules : List<string>)
    (name : 'name)
    (version : int)
    (nameValidator : 'name -> unit)
    : unit =
    List.iter (assertRe "modules name must match" modulePattern) modules
    nameValidator name
    assert_ "version can't be negative" [ "version", version ] (version >= 0)

  let builtin
    (nameValidator : NameValidator<'name>)
    (modules : List<string>)
    (name : 'name)
    (version : int)
    : BuiltIn<'name> =
    assert' modules name version nameValidator
    { modules = modules; name = name; version = version }

  let fqBuiltIn
    (nameValidator : NameValidator<'name>)
    (modules : List<string>)
    (name : 'name)
    (version : int)
    : FQName<'name> =
    BuiltIn(builtin nameValidator modules name version)

  let userProgram
    (nameValidator : NameValidator<'name>)
    (modules : List<string>)
    (name : 'name)
    (version : int)
    : UserProgram<'name> =
    assert' modules name version nameValidator
    { modules = modules; name = name; version = version }

  let fqUserProgram
    (nameValidator : NameValidator<'name>)
    (modules : List<string>)
    (name : 'name)
    (version : int)
    : FQName<'name> =
    UserProgram(userProgram nameValidator modules name version)

  let package
    (nameValidator : NameValidator<'name>)
    (owner : string)
    (modules : List<string>)
    (name : 'name)
    (version : int)
    : Package<'name> =
    assert' modules name version nameValidator
    { owner = owner; modules = modules; name = name; version = version }

  let fqPackage
    (nameValidator : NameValidator<'name>)
    (owner : string)
    (modules : List<string>)
    (name : 'name)
    (version : int)
    : FQName<'name> =
    Package(package nameValidator owner modules name version)

  let builtinToString (s : BuiltIn<'name>) (f : NamePrinter<'name>) : string =
    let name = s.modules @ [ f s.name ] |> String.concat "."
    if s.version = 0 then name else $"{name}_v{s.version}"

  let userProgramToString
    (s : UserProgram<'name>)
    (f : NamePrinter<'name>)
    : string =
    let name = s.modules @ [ f s.name ] |> String.concat "."
    if s.version = 0 then name else $"{name}_v{s.version}"

  let packageToString (s : Package<'name>) (f : NamePrinter<'name>) : string =
    let name =
      ("PACKAGE" :: s.owner :: s.modules @ [ f s.name ]) |> String.concat "."
    if s.version = 0 then name else $"{name}_v{s.version}"

  let toString (name : FQName<'name>) (f : NamePrinter<'name>) : string =
    match name with
    | BuiltIn b -> builtinToString b f
    | UserProgram user -> userProgramToString user f
    | Package pkg -> packageToString pkg f


module TypeName =
  type Name = TypeName of string
  type TypeName = FQName.FQName<Name>
  type BuiltIn = FQName.BuiltIn<Name>
  type UserProgram = FQName.UserProgram<Name>
  type Package = FQName.Package<Name>

  let pattern = @"^[A-Z][a-z0-9A-Z_]*$"
  let assert' (TypeName name : Name) : unit =
    assertRe "type name must match" pattern name
  let builtIn (modules : List<string>) (name : string) (version : int) : BuiltIn =
    FQName.builtin assert' modules (TypeName name) version

  let fqBuiltIn (modules : List<string>) (name : string) (version : int) : TypeName =
    FQName.fqBuiltIn assert' modules (TypeName name) version

  let userProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : UserProgram =
    FQName.userProgram assert' modules (TypeName name) version

  let fqUserProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : TypeName =
    FQName.fqUserProgram assert' modules (TypeName name) version

  let package
    (owner : string)
    (modules : List<string>)
    (name : string)
    (version : int)
    : Package =
    FQName.package assert' owner modules (TypeName name) version

  let fqPackage
    (owner : string)
    (modules : List<string>)
    (name : string)
    (version : int)
    : TypeName =
    FQName.fqPackage assert' owner modules (TypeName name) version

  let builtinToString (s : BuiltIn) : string =
    FQName.builtinToString s (fun (TypeName name) -> name)

  let userProgramToString (s : UserProgram) : string =
    FQName.userProgramToString s (fun (TypeName name) -> name)

  let packageToString (s : Package) : string =
    FQName.packageToString s (fun (TypeName name) -> name)

  let toString (name : TypeName) : string =
    FQName.toString name (fun (TypeName name) -> name)

  let toShortName (name : TypeName) : string =
    match name with
    | FQName.BuiltIn { name = TypeName name; version = version }
    | FQName.UserProgram { name = TypeName name; version = version }
    | FQName.Package { name = TypeName name; version = version } ->
      if version = 0 then name else $"{name}_v{version}"



module FnName =
  type Name = FnName of string
  type FnName = FQName.FQName<Name>
  type BuiltIn = FQName.BuiltIn<Name>
  type UserProgram = FQName.UserProgram<Name>
  type Package = FQName.Package<Name>

  let pattern = @"^[a-z][a-z0-9A-Z_]*$"
  let assert' (FnName name : Name) : unit =
    assertRe "Fn name must match" pattern name

  let builtIn (modules : List<string>) (name : string) (version : int) : BuiltIn =
    FQName.builtin assert' modules (FnName name) version

  let fqBuiltIn (modules : List<string>) (name : string) (version : int) : FnName =
    FQName.fqBuiltIn assert' modules (FnName name) version

  let userProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : UserProgram =
    FQName.userProgram assert' modules (FnName name) version

  let fqUserProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : FnName =
    FQName.fqUserProgram assert' modules (FnName name) version

  let package
    (owner : string)
    (modules : List<string>)
    (name : string)
    (version : int)
    : Package =
    FQName.package assert' owner modules (FnName name) version

  let fqPackage
    (owner : string)
    (modules : List<string>)
    (name : string)
    (version : int)
    : FnName =
    FQName.fqPackage assert' owner modules (FnName name) version

  let builtinToString (s : BuiltIn) : string =
    FQName.builtinToString s (fun (FnName name) -> name)
  let userProgramToString (s : UserProgram) : string =
    FQName.userProgramToString s (fun (FnName name) -> name)
  let packageToString (s : Package) : string =
    FQName.packageToString s (fun (FnName name) -> name)

  let toString (name : FnName) : string =
    FQName.toString name (fun (FnName name) -> name)

  let isInternalFn (fnName : BuiltIn) : bool =
    List.tryHead fnName.modules = Some "DarkInternal"

/// A Fully-Qualified Constant Name
/// Includes package, module, and version information where relevant.
module ConstantName =
  type Name = ConstantName of string
  type ConstantName = FQName.FQName<Name>
  type BuiltIn = FQName.BuiltIn<Name>
  type UserProgram = FQName.UserProgram<Name>
  type Package = FQName.Package<Name>

  let pattern = @"^[a-z][a-z0-9A-Z_']*$"
  let assert' (ConstantName name : Name) : unit =
    assertRe "Constant name must match" pattern name

  let builtIn (modules : List<string>) (name : string) (version : int) : BuiltIn =
    FQName.builtin assert' modules (ConstantName name) version

  let fqBuiltIn
    (modules : List<string>)
    (name : string)
    (version : int)
    : ConstantName =
    FQName.fqBuiltIn assert' modules (ConstantName name) version

  let userProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : UserProgram =
    FQName.userProgram assert' modules (ConstantName name) version

  let fqUserProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : ConstantName =
    FQName.fqUserProgram assert' modules (ConstantName name) version

  let package
    (owner : string)
    (modules : List<string>)
    (name : string)
    (version : int)
    : Package =
    FQName.package assert' owner modules (ConstantName name) version

  let fqPackage
    (owner : string)
    (modules : List<string>)
    (name : string)
    (version : int)
    : ConstantName =
    FQName.fqPackage assert' owner modules (ConstantName name) version

  let packageToString (s : Package) : string =
    FQName.packageToString s (fun (ConstantName name) -> name)

  let toString (name : ConstantName) : string =
    FQName.toString name (fun (ConstantName name) -> name)


/// A KnownType represents the type of a dval.
///
/// Many KnownTypes (such as lists and records) have nested types. Often, these
/// nested types are unknown (such as the contents of an empty list, or the
/// `Result.Error` type for `Ok 5`). As such, KnownTypes always nest ValueTypes
/// (an optional form of KnownType).
type KnownType =
  | KTUnit
  | KTBool
  | KTInt
  | KTFloat
  | KTChar
  | KTString
  | KTUuid
  | KTBytes
  | KTDateTime

  // let empty =    [] // KTList Unknown
  // let intList = [1] // KTList (ValueType.Known KTInt)
  | KTList of ValueType

  // Intuitively, since Dvals generate KnownTypes, you would think that we can
  // use KnownTypes in a KTTuple.
  //
  // However, we sometimes construct a KTTuple to repesent the type of a Tuple
  // which doesn't exist. For example, in `List.zip [] []`, we create the result
  // from the types of the two lists, which themselves might be (and likely are)
  // `Unknown`.
  | KTTuple of ValueType * ValueType * List<ValueType>

  // let f = (fun x -> x)        // KTFn([Unknown], Unknown)
  // let intF = (fun (x: Int) -> x) // KTFn([Known KTInt], Unknown)
  //
  // Note that we could theoretically know some return types by analyzing the
  // code or type signatures of functions. We don't do this yet as it's
  // complicated. When we do decide to do this, some incorrect programs may stop
  // functioning (see example). Our goal is for correctly typed functions to
  // stay working so this might be ok.
  //
  // For example:
  //   let z1 = (fun x -> 5)
  //   let z2 = (fun x -> "str")
  // `[z1, z2]` is allowed now but might not be allowed later
  | KTFn of args : NEList<ValueType> * ret : ValueType

  // At time of writing, all DBs are of a specific type, and DBs may only be
  // referenced directly, but we expect to eventually allow references to DBs
  // where the type may be unknown
  // List.head ([]: List<DB<'a>>) // KTDB (Unknown)
  | KTDB of ValueType

  /// let n = None          // type args: [Unknown]
  /// let s = Some(5)       // type args: [Known KTInt]
  /// let o = Ok (5)        // type args: [Known KTInt, Unknown]
  /// let e = Error ("str") // type args: [Unknown, Known KTString]
  | KTCustomType of TypeName.TypeName * typeArgs : List<ValueType>

  // let myDict = {} // KTDict Unknown
  | KTDict of ValueType

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
  let typeArgsTODO' = None

  let known inner = ValueType.Known inner

  let unit = known KTUnit
  let bool = known KTBool
  let int = known KTInt
  let float = known KTFloat
  let char = known KTChar
  let string = known KTString
  let dateTime = known KTDateTime
  let uuid = known KTUuid
  let bytes = known KTBytes

  let list (inner : ValueType) : ValueType = known (KTList inner)
  let dict (inner : ValueType) : ValueType = known (KTDict inner)
  let tuple
    (first : ValueType)
    (second : ValueType)
    (theRest : List<ValueType>)
    : ValueType =
    KTTuple(first, second, theRest) |> known

  let rec toString (vt : ValueType) : string =
    match vt with
    | ValueType.Unknown -> "_"
    | ValueType.Known kt ->
      match kt with
      | KTUnit -> "Unit"
      | KTBool -> "Bool"
      | KTInt -> "Int"
      | KTFloat -> "Float"
      | KTChar -> "Char"
      | KTString -> "String"
      | KTUuid -> "Uuid"
      | KTBytes -> "Bytes"
      | KTDateTime -> "DateTime"

      | KTList inner -> $"List<{toString inner}>"
      | KTDict inner -> $"Dict<{toString inner}>"
      | KTTuple(first, second, theRest) ->
        first :: second :: theRest
        |> List.map toString
        |> String.concat " * "
        |> fun inner -> $"({inner})"
      | KTCustomType(typeName, typeArgs) ->
        let typeArgsPart =
          match typeArgs with
          | [] -> ""
          | _ ->
            typeArgs
            |> List.map toString
            |> String.concat ", "
            |> fun inner -> $"<{inner}>"

        $"{TypeName.toString typeName}{typeArgsPart}"

      | KTFn(args, ret) ->
        NEList.toList args @ [ ret ] |> List.map toString |> String.concat " -> "

      | KTDB inner -> $"DB<{toString inner}>"



type NameResolution<'a> = Result<'a, RuntimeError>

// Dark runtime type
and TypeReference =
  | TUnit
  | TBool
  | TInt
  | TFloat
  | TChar
  | TString
  | TUuid
  | TBytes
  | TDateTime
  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TFn of NEList<TypeReference> * TypeReference
  | TDB of TypeReference
  | TVariable of string
  | TCustomType of NameResolution<TypeName.TypeName> * typeArgs : List<TypeReference>
  | TDict of TypeReference // CLEANUP add key type

  member this.isFn() : bool =
    match this with
    | TFn _ -> true
    | _ -> false

  member this.isConcrete() : bool =
    let rec isConcrete (t : TypeReference) : bool =
      match this with
      | TVariable _ -> false
      | TList t -> isConcrete t
      | TTuple(t1, t2, ts) ->
        isConcrete t1 && isConcrete t2 && List.forall isConcrete ts
      | TFn(ts, t) -> NEList.forall isConcrete ts && isConcrete t
      | TDB t -> isConcrete t
      | TCustomType(_, ts) -> List.forall isConcrete ts
      | TDict t -> isConcrete t
      // exhaustiveness
      | TUnit
      | TBool
      | TInt
      | TFloat
      | TChar
      | TString
      | TUuid
      | TBytes
      | TDateTime -> true
    isConcrete this


// Expressions here are runtime variants of the AST in ProgramTypes, having had
// superfluous information removed.
and Expr =
  | EInt of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | EUnit of id
  | EChar of id * string
  | EFloat of id * double
  | EConstant of id * ConstantName.ConstantName
  | ELet of id * LetPattern * Expr * Expr
  | EIf of id * cond : Expr * thenExpr : Expr * elseExpr : option<Expr>
  | ELambda of id * NEList<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EApply of id * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>
  | EFnName of id * FnName.FnName
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | ERecord of id * TypeName.TypeName * NEList<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : NEList<string * Expr>
  | EDict of id * List<string * Expr>
  | EEnum of id * TypeName.TypeName * caseName : string * fields : List<Expr>
  | EMatch of id * Expr * NEList<MatchPattern * Expr>
  | EAnd of id * Expr * Expr
  | EOr of id * Expr * Expr

  // A runtime error. This is included so that we can allow the program to run in the
  // presence of compile-time errors (which are converted to this error). We may
  // adapt this to include more information as we go. This list of exprs is the
  // subexpressions to evaluate before evaluating the error.
  | EError of id * RuntimeError * List<Expr>

and LetPattern =
  | LPVariable of id * name : string
  | LPUnit of id
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and MatchPattern =
  | MPVariable of id * string
  | MPEnum of id * caseName : string * fieldPatterns : List<MatchPattern>
  | MPInt of id * int64
  | MPBool of id * bool
  | MPChar of id * string
  | MPString of id * string
  | MPFloat of id * double
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>
  | MPList of id * List<MatchPattern>
  | MPListCons of id * head : MatchPattern * tail : MatchPattern

and DvalMap = Map<string, Dval>

and LambdaImpl =
  { typeSymbolTable : TypeSymbolTable
    symtable : Symtable
    parameters : NEList<id * string>
    body : Expr }

and FnValImpl =
  | Lambda of LambdaImpl // A fn value
  | NamedFn of FnName.FnName // A reference to an Fn in the executionState

and DDateTime = NodaTime.LocalDate

/// RuntimeError is the major way of representing errors in the runtime. These are
/// primarily used for things where the user made an error, such as a type error, as
/// opposed to a place where the runtime is flawed (use Exception.raiseInternal for those).
/// See docs/errors.md for detailed discussion.
and RuntimeError = private RuntimeError of Dval

// We use NoComparison here to avoid accidentally using structural comparison
and [<NoComparison>] Dval =
  | DUnit

  // Simple types
  | DBool of bool
  | DInt of int64
  | DFloat of double
  | DChar of string // TextElements (extended grapheme clusters) are provided as strings
  | DString of string
  | DDateTime of DarkDateTime.T
  | DUuid of System.Guid
  | DBytes of byte array

  // Compound types
  | DList of ValueType * List<Dval>
  | DTuple of first : Dval * second : Dval * theRest : List<Dval>
  | DDict of
    // This is the type of the _values_, not the keys. Once users can specify the
    // key type, we likely will need to add a `keyType: ValueType` field here.
    valueType : ValueType *
    entries : DvalMap

  | DRecord of
    // CLEANUP nitpick: maybe move sourceTypeName before runtimeTypeName?
    // CLEANUP we may need a sourceTypeArgs here as well
    runtimeTypeName : TypeName.TypeName *
    sourceTypeName : TypeName.TypeName *
    typeArgs : List<ValueType> *
    fields : DvalMap

  | DEnum of
    // CLEANUP nitpick: maybe move sourceTypeName before runtimeTypeName?
    // CLEANUP we may need a sourceTypeArgs here as well
    runtimeTypeName : TypeName.TypeName *
    sourceTypeName : TypeName.TypeName *
    typeArgs : List<ValueType> *
    caseName : string *
    fields : List<Dval>

  // Functions
  | DFnVal of FnValImpl // VTTODO I'm not sure how ValueType fits in here

  // References
  | DDB of name : string


and DvalTask = Ply<Dval>

/// our record of any variable bindings in scope
///
/// i.e. within the execution of `x+y` in
///  `let x = 1; let y = 2; x + y`
/// , we would have a Symtable of
///   `{ "x" => DInt 1; "y" => DInt 2 }`
and Symtable = Map<string, Dval>

/// Our record of any type arguments in scope
///
/// i.e. within the execution of
///   `let serialize<'a> (x : 'a) : string = ...`,
/// called with inputs
///   `serialize<int> 1`,
/// we would have a TypeSymbolTable of
///  { "a" => TInt }
and TypeSymbolTable = Map<string, TypeReference>


// Record the source of an incomplete or error. Would be useful to add more
// information later, such as the iteration count that led to this, or
// something like a stack trace
and DvalSource =
  // We do not have context to supply an identifier
  | SourceNone

  // Caused by an expression of `id` within the given `tlid`
  | SourceID of tlid * id

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




module TypeReference =
  let result (t1 : TypeReference) (t2 : TypeReference) : TypeReference =
    TCustomType(
      Ok(TypeName.fqPackage "Darklang" [ "Stdlib"; "Result" ] "Result" 0),
      [ t1; t2 ]
    )

  let option (t : TypeReference) : TypeReference =
    TCustomType(
      Ok(TypeName.fqPackage "Darklang" [ "Stdlib"; "Option" ] "Option" 0),
      [ t ]
    )

module RuntimeError =
  let toDT (RuntimeError e : RuntimeError) : Dval = e

  let fromDT (dv : Dval) : RuntimeError = RuntimeError dv

  let name (modules : List<string>) (typeName : string) (version : int) =
    TypeName.fqPackage
      "Darklang"
      ("LanguageTools" :: "RuntimeErrors" :: modules)
      typeName
      version

  let case (caseName : string) (fields : List<Dval>) : RuntimeError =
    let typeName = name [] "Error" 0

    // VTTODO This should be created via Dval.enum, but we have an F# cyclical dependency issue:
    // - we want to create a RuntimeError via Dval.enum
    // - when Dval.enum fails, it creates a RuntimeError
    DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError


  let cliError field = case "CliError" [ field ]

  let nameResolutionError field = case "NameResolutionError" [ field ]

  let typeCheckerError field = case "TypeCheckerError" [ field ]

  let sqlCompilerRuntimeError (internalError : RuntimeError) =
    case "SqlCompilerRuntimeError" [ toDT internalError ]

  let executionError field = case "ExecutionError" [ field ]


  // let exceptionThrown (ex : System.Exception) : RuntimeError =
  //   case
  //     "ExceptionThrown"
  //     [ DRecord(
  //         name [ "ExceptionThrown" ] "ExceptionThrown" 0,
  //         name [ "ExceptionThrown" ] "ExceptionThrown" 0,
  //         Map.ofList
  //           [ "message", DString ex.Message
  //             "stackTrace", DString ex.StackTrace
  //             "metadata", DList [] ]
  //       ) ]

  // TODO remove all usages of this in favor of better error cases
  let oldError (msg : string) : RuntimeError =
    case "OldStringErrorTODO" [ DString msg ]

exception RuntimeErrorException of DvalSource * RuntimeError

let raiseRTE (source : DvalSource) (rte : RuntimeError) =
  raise (RuntimeErrorException(source, rte))

// TODO add sources to all RTEs
let raiseUntargetedRTE (rte : RuntimeError) =
  raise (RuntimeErrorException(SourceNone, rte))

// TODO remove all usages of this in favor of better error cases
let raiseString (s : string) : 'a = raiseUntargetedRTE (RuntimeError.oldError s)

/// Internally in the runtime, we allow throwing RuntimeErrorExceptions. At the
/// boundary, typically in Execution.fs, we will catch the exception, and return this
/// type.
type ExecutionResult = Result<Dval, DvalSource * RuntimeError>

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


module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference }
  type EnumCase = { name : string; fields : List<TypeReference> }

  type Definition =
    | Alias of TypeReference
    | Record of NEList<RecordField>
    | Enum of NEList<EnumCase>

  type T = { typeParams : List<string>; definition : Definition }

// Functions for working with Dark runtime expressions
module Expr =
  let toID (expr : Expr) : id =
    match expr with
    | EInt(id, _)
    | EString(id, _)
    | EChar(id, _)
    | EBool(id, _)
    | EUnit id
    | EConstant(id, _)
    | EFloat(id, _)
    | EVariable(id, _)
    | EFieldAccess(id, _, _)
    | ELambda(id, _, _)
    | ELet(id, _, _, _)
    | EIf(id, _, _, _)
    | EApply(id, _, _, _)
    | EFnName(id, _)
    | EList(id, _)
    | ETuple(id, _, _, _)
    | ERecord(id, _, _)
    | ERecordUpdate(id, _, _)
    | EDict(id, _)
    | EEnum(id, _, _, _)
    | EMatch(id, _, _)
    | EError(id, _, _)
    | EAnd(id, _, _)
    | EOr(id, _, _) -> id

// Functions for working with Dark Let patterns
module LetPattern =
  let toID (pat : LetPattern) : id =
    match pat with
    | LPVariable(id, _) -> id
    | LPUnit id -> id
    | LPTuple(id, _, _, _) -> id

// Functions for working with Dark match patterns
module MatchPattern =
  let toID (pat : MatchPattern) : id =
    match pat with
    | MPInt(id, _)
    | MPString(id, _)
    | MPChar(id, _)
    | MPBool(id, _)
    | MPUnit id
    | MPFloat(id, _)
    | MPVariable(id, _)
    | MPTuple(id, _, _, _)
    | MPEnum(id, _, _)
    | MPListCons(id, _, _)
    | MPList(id, _) -> id

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
    | _, TVariable _ -> true
    | DInt _, TInt
    | DFloat _, TFloat
    | DBool _, TBool
    | DUnit, TUnit
    | DString _, TString
    | DDateTime _, TDateTime
    | DUuid _, TUuid
    | DChar _, TChar
    | DDB _, TDB _
    | DBytes _, TBytes -> true
    | DTuple(first, second, theRest), TTuple(firstType, secondType, otherTypes) ->
      let pairs =
        [ (first, firstType); (second, secondType) ] @ List.zip theRest otherTypes

      pairs |> List.all (fun (v, subtype) -> r subtype v)
    | DList(_vtTODO, l), TList t -> List.all (r t) l
    | DDict(_vtTODO, m), TDict t -> Map.all (r t) m
    | DFnVal(Lambda l), TFn(parameters, _) ->
      NEList.length parameters = NEList.length l.parameters

    | DRecord(typeName, _, _typeArgsTODO, fields),
      TCustomType(Ok typeName', _typeArgs) ->
      // TYPESCLEANUP: should load type by name
      // TYPESCLEANUP: are we handling type arguments here?
      // TYPESCLEANUP: do we need to check fields?
      typeName = typeName'

    | DEnum(typeName, _, _typeArgsDEnumTODO, _casename, _fields),
      TCustomType(Ok typeName', _typeArgsExpected) ->
      // TYPESCLEANUP: should load type by name
      // TYPESCLEANUP: convert TCustomType's typeArgs to valueTypes, and compare
      // against the typeArgs in the DEnum - their zipped values should merge OK
      typeName = typeName'

    // exhaustiveness checking
    | DInt _, _
    | DFloat _, _
    | DBool _, _
    | DUnit, _
    | DString _, _
    | DDateTime _, _
    | DUuid _, _
    | DChar _, _
    | DDB _, _
    | DBytes _, _
    | DList _, _
    | DTuple _, _
    | DDict _, _
    | DRecord _, _
    | DFnVal _, _
    | DEnum _, _ -> false




  let asList (dv : Dval) : Option<List<Dval>> =
    match dv with
    | DList(_, l) -> Some l
    | _ -> None

  let asDict (dv : Dval) : Option<Map<string, Dval>> =
    match dv with
    | DDict(_, d) -> Some d
    | _ -> None

  let asTuple2 (dv : Dval) : Option<Dval * Dval> =
    match dv with
    | DTuple(first, second, _) -> Some(first, second)
    | _ -> None

  let asTuple3 (dv : Dval) : Option<Dval * Dval * Dval> =
    match dv with
    | DTuple(first, second, [ third ]) -> Some(first, second, third)
    | _ -> None

  let asString (dv : Dval) : Option<string> =
    match dv with
    | DString s -> Some s
    | _ -> None

  let asInt (dv : Dval) : Option<int64> =
    match dv with
    | DInt i -> Some i
    | _ -> None

  let asFloat (dv : Dval) : Option<double> =
    match dv with
    | DFloat f -> Some f
    | _ -> None

  let asBool (dv : Dval) : Option<bool> =
    match dv with
    | DBool b -> Some b
    | _ -> None

  let asUuid (dv : Dval) : Option<System.Guid> =
    match dv with
    | DUuid u -> Some u
    | _ -> None


module Handler =
  type CronInterval =
    | EveryDay
    | EveryWeek
    | EveryFortnight
    | EveryHour
    | Every12Hours
    | EveryMinute

  type Spec =
    | HTTP of path : string * method : string
    | Worker of name : string
    | Cron of name : string * interval : CronInterval
    | REPL of name : string

  type T = { tlid : tlid; ast : Expr; spec : Spec }

module DB =
  type T = { tlid : tlid; name : string; typ : TypeReference; version : int }

module UserType =
  type T =
    { tlid : tlid; name : TypeName.UserProgram; declaration : TypeDeclaration.T }

type Const =
  | CInt of int64
  | CBool of bool
  | CString of string
  | CChar of string
  | CFloat of Sign * string * string
  | CUnit
  | CTuple of first : Const * second : Const * rest : List<Const>
  | CEnum of NameResolution<TypeName.TypeName> * caseName : string * List<Const>
  | CList of List<Const>
  | CDict of List<string * Const>


module UserConstant =
  type T = { tlid : tlid; name : ConstantName.UserProgram; body : Const }

module UserFunction =
  type Parameter = { name : string; typ : TypeReference }

  type T =
    { tlid : tlid
      name : FnName.UserProgram
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      body : Expr }

module Toplevel =
  type T =
    | TLHandler of Handler.T
    | TLDB of DB.T
    | TLFunction of UserFunction.T
    | TLType of UserType.T
    | TLConstant of UserConstant.T

  let toTLID (tl : T) : tlid =
    match tl with
    | TLHandler h -> h.tlid
    | TLDB db -> db.tlid
    | TLFunction f -> f.tlid
    | TLType t -> t.tlid
    | TLConstant c -> c.tlid

module Secret =
  type T = { name : string; value : string; version : int }


// ------------
// Functions
// ------------

module PackageFn =
  type Parameter = { name : string; typ : TypeReference }

  type T =
    { name : FnName.Package
      tlid : tlid
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      body : Expr }

module PackageType =
  type T = { name : TypeName.Package; declaration : TypeDeclaration.T }

module PackageConstant =
  type T = { name : ConstantName.Package; body : Const }

// <summary>
// Used to mark whether a function can be run on the client rather than backend.
// </summary>
// <remarks>
// The runtime needs to know whether to save a function's results when it
// runs. Pure functions that can be run on the client do not need to have
// their results saved.
// In addition, some functions can be run without side-effects; to give
// the user a good experience, we can run them as soon as they are added.
// this includes DateTime.now and Int.random.
// </remarks>
type Previewable =
  // The same inputs will always yield the same outputs,
  // so we don't need to save results. e.g. `DateTime.addSeconds`
  | Pure

  // Output may vary with the same inputs, though we can safely preview.
  // e.g. `DateTime.now`. We should save the results.
  | ImpurePreviewable

  // Can only be run on the server. e.g. `DB.update`
  // We should save the results.
  | Impure

// Used to mark whether a function has an equivalent that can be
// used within a Postgres query.
type SqlSpec =
  // Can be implemented, but we haven't yet
  | NotYetImplemented

  // This is not a function which can be queried
  | NotQueryable

  // A query function (it can't be called inside a query, but its argument can be a query)
  | QueryFunction

  // Can be implemented by a given builtin postgres 9.6 operator with 1 arg (eg `@ x`)
  | SqlUnaryOp of string

  // Can be implemented by a given builtin postgres 9.6 operator with 2 args (eg `x + y`)
  | SqlBinOp of string

  // Can be implemented by a given builtin postgres 9.6 function
  | SqlFunction of string

  // Can be implemented by a given builtin postgres 9.6 function with extra arguments that go first
  | SqlFunctionWithPrefixArgs of string * List<string>

  // Can be implemented by a given builtin postgres 9.6 function with extra arguments that go last
  | SqlFunctionWithSuffixArgs of string * List<string>

  // Can be implemented by given callback that receives 1 SQLified-string argument
  // | SqlCallback of (string -> string)

  // Can be implemented by given callback that receives 2 SQLified-string argument
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

// A built-in standard library type
type BuiltInType =
  { name : TypeName.BuiltIn
    declaration : TypeDeclaration.T
    // description and deprecated are here because they're not needed in
    // TypeDeclaration for Package and UserProgram types, where we have them in
    // ProgramTypes and don't propagate them to RuntimeTypes
    description : string
    deprecated : Deprecation<TypeName.TypeName> }

type BuiltInConstant =
  { name : ConstantName.BuiltIn
    typ : TypeReference
    description : string
    deprecated : Deprecation<ConstantName.ConstantName>
    body : Dval }

// A built-in standard library function
type BuiltInFn =
  { name : FnName.BuiltIn
    typeParams : List<string>
    parameters : List<BuiltInParam> // TODO: should be NEList but there's so much to change!
    returnType : TypeReference
    description : string
    previewable : Previewable
    deprecated : Deprecation<FnName.FnName>
    sqlSpec : SqlSpec
    fn : BuiltInFnSig }

and Fn =
  { name : FnName.FnName
    typeParams : List<string>
    parameters : NEList<Param>
    returnType : TypeReference
    previewable : Previewable
    sqlSpec : SqlSpec

    // Functions can be run in WASM if they have an implementation in LibExecution.
    // Functions whose implementation is in BuiltinCloudExecution can only be implemented on the server.

    // <remarks>
    // May throw an exception, though we're trying to get them to never throw exceptions.
    // </remarks>
    fn : FnImpl }

and BuiltInFnSig =
  (ExecutionState *

  // type args
  List<TypeReference> *

  // fn args
  List<Dval>)
    -> DvalTask

and FnImpl =
  | BuiltInFunction of BuiltInFnSig
  | UserProgramFunction of tlid * Expr
  | PackageFunction of tlid * Expr


// CLEANUP consider renaming to `ExecutionType`, `EvaluationMode`, etc.
// Represents the context in which we're evaluating some code
and RealOrPreview =
  // We are evaluating an expression normally
  | Real

  // We are previewing the evaluation of some expression within the editor.
  | Preview

and FunctionRecord = tlid * FnName.FnName * id

and TraceDval = bool -> id -> Dval -> unit

and TraceTLID = tlid -> unit

and LoadFnResult = FunctionRecord -> NEList<Dval> -> Option<Dval * NodaTime.Instant>

and StoreFnResult = FunctionRecord -> NEList<Dval> -> Dval -> unit

/// Every part of a user's program
and Program =
  { canvasID : CanvasID
    internalFnsAllowed : bool // whether this canvas is allowed call internal functions
    dbs : Map<string, DB.T>
    fns : Map<FnName.UserProgram, UserFunction.T>
    types : Map<TypeName.UserProgram, UserType.T>
    constants : Map<ConstantName.UserProgram, UserConstant.T>
    secrets : List<Secret.T> }

/// Set of callbacks used to trace the interpreter, and other context needed to run code
and Tracing =
  { traceDval : TraceDval
    traceTLID : TraceTLID
    loadFnResult : LoadFnResult
    storeFnResult : StoreFnResult
    realOrPreview : RealOrPreview }

// Used for testing
and TestContext =
  { mutable sideEffectCount : int

    mutable exceptionReports : List<string * string * Metadata>
    mutable expectedExceptionCount : int
    postTestExecutionHook : TestContext -> Dval -> unit }

// Functionally written in F# and shipped with the executable
and BuiltIns =
  { types : Map<TypeName.BuiltIn, BuiltInType>
    constants : Map<ConstantName.BuiltIn, BuiltInConstant>
    fns : Map<FnName.BuiltIn, BuiltInFn> }

// Functionality written in Dark stored and managed outside of user space
and PackageManager =
  { getType : TypeName.Package -> Ply<Option<PackageType.T>>
    getFn : FnName.Package -> Ply<Option<PackageFn.T>>
    getFnByTLID : tlid -> Ply<Option<PackageFn.T>>
    getConstant : ConstantName.Package -> Ply<Option<PackageConstant.T>>

    init : Ply<unit> }

  static member Empty =
    { getType = (fun _ -> Ply None)
      getFn = (fun _ -> Ply None)
      getFnByTLID = (fun _ -> Ply None)
      getConstant = (fun _ -> Ply None)

      init = uply { return () } }

and ExceptionReporter = ExecutionState -> Metadata -> exn -> unit

and Notifier = ExecutionState -> string -> Metadata -> unit

// All state used while running a program
and ExecutionState =
  { builtIns : BuiltIns
    packageManager : PackageManager
    tracing : Tracing
    program : Program
    test : TestContext

    // Called to report exceptions
    reportException : ExceptionReporter

    // Called to notify that something of interest (that isn't an exception)
    // has happened.
    //
    // Useful for tracking behaviour we want to deprecate, understanding what
    // users are doing, etc.
    notify : Notifier

    // TLID of the currently executing handler/fn
    tlid : tlid

    executingFnName : Option<FnName.FnName>

    // <summary>
    // Callstack of functions that have been called as part of execution
    // </summary>
    //
    // <remarks>
    // Used for recursion detection in the editor.
    // In the editor, we call all paths to show live values,
    // but with recursion that causes infinite recursion.
    // </remarks>
    callstack : Set<FnName.FnName>

    // Whether the currently executing code is really being executed
    // (as opposed to being previewed for traces)
    onExecutionPath : bool }

and Functions =
  { builtIn : Map<FnName.BuiltIn, BuiltInFn>
    package : FnName.Package -> Ply<Option<PackageFn.T>>
    userProgram : Map<FnName.UserProgram, UserFunction.T> }

and Constants =
  { builtIn : Map<ConstantName.BuiltIn, BuiltInConstant>
    package : ConstantName.Package -> Ply<Option<PackageConstant.T>>
    userProgram : Map<ConstantName.UserProgram, UserConstant.T> }

and Types =
  { builtIn : Map<TypeName.BuiltIn, BuiltInType>
    package : TypeName.Package -> Ply<Option<PackageType.T>>
    userProgram : Map<TypeName.UserProgram, UserType.T> }


module ExecutionState =
  let availableTypes (state : ExecutionState) : Types =
    { builtIn = state.builtIns.types
      package = state.packageManager.getType
      userProgram = state.program.types }

  let availableFunctions (state : ExecutionState) : Functions =
    { builtIn = state.builtIns.fns
      package = state.packageManager.getFn
      userProgram = state.program.fns }

  let availableConstants (state : ExecutionState) : Constants =
    { builtIn = state.builtIns.constants
      package = state.packageManager.getConstant
      userProgram = state.program.constants }


module Types =
  let empty =
    { builtIn = Map.empty; package = (fun _ -> Ply None); userProgram = Map.empty }

  let find
    (name : TypeName.TypeName)
    (types : Types)
    : Ply<Option<TypeDeclaration.T>> =
    match name with
    | FQName.BuiltIn b ->
      Map.find b types.builtIn |> Option.map (fun t -> t.declaration) |> Ply

    | FQName.UserProgram user ->
      Map.find user types.userProgram |> Option.map (fun t -> t.declaration) |> Ply

    | FQName.Package pkg ->
      types.package pkg |> Ply.map (Option.map (fun t -> t.declaration))

  // Swap concrete types for type parameters
  let rec substitute
    (typeParams : List<string>)
    (typeArguments : List<TypeReference>)
    (typ : TypeReference)
    : TypeReference =
    let substitute = substitute typeParams typeArguments
    match typ with
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


    | TUnit
    | TBool
    | TInt
    | TFloat
    | TChar
    | TString
    | TUuid
    | TBytes
    | TDateTime -> typ

    | TList t -> TList(substitute t)
    | TTuple(t1, t2, rest) ->
      TTuple(substitute t1, substitute t2, List.map substitute rest)
    | TFn _ -> typ // TYPESTODO
    | TDB _ -> typ // TYPESTODO
    | TCustomType(typeName, typeArgs) ->
      TCustomType(typeName, List.map substitute typeArgs)
    | TDict t -> TDict(substitute t)



let rec getTypeReferenceFromAlias
  (types : Types)
  (typ : TypeReference)
  : Ply<Result<TypeReference, RuntimeError>> =
  match typ with
  | TCustomType(Ok outerTypeName, outerTypeArgs) ->
    uply {
      match! Types.find outerTypeName types with
      | Some { definition = TypeDeclaration.Alias typ; typeParams = typeParams } as decl ->
        let typ = Types.substitute typeParams outerTypeArgs typ
        return! getTypeReferenceFromAlias types typ
      | _ -> return Ok typ
    }

  | TCustomType(Error err, _) -> Ply(Error err)

  | _ -> Ply(Ok typ)


let consoleReporter : ExceptionReporter =
  fun _state (metadata : Metadata) (exn : exn) ->
    printException "runtime-error" metadata exn

let consoleNotifier : Notifier =
  fun _state msg tags ->
    print $"A notification happened in the runtime:\n  {msg}\n  {tags}\n\n"

let builtInParamToParam (p : BuiltInParam) : Param = { name = p.name; typ = p.typ }

let builtInFnToFn (fn : BuiltInFn) : Fn =
  { name = FQName.BuiltIn fn.name
    typeParams = fn.typeParams
    parameters =
      fn.parameters
      |> List.map builtInParamToParam
      // We'd like to remove this and use NELists, but it's much too annoying to put
      // this in every builtin fn definition
      |> NEList.ofListUnsafe "builtInFnToFn" [ "name", fn.name ]
    returnType = fn.returnType
    previewable = fn.previewable
    sqlSpec = fn.sqlSpec
    fn = BuiltInFunction fn.fn }

let userFnToFn (fn : UserFunction.T) : Fn =
  let toParam (p : UserFunction.Parameter) : Param = { name = p.name; typ = p.typ }

  { name = FQName.UserProgram fn.name
    typeParams = fn.typeParams
    parameters = NEList.map toParam fn.parameters
    returnType = fn.returnType
    previewable = Impure
    sqlSpec = NotQueryable
    fn = UserProgramFunction(fn.tlid, fn.body) }

let packageFnToFn (fn : PackageFn.T) : Fn =
  let toParam (p : PackageFn.Parameter) : Param = { name = p.name; typ = p.typ }

  { name = FQName.Package fn.name
    typeParams = fn.typeParams
    parameters = fn.parameters |> NEList.map toParam
    returnType = fn.returnType
    previewable = Impure
    sqlSpec = NotQueryable
    fn = PackageFunction(fn.tlid, fn.body) }
