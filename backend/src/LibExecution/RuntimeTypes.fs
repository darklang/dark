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
open VendoredTablecloth


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
    let name = [ "PACKAGE"; s.owner ] @ s.modules @ [ f s.name ] |> String.concat "."
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

  let toString (name : ConstantName) : string =
    FQName.toString name (fun (ConstantName name) -> name)


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
  | TPassword
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
      | TDateTime
      | TPassword -> true
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
  | EIf of id * Expr * Expr * Expr
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
  // adapt this to include more information as we go, possibly using a standard Error
  // type (the same as used in DErrors and Results). This list of exprs is the
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
  { typeArgTable : TypeArgTable
    symtable : Symtable
    parameters : NEList<id * string>
    body : Expr }

and FnValImpl =
  | Lambda of LambdaImpl // A fn value
  | NamedFn of FnName.FnName // A reference to an Fn in the executionState

and DDateTime = NodaTime.LocalDate

and RuntimeError = private RuntimeError of Dval

// We use NoComparison here to avoid accidentally using structural comparison
and [<NoComparison>] Dval =
  | DInt of int64
  | DFloat of double
  | DBool of bool
  | DUnit
  | DString of string
  | DChar of string // TextElements (extended grapheme clusters) are provided as strings

  // compound types
  | DList of List<Dval>
  | DTuple of Dval * Dval * List<Dval>

  | DFnVal of FnValImpl

  /// Represents something that shouldn't have happened in the engine,
  /// that should have been reported elsewhere. It's usually a type error of
  /// some kind, but occasionally we'll paint ourselves into a corner and need
  /// to represent a runtime error using this.
  | DError of DvalSource * RuntimeError

  | DDB of string
  | DDateTime of DarkDateTime.T
  | DPassword of Password
  | DUuid of System.Guid
  | DBytes of byte array

  | DDict of DvalMap
  | DRecord of
    runtimeTypeName : TypeName.TypeName *
    sourceTypeName : TypeName.TypeName *
    DvalMap
  | DEnum of
    runtimeTypeName : TypeName.TypeName *
    sourceTypeName : TypeName.TypeName *
    caseName : string *
    List<Dval>


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
/// we would have a TypeArgTable of
///  { "a" => TInt }
and TypeArgTable = Map<string, TypeReference>


// Record the source of an incomplete or error. Would be useful to add more
// information later, such as the iteration count that led to this, or
// something like a stack trace
and DvalSource =
  // We do not have context to supply an identifier
  | SourceNone

  // Caused by an expression of `id` within the given `tlid`
  | SourceID of tlid * id

// TODO should use rename this to just Param?
// it feels a bit odd that Stdlib.Param is just an abbrev of this
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


exception UncaughtRuntimeError of RuntimeError
let raiseRTE (rte : RuntimeError) = raise (UncaughtRuntimeError rte)



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
    DEnum(name [] "Error" 0, name [] "Error" 0, caseName, fields) |> RuntimeError


  let cliError field = case "CliError" [ field ]

  let nameResolutionError field = case "NameResolutionError" [ field ]

  let typeCheckerError field = case "TypeCheckerError" [ field ]

  let sqlCompilerRuntimeError (internalError : RuntimeError) =
    case "SqlCompilerRuntimeError" [ toDT internalError ]

  let incomplete = case "Incomplete" []

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
  // A Fake Dval is some control-flow that's modelled in the interpreter as a
  // Dval. This is sort of like an Exception. Anytime we see a FakeDval we return
  // it instead of operating on it, including when they're put in a list, in a
  // value, in a record, as a parameter to a function, etc.
  // There used to be multiple types of FakeVal but now there's only DError - which
  // will be removed as well soon.
  let isFake (dv : Dval) : bool =
    match dv with
    | DError _ -> true
    | _ -> false


  let toPairs (dv : Dval) : Result<List<string * Dval>, string> =
    match dv with
    | DDict obj -> Ok(Map.toList obj)
    | _ -> Error "expecting str"


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
    match (dv, typ) with
    | _, TVariable _ -> true
    | DInt _, TInt
    | DFloat _, TFloat
    | DBool _, TBool
    | DUnit, TUnit
    | DString _, TString
    | DDateTime _, TDateTime
    | DPassword _, TPassword
    | DUuid _, TUuid
    | DChar _, TChar
    | DDB _, TDB _
    | DBytes _, TBytes -> true
    | DTuple(first, second, theRest), TTuple(firstType, secondType, otherTypes) ->
      let pairs =
        [ (first, firstType); (second, secondType) ] @ List.zip theRest otherTypes

      pairs |> List.all (fun (v, subtype) -> typeMatches subtype v)
    | DList l, TList t -> List.all (typeMatches t) l
    | DDict m, TDict t -> Map.all (typeMatches t) m
    | DFnVal(Lambda l), TFn(parameters, _) ->
      NEList.length parameters = NEList.length l.parameters

    | DRecord(typeName, _, fields), TCustomType(Ok typeName', typeArgs) ->
      // TYPESCLEANUP: should load type by name
      // TYPESCLEANUP: are we handling type arguments here?
      // TYPESCLEANUP: do we need to check fields?
      typeName = typeName'

    | DEnum(typeName, _, casename, fields), TCustomType(Ok typeName', typeArgs) ->
      // TYPESCLEANUP: should load type by name
      // TYPESCLEANUP: are we handling type arguments here?
      // TYPESCLEANUP: do we need to check fields?
      typeName = typeName'

    // Dont match these fakevals, functions do not have these types
    | DError _, _ -> false

    // exhaustiveness checking
    | DInt _, _
    | DFloat _, _
    | DBool _, _
    | DUnit, _
    | DString _, _
    | DDateTime _, _
    | DPassword _, _
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


  let int (i : int) = DInt(int64 i)


  // Dvals should never be constructed that contain fakevals - the fakeval
  // should always propagate (though, there are specific cases in the
  // interpreter where they are discarded instead of propagated; still they are
  // never put into other dvals). These static members check before creating the values

  let list (list : List<Dval>) : Dval =
    List.find (fun (dv : Dval) -> isFake dv) list
    |> Option.defaultValue (DList list)

  let asList (dv : Dval) : Option<List<Dval>> =
    match dv with
    | DList l -> Some l
    | _ -> None

  let asDict (dv : Dval) : Option<Map<string, Dval>> =
    match dv with
    | DDict d -> Some d
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

  let record (typeName : TypeName.TypeName) (fields : List<string * Dval>) : Dval =
    // Give a warning for duplicate keys
    List.fold
      (DRecord(typeName, typeName, Map.empty))
      (fun m (k, v) ->
        match m, k, v with
        // TYPESCLEANUP: remove hacks
        // If we're propagating a fakeval keep doing it. We handle it without this line but let's be certain
        | m, _k, _v when isFake m -> m
        // Errors should propagate (but only if we're not already propagating an error)
        | DRecord _, _, v when isFake v -> v
        // Skip empty rows
        | _, "", _ -> DError(SourceNone, RuntimeError.oldError $"Empty key {k}")
        // Error if the key appears twice
        | DRecord(_, _, m), k, _v when Map.containsKey k m ->
          DError(SourceNone, RuntimeError.oldError $"Duplicate key: {k}")
        // Otherwise add it
        | DRecord(tn, o, m), k, v -> DRecord(tn, o, Map.add k v m)
        // If we haven't got a DDict we're propagating an error so let it go
        | m, _, _ -> m)
      fields

  let enum
    (typeName : TypeName.TypeName)
    (caseName : string)
    (fields : List<Dval>)
    : Dval =
    match List.find isFake fields with
    | Some v -> v
    | None -> DEnum(typeName, typeName, caseName, fields)

  // CLEANUP - this fn was unused so I commented it out
  // emove? or will it be handy?
  // let dict (fields : List<string * Dval>) : Dval =
  //   // Give a warning for duplicate keys
  //   List.fold
  //     (DDict(Map.empty))
  //     (fun m (k, v) ->
  //       match m, k, v with
  //       // TYPESCLEANUP: remove hacks
  //       // If we're propagating a fakeval keep doing it. We handle it without this line but let's be certain
  //       | m, _k, _v when isFake m -> m
  //       // Errors should propagate (but only if we're not already propagating an error)
  //       | DDict _, _, v when isFake v -> v
  //       // Skip empty rows
  //       | _, "", _ -> DError(SourceNone, $"Empty key: {k}")
  //       // Error if the key appears twice
  //       | DDict m, k, _v when Map.containsKey k m ->
  //         DError(SourceNone, $"Duplicate key: {k}")
  //       // Otherwise add it
  //       | DDict m, k, v -> DDict(Map.add k v m)
  //       // If we haven't got a DDict we're propagating an error so let it go
  //       | m, _, _ -> m)
  //     fields


  let resultType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Result" ] "Result" 0

  let optionType = TypeName.fqPackage "Darklang" [ "Stdlib"; "Option" ] "Option" 0

  let resultOk (dv : Dval) : Dval =
    if isFake dv then dv else DEnum(resultType, resultType, "Ok", [ dv ])
  let resultError (dv : Dval) : Dval =
    if isFake dv then dv else DEnum(resultType, resultType, "Error", [ dv ])

  // Wraps in a Result after checking that the value is not a fakeval
  let result (dv : Result<Dval, Dval>) : Dval =
    match dv with
    | Ok dv -> resultOk dv
    | Error dv -> resultError dv


  let optionSome (dv : Dval) : Dval =
    if isFake dv then dv else DEnum(optionType, optionType, "Some", [ dv ])

  let optionNone : Dval = DEnum(optionType, optionType, "None", [])

  // Wraps in an Option after checking that the value is not a fakeval
  let option (dv : Option<Dval>) : Dval =
    match dv with
    | Some dv -> optionSome dv // checks isFake
    | None -> optionNone

  let errStr (s : string) : Dval = DError(SourceNone, RuntimeError.oldError s)

  let errSStr (source : DvalSource) (s : string) : Dval =
    DError(source, RuntimeError.oldError s)

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

module UserConstant =
  type T = { tlid : tlid; name : ConstantName.UserProgram; body : Dval }

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
  type T = { name : ConstantName.Package; body : Dval }

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
    // Functions whose implementation is in StdLibCloudExecution can only be implemented on the server.

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
    getConstant : ConstantName.Package -> Ply<Option<PackageConstant.T>>

    init : Ply<unit> }

  static member Empty =
    { getType = (fun _ -> Ply None)
      getFn = (fun _ -> Ply None)
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
      Map.tryFind b types.builtIn |> Option.map (fun t -> t.declaration) |> Ply

    | FQName.UserProgram user ->
      Map.tryFind user types.userProgram
      |> Option.map (fun t -> t.declaration)
      |> Ply

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
    | TDateTime
    | TPassword -> typ

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
  | TCustomType(Ok typeName, typeArgs) ->
    uply {
      match! Types.find typeName types with
      | Some({ definition = TypeDeclaration.Alias(TCustomType(innerTypeName, _)) }) ->
        return!
          getTypeReferenceFromAlias types (TCustomType(innerTypeName, typeArgs))
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
