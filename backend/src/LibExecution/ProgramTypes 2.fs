/// The types that the user sees
module LibExecution.ProgramTypes

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
    // Eg @darklang.latestVersion()
    { owner : string
      modules : List<string>
      name : 'name
      version : int }

  type T<'name> =
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
    : T<'name> =
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
    : T<'name> =
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
    : T<'name> =
    Package(package nameValidator owner modules name version)

  let builtinToString (f : NamePrinter<'name>) (s : BuiltIn<'name>) : string =
    let name = s.modules @ [ f s.name ] |> String.concat "."
    if s.version = 0 then name else $"{name}_v{s.version}"

  let userProgramToString
    (f : NamePrinter<'name>)
    (s : UserProgram<'name>)
    : string =
    let name = s.modules @ [ f s.name ] |> String.concat "."
    if s.version = 0 then name else $"{name}_v{s.version}"

  let packageToString (f : NamePrinter<'name>) (s : Package<'name>) : string =
    let name = [ "PACKAGE"; s.owner ] @ s.modules @ [ f s.name ] |> String.concat "."
    if s.version = 0 then name else $"{name}_v{s.version}"

  let toString (f : NamePrinter<'name>) (name : T<'name>) : string =
    match name with
    | BuiltIn b -> builtinToString f b
    | UserProgram user -> userProgramToString f user
    | Package pkg -> packageToString f pkg

module TypeName =
  type Name = TypeName of string
  type T = FQName.T<Name>
  type BuiltIn = FQName.BuiltIn<Name>
  type UserProgram = FQName.UserProgram<Name>
  type Package = FQName.Package<Name>

  let pattern = @"^[A-Z][a-z0-9A-Z_']*$"
  let assert' (TypeName name : Name) : unit =
    assertRe "type name must match" pattern name
  let builtIn (modules : List<string>) (name : string) (version : int) : BuiltIn =
    FQName.builtin assert' modules (TypeName name) version

  let fqBuiltIn (modules : List<string>) (name : string) (version : int) : T =
    FQName.fqBuiltIn assert' modules (TypeName name) version

  let userProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : UserProgram =
    FQName.userProgram assert' modules (TypeName name) version

  let fqUserProgram (modules : List<string>) (name : string) (version : int) : T =
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
    : T =
    FQName.fqPackage assert' owner modules (TypeName name) version

  let nameToString (TypeName name) : string = name

  let toString (name : T) : string = FQName.toString nameToString name


module FnName =
  type Name = FnName of string
  type T = FQName.T<Name>
  type BuiltIn = FQName.BuiltIn<Name>
  type UserProgram = FQName.UserProgram<Name>
  type Package = FQName.Package<Name>

  let pattern = @"^[a-z][a-z0-9A-Z_']*$"
  let assert' (FnName name : Name) : unit =
    assertRe "Fn name must match" pattern name

  let builtIn (modules : List<string>) (name : string) (version : int) : BuiltIn =
    FQName.builtin assert' modules (FnName name) version

  let fqBuiltIn (modules : List<string>) (name : string) (version : int) : T =
    FQName.fqBuiltIn assert' modules (FnName name) version

  let userProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : UserProgram =
    FQName.userProgram assert' modules (FnName name) version

  let fqUserProgram (modules : List<string>) (name : string) (version : int) : T =
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
    : T =
    FQName.fqPackage assert' owner modules (FnName name) version

  let nameToString (FnName name) : string = name

  let toString (name : T) : string = FQName.toString nameToString name


/// A Fully-Qualified Constant Name
/// Includes package, module, and version information where relevant.
module ConstantName =
  type Name = ConstantName of string
  type T = FQName.T<Name>
  type BuiltIn = FQName.BuiltIn<Name>
  type UserProgram = FQName.UserProgram<Name>
  type Package = FQName.Package<Name>

  let pattern = @"^[a-z][a-z0-9A-Z_']*$"
  let assert' (ConstantName name : Name) : unit =
    assertRe "Constant name must match" pattern name

  let builtIn (modules : List<string>) (name : string) (version : int) : BuiltIn =
    FQName.builtin assert' modules (ConstantName name) version

  let fqBuiltIn (modules : List<string>) (name : string) (version : int) : T =
    FQName.fqBuiltIn assert' modules (ConstantName name) version

  let userProgram
    (modules : List<string>)
    (name : string)
    (version : int)
    : UserProgram =
    FQName.userProgram assert' modules (ConstantName name) version

  let fqUserProgram (modules : List<string>) (name : string) (version : int) : T =
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
    : T =
    FQName.fqPackage assert' owner modules (ConstantName name) version

  let toString (name : T) : string =
    FQName.toString (fun (ConstantName name) -> name) name


  /// Used in a transformation from fn with the empty args list to constant
  let fromFnName (name : FnName.T) : T =
    match name with
    | FQName.BuiltIn fn ->
      let (FnName.FnName fnName) = fn.name
      let newBuiltIn =
        FQName.builtin assert' fn.modules (ConstantName fnName) fn.version
      FQName.BuiltIn newBuiltIn
    | FQName.UserProgram fn ->
      let (FnName.FnName fnName) = fn.name
      let newUserProgram =
        FQName.userProgram assert' fn.modules (ConstantName fnName) fn.version
      FQName.UserProgram newUserProgram
    | FQName.Package fn ->
      let (FnName.FnName fnName) = fn.name
      let newPackage =
        FQName.package assert' fn.owner fn.modules (ConstantName fnName) fn.version
      FQName.Package newPackage



// In ProgramTypes, names (FnNames, TypeNames, ConstantNames) have already been
// resolved. The user wrote them in WrittenTypes, and the WrittenTypesToProgramTypes
// pass looked them up and specified them exactly in ProgramTypes.
//
// However, sometimes the name/fn/type/constant could not be found, which means the
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

type NameResolution<'a> = Result<'a, Errors.NameResolution.Error>

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
  | MPListCons of id * head : MatchPattern * tail : MatchPattern

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
  | TDB of TypeReference
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TBytes
  // A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string
  | TFn of NEList<TypeReference> * TypeReference

  /// A type defined by a standard library module, a canvas/user, or a package
  /// e.g. `Result<Int, String>` is represented as `TCustomType("Result", [TInt, TString])`
  /// `typeArgs` is the list of type arguments, if any
  | TCustomType of NameResolution<TypeName.T> * typeArgs : List<TypeReference>


/// Expressions - the main part of the language.
type Expr =
  | EInt of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | EUnit of id

  /// A character is an Extended Grapheme Cluster (hence why we use a string). This
  /// is equivalent to one screen-visible "character" in Unicode.
  | EChar of id * string

  // Allow the user to have arbitrarily big numbers, even if they don't make sense as
  // floats. The float is split as we want to preserve what the user entered.
  // Strings are used as numbers lose the leading zeros (eg 7.00007)
  | EFloat of id * Sign * string * string

  | EConstant of id * NameResolution<ConstantName.T>

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

  // Composed of condition, expr if true, and expr if false
  | EIf of id * cond : Expr * thenExpr : Expr * elseExpr : option<Expr>

  // Composed of a parameters * the expression itself
  // The id in the varname list is the analysis id, used to get a livevalue
  // from the analysis engine
  | ELambda of id * NEList<id * string> * Expr


  // Access a field of some expression (e.g. `someExpr.fieldName`)
  | EFieldAccess of id * Expr * string

  // Reference some local variable by name
  //
  // i.e. after a `let binding = value`, any use of `binding`
  | EVariable of id * string

  // This is a function call, the first expression is the value of the function.
  | EApply of id * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>
  | EFnName of id * NameResolution<FnName.T>

  | EInfix of id * Infix * Expr * Expr
  | EPipe of id * Expr * List<PipeExpr>
  | EList of id * List<Expr>
  | EDict of id * List<string * Expr>
  | ETuple of id * Expr * Expr * List<Expr>

  // See NameResolution comment above
  | ERecord of
    id *
    NameResolution<TypeName.T> *
    // User is allowed type `Name {}` even if that's an error
    List<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : NEList<string * Expr>

  // Enums include `Some`, `None`, `Error`, `Ok`, as well
  // as user-defined enums.
  //
  /// Given an Enum type of:
  ///   `type MyEnum = A | B of int | C of int * (label: string) | D of MyEnum`
  /// , this is the expression
  ///   `C (1, "title")`
  /// represented as
  ///   `EEnum(Some UserType.MyEnum, "C", [EInt(1), EString("title")]`
  | EEnum of
    id *
    typeName : NameResolution<TypeName.T> *
    caseName : string *
    fields : List<Expr>

  /// Supports `match` expressions
  /// ```fsharp
  /// match x + 2 with // arg
  /// | pattern -> expr
  /// | pattern -> expr
  /// | ...
  /// ```
  // cases is a list to represent when a user starts typing but doesn't complete it
  | EMatch of id * arg : Expr * cases : List<MatchPattern * Expr>

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and PipeExpr =
  | EPipeVariable of id * string
  | EPipeLambda of id * NEList<id * string> * Expr
  | EPipeInfix of id * Infix * Expr
  | EPipeFnCall of
    id *
    NameResolution<FnName.T> *
    typeArgs : List<TypeReference> *
    args : List<Expr>
  | EPipeEnum of
    id *
    typeName : NameResolution<TypeName.T> *
    caseName : string *
    fields : List<Expr>

module Expr =
  let toID (expr : Expr) : id =
    match expr with
    | EInt(id, _)
    | EBool(id, _)
    | EString(id, _)
    | EChar(id, _)
    | EFloat(id, _, _, _)
    | EUnit id
    | EConstant(id, _)
    | ELet(id, _, _, _)
    | EIf(id, _, _, _)
    | EInfix(id, _, _, _)
    | ELambda(id, _, _)
    | EFnName(id, _)
    | EFieldAccess(id, _, _)
    | EVariable(id, _)
    | EApply(id, _, _, _)
    | EList(id, _)
    | EDict(id, _)
    | ETuple(id, _, _, _)
    | EPipe(id, _, _)
    | ERecord(id, _, _)
    | ERecordUpdate(id, _, _)
    | EEnum(id, _, _, _)
    | EMatch(id, _, _) -> id

module PipeExpr =
  let toID (expr : PipeExpr) : id =
    match expr with
    | EPipeVariable(id, _)
    | EPipeLambda(id, _, _)
    | EPipeInfix(id, _, _)
    | EPipeFnCall(id, _, _, _)
    | EPipeEnum(id, _, _, _) -> id


/// A type defined by a standard library module, a canvas/user, or a package
module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference; description : string }

  type EnumField =
    { typ : TypeReference; label : Option<string>; description : string }

  type EnumCase = { name : string; fields : List<EnumField>; description : string }

  /// The right-hand-side of the declaration: eg List<'a>
  type Definition =
    /// `type MyAlias = Int`
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
  type T =
    { tlid : tlid
      name : TypeName.UserProgram
      declaration : TypeDeclaration.T
      description : string
      deprecated : Deprecation<TypeName.T> }




type Const =
  | CInt of int64
  | CBool of bool
  | CString of string
  | CChar of string
  | CFloat of Sign * string * string
  | CUnit
  | CTuple of first : Const * second : Const * rest : List<Const>
  | CEnum of NameResolution<TypeName.T> * caseName : string * List<Const>

module UserConstant =
  type T =
    { tlid : tlid
      name : ConstantName.UserProgram
      description : string
      deprecated : Deprecation<ConstantName.T>
      body : Const }

module UserFunction =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { tlid : tlid
      name : FnName.UserProgram
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string
      deprecated : Deprecation<FnName.T>
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

module PackageConstant =
  type T =
    { tlid : tlid
      id : System.Guid
      name : ConstantName.Package
      description : string
      deprecated : Deprecation<ConstantName.T>
      body : Const }

module PackageFn =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { tlid : tlid
      id : System.Guid
      name : FnName.Package
      body : Expr
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string
      deprecated : Deprecation<FnName.T> }

module PackageType =
  type T =
    { tlid : tlid
      id : System.Guid
      name : TypeName.Package
      declaration : TypeDeclaration.T
      description : string
      deprecated : Deprecation<TypeName.T> }
