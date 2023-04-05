/// The types that the user sees
module LibExecution.ProgramTypes

open FSharp.Reflection

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign

/// Used to reference a type defined by a User, Standard Library module, or Package
module FQTypeName =
  type StdlibTypeName = { typ : string }

  /// A type written by a Developer in their canvas
  type UserTypeName = { typ : string; version : int }

  type T =
    // TODO: add `| Package of PackageTypeName` case
    | Stdlib of StdlibTypeName
    | User of UserTypeName

  // TODO: review this - OK?
  let stdlibTypeNamePat = @"^[A-Z][a-z0-9A-Z_]*$"

  let stdlibTypeName (typ : string) : StdlibTypeName =
    Prelude.assertRe "stdlib type name must match" stdlibTypeNamePat typ

    { typ = typ }



/// A Fully-Qualified Function Name
/// Includes package, module, and version information where relevant.
module FQFnName =

  /// Standard Library Function Name
  type StdlibFnName = { module_ : string; function_ : string; version : int }

  /// A UserFunction is a function written by a Developer in their canvas
  type UserFnName = string

  /// The name of a function in the package manager
  type PackageFnName =
    { owner : string
      package : string
      module_ : string
      function_ : string
      version : int }

  // We don't include InfixStdlibFnName here as that is used directly by EInfix
  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

  let oneWordFunctions =
    Set [ "equals"; "notEquals"; "equals_v0"; "notEquals_v0"; "emit_v1" ]

  // CLEANUP Packages should just have a uuid
  let namePat = @"^[a-z][a-z0-9_]*$"
  let modNamePat = @"^[A-Z][a-z0-9A-Z_]*$"
  let fnnamePat = @"^([a-z][a-z0-9A-Z_]*)$"
  let userFnNamePat = @"^([a-z][a-z0-9A-Z_]*)$"

  let packageFnName
    (owner : string)
    (package : string)
    (module_ : string)
    (function_ : string)
    (version : int)
    : PackageFnName =
    Prelude.assertRe "owner must match" namePat owner
    Prelude.assertRe "package must match" namePat package
    Prelude.assertRe "modName name must match" modNamePat module_
    Prelude.assertRe "package function name must match" fnnamePat function_
    Prelude.assert_ "version can't be negative" [ "version", version ] (version >= 0)
    { owner = owner
      package = package
      module_ = module_
      function_ = function_
      version = version }

  let packageFqName
    (owner : string)
    (package : string)
    (module_ : string)
    (function_ : string)
    (version : int)
    : T =
    Package(packageFnName owner package module_ function_ version)

  let userFnName (fnName : string) : UserFnName =
    Prelude.assertRe "user function name must match" userFnNamePat fnName
    fnName


  let userFqName (fnName : string) = User(userFnName fnName)

  let stdlibFnName
    (module_ : string)
    (function_ : string)
    (version : int)
    : StdlibFnName =
    if module_ <> "" then
      Prelude.assertRe "modName name must match" modNamePat module_
    Prelude.assertRe "stdlib function name must match" fnnamePat function_
    Prelude.assert_
      "version can't be negative"
      [ "function", function_; "version", version ]
      (version >= 0)
    { module_ = module_; function_ = function_; version = version }

  let stdlibFqName (module_ : string) (function_ : string) (version : int) : T =
    Stdlib(stdlibFnName module_ function_ version)

type LetPattern = LPVariable of id * name : string

/// Used for pattern matching in a match statement
type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * caseName : string * fieldPats : List<MatchPattern>
  | MPInteger of id * int64
  | MPBool of id * bool
  | MPCharacter of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

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
type DType =
  | TInt
  | TFloat
  | TBool
  | TUnit
  | TStr
  | TList of DType
  | TTuple of DType * DType * List<DType>
  | TDict of DType
  | TIncomplete
  | TError
  | THttpResponse of DType
  | TDB of DType
  | TDateTime
  | TChar
  | TPassword
  | TUuid
  | TBytes
  // A named variable, eg `a` in `List<a>`, matches anything
  | TVariable of string // replaces TAny
  | TFn of List<DType> * DType // replaces TLambda

  | TDbList of DType // TODO: cleanup and remove

  /// A type defined by a standard library module, a canvas/user, or a package
  /// e.g. `Result<Int, String>` is represented as `TCustomType("Result", [TInt, TStr])`
  /// `typeArgs` is the list of type arguments, if any
  | TCustomType of FQTypeName.T * typeArgs : List<DType>

  // TODO: collapse into TCustomType once Stdlib-defined types are supported in FQTypeName
  // and the Option module defines the custom `Option` type
  | TOption of DType

  // TODO: collapse into TCustomType once Stdlib-defined types are supported in FQTypeName
  // and the Result module defines the custom `Result` type
  | TResult of DType * DType

  // TODO: remove in favor of `TCustomType` referring to defined `CustomType.Record`s
  | TRecord of List<string * DType>


/// Expressions - the main part of the language.
type Expr =
  | EInteger of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  /// A character is an Extended Grapheme Cluster (hence why we use a string). This
  /// is equivalent to one screen-visible "character" in Unicode.
  | ECharacter of id * string
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
  | EFnCall of id * FQFnName.T * typeArgs : List<DType> * args : List<Expr>
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | EPipe of id * Expr * PipeExpr * List<PipeExpr>
  | ERecord of id * Option<FQTypeName.T> * List<string * Expr>

  // Constructors include `Just`, `Nothing`, `Error`, `Ok`, as well
  // as user-defined enums.
  //
  /// Given an Enum type of:
  ///   `type MyEnum = A | B of int | C of int * (label: string) | D of MyEnum`
  /// , this is the expression
  ///   `C (1, "title")`
  /// represented as
  ///   `EConstructor(Some UserType.MyEnum, "C", [EInteger(1), EString("title")]`
  /// TODO: the UserTypeName should eventually be a non-optional FQTypeName.
  | EConstructor of
    id *
    typeName : Option<FQTypeName.T> *
    caseName : string *
    fields : List<Expr>

  /// Supports `match` expressions
  /// ```fsharp
  /// match x + 2 with // arg
  /// // cases
  /// | pattern -> expr
  /// | pattern -> expr
  /// | ...
  /// ```
  | EMatch of id * arg : Expr * cases : List<MatchPattern * Expr>

  // Placeholder that indicates the target of the Thread. May be movable at
  // some point
  | EPipeTarget of id

  // Using to mark forbidden Expr for certain cases like using literal in the middle of PipeExpr
  | EForbiddenExpr of id * message : string * Expr

  /// Like an if statement, but with a label
  /// TODO: continue describing
  | EFeatureFlag of
    id *
    flagName : string *
    cond : Expr *
    caseA : Expr *
    caseB : Expr

and PipeExpr =
  | EPipeVariable of id * string
  | EPipeLambda of id * List<id * string> * Expr
  | EPipeInfix of id * Infix * Expr * Expr
  | EPipeFnCall of id * FQFnName.T * typeArgs : List<DType> * args : List<Expr>
  | EPipeConstructor of
    id *
    typeName : Option<FQTypeName.T> *
    caseName : string *
    fields : List<Expr>
  | EPipeForbiddenExpr of id * message : string * Expr


and StringSegment =
  | StringText of string
  | StringInterpolation of Expr



/// A type defined by a standard library module, a canvas/user, or a package
module CustomType =
  type RecordField = { id : id; name : string; typ : DType }

  type EnumField = { id : id; typ : DType; label : Option<string> }
  type EnumCase = { id : id; name : string; fields : List<EnumField> }

  type T =
    // TODO: //| Abbreviation/Alias of DType

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

  // We need to keep the IDs around until we get rid of them on the client
  type ids = { moduleID : id; nameID : id; modifierID : id }

  type Spec =
    | HTTP of route : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids

  type T = { tlid : tlid; ast : Expr; spec : Spec }


module DB =
  type Col = { name : Option<string>; typ : Option<DType>; nameID : id; typeID : id }

  type T =
    { tlid : tlid
      name : string
      nameID : id
      version : int
      cols : List<Col> }

module UserType =
  // TODO: consider flattening this (just type UserType = { ... }, without the module level)
  type Definition = CustomType.T
  type T = { tlid : tlid; name : FQTypeName.UserTypeName; definition : Definition }

module UserFunction =
  type Parameter = { id : id; name : string; typ : DType; description : string }

  type T =
    { tlid : tlid
      name : string
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : DType
      description : string
      infix : bool
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

module Pipe =
  /// Convert Regular Expr to PipeExpr
  let toPipeExpr (expr : Expr) : PipeExpr =

    let getId (e : Expr) =
      let _, fields = FSharpValue.GetUnionFields(e, typeof<Expr>)
      let idObj = fields.[0]
      match idObj with
      | :? id as id -> id
      | _ ->
        Prelude.Exception.raiseInternal
          "Expected an Expr with id field"
          [ "Expr", e ]

    let getErrorFromFnArgs (args : Expr list) : string option =
      match List.tryItem 0 args with
      | Some (EString (_, s :: _)) ->
        match s with
        | StringText text -> Some text
        | _ -> None
      | _ -> None

    match expr with
    | EVariable (id, var) -> EPipeVariable(id, var)
    | ELambda (id, lid, expr) -> EPipeLambda(id, lid, expr)
    | EInfix (id, infix, expr1, expr2) -> EPipeInfix(id, infix, expr1, expr2)
    | EFnCall (id, fQFnName, ltypeArgs, args) ->
      match fQFnName with
      | FQFnName.Stdlib std when std.function_ = "typeError" && List.length args > 0 ->
        match getErrorFromFnArgs args with
        | Some text ->
          EPipeForbiddenExpr(id, "", (EString(id, [ StringText(text) ])))
        | _ -> EPipeFnCall(id, fQFnName, ltypeArgs, args)
      | _ -> EPipeFnCall(id, fQFnName, ltypeArgs, args)
    | EConstructor (id, typeName, caseName, fields) ->
      EPipeConstructor(id, typeName, caseName, fields)
    | EForbiddenExpr (id, message, exp) -> EPipeForbiddenExpr(id, message, exp)
    | _ as forbiddenExp ->
      let message = "Expected a function value, got something else: "
      let id = getId forbiddenExp
      EPipeForbiddenExpr(id, message, forbiddenExp)


  /// Convert PipeExpr to Regular Expr
  let toExpr (expr : PipeExpr) : Expr =
    match expr with
    | EPipeVariable (id, var) -> EVariable(id, var)
    | EPipeLambda (id, lid, expr) -> ELambda(id, lid, expr)
    | EPipeInfix (id, infix, expr1, expr2) -> EInfix(id, infix, expr1, expr2)
    | EPipeFnCall (id, fQFnName, ltypeArgs, args) ->
      EFnCall(id, fQFnName, ltypeArgs, args)
    | EPipeConstructor (id, typeName, caseName, fields) ->
      EConstructor(id, typeName, caseName, fields)
    | EPipeForbiddenExpr (id, message, exp) -> EForbiddenExpr(id, message, exp)


/// An Operation on a Canvas
///
/// "Op" is an abbreviation for Operation,
/// and is preferred throughout code and documentation.
type Op =
  | SetHandler of tlid * Handler.T
  | CreateDB of tlid * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid // CLEANUP move Deletes to API calls instead of Ops
  | SetFunction of UserFunction.T
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | SetExpr of tlid * id * Expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid // CLEANUP move Deletes to API calls instead of Ops
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * id * string
  | SetType of UserType.T
  | DeleteType of tlid // CLEANUP move Deletes to API calls instead of Ops

type Oplist = List<Op>

type TLIDOplists = List<tlid * Oplist>

module Secret =
  type T = { name : string; value : string; version : int }

module Package =
  type Parameter = { name : string; typ : DType; description : string }

  type Fn =
    { name : FQFnName.PackageFnName
      body : Expr
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : DType
      description : string
      author : string
      deprecated : bool
      tlid : tlid }


/// A built-in standard library type
type BuiltInType =
  { name : FQTypeName.StdlibTypeName
    typeParams : List<string>
    definition : CustomType.T
    description : string }
