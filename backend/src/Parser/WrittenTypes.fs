/// The types that the user writes. Think of this as the Syntax Tree.
module Parser.WrittenTypes

open Prelude

// TODO: stop using ProgramTypes
// We borrow this for now to use FQNames, but they will be removed soon
module PT = LibExecution.ProgramTypes

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
  | TVariable of string // replaces TAny
  | TFn of
    List<TypeReference> *  // CLEANUP: NonEmptyList
    TypeReference // replaces TLambda

  /// A type defined by a standard library module, a canvas/user, or a package
  /// e.g. `Result<Int, String>` is represented as `TCustomType("Result", [TInt, TString])`
  /// `typeArgs` is the list of type arguments, if any
  | TCustomType of PT.TypeName.T * typeArgs : List<TypeReference>


/// Expressions - the main part of the language.
type Expr =
  | EInt of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  /// A character is an Extended Grapheme Cluster (hence why we use a string). This
  /// is equivalent to one screen-visible "character" in Unicode.
  | EChar of id * string
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
  | EApply of id * FnTarget * typeArgs : List<TypeReference> * args : List<Expr>
  | EList of id * List<Expr>
  | EDict of id * List<string * Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | EPipe of id * Expr * PipeExpr * List<PipeExpr>

  | ERecord of id * PT.TypeName.T * List<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : List<string * Expr>

  // Enums include `Just`, `Nothing`, `Error`, `Ok`, as well
  // as user-defined enums.
  //
  /// Given an Enum type of:
  ///   `type MyEnum = A | B of int | C of int * (label: string) | D of MyEnum`
  /// , this is the expression
  ///   `C (1, "title")`
  /// represented as
  ///   `EEnum(Some UserType.MyEnum, "C", [EInt(1), EString("title")]`
  /// TODO: the UserTypeName should eventually be a non-optional TypeName.
  | EEnum of id * typeName : PT.TypeName.T * caseName : string * fields : List<Expr>

  /// Supports `match` expressions
  /// ```fsharp
  /// match x + 2 with // arg
  /// // cases
  /// | pattern -> expr
  /// | pattern -> expr
  /// | ...
  /// ```
  | EMatch of id * arg : Expr * cases : List<MatchPattern * Expr>

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and FnTarget =
  | FnTargetName of PT.FnName.T
  | FnTargetExpr of Expr

and PipeExpr =
  | EPipeVariable of id * string
  | EPipeLambda of id * List<id * string> * Expr
  | EPipeInfix of id * Infix * Expr
  | EPipeFnCall of
    id *
    PT.FnName.T *
    typeArgs : List<TypeReference> *
    args : List<Expr>
  | EPipeEnum of
    id *
    typeName : PT.TypeName.T *
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
    | ELet(id, _, _, _)
    | EIf(id, _, _, _)
    | EInfix(id, _, _, _)
    | ELambda(id, _, _)
    | EFieldAccess(id, _, _)
    | EVariable(id, _)
    | EApply(id, _, _, _)
    | EList(id, _)
    | EDict(id, _)
    | ETuple(id, _, _, _)
    | EPipe(id, _, _, _)
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
    | Record of firstField : RecordField * additionalFields : List<RecordField>

    /// `type MyEnum = A | B of int | C of int * (label: string)`
    | Enum of firstCase : EnumCase * additionalCases : List<EnumCase>

  /// Combined the RHS definition, with the list of type parameters. Eg type
  /// MyType<'a> = List<'a>
  type T = { typeParams : List<string>; definition : Definition }


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
      name : PT.TypeName.UserProgram
      declaration : TypeDeclaration.T
      description : string }


module UserFunction =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { tlid : tlid
      name : PT.FnName.UserProgram
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string
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

module Secret =
  type T = { name : string; value : string; version : int }

module PackageFn =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { tlid : tlid
      id : System.Guid
      name : PT.FnName.Package
      body : Expr
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string }

module PackageType =
  type T =
    { tlid : tlid
      id : System.Guid
      name : PT.TypeName.Package
      declaration : TypeDeclaration.T
      description : string }
