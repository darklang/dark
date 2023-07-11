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
  | TVariable of string
  | TFn of List<TypeReference> * TypeReference
  | TCustomType of PT.TypeName.T * typeArgs : List<TypeReference>


/// Expressions - the main part of the language.
type Expr =
  | EInt of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | EChar of id * string
  | EFloat of id * Sign * string * string
  | EUnit of id
  | ELet of id * LetPattern * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EInfix of id * Infix * Expr * Expr
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
  | EEnum of id * typeName : PT.TypeName.T * caseName : string * fields : List<Expr>
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

  type T = { ast : Expr; spec : Spec }


module DB =
  type T = { name : string; version : int; typ : TypeReference }

module UserType =
  type T =
    { name : PT.TypeName.UserProgram
      declaration : TypeDeclaration.T
      description : string }

module UserFunction =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { name : PT.FnName.UserProgram
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

module Secret =
  type T = { name : string; value : string; version : int }

module PackageFn =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { name : PT.FnName.Package
      body : Expr
      typeParams : List<string>
      parameters : List<Parameter>
      returnType : TypeReference
      description : string }

module PackageType =
  type T =
    { name : PT.TypeName.Package
      declaration : TypeDeclaration.T
      description : string }
