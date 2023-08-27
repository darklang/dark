/// The types that the user writes. Think of this as the Syntax Tree.
module LibParser.WrittenTypes

open Prelude

// Unless otherwise noted, all types in this file correspond pretty directly to
// LibExecution.ProgramTypes.

// TODO: stop using ProgramTypes
// We borrow this for now to use FQNames, but they will be removed soon
module PT = LibExecution.ProgramTypes

type Name =
  // Used when a syntactic construct turns into a function (eg some operators)
  | KnownBuiltin of List<string> * string * int
  // Basically all names are unresolved at this point, and will be resolved during
  // WrittenTypesToProgramTypes
  | Unresolved of NEList<string>

// Ideally, we'd use a Name for the Enum typenames, like we do for ERecords and
// EFnNames. However there are a number of problems of reusing it, that either
// aren't problems for ERecord typenames and EFnNames, or conflict with them.
//
// The core problem that we're trying to model is that a Name is something the
// user types, and the fact that users must type _something_. For DRecords or
// EFnNames, that "at least 1 thing" is well modelled as an NEList, which avoids
// a ton of error handling of impossible states from using empty lists (which
// the users shouldn't even be able to type).
//
// However, in addition to the TypeName, EEnums have Casenames casenames (while
// EFnName and DRecord typenames don't). If we use an NEList for the typename
// alone, that doesn't allow someone to type `Ok`, which is allowed. (It doesn't
// resolve to anything but we want to allow them to type it all the same.
//
// Looking at the four possible ways we could use Names to model EEnum:
//
// Name as List, plus separate casename field in EEnum:
//  - ✅ supports [] + caseName which is valid for EEnums
//  - ❌ supports [] for FnName and DRecord, which is invalid.
//  - ✅ KnownBuiltin will work in EEnums

// Name as List, including casename (no separate caseName field in EEnum):
//  - 🤔 supports [] which is invalid but won't appear in practice so we can error
//
// Name as NEList, plus separate casename field in EEnum:
//  - ❌ doesn't support [ "Ok" ] - can't do this
//
// Name as NEList, including casename (no separate caseName field in EEnum):
//  - ❌ KnownBuiltin won't work as we won't have a caseName field, but we can
//       error safely since we just won't do this
//
// Alternative: don't use Name for EEnum, and instead use List<string> plus a
// separate caseName field.
type UnresolvedEnumTypeName = List<string>


type LetPattern =
  | LPVariable of id * name : string
  | LPUnit of id
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

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
  // TODO
  // | Named of Name * typeArgs : List<TypeReference>
  // | Fn of int // ...
  // | Variable of string
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
  | TFn of NEList<TypeReference> * TypeReference
  | TCustomType of Name * typeArgs : List<TypeReference>


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
  | ELambda of id * NEList<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EApply of id * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>
  | EList of id * List<Expr>
  | EDict of id * List<string * Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | EPipe of id * Expr * List<PipeExpr>
  | ERecord of id * Name * List<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : NEList<string * Expr>

  | EEnum of
    id *
    typeName : UnresolvedEnumTypeName *
    caseName : string *
    fields : List<Expr>
  | EMatch of id * arg : Expr * cases : List<MatchPattern * Expr>
  | EFnName of id * Name
  | EPlaceHolder // Used to start exprs that aren't filled in yet, not in ProgramTypes

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and PipeExpr =
  | EPipeInfix of id * Infix * Expr

  | EPipeLambda of id * NEList<id * string> * Expr

  | EPipeEnum of
    id *
    typeName : UnresolvedEnumTypeName *
    caseName : string *
    fields : List<Expr>

  | EPipeFnCall of
    id *
    fnName : Name *
    typeArgs : List<TypeReference> *
    args : List<Expr>

  /// When parsing, the following is a bit ambiguous:
  ///   `dir |> listDirectoryRecursive`
  ///
  /// It could either be a local variable,
  ///   or a user function with only one argument or type args.
  ///
  /// We resolve this ambiguity during name resolution of WT2PT.
  | EPipeVariableOrUserFunction of id * string


type Const =
  | CInt of int64
  | CBool of bool
  | CString of string
  | CChar of string
  | CFloat of Sign * string * string
  | CUnit
  | CTuple of first : Const * second : Const * rest : List<Const>
  | CEnum of typeName : UnresolvedEnumTypeName * caseName : string * List<Const>


module TypeDeclaration =
  type RecordField = { name : string; typ : TypeReference; description : string }

  type EnumField =
    { typ : TypeReference; label : Option<string>; description : string }

  type EnumCase = { name : string; fields : List<EnumField>; description : string }

  type Definition =
    | Alias of TypeReference
    | Record of NEList<RecordField>
    | Enum of NEList<EnumCase>

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
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string
      body : Expr }

module UserConstant =
  type T = { name : PT.ConstantName.UserProgram; description : string; body : Const }


module PackageFn =
  type Parameter = { name : string; typ : TypeReference; description : string }

  type T =
    { name : PT.FnName.Package
      body : Expr
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string }

module PackageType =
  type T =
    { name : PT.TypeName.Package
      declaration : TypeDeclaration.T
      description : string }

module PackageConstant =
  type T = { name : PT.ConstantName.Package; description : string; body : Const }
