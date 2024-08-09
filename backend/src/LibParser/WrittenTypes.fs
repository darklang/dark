/// The types that the user writes. Think of this as the Syntax Tree.
module LibParser.WrittenTypes

open Prelude

// Unless otherwise noted, all types in this file correspond pretty directly to
// LibExecution.ProgramTypes.

type Name =
  // Used when a syntactic construct turns into a function (e.g. some operators)
  | KnownBuiltin of string * int
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
// However, in addition to the TypeName, EEnums have a case name required (while
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
  | LPUnit of id
  | LPVariable of id * name : string
  | LPTuple of
    id *
    first : LetPattern *
    second : LetPattern *
    theRest : List<LetPattern>

type MatchPattern =
  | MPUnit of id
  | MPBool of id * bool
  | MPInt64 of id * int64
  | MPUInt64 of id * uint64
  | MPInt8 of id * int8
  | MPUInt8 of id * uint8
  | MPInt16 of id * int16
  | MPUInt16 of id * uint16
  | MPInt32 of id * int32
  | MPUInt32 of id * uint32
  | MPInt128 of id * System.Int128
  | MPUInt128 of id * System.UInt128
  | MPFloat of id * Sign * string * string
  | MPChar of id * string
  | MPString of id * string

  | MPList of id * List<MatchPattern>
  | MPListCons of id * head : MatchPattern * tail : MatchPattern
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

  | MPVariable of id * string

  | MPEnum of id * caseName : string * fieldPats : List<MatchPattern>

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
  | TUnit
  | TBool
  | TInt64
  | TUInt64
  | TInt8
  | TUInt8
  | TInt16
  | TUInt16
  | TInt32
  | TUInt32
  | TInt128
  | TUInt128
  | TFloat
  | TChar
  | TString
  | TDateTime
  | TUuid

  | TList of TypeReference
  | TTuple of TypeReference * TypeReference * List<TypeReference>
  | TDict of TypeReference
  | TCustomType of Name * typeArgs : List<TypeReference>

  | TFn of NEList<TypeReference> * TypeReference

  | TDB of TypeReference

  | TVariable of string


type Expr =
  | EUnit of id
  | EBool of id * bool
  | EInt64 of id * int64
  | EUInt64 of id * uint64
  | EInt8 of id * int8
  | EUInt8 of id * uint8
  | EInt16 of id * int16
  | EUInt16 of id * uint16
  | EInt32 of id * int32
  | EUInt32 of id * uint32
  | EInt128 of id * System.Int128
  | EUInt128 of id * System.UInt128
  | EFloat of id * Sign * string * string
  | EChar of id * string
  | EString of id * List<StringSegment>

  | EList of id * List<Expr>
  | EDict of id * List<string * Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | ERecord of id * Name * List<string * Expr>
  | ERecordUpdate of id * record : Expr * updates : NEList<string * Expr>
  | EEnum of
    id *
    typeName : UnresolvedEnumTypeName *
    caseName : string *
    fields : List<Expr>

  | ELet of id * LetPattern * Expr * Expr
  | EVariable of id * string
  | ERecordFieldAccess of id * Expr * string

  | EIf of id * cond : Expr * thenExpr : Expr * elseExpr : Option<Expr>
  // CLEANUP: why is this not an NEList?
  | EPipe of id * Expr * List<PipeExpr>
  | EMatch of id * arg : Expr * cases : List<MatchCase>

  | EFnName of id * Name
  | EInfix of id * Infix * Expr * Expr
  | ELambda of id * pats : NEList<LetPattern> * body : Expr
  | EApply of id * Expr * typeArgs : List<TypeReference> * args : NEList<Expr>

  | EPlaceHolder // Used to start exprs that aren't filled in yet, not in ProgramTypes

and MatchCase = { pat : MatchPattern; whenCondition : Option<Expr>; rhs : Expr }

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

and PipeExpr =
  | EPipeInfix of id * Infix * Expr

  | EPipeLambda of id * pats : NEList<LetPattern> * body : Expr

  | EPipeEnum of
    id *
    typeName : UnresolvedEnumTypeName *
    caseName : string *
    fields : List<Expr>

  | EPipeFnCall of
    id *
    // CLEANUP: should this be an Expr?
    // let something = {add = /x -> x + 1}
    // 1 |> something.add 1
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
  | EPipeVariableOrFnCall of id * string


type Const =
  | CUnit
  | CBool of bool
  | CInt64 of int64
  | CUInt64 of uint64
  | CInt8 of int8
  | CUInt8 of uint8
  | CInt16 of int16
  | CUInt16 of uint16
  | CInt32 of int32
  | CUInt32 of uint32
  | CInt128 of System.Int128
  | CUInt128 of System.UInt128
  | CFloat of Sign * string * string
  | CChar of string
  | CString of string
  | CList of List<Const>
  | CDict of List<string * Const>
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


module PackageType =
  type Name = { owner : string; modules : List<string>; name : string }

  type PackageType =
    { name : Name; declaration : TypeDeclaration.T; description : string }

module PackageConstant =
  type Name = { owner : string; modules : List<string>; name : string }

  type PackageConstant = { name : Name; description : string; body : Const }

module PackageFn =
  type Name = { owner : string; modules : List<string>; name : string }

  type Parameter = { name : string; typ : TypeReference; description : string }

  type PackageFn =
    { name : Name
      body : Expr
      typeParams : List<string>
      parameters : NEList<Parameter>
      returnType : TypeReference
      description : string }


module DB =
  type T = { name : string; version : int; typ : TypeReference }

module Handler =
  type CronInterval =
    | EveryFortnight
    | EveryWeek
    | EveryDay
    | Every12Hours
    | EveryHour
    | EveryMinute

  type Spec =
    | HTTP of route : string * method : string
    | Worker of name : string
    | Cron of name : string * interval : CronInterval
    | REPL of name : string

  type T = { ast : Expr; spec : Spec }
