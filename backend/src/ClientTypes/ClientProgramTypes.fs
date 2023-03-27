/// Program Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// These should all directly match `ProgramTypes.res` in `client`
/// See ProgramTypes.fs for documentation of these types
module ClientTypes.Program

open Prelude
open Tablecloth

type id = Prelude.id
type tlid = Prelude.tlid
type Sign = Prelude.Sign


/// Used to reference a type defined by a User, Standard Library module, or Package
module FQTypeName =
  type StdlibTypeName = { typ : string }

  /// A type written by a Developer in their canvas
  type UserTypeName = { typ : string; version : int }

  type T =
    | Stdlib of StdlibTypeName
    | User of UserTypeName

module FQFnName =
  type StdlibFnName = { module_ : string; function_ : string; version : int }

  type UserFnName = string

  type PackageFnName =
    { owner : string
      package : string
      module_ : string
      function_ : string
      version : int }

  type T =
    | User of UserFnName
    | Stdlib of StdlibFnName
    | Package of PackageFnName

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
  | TOption of DType
  | TCustomType of FQTypeName.T * typeArgs : List<DType>
  | TBytes
  | TResult of DType * DType
  | TVariable of string
  | TFn of List<DType> * DType
  | TRecord of List<string * DType>
  | TDbList of DType

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

type LetPattern = LPVariable of id * name : string

type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * caseName : string * fieldPatterns : List<MatchPattern>
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

type Infix =
  | InfixFnCall of InfixFnName
  | BinOp of BinaryOperation

type Expr =
  | EInteger of id * int64
  | EBool of id * bool
  | EString of id * List<StringSegment>
  | ECharacter of id * string
  | EFloat of id * Sign * string * string
  | EUnit of id
  | ELet of id * LetPattern * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EInfix of id * Infix * Expr * Expr
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * typeArgs : List<DType> * args : List<Expr>
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | ERecord of id * Option<FQTypeName.T> * List<string * Expr>
  | EPipe of id * Expr * PipeExpr * List<PipeExpr>
  | EConstructor of
    id *
    typeName : Option<FQTypeName.T> *
    caseName : string *
    fields : List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr

and PipeExpr =
  | EPipeLambda of id * List<id * string> * Expr
  | EPipeInfix of id * Infix * Expr * Expr
  | EPipeFnCall of id *
    FQFnName.T *
    typeArgs : List<DType> *
    args : List<Expr>
  | EPipeConstructor of id *
    typeName : Option<FQTypeName.T> *
    caseName : string *
    fields : List<Expr>
  | EPipeForbiddenExpr of Expr

and StringSegment =
  | StringText of string
  | StringInterpolation of Expr

module Pipe =
  /// Convert Regular Expr to PipeExpr
  let toPipeExpr (expr: Expr): PipeExpr  =
    match expr with
    | ELambda(id, lid, expr) -> EPipeLambda (id, lid, expr)
    | EInfix (id, infix, expr1, expr2) -> EPipeInfix (id, infix, expr1, expr2)
    | EFnCall (id, fQFnName, ltypeArgs, args) -> EPipeFnCall (id, fQFnName, ltypeArgs, args)
    | EConstructor (id,typeName,caseName,fields) -> EPipeConstructor (id,typeName,caseName,fields)
    | exp -> EPipeForbiddenExpr exp


  /// Convert PipeExpr to Regular Expr
  let toExpr (expr: PipeExpr): Expr =
    match expr with
    | EPipeLambda(id, lid, expr) -> ELambda (id, lid, expr)
    | EPipeInfix (id, infix, expr1, expr2) -> EInfix (id, infix, expr1, expr2)
    | EPipeFnCall (id, fQFnName, ltypeArgs, args) -> EFnCall (id, fQFnName, ltypeArgs, args)
    | EPipeConstructor (id,typeName,caseName,fields) -> EConstructor (id,typeName,caseName,fields)
    | EPipeForbiddenExpr exp -> exp

module CustomType =
  type RecordField = { id : id; name : string; typ : DType }

  type EnumField = { id : id; typ : DType; label : Option<string> }
  type EnumCase = { id : id; name : string; fields : List<EnumField> }

  type T =
    | Record of firstField : RecordField * additionalFields : List<RecordField>
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
  type Definition = CustomType.T
  type T = { tlid : tlid; name : FQTypeName.UserTypeName; definition : Definition }


module UserFunction =
  type Parameter = { id : id; name : string; typ : DType; description : string }

  type T =
    { tlid : tlid
      name : string
      returnType : DType
      typeParams : List<string>
      parameters : List<Parameter>
      description : string
      infix : bool
      body : Expr }

type Toplevel =
  | TLHandler of Handler.T
  | TLDB of DB.T
  | TLFunction of UserFunction.T
  | TLType of UserType.T

type Op =
  | SetHandler of tlid * Handler.T
  | CreateDB of tlid * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid
  | SetFunction of UserFunction.T
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | SetExpr of tlid * id * Expr
  | TLSavepoint of tlid
  | DeleteFunction of tlid
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * id * string
  | SetType of UserType.T
  | DeleteType of tlid

type Oplist = List<Op>

type TLIDOplists = List<tlid * Oplist>

type Secret = { name : string; value : string; version : int }

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
