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

type Position = { x : int; y : int }

module FQFnName =
  type StdlibFnName = { module_ : string; function_ : string; version : int }

  type InfixStdlibFnName = { module_ : Option<string>; function_ : string }

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

type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * string * List<MatchPattern>
  | MPInteger of id * int64
  | MPBool of id * bool
  | MPCharacter of id * string
  | MPString of id * string
  | MPFloat of id * Sign * string * string
  | MPNull of id
  | MPBlank of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

type LetPattern = LPVariable of id * name : string

type SendToRail =
  | Rail
  | NoRail

type BinaryOperation =
  | BinOpAnd
  | BinOpOr

type Infix =
  | InfixFnCall of FQFnName.InfixStdlibFnName * SendToRail
  | BinOp of BinaryOperation

type Expr =
  | EInteger of id * int64
  | EBool of id * bool
  | EString of id * string
  | ECharacter of id * string
  | EFloat of id * Sign * string * string
  | ENull of id
  | EBlank of id
  | ELet of id * string * Expr * Expr
  | ELetWithPattern of id * LetPattern * Expr * Expr
  | EIf of id * Expr * Expr * Expr
  | EInfix of id * Infix * Expr * Expr
  | ELambda of id * List<id * string> * Expr
  | EFieldAccess of id * Expr * string
  | EVariable of id * string
  | EFnCall of id * FQFnName.T * List<Expr> * SendToRail
  | EPartial of id * string * Expr
  | ERightPartial of id * string * Expr
  | ELeftPartial of id * string * Expr
  | EList of id * List<Expr>
  | ETuple of id * Expr * Expr * List<Expr>
  | ERecord of id * List<string * Expr>
  | EPipe of id * Expr * Expr * List<Expr>
  | EConstructor of id * string * List<Expr>
  | EMatch of id * Expr * List<MatchPattern * Expr>
  | EPipeTarget of id
  | EFeatureFlag of id * string * Expr * Expr * Expr


type DType =
  | TInt
  | TFloat
  | TBool
  | TNull
  | TStr
  | TList of DType
  | TTuple of DType * DType * List<DType>
  | TDict of DType
  | TIncomplete
  | TError
  | THttpResponse of DType
  | TDB of DType
  | TDate
  | TChar
  | TPassword
  | TUuid
  | TOption of DType
  | TErrorRail
  | TUserType of string * int
  | TBytes
  | TResult of DType * DType
  | TVariable of string
  | TFn of List<DType> * DType
  | TRecord of List<string * DType>
  | TDbList of DType


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
    | HTTPBasic of route : string * method : string * ids : ids
    | Worker of name : string * ids : ids
    | OldWorker of modulename : string * name : string * ids : ids
    | Cron of name : string * interval : Option<CronInterval> * ids : ids
    | REPL of name : string * ids : ids
    | UnknownHandler of string * string * ids

  type T = { tlid : tlid; pos : Position; ast : Expr; spec : Spec }


module DB =
  type Col = { name : Option<string>; typ : Option<DType>; nameID : id; typeID : id }

  type T =
    { tlid : tlid
      pos : Position
      name : string
      nameID : id
      version : int
      cols : List<Col> }


module UserType =
  type RecordField = { name : string; typ : Option<DType>; nameID : id; typeID : id }

  type Definition = Record of List<RecordField>

  type T =
    { tlid : tlid
      name : string
      nameID : id
      version : int
      definition : Definition }


module UserFunction =
  type Parameter =
    { name : string
      nameID : id
      typ : Option<DType>
      typeID : id
      description : string }

  type T =
    { tlid : tlid
      name : string
      nameID : id
      parameters : List<Parameter>
      returnType : DType
      returnTypeID : id
      description : string
      infix : bool
      body : Expr }

type Toplevel =
  | TLHandler of Handler.T
  | TLDB of DB.T
  | TLFunction of UserFunction.T
  | TLType of UserType.T

type Op =
  | SetHandler of tlid * Position * Handler.T
  | CreateDB of tlid * Position * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid
  | MoveTL of tlid * Position
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
  | CreateDBWithBlankOr of tlid * Position * id * string
  | SetType of UserType.T
  | DeleteType of tlid

type Oplist = List<Op>

type TLIDOplists = List<tlid * Oplist>

type Secret = { name : string; value : string }

module Package =
  type Parameter = { name : string; typ : DType; description : string }

  type Fn =
    { name : FQFnName.PackageFnName
      body : Expr
      parameters : List<Parameter>
      returnType : DType
      description : string
      author : string
      deprecated : bool
      tlid : tlid }
