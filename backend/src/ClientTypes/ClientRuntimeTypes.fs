/// Runtime Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// These should all directly match `RuntimeTypes.res` in `client`.
/// See `RuntimeTypes.fs` for documentation of these types.
module ClientTypes.Runtime

open Prelude
open Tablecloth


module FQFnName =
  type UserFnName = string

  type PackageFnName =
    { owner : string
      package : string
      module_ : string
      function_ : string
      version : int }

  type StdlibFnName = { module_ : string; function_ : string; version : int }

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
  | TUserType of string * int
  | TBytes
  | TResult of DType * DType
  | TVariable of string
  | TFn of List<DType> * DType
  | TRecord of List<string * DType>

type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * string * List<MatchPattern>
  | MPInteger of id * int64
  | MPBool of id * bool
  | MPCharacter of id * string
  | MPString of id * string
  | MPFloat of id * double
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>

module Expr =
  type T =
    | EInteger of id * int64
    | EBool of id * bool
    | EString of id * List<StringSegment>
    | ECharacter of id * string
    | EFloat of id * double
    | EUnit of id
    | ELet of id * string * T * T
    | EIf of id * T * T * T
    | ELambda of id * List<id * string> * T
    | EFieldAccess of id * T * string
    | EVariable of id * string
    | EApply of id * T * List<T> * IsInPipe
    | EFQFnValue of id * FQFnName.T
    | EList of id * List<T>
    | ETuple of id * T * T * List<T>
    | ERecord of id * List<string * T>
    | EConstructor of id * string * List<T>
    | EMatch of id * T * List<MatchPattern * T>
    | EFeatureFlag of id * T * T * T
    | EAnd of id * T * T
    | EOr of id * T * T

  and StringSegment =
    | StringText of string
    | StringInterpolation of T

  and IsInPipe =
    | InPipe of id
    | NotInPipe


module Dval =
  type DvalSource =
    | SourceNone
    | SourceID of tlid * id

  and Symtable = Map<string, T>

  and LambdaImpl =
    { parameters : List<id * string>
      symtable : Symtable
      body : Expr.T }

  and FnValImpl =
    | Lambda of LambdaImpl
    | FnName of FQFnName.T

  and T =
    | DInt of int64
    | DFloat of double
    | DBool of bool
    | DUnit
    | DStr of string
    | DChar of string
    | DList of List<T>
    | DTuple of T * T * List<T>
    | DFnVal of FnValImpl // See docs/dblock-serialization.md
    | DObj of Map<string, T>
    | DError of DvalSource * string
    | DIncomplete of DvalSource
    | DHttpResponse of int64 * List<string * string> * T
    | DDB of string
    | DDateTime of NodaTime.LocalDateTime
    | DPassword of Password
    | DUuid of System.Guid
    | DOption of Option<T>
    | DResult of Result<T, T>
    | DBytes of byte array
