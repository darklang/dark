/// Runtime Types used for client-server communication so we may update backend
/// types without affecting APIs.
///
/// These should all directly match `RuntimeTypes.res` in `client`.
/// See `RuntimeTypes.fs` for documentation of these types.
module ClientTypes.Runtime

open Prelude
open Tablecloth

/// Used to reference a type defined by a User, Standard Library module, or Package
module FQTypeName =
  type StdlibTypeName = { typ : string }

  /// A type written by a Developer in their canvas
  type UserTypeName = { typ : string; version : int }

  type T =
    | Stdlib of StdlibTypeName
    | User of UserTypeName


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
  | TString
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


type LetPattern = LPVariable of id * name : string


type MatchPattern =
  | MPVariable of id * string
  | MPConstructor of id * caseName : string * fieldPatterns : List<MatchPattern>
  | MPInteger of id * int64
  | MPBool of id * bool
  | MPChar of id * string
  | MPString of id * string
  | MPFloat of id * double
  | MPUnit of id
  | MPTuple of id * MatchPattern * MatchPattern * List<MatchPattern>
  | MPList of id * List<MatchPattern>


module Expr =
  type T =
    | EInt of id * int64
    | EBool of id * bool
    | EString of id * List<StringSegment>
    | EChar of id * string
    | EFloat of id * double
    | EUnit of id
    | ELet of id * LetPattern * T * T
    | EIf of id * T * T * T
    | ELambda of id * List<id * string> * T
    | EFieldAccess of id * T * string
    | EVariable of id * string
    | EApply of id * FnTarget * List<DType> * List<T>
    | EList of id * List<T>
    | ETuple of id * T * T * List<T>
    | ERecord of id * typeName : Option<FQTypeName.T> * fields : List<string * T>
    | EConstructor of
      id *
      typeName : Option<FQTypeName.T> *
      caseName : string *
      fields : List<T>
    | EMatch of id * T * List<MatchPattern * T>
    | EFeatureFlag of id * T * T * T
    | EAnd of id * T * T
    | EOr of id * T * T

  and StringSegment =
    | StringText of string
    | StringInterpolation of T

  and FnTarget =
    | FnName of FQFnName.T
    | FnTargetExpr of T


module Dval =
  type DvalSource =
    | SourceNone
    | SourceID of tlid * id

  and Symtable = Map<string, T>

  and LambdaImpl =
    { parameters : List<id * string>
      symtable : Symtable
      body : Expr.T }

  and FnValImpl = Lambda of LambdaImpl

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
    | DDict of Map<string, T>
    | DRecord of Map<string, T> // CLEANUP add type name
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
    | DConstructor of
      typeName : Option<FQTypeName.T> *
      caseName : string *
      fields : List<T>
