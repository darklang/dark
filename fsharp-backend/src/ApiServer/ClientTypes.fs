/// Types used in many APIs
module ApiServer.ClientTypes

open Prelude
open Tablecloth

// ProgramTypes can be used in APIs, because that's kinda what they do. But
// RuntimeTypes are designed for the interpreter, and it's important that we're able
// to change the types without affecting APIs. So we add types here that we can
// standardize in the API without affecting the RuntimeTypes.

module RT = LibExecution.RuntimeTypes

module DType =
  type T =
    | TInt
    | TFloat
    | TBool
    | TNull
    | TStr
    | TList of T
    | TTuple of T * T * List<T>
    | TDict of T
    | TIncomplete
    | TError
    | THttpResponse of T
    | TDB of T
    | TDate
    | TChar
    | TPassword
    | TUuid
    | TOption of T
    | TErrorRail
    | TUserType of string * int
    | TBytes
    | TResult of T * T
    | TVariable of string
    | TFn of List<T> * T
    | TRecord of List<string * T>

  let rec fromRT (t : RT.DType) =
    let r = fromRT
    let rl = List.map fromRT
    match t with
    | RT.TInt -> TInt
    | RT.TFloat -> TFloat
    | RT.TBool -> TBool
    | RT.TNull -> TNull
    | RT.TStr -> TStr
    | RT.TList t -> TList(r t)
    | RT.TTuple (t1, t2, ts) -> TTuple(r t1, r t2, rl ts)
    | RT.TDict t -> TDict(r t)
    | RT.TIncomplete -> TIncomplete
    | RT.TError -> TError
    | RT.THttpResponse t -> THttpResponse(r t)
    | RT.TDB t -> TDB(r t)
    | RT.TDate -> TDate
    | RT.TChar -> TChar
    | RT.TPassword -> TPassword
    | RT.TUuid -> TUuid
    | RT.TOption t -> TOption(r t)
    | RT.TErrorRail -> TErrorRail
    | RT.TUserType (str, version) -> TUserType(str, version)
    | RT.TBytes -> TBytes
    | RT.TResult (ok, error) -> TResult(r ok, r error)
    | RT.TVariable (name) -> TVariable(name)
    | RT.TFn (ts, returnType) -> TFn(rl ts, r returnType)
    | RT.TRecord (pairs) -> TRecord(List.map (fun (k, t) -> (k, r t)) pairs)

module Dval =

  type DvalSource =
    | SourceNone
    | SourceID of tlid * id

  and DHTTP =
    | Redirect of string
    | Response of int64 * List<string * string> * T

  and T =
    | DInt of int64
    | DFloat of double
    | DBool of bool
    | DNull
    | DStr of string
    | DChar of string
    | DList of List<T>
    | DTuple of T * T * List<T>
    | DLambda // See docs/dblock-serialization.md
    | DObj of Map<string, T>
    | DError of DvalSource * string
    | DIncomplete of DvalSource
    | DErrorRail of T
    | DHttpResponse of DHTTP
    | DDB of string
    | DDate of NodaTime.LocalDateTime
    | DPassword of byte array // We are allowed serialize this here, so don't use the Password type which doesn't deserialize
    | DUuid of System.Guid
    | DOption of Option<T>
    | DResult of Result<T, T>
    | DBytes of byte array

  let rec toRT (dv : T) : RT.Dval =
    match dv with
    | DStr s -> RT.DStr s
    | DChar c -> RT.DChar c
    | DInt i -> RT.DInt i
    | DBool b -> RT.DBool b
    | DFloat f -> RT.DFloat f
    | DNull -> RT.DNull
    | DLambda ->
      RT.DFnVal(
        RT.Lambda { parameters = []; symtable = Map []; body = RT.Expr.EBlank 0UL }
      )
    | DIncomplete SourceNone -> RT.DIncomplete RT.SourceNone
    | DIncomplete (SourceID (tlid, id)) -> RT.DIncomplete(RT.SourceID(tlid, id))
    | DError (SourceNone, msg) -> RT.DError(RT.SourceNone, msg)
    | DError (SourceID (tlid, id), msg) -> RT.DError(RT.SourceID(tlid, id), msg)
    | DDate d -> RT.DDate d
    | DDB name -> RT.DDB name
    | DUuid uuid -> RT.DUuid uuid
    | DPassword pw -> RT.DPassword(Password pw)
    | DHttpResponse (Redirect url) -> RT.DHttpResponse(RT.Redirect url)
    | DHttpResponse (Response (code, headers, hdv)) ->
      RT.DHttpResponse(RT.Response(code, headers, toRT hdv))
    | DList l -> RT.DList(List.map toRT l)
    | DTuple (first, second, theRest) ->
      RT.DTuple(toRT first, toRT second, List.map toRT theRest)
    | DObj o -> RT.DObj(Map.map toRT o)
    | DOption None -> RT.DOption None
    | DOption (Some dv) -> RT.DOption(Some(toRT dv))
    | DResult (Ok dv) -> RT.DResult(Ok(toRT dv))
    | DResult (Error dv) -> RT.DResult(Error(toRT dv))
    | DErrorRail dv -> RT.DErrorRail(toRT dv)
    | DBytes bytes -> RT.DBytes bytes


  let rec fromRT (dv : RT.Dval) : T =
    match dv with
    | RT.DStr s -> DStr s
    | RT.DChar c -> DChar c
    | RT.DInt i -> DInt i
    | RT.DBool b -> DBool b
    | RT.DFloat f -> DFloat f
    | RT.DNull -> DNull
    | RT.DFnVal _ -> DLambda
    | RT.DIncomplete RT.SourceNone -> DIncomplete SourceNone
    | RT.DIncomplete (RT.SourceID (tlid, id)) -> DIncomplete(SourceID(tlid, id))
    | RT.DError (RT.SourceNone, msg) -> DError(SourceNone, msg)
    | RT.DError (RT.SourceID (tlid, id), msg) -> DError(SourceID(tlid, id), msg)
    | RT.DDate d -> DDate d
    | RT.DDB name -> DDB name
    | RT.DUuid uuid -> DUuid uuid
    | RT.DPassword (Password pw) -> DPassword pw
    | RT.DHttpResponse (RT.Redirect url) -> DHttpResponse(Redirect url)
    | RT.DHttpResponse (RT.Response (code, headers, hdv)) ->
      DHttpResponse(Response(code, headers, fromRT hdv))
    | RT.DList l -> DList(List.map fromRT l)
    | RT.DTuple (first, second, theRest) ->
      DTuple(fromRT first, fromRT second, List.map fromRT theRest)
    | RT.DObj o -> DObj(Map.map fromRT o)
    | RT.DOption None -> DOption None
    | RT.DOption (Some dv) -> DOption(Some(fromRT dv))
    | RT.DResult (Ok dv) -> DResult(Ok(fromRT dv))
    | RT.DResult (Error dv) -> DResult(Error(fromRT dv))
    | RT.DErrorRail dv -> DErrorRail(fromRT dv)
    | RT.DBytes bytes -> DBytes bytes
