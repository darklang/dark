/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in the BwdServer, in HttpClient,
/// etc.
module LibExecution.DvalReprInternalNew

open Prelude
open VendoredTablecloth

module RT = RuntimeTypes


module RoundtrippableSerializationFormatV0 =

  // In the past, we used bespoke serialization formats, that were terrifying to
  // change. Going forward, if we want to use a format that we want to save and
  // reload, but don't need to search for, let's just use the simplest possible
  // format, using standard serializers.

  // We create our own format here because:
  // 1. we don't want to serialize some things, such as lambdas. Our own type allows
  //    us to be careful
  // 2. This needs to be backwards compatible, but we don't want to constrain how we
  //    change RT.Dval.

  type DvalMap = Map<string, Dval>
  and DvalSource =
    | SourceNone
    | SourceID of tlid * id

  and DHTTP =
    | Redirect of string
    | Response of int64 * List<string * string> * Dval

  and Dval =
    | DInt of int64
    | DFloat of double
    | DBool of bool
    | DNull
    | DStr of string
    | DChar of string
    | DList of List<Dval>
    | DTuple of Dval * Dval * List<Dval>
    | DLambda // See docs/dblock-serialization.md
    | DObj of DvalMap
    | DError of DvalSource * string
    | DIncomplete of DvalSource
    | DErrorRail of Dval
    | DHttpResponse of DHTTP
    | DDB of string
    | DDate of NodaTime.LocalDateTime
    | DPassword of byte array // We are allowed serialize this here, so don't use the Password type which doesn't deserialize
    | DUuid of System.Guid
    | DOption of Option<Dval>
    | DResult of Result<Dval, Dval>
    | DBytes of byte array

  let rec toRT (dv : Dval) : RT.Dval =
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


  let rec fromRT (dv : RT.Dval) : Dval =
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


let toRoundtrippableJsonV0 (dv : RT.Dval) : string =
  dv |> RoundtrippableSerializationFormatV0.fromRT |> Json.Vanilla.serialize

let parseRoundtrippableJsonV0 (json : string) : RT.Dval =
  json
  |> Json.Vanilla.deserialize<RoundtrippableSerializationFormatV0.Dval>
  |> RoundtrippableSerializationFormatV0.toRT

let toHashV2 (dvals : list<RT.Dval>) : string =
  dvals
  |> List.map RoundtrippableSerializationFormatV0.fromRT
  |> RoundtrippableSerializationFormatV0.DList
  |> Json.Vanilla.serialize
  |> UTF8.toBytes
  |> System.IO.Hashing.XxHash64.Hash // fastest in .NET, does not need to be secure
  |> Base64.urlEncodeToString



module Test =
  let rec isRoundtrippableDval (dval : RT.Dval) : bool =
    match dval with
    | RT.DFnVal _ -> false // not supported
    | RT.DStr _
    | RT.DInt _
    | RT.DFloat _
    | RT.DNull _
    | RT.DBool _
    | RT.DChar _
    | RT.DBytes _
    | RT.DDate _
    | RT.DOption None
    | RT.DPassword _ -> true
    | RT.DList dvals -> List.all isRoundtrippableDval dvals
    | RT.DObj map -> map |> Map.values |> List.all isRoundtrippableDval
    | RT.DUuid _ -> true
    | RT.DTuple (v1, v2, rest) -> List.all isRoundtrippableDval (v1 :: v2 :: rest)
    | RT.DOption (Some v)
    | RT.DHttpResponse (RT.Response (_, _, v))
    | RT.DResult (Error v)
    | RT.DResult (Ok v) -> isRoundtrippableDval v
    | RT.DHttpResponse (RT.Redirect _) -> true
    | RT.DDB _
    | RT.DError _
    | RT.DIncomplete _
    | RT.DErrorRail _ -> true
