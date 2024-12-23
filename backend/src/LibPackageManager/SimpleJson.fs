/// Minimal support for:
/// - parsing a JSON string into a Json value
/// - decoding a Json value into some F# type
///
/// Note: assumes types generally match the shape of the Json pretty well
///
/// This is basically Thoth.Json,
/// but with some differences on things that bugged me about Thoth.
///
/// We need this because our application is AOT-compiled,
/// and System.Text.Json's reflection-based approach won't work in such an application.
///
/// In this,
/// - 'parsing' and 'decoding' are separate steps,
///   where 'parsing' is the act of converting a string into a Json value,
///   and 'decoding' is the act of converting a Json value into some F# type
/// - 'deserializing' is when you do both
///
/// these aren't universal definitions, but they're the ones I'm using here,
/// because it's otherwise confusing to talk about this stuff.
module LibPackageManager.SimpleJson

open System.Text.Json

type Json =
  | Null
  | Bool of bool
  | Number of double
  | String of string
  | Array of List<Json>
  | Object of List<string * Json>

type JsonPathPart =
  | Index of int
  | Field of string

/// A single 'root' is always implied
type JsonPath = List<JsonPathPart>


// -- PARSING --

type JsonParseError = | NotJson
exception JsonParseException of JsonParseError

let private dotnetParsingOptions =
  new JsonDocumentOptions(
    CommentHandling = JsonCommentHandling.Skip,
    MaxDepth = System.Int32.MaxValue
  )

let parseJson (str : string) : Result<Json, JsonParseError> =
  let rec convert (j : JsonElement) : Json =
    match j.ValueKind with
    | JsonValueKind.Null -> Json.Null

    | JsonValueKind.True -> Json.Bool true
    | JsonValueKind.False -> Json.Bool false

    | JsonValueKind.Number -> j.GetDouble() |> Json.Number

    | JsonValueKind.String -> j.GetString() |> Json.String

    | JsonValueKind.Array ->
      j.EnumerateArray() |> Seq.map convert |> Seq.toList |> Json.Array

    | JsonValueKind.Object ->
      j.EnumerateObject()
      |> Seq.map (fun jp -> (jp.Name, convert jp.Value))
      |> Seq.toList
      |> Json.Object

    | _ -> raise (JsonParseException JsonParseError.NotJson)

  // .net does the hard work of actually parsing the JSON
  let parsedByDotNet =
    try
      Ok(JsonDocument.Parse(str, dotnetParsingOptions).RootElement)
    with _ex ->
      Error JsonParseError.NotJson

  match parsedByDotNet with
  | Error err -> Error err
  | Ok parsed ->
    try
      Ok(convert parsed)
    with JsonParseException ex ->
      Error ex


// -- DECODING --
type DecodingContext =
  { path : JsonPath
    json : Json }

  /// Represents traversing into the json via some index
  ///
  /// i.e. `root[0]`
  member this.Index (index : int) (json : Json) =
    { path = Index index :: this.path; json = json }

  /// Represents traversing into the json via some field
  ///
  /// i.e. `root.fieldName`
  member this.Field (fieldName : string) (json : Json) =
    { path = Field fieldName :: this.path; json = json }


type JsonDecodeError = DecodingContext * string
type JsonDecodeResult<'T> = Result<'T, JsonDecodeError>

type JsonDecoder<'T> = DecodingContext -> JsonDecodeResult<'T>
exception JsonDecodeException of JsonDecodeError


module Decoders =
  // TODO: some of these are untested and almost definitely broken
  // (i.e. we'll raise an exception on uint8 upon a Number that's too big)

  let value (a : 'a) : JsonDecoder<'a> = fun _ctx -> Ok a

  let bool : JsonDecoder<bool> =
    fun ctx ->
      match ctx.json with
      | Bool b -> Ok b
      | _ -> Error(ctx, "Expected bool")

  let uint8 : JsonDecoder<uint8> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(uint8 n)
      | _ -> Error(ctx, "Expected uint8")

  let int8 : JsonDecoder<int8> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(int8 n)
      | _ -> Error(ctx, "Expected int8")

  let uint16 : JsonDecoder<uint16> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(uint16 n)
      | _ -> Error(ctx, "Expected uint16")

  let int16 : JsonDecoder<int16> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(int16 n)
      | _ -> Error(ctx, "Expected int16")

  let uint32 : JsonDecoder<uint32> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(uint32 n)
      | _ -> Error(ctx, "Expected uint32")

  let int32 : JsonDecoder<int32> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(int32 n)
      | _ -> Error(ctx, "Expected int32")

  let uint64 : JsonDecoder<uint64> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(uint64 n)
      | _ -> Error(ctx, "Expected uint64")

  let int64 : JsonDecoder<int64> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(int64 n)
      | _ -> Error(ctx, "Expected int64")

  let uint128 : JsonDecoder<System.UInt128> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(System.UInt128.Parse(string n))
      | _ -> Error(ctx, "Expected uint128")

  let int128 : JsonDecoder<System.Int128> =
    fun ctx ->
      match ctx.json with
      | Number n -> Ok(System.Int128.Parse(string n))
      | _ -> Error(ctx, "Expected int128")

  let pair (d1 : JsonDecoder<'T1>) (d2 : JsonDecoder<'T2>) : JsonDecoder<'T1 * 'T2> =
    fun ctx ->
      match ctx.json with
      | Array [ f1; f2 ] ->
        match d1 (ctx.Index 0 f1), d2 (ctx.Index 1 f2) with
        | Ok f1, Ok f2 -> Ok(f1, f2)
        | Error err, _
        | _, Error err -> Error err
      | _ -> Error(ctx, "Expected two fields")

  let string : JsonDecoder<string> =
    fun ctx ->
      match ctx.json with
      | String s -> Ok s
      | _ -> Error(ctx, "Expected string")

  let uuid : JsonDecoder<System.Guid> =
    fun ctx ->
      match ctx.json with
      | String s ->
        match System.Guid.TryParse(s) with
        | true, guid -> Ok guid
        | _ -> Error(ctx, "Expected guid")
      | _ -> Error(ctx, "Expected guid")


  let enum0Fields (ctor : 'T) (fields : List<Json>) : JsonDecoder<'T> =
    fun ctx ->
      match fields with
      | [] -> Ok ctor
      | _ -> Error(ctx, "Expected one field")

  let enum1Field
    (d1 : JsonDecoder<'T1>)
    (ctor : 'T1 -> 'T)
    (fields : List<Json>)
    : JsonDecoder<'T> =
    fun ctx ->
      match fields with
      | [ f1 ] -> d1 (ctx.Index 0 f1) |> Result.map ctor
      | _ -> Error(ctx, "Expected one field")

  let enum2Fields
    (d1 : JsonDecoder<'T1>)
    (d2 : JsonDecoder<'T2>)
    (ctor : 'T1 -> 'T2 -> 'T)
    (fields : List<Json>)
    : JsonDecoder<'T> =
    fun ctx ->
      match fields with
      | [ f1; f2 ] ->
        match d1 (ctx.Index 1 f1), d2 (ctx.Index 2 f2) with
        | Ok f1, Ok f2 -> Ok(ctor f1 f2)
        | Error err, _
        | _, Error err -> Error err
      | _ -> Error(ctx, "Expected two fields")

  let enum3Fields
    (d1 : JsonDecoder<'T1>)
    (d2 : JsonDecoder<'T2>)
    (d3 : JsonDecoder<'T3>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T)
    (fields : List<Json>)
    : JsonDecoder<'T> =
    fun ctx ->
      match fields with
      | [ f1; f2; f3 ] ->
        match d1 (ctx.Index 1 f1), d2 (ctx.Index 2 f2), d3 (ctx.Index 3 f3) with
        | Ok f1, Ok f2, Ok f3 -> Ok(ctor f1 f2 f3)
        | Error err, _, _
        | _, Error err, _
        | _, _, Error err -> Error err
      | _ -> Error(ctx, "Expected three fields")

  let enum4Fields
    (d1 : JsonDecoder<'T1>)
    (d2 : JsonDecoder<'T2>)
    (d3 : JsonDecoder<'T3>)
    (d4 : JsonDecoder<'T4>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T)
    (fields : List<Json>)
    : JsonDecoder<'T> =
    fun ctx ->
      match fields with
      | [ f1; f2; f3; f4 ] ->
        match
          d1 (ctx.Index 1 f1),
          d2 (ctx.Index 2 f2),
          d3 (ctx.Index 3 f3),
          d4 (ctx.Index 4 f4)
        with
        | Ok f1, Ok f2, Ok f3, Ok f4 -> Ok(ctor f1 f2 f3 f4)
        | Error err, _, _, _
        | _, Error err, _, _
        | _, _, Error err, _
        | _, _, _, Error err -> Error err
      | _ -> Error(ctx, "Expected four fields")

  let enum5Fields
    (d1 : JsonDecoder<'T1>)
    (d2 : JsonDecoder<'T2>)
    (d3 : JsonDecoder<'T3>)
    (d4 : JsonDecoder<'T4>)
    (d5 : JsonDecoder<'T5>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T)
    (fields : List<Json>)
    : JsonDecoder<'T> =
    fun ctx ->
      match fields with
      | [ f1; f2; f3; f4; f5 ] ->
        match
          d1 (ctx.Index 1 f1),
          d2 (ctx.Index 2 f2),
          d3 (ctx.Index 3 f3),
          d4 (ctx.Index 4 f4),
          d5 (ctx.Index 5 f5)
        with
        | Ok f1, Ok f2, Ok f3, Ok f4, Ok f5 -> Ok(ctor f1 f2 f3 f4 f5)
        | Error err, _, _, _, _
        | _, Error err, _, _, _
        | _, _, Error err, _, _
        | _, _, _, Error err, _
        | _, _, _, _, Error err -> Error err
      | _ -> Error(ctx, "Expected four fields")


  let du (cases : Map<string, List<Json> -> JsonDecoder<'T>>) : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object [ (caseName, Array fields) ] ->
        match Map.tryFind caseName cases with
        | Some decoder -> decoder fields ctx
        | None -> Error(ctx, sprintf "Unknown enum case: %s" caseName)
      | _ -> Error(ctx, "Expected enum to be an object with 1 key")

  // this is an enum - NOT expecting null, but rather a DU setup
  let option (innerDecoder : JsonDecoder<'T>) : JsonDecoder<'T option> =
    fun ctx ->
      match ctx.json with
      | Object [ (caseName, Array fields) ] ->
        match caseName with
        | "None" -> enum0Fields None fields ctx
        | "Some" -> enum1Field innerDecoder Some fields ctx
        | _ -> Error(ctx, sprintf "Unknown Option case: %s" caseName)
      | _ -> Error(ctx, "Option enum should be an object with 1 key")

  let result
    (okDecoder : JsonDecoder<'T1>)
    (errDecoder : JsonDecoder<'T2>)
    : JsonDecoder<Result<'T1, 'T2>> =
    fun ctx ->
      match ctx.json with
      | Object [ (caseName, Array fields) ] ->
        match caseName with
        | "Ok" -> enum1Field okDecoder Ok fields ctx
        | "Error" -> enum1Field errDecoder Error fields ctx
        | _ -> Error(ctx, sprintf "Unknown Result case: %s" caseName)
      | _ -> Error(ctx, "Result enum should be an object with 1 key")


  let list (innerDecoder : JsonDecoder<'T>) : JsonDecoder<List<'T>> =
    fun ctx ->
      match ctx.json with
      | Array items ->
        (List.foldWithIndex
          (fun i acc item ->
            match acc with
            | Error err -> Error err
            | Ok decodedItems ->
              innerDecoder (ctx.Index i item)
              |> Result.map (fun d -> d :: decodedItems))
          (Ok [])
          items)
        |> Result.map List.reverse

      | _ -> Error(ctx, "Expected a list")


  let decodeField (fields : List<string * Json>) (d : string * JsonDecoder<'T>) =
    fun (ctx : DecodingContext) ->
      let fieldName, decoder = d

      match fields |> List.find (fun (k, _v) -> k = fieldName) with
      | None -> Error(ctx, "missing field: " + fieldName)
      | Some(_, found) ->
        let fieldCtx = ctx.Field fieldName found
        match decoder fieldCtx with
        | Ok decoded -> Ok decoded
        | Error _ -> Error(fieldCtx, "field of wrong type: " + fieldName)


  let obj1Field
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (ctor : 'T1 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        match decodeField fields d1 ctx with
        | Ok f1 -> Ok(ctor f1)
        | Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)

  let obj2Fields
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (d2 : string * JsonDecoder<'T2>)
    (ctor : 'T1 -> 'T2 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        match decodeField fields d1 ctx, decodeField fields d2 ctx with
        | Ok f1, Ok f2 -> Ok(ctor f1 f2)
        | Error err, _
        | _, Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)

  let obj3Fields
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (d2 : string * JsonDecoder<'T2>)
    (d3 : string * JsonDecoder<'T3>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        let f1 = decodeField fields d1 ctx
        let f2 = decodeField fields d2 ctx
        let f3 = decodeField fields d3 ctx

        match f1, f2, f3 with
        | Ok f1, Ok f2, Ok f3 -> Ok(ctor f1 f2 f3)
        | Error err, _, _
        | _, Error err, _
        | _, _, Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)

  let obj4Fields
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (d2 : string * JsonDecoder<'T2>)
    (d3 : string * JsonDecoder<'T3>)
    (d4 : string * JsonDecoder<'T4>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        let f1 = decodeField fields d1 ctx
        let f2 = decodeField fields d2 ctx
        let f3 = decodeField fields d3 ctx
        let f4 = decodeField fields d4 ctx

        match f1, f2, f3, f4 with
        | Ok f1, Ok f2, Ok f3, Ok f4 -> Ok(ctor f1 f2 f3 f4)
        | Error err, _, _, _
        | _, Error err, _, _
        | _, _, Error err, _
        | _, _, _, Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)

  let obj5Fields
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (d2 : string * JsonDecoder<'T2>)
    (d3 : string * JsonDecoder<'T3>)
    (d4 : string * JsonDecoder<'T4>)
    (d5 : string * JsonDecoder<'T5>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        let f1 = decodeField fields d1 ctx
        let f2 = decodeField fields d2 ctx
        let f3 = decodeField fields d3 ctx
        let f4 = decodeField fields d4 ctx
        let f5 = decodeField fields d5 ctx

        match f1, f2, f3, f4, f5 with
        | Ok f1, Ok f2, Ok f3, Ok f4, Ok f5 -> Ok(ctor f1 f2 f3 f4 f5)
        | Error err, _, _, _, _
        | _, Error err, _, _, _
        | _, _, Error err, _, _
        | _, _, _, Error err, _
        | _, _, _, _, Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)

  let obj6Fields
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (d2 : string * JsonDecoder<'T2>)
    (d3 : string * JsonDecoder<'T3>)
    (d4 : string * JsonDecoder<'T4>)
    (d5 : string * JsonDecoder<'T5>)
    (d6 : string * JsonDecoder<'T6>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        let f1 = decodeField fields d1 ctx
        let f2 = decodeField fields d2 ctx
        let f3 = decodeField fields d3 ctx
        let f4 = decodeField fields d4 ctx
        let f5 = decodeField fields d5 ctx
        let f6 = decodeField fields d6 ctx

        match f1, f2, f3, f4, f5, f6 with
        | Ok f1, Ok f2, Ok f3, Ok f4, Ok f5, Ok f6 -> Ok(ctor f1 f2 f3 f4 f5 f6)
        | Error err, _, _, _, _, _
        | _, Error err, _, _, _, _
        | _, _, Error err, _, _, _
        | _, _, _, Error err, _, _
        | _, _, _, _, Error err, _
        | _, _, _, _, _, Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)

  let obj8Fields
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (d2 : string * JsonDecoder<'T2>)
    (d3 : string * JsonDecoder<'T3>)
    (d4 : string * JsonDecoder<'T4>)
    (d5 : string * JsonDecoder<'T5>)
    (d6 : string * JsonDecoder<'T6>)
    (d7 : string * JsonDecoder<'T7>)
    (d8 : string * JsonDecoder<'T8>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'T7 -> 'T8 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        let f1 = decodeField fields d1 ctx
        let f2 = decodeField fields d2 ctx
        let f3 = decodeField fields d3 ctx
        let f4 = decodeField fields d4 ctx
        let f5 = decodeField fields d5 ctx
        let f6 = decodeField fields d6 ctx
        let f7 = decodeField fields d7 ctx
        let f8 = decodeField fields d8 ctx

        match f1, f2, f3, f4, f5, f6, f7, f8 with
        | Ok f1, Ok f2, Ok f3, Ok f4, Ok f5, Ok f6, Ok f7, Ok f8 ->
          Ok(ctor f1 f2 f3 f4 f5 f6 f7 f8)
        | Error err, _, _, _, _, _, _, _
        | _, Error err, _, _, _, _, _, _
        | _, _, Error err, _, _, _, _, _
        | _, _, _, Error err, _, _, _, _
        | _, _, _, _, Error err, _, _, _
        | _, _, _, _, _, Error err, _, _
        | _, _, _, _, _, _, Error err, _
        | _, _, _, _, _, _, _, Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)

  let obj9Fields
    (name : string)
    (d1 : string * JsonDecoder<'T1>)
    (d2 : string * JsonDecoder<'T2>)
    (d3 : string * JsonDecoder<'T3>)
    (d4 : string * JsonDecoder<'T4>)
    (d5 : string * JsonDecoder<'T5>)
    (d6 : string * JsonDecoder<'T6>)
    (d7 : string * JsonDecoder<'T7>)
    (d8 : string * JsonDecoder<'T8>)
    (d9 : string * JsonDecoder<'T9>)
    (ctor : 'T1 -> 'T2 -> 'T3 -> 'T4 -> 'T5 -> 'T6 -> 'T7 -> 'T8 -> 'T9 -> 'T)
    : JsonDecoder<'T> =
    fun ctx ->
      match ctx.json with
      | Object fields ->
        let f1 = decodeField fields d1 ctx
        let f2 = decodeField fields d2 ctx
        let f3 = decodeField fields d3 ctx
        let f4 = decodeField fields d4 ctx
        let f5 = decodeField fields d5 ctx
        let f6 = decodeField fields d6 ctx
        let f7 = decodeField fields d7 ctx
        let f8 = decodeField fields d8 ctx
        let f9 = decodeField fields d9 ctx

        match f1, f2, f3, f4, f5, f6, f7, f8, f9 with
        | Ok f1, Ok f2, Ok f3, Ok f4, Ok f5, Ok f6, Ok f7, Ok f8, Ok f9 ->
          Ok(ctor f1 f2 f3 f4 f5 f6 f7 f8 f9)
        | Error err, _, _, _, _, _, _, _, _
        | _, Error err, _, _, _, _, _, _, _
        | _, _, Error err, _, _, _, _, _, _
        | _, _, _, Error err, _, _, _, _, _
        | _, _, _, _, Error err, _, _, _, _
        | _, _, _, _, _, Error err, _, _, _
        | _, _, _, _, _, _, Error err, _, _
        | _, _, _, _, _, _, _, Error err, _
        | _, _, _, _, _, _, _, _, Error err -> Error err

      | _ -> Error(ctx, sprintf "Expected %s to be an object" name)


// -- DESERIALIZATION --
type JsonDeserializationError =
  | ParseError of JsonParseError
  | DecodeError of JsonDecodeError

let deserialize<'T>
  (decoder : JsonDecoder<'T>)
  (json : string)
  : Result<'T, JsonDeserializationError> =
  match parseJson json with
  | Error err -> Error(ParseError err)
  | Ok parsed ->
    match decoder { path = []; json = parsed } with
    | Ok decoded -> Ok decoded
    | Error err -> Error(DecodeError err)
