module LibExecutionStdLib.LibJson

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors
module DvalReprExternal = LibExecution.DvalReprExternal

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

// SERIALIZER_DEF STJ LibJson.OfUnknownJsonV1.parse
// Parses some JSON into some Dval
// Plan: add a Json::parse that can takes a type parameter; deprecate this fn,
// and leave it in a corner.
module OfUnknownJsonV1 =
  open System.Text.Json

  let (|JString|_|) (j : JsonElement) : Option<string> =
    match j.ValueKind with
    | JsonValueKind.String -> Some(JString(j.GetString()))
    | _ -> None

  let (|JNull|_|) (j : JsonElement) : Option<unit> =
    match j.ValueKind with
    | JsonValueKind.Null -> Some(JNull)
    | _ -> None

  let (|JInteger|_|) (j : JsonElement) : Option<int64> =
    match j.ValueKind with
    | JsonValueKind.Number ->
      try
        Some(JInteger(j.GetInt64()))
      with
      | :? System.FormatException -> None
    | _ -> None

  let (|JFloat|_|) (j : JsonElement) : Option<float> =
    match j.ValueKind with
    | JsonValueKind.Number -> Some(JFloat(j.GetDouble()))
    | _ -> None

  let (|JBoolean|_|) (j : JsonElement) : Option<bool> =
    match j.ValueKind with
    | JsonValueKind.False -> Some(JBoolean(false))
    | JsonValueKind.True -> Some(JBoolean(true))
    | _ -> None

  let (|JList|_|) (j : JsonElement) : Option<List<JsonElement>> =
    match j.ValueKind with
    | JsonValueKind.Array -> Some(JList(j.EnumerateArray() |> Seq.toList))
    | _ -> None

  let (|JObject|_|) (j : JsonElement) : Option<List<string * JsonElement>> =
    match j.ValueKind with
    | JsonValueKind.Object ->
      let list =
        j.EnumerateObject()
        |> Seq.toList
        |> List.map (fun (jp : JsonProperty) -> (jp.Name, jp.Value))
      Some(JObject(list))
    | _ -> None

  let (|JUndefined|_|) (j : JsonElement) : Option<unit> =
    match j.ValueKind with
    | JsonValueKind.Undefined -> Some()
    | _ -> None

  let jsonDocumentOptions : JsonDocumentOptions =
    let mutable options = new JsonDocumentOptions()
    options.CommentHandling <- JsonCommentHandling.Skip
    options.MaxDepth <- System.Int32.MaxValue // infinite
    options

  let parseJson (s : string) : JsonDocument =
    JsonDocument.Parse(s, jsonDocumentOptions)

  // When receiving unknown json from the user, or via a HTTP API, attempt to
  // convert everything into reasonable types, in the absense of a schema.
  let parse str : Result<Dval, string> =
    let rec convert json =
      match json with
      | JInteger i -> DInt i
      | JFloat f -> DFloat f
      | JBoolean b -> DBool b
      | JNull -> DNull
      | JString s -> DStr s
      | JList l -> l |> List.map convert |> Dval.list
      | JObject fields ->
        fields |> List.map (fun (k, v) -> k, (convert v)) |> Map |> DObj
      | JUndefined
      | _ -> Exception.raiseInternal "Invalid type in json" [ "json", json ]

    try
      use document = str |> parseJson
      document.RootElement |> convert |> Ok
    with
    | :? JsonException as e ->
      let msg =
        if str = "" then
          "JSON string was empty"
        else
          let msg = e.Message
          // The full message has .NET specific advice, so just stick to the good stuff
          let trailingCommaMsg =
            "The JSON array contains a trailing comma at the end"
          if msg.Contains trailingCommaMsg then
            $"{trailingCommaMsg}, at on line {e.LineNumber}, position {e.BytePositionInLine}"
          else
            msg
      Error msg
    | e -> Error e.Message


let varErr = TVariable "err"
let varA = TVariable "a"

let fns : List<BuiltInFn> =
  [ { name = fn "JSON" "read" 0
      parameters = [ Param.make "json" TStr "" ]
      returnType = varA
      description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
      fn =
        (function
        | _, [ DStr json ] ->
          uply {
            try
              return DvalReprExternal.unsafeOfUnknownJsonV0 json
            with
            | _ -> return DNull
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "JSON" "read" 1) }


    { name = fn "JSON" "parse" 0
      parameters = [ Param.make "json" TStr "" ]
      returnType = varA
      description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
      fn =
        (function
        | _, [ DStr json ] ->
          match OfUnknownJsonV1.parse json with
          | Ok dv -> Ply dv
          | Error msg -> Ply(DError(SourceNone, msg))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "JSON" "parse" 1) }


    { name = fn "JSON" "parse" 1
      parameters = [ Param.make "json" TStr "" ]
      returnType = TResult(varA, varErr)
      description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
      fn =
        (function
        | _, [ DStr json ] ->
          json |> OfUnknownJsonV1.parse |> Result.mapError DStr |> DResult |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
