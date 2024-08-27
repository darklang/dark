module BuiltinExecution.Libs.AltJson

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs



module Json =
  type Json =
    | Null
    | Bool of bool
    | Number of double
    | String of string
    | Array of List<Json>
    | Object of List<string * Json>

  let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.AltJson.json
  let typeRef = TCustomType(Ok typeName, [])
  let knownType = KTCustomType(typeName, [])

  let rec fromDT (dv : Dval) : Json =
    match dv with
    | DEnum(_, _, [], "Null", []) -> Null
    | DEnum(_, _, [], "Bool", [ DBool b ]) -> Bool b
    | DEnum(_, _, [], "Number", [ DFloat n ]) -> Number n
    | DEnum(_, _, [], "String", [ DString str ]) -> String str

    | DEnum(_, _, [], "Array", [ DList(_, items) ]) ->
      items |> List.map fromDT |> Array

    | DEnum(_, _, [], "Object", [ DList(_, entries) ]) ->
      entries
      |> List.map (fun pair ->
        match pair with
        | DTuple(DString k, v, _) -> (k, fromDT v)
        | _ -> Exception.raiseInternal "Invalid Json" [])
      |> Object

    | _ -> Exception.raiseInternal "Invalid Json" []


  let rec toDT (token : Json) : Dval =
    let (caseName, fields) =
      match token with
      | Null -> "Null", []
      | Bool b -> "Bool", [ DBool b ]
      | Number n -> "Number", [ DFloat n ]
      | String str -> "String", [ DString str ]

      | Array items ->
        "Array", [ items |> List.map toDT |> Dval.list (KTCustomType(typeName, [])) ]

      | Object entries ->
        let entries =
          entries
          |> List.map (fun (k, v) -> DTuple(DString k, toDT v, []))
          |> Dval.list (KTTuple(VT.string, VT.customType typeName [], []))
        "Object", [ entries ]

    DEnum(typeName, typeName, [], caseName, fields)


module ParseError =
  let typeName = FQTypeName.fqPackage PackageIDs.Type.Stdlib.AltJson.parseError
  let typeRef = TCustomType(Ok typeName, [])
  let knownType = KTCustomType(typeName, [])

  type ParseError = | NotJson

  exception JsonException of ParseError

  let toDT (e : ParseError) : Dval =
    let (caseName, fields) =
      match e with
      | NotJson -> "NotJson", []

    DEnum(typeName, typeName, [], caseName, fields)



module Parsing =
  let dotnetParsingOptions =
    new JsonDocumentOptions(
      CommentHandling = JsonCommentHandling.Skip,
      MaxDepth = System.Int32.MaxValue
    )

  let parse (str : string) : Result<Json.Json, ParseError.ParseError> =
    let rec convert (j : JsonElement) : Json.Json =
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

      | _ -> raise (ParseError.JsonException ParseError.NotJson)

    // .NET does the hard work of actually parsing the JSON
    let parsedByDotNet =
      try
        Ok(JsonDocument.Parse(str, dotnetParsingOptions).RootElement)
      with _ex ->
        Error ParseError.NotJson

    match parsedByDotNet with
    | Error err -> Error err
    | Ok parsed ->
      try
        Ok(convert parsed)
      with ParseError.JsonException ex ->
        Error ex


module Serialize =
  let jsonWriterOptions =
    new JsonWriterOptions(
      // TODO: `true` here makes it hard to write tests,
      // because the output is spread across multiple lines
      Indented = false,
      SkipValidation = true,
      Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

  let writeJson (f : Utf8JsonWriter -> unit) : string =
    let stream = new System.IO.MemoryStream()
    let w = new Utf8JsonWriter(stream, jsonWriterOptions)
    f w
    w.Flush()

    UTF8.ofBytesUnsafe (stream.ToArray())


  let rec writeToken (w : Utf8JsonWriter) (jsonToken : Json.Json) : unit =
    let r = writeToken w
    match jsonToken with
    | Json.Null -> w.WriteNullValue()
    | Json.Bool b -> w.WriteBooleanValue b
    | Json.Number n -> w.WriteNumberValue n
    | Json.String s -> w.WriteStringValue s

    | Json.Array l ->
      w.WriteStartArray()
      List.iter r l
      w.WriteEndArray()

    | Json.Object entries ->
      w.WriteStartObject() // {

      entries
      |> List.iter (fun (k, v) ->
        // write `key :`
        w.WritePropertyName k

        // write value on RHS of the already-written `:`
        r v)

      w.WriteEndObject() // }


let fns : List<BuiltInFn> =
  [ { name = fn "altJsonFormat" 0
      typeParams = []
      parameters = [ Param.make "json" Json.typeRef "" ]
      returnType = TString
      description = "Formats a JSON value as a JSON string."
      fn =
        (function
        | _, _, [], [ jtDval ] ->
          let jt = Json.fromDT jtDval
          let jsonString = Serialize.writeJson (fun w -> Serialize.writeToken w jt)
          Ply(DString jsonString)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "altJsonParse" 0
      typeParams = []
      parameters = [ Param.make "jsonString" TString "" ]
      returnType = TypeReference.result Json.typeRef ParseError.typeRef
      description = "Tries to parse a string <param jsonString> as Json"
      fn =
        let result = Dval.result Json.knownType ParseError.knownType

        (function
        | _, _, [], [ DString jsonString ] ->
          match Parsing.parse jsonString with
          | Ok jt -> jt |> Json.toDT |> Ok |> result |> Ply
          | Error e -> e |> ParseError.toDT |> Error |> result |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
