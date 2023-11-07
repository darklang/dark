module BuiltinExecution.Libs.AltJson

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval


let typ
  (addlModules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  TypeName.fqPackage "Darklang" ([ "Stdlib"; "AltJson" ] @ addlModules) name version


module JsonToken =
  type JsonToken =
    | Null
    | Bool of bool
    | Number of double
    | String of string
    | Array of List<JsonToken>
    | Object of List<string * JsonToken>

  let typeName = typ [] "JsonToken" 0
  let typeRef = TCustomType(Ok typeName, [])
  let knownType = KTCustomType(typeName, [])

  let rec fromDT (dv : Dval) : JsonToken =
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
        | _ -> Exception.raiseInternal "Invalid JsonToken" [])
      |> Object

    | _ -> Exception.raiseInternal "Invalid JsonToken" []


  let rec toDT (token : JsonToken) : Dval =
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


module TokenParseError =
  let typeName = typ [ "TokenParseError" ] "TokenParseError" 0
  let typeRef = TCustomType(Ok typeName, [])
  let knownType = KTCustomType(typeName, [])

  type TokenParseError = | NotJson

  exception JsonException of TokenParseError

  let toDT (e : TokenParseError) : Dval =
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

  let parse
    (str : string)
    : Result<JsonToken.JsonToken, TokenParseError.TokenParseError> =
    let rec convert (j : JsonElement) : JsonToken.JsonToken =
      match j.ValueKind with
      | JsonValueKind.Null -> JsonToken.Null

      | JsonValueKind.True -> JsonToken.Bool true
      | JsonValueKind.False -> JsonToken.Bool false

      | JsonValueKind.Number -> j.GetDouble() |> JsonToken.Number

      | JsonValueKind.String -> j.GetString() |> JsonToken.String

      | JsonValueKind.Array ->
        j.EnumerateArray() |> Seq.map convert |> Seq.toList |> JsonToken.Array

      | JsonValueKind.Object ->
        j.EnumerateObject()
        |> Seq.map (fun jp -> (jp.Name, convert jp.Value))
        |> Seq.toList
        |> JsonToken.Object

      | _ -> raise (TokenParseError.JsonException TokenParseError.NotJson)

    // .net does the hard work of actually parsing the JSON
    let parsedByDotNet =
      try
        Ok(JsonDocument.Parse(str, dotnetParsingOptions).RootElement)
      with _ex ->
        Error TokenParseError.NotJson

    match parsedByDotNet with
    | Error err -> Error err
    | Ok parsed ->
      try
        Ok(convert parsed)
      with TokenParseError.JsonException ex ->
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


  let rec writeToken (w : Utf8JsonWriter) (jsonToken : JsonToken.JsonToken) : unit =
    let r = writeToken w
    match jsonToken with
    | JsonToken.Null -> w.WriteNullValue()
    | JsonToken.Bool b -> w.WriteBooleanValue b
    | JsonToken.Number n -> w.WriteNumberValue n
    | JsonToken.String s -> w.WriteStringValue s

    | JsonToken.Array l ->
      w.WriteStartArray()
      List.iter r l
      w.WriteEndArray()

    | JsonToken.Object entries ->
      w.WriteStartObject() // {

      entries
      |> List.iter (fun (k, v) ->
        // write `key :`
        w.WritePropertyName k

        // write value on RHS of the already-written `:`
        r v)

      w.WriteEndObject() // }



let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "AltJson" ]

let fns : List<BuiltInFn> =
  [ { name = fn "serializeToken" 0
      typeParams = []
      parameters = [ Param.make "token" JsonToken.typeRef "" ]
      returnType = TString
      description = "Serializes a JsonToken to a JSON string."
      fn =
        (function
        | _, [], [ jtDval ] ->
          let jt = JsonToken.fromDT jtDval
          let jsonString = Serialize.writeJson (fun w -> Serialize.writeToken w jt)
          Ply(DString jsonString)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parseToken" 0
      typeParams = []
      parameters = [ Param.make "json" TString "" ]
      returnType = TypeReference.result JsonToken.typeRef TokenParseError.typeRef
      description = "Parses a JSON string <param json> as a JsonToken"
      fn =
        let result = Dval.result JsonToken.knownType TokenParseError.knownType

        (function
        | _, [], [ DString jsonString ] ->
          match Parsing.parse jsonString with
          | Ok jt -> jt |> JsonToken.toDT |> Ok |> result |> Ply
          | Error e -> e |> TokenParseError.toDT |> Error |> result |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let contents = (fns, types, constants)
