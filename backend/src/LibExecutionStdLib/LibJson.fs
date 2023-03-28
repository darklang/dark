module LibExecutionStdLib.LibJson

open LibExecution.RuntimeTypes
open Prelude

module PT = LibExecution.ProgramTypes


module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName
let tp = PT.FQTypeName.stdlibTypeName

let incorrectArgs = Errors.incorrectArgs


// let's imagine we have a type like this:
// type User = { name: string; age: int }
// type Data = { users: User[] }
// and we have a json string like this, with 2 users, one of which doesn't fully match the user type:
// { "users": [ { "name": "Alice", "age": 20 }, { "name": "Bob" } ] }
// then we want to parse it into a Data value, and we want to know that the second user didn't match the type

// so given `let parse<'a>: Result<'a, JsonParseError> = ...`
// we should expect `parse<Data> json` to return a Result with an error like this:
// Error <| CantMatchWithType { typ = "User"; json = "{ \"name\": \"Bob\" }"; location = "users[1]" }

// also, if we try to call `parse<bool> "1"` we should get a Result with an error like this:
// Error <| CantMatchWithType { typ = "bool"; json = "1"; location = "Root" }

// TODO: eventually, expose this
module JsonParseError =
  type ErrorPath =
    | Root
    | Index of int
    | Field of string

  type T =
    | NotJson
    | TypeUnsupported of DType
    | TypeNotYetSupported of DType
    | CantMatchWithType of typ : DType * json : string * errorPath : List<ErrorPath>

  exception JsonParseException of T

  let toString (err : T) : string =
    match err with
    | NotJson -> "Not JSON"
    | TypeUnsupported typ -> $"Type not supported (intentionally): {typ}"
    | TypeNotYetSupported typ -> $"Type not yet supported: {typ}"
    | CantMatchWithType (typ, json, errorPath) ->
      let errorPath =
        errorPath
        |> List.map (function
          | Root -> "root"
          | Index i -> $"[{i}]"
          | Field f -> $".{f}")
        |> String.concat ""

      $"Can't match JSON with type {typ} at {errorPath}: {json}"


open System.Text.Json

// parsing
let parseJson (s : string) : JsonElement =
  let mutable options = new JsonDocumentOptions()
  options.CommentHandling <- JsonCommentHandling.Skip
  options.MaxDepth <- System.Int32.MaxValue // infinite

  JsonDocument.Parse(s, options).RootElement


// serialization
let writeJson (f : Utf8JsonWriter -> unit) : string =
  let mutable options = new JsonWriterOptions()
  options.Indented <-
    // TODO: `true` here makes it hard to write tests, because the output is
    // spread across multiple lines.
    //true
    false
  options.SkipValidation <- true
  let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
  options.Encoder <- encoder

  let stream = new System.IO.MemoryStream()
  let w = new Utf8JsonWriter(stream, options)
  f w
  w.Flush()
  UTF8.ofBytesUnsafe (stream.ToArray())

type Utf8JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEndObject()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEndArray()


let rec serialize (w : Utf8JsonWriter) (typ : DType) (dv : Dval) : unit =
  let r = serialize w

  match typ, dv with
  // basic types
  | TUnit, DUnit -> w.WriteNullValue()

  | TBool, DBool b -> w.WriteBooleanValue b

  | TInt, DInt i ->
    // CLEANUP if the number is outside the range, store as a string?
    // TODO for now, maybe just throw an error?
    w.WriteNumberValue i

  | TFloat, DFloat f ->
    if System.Double.IsNaN f then
      w.WriteStringValue "NaN"
    else if System.Double.IsNegativeInfinity f then
      w.WriteStringValue "-Infinity"
    else if System.Double.IsPositiveInfinity f then
      w.WriteStringValue "Infinity"
    else
      let result = sprintf "%.12g" f
      let result = if result.Contains "." then result else $"{result}.0"
      w.WriteRawValue result

  | TChar, DChar c -> w.WriteStringValue c
  | TStr, DStr s -> w.WriteStringValue s

  | TDateTime, DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

  | TUuid, DUuid uuid -> w.WriteStringValue(string uuid)

  | TBytes, DBytes bytes ->
    bytes |> Base64.defaultEncodeToString |> w.WriteStringValue

  | TPassword, DPassword (Password hashed) ->
    hashed |> Base64.defaultEncodeToString |> w.WriteStringValue


  // Nested types
  | TList ltype, DList l -> w.writeArray (fun () -> List.iter (r ltype) l)

  | TDict objType, DObj o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          r objType v)
        o)
  | TRecord fields, DObj dvalMap ->
    let schema = Map.ofList fields
    w.writeObject (fun () ->
      dvalMap
      |> Map.toList
      |> List.iter (fun (k, dval) ->
        w.WritePropertyName k
        r (Map.find k schema) dval))

  | TTuple (t1, t2, trest), DTuple (d1, d2, rest) ->
    let zipped = List.zip (t1 :: t2 :: trest) (d1 :: d2 :: rest)
    w.writeArray (fun () -> List.iter (fun (t, d) -> r t d) zipped)

  | TOption _, DOption None -> w.writeObject (fun () -> w.WriteNull "Nothing")
  | TOption oType, DOption (Some dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Just"
      r oType dv)
  | TResult (okType, _), DResult (Ok dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Ok"
      r okType dv)
  | TResult (_, errType), DResult (Error dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Error"
      r errType dv)

  | TCustomType _, _ ->
    Exception.raiseInternal
      "TODO: pass in availableTypes. then, handle both Records and Enums"
      []


  // Not supported
  | TVariable _, _ ->
    Exception.raiseInternal
      "Variable types (i.e. 'a in List<'a>) cannot not be used as arguments"
      []

  | TFn _, DFnVal _ -> Exception.raiseInternal "Cannot serialize functions" []

  | TDB _, DDB _ -> Exception.raiseInternal "Cannot serialize DB references" []

  | TError _, DError _
  | TIncomplete, DIncomplete _ ->
    Exception.raiseInternal "Fake types/values are not supported" []

  | THttpResponse _, _ ->
    Exception.raiseInternal "Not worth supporting - about to be deleted" []


  // Exhaust the types
  | TUnit, _
  | TBool, _
  | TInt, _
  | TFloat, _
  | TChar, _
  | TStr, _
  | TUuid, _
  | TBytes, _
  | TDateTime, _
  | TPassword, _
  | TList _, _
  | TTuple _, _
  | TFn _, _
  | TDB _, _
  | TIncomplete, _
  | TError, _
  | TVariable _, _
  | TCustomType _, _
  | TOption _, _
  | TResult _, _
  | TDict _, _
  | TRecord _, _ ->
    Exception.raiseInternal
      "Can't currently serialize this type/value combination"
      [ "value", dv; "type", typ ]


let parse (typ : DType) (str : string) : Result<Dval, string> =
  let rec convert (typ : DType) (j : JsonElement) : Dval =
    match typ, j.ValueKind with
    // basic types
    | TUnit, JsonValueKind.Null -> DUnit

    | TBool, JsonValueKind.True -> DBool true
    | TBool, JsonValueKind.False -> DBool false

    | TInt, JsonValueKind.Number -> j.GetInt64() |> DInt

    | TFloat, JsonValueKind.Number -> j.GetDouble() |> DFloat
    | TFloat, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat System.Double.NaN
      | "Infinity" -> DFloat System.Double.PositiveInfinity
      | "-Infinity" -> DFloat System.Double.NegativeInfinity
      | v -> Exception.raiseInternal "Invalid float" [ "value", v ]

    | TChar, JsonValueKind.String -> DChar(j.GetString())
    | TStr, JsonValueKind.String -> DStr(j.GetString())

    | TBytes, JsonValueKind.String ->
      j.GetString() |> Base64.decodeFromString |> DBytes

    | TUuid, JsonValueKind.String -> DUuid(System.Guid(j.GetString()))

    | TDateTime, JsonValueKind.String ->
      j.GetString()
      |> NodaTime.Instant.ofIsoString
      |> DarkDateTime.fromInstant
      |> DDateTime

    | TPassword, JsonValueKind.String ->
      j.GetString() |> Base64.decodeFromString |> Password |> DPassword


    // Nested types

    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray() |> Seq.map (convert nested) |> Seq.toList |> DList

    | TTuple (t1, t2, rest), JsonValueKind.Array ->
      let mapped =
        j.EnumerateArray()
        |> Seq.toList
        |> List.zip (t1 :: t2 :: rest)
        |> List.map (fun (t, v) -> convert t v)

      match mapped with
      | (d1 :: d2 :: rest) -> DTuple(d1, d2, rest)
      | _ ->
        Exception.raiseInternal
          "Invalid tuple - really shouldn't be possible to hit this"
          []

    | TRecord typFields, JsonValueKind.Object ->
      // Use maps to cooalesce duplicate keys and ensure the obj matches the type
      let typFields = Map typFields
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map

      if Map.count objFields = Map.count typFields then
        objFields
        |> Map.map (fun k v ->
          match Map.tryFind k typFields with
          | Some t -> convert t v
          | None -> Exception.raiseInternal "Missing field" [ "field", k ])
        |> DObj
      else
        Exception.raiseInternal "Invalid fields" []

    | TOption _, JsonValueKind.Null -> DOption None
    | TOption oType, JsonValueKind.Object ->
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map

      match Map.tryFind "Just" objFields with
      | Some v -> DOption(Some(convert oType v))
      | None -> DOption None

    | TResult (okType, errType), JsonValueKind.Object ->
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map

      match (Map.tryFind "Ok" objFields, Map.tryFind "Error" objFields) with
      | (Some vOk, None) -> DResult(Ok(convert okType vOk))
      | (None, Some vError) -> DResult(Error(convert errType vError))
      | _ -> Exception.raiseInternal "Invalid result object" []

    | TDict tDict, JsonValueKind.Object ->
      j.EnumerateObject()
      |> Seq.map (fun jp -> (jp.Name, convert tDict jp.Value))
      |> Map.ofSeq
      |> DObj

    | TCustomType _, _ ->
      // TODO: need to pass in availableTypes, then handle both Enums and Records
      JsonParseError.TypeNotYetSupported typ
      |> JsonParseError.JsonParseException
      |> raise

    | TVariable _, _ ->
      JsonParseError.TypeUnsupported typ
      |> JsonParseError.JsonParseException
      |> raise



    // Explicitly not supported
    | TFn _, _ -> Exception.raiseInternal "Cannot parse functions" []

    | TDB _, _ -> Exception.raiseInternal "Cannot serialize DB references" []

    | TIncomplete, _
    | TError, _ -> Exception.raiseInternal "Fake types/values are not supported" []

    | THttpResponse _, _ ->
      Exception.raiseInternal
        "Can't currently parse this type/value combination"
        [ "type", typ; "value", j ]


    // exhaust DTypes
    | TUnit, _
    | TBool, _
    | TInt, _
    | TFloat, _
    | TChar, _
    | TStr, _
    | TUuid, _
    | TBytes, _
    | TDateTime, _
    | TPassword, _
    | TList _, _
    | TTuple _, _
    | TFn _, _
    | TDB _, _
    | TIncomplete, _
    | TError, _
    | TVariable _, _
    | TCustomType _, _
    | TOption _, _
    | TResult _, _
    | TDict _, _
    | TRecord _, _
    | THttpResponse _, _ ->
      Exception.raiseInternal
        "Can't currently parse this type/value combination"
        [ "type", typ; "value", j ]

  let parsed =
    try
      Ok(parseJson str)
    with
    | _ex -> Error "not JSON"

  match parsed with
  | Error err -> Error err
  | Ok parsed ->
    try
      parsed |> convert typ |> Ok
    with
    | JsonParseError.JsonParseException ex -> JsonParseError.toString ex |> Error
    | ex -> Error ex.Message







let fns : List<BuiltInFn> =
  [ { name = fn "Json" "serialize" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "arg" (TVariable "a") "" ]
      returnType = TResult(TStr, TStr)
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | _, [ typeArg ], [ arg ] ->
          try
            let response = writeJson (fun w -> serialize w typeArg arg)
            Ply(DResult(Ok(DStr response)))
          with
          | ex -> Ply(DResult(Error(DStr ex.Message)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Json" "parse" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "json" TStr "" ]
      returnType = TResult(TVariable "a", TStr)
      description =
        "Parses a JSON string <param json> as a Dark value, matching the type <typeParam a>"
      fn =
        (function
        | _, [ typeArg ], [ DStr arg ] ->
          match parse typeArg arg with
          | Ok v -> Ply(DResult(Ok v))
          | Error e -> Ply(DResult(Error(DStr e)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]
