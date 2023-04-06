module LibExecutionStdLib.LibJson

open System.Text.Json

open LibExecution.RuntimeTypes
open Prelude

module PT = LibExecution.ProgramTypes
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName
let tp = PT.FQTypeName.stdlibTypeName

let incorrectArgs = Errors.incorrectArgs


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


let rec serialize
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (w : Utf8JsonWriter)
  (typ : DType)
  (dv : Dval)
  : unit =
  let r = serialize availableTypes w

  match typ, dv with
  // basic types
  | TUnit, DUnit -> w.WriteNullValue()

  | TBool, DBool b -> w.WriteBooleanValue b

  | TInt, DInt i ->
    // CLEANUP if the number is outside the range, store as a string?
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
  | TString, DStr s -> w.WriteStringValue s

  | TDateTime, DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

  | TUuid, DUuid uuid -> w.WriteStringValue(string uuid)

  | TBytes, DBytes bytes ->
    bytes |> Base64.defaultEncodeToString |> w.WriteStringValue

  | TPassword, DPassword (Password hashed) ->
    hashed |> Base64.defaultEncodeToString |> w.WriteStringValue


  // Nested types
  | TList ltype, DList l -> w.writeArray (fun () -> List.iter (r ltype) l)

  | TDict objType, DDict o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          r objType v)
        o)
  | TRecord fields, DDict dvalMap ->
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

  | TCustomType (typeName, _typeArgs), DDict dvalMap ->
    // TODO: try find exactly one matching type
    let matchingType = availableTypes[typeName]

    match matchingType with
    | CustomType.Enum (firstCase, additionalCases) ->
      Exception.raiseInternal "TODO" []
    | CustomType.Record (firstField, additionalFields) ->
      let fieldDefs = firstField :: additionalFields
      w.writeObject (fun () ->
        dvalMap
        |> Map.toList
        |> List.iter (fun (fieldName, dval) ->
          w.WritePropertyName fieldName

          let matchingDType =
            fieldDefs
            // TODO: handle case where field isn't found
            |> List.find (fun def -> def.name = fieldName)
            |> fun def -> def.typ

          r matchingDType dval))

  | TCustomType (typeName, _typeArgs), DRecord dvalMap ->
    // TODO: try find exactly one matching type
    let matchingType = availableTypes[typeName]

    match matchingType with
    | CustomType.Enum (firstCase, additionalCases) ->
      Exception.raiseInternal "TODO" []
    | CustomType.Record (firstField, additionalFields) ->
      let fieldDefs = firstField :: additionalFields
      w.writeObject (fun () ->
        dvalMap
        |> Map.toList
        |> List.iter (fun (fieldName, dval) ->
          w.WritePropertyName fieldName

          let matchingDType =
            fieldDefs
            |> List.find (fun def -> def.name = fieldName)
            |> fun def -> def.typ

          r matchingDType dval))



  | TCustomType (tTypeName, _typeArgs), DConstructor (dTypeName, caseName, fields) ->
    // TODO: ensure that the type names are the same
    // TODO: _something_ with the type args
    //   (or maybe we just need to revisit once TypeDefinition is present)

    // TODO: try find exactly one matching type
    let matchingType = availableTypes[tTypeName]
    match matchingType with
    | CustomType.Enum (firstCase, additionalCases) ->
      let matchingCase =
        (firstCase :: additionalCases) |> List.find (fun c -> c.name = caseName)
      // TODO: handle 0 or 2+ cases

      let fieldDefs = matchingCase.fields |> List.map (fun def -> def.typ)
      // TODO: ensure that the field count is the same

      w.writeObject (fun () ->
        w.WritePropertyName caseName
        w.writeArray (fun () ->
          List.zip fieldDefs fields
          |> List.iter (fun (fieldDef, fieldVal) -> r fieldDef fieldVal)))

    | CustomType.Record _ -> Exception.raiseInternal "TODO" []


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
  | TString, _
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

/// Let's imagine we have these types:
///
///   ```fsharp
///   type User = { name: string; age: int }
///   type Data = { users: User[] }
///   ```
///
/// and we have a json string like this, with 2 users,
///   one of which doesn't fully match the user type:
///
///   ```fsharp
///   { "users": [ { "name": "Alice", "age": 20 }, { "name": "Bob" } ] }
///   ```
///
/// When we run `parse<Data> json`, we'd like a structured error,
///   indicating that the second user didn't match the type. something like:
///
/// ```fsharp
/// CantMatchWithType
///   { typ = "User"
///     json = "{ \"name\": \"Bob\" }"
///     location = [ Root; Field "users"; Index 1] }
/// |> Error
/// ```
///
/// simpler case: if we try to call `parse<bool> "1"` we should get:
/// ```fsharp
/// Error <| CantMatchWithType { typ = "bool"; json = "1"; location = [Root] }
/// ```
///
/// TODO: eventually, expose this in the JSON module?
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
    | Uncaught of System.Exception

  exception JsonParseException of T

  let toString (err : T) : string =
    match err with
    | Uncaught ex -> "Uncaught error: " + ex.Message
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

let parse
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (typ : DType)
  (str : string)
  : Result<Dval, string> =
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
    | TString, JsonValueKind.String -> DStr(j.GetString())

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
        |> DDict
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
      |> DDict

    | TCustomType (typeName, typeArgs), JsonValueKind.Object ->
      // TODO: something with typeArgs

      let matchingType = availableTypes[typeName]
      // TODO: handle type missing
      match matchingType with
      | CustomType.Enum (firstCase, additionalCases) ->
        let enumerated =
          j.EnumerateObject()
          |> Seq.map (fun jp -> (jp.Name, jp.Value))
          |> Seq.toList

        match enumerated with
        | [ (caseName, j) ] ->
          let matchingCase =
            (firstCase :: additionalCases) |> List.find (fun c -> c.name = caseName)
          // TODO: handle 0 or 2+ matching cases
          let j = j.EnumerateArray() |> Seq.toList
          // TODO: handle when lists aren't of same len
          let mapped =
            List.zip matchingCase.fields j
            |> List.map (fun (typ, j) -> convert typ.typ j)
          DConstructor(Some typeName, caseName, mapped)

        | _ -> Exception.raiseInternal "TODO" []

      | CustomType.Record (firstField, additionalFields) ->
        let fieldDefs = firstField :: additionalFields
        let dvalMap =
          j.EnumerateObject()
          |> Seq.map (fun jp ->
            let correspondingType =
              fieldDefs
              // TODO: handle case where field isn't found
              |> List.find (fun def -> def.name = jp.Name)
              |> fun def -> def.typ
            let converted = convert correspondingType jp.Value
            (jp.Name, converted))
          |> Map.ofSeq

        // TODO: this should really be a DRecord
        // but isn't currently, as our parser doesn't know
        // when to parse an object as a record or a dict
        DDict(dvalMap)


    // Explicitly not supported
    | TVariable _, _ ->
      // this should disappear when we use a TypeReference here rather than a DType
      JsonParseError.TypeUnsupported typ
      |> JsonParseError.JsonParseException
      |> raise

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
    | TString, _
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
      returnType = TResult(TString, TString)
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | state, [ typeArg ], [ arg ] ->
          let availableTypes = ExecutionState.availableTypes state

          try
            let response =
              writeJson (fun w -> serialize availableTypes w typeArg arg)
            Ply(DResult(Ok(DStr response)))
          with
          | ex -> Ply(DResult(Error(DStr ex.Message)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Json" "parse" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "json" TString "" ]
      returnType = TResult(TVariable "a", TString)
      description =
        "Parses a JSON string <param json> as a Dark value, matching the type <typeParam a>"
      fn =
        (function
        | state, [ typeArg ], [ DStr arg ] ->
          let availableTypes = ExecutionState.availableTypes state

          match parse availableTypes typeArg arg with
          | Ok v -> Ply(DResult(Ok v))
          | Error e -> Ply(DResult(Error(DStr e)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]
