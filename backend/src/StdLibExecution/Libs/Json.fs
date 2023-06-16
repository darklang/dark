module StdLibExecution.Libs.Json

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts



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
  (types : Types)
  (w : Utf8JsonWriter)
  (typ : TypeReference)
  (dv : Dval)
  : unit =
  let r = serialize types w

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
  | TString, DString s -> w.WriteStringValue s

  | TDateTime, DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

  | TUuid, DUuid uuid -> w.WriteStringValue(string uuid)

  | TBytes, DBytes bytes ->
    bytes |> Base64.defaultEncodeToString |> w.WriteStringValue

  | TPassword, DPassword(Password hashed) ->
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

  | TTuple(t1, t2, trest), DTuple(d1, d2, rest) ->
    let ts = t1 :: t2 :: trest
    let ds = d1 :: d2 :: rest

    if List.length ts <> List.length ds then
      Exception.raiseInternal
        $"Not sure what to do with mismatched tuple ({ds} doesn't match {ts})"
        []

    let zipped = List.zip (t1 :: t2 :: trest) (d1 :: d2 :: rest)
    w.writeArray (fun () -> List.iter (fun (t, d) -> r t d) zipped)

  | TOption _, DOption None -> w.writeObject (fun () -> w.WriteNull "Nothing")
  | TOption oType, DOption(Some dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Just"
      r oType dv)
  | TResult(okType, _), DResult(Ok dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Ok"
      r okType dv)
  | TResult(_, errType), DResult(Error dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Error"
      r errType dv)

  | TCustomType(typeName, _typeArgs), dval ->
    // TODO: try find exactly one matching type

    match Types.find typeName types with
    | Some(CustomType.Alias typ) -> r typ dv

    | Some(CustomType.Enum(firstCase, additionalCases)) ->
      // TODO: ensure that the type names are the same
      // TODO: _something_ with the type args
      //   (or maybe we just need to revisit once TypeDefinition is present)

      match dval with
      | DEnum(dTypeName, caseName, fields) ->
        let matchingCase =
          (firstCase :: additionalCases) |> List.filter (fun c -> c.name = caseName)

        match matchingCase with
        | [] ->
          Exception.raiseInternal $"Couldn't find matching case for {caseName}" []
        | [ matchingCase ] ->
          let fieldDefs = matchingCase.fields |> List.map (fun def -> def.typ)

          if List.length fieldDefs <> List.length fields then
            Exception.raiseInternal
              $"Couldn't serialize Enum as incorrect # of fields provided"
              [ "defs", fieldDefs
                "fields", fields
                "typeName", typeName
                "caseName", caseName ]

          w.writeObject (fun () ->
            w.WritePropertyName caseName
            w.writeArray (fun () ->
              List.zip fieldDefs fields
              |> List.iter (fun (fieldDef, fieldVal) -> r fieldDef fieldVal)))
        | _ -> Exception.raiseInternal "Too many matching cases" []


      | _ -> Exception.raiseInternal "Expected a DEnum but got something else" []

    | Some(CustomType.Record(firstField, additionalFields)) ->
      match dval with
      | DRecord(actualTypeName, dvalMap) when actualTypeName = typeName ->
        let fieldDefs = firstField :: additionalFields

        w.writeObject (fun () ->
          dvalMap
          |> Map.toList
          |> List.iter (fun (fieldName, dval) ->
            w.WritePropertyName fieldName

            let matchingFieldDef =
              fieldDefs |> List.filter (fun def -> def.name = fieldName)

            match matchingFieldDef with
            | [] -> Exception.raiseInternal "Couldn't find matching field" []
            | [ matchingFieldDef ] -> r matchingFieldDef.typ dval
            | _ -> Exception.raiseInternal "Too many matching fields" []))

      | DRecord(_, _) -> Exception.raiseInternal "Incorrect record type" []
      | _ -> Exception.raiseInternal "Expected a DRecord but got something else" []

    | None -> Exception.raiseInternal "Couldn't find type" [ "typeName", typeName ]

  // Not supported
  | TVariable _, _ ->
    Exception.raiseInternal
      "Variable types (i.e. 'a in List<'a>) cannot not be used as arguments"
      []

  | TFn _, DFnVal _ -> Exception.raiseInternal "Cannot serialize functions" []

  | TDB _, DDB _ -> Exception.raiseInternal "Cannot serialize DB references" []


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
  | TVariable _, _
  | TCustomType _, _
  | TOption _, _
  | TResult _, _
  | TDict _, _ ->
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
    | TypeUnsupported of TypeReference
    | TypeNotYetSupported of TypeReference
    | CantMatchWithType of
      typ : TypeReference *
      json : string *
      errorPath : List<ErrorPath>
    | Uncaught of System.Exception

  exception JsonParseException of T

  let toString (err : T) : string =
    match err with
    | Uncaught ex -> "Uncaught error: " + ex.Message
    | NotJson -> "Not JSON"
    | TypeUnsupported typ -> $"Type not supported (intentionally): {typ}"
    | TypeNotYetSupported typ -> $"Type not yet supported: {typ}"
    | CantMatchWithType(typ, json, errorPath) ->
      let errorPath =
        errorPath
        |> List.map (function
          | Root -> "root"
          | Index i -> $"[{i}]"
          | Field f -> $".{f}")
        |> String.concat ""

      $"Can't match JSON with type {typ} at {errorPath}: {json}"

let parse
  (types : Types)
  (typ : TypeReference)
  (str : string)
  : Result<Dval, string> =
  let rec convert (typ : TypeReference) (j : JsonElement) : Dval =
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
    | TString, JsonValueKind.String -> DString(j.GetString())

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

    | TTuple(t1, t2, rest), JsonValueKind.Array ->
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

    | TOption _, JsonValueKind.Null -> DOption None
    | TOption oType, JsonValueKind.Object ->
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map

      match Map.tryFind "Just" objFields with
      | Some v -> DOption(Some(convert oType v))
      | None -> DOption None
    | TOption oType, v -> convert oType j |> Some |> DOption

    | TResult(okType, errType), JsonValueKind.Object ->
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

    | TCustomType(typeName, typeArgs), jsonValueKind ->
      // TODO: something with typeArgs

      // TODO: handle type missing
      match Types.find typeName types, jsonValueKind with
      | Some(CustomType.Alias alias), _ -> convert alias j

      | Some(CustomType.Enum(firstCase, additionalCases)), JsonValueKind.Object ->

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

          if List.length matchingCase.fields <> List.length j then
            Exception.raiseInternal
              $"Couldn't parse Enum as incorrect # of fields provided"
              []

          let mapped =
            List.zip matchingCase.fields j
            |> List.map (fun (typ, j) -> convert typ.typ j)

          DEnum(typeName, caseName, mapped)

        | _ -> Exception.raiseInternal "TODO" []

      | Some(CustomType.Record(firstField, additionalFields)), JsonValueKind.Object ->
        let fieldDefs = firstField :: additionalFields
        let enumerated = j.EnumerateObject() |> Seq.toList

        let dvalMap =
          fieldDefs
          |> List.map (fun def ->
            let correspondingValue =
              let matchingFieldDef =
                enumerated
                // TODO: handle case where value isn't found for
                // and maybe, if it's an Option<>al thing, don't complain
                |> List.filter (fun v -> v.Name = def.name)

              match matchingFieldDef with
              | [] -> Exception.raiseInternal "Couldn't find matching field" []
              | [ matchingFieldDef ] -> matchingFieldDef.Value
              | _ -> Exception.raiseInternal "Too many matching fields" []

            let converted = convert def.typ correspondingValue
            (def.name, converted))
          |> Map.ofSeq

        DRecord(typeName, dvalMap)
      | None, _ -> Exception.raiseInternal "TODO - type not found" []
      | _ ->
        Exception.raiseInternal
          "Can't currently parse this custom type"
          [ "type", typeName ]


    // Explicitly not supported
    | TVariable _, _ ->
      // this should disappear when we use a TypeReference here rather than a TypeReference
      // TYPESCLEANUP-I don't think it does, bunecessarily, but it should be knowable here
      JsonParseError.TypeUnsupported typ
      |> JsonParseError.JsonParseException
      |> raise

    | TFn _, _ -> Exception.raiseInternal "Cannot parse functions" []

    | TDB _, _ -> Exception.raiseInternal "Cannot serialize DB references" []

    // exhaust TypeReferences
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
    | TVariable _, _
    | TCustomType _, _
    | TOption _, _
    | TResult _, _
    | TDict _, _ ->
      Exception.raiseInternal
        "Can't currently parse this type/value combination"
        [ "type", typ; "value", j ]

  let parsed =
    try
      Ok(parseJson str)
    with _ex ->
      Error "not JSON"

  match parsed with
  | Error err -> Error err
  | Ok parsed ->
    try
      parsed |> convert typ |> Ok
    with
    | JsonParseError.JsonParseException ex -> JsonParseError.toString ex |> Error
    | ex -> Error ex.Message


let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "Json" "serialize" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "arg" (TVariable "a") "" ]
      returnType = TResult(TString, TString)
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | state, [ typeArg ], [ arg ] ->
          // TODO: somehow collect list of TVariable -> TypeReference
          // "'b = Int",
          // so we can Json.serialize<'b>, if 'b is in the surrounding context

          try
            let types = ExecutionState.availableTypes state
            let response = writeJson (fun w -> serialize types w typeArg arg)
            Ply(DResult(Ok(DString response)))
          with ex ->
            Ply(DResult(Error(DString ex.Message)))
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
        | state, [ typeArg ], [ DString arg ] ->
          let types = ExecutionState.availableTypes state
          match parse types typeArg arg with
          | Ok v -> Ply(DResult(Ok v))
          | Error e -> Ply(DResult(Error(DString e)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
