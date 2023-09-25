module BuiltinExecution.Libs.Json

open System.Text.Json

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module DarkDateTime = LibExecution.DarkDateTime
module VT = ValueType
module Dval = LibExecution.Dval


// parsing
let parseJson (s : string) : JsonElement =
  let mutable options = new JsonDocumentOptions()
  options.CommentHandling <- JsonCommentHandling.Skip
  options.MaxDepth <- System.Int32.MaxValue // infinite

  JsonDocument.Parse(s, options).RootElement


// serialization
let writeJson (f : Utf8JsonWriter -> Ply<unit>) : Ply<string> =
  uply {
    let mutable options = new JsonWriterOptions()
    options.Indented <-
      // TODO: `true` here makes it hard to write tests, because the output is
      // spread across multiple lines.
      //true
      false
    options.SkipValidation <- true
    let encoder =
      System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    options.Encoder <- encoder

    let stream = new System.IO.MemoryStream()
    let w = new Utf8JsonWriter(stream, options)
    do! f w
    w.Flush()

    return UTF8.ofBytesUnsafe (stream.ToArray())
  }

type Utf8JsonWriter with

  member this.writeObject(f : unit -> Ply<unit>) =
    uply {
      this.WriteStartObject()
      do! f ()
      this.WriteEndObject()
    }

  member this.writeArray(f : unit -> Ply<unit>) =
    uply {
      this.WriteStartArray()
      do! f ()
      this.WriteEndArray()
    }


let rec serialize
  (types : Types)
  (w : Utf8JsonWriter)
  (typ : TypeReference)
  (dv : Dval)
  : Ply<unit> =
  let r = serialize types w
  uply {
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
        let result = sprintf "%.16g" f
        let result = if result.Contains "." then result else $"{result}.0"
        w.WriteRawValue result

    | TChar, DChar c -> w.WriteStringValue c
    | TString, DString s -> w.WriteStringValue s

    | TDateTime, DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

    | TUuid, DUuid uuid -> w.WriteStringValue(string uuid)

    | TBytes, DBytes bytes ->
      bytes |> Base64.defaultEncodeToString |> w.WriteStringValue


    // Nested types
    | TList ltype, DList(_, l) ->
      do! w.writeArray (fun () -> Ply.List.iterSequentially (r ltype) l)

    | TDict dictType, DDict(_vtTODO, fields) ->
      do!
        w.writeObject (fun () ->
          fields
          |> Map.toList
          |> Ply.List.iterSequentially (fun (k, v) ->
            uply {
              w.WritePropertyName k
              do! r dictType v
            }))

    | TTuple(t1, t2, trest), DTuple(d1, d2, rest) ->
      let zipped = List.zip (t1 :: t2 :: trest) (d1 :: d2 :: rest)
      do!
        w.writeArray (fun () ->
          Ply.List.iterSequentially (fun (t, d) -> r t d) zipped)

    | TCustomType(Ok typeName, typeArgs), dval ->

      match! Types.find typeName types with
      | None -> Exception.raiseInternal "Couldn't find type" [ "typeName", typeName ]
      | Some decl ->

        match decl.definition with
        | TypeDeclaration.Alias typ ->
          let typ = Types.substitute decl.typeParams typeArgs typ
          return! r typ dv

        | TypeDeclaration.Enum cases ->
          match dval with
          | DEnum(dTypeName, _, _typeArgsDEnumTODO, caseName, fields) ->
            let matchingCase =
              cases
              |> NEList.find (fun c -> c.name = caseName)
              |> Exception.unwrapOptionInternal
                "Couldn't find matching case"
                [ "typeName", dTypeName ]

            do!
              w.writeObject (fun () ->
                w.WritePropertyName caseName
                w.writeArray (fun () ->
                  List.zip matchingCase.fields fields
                  |> Ply.List.iterSequentially (fun (fieldType, fieldVal) ->
                    let typ = Types.substitute decl.typeParams typeArgs fieldType
                    r typ fieldVal)))

          | _ -> Exception.raiseInternal "Expected a DEnum but got something else" []

        | TypeDeclaration.Record fields ->
          match dval with
          | DRecord(_, _, _typeArgsTODO, dvalMap) ->
            do!
              w.writeObject (fun () ->
                dvalMap
                |> Map.toList
                |> Ply.List.iterSequentially (fun (fieldName, dval) ->
                  w.WritePropertyName fieldName

                  let matchingFieldDef =
                    fields
                    |> NEList.find (fun def -> def.name = fieldName)
                    |> Exception.unwrapOptionInternal
                      "Couldn't find matching field"
                      [ "fieldName", fieldName ]

                  let typ =
                    Types.substitute decl.typeParams typeArgs matchingFieldDef.typ
                  r typ dval))
          | _ ->
            Exception.raiseInternal
              "Expected a DRecord but got something else"
              [ "actualDval", dval
                "actualType", LibExecution.DvalReprDeveloper.toTypeName dval
                "expectedType", typeName
                "expectedFields", fields ]


    | TCustomType(Error err, _typeArgs), dval -> raiseUntargetedRTE err


    // Not supported
    // TODO these should be runtime errors
    | TVariable name, dv ->
      Exception.raiseInternal
        "Variable types (i.e. 'a in List<'a>) cannot not be used as arguments"
        [ "variable", name; "dval", dv ]

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
    | TList _, _
    | TTuple _, _
    | TFn _, _
    | TDB _, _
    | TVariable _, _
    | TCustomType _, _
    | TDict _, _ ->
      // Internal error as this shouldn't get past the typechecker
      Exception.raiseInternal
        "Can't serialize this type/value combination"
        [ "value", dv; "type", DString(LibExecution.DvalReprDeveloper.typeName typ) ]
  }


module JsonPath =
  type JsonPathPart =
    | Root
    | Index of int
    | Field of string

  type JsonPath = List<JsonPathPart>

  let toString (path : JsonPath) : string =
    path
    |> List.rev
    |> List.map (function
      | Root -> "root"
      | Index i -> $"[{i}]"
      | Field f -> $".{f}")
    |> String.concat ""

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
  type T =
    | NotJson
    | TypeUnsupported of TypeReference
    | TypeNotYetSupported of TypeReference
    | TypeNotFound of TypeReference
    | CantMatchWithType of
      typ : TypeReference *
      json : string *
      path : JsonPath.JsonPath
    | Uncaught of System.Exception

  exception JsonParseException of T

  let toString (err : T) : string =
    match err with
    | Uncaught ex -> "Uncaught error: " + ex.Message
    | NotJson -> "Not JSON"
    | TypeNotFound typ -> $"Type not found: {typ}"
    | TypeUnsupported typ -> $"Type not supported (intentionally): {typ}"
    | TypeNotYetSupported typ -> $"Type not yet supported: {typ}"
    | CantMatchWithType(typ, json, errorPath) ->
      $"Can't parse JSON `{json}` as type `{LibExecution.DvalReprDeveloper.typeName typ}` at path: `{JsonPath.toString errorPath}`"

let raiseCantMatchWithType
  (typ : TypeReference)
  (j : JsonElement)
  (pathSoFar : JsonPath.JsonPath)
  =
  JsonParseError.CantMatchWithType(typ, j.GetRawText(), pathSoFar)
  |> JsonParseError.JsonParseException
  |> raise


let parse
  (types : Types)
  (typ : TypeReference)
  (str : string)
  : Ply<Result<Dval, string>> =

  let rec convert
    (typ : TypeReference)
    (pathSoFar : JsonPath.JsonPath)
    (j : JsonElement)
    : Ply<Dval> =
    match typ, j.ValueKind with
    // basic types
    | TUnit, JsonValueKind.Null -> DUnit |> Ply

    | TBool, JsonValueKind.True -> DBool true |> Ply
    | TBool, JsonValueKind.False -> DBool false |> Ply

    | TInt, JsonValueKind.Number ->
      let mutable i64 = 0L
      let mutable ui64 = 0UL
      let mutable d = 0.0
      // dotnet will wrap 9223372036854775808 to be -9223372036854775808 instead, we
      // don't want that and will error instead
      if j.TryGetUInt64(&ui64) then
        if ui64 <= uint64 System.Int64.MaxValue then
          DInt(int64 ui64) |> Ply
        else
          raiseCantMatchWithType TInt j pathSoFar |> Ply
      else if j.TryGetInt64(&i64) then
        DInt i64 |> Ply
      // We allow the user to specify numbers in int or float format (e.g. 1 or 1.0
      // or even 1E+0) -- JSON uses floating point numbers, and the person/API
      // client/server that is creating a field we understand to be an int may choose
      // to print an int in a floating point format.
      else if
        j.TryGetDouble(&d)
        && d <= (float System.Int64.MaxValue)
        && d >= (float System.Int64.MinValue)
        && System.Double.IsInteger d
      then
        int64 d |> DInt |> Ply
      else
        raiseCantMatchWithType TInt j pathSoFar |> Ply

    | TFloat, JsonValueKind.Number -> j.GetDouble() |> DFloat |> Ply
    | TFloat, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat System.Double.NaN
      | "Infinity" -> DFloat System.Double.PositiveInfinity
      | "-Infinity" -> DFloat System.Double.NegativeInfinity
      | _ -> raiseCantMatchWithType TFloat j pathSoFar
      |> Ply

    | TChar, JsonValueKind.String ->
      match String.toEgcChar (j.GetString()) with
      | Some c -> Ply(DChar c)
      | None -> raiseCantMatchWithType TChar j pathSoFar


    | TString, JsonValueKind.String -> DString(j.GetString()) |> Ply

    | TBytes, JsonValueKind.String ->
      j.GetString() |> Base64.decodeFromString |> DBytes |> Ply

    | TUuid, JsonValueKind.String ->
      try
        DUuid(System.Guid(j.GetString())) |> Ply
      with _ ->
        raiseCantMatchWithType TUuid j pathSoFar

    | TDateTime, JsonValueKind.String ->
      try
        j.GetString()
        |> NodaTime.Instant.ofIsoString
        |> DarkDateTime.fromInstant
        |> DDateTime
        |> Ply
      with _ ->
        raiseCantMatchWithType TDateTime j pathSoFar


    // Nested types

    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray()
      |> Seq.mapi (fun i v -> convert nested (JsonPath.Index i :: pathSoFar) v)
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (Dval.list VT.unknownTODO)

    | TTuple(t1, t2, rest), JsonValueKind.Array ->
      let values = j.EnumerateArray() |> Seq.toList
      let types = t1 :: t2 :: rest
      if values.Length <> types.Length then raiseCantMatchWithType typ j pathSoFar

      List.zip types values
      |> List.mapi (fun i (t, v) -> convert t (JsonPath.Index i :: pathSoFar) v)
      |> Ply.List.flatten
      |> Ply.map (fun mapped ->
        match mapped with
        | (d1 :: d2 :: rest) -> DTuple(d1, d2, rest)
        | _ -> Exception.raiseInternal "Invalid tuple" [])

    | TDict tDict, JsonValueKind.Object ->
      j.EnumerateObject()
      |> Seq.map (fun jp ->
        uply {
          let! converted =
            convert tDict (JsonPath.Field jp.Name :: pathSoFar) jp.Value
          return (jp.Name, converted)
        })
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (Dval.dict VT.unknownTODO)

    | TCustomType(Ok typeName, typeArgs), jsonValueKind ->
      uply {
        match! Types.find typeName types with
        | None ->
          // TODO should be an RTE
          return
            JsonParseError.TypeNotFound typ
            |> JsonParseError.JsonParseException
            |> raise

        | Some decl ->
          match decl.definition with
          | TypeDeclaration.Alias alias ->
            let aliasType = Types.substitute decl.typeParams typeArgs alias
            return! convert aliasType pathSoFar j

          | TypeDeclaration.Enum cases ->
            if jsonValueKind <> JsonValueKind.Object then
              raiseCantMatchWithType typ j pathSoFar

            let enumerated =
              j.EnumerateObject()
              |> Seq.map (fun jp -> (jp.Name, jp.Value))
              |> Seq.toList

            match enumerated with
            | [ (caseName, j) ] ->
              let matchingCase =
                cases
                |> NEList.find (fun c -> c.name = caseName)
                |> Exception.unwrapOptionInternal
                  "Couldn't find matching case"
                  [ "caseName ", caseName ]

              let j = j.EnumerateArray() |> Seq.toList

              if List.length matchingCase.fields <> List.length j then
                Exception.raiseInternal
                  $"Couldn't parse Enum as incorrect # of fields provided"
                  []

              let! fields =
                List.zip matchingCase.fields j
                |> List.map (fun (typ, j) ->
                  let typ = Types.substitute decl.typeParams typeArgs typ
                  convert typ pathSoFar j) // TODO revisit if we need to do anything with path
                |> Ply.List.flatten

              return! Dval.enum typeName typeName VT.typeArgsTODO' caseName fields

            // TODO shouldn't be an internal error
            | _ -> return Exception.raiseInternal "TODO" []

          | TypeDeclaration.Record fields ->
            if jsonValueKind <> JsonValueKind.Object then
              // TODO should be user facing
              Exception.raiseInternal
                "Expected an object for a record"
                [ "type", typeName; "value", j ]

            let enumerated = j.EnumerateObject() |> Seq.toList

            let! fields =
              fields
              |> NEList.toList
              |> List.map (fun def ->
                uply {
                  let correspondingValue =
                    let matchingFieldDef =
                      enumerated
                      // TODO: handle case where value isn't found for
                      // and maybe, if it's an Option<>al thing, don't complain
                      |> List.filter (fun v -> v.Name = def.name)

                    match matchingFieldDef with
                    | [] ->
                      // TODO should be user-facing error
                      Exception.raiseInternal "Couldn't find matching field" []
                    | [ matchingFieldDef ] -> matchingFieldDef.Value
                    // TODO should be a user-facing error
                    | _ -> Exception.raiseInternal "Too many matching fields" []

                  let typ = Types.substitute decl.typeParams typeArgs def.typ
                  let! converted =
                    convert
                      typ
                      (JsonPath.Field def.name :: pathSoFar)
                      correspondingValue
                  return (def.name, converted)
                })
              |> Ply.List.flatten

            return! Dval.record typeName VT.typeArgsTODO' fields
      }


    // Explicitly not supported
    | TVariable _, _
    | TFn _, _ ->
      JsonParseError.TypeUnsupported typ
      |> JsonParseError.JsonParseException
      |> raise

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
    | TList _, _
    | TTuple _, _
    | TCustomType _, _
    | TDict _, _ -> raiseCantMatchWithType typ j pathSoFar

  let parsed =
    try
      Ok(parseJson str)
    with _ex ->
      Error "not JSON"

  match parsed with
  | Error err -> Error err |> Ply
  | Ok parsed ->
    uply {
      try
        let! converted = convert typ [ JsonPath.Root ] parsed
        return Ok converted
      with JsonParseError.JsonParseException ex ->
        // CLEANUP expose .NET error (converting to some Dark error type)
        return JsonParseError.toString ex |> Error
    }


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Json" ]

let fns : List<BuiltInFn> =
  [ { name = fn "serialize" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "arg" (TVariable "a") "" ]
      returnType = TypeReference.result TString TString
      description = "Serializes a Dark value to a JSON string."
      fn =
        (function
        | state, [ typeToSerializeAs ], [ arg ] ->
          uply {
            // TODO: somehow collect list of TVariable -> TypeReference
            // "'b = Int",
            // so we can Json.serialize<'b>, if 'b is in the surrounding context

            // CLEANUP this should not return a Result
            // if anything fails due to types, it should result as an InternalException
            // naturally
            let types = ExecutionState.availableTypes state
            let! response =
              writeJson (fun w -> serialize types w typeToSerializeAs arg)
            return Dval.resultOk VT.string VT.string (DString response)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "parse" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "json" TString "" ]
      returnType = TypeReference.result (TVariable "a") TString
      description =
        "Parses a JSON string <param json> as a Dark value, matching the type <typeParam a>"
      fn =
        let resultOk = Dval.resultOk VT.unknownTODO VT.string
        let resultError = Dval.resultError VT.unknownTODO VT.string
        (function
        | state, [ typeArg ], [ DString arg ] ->
          let types = ExecutionState.availableTypes state
          uply {
            match! parse types typeArg arg with
            | Ok v -> return resultOk v
            | Error e -> return resultError (DString e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let contents = (fns, types, constants)
