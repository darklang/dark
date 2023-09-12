/// <summary>
/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in the BwdServer, in HttpClient,
/// etc.
/// </summary>
module LibExecution.DvalReprInternalQueryable

open System.Text.Json

open Prelude

open RuntimeTypes


let parseJson (s : string) : JsonElement =
  let mutable options = new JsonDocumentOptions()
  options.CommentHandling <- JsonCommentHandling.Skip
  options.MaxDepth <- System.Int32.MaxValue // infinite

  JsonDocument.Parse(s, options).RootElement

let writeJson (f : Utf8JsonWriter -> Ply<unit>) : Ply<string> =
  uply {
    let mutable options = new JsonWriterOptions()
    options.Indented <- true
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

// -------------------------
// Queryable - for the DB
// -------------------------

// This is a format used for roundtripping dvals internally, while still being
// queryable using jsonb in our DB. Also roundtrippable, but has some records it
// doesn't support (notably a record with both "type" and "value" keys in the right
// shape). Does not redact.

// The only formats allowed in the DB so far:
// Int
// Float
// Boolean
// String
// List
// Dict
// Date
// Password
// UUID

let rec private toJsonV0
  (w : Utf8JsonWriter)
  (types : Types)
  (typ : TypeReference)
  (dv : Dval)
  : Ply<unit> =
  uply {
    let writeDval = toJsonV0 w types

    match typ, dv with
    // basic types
    | TUnit, DUnit -> w.WriteNumberValue(0)
    | TBool, DBool b -> w.WriteBooleanValue b
    | TInt, DInt i -> w.WriteNumberValue i // CLEANUP if the number is outside the range, store as a string?
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
    | TUuid, DUuid uuid -> w.WriteStringValue(string uuid)
    | TBytes, DBytes bytes ->
      bytes |> Base64.defaultEncodeToString |> w.WriteStringValue
    | TDateTime, DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)
    | TPassword, DPassword(Password hashed) ->
      hashed |> Base64.defaultEncodeToString |> w.WriteStringValue

    // nested types
    | TList ltype, DList(_, l) ->
      do! w.writeArray (fun () -> Ply.List.iterSequentially (writeDval ltype) l)

    | TTuple(t1, t2, trest), DTuple(d1, d2, rest) ->
      let zipped = List.zip (t1 :: t2 :: trest) (d1 :: d2 :: rest)
      do!
        w.writeArray (fun () ->
          Ply.List.iterSequentially (fun (t, d) -> writeDval t d) zipped)

    | TDict objType, DDict o ->
      do!
        w.writeObject (fun () ->
          Ply.List.iterSequentially
            (fun (k : string, v) ->
              uply {
                w.WritePropertyName k
                do! writeDval objType v
              })
            (Map.toList o))

    | TCustomType(Ok typeName, typeArgs), dv ->
      match! Types.find typeName types with
      | None -> Exception.raiseInternal "Type not found" [ "typeName", typeName ]
      | Some decl ->
        match decl.definition, dv with
        | TypeDeclaration.Alias typeRef, dv ->
          let typ = Types.substitute decl.typeParams typeArgs typeRef
          do! writeDval typ dv

        | TypeDeclaration.Record fields, DRecord(_, _, dm) ->
          let fields = NEList.toList fields
          do!
            w.writeObject (fun () ->
              Ply.List.iterSequentially
                (fun (f : TypeDeclaration.RecordField) ->
                  uply {
                    w.WritePropertyName f.name
                    let dval = Map.find f.name dm
                    let typ = Types.substitute decl.typeParams typeArgs f.typ
                    do! writeDval typ dval
                  })
                fields)

        | TypeDeclaration.Enum(cases), DEnum(_, _, caseName, fields) ->

          let matchingCase =
            cases
            |> NEList.find (fun c -> c.name = caseName)
            |> Exception.unwrapOptionInternal
              $"Couldn't find matching case for {caseName}"
              []

          let fieldDefs =
            matchingCase.fields
            |> List.map (Types.substitute decl.typeParams typeArgs)

          if List.length fieldDefs <> List.length fields then
            Exception.raiseInternal
              $"Couldn't serialize Enum to as incorrect # of fields provided"
              [ "defs", fieldDefs
                "fields", fields
                "typeName", typeName
                "caseName", caseName ]

          do!
            w.writeObject (fun () ->
              w.WritePropertyName caseName
              w.writeArray (fun () ->
                List.zip fieldDefs fields
                |> Ply.List.iterSequentially (fun (fieldDef, fieldVal) ->
                  writeDval fieldDef fieldVal)))


        | TypeDeclaration.Alias _, _
        | TypeDeclaration.Record _, _
        | TypeDeclaration.Enum _, _ ->
          Exception.raiseInternal
            "Value to be stored does not match a declared type"
            [ "value", dv; "type", typ; "typeName", typeName ]

    | TCustomType(Error err, _), _ -> raiseRTE err

    // Not supported
    | TVariable _, _
    | TFn _, DFnVal _
    | TDB _, DDB _ ->
      Exception.raiseInternal
        "Not supported in queryable"
        [ "value", dv; "type", typ ]

    // exhaustiveness checking
    | TInt, _
    | TFloat, _
    | TBool, _
    | TUnit, _
    | TString, _
    | TList _, _
    | TDict _, _
    | TChar, _
    | TDateTime, _
    | TPassword, _
    | TUuid, _
    | TTuple _, _
    | TBytes, _
    | TDB _, _
    | TFn _, _ ->
      Exception.raiseInternal
        "Value to be stored does not match Datastore type"
        [ "value", dv; "type", typ ]
  }



let toJsonStringV0
  (types : Types)
  (typ : TypeReference)
  (dval : Dval)
  : Ply<string> =
  writeJson (fun w -> toJsonV0 w types typ dval)


let parseJsonV0 (types : Types) (typ : TypeReference) (str : string) : Ply<Dval> =
  let rec convert (typ : TypeReference) (j : JsonElement) : Ply<Dval> =
    match typ, j.ValueKind with
    // simple cases
    | TUnit, JsonValueKind.Number -> DUnit |> Ply
    | TBool, JsonValueKind.True -> DBool true |> Ply
    | TBool, JsonValueKind.False -> DBool false |> Ply
    | TInt, JsonValueKind.Number -> j.GetInt64() |> DInt |> Ply
    | TFloat, JsonValueKind.Number -> j.GetDouble() |> DFloat |> Ply
    | TFloat, JsonValueKind.String ->
      match j.GetString() with
      | "NaN" -> DFloat System.Double.NaN
      | "Infinity" -> DFloat System.Double.PositiveInfinity
      | "-Infinity" -> DFloat System.Double.NegativeInfinity
      | v -> Exception.raiseInternal "Invalid float" [ "value", v ]
      |> Ply
    | TChar, JsonValueKind.String -> DChar(j.GetString()) |> Ply
    | TString, JsonValueKind.String -> DString(j.GetString()) |> Ply
    | TUuid, JsonValueKind.String -> DUuid(System.Guid(j.GetString())) |> Ply
    | TPassword, JsonValueKind.String ->
      j.GetString() |> Base64.decodeFromString |> Password |> DPassword |> Ply
    | TDateTime, JsonValueKind.String ->
      j.GetString()
      |> NodaTime.Instant.ofIsoString
      |> DarkDateTime.fromInstant
      |> DDateTime
      |> Ply


    // nested structures
    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray()
      |> Seq.map (convert nested)
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (Dval.list valueTypeTODO)

    | TTuple(t1, t2, rest), JsonValueKind.Array ->
      let arr = j.EnumerateArray() |> Seq.toList
      if List.length arr = 2 + List.length rest then
        uply {
          let! d1 = convert t1 arr[0]
          let! d2 = convert t2 arr[1]
          let! rest = List.map2 convert rest arr[2..] |> Ply.List.flatten
          return DTuple(d1, d2, rest)
        }
      else
        Exception.raiseInternal "Invalid tuple" []

    | TDict typ, JsonValueKind.Object ->
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map

      objFields
      |> Map.toList
      |> List.map (fun (k, v) -> convert typ v |> Ply.map (fun v -> k, v))
      |> Ply.List.flatten
      |> Ply.map (Map >> DDict)


    | TCustomType(Ok typeName, typeArgs), valueKind ->
      uply {
        match! Types.find typeName types with
        | None ->
          return Exception.raiseInternal "Type not found" [ "typeName", typeName ]
        | Some decl ->
          match decl.definition, valueKind with
          | TypeDeclaration.Alias(typeRef), _ ->
            let typ = Types.substitute decl.typeParams typeArgs typeRef
            return! convert typ j

          // JS object with the named fields
          | TypeDeclaration.Record fields, JsonValueKind.Object ->
            let fields = NEList.toList fields
            let objFields =
              j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map
            if Map.count objFields = List.length fields then
              return!
                fields
                |> List.map (fun f ->
                  let dval =
                    match Map.tryFind f.name objFields with
                    | Some j ->
                      let typ = Types.substitute decl.typeParams typeArgs f.typ
                      convert typ j
                    | None ->
                      Exception.raiseInternal "Missing field" [ "field", f.name ]

                  dval |> Ply.map (fun dval -> f.name, dval))
                |> Ply.List.flatten
                // TYPESCLEANUP: I don't think the original is name right here?
                |> Ply.map (fun mapped -> DRecord(typeName, typeName, Map mapped))
            else
              return
                Exception.raiseInternal
                  "Record has incorrect field count"
                  [ "expected", List.length fields; "actual", Map.count objFields ]

          | TypeDeclaration.Enum cases, JsonValueKind.Object ->
            let objFields =
              j.EnumerateObject()
              |> Seq.toList
              |> List.map (fun jp -> (jp.Name, jp.Value))

            match objFields with
            | []
            | _ :: _ :: _ -> return Exception.raiseInternal "Invalid enum" []
            | [ caseName, fields ] ->
              let caseDesc =
                cases
                |> NEList.find (fun c -> c.name = caseName)
                |> Exception.unwrapOptionInternal "Couldn't find matching case" []

              let fieldTypes =
                caseDesc.fields
                |> List.map (Types.substitute decl.typeParams typeArgs)

              let! fields =
                fields.EnumerateArray()
                |> Seq.map2 convert fieldTypes
                |> Seq.toList
                |> Ply.List.flatten

              // TYPESCLEANUP: I don't think the original is name right here?
              return DEnum(typeName, typeName, caseName, fields)
          | _, _ ->
            return
              Exception.raiseInternal
                "Couldn't parse custom type"
                [ "typeName", typeName; "valueKind", valueKind ]
      }
    | TBytes _, _ -> Exception.raiseInternal "Bytes values not supported yet" []

    | TFn _, _ -> Exception.raiseInternal "Fn values not supported" []
    | TDB _, _ -> Exception.raiseInternal "DB values not supported" []
    | TVariable _, _ -> Exception.raiseInternal "Variables not supported yet" []

    // Exhaustiveness checking
    | TUnit, _
    | TBool, _
    | TInt, _
    | TFloat, _
    | TChar, _
    | TString, _
    | TUuid, _
    | TPassword, _
    | TDateTime, _
    | TTuple _, _
    | TList _, _
    | TDict _, _
    | TCustomType _, _ ->
      Exception.raiseInternal
        "Value in Datastore does not match Datastore expected type"
        [ "type", typ; "value", j ]

  str |> parseJson |> convert typ



module Test =
  let rec isQueryableDval (dval : Dval) : bool =
    match dval with
    | DInt _
    | DString _
    | DUnit _
    | DBool _
    | DDateTime _
    | DPassword _
    | DChar _
    | DFloat _
    | DUuid _ -> true
    | DList(_, dvals) -> List.all isQueryableDval dvals
    | DDict map -> map |> Map.values |> List.all isQueryableDval
    | DEnum(_typeName, _, _caseName, fields) -> fields |> List.all isQueryableDval
    | DTuple(d1, d2, rest) -> List.all isQueryableDval (d1 :: d2 :: rest)

    // TODO support
    | DRecord _ // TYPESCLEANUP
    | DBytes _

    // Maybe never support
    | DFnVal _
    | DError _
    | DDB _ -> false
