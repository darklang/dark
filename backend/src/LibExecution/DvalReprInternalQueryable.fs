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
module VT = ValueType


let parseJson (s : string) : JsonElement =
  let options =
    new JsonDocumentOptions(
      CommentHandling = JsonCommentHandling.Skip,
      MaxDepth = System.Int32.MaxValue // infinite
    )

  JsonDocument.Parse(s, options).RootElement

let writeJson (f : Utf8JsonWriter -> Ply<unit>) : Ply<string> =
  uply {
    let options =
      new JsonWriterOptions(
        Indented = true,
        SkipValidation = true,
        Encoder =
          System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
      )

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
// UUID

let rec private toJsonV0
  (w : Utf8JsonWriter)
  (callStack : CallStack)
  (types : Types)
  (typ : TypeReference)
  (dv : Dval)
  : Ply<unit> =
  uply {
    let writeDval = toJsonV0 w callStack types

    match typ, dv with
    // basic types
    | TUnit, DUnit -> w.WriteNumberValue(0)
    | TBool, DBool b -> w.WriteBooleanValue b
    | TInt64, DInt64 i -> w.WriteNumberValue i // CLEANUP if the number is outside the range, store as a string?
    | TUInt64, DUInt64 i -> w.WriteNumberValue i
    | TInt8, DInt8 i -> w.WriteNumberValue i
    | TUInt8, DUInt8 i -> w.WriteNumberValue i
    | TInt16, DInt16 i -> w.WriteNumberValue i
    | TUInt16, DUInt16 i -> w.WriteNumberValue i
    | TInt32, DInt32 i -> w.WriteNumberValue i
    | TUInt32, DUInt32 i -> w.WriteNumberValue i
    | TInt128, DInt128 i -> w.WriteRawValue(i.ToString())
    | TUInt128, DUInt128 i -> w.WriteRawValue(i.ToString())
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
    | TDateTime, DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

    // nested types
    | TList ltype, DList(_, l) ->
      do! w.writeArray (fun () -> Ply.List.iterSequentially (writeDval ltype) l)

    | TTuple(t1, t2, trest), DTuple(d1, d2, rest) ->
      let zipped = List.zip (t1 :: t2 :: trest) (d1 :: d2 :: rest)
      do!
        w.writeArray (fun () ->
          Ply.List.iterSequentially (fun (t, d) -> writeDval t d) zipped)

    | TDict objType, DDict(_typeArgsTODO, o) ->
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

        | TypeDeclaration.Record fields, DRecord(_, _, _, dm) ->
          let fields = NEList.toList fields
          do!
            w.writeObject (fun () ->
              Ply.List.iterSequentially
                (fun (f : TypeDeclaration.RecordField) ->
                  uply {
                    w.WritePropertyName f.name
                    let dval = Map.findUnsafe f.name dm
                    let typ = Types.substitute decl.typeParams typeArgs f.typ
                    do! writeDval typ dval
                  })
                fields)

        | TypeDeclaration.Enum cases,
          DEnum(_, _, _typeArgsDEnumTODO, caseName, fields) ->
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

    | TCustomType(Error err, _), _ -> raiseRTE callStack err

    // Not supported
    | TVariable _, _
    | TFn _, DFnVal _
    | TDB _, DDB _ ->
      Exception.raiseInternal
        "Not supported in queryable"
        [ "value", dv; "type", typ ]

    // exhaustiveness checking
    | TInt64, _
    | TUInt64, _
    | TInt8, _
    | TUInt8, _
    | TInt16, _
    | TUInt16, _
    | TInt32, _
    | TUInt32, _
    | TInt128, _
    | TUInt128, _
    | TFloat, _
    | TBool, _
    | TUnit, _
    | TString, _
    | TList _, _
    | TDict _, _
    | TChar, _
    | TDateTime, _
    | TUuid, _
    | TTuple _, _
    | TDB _, _
    | TFn _, _ ->
      Exception.raiseInternal
        "Value to be stored does not match Datastore type"
        [ "value", dv; "type", typ ]
  }



let toJsonStringV0
  (callStack : CallStack)
  (types : Types)
  (typ : TypeReference)
  (dval : Dval)
  : Ply<string> =
  writeJson (fun w -> toJsonV0 w callStack types typ dval)


let parseJsonV0
  (callStack : CallStack)
  (types : Types)
  (typ : TypeReference)
  (str : string)
  : Ply<Dval> =
  let rec convert (typ : TypeReference) (j : JsonElement) : Ply<Dval> =
    match typ, j.ValueKind with
    // simple cases
    | TUnit, JsonValueKind.Number -> DUnit |> Ply
    | TBool, JsonValueKind.True -> DBool true |> Ply
    | TBool, JsonValueKind.False -> DBool false |> Ply
    | TInt64, JsonValueKind.Number -> j.GetInt64() |> DInt64 |> Ply
    | TUInt64, JsonValueKind.Number -> j.GetUInt64() |> DUInt64 |> Ply
    | TInt8, JsonValueKind.Number -> j.GetSByte() |> DInt8 |> Ply
    | TUInt8, JsonValueKind.Number -> j.GetByte() |> DUInt8 |> Ply
    | TInt16, JsonValueKind.Number -> j.GetInt16() |> DInt16 |> Ply
    | TUInt16, JsonValueKind.Number -> j.GetUInt16() |> DUInt16 |> Ply
    | TInt32, JsonValueKind.Number -> j.GetInt32() |> DInt32 |> Ply
    | TUInt32, JsonValueKind.Number -> j.GetUInt32() |> DUInt32 |> Ply
    | TInt128, JsonValueKind.Number ->
      j.GetRawText() |> System.Int128.Parse |> DInt128 |> Ply
    | TUInt128, JsonValueKind.Number ->
      j.GetRawText() |> System.UInt128.Parse |> DUInt128 |> Ply
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
      |> Ply.map (TypeChecker.DvalCreator.list callStack VT.unknownTODO)

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
      |> Ply.map (TypeChecker.DvalCreator.dict VT.unknownTODO)


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
                    match Map.find f.name objFields with
                    | Some j ->
                      let typ = Types.substitute decl.typeParams typeArgs f.typ
                      convert typ j
                    | None ->
                      Exception.raiseInternal "Missing field" [ "field", f.name ]

                  dval |> Ply.map (fun dval -> f.name, dval))
                |> Ply.List.flatten
                // TYPESCLEANUP: I don't think the original is name right here?
                |> Ply.map (fun mapped ->
                  DRecord(typeName, typeName, VT.typeArgsTODO, Map mapped))
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

              // TYPESCLEANUP: I don't think the sourceTypeName is right here?
              return! TypeChecker.DvalCreator.enum typeName typeName caseName fields
          | _, _ ->
            return
              Exception.raiseInternal
                "Couldn't parse custom type"
                [ "typeName", typeName; "valueKind", valueKind ]
      }
    | TFn _, _ -> Exception.raiseInternal "Fn values not supported" []
    | TDB _, _ -> Exception.raiseInternal "DB values not supported" []
    | TVariable _, _ -> Exception.raiseInternal "Variables not supported yet" []

    // Exhaustiveness checking
    | TUnit, _
    | TBool, _
    | TInt64, _
    | TUInt64, _
    | TInt8, _
    | TUInt8, _
    | TInt16, _
    | TUInt16, _
    | TInt32, _
    | TUInt32, _
    | TInt128, _
    | TUInt128, _
    | TFloat, _
    | TChar, _
    | TString, _
    | TUuid, _
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
    | DUnit
    | DBool _
    | DInt8 _
    | DUInt8 _
    | DInt16 _
    | DUInt16 _
    | DInt32 _
    | DUInt32 _
    | DInt64 _
    | DUInt64 _
    | DInt128 _
    | DUInt128 _
    | DFloat _
    | DChar _
    | DString _
    | DDateTime _
    | DUuid _ -> true

    // VTTODO these should probably just check the valueType, not any internal data
    | DList(_, dvals) -> List.all isQueryableDval dvals
    | DDict(_, map) -> map |> Map.values |> List.all isQueryableDval
    | DTuple(d1, d2, rest) -> List.all isQueryableDval (d1 :: d2 :: rest)

    | DEnum(_typeName, _, _, _caseName, fields) -> fields |> List.all isQueryableDval

    // TODO support
    | DRecord _ // TYPESCLEANUP

    // Maybe never support
    | DFnVal _
    | DDB _ -> false
