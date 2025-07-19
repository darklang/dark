/// Ways of converting Dvals to/from Sqlite-compatible JSON blobs.
///
/// These are intended to be used exclusively internally.
/// That is, they should not be used in libraries, BwdServer, HttpClient, etc.
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

let rec private toJsonV0
  (w : Utf8JsonWriter)
  (threadID : ThreadID)
  (types : Types)
  (dv : Dval)
  : Ply<unit> =
  uply {
    let writeDval = toJsonV0 w threadID types

    match dv with
    // basic types
    | DUnit -> w.WriteNumberValue 0

    | DBool b -> w.WriteBooleanValue b

    | DInt8 i -> w.WriteNumberValue i
    | DUInt8 i -> w.WriteNumberValue i
    | DInt16 i -> w.WriteNumberValue i
    | DUInt16 i -> w.WriteNumberValue i
    | DInt32 i -> w.WriteNumberValue i
    | DUInt32 i -> w.WriteNumberValue i
    | DInt64 i -> w.WriteNumberValue i
    | DUInt64 i -> w.WriteNumberValue i
    | DInt128 i -> w.WriteRawValue(i.ToString())
    | DUInt128 i -> w.WriteRawValue(i.ToString())

    | DFloat f ->
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

    | DChar c -> w.WriteStringValue c
    | DString s -> w.WriteStringValue s

    | DUuid uuid -> w.WriteStringValue(string uuid)

    | DDateTime date -> w.WriteStringValue(DarkDateTime.toIsoString date)

    // nested types
    | DTuple(d1, d2, rest) ->
      do!
        w.writeArray (fun () ->
          Ply.List.iterSequentially writeDval (d1 :: d2 :: rest))

    | DList(_, l) ->
      do! w.writeArray (fun () -> Ply.List.iterSequentially writeDval l)

    | DDict(_typeArgsTODO, o) ->
      do!
        w.writeObject (fun () ->
          Ply.List.iterSequentially
            (fun (k : string, v) ->
              uply {
                w.WritePropertyName k
                do! writeDval v
              })
            (Map.toList o))

    | DRecord(_, _, _typeArgsDEnum, fields) ->
      do!
        w.writeObject (fun () ->
          Ply.List.iterSequentially
            (fun (k : string, v) ->
              uply {
                w.WritePropertyName k
                do! writeDval v
              })
            (Map.toList fields))

    | DEnum(_, _, _typeArgsDEnum, caseName, fields) ->
      do!
        w.writeObject (fun () ->
          w.WritePropertyName caseName
          // TODO: this might be where the type args go? hmmm
          w.writeArray (fun () ->
            fields
            |> Ply.List.iterSequentially (fun fieldVal -> writeDval fieldVal)))


    // Not supported
    | DApplicable _
    | DDB _ -> Exception.raiseInternal "Not supported in queryable" [ "value", dv ]
  }



let toJsonStringV0
  (types : Types)
  (threadID : ThreadID)
  (dval : Dval)
  : Ply<string> =
  writeJson (fun w -> toJsonV0 w threadID types dval)


let parseJsonV0
  (types : Types)
  (threadID : ThreadID)
  (tst : TypeSymbolTable)
  (typ : TypeReference)
  (str : string)
  : Ply<Dval> =
  let rec convert (typ : TypeReference) (j : JsonElement) : Ply<Dval> =
    match typ, j.ValueKind with
    // simple cases
    | TUnit, JsonValueKind.Number -> DUnit |> Ply

    | TBool, JsonValueKind.True -> DBool true |> Ply
    | TBool, JsonValueKind.False -> DBool false |> Ply

    | TInt8, JsonValueKind.Number -> j.GetSByte() |> DInt8 |> Ply
    | TUInt8, JsonValueKind.Number -> j.GetByte() |> DUInt8 |> Ply
    | TInt16, JsonValueKind.Number -> j.GetInt16() |> DInt16 |> Ply
    | TUInt16, JsonValueKind.Number -> j.GetUInt16() |> DUInt16 |> Ply
    | TInt32, JsonValueKind.Number -> j.GetInt32() |> DInt32 |> Ply
    | TUInt32, JsonValueKind.Number -> j.GetUInt32() |> DUInt32 |> Ply
    | TInt64, JsonValueKind.Number -> j.GetInt64() |> DInt64 |> Ply
    | TUInt64, JsonValueKind.Number -> j.GetUInt64() |> DUInt64 |> Ply
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

    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray()
      |> Seq.map (convert nested)
      |> Seq.toList
      |> Ply.List.flatten
      |> Ply.map (TypeChecker.DvalCreator.list threadID VT.unknownTODO)

    | TDict typ, JsonValueKind.Object ->
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map

      objFields
      |> Map.toList
      |> List.map (fun (k, v) -> convert typ v |> Ply.map (fun v -> k, v))
      |> Ply.List.flatten
      |> Ply.map (TypeChecker.DvalCreator.dict threadID VT.unknownTODO)


    | TCustomType(Ok typeName, typeArgs), valueKind ->
      uply {
        match! Types.find types typeName with
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
              let! fields =
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

              return!
                TypeChecker.DvalCreator.record
                  types
                  threadID
                  tst
                  typeName
                  VT.typeArgsTODO
                  fields
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

              let! enum =
                TypeChecker.DvalCreator.enum
                  types
                  threadID
                  tst
                  typeName
                  []
                  caseName
                  fields
              return enum
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
    | TInt8, _
    | TUInt8, _
    | TInt16, _
    | TUInt16, _
    | TInt32, _
    | TUInt32, _
    | TInt64, _
    | TUInt64, _
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
    | DTuple(d1, d2, rest) -> List.all isQueryableDval (d1 :: d2 :: rest)
    | DList(_, dvals) -> List.all isQueryableDval dvals
    | DDict(_, map) -> map |> Map.values |> List.all isQueryableDval

    | DEnum(_typeName, _, _, _caseName, fields) -> fields |> List.all isQueryableDval

    // TODO support
    | DRecord _ // TYPESCLEANUP

    // Maybe never support
    | DApplicable _
    | DDB _ -> false
