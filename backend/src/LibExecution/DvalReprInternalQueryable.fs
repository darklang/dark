/// <summary>
/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in the BwdServer, in HttpClient,
/// etc.
/// </summary>
module LibExecution.DvalReprInternalQueryable

open Prelude
open VendoredTablecloth

open RuntimeTypes

open System.Text.Json


let parseJson (s : string) : JsonElement =
  let mutable options = new JsonDocumentOptions()
  options.CommentHandling <- JsonCommentHandling.Skip
  options.MaxDepth <- System.Int32.MaxValue // infinite

  JsonDocument.Parse(s, options).RootElement

let writeJson (f : Utf8JsonWriter -> unit) : string =
  let mutable options = new JsonWriterOptions()
  options.Indented <- true
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
  : unit =
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
  | TList ltype, DList l -> w.writeArray (fun () -> List.iter (writeDval ltype) l)
  | TTuple(t1, t2, trest), DTuple(d1, d2, rest) ->
    w.writeArray (fun () ->
      List.iter2 writeDval (t1 :: t2 :: trest) (d1 :: d2 :: rest))
  | TDict objType, DDict o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          writeDval objType v)
        o)
  | TCustomType(typeName, typeArgs), dv ->
    match Types.find typeName types with
    | None -> Exception.raiseInternal "Type not found" [ "typeName", typeName ]
    | Some decl ->
      match decl.definition, dv with
      | TypeDeclaration.Alias(typeRef), dv ->
        let typ = Types.substitute decl.typeParams typeArgs typeRef
        writeDval typ dv

      | TypeDeclaration.Record(f1, fs), DRecord(_, dm) ->
        w.writeObject (fun () ->
          f1 :: fs
          |> List.iter (fun f ->
            w.WritePropertyName f.name
            let dval = Map.find f.name dm
            let typ = Types.substitute decl.typeParams typeArgs f.typ
            writeDval typ dval))


      | TypeDeclaration.Enum(firstCase, additionalCases), DEnum(_, caseName, fields) ->

        let matchingCase =
          firstCase :: additionalCases
          |> List.find (fun c -> c.name = caseName)
          |> Exception.unwrapOptionInternal
            $"Couldn't find matching case for {caseName}"
            []

        let fieldDefs =
          matchingCase.fields
          |> List.map (fun def -> Types.substitute decl.typeParams typeArgs def.typ)

        if List.length fieldDefs <> List.length fields then
          Exception.raiseInternal
            $"Couldn't serialize Enum to as incorrect # of fields provided"
            [ "defs", fieldDefs
              "fields", fields
              "typeName", typeName
              "caseName", caseName ]

        w.writeObject (fun () ->
          w.WritePropertyName caseName
          w.writeArray (fun () ->
            List.zip fieldDefs fields
            |> List.iter (fun (fieldDef, fieldVal) -> writeDval fieldDef fieldVal)))


      | TypeDeclaration.Alias _, _
      | TypeDeclaration.Record _, _
      | TypeDeclaration.Enum _, _ ->
        Exception.raiseInternal
          "Value to be stored does not match a declared type"
          [ "value", dv; "type", typ; "typeName", typeName ]

  // Not supported
  | TOption _, DOption None -> w.writeObject (fun () -> w.WriteNull "Nothing")
  | TOption oType, DOption(Some dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Just"
      writeDval oType dv)

  | TVariable _, _
  | TFn _, DFnVal _
  | TDB _, DDB _ -> Exception.raiseInternal "Not supported in queryable" []

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
  | TOption _, _
  | TDB _, _
  | TFn _, _ ->
    Exception.raiseInternal
      "Value to be stored does not match Datastore type"
      [ "value", dv; "type", typ ]



let toJsonStringV0 (types : Types) (typ : TypeReference) (dval : Dval) : string =
  writeJson (fun w -> toJsonV0 w types typ dval)


let parseJsonV0 (types : Types) (typ : TypeReference) (str : string) : Dval =
  let rec convert (typ : TypeReference) (j : JsonElement) : Dval =
    match typ, j.ValueKind with
    | TUnit, JsonValueKind.Number -> DUnit
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
    | TUuid, JsonValueKind.String -> DUuid(System.Guid(j.GetString()))
    | TPassword, JsonValueKind.String ->
      j.GetString() |> Base64.decodeFromString |> Password |> DPassword
    | TDateTime, JsonValueKind.String ->
      j.GetString()
      |> NodaTime.Instant.ofIsoString
      |> DarkDateTime.fromInstant
      |> DDateTime
    | TList nested, JsonValueKind.Array ->
      j.EnumerateArray() |> Seq.map (convert nested) |> Seq.toList |> DList
    | TTuple(t1, t2, rest), JsonValueKind.Array ->
      let arr = j.EnumerateArray() |> Seq.toList
      if List.length arr = 2 + List.length rest then
        let d1 = convert t1 arr[0]
        let d2 = convert t2 arr[1]
        let rest = List.map2 convert rest arr[2..]
        DTuple(d1, d2, rest)
      else
        Exception.raiseInternal "Invalid tuple" []
    | TDict typ, JsonValueKind.Object ->
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map
      objFields |> Map.mapWithIndex (fun k v -> convert typ v) |> DDict
    | TCustomType(typeName, typeArgs), JsonValueKind.Object ->
      match Types.find typeName types with
      | None -> Exception.raiseInternal "Type not found" [ "typeName", typeName ]
      | Some decl ->
        match decl.definition with

        | TypeDeclaration.Alias(typeRef) ->
          let typ = Types.substitute decl.typeParams typeArgs typeRef
          convert typ j

        // JS object with the named fields
        | TypeDeclaration.Record(f1, fs) ->
          let fields = f1 :: fs
          let objFields =
            j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map
          if Map.count objFields = List.length fields then
            fields
            |> List.map (fun f ->
              let dval =
                match Map.tryFind f.name objFields with
                | Some j ->
                  let typ = Types.substitute decl.typeParams typeArgs f.typ
                  convert typ j
                | None ->
                  Exception.raiseInternal "Missing field" [ "field", f.name ]
              f.name, dval)
            |> Map
            |> fun map -> DRecord(typeName, map)
          else
            Exception.raiseInternal
              "Record has incorrect field count"
              [ "expected", List.length fields; "actual", Map.count objFields ]


        | TypeDeclaration.Enum(f1, fs) ->
          let fieldDefs = f1 :: fs
          let objFields =
            j.EnumerateObject()
            |> Seq.toList
            |> List.map (fun jp -> (jp.Name, jp.Value))

          match objFields with
          | []
          | _ :: _ :: _ -> Exception.raiseInternal "Invalid enum" []
          | [ caseName, fields ] ->
            let caseDesc =
              fieldDefs
              |> List.find (fun c -> c.name = caseName)
              |> Exception.unwrapOptionInternal "Couldn't find matching case" []

            let fieldTypes =
              caseDesc.fields
              |> List.map (fun def ->
                Types.substitute decl.typeParams typeArgs def.typ)

            let fields =
              fields.EnumerateArray() |> Seq.map2 convert fieldTypes |> Seq.toList
            DEnum(typeName, caseName, fields)

    | TBytes _, _ -> Exception.raiseInternal "Bytes values not supported yet" []
    | TOption _, _ -> Exception.raiseInternal "Option values not supported yet" []

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
    | DList dvals -> List.all isQueryableDval dvals
    | DDict map -> map |> Map.values |> List.all isQueryableDval
    | DEnum(_typeName, _caseName, fields) -> fields |> List.all isQueryableDval
    | DTuple(d1, d2, rest) -> List.all isQueryableDval (d1 :: d2 :: rest)

    // TODO support
    | DRecord _ // TYPESCLEANUP
    | DBytes _
    | DOption _

    // Maybe never support
    | DFnVal _
    | DError _
    | DDB _
    | DIncomplete _ -> false
