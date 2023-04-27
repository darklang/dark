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
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (typ : TypeReference)
  (dv : Dval)
  : unit =
  let writeDval = toJsonV0 w availableTypes

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
  | TPassword, DPassword (Password hashed) ->
    hashed |> Base64.defaultEncodeToString |> w.WriteStringValue
  | TList ltype, DList l -> w.writeArray (fun () -> List.iter (writeDval ltype) l)
  | TTuple (t1, t2, trest), DTuple (d1, d2, rest) ->
    w.writeArray (fun () ->
      List.iter2 writeDval (t1 :: t2 :: trest) (d1 :: d2 :: rest))
  | TDict objType, DDict o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          writeDval objType v)
        o)
  | TCustomType (typeName, args), dv ->
    match Map.tryFind typeName availableTypes, dv with
    | None, _ -> Exception.raiseInternal "Type not found" [ "typeName", typeName ]
    | Some (CustomType.Record (f1, fs)), DRecord (_, dm) ->
      // TYPESCLEANUP: shouldn't we be using `args` here?
      let fields = f1 :: fs
      w.writeObject (fun () ->
        fields
        |> List.iter (fun f ->
          w.WritePropertyName f.name
          let dval = Map.find f.name dm
          writeDval f.typ dval))
    | Some (CustomType.Enum _), DEnum _ ->
      Exception.raiseInternal "Enum not handled yet" [ "typeName", typeName ]
    | Some typ, dv ->
      Exception.raiseInternal
        "Value to be stored does not match Datastore type"
        [ "value", dv; "type", typ; "typeName", typeName ]

  // Not supported
  | TOption _, DOption None -> w.writeObject (fun () -> w.WriteNull "Nothing")
  | TOption oType, DOption (Some dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Just"
      writeDval oType dv)
  | TResult (okType, _), DResult (Ok dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Ok"
      writeDval okType dv)
  | TResult (_, errType), DResult (Error dv) ->
    w.writeObject (fun () ->
      w.WritePropertyName "Error"
      writeDval errType dv)
  | THttpResponse _, DHttpResponse _
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
  | TCustomType _, _
  | TChar, _
  | TDateTime, _
  | TPassword, _
  | TUuid, _
  | TTuple _, _
  | TBytes, _
  | TOption _, _
  | TResult _, _
  | THttpResponse _, _
  | TVariable _, _ // CLEANUP: pass the map of variable names in
  | TDB _, _
  | TFn _, _ ->
    Exception.raiseInternal
      "Value to be stored does not match Datastore type"
      [ "value", dv; "type", typ ]



let toJsonStringV0
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (typ : TypeReference)
  (dval : Dval)
  : string =
  writeJson (fun w -> toJsonV0 w availableTypes typ dval)


let parseJsonV0
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (typ : TypeReference)
  (str : string)
  : Dval =
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
    | TTuple (t1, t2, rest), JsonValueKind.Array ->
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
    | TCustomType (typeName, args), JsonValueKind.Object ->
      match Map.tryFind typeName availableTypes with
      | None -> Exception.raiseInternal "Type not found" [ "typeName", typeName ]
      | Some (CustomType.Record (f1, fs)) ->
        let fields = f1 :: fs
        let objFields =
          j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map
        if Map.count objFields = List.length fields then
          fields
          |> List.map (fun f ->
            let dval =
              match Map.tryFind f.name objFields with
              | Some j -> convert f.typ j
              | None -> Exception.raiseInternal "Missing field" [ "field", f.name ]
            f.name, dval)
          |> Map
          |> fun map -> DRecord(typeName, map)
        else
          Exception.raiseInternal
            "Record has incorrect field count"
            [ "expected", List.length fields; "actual", Map.count objFields ]
      | Some (CustomType.Enum (f1, fs)) ->
        Exception.raiseInternal "Enum not handled yet" [ "typeName", typeName ]

    | TBytes _, _ -> Exception.raiseInternal "Not supported yet" []
    | TOption _, _ -> Exception.raiseInternal "Not supported yet" []
    | TResult _, _ -> Exception.raiseInternal "Not supported yet" []
    | THttpResponse _, _ -> Exception.raiseInternal "Not supported yet" []
    | TFn _, _ -> Exception.raiseInternal "Not supported yet" []
    | TDB _, _ -> Exception.raiseInternal "Not supported yet" []
    | TVariable _, _ -> Exception.raiseInternal "Not supported yet" []
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
    | DEnum (_typeName, _caseName, fields) -> fields |> List.all isQueryableDval
    | DTuple (d1, d2, rest) -> List.all isQueryableDval (d1 :: d2 :: rest)

    // TODO support
    | DRecord _ // TYPESCLEANUP
    | DBytes _
    | DHttpResponse _
    | DOption _
    | DResult _

    // Maybe never support
    | DFnVal _
    | DError _
    | DDB _
    | DIncomplete _ -> false
