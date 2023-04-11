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
  (typ : TypeReference)
  (dv : Dval)
  : unit =
  let writeDval = toJsonV0 w

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
  | TCustomType (typeName, _), DRecord dvalMap ->
    Exception.raiseInternal "Pass in type map" []
  // let schema = Map.ofList fields
  // w.writeObject (fun () ->
  //   dvalMap
  //   |> Map.toList
  //   |> List.iter (fun (k, dval) ->
  //     w.WritePropertyName k
  //     writeDval (Map.find k schema) dval))

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
  (fieldTypes : List<string * TypeReference>)
  (dvalMap : DvalMap)
  : string =
  let fieldTypes = Map fieldTypes
  writeJson (fun w ->
    w.writeObject (fun () ->
      dvalMap
      |> Map.toList
      |> List.iter (fun (k, dval) ->
        w.WritePropertyName k
        toJsonV0 w (Map.find k fieldTypes) dval)))


let parseJsonV0 (typ : TypeReference) (str : string) : Dval =
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
    // | TTuple (t1, t2, rest), JsonValueKind.Array ->
    //   j.EnumerateArray() |> Seq.map (convert nested) |> Seq.toList |> DList
    | TDict typ, JsonValueKind.Object ->
      let objFields =
        j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map
      objFields |> Map.mapWithIndex (fun k v -> convert typ v) |> DDict
    | TCustomType _, JsonValueKind.Object ->
      Exception.raiseInternal "Need actual type" []
    //   // Use maps to cooalesce duplicate keys and ensure the obj matches the type
    //   let typFields = Map typFields
    //   let objFields =
    //     j.EnumerateObject() |> Seq.map (fun jp -> (jp.Name, jp.Value)) |> Map
    //   if Map.count objFields = Map.count typFields then
    //     objFields
    //     |> Map.mapWithIndex (fun k v ->
    //       match Map.tryFind k typFields with
    //       | Some t -> convert t v
    //       | None -> Exception.raiseInternal "Missing field" [ "field", k ])
    //     |> DDict // CLEANUPRECORD
    //   else
    //     Exception.raiseInternal "Invalid fields" []
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
    | DConstructor (_typeName, _caseName, fields) ->
      // TODO I'm not sure what's appropriate here.
      fields |> List.all isQueryableDval

    // TODO support
    | DRecord _ // CLEANUPRECORD
    | DTuple _
    | DBytes _
    | DHttpResponse _
    | DOption _
    | DResult _

    // Maybe never support
    | DFnVal _
    | DError _
    | DDB _
    | DIncomplete _ -> false
