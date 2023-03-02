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

let rec private toJsonV0 (w : Utf8JsonWriter) (dv : Dval) : unit =
  let writeDval = toJsonV0 w

  let wrapStringValue (typ : string) (str : string) =
    w.writeObject (fun () ->
      w.WritePropertyName "type"
      w.WriteStringValue(typ)
      w.WritePropertyName "value"
      w.WriteStringValue(str))

  match dv with
  // basic types
  | DInt i -> w.WriteNumberValue i
  | DFloat f ->
    // TODO: These can't be parsed as System.Text.Json doesn't allow it. I went ahead
    // with implementing this anyway as when we use types during serialization, we'll
    // be able to use `"Infinity"` (a string) and we'll know from the type that we
    // want a float not a string here. So I've disabled the infinity/NaN tests until
    // we have this in place.
    if System.Double.IsNaN f then
      w.WriteRawValue "NaN"
    else if System.Double.IsNegativeInfinity f then
      w.WriteRawValue "-Infinity"
    else if System.Double.IsPositiveInfinity f then
      w.WriteRawValue "Infinity"
    else
      let result = sprintf "%.12g" f
      let result = if result.Contains "." then result else $"{result}.0"
      w.WriteRawValue result
  | DBool b -> w.WriteBooleanValue b
  | DUnit -> w.WriteNullValue()
  | DStr s -> w.WriteStringValue s
  | DList l -> w.writeArray (fun () -> List.iter writeDval l)
  | DObj o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          writeDval v)
        o)
  | DChar c -> wrapStringValue "character" c
  | DDateTime date -> wrapStringValue "date" (DarkDateTime.toIsoString date)
  | DPassword (Password hashed) ->
    hashed |> Base64.defaultEncodeToString |> wrapStringValue "password"
  | DUuid uuid -> wrapStringValue "uuid" (string uuid)
  // Not supported
  | DTuple _
  | DFnVal _
  | DError _
  | DIncomplete _
  | DHttpResponse _
  | DDB _
  | DOption _
  | DResult _
  | DBytes _ -> Exception.raiseInternal "Not supported in queryable" []



let toJsonStringV0 (dvalMap : DvalMap) : string =
  writeJson (fun w ->
    w.writeObject (fun () ->
      dvalMap
      |> Map.toList
      |> List.iter (fun (k, dval) ->
        w.WritePropertyName k
        toJsonV0 w dval)))


let parseJsonV0 (str : string) : Dval =
  let rec convert (j : JsonElement) : Dval =
    match j.ValueKind with
    | JsonValueKind.Number ->
      let mutable i : int64 = 0L
      if j.TryGetInt64(&i) then DInt i else DFloat(j.GetDouble())
    | JsonValueKind.True -> DBool true
    | JsonValueKind.False -> DBool false
    | JsonValueKind.String -> DStr(j.GetString())
    | JsonValueKind.Null -> DUnit
    | JsonValueKind.Array ->
      j.EnumerateArray() |> Seq.map convert |> Seq.toList |> DList
    | JsonValueKind.Object ->
      let fields =
        j.EnumerateObject()
        |> Seq.map (fun jp -> (jp.Name, convert jp.Value))
        |> Seq.toList
        |> List.sortBy (fun (k, _) -> k)
      // These are the only types that are allowed in the queryable
      // representation. We may allow more in the future, but the real thing to
      // do is to use the DB's type and version to encode/decode them correctly
      match fields with
      | [ ("type", DStr "character"); ("value", DStr v) ] -> DChar v
      | [ ("type", DStr "date"); ("value", DStr v) ] ->
        DDateTime(NodaTime.Instant.ofIsoString v |> DarkDateTime.fromInstant)
      | [ ("type", DStr "password"); ("value", DStr v) ] ->
        v |> Base64.decodeFromString |> Password |> DPassword
      | [ ("type", DStr "uuid"); ("value", DStr v) ] -> DUuid(System.Guid v)
      | _ -> fields |> Map.ofList |> DObj
    | _ ->
      Exception.raiseInternal
        "Invalid type in internalQueryableV1 json"
        [ "json", j ]

  str |> parseJson |> convert



module Test =
  let rec isQueryableDval (dval : Dval) : bool =
    match dval with
    | DInt _
    | DStr _
    | DUnit _
    | DBool _
    | DDateTime _
    | DPassword _
    | DUuid _ -> true
    | DFloat f -> System.Double.IsFinite f // See comment above
    | DList dvals -> List.all isQueryableDval dvals
    | DObj map -> map |> Map.values |> List.all isQueryableDval
    // TODO support
    | DTuple _
    | DChar _
    | DBytes _
    | DHttpResponse _
    | DOption _
    | DResult _
    // Maybe never support
    | DFnVal _
    | DError _
    | DDB _
    | DIncomplete _ -> false
