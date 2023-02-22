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

open Newtonsoft.Json
open Newtonsoft.Json.Linq

let writeJson (f : JsonWriter -> unit) : string =
  let stream = new System.IO.StringWriter()
  let w = new JsonTextWriter(stream)
  // Match yojson
  w.FloatFormatHandling <- FloatFormatHandling.Symbol
  f w
  string stream

type JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEnd()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEnd()


let private parseJson (s : string) : JToken =
  let reader = new JsonTextReader(new System.IO.StringReader(s))
  let jls = JsonLoadSettings()
  jls.CommentHandling <- CommentHandling.Load // Load them so we can error later
  jls.DuplicatePropertyNameHandling <- DuplicatePropertyNameHandling.Error
  jls.CommentHandling <- CommentHandling.Ignore

  reader.DateParseHandling <- DateParseHandling.None
  JToken.ReadFrom(reader)


let (|JString|_|) (j : JToken) : Option<string> =
  match j.Type with
  | JTokenType.String -> Some(JString(j.Value<string>()))
  | _ -> None

let (|JNull|_|) (j : JToken) : Option<unit> =
  match j.Type with
  | JTokenType.Null -> Some(JNull)
  | _ -> None

let (|JInteger|_|) (j : JToken) : Option<int64> =
  match j.Type with
  | JTokenType.Integer -> Some(JInteger(j.Value<int64>()))
  | _ -> None

let (|JFloat|_|) (j : JToken) : Option<float> =
  match j.Type with
  | JTokenType.Float -> Some(JFloat(j.Value<float>()))
  | _ -> None

let (|JBoolean|_|) (j : JToken) : Option<bool> =
  match j.Type with
  | JTokenType.Boolean -> Some(JBoolean(j.Value<bool>()))
  | _ -> None

let (|JList|_|) (j : JToken) : Option<List<JToken>> =
  match j.Type with
  | JTokenType.Array -> Some(JList(j.Values<JToken>() |> Seq.toList))
  | _ -> None

let (|JObject|_|) (j : JToken) : Option<List<string * JToken>> =
  match j.Type with
  | JTokenType.Object ->
    let list =
      j.Values()
      |> seq
      |> Seq.toList
      |> List.map (fun (jp : JProperty) -> (jp.Name, jp.Value))

    Some(JObject list)
  | _ -> None


let (|JNonStandard|_|) (j : JToken) : Option<unit> =
  match j.Type with
  | JTokenType.None
  | JTokenType.Undefined
  | JTokenType.Constructor
  | JTokenType.Property
  | JTokenType.Guid
  | JTokenType.Raw
  | JTokenType.Bytes
  | JTokenType.TimeSpan
  | JTokenType.Uri
  | JTokenType.Comment
  | JTokenType.Date -> Some()
  | _ -> None

let rec private toJsonV0 (w : JsonWriter) (dv : Dval) : unit =
  let writeDval = toJsonV0 w

  let wrapStringValue (typ : string) (str : string) =
    w.writeObject (fun () ->
      w.WritePropertyName "type"
      w.WriteValue(typ)
      w.WritePropertyName "value"
      w.WriteValue(str))

  match dv with
  // basic types
  | DInt i -> w.WriteValue i
  | DFloat f -> w.WriteValue f
  | DBool b -> w.WriteValue b
  | DNull -> w.WriteNull()
  | DStr s -> w.WriteValue s
  | DList l -> w.writeArray (fun () -> List.iter writeDval l)
  | DObj o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          writeDval v)
        o)
  | DChar c -> wrapStringValue "character" c
  | DDate date -> wrapStringValue "date" (DDateTime.toIsoString date)
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
  | DErrorRail _
  | DResult _
  | DBytes _ -> Exception.raiseInternal "Not supported in queryable" []



// -------------------------
// Queryable - for the DB
// -------------------------

// This is a format used for roundtripping dvals internally, while still being
// queryable using jsonb in our DB. This reduces some of the v0 bugs, but at
// the cost of not supporting many typed that we'll want to put in it.  Also
// roundtrippable. Does not redact.
let toJsonStringV0 (dvalMap : DvalMap) : string =
  writeJson (fun w ->
    w.writeObject (fun () ->
      dvalMap
      |> Map.toList
      |> List.iter (fun (k, dval) ->
        w.WritePropertyName k
        toJsonV0 w dval)))

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

// This is a format used for roundtripping dvals internally, while still being
// queryable using jsonb in our DB. There are some rare cases where it will
// parse incorrectly without error. Throws on Json bugs.
let parseJsonV0 (str : string) : Dval =
  // The first level _must_ be an object at the moment
  let rec convertTopLevel (json : JToken) : Dval =
    match json with
    | JObject _ -> convert json
    | _ -> Exception.raiseInternal "Value that isn't an object" [ "json", json ]

  and convert (json : JToken) : Dval =
    match json with
    | JInteger i -> DInt i
    | JFloat f -> DFloat f
    | JBoolean b -> DBool b
    | JNull -> DNull
    | JString s -> DStr s
    | JList l ->
      // We shouldn't have saved dlist that have incompletes or error rails, but we might have
      l |> List.map convert |> Dval.list
    | JObject fields ->
      let fields = fields |> List.sortBy (fun (k, _) -> k)
      // These are the only types that are allowed in the queryable
      // representation. We may allow more in the future, but the real thing to
      // do is to use the DB's type and version to encode/decode them correctly
      match fields with
      | [ ("type", JString "date"); ("value", JString v) ] ->
        DDate(NodaTime.Instant.ofIsoString v |> DDateTime.fromInstant)
      | [ ("type", JString "password"); ("value", JString v) ] ->
        v |> Base64.decodeFromString |> Password |> DPassword
      | [ ("type", JString "uuid"); ("value", JString v) ] -> DUuid(System.Guid v)
      | _ -> fields |> List.map (fun (k, v) -> (k, convert v)) |> Map.ofList |> DObj
    // Json.NET does a bunch of magic based on the contents of various types.
    // For example, it has tokens for Dates, constructors, etc. We've tried to
    // disable all those so we fail if we see them. However, we might need to
    // just convert some of these into strings.
    | JNonStandard _
    | _ ->
      Exception.raiseInternal
        "Invalid type in internalQueryableV1 json"
        [ "json", json ]

  str |> parseJson |> convertTopLevel


module Test =
  let rec isQueryableDval (dval : Dval) : bool =
    match dval with
    | DInt i -> i > -4611686018427387904L && i < 4611686018427387904L
    | DStr _
    | DNull _
    | DBool _
    | DFloat _
    | DDate _
    | DPassword _
    | DUuid _ -> true
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
    | DIncomplete _
    | DErrorRail _ -> false
