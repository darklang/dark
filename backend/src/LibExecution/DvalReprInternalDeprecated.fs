/// <summary>
/// Ways of converting Dvals to/from strings, to be used exclusively internally.
///
/// That is, they should not be used in libraries, in the BwdServer, in HttpClient,
/// etc.
/// </summary>
/// <remarks>
/// We're trying to get rid of JSON.NET. However, these format have saved millions
/// of values using them, so we need to do a migration from the old serialization
/// to a new one.
/// If possible, migrate from serializers in here to serializers in DvalReprInternalDeprecatedNew.
/// </remarks>
module LibExecution.DvalReprInternalDeprecated

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




// TODO CLEANUP - remove all the unsafeDval by inlining them into the named
// functions that use them, such as toQueryable or toRoundtrippable


let parseJson (s : string) : JToken =
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

let ocamlStringOfFloat (f : float) : string =
  // We used OCaml's string_of_float in lots of different places and now we're
  // reliant on it. Ugh.  string_of_float in OCaml is C's sprintf with the
  // format "%.12g".
  // https://github.com/ocaml/ocaml/blob/4.07/stdlib/stdlib.ml#L274

  // CLEANUP We should move on to a nicer format. See DvalReprLegacyExternal.tests for edge cases. See:
  if System.Double.IsPositiveInfinity f then
    "inf"
  else if System.Double.IsNegativeInfinity f then
    "-inf"
  else if System.Double.IsNaN f then
    "nan"
  else
    let result = sprintf "%.12g" f
    if result.Contains "." then result else $"{result}."



let rec private unsafeDvalToJsonValueV0 (w : JsonWriter) (dv : Dval) : unit =
  let writeDval = unsafeDvalToJsonValueV0 w

  let wrapStringValue (typ : string) (str : string) =
    w.writeObject (fun () ->
      w.WritePropertyName "type"
      w.WriteValue(typ)
      w.WritePropertyName "value"
      w.WriteValue(str))

  let wrapNullValue (typ : string) =
    w.writeObject (fun () ->
      w.WritePropertyName "type"
      w.WriteValue(typ)
      w.WritePropertyName "value"
      w.WriteNull())

  let wrapNestedDvalValue (typ : string) (dv : Dval) =
    w.writeObject (fun () ->
      w.WritePropertyName "type"
      w.WriteValue(typ)
      w.WritePropertyName "value"
      writeDval dv)

  match dv with
  // basic types
  | DInt i -> w.WriteValue i
  | DFloat f -> w.WriteValue f
  | DBool b -> w.WriteValue b
  | DNull -> w.WriteNull()
  | DStr s -> w.WriteValue s
  | DList l -> w.writeArray (fun () -> List.iter writeDval l)
  | DTuple (first, second, theRest) ->
    w.writeObject (fun () ->
      w.WritePropertyName "type"
      w.WriteValue("tuple")

      w.WritePropertyName "first"
      writeDval first

      w.WritePropertyName "second"
      writeDval second

      w.WritePropertyName "theRest"
      w.writeArray (fun () -> List.iter writeDval theRest))

  | DObj o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          writeDval v)
        o)
  | DFnVal _ ->
    // See docs/dblock-serialization.md
    wrapNullValue "block"
  | DIncomplete _ -> wrapNullValue "incomplete"
  | DChar c -> wrapStringValue "character" c
  | DError (_, msg) ->
    // Only used internally, so this is safe to save here
    wrapStringValue "error" msg
  | DHttpResponse (h) ->
    w.writeObject (fun () ->
      w.WritePropertyName "type"
      w.WriteValue "response"
      w.WritePropertyName "value"
      w.writeArray (fun () ->
        match h with
        | Redirect str ->
          w.writeArray (fun () ->
            w.WriteValue "Redirect"
            w.WriteValue str)

          writeDval DNull
        | Response (code, headers, hdv) ->
          w.writeArray (fun () ->
            w.WriteValue "Response"
            w.WriteValue code

            w.writeArray (fun () ->
              List.iter
                (fun (k : string, v : string) ->
                  w.writeArray (fun () ->
                    w.WriteValue k
                    w.WriteValue v))
                headers))

          writeDval hdv))
  | DDB dbname -> wrapStringValue "datastore" dbname
  | DDate date -> wrapStringValue "date" (DDateTime.toIsoString date)
  | DPassword (Password hashed) ->
    hashed |> Base64.defaultEncodeToString |> wrapStringValue "password"
  | DUuid uuid -> wrapStringValue "uuid" (string uuid)
  | DOption opt ->
    (match opt with
     | None -> wrapNullValue "option"
     | Some ndv -> wrapNestedDvalValue "option" ndv)
  | DErrorRail erdv -> wrapNestedDvalValue "errorrail" erdv
  | DResult res ->
    (match res with
     | Ok rdv ->
       w.writeObject (fun () ->
         w.WritePropertyName "type"
         w.WriteValue("result")
         w.WritePropertyName "constructor"
         w.WriteValue("Ok")
         w.WritePropertyName "values"
         w.writeArray (fun () -> writeDval rdv))
     | Error rdv ->
       w.writeObject (fun () ->
         w.WritePropertyName "type"
         w.WriteValue("result")
         w.WritePropertyName "constructor"
         w.WriteValue("Error")
         w.WritePropertyName "values"
         w.writeArray (fun () -> writeDval rdv)))
  | DBytes bytes ->
    // Note that the OCaml version uses the non-url-safe b64 encoding here
    bytes |> Base64.defaultEncodeToString |> wrapStringValue "bytes"



// -------------------------
// Queryable - for the DB
// -------------------------

// This is a format used for roundtripping dvals internally, while still being
// queryable using jsonb in our DB. This reduces some of the v0 bugs, but at
// the cost of not supporting many typed that we'll want to put in it.  Also
// roundtrippable. Does not redact.
let toInternalQueryableV1 (dvalMap : DvalMap) : string =
  writeJson (fun w ->
    w.writeObject (fun () ->
      dvalMap
      |> Map.toList
      |> List.iter (fun (k, dval) ->
        w.WritePropertyName k
        unsafeDvalToJsonValueV0 w dval)))

// The only formats allowed in the DB so far:
// String
// Int
// Boolean
// Float
// Password
// Date
// UUID
// Dict
// List

// This is a format used for roundtripping dvals internally, while still being
// queryable using jsonb in our DB. There are some rare cases where it will
// parse incorrectly without error. Throws on Json bugs.
let ofInternalQueryableV1 (str : string) : Dval =
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
    | DStr _ -> true
    | DInt i -> i > -4611686018427387904L && i < 4611686018427387904L
    | DNull _ -> true
    | DBool _ -> true
    | DFloat _ -> true
    | DList dvals -> List.all isQueryableDval dvals
    | DObj map -> map |> Map.values |> List.all isQueryableDval
    | DDate _ -> true
    | DPassword _ -> true
    | DUuid _ -> true
    // TODO support
    | DTuple _ -> false
    | DChar _ -> false
    | DBytes _ -> false
    | DHttpResponse _ -> false
    | DOption _ -> false
    | DResult _ -> false
    // Not supportable I think
    | DDB _ -> false
    | DFnVal _ -> false // not supported
    | DError _ -> false
    | DIncomplete _ -> false
    | DErrorRail _ -> false
