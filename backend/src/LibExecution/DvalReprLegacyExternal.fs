/// Ways of converting Dvals to/from strings, with external use allowed
module LibExecution.DvalReprLegacyExternal

// The remaining functions in this file are used by the HttpClient or the Http
// framework, and cannot be changed in their existing usages. The plan to
// remove them is to replace both the Http framework with a middleware based
// design, with some of that middleware being a typed json serializer. The
// typed json serializer would be a standard library function, not something
// that's an inate part of the framework/client in the way that these functions
// are.

// Printing Dvals is more complicated than you'd expect. Different situations
// have different constraints, such as develop-focused representation showing
// explicitly what the value is, vs an API-based representation which does
// something clever with Option/Result types. There is also versioning, as not
// all changes are going to be backward compatible.

// Note: we inline a lot of code which could be reused. This is deliberate: it
// allows us reason more easily about what changes are going to be safe. In
// general, we should avoid general purpose or reusable functions in this file.

// Since it's hard to know where this file is used from, do not throw exceptions in
// here.

open Prelude
open VendoredTablecloth

open RuntimeTypes

open System.Text.Json

let jsonWriterOptions : JsonWriterOptions =
  let mutable options = new JsonWriterOptions()
  options.Indented <- true
  options.SkipValidation <- true
  let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
  options.Encoder <- encoder
  options

let writePrettyJson (f : Utf8JsonWriter -> unit) : string =
  let stream = new System.IO.MemoryStream()
  let w = new Utf8JsonWriter(stream, jsonWriterOptions)
  f w
  w.Flush()
  UTF8.ofBytesUnsafe (stream.ToArray())

let jsonDocumentOptions : JsonDocumentOptions =
  let mutable options = new JsonDocumentOptions()
  options.CommentHandling <- JsonCommentHandling.Skip
  options.MaxDepth <- System.Int32.MaxValue // infinite
  options

let parseJson (s : string) : JsonDocument =
  JsonDocument.Parse(s, jsonDocumentOptions)


type Utf8JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEndObject()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEndArray()


let ocamlStringOfFloat (f : float) : string =
  // Backward compatible way to stringify floats.
  // We used OCaml's string_of_float in lots of different places and now we're
  // reliant on it. Ugh.  string_of_float in OCaml is C's sprintf with the
  // format "%.12g".
  // https://github.com/ocaml/ocaml/blob/4.07/stdlib/stdlib.ml#L274

  // CLEANUP We should move on to a nicer format. See DvalRepr.Tests for edge cases. See:
  if System.Double.IsPositiveInfinity f then
    "inf"
  else if System.Double.IsNegativeInfinity f then
    "-inf"
  else if System.Double.IsNaN f then
    "nan"
  else
    let result = sprintf "%.12g" f
    if result.Contains "." then result else $"{result}."


// -------------------------
// Runtime Types
// -------------------------

// SERIALIZER_DEF DvalReprLegacyExternal.toEnduserReadableTextV0
// Plan: We'd like to deprecate this in favor of an improved version only
// usable/used by StdLib functions in various http clients and middlewares.
/// When printing to grand-users (our users' users) using text/plain, print a
/// human-readable format. Redacts passwords.
let toEnduserReadableTextV0 (dval : Dval) : string =

  let rec nestedreprfn dv =
    // If nesting inside an object or a list, wrap strings in quotes
    match dv with
    | DStr _
    | DUuid _
    | DChar _ -> "\"" + reprfn dv + "\""
    | _ -> reprfn dv

  and toNestedString (dv : Dval) : string =
    let rec inner (indent : int) (dv : Dval) : string =
      let nl = "\n" + String.replicate indent " "
      let inl = "\n" + String.replicate (indent + 2) " "
      let indent = indent + 2
      let recurse = inner indent

      match dv with
      | DTuple (first, second, rest) ->
        let l = [ first; second ] @ rest
        "(" + inl + String.concat ", " (List.map recurse l) + nl + ")"
      | DList l ->
        if l = [] then
          "[]"
        // CLEANUP no good reason to have the space before the newline
        else
          // CLEANUP no good reason to have the space before the newline
          "[ " + inl + String.concat ", " (List.map recurse l) + nl + "]"
      | DObj o ->
        if o = Map.empty then
          "{}"
        else
          let strs =
            Map.fold [] (fun l key value -> (key + ": " + recurse value) :: l) o

          // CLEANUP no good reason to have the space before the newline
          "{ " + inl + String.concat ("," + inl) strs + nl + "}"
      | _ -> nestedreprfn dv

    inner 0 dv

  and reprfn dv =
    match dv with
    | DInt i -> string i
    | DBool true -> "true"
    | DBool false -> "false"
    | DStr s -> s
    | DFloat f -> ocamlStringOfFloat f

    | DChar c -> c
    | DNull -> "null"
    | DDate d -> DDateTime.toIsoString d
    | DUuid uuid -> string uuid
    | DDB dbname -> $"<DB: {dbname}>"
    | DError _ -> "Error"
    | DIncomplete _ -> "<Incomplete>"
    | DFnVal _ ->
      // See docs/dblock-serialization.ml
      "<Block>"
    | DPassword _ ->
      // redacting, do not unredact
      "<Password>"
    | DObj _
    | DList _
    | DTuple _ -> toNestedString dv
    | DErrorRail d ->
      // We don't print error here, because the errorrail value will know
      // whether it's an error or not.
      reprfn d
    | DHttpResponse (Redirect url) -> $"302 {url}\n" + nestedreprfn DNull
    | DHttpResponse (Response (code, headers, body)) ->
      let headerString =
        headers
        |> List.map (fun (k, v) -> k + ": " + v)
        |> String.concat ","
        |> fun s -> "{ " + s + " }"

      $"{code} {headerString}" + "\n" + nestedreprfn body
    | DResult (Ok d) -> reprfn d
    | DResult (Error d) -> "Error: " + reprfn d
    | DOption (Some d) -> reprfn d
    | DOption None -> "Nothing"
    | DBytes bytes -> System.Text.Encoding.UTF8.GetString bytes

  reprfn dval

// SERIALIZER_DEF STJ DvalReprLegacyExternal.toPrettyMachineJsonV1
// Plan: make this a standard library function; use that within current usages
// TODO: revise commentary here - may be inaccurate.
/// For passing to Dark functions that operate on JSON, such as the JWT fns.
/// This turns Option and Result into plain values, or null/error. String-like
/// values are rendered as string. Redacts passwords.
let rec private toPrettyMachineJsonV1 (w : Utf8JsonWriter) (dv : Dval) : unit =
  let writeDval = toPrettyMachineJsonV1 w

  let writeOCamlFloatValue (f : float) =
    if System.Double.IsPositiveInfinity f then
      w.WriteStringValue("Infinity")
    else if System.Double.IsNegativeInfinity f then
      w.WriteStringValue("-Infinity")
    else if System.Double.IsNaN f then
      w.WriteStringValue("NaN")
    else if f = 0.0 && System.Double.IsNegative f then
      // check for -0.0
      w.WriteRawValue("-0.0", true)
    else if System.Math.Abs(f % 1.0) <= System.Double.Epsilon then
      // Check for int-valued floats. By default, STJ prints them with no decimal
      let asString = string f
      if asString.Contains('.') || asString.Contains('E') then // Don't add .0 at the end of 4.2E18
        w.WriteRawValue(String.toLowercase asString, true)
      else
        w.WriteRawValue($"{asString}.0", true)
    else
      let v = f |> string |> String.toLowercase
      w.WriteRawValue(v)

  match dv with
  // basic types
  | DInt i -> w.WriteRawValue(string i)
  | DFloat f -> writeOCamlFloatValue f
  | DBool b -> w.WriteBooleanValue b
  | DNull -> w.WriteNullValue()
  | DStr s -> w.WriteStringValue s
  | DList l -> w.writeArray (fun () -> List.iter writeDval l)
  | DTuple (first, second, rest) ->
    w.writeArray (fun () -> List.iter writeDval ([ first; second ] @ rest))
  | DObj o ->
    w.writeObject (fun () ->
      Map.iter
        (fun k v ->
          w.WritePropertyName k
          writeDval v)
        o)
  | DFnVal _ ->
    // See docs/dblock-serialization.ml
    w.WriteNullValue()
  | DIncomplete _ -> w.WriteNullValue()
  | DChar c -> w.WriteStringValue c
  | DError _ -> w.writeObject (fun () -> w.WriteNull "Error")
  | DHttpResponse (Redirect _) -> writeDval DNull
  | DHttpResponse (Response (_, _, response)) -> writeDval response
  | DDB dbName -> w.WriteStringValue dbName
  | DDate date -> w.WriteStringValue(DDateTime.toIsoString date)
  | DPassword _ ->
    w.writeObject (fun () -> w.WriteString("Error", "Password is redacted"))
  | DUuid uuid -> w.WriteStringValue uuid
  | DOption opt ->
    match opt with
    | None -> w.WriteNullValue()
    | Some v -> writeDval v
  | DErrorRail dv -> writeDval dv
  | DResult res ->
    (match res with
     | Ok dv -> writeDval dv
     | Error dv ->
       w.writeObject (fun () ->
         w.WritePropertyName "Error"
         writeDval dv))
  | DBytes bytes ->
    // CLEANUP: rather than using a mutable byte array, should this be a readonly span?
    w.WriteStringValue(Base64.defaultEncodeToString bytes)

/// When sending json back to the user, or via a HTTP API, attempt to convert
/// everything into reasonable json, in the absence of a schema. This turns
/// Option and Result into plain values, or null/error. String-like values are
/// rendered as string. Redacts passwords.
let toPrettyMachineJsonStringV1 (dval : Dval) : string =
  writePrettyJson (fun w -> toPrettyMachineJsonV1 w dval)


// -------------------------
// Other formats
// -------------------------
open STJParser

// SERIALIZER_DEF DvalReprLegacyExternal.unsafeOfUnknownJsonV0
// Plan: we want to replace this with type-based deserializers.
// TODO: revise commentary here - may be inaccurate.
// When receiving unknown json from the user, or via a HTTP API, attempt to
// convert everything into reasonable types, in the absense of a schema.
// This does type conversion, which it shouldn't and should be avoided for new code.
// Raises CodeException as nearly all callers are in code
let unsafeOfUnknownJsonV0 str : Dval =
  // This special format was originally the default OCaml (yojson-derived) format
  // for this.
  let responseOfJson (dv : Dval) (j : JsonElement) : DHTTP =
    match j with
    | JList [ JString "Redirect"; JString url ] -> Redirect url
    | JList [ JString "Response"; JInteger code; JList headers ] ->
      let headers =
        headers
        |> List.map (fun header ->
          match header with
          | JList [ JString k; JString v ] -> (k, v)
          | h ->
            Exception.raiseInternal "Invalid DHttpResponse headers" [ "headers", h ])

      Response(code, headers, dv)
    | _ -> Exception.raiseInternal "Invalid response json" [ "json", j ]


  let rec convert json =
    match json with
    | JInteger i -> DInt i
    | JFloat f -> DFloat f
    | JBoolean b -> DBool b
    | JNull -> DNull
    | JString s -> DStr s
    | JList l ->
      // We shouldnt have saved dlist that have incompletes or error rails but we might have
      l |> List.map convert |> Dval.list

    | JObject fields ->
      let fields = fields |> List.sortBy (fun (k, _) -> k)
      // These are the only types that are allowed in the queryable
      // representation. We may allow more in the future, but the real thing to
      // do is to use the DB's type and version to encode/decode them correctly
      match fields with
      // DResp (Result.ok_or_Exception.raiseInternal (dhttp_of_yojson a), unsafe_dval_of_yojson_v0 b)
      | [ ("type", JString "response"); ("value", JList [ a; b ]) ] ->
        DHttpResponse(responseOfJson (convert b) a)
      | [ ("type", JString "date"); ("value", JString v) ] ->
        DDate(NodaTime.Instant.ofIsoString v |> DDateTime.fromInstant)
      | [ ("type", JString "password"); ("value", JString v) ] ->
        v |> Base64.fromDefaultEncoded |> Base64.decode |> Password |> DPassword
      | [ ("type", JString "error"); ("value", JString v) ] -> DError(SourceNone, v)
      | [ ("type", JString "bytes"); ("value", JString v) ] ->
        // Note that the OCaml version uses the non-url-safe b64 encoding here
        v |> System.Convert.FromBase64String |> DBytes
      | [ ("type", JString "char"); ("value", JString v) ] -> DChar v
      | [ ("type", JString "character"); ("value", JString v) ] -> DChar v
      | [ ("type", JString "datastore"); ("value", JString v) ] -> DDB v
      | [ ("type", JString "incomplete"); ("value", JNull) ] ->
        DIncomplete SourceNone
      | [ ("type", JString "errorrail"); ("value", dv) ] -> DErrorRail(convert dv)
      | [ ("type", JString "option"); ("value", JNull) ] -> DOption None
      | [ ("type", JString "option"); ("value", dv) ] -> DOption(Some(convert dv))
      | [ ("type", JString "block"); ("value", JNull) ] ->
        // See docs/dblock-serialization.ml
        DFnVal(
          Lambda { body = EBlank(id 56789); symtable = Map.empty; parameters = [] }
        )
      | [ ("type", JString "uuid"); ("value", JString v) ] -> DUuid(System.Guid v)
      | [ ("constructor", JString "Ok")
          ("type", JString "result")
          ("values", JList [ dv ]) ] -> DResult(Ok(convert dv))
      | [ ("constructor", JString "Error")
          ("type", JString "result")
          ("values", JList [ dv ]) ] -> DResult(Error(convert dv))
      | _ -> fields |> List.map (fun (k, v) -> (k, convert v)) |> Map.ofList |> DObj
    | JUndefined
    | _ -> Exception.raiseInternal "Invalid type in json" [ "json", json ]

  try
    use document = parseJson str
    convert document.RootElement
  with
  | _ -> Exception.raiseGrandUser "Invalid json"

// SERIALIZER_DEF STJ DvalReprLegacyExternal.ofUnknownJsonV1
// Plan: add a Json::parse that can takes a type parameter; deprecate this fn
/// When receiving unknown json from the user, or via a HTTP API, attempt to
/// convert everything into reasonable types, in the absense of a schema.
let ofUnknownJsonV1 str : Result<Dval, string> =
  let rec convert json =
    match json with
    | JInteger i -> DInt i
    | JFloat f -> DFloat f
    | JBoolean b -> DBool b
    | JNull -> DNull
    | JString s -> DStr s
    | JList l -> l |> List.map convert |> Dval.list
    | JObject fields ->
      fields |> List.map (fun (k, v) -> k, (convert v)) |> Map |> DObj
    | JUndefined
    | _ -> Exception.raiseInternal "Invalid type in json" [ "json", json ]

  try
    use document = str |> parseJson
    document.RootElement |> convert |> Ok
  with
  | :? JsonException as e ->
    let msg =
      if str = "" then
        "JSON string was empty"
      else
        let msg = e.Message
        // The full message has .NET specific advice, so just stick to the good stuff
        let trailingCommaMsg = "The JSON array contains a trailing comma at the end"
        if msg.Contains trailingCommaMsg then
          $"{trailingCommaMsg}, at on line {e.LineNumber}, position {e.BytePositionInLine}"
        else
          msg
    Error msg
  | e -> Error e.Message
