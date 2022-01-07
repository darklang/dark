module LibExecution.DvalReprExternal

// Printing Dvals is more complicated than you'd expect. Different situations
// have different constaints, such as develop-focused representation showing
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

// FSTODO: move everything in this file into where it's used

open System.Text.Json

// TODO CLEANUP - remove all the unsafeDval by inlining them into the named
// functions that use them, such as toQueryable or toRoundtrippable

let writeJson (f : Utf8JsonWriter -> unit) : string =
  let stream = new System.IO.MemoryStream()
  let mutable options = new JsonWriterOptions()
  options.SkipValidation <- true
  let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
  options.Encoder <- encoder
  let w = new Utf8JsonWriter(stream, options)
  f w
  w.Flush()
  UTF8.ofBytesUnsafe (stream.ToArray())

let writePrettyJson (f : Utf8JsonWriter -> unit) : string =
  let stream = new System.IO.MemoryStream()
  let mutable options = new JsonWriterOptions()
  options.Indented <- true
  options.SkipValidation <- true
  let encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
  options.Encoder <- encoder
  let w = new Utf8JsonWriter(stream, options)
  f w
  w.Flush()
  UTF8.ofBytesUnsafe (stream.ToArray())


let parseJson (s : string) : JsonDocument = JsonDocument.Parse(s)


type Utf8JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEndObject()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEndArray()

  member this.writeOCamlFloatValue(f : float) =
    if System.Double.IsPositiveInfinity f then
      this.WriteRawValue("Infinity", true)
    else if System.Double.IsNegativeInfinity f then
      this.WriteRawValue("-Infinity", true)
    else if System.Double.IsNaN f then
      this.WriteRawValue("NaN", true)
    else if f = 0.0 && System.Double.IsNegative f then
      // check for -0.0
      this.WriteRawValue("-0.0", true)
    else if System.Math.Abs(f % 1.0) <= System.Double.Epsilon then
      // Check for int-valued floats. By default, STJ prints them with no decimal
      let asString = string f
      if asString.Contains('.') || asString.Contains('E') then // Don't add .0 at the end of 4.2E18
        this.WriteRawValue(String.toLowercase asString, true)
      else
        this.WriteRawValue($"{asString}.0", true)
    else
      let v = f |> string |> String.toLowercase
      this.WriteRawValue(v)
  member this.writeFullInt64Value(i : int64) = this.WriteRawValue(string i)


let (|JString|_|) (j : JsonElement) : Option<string> =
  match j.ValueKind with
  | JsonValueKind.String -> Some(JString(j.GetString()))
  | _ -> None

let (|JNull|_|) (j : JsonElement) : Option<unit> =
  match j.ValueKind with
  | JsonValueKind.Null -> Some(JNull)
  | _ -> None

let (|JInteger|_|) (j : JsonElement) : Option<int64> =
  match j.ValueKind with
  | JsonValueKind.Number ->
    try
      Some(JInteger(j.GetInt64()))
    with
    | :? System.FormatException -> None
  | _ -> None

let (|JFloat|_|) (j : JsonElement) : Option<float> =
  match j.ValueKind with
  | JsonValueKind.Number -> Some(JFloat(j.GetDouble()))
  | _ -> None

let (|JBoolean|_|) (j : JsonElement) : Option<bool> =
  match j.ValueKind with
  | JsonValueKind.False -> Some(JBoolean(false))
  | JsonValueKind.True -> Some(JBoolean(true))
  | _ -> None

let (|JList|_|) (j : JsonElement) : Option<List<JsonElement>> =
  match j.ValueKind with
  | JsonValueKind.Array -> Some(JList(j.EnumerateArray() |> Seq.toList))
  | _ -> None

let (|JObject|_|) (j : JsonElement) : Option<List<string * JsonElement>> =
  match j.ValueKind with
  | JsonValueKind.Object ->
    let list =
      j.EnumerateObject()
      |> Seq.toList
      |> List.map (fun (jp : JsonProperty) -> (jp.Name, jp.Value))
    Some(JObject(list))

  | _ -> None


let (|JNonStandard|_|) (j : JsonElement) : Option<unit> =
  match j.ValueKind with
  | JsonValueKind.Undefined -> Some()
  | _ -> None

let ocamlStringOfFloat (f : float) : string =
  // We used OCaml's string_of_float in lots of different places and now we're
  // reliant on it. Ugh.  string_of_float in OCaml is C's sprintf with the
  // format "%.12g".
  // https://github.com/ocaml/ocaml/blob/4.07/stdlib/stdlib.ml#L274

  // CLEANUP We should move on to a nicer format. See DvalReprExternal.tests for edge cases. See:
  if System.Double.IsPositiveInfinity f then
    "inf"
  else if System.Double.IsNegativeInfinity f then
    "-inf"
  else if System.Double.IsNaN f then
    "nan"
  else
    let result = sprintf "%.12g" f
    if result.Contains "." then result else $"{result}."


let ocamlBytesToString (bytes : byte []) =
  // CLEANUP: dumping these as ASCII isn't a great look
  System.Text.Encoding.UTF8.GetString bytes

// -------------------------
// Runtime Types
// -------------------------


// As of Wed Apr 21, 2021, this fn is only used for things that are shown to
// developers, and not for storage or any other thing that needs to be kept
// backwards-compatible.
// CLEANUP: once we no longer support compatibility with OCaml, these messages can get much better.
let rec typeToDeveloperReprV0 (t : DType) : string =
  match t with
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TNull -> "Null"
  | TChar -> "Character"
  | TStr -> "String"
  | TList _ -> "List"
  | TDict _ -> "Dict"
  | TRecord _ -> "Dict"
  | TFn _ -> "Block"
  | TVariable varname -> "Any"
  | TIncomplete -> "Incomplete"
  | TError -> "Error"
  | THttpResponse _ -> "Response"
  | TDB _ -> "Datastore"
  | TDate -> "Date"
  | TPassword -> "Password"
  | TUuid -> "UUID"
  | TOption _ -> "Option"
  | TErrorRail -> "ErrorRail"
  | TResult _ -> "Result"
  | TUserType (name, _) -> name
  | TBytes -> "Bytes"

let prettyTypename (dv : Dval) : string = dv |> Dval.toType |> typeToDeveloperReprV0

// Backwards compatible version of `typeToDeveloperRepr`, should not be visible to
// users (except through LibDarkInternal) but used by things like HttpClient
// (transitively)
let rec typeToBCTypeName (t : DType) : string =
  match t with
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TNull -> "null"
  | TChar -> "character"
  | TStr -> "string"
  | TList _ -> "list"
  | TDict _ -> "dict"
  | TRecord _ -> "dict"
  | TFn _ -> "block"
  | TVariable varname -> "any"
  | TIncomplete -> "incomplete"
  | TError -> "error"
  | THttpResponse _ -> "response"
  | TDB _ -> "datastore"
  | TDate -> "date"
  | TPassword -> "password"
  | TUuid -> "uuid"
  | TOption _ -> "option"
  | TErrorRail -> "errorrail"
  | TResult _ -> "result"
  | TUserType (name, _) -> String.toLowercase name
  | TBytes -> "bytes"

let rec toNestedString (reprfn : Dval -> string) (dv : Dval) : string =
  let rec inner (indent : int) (dv : Dval) : string =
    let nl = "\n" + String.replicate indent " "
    let inl = "\n" + String.replicate (indent + 2) " "
    let indent = indent + 2
    let recurse = inner indent

    match dv with
    | DList l ->
      if l = [] then
        "[]"
      else
        "[ " + inl + String.concat ", " (List.map recurse l) + nl + "]"
    | DObj o ->
      if o = Map.empty then
        "{}"
      else
        let strs =
          Map.fold [] (fun l key value -> (key + ": " + recurse value) :: l) o

        // CLEANUP no good reason to have the space before the newline
        "{ " + inl + String.concat ("," + inl) strs + nl + "}"
    | _ -> reprfn dv

  inner 0 dv

// When printing to grand-users (our users' users) using text/plain, print a
// human-readable format. TODO: this should probably be part of the functions
// generating the responses. Redacts passwords.

let toEnduserReadableTextV0 (dval : Dval) : string =
  let rec nestedreprfn dv =
    (* If nesting inside an object or a list, wrap strings in quotes *)
    match dv with
    | DStr _
    | DUuid _
    | DChar _ -> "\"" + reprfn dv + "\""
    | _ -> reprfn dv

  and reprfn dv =
    match dv with
    | DInt i -> string i
    | DBool true -> "true"
    | DBool false -> "false"
    | DStr s -> s
    | DFloat f -> ocamlStringOfFloat f

    | DChar c -> c
    | DNull -> "null"
    | DDate d -> d.toIsoString ()
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
    | DList _ -> toNestedString nestedreprfn dv
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
    | DBytes bytes -> ocamlBytesToString bytes

  reprfn dval

// For passing to Dark functions that operate on JSON, such as the JWT fns.
// This turns Option and Result into plain values, or null/error. String-like
// values are rendered as string. Redacts passwords.
let rec toPrettyMachineJsonV1 (w : Utf8JsonWriter) (dv : Dval) : unit =
  let writeDval = toPrettyMachineJsonV1 w
  // utf8jsonwriter has different methods for writing into objects vs arrays.
  // Dark doesn't assume the outermost value is an array or object, so try
  // writing the array version to start, then recurse between the two as
  // appropriate based on the content.
  match dv with
  // basic types
  | DInt i -> w.writeFullInt64Value i
  | DFloat f -> w.writeOCamlFloatValue f
  | DBool b -> w.WriteBooleanValue b
  | DNull -> w.WriteNullValue()
  | DStr s -> w.WriteStringValue s
  | DList l -> w.writeArray (fun () -> List.iter writeDval l)
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
  | DDate date -> w.WriteStringValue(date.toIsoString ())
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
    w.WriteStringValue(System.Convert.ToBase64String bytes)

// When sending json back to the user, or via a HTTP API, attempt to convert
// everything into reasonable json, in the absence of a schema. This turns
// Option and Result into plain values, or null/error. String-like values are
// rendered as string. Redacts passwords.
let toPrettyMachineJsonStringV1 (dval : Dval) : string =
  writePrettyJson (fun w -> toPrettyMachineJsonV1 w dval)


// -------------------------
// Other formats
// -------------------------

// For printing something for the developer to read, as a live-value, error
// message, etc. This will faithfully represent the code, textually. Redacts
// passwords. Customers should not come to rely on this format.
let rec toDeveloperReprV0 (dv : Dval) : string =
  let rec toRepr_ (indent : int) (dv : Dval) : string =
    let makeSpaces len = "".PadRight(len, ' ')
    let nl = "\n" + makeSpaces indent
    let inl = "\n" + makeSpaces (indent + 2)
    let indent = indent + 2
    let typename = prettyTypename dv
    let wrap str = $"<{typename}: {str}>"
    let justtipe = $"<{typename}>"

    match dv with
    | DPassword _ -> "<password>"
    | DStr s -> $"\"{s}\""
    | DChar c -> $"'{c}'"
    | DInt i -> string i
    | DBool true -> "true"
    | DBool false -> "false"
    | DFloat f -> ocamlStringOfFloat f
    | DNull -> "null"
    | DFnVal _ ->
      (* See docs/dblock-serialization.ml *)
      justtipe
    | DIncomplete _ -> justtipe
    | DError _ -> "<error>"
    | DDate d -> wrap (d.toIsoString ())
    | DDB name -> wrap name
    | DUuid uuid -> wrap (string uuid)
    | DHttpResponse h ->
      match h with
      | Redirect url -> $"302 {url}" + nl + toRepr_ indent DNull
      | Response (code, headers, hdv) ->
        let headerString =
          headers
          |> List.map (fun (k, v) -> k + ": " + v)
          |> String.concat ","
          |> fun s -> "{ " + s + " }"

        $"{code} {headerString}" + nl + toRepr_ indent hdv
    | DList l ->
      if List.isEmpty l then
        "[]"
      else
        let elems = String.concat ", " (List.map (toRepr_ indent) l)
        // CLEANUP: this space makes no sense
        $"[ {inl}{elems}{nl}]"
    | DObj o ->
      if Map.isEmpty o then
        "{}"
      else
        let strs =
          Map.fold [] (fun l key value -> ($"{key}: {toRepr_ indent value}") :: l) o

        let elems = String.concat $",{inl}" strs
        // CLEANUP: this space makes no sense
        "{ " + $"{inl}{elems}{nl}" + "}"
    | DOption None -> "Nothing"
    | DOption (Some dv) -> "Just " + toRepr_ indent dv
    | DResult (Ok dv) -> "Ok " + toRepr_ indent dv
    | DResult (Error dv) -> "Error " + toRepr_ indent dv
    | DErrorRail dv -> "ErrorRail: " + toRepr_ indent dv
    | DBytes bytes -> bytes |> System.Convert.ToBase64String

  toRepr_ 0 dv


// When receiving unknown json from the user, or via a HTTP API, attempt to
// convert everything into reasonable types, in the absense of a schema.
// This does type conversion, which it shouldn't and should be avoided for new code.
let unsafeOfUnknownJsonV0 str =
  // This special format was originally the default OCaml (yojson-derived) format
  // for this.
  let responseOfJson (dv : Dval) (j : JsonElement) : DHTTP =
    match j with
    | JList [ JString "Redirect"; JString url ] -> Redirect url
    | JList [ JString "Response"; JInteger code; JList headers ] ->
      let headers =
        headers
        |> List.map (function
          | JList [ JString k; JString v ] -> (k, v)
          | h ->
            Exception.raiseInternal "Invalid DHttpResponse headers" [ "header", h ])

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
        DDate(System.DateTime.ofIsoString v)
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
    // Json.NET does a bunch of magic based on the contents of various types.
    // For example, it has tokens for Dates, constructors, etc. We've tried to
    // disable all those so we fail if we see them. However, we might need to
    // just convert some of these into strings.
    | JNonStandard
    | _ -> Exception.raiseInternal "Invalid type in json" [ "json", json ]

  try
    use document = parseJson str
    convert document.RootElement
  with
  | _ -> Exception.raiseInternal "Invalid json" [ "json", str ]


// When receiving unknown json from the user, or via a HTTP API, attempt to
// convert everything into reasonable types, in the absense of a schema.
let ofUnknownJsonV1 str : Result<Dval, string> =
  // FSTODO: there doesn't seem to be a good reason that we use JSON.NET here,
  // might be better to switch to STJ
  let rec convert json =
    match json with
    | JInteger i -> DInt i
    | JFloat f -> DFloat f
    | JBoolean b -> DBool b
    | JNull -> DNull
    | JString s -> DStr s
    | JList l -> l |> List.map convert |> Dval.list
    | JObject fields ->
      fields |> List.fold Map.empty (fun m (k, v) -> Map.add k (convert v) m) |> DObj

    // Json.NET does a bunch of magic based on the contents of various types.
    // For example, it has tokens for Dates, constructors, etc. We've tried to
    // disable all those so we fail if we see them. However, we might need to
    // just convert some of these into strings.
    | JNonStandard
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


// Converts an object to (string,string) pairs. Raises an exception if not an object
let toStringPairs (dv : Dval) : Result<List<string * string>, string> =
  match dv with
  | DObj obj ->
    obj
    |> Map.toList
    |> List.map (function
      | (k, DStr v) -> Ok(k, v)
      | (k, v) ->
        // CLEANUP: this is just to keep the error messages the same with OCaml. It's safe to change the error message
        // Error $"Expected a string, but got: {toDeveloperReprV0 v}"
        Error "expecting str")
    |> Tablecloth.Result.values
  | _ ->
    // CLEANUP As above
    // $"Expected a string, but got: {toDeveloperReprV0 dv}"
    Error "expecting str"

// -------------------------
// URLs and queryStrings
// -------------------------

// For putting into URLs as query params
let rec toUrlString (dv : Dval) : string =
  let r = toUrlString
  match dv with
  | DFnVal _ ->
    (* See docs/dblock-serialization.ml *)
    "<block>"
  | DIncomplete _ -> "<incomplete>"
  | DPassword _ -> "<password>"
  | DInt i -> string i
  | DBool true -> "true"
  | DBool false -> "false"
  | DStr s -> s
  | DFloat f -> ocamlStringOfFloat f
  | DChar c -> c
  | DNull -> "null"
  | DDate d -> d.toIsoString ()
  | DDB dbname -> dbname
  | DErrorRail d -> r d
  | DError _ -> "error="
  | DUuid uuid -> string uuid
  | DHttpResponse (Redirect _) -> "null"
  | DHttpResponse (Response (_, _, hdv)) -> r hdv
  | DList l -> "[ " + String.concat ", " (List.map r l) + " ]"
  | DObj o ->
    let strs = Map.fold [] (fun l key value -> (key + ": " + r value) :: l) o
    "{ " + (String.concat ", " strs) + " }"
  | DOption None -> "none"
  | DOption (Some v) -> r v
  | DResult (Error v) -> "error=" + r v
  | DResult (Ok v) -> r v
  | DBytes bytes -> Base64.defaultEncodeToString bytes

// Convert strings into queryParams. This matches the OCaml Uri.query function. Note that keys and values use slightly different encodings
let queryToEncodedString (queryParams : (List<string * List<string>>)) : string =
  match queryParams with
  | [ key, [] ] -> urlEncodeKey key
  | _ ->
    queryParams
    |> List.map (fun (k, vs) ->
      let k = k |> urlEncodeKey
      vs
      |> List.map urlEncodeValue
      |> fun vs ->
           if vs = [] then
             k
           else
             let vs = String.concat "," vs
             $"{k}={vs}")
    |> String.concat "&"

let toQuery (dv : Dval) : Result<List<string * List<string>>, string> =
  match dv with
  | DObj kvs ->
    kvs
    |> Map.toList
    |> List.map (fun (k, value) ->
      match value with
      | DNull -> Ok(k, [])
      | DList l -> Ok(k, List.map toUrlString l)
      | _ -> Ok(k, [ toUrlString value ]))
    |> Tablecloth.Result.values
  | _ -> Error "attempting to use non-object as query param" // CODE exception


// The queryString passed in should not include the leading '?' from the URL
let parseQueryString (queryString : string) : List<string * List<string>> =
  // This will eat any intended question mark, so add one
  let nvc = System.Web.HttpUtility.ParseQueryString("?" + queryString)
  nvc.AllKeys
  |> Array.map (fun key ->
    let values = nvc.GetValues key
    let split =
      values[values.Length - 1] |> FSharpPlus.String.split [| "," |] |> Seq.toList

    if isNull key then
      // All the values with no key are by GetValues, so make each one a value
      values |> Array.toList |> List.map (fun k -> (k, []))
    else
      [ (key, split) ])
  |> List.concat

let ofQuery (query : List<string * List<string>>) : Dval =
  query
  |> List.map (fun (k, v) ->
    match v with
    | [] -> k, DNull
    | [ "" ] -> k, DNull // CLEANUP this should be a string
    | [ v ] -> k, DStr v
    | list -> k, DList(List.map DStr list))
  |> Map
  |> DObj


let ofQueryString (queryString : string) : Dval =
  queryString |> parseQueryString |> ofQuery

// -------------------------
// Forms
// -------------------------

let toFormEncoding (dv : Dval) : Result<string, string> =
  toQuery dv |> Result.map queryToEncodedString

let ofFormEncoding (f : string) : Dval = f |> parseQueryString |> ofQuery
