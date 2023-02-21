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


// This special format was originally the default OCaml (yojson-derived) format
// for this.
let responseOfJson (dv : Dval) (j : JToken) : DHTTP =
  match j with
  | JList [ JString "Redirect"; JString url ] -> Redirect url
  | JList [ JString "Response"; JInteger code; JList headers ] ->
    let headers =
      headers
      |> List.map (fun header ->
        match header with
        | JList [ JString k; JString v ] -> (k, v)
        | h ->
          Exception.raiseInternal "Invalid DHttpResponse headers" [ "header", h ])

    Response(code, headers, dv)
  | _ -> Exception.raiseInternal "Invalid response json" [ "json", j ]


// Convert a dval (already converted from json) into
let rec unsafeDvalOfJsonV1 (json : JToken) : Dval =
  let convert = unsafeDvalOfJsonV1

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
    // Note: the fields are ordered.
    match fields with
    // DResp (Result.ok_or_Exception.raiseInternal (dhttp_of_yojson a), unsafe_dval_of_yojson_v0 b)
    | [ ("type", JString "response"); ("value", JList [ a; b ]) ] ->
      DHttpResponse(responseOfJson (convert b) a)
    | [ ("type", JString "date"); ("value", JString v) ] ->
      DDate(NodaTime.Instant.ofIsoString v |> DDateTime.fromInstant)
    | [ ("type", JString "password"); ("value", JString v) ] ->
      v |> Base64.fromEncoded |> Base64.decode |> Password |> DPassword
    | [ ("type", JString "error"); ("value", JString v) ] -> DError(SourceNone, v)
    | [ ("type", JString "bytes"); ("value", JString v) ] ->
      // Note that the OCaml version uses the non-url-safe b64 encoding here
      v |> System.Convert.FromBase64String |> DBytes
    | [ ("type", JString "char"); ("value", JString v) ] ->
      v |> String.toEgcSeq |> Seq.head |> DChar
    | [ ("type", JString "character"); ("value", JString v) ] ->
      v |> String.toEgcSeq |> Seq.head |> DChar
    | [ ("type", JString "datastore"); ("value", JString v) ] -> DDB v
    | [ ("type", JString "incomplete"); ("value", JNull) ] -> DIncomplete SourceNone
    | [ ("type", JString "errorrail"); ("value", dv) ] -> DErrorRail(convert dv)
    | [ ("type", JString "option"); ("value", JNull) ] -> DOption None
    | [ ("type", JString "option"); ("value", dv) ] -> DOption(Some(convert dv))
    | [ ("type", JString "block"); ("value", JNull) ] ->
      // See docs/dblock-serialization.ml
      DFnVal(
        Lambda { body = EBlank(id 23456); symtable = Map.empty; parameters = [] }
      )
    | [ ("type", JString "uuid"); ("value", JString v) ] -> DUuid(System.Guid v)
    | [ ("first", first)
        ("second", second)
        ("theRest", JList theRest)
        ("type", JString "tuple") ] ->
      DTuple(convert first, convert second, theRest |> List.map convert)
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



let private unsafeDvalToJsonValueV1 (w : JsonWriter) (dv : Dval) : unit =
  unsafeDvalToJsonValueV0 w dv

// -------------------------
// Roundtrippable - for events and traces
// -------------------------

// This is a format used for roundtripping dvals internally. v0 has bugs due to
// a legacy of trying to make one function useful for everything. Does not
// redact.
let toInternalRoundtrippableV0 (dval : Dval) : string =
  writeJson (fun w -> unsafeDvalToJsonValueV1 w dval)

// Used for fuzzing and to document what's supported. There are a number of
// known bugs in our roundtripping in OCaml - we actually want to reproduce
// these in the F# implementation to make sure nothing changes. We return false
// if any of these appear unless "allowKnownBuggyValues" is true.
let isRoundtrippableDval (allowKnownBuggyValues : bool) (dval : Dval) : bool =
  match dval with
  | DChar c when c.Length = 1 -> true
  | DChar _ -> false // invalid
  | DStr _ -> true
  | DInt i -> i > -4611686018427387904L && i < 4611686018427387904L
  | DNull _ -> true
  | DBool _ -> true
  | DFloat _ -> true
  | DList ls when not allowKnownBuggyValues ->
    // CLEANUP: Bug where Lists containing fake dvals will be replaced with
    // the fakeval
    not (List.any Dval.isFake ls)
  | DList _ -> true
  | DTuple _ -> true
  | DObj _ -> true
  | DDate _ -> true
  | DPassword _ -> true
  | DUuid _ -> true
  | DBytes _ -> true
  | DHttpResponse _ -> true
  | DOption (Some DNull) when not allowKnownBuggyValues ->
    // CLEANUP: Bug where Lists containing fake dvals will be replaced with
    // the fakeval
    false
  | DOption _ -> true
  | DResult _ -> true
  | DDB _ -> true
  | DError _ -> true
  | DIncomplete _ -> true
  | DErrorRail _ -> true
  | DFnVal _ -> false // not supported

// This is a format used for roundtripping dvals internally. There are some
// rare cases where it will parse incorrectly without error. Throws on Json
// bugs.
let ofInternalRoundtrippableJsonV0 (j : JToken) : Result<Dval, string> =
  (* Switched to v1 cause it was a bug fix *)
  try
    unsafeDvalOfJsonV1 j |> Ok
  with
  | e -> Error(string e)

let ofInternalRoundtrippableV0 (str : string) : Dval =
  // cleanup: we know the types here, so we should probably do type directed parsing and simplify what's stored
  str |> parseJson |> unsafeDvalOfJsonV1

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

// -------------------------
// Hashes
// -------------------------

// This has been used to save millions of values in our DB, so the format isn't
// amenable to change without a migration. Don't change ANYTHING for existing
// values, but continue to add representations for new values. Also, inline
// everything!
let rec toHashableRepr (indent : int) (oldBytes : bool) (dv : Dval) : byte [] =
  let makeSpaces len = "".PadRight(len, ' ')
  let nl = "\n" + makeSpaces indent
  let inl = "\n" + makeSpaces (indent + 2)
  let indent = indent + 2 in

  match dv with
  | DDB dbname -> ("<db: " + dbname + ">") |> UTF8.toBytes
  | DInt i -> string i |> UTF8.toBytes
  | DBool true -> "true" |> UTF8.toBytes
  | DBool false -> "false" |> UTF8.toBytes
  | DFloat f -> ocamlStringOfFloat f |> UTF8.toBytes
  | DNull -> "null" |> UTF8.toBytes
  | DStr s -> "\"" + string s + "\"" |> UTF8.toBytes
  | DChar c -> "'" + string c + "'" |> UTF8.toBytes
  | DIncomplete _ ->
    "<incomplete: <incomplete>>" |> UTF8.toBytes (* Can't be used anyway *)
  | DFnVal _ ->
    (* See docs/dblock-serialization.ml *)
    "<block: <block>>" |> UTF8.toBytes
  | DError _ -> "<error>" |> UTF8.toBytes
  | DDate d -> "<date: " + DDateTime.toIsoString d + ">" |> UTF8.toBytes
  | DPassword _ -> "<password: <password>>" |> UTF8.toBytes
  | DUuid id -> "<uuid: " + string id + ">" |> UTF8.toBytes
  | DHttpResponse d ->
    let formatted, hdv =
      match d with
      | Redirect url -> ("302 " + url, DNull)
      | Response (c, hs, hdv) ->
        let stringOfHeaders hs =
          hs
          |> List.map (fun (k, v) -> k + ": " + v)
          |> String.concat ","
          |> fun s -> "{ " + s + " }"

        (string c + " " + stringOfHeaders hs, hdv)

    [ (formatted + nl) |> UTF8.toBytes; toHashableRepr indent false hdv ]
    |> Array.concat
  | DList l ->
    if List.isEmpty l then
      "[]" |> UTF8.toBytes
    else
      let body =
        l
        |> List.map (toHashableRepr indent false)
        |> List.intersperse (UTF8.toBytes ", ")
        |> Array.concat

      // CLEANUP this space is useless
      Array.concat [ "[ " |> UTF8.toBytes
                     inl |> UTF8.toBytes
                     body
                     nl |> UTF8.toBytes
                     "]" |> UTF8.toBytes ]
  | DTuple (first, second, theRest) ->
    let l = [ first; second ] @ theRest
    let body =
      l
      |> List.map (toHashableRepr indent false)
      |> List.intersperse (UTF8.toBytes ", ")
      |> Array.concat

    Array.concat [ "(" |> UTF8.toBytes
                   inl |> UTF8.toBytes
                   body
                   nl |> UTF8.toBytes
                   ")" |> UTF8.toBytes ]
  | DObj o ->
    if Map.isEmpty o then
      "{}" |> UTF8.toBytes
    else
      let rows =
        o
        |> Map.fold [] (fun l key value ->
          (Array.concat [ UTF8.toBytes (key + ": ")
                          toHashableRepr indent false value ]
           :: l))
        |> List.intersperse (UTF8.toBytes ("," + inl))

      Array.concat (
        [ UTF8.toBytes "{ "; UTF8.toBytes inl ]
        @ rows @ [ UTF8.toBytes nl; UTF8.toBytes "}" ]
      )
  | DOption None -> "Nothing" |> UTF8.toBytes
  | DOption (Some dv) ->
    Array.concat [ "Just " |> UTF8.toBytes; toHashableRepr indent false dv ]
  | DErrorRail dv ->
    Array.concat [ "ErrorRail: " |> UTF8.toBytes; toHashableRepr indent false dv ]
  | DResult (Ok dv) ->
    Array.concat [ "ResultOk " |> UTF8.toBytes; toHashableRepr indent false dv ]
  | DResult (Error dv) ->
    Array.concat [ "ResultError " |> UTF8.toBytes; toHashableRepr indent false dv ]
  | DBytes bytes ->
    if oldBytes then
      bytes
    else
      bytes
      |> System.Security.Cryptography.SHA384.HashData
      |> Base64.urlEncodeToString
      |> UTF8.toBytes



// Deprecated because it has a collision between [b"a"; b"bc"] and
// [b"ab"; b"c"]
let toHashV0 (arglist : List<Dval>) : string =
  arglist
  |> List.map (toHashableRepr 0 true)
  |> Array.concat
  |> System.Security.Cryptography.SHA384.HashData
  |> Base64.urlEncodeToString

let toHashV1 (arglist : List<Dval>) : string =
  DList arglist
  |> toHashableRepr 0 false
  |> System.Security.Cryptography.SHA384.HashData
  |> Base64.urlEncodeToString


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
