module LibExecution.DvalRepr

// Printing Dvals is more complicated than you'd expect. Different situations
// have different constaints, such as develop-focused representation showing
// explicitly what the value is, vs an API-based representation which does
// something clever with Option/Result types. There is also versioning, as not
// all changes are going to be backward compatible.

// Note: we inline a lot of code which could be reused. This is deliberate: it
// allows us reason more easily about what changes are going to be safe. In
// general, we should avoid general purpose or reusable functions in this file.

open Prelude
open Tablecloth

open RuntimeTypes

// FSTODO: json.net is very permissive. Make sure we only use it for internal
// types. What do we use to read from users? It should be strict.

// I tried System.Text.Json but ran into a number of problems:
//
// - Infinity/Nan: The original OCaml Yojson converter represented these
// special floats as invalid JSON, which System.Text.Json threw an exception
// over.
//
// - 82.0: It was possible to workaround this using incredibly ugly hacks, but
// by default 82.0 would be printed as `82` and there was no way to change
// that.

open Newtonsoft.Json
open Newtonsoft.Json.Linq

// TODO CLEANUP - remove all the unsafeDval by inlining them into the named
// functions that use them, such as toQueryable or toRoundtrippable

let writeJson (f : JsonWriter -> unit) : string =
  let stream = new System.IO.StringWriter()
  let w = new JsonTextWriter(stream)
  // Match yojson
  w.FloatFormatHandling <- FloatFormatHandling.Symbol
  f w
  stream.ToString()

let parseJson (s : string) : JToken =
  let reader = new JsonTextReader(new System.IO.StringReader(s))
  let jls = new JsonLoadSettings()
  jls.CommentHandling <- CommentHandling.Load // Load them so we can error later
  jls.DuplicatePropertyNameHandling <- DuplicatePropertyNameHandling.Error
  jls.CommentHandling <- CommentHandling.Ignore

  reader.DateParseHandling <- DateParseHandling.None
  JToken.ReadFrom(reader)

let formatFloat (f : float) : string = f.ToString("0.0################")

let writeFloat (w : JsonWriter) (f : float) =
  if System.Double.IsFinite f then
    // make sure to get the zeroes
    w.WriteRawValue(formatFloat f)
  else
    // json.NET encodes infinity, etc, appropriately for our use case
    w.WriteValue f

type JsonWriter with

  member this.writeObject(f : unit -> unit) =
    this.WriteStartObject()
    f ()
    this.WriteEnd()

  member this.writeArray(f : unit -> unit) =
    this.WriteStartArray()
    f ()
    this.WriteEnd()



// Determine whethere the number is a float or an int and parse appropriately.
// let parseJsonNumber (j : JsonElement) : Dval =
//   let str = j.GetRawText()
//
//   if String.includes "." str then
//     str |> System.Double.Parse |> DFloat
//   else
//     match str with
//     | "Infinity" -> DFloat System.Double.PositiveInfinity
//     | "-Infinity" -> DFloat System.Double.NegativeInfinity
//     | "infinity" -> DFloat System.Double.PositiveInfinity
//     | "-infinity" -> DFloat System.Double.NegativeInfinity
//     | "NaN" -> DFloat System.Double.NaN
//     | "nan" -> DFloat System.Double.NaN
//     | _ -> str |> parseBigint |> DInt
//

// -------------------------
// Runtime Types
// -------------------------
let rec dtypeToString (t : DType) : string =
  // TODO CLEANUP its unclear the full scope of what we use these for, and the
  // impact of cleaning up the terrible names here.
  // This function is used for putting lots of Data in the DB so we need to be super careful.
  match t with
  | TAny -> "Any"
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TNull -> "Nothing"
  | TChar -> "Character"
  | TStr -> "Str"
  | TList _ -> "List"
  | TFn _ -> "Block"
  | TLambda -> "Block"
  | TRecord _ -> "Dict"
  | TVariable name -> fstodo "dtype of TVariable"
  | TIncomplete -> "Incomplete"
  | TError -> "Error"
  | THttpResponse _ -> "Response"
  | TDB _ -> "Datastore"
  | TDate -> "Date"
  | TDict _ -> "Dict"
  | TPassword -> "Password"
  | TUuid -> "UUID"
  | TOption _ -> "Option"
  | TErrorRail -> "ErrorRail"
  | TResult _ -> "Result"
  | TUserType (name, _) -> name
  | TBytes -> "Bytes"


let rec typeToDeveloperReprV0 (t : DType) : string =
  match t with
  | TAny -> "Any"
  | TInt -> "Int"
  | TFloat -> "Float"
  | TBool -> "Bool"
  | TNull -> "Null"
  | TChar -> "Character"
  | TStr -> "String"
  | TList _ -> "List"
  | TDict _ -> "Dict"
  | TRecord _ -> "Dict"
  | TLambda -> "Block"
  | TFn _ -> "Block"
  | TVariable varname -> varname
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


let rec dtypeOfString (str : string) : DType =
  match String.toLowercase str with
  | "any" -> TAny
  | "int" -> TInt
  | "integer" -> TInt
  | "float" -> TFloat
  | "bool" -> TBool
  | "boolean" -> TBool
  | "nothing" -> TNull
  | "character"
  | "char" -> TChar
  | "str" -> TStr
  | "string" -> TStr
  | "list" -> TList TAny
  | "obj" -> TDict TAny
  | "block" -> TLambda
  | "incomplete" -> TIncomplete
  | "error" -> TError
  | "response" -> THttpResponse TAny
  | "datastore" -> TDB TAny
  | "date" -> TDate
  | "password" -> TPassword
  | "uuid" -> TUuid
  | "option" -> TOption TAny
  | "errorrail" -> TErrorRail
  | "result" -> TResult(TAny, TAny)
  | "dict" -> TDict TAny
  | _ -> failwith "unsupported runtime type"


(* Users should not be aware of this *)
let dtypeName (dv : Dval) : string =
  dv |> Dval.toType |> dtypeToString |> String.toLowercase

let prettyTypename (dv : Dval) : string = dv |> Dval.toType |> typeToDeveloperReprV0

let unsafeDTypeToJson (typ : DType) : string =
  typ |> dtypeToString |> String.toLowercase

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
            Map.fold [] (fun key value l -> (key + ": " + recurse value) :: l) o

          "{ " + inl + String.concat ("," + inl) strs + nl + "}"
    | _ -> reprfn dv

  inner 0 dv

let httpResponseToRepr (h) : string =
  match h with
  | Redirect url -> $"302 {url}"
  | Response (code, headers) ->
      let headerString =
        headers
        |> List.map (fun (k, v) -> k + ": " + v)
        |> String.concat ","
        |> fun s -> "{ " + s + " }"

      $"{code} {headerString}" + "\n"

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
    | DInt i -> i.ToString()
    | DBool true -> "true"
    | DBool false -> "false"
    | DStr s -> s
    | DFloat f -> formatFloat f
    | DChar c -> c
    | DNull -> "null"
    | DDate d -> d.toIsoString ()
    | DUuid uuid -> uuid.ToString()
    | DDB dbname -> $"<DB: {dbname}>"
    | DError _ ->
        // FSTODO make this a string again
        "Error: TODO: print message"
    | DIncomplete _ -> "<Incomplete>"
    | DFnVal _ ->
        // See docs/dblock-serialization.ml
        "<Block>"
    // | DPassword _ ->
    //     (* redacting, do not unredact *)
    //     "<Password>"
    | DObj _
    | DList _ -> toNestedString nestedreprfn dv
    | DErrorRail d ->
        // We don't print error here, because the errorrail value will know
        // whether it's an error or not.
        reprfn d
    | DHttpResponse (h, body) ->
        match h with
        | Redirect url -> $"302 {url}"
        | Response (code, headers) ->
            let headerString =
              headers
              |> List.map (fun (k, v) -> k + ": " + v)
              |> String.concat ","
              |> fun s -> "{ " + s + " }"

            $"{code} {headerString}" + "\n" + nestedreprfn dv
    | DResult (Ok d) -> reprfn d
    | DResult (Error d) -> "Error: " + reprfn d
    | DOption (Some d) -> reprfn d
    | DOption None -> "Nothing"
    | DBytes bytes -> System.BitConverter.ToString bytes

  reprfn dval

let rec toPrettyMachineJsonV1 (w : JsonWriter) (dv : Dval) : unit =
  let writeDval = toPrettyMachineJsonV1 w
  // utf8jsonwriter has different methods for writing into objects vs arrays.
  // Dark doesn't assume the outermost value is an array or object, so try
  // writing the array version to start, then recurse between the two as
  // appropriate based on the content.
  match dv with
  (* basic types *)
  | DInt i -> w.WriteValue i
  | DFloat f -> writeFloat w f
  | DBool b -> w.WriteValue b
  | DNull -> w.WriteNull()
  | DStr s -> w.WriteValue s
  | DList l -> w.writeArray (fun () -> List.iter writeDval l)
  | DObj o ->
      w.writeObject
        (fun () ->
          Map.iter
            (fun k v ->
              w.WritePropertyName k
              writeDval v)
            o)
  | DFnVal _ ->
      (* See docs/dblock-serialization.ml *)
      w.WriteNull()
  | DIncomplete _ -> w.WriteNull()
  | DChar c -> w.WriteValue c
  | DError (_, msg) ->
      w.writeObject
        (fun () ->
          w.WritePropertyName "Error"
          w.WriteValue msg)
  | DHttpResponse (h, response) -> fstodo "httpresponse"
  | DDB dbName -> w.WriteValue dbName
  | DDate date -> w.WriteValue(date.toIsoString ())
  // FSTODO
  // | DPassword hashed ->
  //     `Assoc [("Error", `String "Password is redacted")]
  | DUuid uuid -> w.WriteValue uuid
  | DOption opt ->
      match opt with
      | None -> w.WriteNull()
      | Some v -> writeDval v
  | DErrorRail dv -> writeDval dv
  | DResult res ->
      (match res with
       | Ok dv -> writeDval dv
       | Error dv ->
           w.writeObject
             (fun () ->
               w.WritePropertyName "Error"
               writeDval dv))
  | DBytes bytes ->
      //FSTODO: rather than using a mutable byte array, should this be a readonly span?
      w.WriteValue(System.Convert.ToBase64String bytes)


let toPrettyMachineJsonStringV1 (dval : Dval) : string =
  writeJson (fun w -> toPrettyMachineJsonV1 w dval)

// This special format was originally the default OCaml (yojson-derived) format
// for this.
let responseOfJson (a : Dval) : DHTTP =
  match a with
  | DList [ DStr "Redirect"; DStr url ] -> Redirect url
  | DList [ DStr "Response"; DInt code; DList headers ] ->
      let headers =
        headers
        |> List.map
             (function
             | DList [ DStr k; DStr v ] -> (k, v)
             | _ -> failwith "Invalid DHttpResponse headers")

      Response(int code, headers)
  | _ -> failwith "invalid response json"

#nowarn "104" // ignore warnings about enums out of range

// The "unsafe" variations here are bad. They encode data ambiguously, and
// though we mostly have the decoding right, it's brittle and unsafe.  This
// should be considered append only. There's a ton of dangerous things in this,
// and we really need to move off it, but for now we're here. Do not change
// existing encodings - this will break everything.
let rec unsafeDvalOfJsonV0 (json : JToken) : Dval =
  let convert = unsafeDvalOfJsonV0

  match json.Type with
  | JTokenType.Integer -> DInt(json.Value<int64>() |> bigint)
  | JTokenType.Float -> DFloat(json.Value<double>())
  | JTokenType.Boolean -> DBool(json.Value<bool>())
  | JTokenType.None
  | JTokenType.Null
  | JTokenType.Undefined -> DNull
  | JTokenType.String -> DStr(json.Value<string>())
  | JTokenType.Date ->
      // If json.NET finds a string-looking thing, it will encode it as a Date.
      // We don't want that because we have our own format for Dates, and this
      // might be a string with a date in it.
      DStr(json.Value<string>())

  | JTokenType.Array ->
      // We shouldnt have saved dlist that have incompletes or error rails but we might have
      seq (json.AsJEnumerable()) |> Seq.toList |> List.map convert |> Dval.list
  | JTokenType.Object ->
      let fields =
        seq (json.Values())
        |> Seq.toList
        |> List.map (fun (jp : JProperty) -> (jp.Name, convert jp.Value))
        |> List.sortBy (fun (k, _) -> k)
      // These are the only types that are allowed in the queryable
      // representation. We may allow more in the future, but the real thing to
      // do is to use the DB's type and version to encode/decode them correctly
      match fields with
      // DResp (Result.ok_or_failwith (dhttp_of_yojson a), unsafe_dval_of_yojson_v0 b)
      | [ ("type", DStr "response"); ("value", DList [ a; b ]) ] ->
          DHttpResponse(responseOfJson a, b)
      | [ ("type", DStr "date"); ("value", DStr v) ] ->
          DDate(System.DateTime.ofIsoString v)
      | [ ("type", DStr "password"); ("value", DStr v) ] ->
          // v |> B64.decode |> Bytes.of_string |> DPassword
          fstodo "password"
      | [ ("type", DStr "error"); ("value", DStr v) ] -> DError(SourceNone, v)
      | [ ("type", DStr "bytes"); ("value", DStr v) ] ->
          v |> System.Convert.FromBase64String |> DBytes
      | [ ("type", DStr "char"); ("value", DStr v) ] -> DChar v
      | [ ("type", DStr "character"); ("value", DStr v) ] -> DChar v
      | [ ("type", DStr "datastore"); ("value", DStr v) ] -> DDB v
      | [ ("type", DStr "incomplete"); ("value", DNull) ] -> DIncomplete SourceNone
      | [ ("type", DStr "errorrail"); ("value", dv) ] -> DErrorRail dv
      | [ ("type", DStr "option"); ("value", DNull) ] -> DOption None
      | [ ("type", DStr "option"); ("value", dv) ] -> DOption(Some dv)
      | [ ("type", DStr "block"); ("value", DNull) ] ->
          // See docs/dblock-serialization.ml
          DFnVal(
            Lambda { body = EBlank(id 56789); symtable = Map.empty; parameters = [] }
          )
      | [ ("type", DStr "uuid"); ("value", DStr v) ] -> DUuid(System.Guid v)
      | [ ("constructor", DStr "Ok"); ("type", DStr "result");
          ("values", DList [ dv ]) ] -> DResult(Ok dv)
      | [ ("constructor", DStr "Error"); ("type", DStr "result");
          ("values", DList [ dv ]) ] -> DResult(Error dv)
      | _ -> fields |> Map.ofList |> DObj
  // Json.NET does a bunch of magic based on the contents of various types.
  // For example, it has tokens for Dates, constructors, etc. We've tried to
  // disable all those so we fail if we see them. Hwoever, we might need to
  // just convert some of these into strings.
  | JTokenType.Date
  | JTokenType.Constructor
  | JTokenType.Property
  | JTokenType.Guid
  | JTokenType.Raw
  | JTokenType.Bytes
  | JTokenType.TimeSpan
  | JTokenType.Uri -> failwith $"Invalid type in json: {json}"
  | JTokenType.Comment -> failwith "Comments in JSON are not supported"




// and unsafe_dvalmap_of_yojson_v0 (json : Yojson.Safe.t) : dval_map =
//   match json with
//   | `Assoc alist ->
//       List.fold_left
//         alist
//         ~f:(fun m (k, v) ->
//           DvalMap.insert m ~key:k ~value:(unsafe_dval_of_yojson_v0 v))
//         ~init:DvalMap.empty
//   | _ ->
//       Exception.internal "Not a json object"

// Convert a dval (already converted from json) into
let rec unsafeDvalOfJsonV1 (json : JToken) : Dval =
  let convert = unsafeDvalOfJsonV1

  match json.Type with
  | JTokenType.Integer -> DInt(json.Value<int64>() |> bigint)
  | JTokenType.Float -> DFloat(json.Value<double>())
  | JTokenType.Boolean -> DBool(json.Value<bool>())
  | JTokenType.None
  | JTokenType.Null
  | JTokenType.Undefined -> DNull
  | JTokenType.String -> DStr(json.Value<string>())
  | JTokenType.Date ->
      // If json.NET finds a string-looking thing, it will encode it as a Date.
      // We don't want that because we have our own format for Dates, and this
      // might be a string with a date in it.
      DStr(json.Value<string>())
  | JTokenType.Array ->
      // We shouldnt have saved dlist that have incompletes or error rails but we might have
      // FSTODO: to_list here is preventing us from saving DErrors in arrays
      seq (json.AsJEnumerable()) |> Seq.toList |> List.map convert |> Dval.list
  | JTokenType.Object ->
      let fields =
        seq (json.Values())
        |> Seq.toList
        |> List.map (fun (jp : JProperty) -> (jp.Name, convert jp.Value))
        |> List.sortBy (fun (k, _) -> k)
      // These are the only types that are allowed in the queryable
      // representation. We may allow more in the future, but the real thing to
      // do is to use the DB's type and version to encode/decode them correctly
      match fields with
      // DResp (Result.ok_or_failwith (dhttp_of_yojson a), unsafe_dval_of_yojson_v0 b)
      | [ ("type", DStr "response"); ("value", DList [ a; b ]) ] ->
          DHttpResponse(responseOfJson a, b)
      | [ ("type", DStr "date"); ("value", DStr v) ] ->
          DDate(System.DateTime.ofIsoString v)
      | [ ("type", DStr "password"); ("value", DStr v) ] ->
          // v |> B64.decode |> Bytes.of_string |> DPassword
          fstodo "password"
      | [ ("type", DStr "error"); ("value", DStr v) ] -> DError(SourceNone, v)
      | [ ("type", DStr "bytes"); ("value", DStr v) ] ->
          v |> System.Convert.FromBase64String |> DBytes
      | [ ("type", DStr "char"); ("value", DStr v) ] ->
          v |> String.toEgcSeq |> Seq.head |> DChar
      | [ ("type", DStr "character"); ("value", DStr v) ] ->
          v |> String.toEgcSeq |> Seq.head |> DChar
      | [ ("type", DStr "datastore"); ("value", DStr v) ] -> DDB v
      | [ ("type", DStr "incomplete"); ("value", DNull) ] -> DIncomplete SourceNone
      | [ ("type", DStr "errorrail"); ("value", dv) ] -> DErrorRail dv
      | [ ("type", DStr "option"); ("value", DNull) ] -> DOption None
      | [ ("type", DStr "option"); ("value", dv) ] -> DOption(Some dv)
      | [ ("type", DStr "block"); ("value", DNull) ] ->
          // See docs/dblock-serialization.ml
          DFnVal(
            Lambda { body = EBlank(id 23456); symtable = Map.empty; parameters = [] }
          )
      | [ ("type", DStr "uuid"); ("value", DStr v) ] -> DUuid(System.Guid v)
      | [ ("constructor", DStr "Ok"); ("type", DStr "result");
          ("values", DList [ dv ]) ] -> DResult(Ok dv)
      | [ ("constructor", DStr "Error"); ("type", DStr "result");
          ("values", DList [ dv ]) ] -> DResult(Error dv)
      | _ -> fields |> Map.ofList |> DObj
  // Json.NET does a bunch of magic based on the contents of various types.
  // For example, it has tokens for Dates, constructors, etc. We've tried to
  // disable all those so we fail if we see them. Hwoever, we might need to
  // just convert some of these into strings.
  | JTokenType.Date
  | JTokenType.Constructor
  | JTokenType.Property
  | JTokenType.Guid
  | JTokenType.Raw
  | JTokenType.Bytes
  | JTokenType.TimeSpan
  | JTokenType.Uri -> failwith $"Invalid type in json: {json}"
  | JTokenType.Comment -> failwith "Comments in JSON are not supported"




// and unsafeDvalmapOfJsonV1 (j : J.JsonValue) : DvalMap =
//   match j with
//   | J.JsonValue.Record records ->
//       Array.fold
//         Map.empty
//         (fun m (k, v) -> Map.add k (unsafeDvalOfJsonV1 v) m)
//         records
//   | _ -> failwith "Not a json object"

let rec unsafeDvalToJsonValueV0 (w : JsonWriter) (redact : bool) (dv : Dval) : unit =
  let writeDval = unsafeDvalToJsonValueV0 w redact

  let wrapStringValue (typ : string) (str : string) =
    w.writeObject
      (fun () ->
        w.WritePropertyName "type"
        w.WriteValue(typ)
        w.WritePropertyName "value"
        w.WriteValue(str))

  let wrapNullValue (typ : string) =
    w.writeObject
      (fun () ->
        w.WritePropertyName "type"
        w.WriteValue(typ)
        w.WritePropertyName "value"
        w.WriteNull())

  let wrapNestedDvalValue (typ : string) (dv : Dval) =
    w.writeObject
      (fun () ->
        w.WritePropertyName "type"
        w.WriteValue(typ)
        w.WritePropertyName "value"
        writeDval dv)

  match dv with
  (* basic types *)
  | DInt i -> w.WriteValue i
  | DFloat f -> writeFloat w f
  | DBool b -> w.WriteValue b
  | DNull -> w.WriteNull()
  | DStr s -> w.WriteValue s
  | DList l -> w.writeArray (fun () -> List.iter writeDval l)
  | DObj o ->
      w.writeObject
        (fun () ->
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
  | DError (_, msg) -> wrapStringValue "error" msg
  | DHttpResponse (h, hdv : Dval) ->
      w.writeObject
        (fun () ->
          w.WritePropertyName "type"
          w.WriteValue "response"
          w.WritePropertyName "value"

          w.writeArray
            (fun () ->
              match h with
              | Redirect str ->
                  w.writeArray
                    (fun () ->
                      w.WriteValue "Redirect"
                      w.WriteValue str)
              | Response (code, headers) ->
                  w.writeArray
                    (fun () ->
                      w.WriteValue "Response"
                      w.WriteValue code

                      w.writeArray
                        (fun () ->
                          List.iter
                            (fun (k : string, v : string) ->
                              w.writeArray
                                (fun () ->
                                  w.WriteValue k
                                  w.WriteValue v))
                            headers))

              writeDval hdv))
  | DDB dbname -> wrapStringValue "datastore" dbname
  | DDate date -> wrapStringValue "date" (date.toIsoString ())
  // | DPassword hashed ->
  //     if redact then
  //       wrap_user_type J.nil
  //     else
  //       hashed |> Bytes.to_string |> B64.encode |> wrap_user_str
  | DUuid uuid -> wrapStringValue "uuid" (uuid.ToString())
  | DOption opt ->
      (match opt with
       | None -> wrapNullValue "option"
       | Some ndv -> wrapNestedDvalValue "option" ndv)
  | DErrorRail erdv -> wrapNestedDvalValue "errorrail" erdv
  | DResult res ->
      (match res with
       | Ok rdv ->
           w.writeObject
             (fun () ->
               w.WritePropertyName "type"
               w.WriteValue("result")
               w.WritePropertyName "constructor"
               w.WriteValue("Ok")
               w.WritePropertyName "values"
               w.writeArray (fun () -> writeDval rdv))
       | Error rdv ->
           w.writeObject
             (fun () ->
               w.WritePropertyName "type"
               w.WriteValue("result")
               w.WritePropertyName "constructor"
               w.WriteValue("Error")
               w.WritePropertyName "values"
               w.writeArray (fun () -> writeDval rdv)))
  | DBytes bytes -> wrapStringValue "bytes" (System.Convert.ToBase64String bytes)



let unsafeDvalToJsonValueV1 (w : JsonWriter) (redact : bool) (dv : Dval) : unit =
  unsafeDvalToJsonValueV0 w redact dv

(* ------------------------- *)
(* Roundtrippable - for events and traces *)
(* ------------------------- *)
let toInternalRoundtrippableV0 (dval : Dval) : string =
  writeJson (fun w -> unsafeDvalToJsonValueV1 w false dval)


let ofInternalRoundtrippableJsonV0 (j : JToken) : Result<Dval, string> =
  (* Switched to v1 cause it was a bug fix *)
  try
    unsafeDvalOfJsonV1 j |> Ok
  with e -> Error(e.ToString())

let ofInternalRoundtrippableV0 (str : string) : Dval =
  // cleanup: we know the types here, so we should probably do type directed parsing and simplify what's stored
  str |> parseJson |> unsafeDvalOfJsonV1

// -------------------------
// Queryable - for the DB *)
// -------------------------
let toInternalQueryableV0 (dval : Dval) : string =
  writeJson (fun w -> unsafeDvalToJsonValueV0 w false dval)


let ofInternalQueryableV0 (str : string) : Dval =
  str |> parseJson |> unsafeDvalOfJsonV0


let toInternalQueryableV1 (dvalMap : DvalMap) : string =
  writeJson
    (fun w ->
      w.WriteStartObject()

      dvalMap
      |> Map.toList
      |> List.iter
           (fun (k, dval) ->
             (w.WritePropertyName k
              unsafeDvalToJsonValueV0 w false dval))

      w.WriteEnd())

let ofInternalQueryableV1 (str : string) : Dval =
  // The first level _must_ be an object at the moment
  let rec convertTopLevel (json : JToken) : Dval =
    match json.Type with
    | JTokenType.Object -> convert json
    | _ -> failwith "Value that isn't an object"

  and convert (json : JToken) : Dval =
    match json.Type with
    | JTokenType.Integer -> DInt(json.Value<int64>() |> bigint)
    | JTokenType.Float -> DFloat(json.Value<double>())
    | JTokenType.Boolean -> DBool(json.Value<bool>())
    | JTokenType.None
    | JTokenType.Null
    | JTokenType.Undefined -> DNull
    | JTokenType.String -> DStr(json.Value<string>())
    | JTokenType.Array ->
        // We shouldnt have saved dlist that have incompletes or error rails but we might have
        seq (json.AsJEnumerable()) |> Seq.toList |> List.map convert |> Dval.list
    | JTokenType.Object ->
        let fields =
          seq (json.Values())
          |> Seq.toList
          |> List.map (fun (jp : JProperty) -> (jp.Name, convert jp.Value))
          |> List.sortBy (fun (k, _) -> k)
        // These are the only types that are allowed in the queryable
        // representation. We may allow more in the future, but the real thing to
        // do is to use the DB's type and version to encode/decode them correctly
        match fields with
        | [ ("type", DStr "date"); ("value", DStr v) ] ->
            DDate(System.DateTime.ofIsoString v)
        | [ ("type", DStr "password"); ("value", DStr v) ] -> fstodo "password"
        // v |> B64.decode |> Bytes.of_string |> DPassword
        | [ ("type", DStr "uuid"); ("value", DStr v) ] -> DUuid(System.Guid v)
        | _ -> fields |> Map.ofList |> DObj
    // Json.NET does a bunch of magic based on the contents of various types.
    // For example, it has tokens for Dates, constructors, etc. We've tried to
    // disable all those so we fail if we see them. Hwoever, we might need to
    // just convert some of these into strings.
    | JTokenType.Date
    | JTokenType.Constructor
    | JTokenType.Property
    | JTokenType.Guid
    | JTokenType.Raw
    | JTokenType.Bytes
    | JTokenType.TimeSpan
    | JTokenType.Uri -> failwith $"Invalid type in json: {json}"
    | JTokenType.Comment -> failwith "Comments in JSON are not supported"


  str |> parseJson |> convertTopLevel

// -------------------------
// Other formats *)
// -------------------------
// let rec to_enduser_readable_text_v0 dval =
//   let rec nestedreprfn dv =
//     (* If nesting inside an object or a list, wrap strings in quotes *)
//     match dv with
//     | DStr _ | DUuid _ | DCharacter _ ->
//         "\"" ^ reprfn dv ^ "\""
//     | _ ->
//         reprfn dv
//   and reprfn dv =
//     match dv with
//     | DInt i ->
//         Dint.to_string i
//     | DBool true ->
//         "true"
//     | DBool false ->
//         "false"
//     | DStr s ->
//         Unicode_string.to_string s
//     | DFloat f ->
//         string_of_float f
//     | DCharacter c ->
//         Unicode_string.Character.to_string c
//     | DNull ->
//         "null"
//     | DDate d ->
//         Util.isostring_of_date d
//     | DUuid uuid ->
//         Uuidm.to_string uuid
//     | DDB dbname ->
//         "<DB: " ^ dbname ^ ">"
//     | DError (_, msg) ->
//         "Error: " ^ msg
//     | DIncomplete _ ->
//         "<Incomplete>"
//     | DBlock _ ->
//         (* See docs/dblock-serialization.ml *)
//         "<Block>"
//     | DPassword _ ->
//         (* redacting, do not unredact *)
//         "<Password>"
//     | DObj o ->
//         to_nested_string ~reprfn:nestedreprfn dv
//     | DList l ->
//         to_nested_string ~reprfn:nestedreprfn dv
//     | DErrorRail d ->
//         (* We don't print error here, because the errorrail value will know
//            * whether it's an error or not. *)
//         reprfn d
//     | DResp (dh, dv) ->
//         dhttp_to_formatted_string dh ^ "\n" ^ nestedreprfn dv ^ ""
//     | DResult (ResOk d) ->
//         reprfn d
//     | DResult (ResError d) ->
//         "Error: " ^ reprfn d
//     | DOption (OptJust d) ->
//         reprfn d
//     | DOption OptNothing ->
//         "Nothing"
//     | DBytes bytes ->
//         Bytes.to_string bytes
//   in
//   reprfn dval
//
//
// let to_enduser_readable_html_v0 dv = to_enduser_readable_text_v0 dv

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
    // | DPassword _ ->
    //     "<password>"
    | DStr s -> $"\"{s}\""
    | DChar c -> $"'{c}'"
    | DInt i -> i.ToString()
    | DBool true -> "true"
    | DBool false -> "false"
    | DFloat f -> formatFloat f
    | DNull -> "null"
    | DFnVal _ ->
        (* See docs/dblock-serialization.ml *)
        justtipe
    | DIncomplete _ -> justtipe
    | DError (_, msg) -> wrap msg
    | DDate d -> wrap (d.toIsoString ())
    | DDB name -> wrap name
    | DUuid uuid -> wrap (uuid.ToString())
    | DHttpResponse (h, hdv) -> httpResponseToRepr h + nl + toRepr_ indent hdv
    | DList l ->
        if List.is_empty l then
          "[]"
        else
          let elems = String.concat ", " (List.map (toRepr_ indent) l)
          $"[{inl}{elems}{nl}]"
    | DObj o ->
        if Map.isEmpty o then
          "{}"
        else
          let strs =
            Map.fold
              []
              (fun key value l -> ($"{key}: {toRepr_ indent value}") :: l)
              o

          let elems = String.concat $",{inl}" strs
          "{" + $"{inl}{elems}{nl}" + "}}"
    | DOption None -> "Nothing"
    | DOption (Some dv) -> "Just " + toRepr_ indent dv
    | DResult (Ok dv) -> "Ok " + toRepr_ indent dv
    | DResult (Error dv) -> "Error " + toRepr_ indent dv
    | DErrorRail dv -> "ErrorRail: " + toRepr_ indent dv
    | DBytes bytes -> bytes |> System.Convert.ToBase64String

  toRepr_ 0 dv

//
// let to_pretty_machine_yojson_v1 dval =
//   let rec recurse dv =
//     match dv with
//     (* basic types *)
//     | DInt i ->
//         Dint.to_yojson i
//     | DFloat f ->
//         `Float f
//     | DBool b ->
//         `Bool b
//     | DNull ->
//         `Null
//     | DStr s ->
//         Unicode_string.to_yojson s
//     | DList l ->
//         `List (List.map l recurse)
//     | DObj o ->
//         o
//         |> DvalMap.to_list
//         |> List.map ~f:(fun (k, v) -> (k, recurse v))
//         |> fun x -> `Assoc x
//     | DBlock _ ->
//         (* See docs/dblock-serialization.ml *)
//         `Null
//     | DIncomplete _ ->
//         `Null
//     | DCharacter c ->
//         `String (Unicode_string.Character.to_string c)
//     | DError (_, msg) ->
//         `Assoc [("Error", `String msg)]
//     | DResp (h, hdv) ->
//         recurse hdv
//     | DDB dbname ->
//         `String dbname
//     | DDate date ->
//         `String (Util.isostring_of_date date)
//     | DPassword hashed ->
//         `Assoc [("Error", `String "Password is redacted")]
//     | DUuid uuid ->
//         `String (Uuidm.to_string uuid)
//     | DOption opt ->
//       (match opt with OptNothing -> `Null | OptJust dv -> recurse dv)
//     | DErrorRail dv ->
//         recurse dv
//     | DResult res ->
//       ( match res with
//       | ResOk dv ->
//           recurse dv
//       | ResError dv ->
//           `Assoc [("Error", recurse dv)] )
//     | DBytes bytes ->
//         `String (bytes |> RawBytes.to_string |> B64.encode)
//   in
//   recurse dval
//
//
// let to_pretty_machine_json_v1 dval : string =
//   to_pretty_machine_yojson_v1 dval |> Yojson.Safe.pretty_to_string
//
//
// let of_unknown_json_v0 str =
//   try str |> Yojson.Safe.from_string |> unsafe_dval_of_yojson_v0
//   with e ->
//     Exception.code ~actual:str ("Invalid json: " ^ Exception.to_string e)
//
//
// let of_unknown_json_v1 str =
//   let rec convert json =
//     match json with
//     | `Int i ->
//         DInt (Dint.of_int i)
//     | `Intlit i ->
//         DInt (Dint.of_string_exn i)
//     | `Float f ->
//         DFloat f
//     | `Bool b ->
//         DBool b
//     | `Null ->
//         DNull
//     | `String s ->
//         dstr_of_string_exn s
//     | `List l ->
//         to_list (List.map ~f:convert l)
//     | `Variant v ->
//         Exception.internal "We dont use variants"
//     | `Tuple v ->
//         Exception.internal "We dont use tuples"
//     | `Assoc alist ->
//         DObj
//           (List.fold_left
//              alist
//              ~f:(fun m (k, v) -> DvalMap.insert m ~key:k ~value:(convert v))
//              ~init:DvalMap.empty)
//   in
//   str |> Yojson.Safe.from_string |> convert
//
//
// let rec show dv =
//   match dv with
//   | DInt i ->
//       Dint.to_string i
//   | DBool true ->
//       "true"
//   | DBool false ->
//       "false"
//   | DStr s ->
//       Unicode_string.to_string s
//   | DFloat f ->
//       string_of_float f
//   | DCharacter c ->
//       Unicode_string.Character.to_string c
//   | DNull ->
//       "null"
//   | DDate d ->
//       Util.isostring_of_date d
//   | DUuid uuid ->
//       Uuidm.to_string uuid
//   | DDB dbname ->
//       "<DB: " ^ dbname ^ ">"
//   | DError (_, msg) ->
//       "<Error: " ^ msg ^ ">"
//   | DIncomplete SourceNone ->
//       "<Incomplete>"
//   | DIncomplete (SourceId (tlid, id)) ->
//       Printf.sprintf "<Incomplete[%s,%s]>" (string_of_id tlid) (string_of_id id)
//   | DBlock _ ->
//       (* See docs/dblock-serialization.ml *)
//       "<Block>"
//   | DPassword _ ->
//       (* redacting, do not unredact *)
//       "<Password>"
//   | DObj o ->
//       to_nested_string ~reprfn:show dv
//   | DList l ->
//       to_nested_string ~reprfn:show dv
//   | DErrorRail d ->
//       (* We don't print error here, because the errorrail value will know
//           * whether it's an error or not. *)
//       "<ErrorRail: " ^ show d ^ ">"
//   | DResp (dh, dv) ->
//       dhttp_to_formatted_string dh ^ "\n" ^ show dv ^ ""
//   | DResult (ResOk d) ->
//       "Ok " ^ show d
//   | DResult (ResError d) ->
//       "Error " ^ show d
//   | DOption (OptJust d) ->
//       "Just " ^ show d
//   | DOption OptNothing ->
//       "Nothing"
//   | DBytes bytes ->
//       "<Bytes: length=" ^ string_of_int (RawBytes.length bytes) ^ ">"
//
//
// let parse_literal (str : string) : dval option =
//   let len = String.length str in
//   (* Character *)
//   if len > 2 && str.[0] = '\'' && str.[len - 1] = '\''
//   then
//     Some
//       (DCharacter
//          (Unicode_string.Character.unsafe_of_string
//             (String.sub ~pos:1 ~len:(len - 2) str)))
//     (* String *)
//   else if len > 1 && str.[0] = '"' && str.[len - 1] = '"'
//   then
//     (* It might have \n characters in it (as well as probably other codes like
//      * \r or some shit that we haven't taken into account), which need to be
//      * converted manually to appropriate string chars. *)
//     str
//     |> String.sub ~pos:1 ~len:(len - 2)
//     |> Util.string_replace "\\\"" "\""
//     |> fun s -> Some (dstr_of_string_exn s)
//   else if str = "null"
//   then Some DNull
//   else if str = "true"
//   then Some (DBool true)
//   else if str = "false"
//   then Some (DBool false)
//   else
//     try Some (DInt (Dint.of_string_exn str))
//     with _ ->
//       ( match float_of_string_opt str with
//       | Some v ->
//           Some (DFloat v)
//       | None ->
//           None )
//
//
// (* ------------------------- *)
// (* Conversion Functions *)
// (* ------------------------- *)
// let to_char dv : string option =
//   match dv with
//   | DCharacter c ->
//       Some (Unicode_string.Character.to_string c)
//   | _ ->
//       None
//
//
// let to_int dv : Dint.t option = match dv with DInt i -> Some i | _ -> None
//
// let to_float dv : Float.t option =
//   match dv with DFloat f -> Some f | _ -> None
//
//
// let dint (i : int) : dval = DInt (Dint.of_int i)
//
// let to_dobj_exn (pairs : (string * dval) list) : dval =
//   match DvalMap.from_list_unique pairs with
//   | Ok ok ->
//       DObj ok
//   | Error err ->
//       DError (SourceNone, err)
//
//
// let to_string_opt dv : string option =
//   match dv with DStr s -> Some (Unicode_string.to_string s) | _ -> None
//
//
// let to_string_exn dv : string =
//   match to_string_opt dv with
//   | Some s ->
//       s
//   | None ->
//       Exception.code "expecting str" ~actual:(to_developer_repr_v0 dv)
//
//
// let to_dval_pairs_exn dv : (string * dval) list =
//   match dv with
//   | DObj obj ->
//       DvalMap.to_list obj
//   | _ ->
//       Exception.code "expecting str" ~actual:(to_developer_repr_v0 dv)
//
//
// let to_string_pairs_exn dv : (string * string) list =
//   dv |> to_dval_pairs_exn |> List.map ~f:(fun (k, v) -> (k, to_string_exn v))
//
//
// (* For putting into URLs as query params *)
// let rec to_url_string_exn (dv : dval) : string =
//   match dv with
//   | DBlock _ ->
//       (* See docs/dblock-serialization.ml *)
//       "<" ^ (dv |> tipename) ^ ">"
//   | DIncomplete _ | DPassword _ ->
//       "<" ^ (dv |> tipename) ^ ">"
//   | DInt i ->
//       Dint.to_string i
//   | DBool true ->
//       "true"
//   | DBool false ->
//       "false"
//   | DStr s ->
//       Unicode_string.to_string s
//   | DFloat f ->
//       string_of_float f
//   | DCharacter c ->
//       Unicode_string.Character.to_string c
//   | DNull ->
//       "null"
//   | DDate d ->
//       Util.isostring_of_date d
//   | DDB dbname ->
//       dbname
//   | DErrorRail d ->
//       to_url_string_exn d
//   | DError (_, msg) ->
//       "error=" ^ msg
//   | DUuid uuid ->
//       Uuidm.to_string uuid
//   | DResp (_, hdv) ->
//       to_url_string_exn hdv
//   | DList l ->
//       "[ " ^ String.concat ~sep:", " (List.map ~f:to_url_string_exn l) ^ " ]"
//   | DObj o ->
//       let strs =
//         DvalMap.foldl o ~init:[] ~f:(fun ~key ~value l ->
//             (key ^ ": " ^ to_url_string_exn value) :: l)
//       in
//       "{ " ^ String.concat ~sep:", " strs ^ " }"
//   | DOption OptNothing ->
//       "none"
//   | DOption (OptJust v) ->
//       to_url_string_exn v
//   | DResult (ResError v) ->
//       "error=" ^ to_url_string_exn v
//   | DResult (ResOk v) ->
//       to_url_string_exn v
//   | DBytes bytes ->
//       bytes |> RawBytes.to_string |> B64.encode
//
//
// (* ------------------------- *)
// (* Forms and queries Functions *)
// (* ------------------------- *)
//
// let query_to_dval (query : (string * string list) list) : dval =
//   query
//   |> List.map ~f:(fun (key, vals) ->
//          let dval =
//            match vals with
//            | [] ->
//                DNull
//            | [v] ->
//                if v = "" then DNull else dstr_of_string_exn v
//            | vals ->
//                DList (List.map ~f:(fun x -> dstr_of_string_exn x) vals)
//          in
//          (key, dval))
//   |> DvalMap.from_list
//   |> DObj
//
//
// let dval_to_query (dv : dval) : (string * string list) list =
//   match dv with
//   | DObj kvs ->
//       kvs
//       |> DvalMap.to_list
//       |> List.map ~f:(fun (k, value) ->
//              match value with
//              | DNull ->
//                  (k, [])
//              | DList l ->
//                  (k, List.map ~f:to_url_string_exn l)
//              | _ ->
//                  (k, [to_url_string_exn value]))
//   | _ ->
//       Exception.code "attempting to use non-object as query param"
//
//
// let to_form_encoding (dv : dval) : string =
//   dv |> dval_to_query |> Uri.encoded_of_query
//
//
// let of_form_encoding (f : string) : dval =
//   f |> Uri.query_of_encoded |> query_to_dval
//
//
// (* ------------------------- *)
// (* Hashes *)
// (* ------------------------- *)
//
// (* This has been used to save millions of values in our DB, so the format isn't
//  * amenable to change without a migration. Don't change ANYTHING for existing
//  * values, but continue to add representations for new values. Also, inline
//  * everything! *)
// let rec to_hashable_repr ?(indent = 0) ?(old_bytes = false) (dv : dval) : string
//     =
//   let nl = "\n" ^ String.make indent ' ' in
//   let inl = "\n" ^ String.make (indent + 2) ' ' in
//   let indent = indent + 2 in
//   match dv with
//   | DDB dbname ->
//       "<db: " ^ dbname ^ ">"
//   | DInt i ->
//       Dint.to_string i
//   | DBool true ->
//       "true"
//   | DBool false ->
//       "false"
//   | DFloat f ->
//       string_of_float f
//   | DNull ->
//       "null"
//   | DStr s ->
//       "\"" ^ Unicode_string.to_string s ^ "\""
//   | DCharacter c ->
//       "'" ^ Unicode_string.Character.to_string c ^ "'"
//   | DIncomplete _ ->
//       "<incomplete: <incomplete>>" (* Can't be used anyway *)
//   | DBlock _ ->
//       (* See docs/dblock-serialization.ml *)
//       "<block: <block>>"
//   | DError (_, msg) ->
//       "<error: " ^ msg ^ ">"
//   | DDate d ->
//       "<date: " ^ Util.isostring_of_date d ^ ">"
//   | DPassword _ ->
//       "<password: <password>>"
//   | DUuid id ->
//       "<uuid: " ^ Uuidm.to_string id ^ ">"
//   | DResp (h, hdv) ->
//       (* deliberately inlined *)
//       let dhttp_to_formatted_string (d : dhttp) : string =
//         match d with
//         | Redirect url ->
//             "302 " ^ url
//         | Response (c, hs) ->
//             let string_of_headers hs =
//               hs
//               |> List.map ~f:(fun (k, v) -> k ^ ": " ^ v)
//               |> String.concat ~sep:","
//               |> fun s -> "{ " ^ s ^ " }"
//             in
//             string_of_int c ^ " " ^ string_of_headers hs
//       in
//       dhttp_to_formatted_string h ^ nl ^ to_hashable_repr ~indent hdv
//   | DList l ->
//       if List.is_empty l
//       then "[]"
//       else
//         "[ "
//         ^ inl
//         ^ String.concat ~sep:", " (List.map ~f:(to_hashable_repr ~indent) l)
//         ^ nl
//         ^ "]"
//   | DObj o ->
//       if DvalMap.is_empty o
//       then "{}"
//       else
//         let strs =
//           DvalMap.foldl o ~init:[] ~f:(fun ~key ~value l ->
//               (key ^ ": " ^ to_hashable_repr ~indent value) :: l)
//         in
//         "{ " ^ inl ^ String.concat ~sep:("," ^ inl) strs ^ nl ^ "}"
//   | DOption OptNothing ->
//       "Nothing"
//   | DOption (OptJust dv) ->
//       "Just " ^ to_hashable_repr ~indent dv
//   | DErrorRail dv ->
//       "ErrorRail: " ^ to_hashable_repr ~indent dv
//   | DResult (ResOk dv) ->
//       "ResultOk " ^ to_hashable_repr ~indent dv
//   | DResult (ResError dv) ->
//       "ResultError " ^ to_hashable_repr ~indent dv
//   | DBytes bytes ->
//       if old_bytes
//       then bytes |> RawBytes.to_string
//       else bytes |> Util.hash_bytes
//
//
// let supported_hash_versions : int list = [0; 1]
//
// let current_hash_version = 1
//
// (* Originally to prevent storing sensitive data to disk, this also reduces the
//  * size of the data stored by only storing a hash *)
// let hash (version : int) (arglist : dval list) : string =
//   (* Version 0 deprecated because it has a collision between [b"a"; b"bc"] and
//    * [b"ab"; b"c"] *)
//   match version with
//   | 0 ->
//       arglist
//       |> List.map ~f:(to_hashable_repr ~old_bytes:true)
//       |> String.concat
//       |> Util.hash
//   | 1 ->
//       DList arglist |> to_hashable_repr |> Util.hash
//   | _ ->
//       Exception.internal ("Invalid Dval.hash version: " ^ string_of_int version)
//
