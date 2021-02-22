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

open System.Text.Json

let writeJson (f : Utf8JsonWriter -> unit) : string =
  let stream = new System.IO.MemoryStream ()
  let w = new Utf8JsonWriter(stream)
  f w
  w.Flush ()
  System.Text.Encoding.UTF8.GetString (stream.ToArray())

let parseJson (s : string) : JsonElement =
  (JsonDocument.Parse s).RootElement

let formatFloat (f : float) : string =
  f.ToString("0.0################")

let writeFloat (w : Utf8JsonWriter) (f : float) =
  // This is an ugly hack to get around what appears to be missing
  // functionality, while trying to make a float like "82.0" appear with the
  // ".0":
  //
  // - The standard Utf8JsonWriter.WriteNumberValue method takes a double and
  // formats it for me, and the format (which is the standard 'G' format) omits
  // the ".0" suffix.
  //
  // By design it seems that I can't just write a raw string to Utf8JsonWriter,
  // but I found a workaround: to create a JsonElement and call
  // JsonElement.WriteTo`.  This calls a private method in Utf8JsonWriter and
  // writes the string directly into it.
  //
  // Obviously the performance here will be bad. Looking for a better solution.
  let floatStr = formatFloat f
  let jse = JsonDocument.Parse(floatStr).RootElement
  jse.WriteTo w

let writeFloatProperty (w : Utf8JsonWriter) (field : string) (f : float) =
  let floatStr = formatFloat f
  let jse = JsonDocument.Parse(floatStr).RootElement
  w.WritePropertyName field
  jse.WriteTo w




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

let unsafeDTypeToJson (typ : DType) : string = typ |> dtypeToString |> String.toLowercase

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

let toPrettyMachineJsonV1 (w : Utf8JsonWriter) (dval : Dval) : unit =
  // utf8jsonwriter has different methods for writing into objects vs arrays.
  // Dark doesn't assume the outermost value is an array or object, so try
  // writing the array version to start, then recurse between the two as
  // appropriate based on the content.
  let rec writeValue (dv : Dval) =
    match dv with
    (* basic types *)
    | DInt i -> w.WriteNumberValue (decimal i)
    | DFloat f -> writeFloat w f
    | DBool b -> w.WriteBooleanValue b
    | DNull -> w.WriteNullValue ()
    | DStr s -> w.WriteStringValue s
    | DList l ->
        w.WriteStartArray ()
        List.iter writeValue l
        w.WriteEndArray ()
    | DObj o ->
        w.WriteStartObject ()
        Map.iter (fun k v -> writeInObj k v) o
        w.WriteEndObject ()
    | DFnVal _ ->
        (* See docs/dblock-serialization.ml *)
        w.WriteNullValue ()
    | DIncomplete _ -> w.WriteNullValue ()
    | DChar c -> w.WriteStringValue c
    | DError (_, msg) ->
        w.WriteStartObject ()
        fstodo "handle derror"
        writeInObj "Error" (DStr msg)
        w.WriteEndObject ()
    | DHttpResponse (h, response) -> fstodo "httpresponse"
    | DDB dbName -> w.WriteStringValue dbName
    | DDate date -> w.WriteStringValue (date.toIsoString ())
    // FSTODO
    // | DPassword hashed ->
    //     `Assoc [("Error", `String "Password is redacted")]
    | DUuid uuid -> w.WriteStringValue uuid
    | DOption opt -> match opt with | None -> w.WriteNullValue () | Some v -> writeValue v
    | DErrorRail dv -> writeValue dv
    | DResult res ->
         (match res with
         | Ok dv -> writeValue dv
         | Error dv ->
            w.WriteStartObject ()
            writeInObj "Error" dv
            w.WriteEndObject ())
    | DBytes bytes ->
        //FSTODO: rather than using a mutable byte array, should this be a readonly span?
        w.WriteStringValue (System.Convert.ToBase64String bytes)

  and writeInObj (field : string) (dv : Dval) =
    match dv with
    (* basic types *)
    | DInt i -> w.WriteNumber(field, decimal i)
    | DFloat f -> writeFloatProperty w field f
    | DBool b -> w.WriteBoolean(field, b)
    | DNull -> w.WriteNull field
    | DStr s -> w.WriteString (field, s)
    | DList l ->
        w.WriteStartArray field
        List.iter writeValue l
        w.WriteEndArray ()
    | DObj o ->
        w.WriteStartObject field
        Map.iter (fun k v -> writeInObj k v) o
        w.WriteEndObject ()
    | DFnVal _ ->
        (* See docs/dblock-serialization.ml *)
        w.WriteNull field
    | DIncomplete _ -> w.WriteNull field
    | DChar c -> w.WriteString(field, c)
    | DError (_, msg) ->
        w.WriteStartObject field
        fstodo "handle derror"
        writeInObj "Error" (DStr "msg")
        w.WriteEndObject ()
    | DHttpResponse (h, response) -> fstodo "httpresponse"
    | DDB dbName -> w.WriteString(field, dbName)
    | DDate date -> w.WriteString(field, date.toIsoString ())
    // | DPassword hashed -> // FSOTOD
    //     `Assoc [("Error", `String "Password is redacted")]
    | DUuid uuid -> w.WriteString(field, uuid)
    | DOption opt -> match opt with | None -> w.WriteNull field | Some v -> writeInObj field v
    | DErrorRail dv -> writeInObj field dv
    | DResult res ->
         (match res with
         | Ok dv -> writeInObj field dv
         | Error dv ->
            w.WriteStartObject ()
            writeInObj "Error" dv
            w.WriteEndObject ())
    | DBytes bytes ->
        //FSTODO: rather than using a mutable byte array, should this be a readonly span?
        w.WriteString (field,System.Convert.ToBase64String bytes)
  writeValue dval


let toPrettyMachineJsonStringV1 (dval : Dval) : string =
  writeJson (fun w ->
    toPrettyMachineJsonV1 w dval
  )


// The "unsafe" variations here are bad. They encode data ambiguously, and
// though we mostly have the decoding right, it's brittle and unsafe.  This
// should be considered append only. There's a ton of dangerous things in this,
// and we really need to move off it, but for now we're here. Do not change
// existing encodings - this will break everything.
let rec unsafeDvalOfJsonV0 (json : JsonElement) : Dval =
  fstodo "unsafeDvalOfJsonV0"
//   (* sort so this isn't key-order-dependent. *)
//   let json = Yojson.Safe.sort json in
//   match json with
//   | `Int i ->
//       DInt (Dint.of_int i)
//   | `Intlit i ->
//       DInt (Dint.of_string_exn i)
//   | `Float f ->
//       DFloat f
//   | `Bool b ->
//       DBool b
//   | `Null ->
//       DNull
//   | `String s ->
//       dstr_of_string_exn s
//   | `List l ->
//       (* We shouldnt have saved dlist that have incompletes or error rails but we might have *)
//       to_list (List.map ~f:unsafe_dval_of_yojson_v0 l)
//   | `Variant v ->
//       Exception.internal "We dont use variants"
//   | `Tuple v ->
//       Exception.internal "We dont use tuples"
//   | `Assoc [("type", `String "response"); ("value", `List [a; b])] ->
//       DResp
//         (Result.ok_or_failwith (dhttp_of_yojson a), unsafe_dval_of_yojson_v0 b)
//   | `Assoc
//       [ ("constructor", `String constructor)
//       ; ("type", `String tipe)
//       ; ("values", `List vs) ] ->
//       let expectOne ~f vs =
//         match vs with
//         | [v] ->
//             f v
//         | _ ->
//             DObj (unsafe_dvalmap_of_yojson_v0 json)
//       in
//       ( match (tipe, constructor) with
//       | "result", "Ok" ->
//           vs
//           |> expectOne ~f:(fun v ->
//                  DResult (ResOk (unsafe_dval_of_yojson_v0 v)))
//       | "result", "Error" ->
//           vs
//           |> expectOne ~f:(fun v ->
//                  DResult (ResError (unsafe_dval_of_yojson_v0 v)))
//       | _ ->
//           DObj (unsafe_dvalmap_of_yojson_v0 json) )
//   | `Assoc [("type", `String tipe); ("value", `Null)] ->
//     ( match tipe with
//     | "incomplete" ->
//         DIncomplete SourceNone
//     | "option" ->
//         DOption OptNothing
//     | "block" ->
//         (* See docs/dblock-serialization.ml *)
//         DBlock
//           { body = EBlank (id_of_int 56789)
//           ; symtable = DvalMap.empty
//           ; params = [] }
//     | "errorrail" ->
//         DErrorRail DNull
//     | _ ->
//         DObj (unsafe_dvalmap_of_yojson_v0 json) )
//   | `Assoc [("type", `String tipe); ("value", `String v)] ->
//     ( match tipe with
//     | "date" ->
//         DDate (Util.date_of_isostring v)
//     | "title" ->
//         Exception.internal "Deprecated type"
//     | "url" ->
//         Exception.internal "Deprecated type"
//     | "error" ->
//         DError (SourceNone, v)
//     | "password" ->
//         v |> B64.decode |> Bytes.of_string |> DPassword
//     | "datastore" ->
//         DDB v
//     | "uuid" ->
//         DUuid (Uuidm.of_string v |> Option.value_exn)
//     | "char" | "character" ->
//         DCharacter (Unicode_string.Character.unsafe_of_string v)
//     | "bytes" ->
//         DBytes (v |> B64.decode |> RawBytes.of_string)
//     | _ ->
//         DObj (unsafe_dvalmap_of_yojson_v0 json) )
//   | `Assoc [("type", `String "option"); ("value", dv)] ->
//       DOption (OptJust (unsafe_dval_of_yojson_v0 dv))
//   | `Assoc [("type", `String "errorrail"); ("value", dv)] ->
//       DErrorRail (unsafe_dval_of_yojson_v0 dv)
//   | `Assoc _ ->
//       DObj (unsafe_dvalmap_of_yojson_v0 json)
//
//
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

let rec unsafeDvalOfJsonV1 (json : JsonElement) : Dval =
  fstodo "unsafeDvalOfJsonV1"
  (* sort so this isn't key-order-dependent. *)
//   fstodo "sort"
//   // let json = Yojson.Safe.sort json in
//   match json with
//   | J.JsonValue.Number d ->
//       let str = d.ToString()
//
//       if String.includes "." str then
//         str |> System.Double.Parse |> DFloat
//       else
//         str |> parseBigint |> DInt
//   | J.JsonValue.Float b -> DFloat b
//   | J.JsonValue.Boolean b -> DBool b
//   | J.JsonValue.Null -> DNull
//   | J.JsonValue.String s -> DStr s
//   | J.JsonValue.Array l ->
//       // We shouldnt have saved dlist that have incompletes or error rails but we might have
//       l |> Array.map unsafeDvalOfJsonV1 |> Array.toList |> Dval.list
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "response");
//                           ("value", J.JsonValue.Array [| a; b |]) |] ->
//       let dhttp =
//         match a with
//         | J.JsonValue.Array [| J.JsonValue.String "Redirect"; J.JsonValue.String url |] ->
//             Redirect url
//         | J.JsonValue.Array [| J.JsonValue.String "Response"; J.JsonValue.Number i;
//                                J.JsonValue.Array headers |] ->
//             let headers =
//               headers
//               |> Array.map
//                    (function
//                    | J.JsonValue.Array [| J.JsonValue.String k; J.JsonValue.String v |] ->
//                        (k, v)
//                    | _ -> failwith "invalid dresponse header")
//               |> Array.toList
//
//             Response(int i, headers)
//         | _ -> failwith "invalid response json"
//
//       DHttpResponse(dhttp, unsafeDvalOfJsonV1 b)
//   | J.JsonValue.Record [| ("constructor", J.JsonValue.String constructor);
//                           ("type", J.JsonValue.String tipe);
//                           ("values", J.JsonValue.Array vs) |] ->
//       let expectOne f vs =
//         match vs with
//         | [| v |] -> f v
//         | _ -> DObj(unsafeDvalmapOfJsonV1 json)
//
//       (match (tipe, constructor) with
//        | "result", "Ok" ->
//            vs |> expectOne (fun v -> DResult(Ok(unsafeDvalOfJsonV1 v)))
//        | "result", "Error" ->
//            vs |> expectOne (fun v -> DResult(Error(unsafeDvalOfJsonV1 v)))
//        | _ -> DObj(unsafeDvalmapOfJsonV1 json))
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "incomplete");
//                           ("value", J.JsonValue.Null) |] ->
//       DIncomplete SourceNone
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "option"); ("value", dv) |] ->
//       if dv = J.JsonValue.Null then
//         DOption None
//       else
//         DOption(Some(unsafeDvalOfJsonV1 dv))
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "block");
//                           ("value", J.JsonValue.Null) |] ->
//       (* See docs/dblock-serialization.ml *)
//       DFnVal(
//         Lambda { body = EBlank(id 23456); symtable = Map.empty; parameters = [] }
//       )
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "errorrail"); ("value", dv) |] ->
//       DErrorRail(unsafeDvalOfJsonV1 dv)
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "date");
//                           ("value", J.JsonValue.String v) |] ->
//       DDate(System.DateTime.ofIsoString v)
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "error");
//                           ("value", J.JsonValue.String v) |] -> Dval.errStr v
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "password");
//                           ("value", J.JsonValue.String v) |] ->
//       fstodo "support password"
//   // v |> base64Decode |> toBytes |> DPassword
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "datastore");
//                           ("value", J.JsonValue.String v) |] -> DDB v
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "uuid");
//                           ("value", J.JsonValue.String v) |] -> DUuid(System.Guid v)
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "char");
//                           ("value", J.JsonValue.String v) |]
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "character");
//                           ("value", J.JsonValue.String v) |] ->
//       v |> String.toEgcSeq |> Seq.head |> DChar
//   | J.JsonValue.Record [| ("type", J.JsonValue.String "bytes");
//                           ("value", J.JsonValue.String v) |] ->
//       v |> base64Decode |> toBytes |> DBytes
//   | J.JsonValue.Record _ -> DObj(unsafeDvalmapOfJsonV1 json)
//
// and unsafeDvalmapOfJsonV1 (j : J.JsonValue) : DvalMap =
//   match j with
//   | J.JsonValue.Record records ->
//       Array.fold
//         Map.empty
//         (fun m (k, v) -> Map.add k (unsafeDvalOfJsonV1 v) m)
//         records
//   | _ -> failwith "Not a json object"

let rec unsafeDvalToJsonValueV0 (w : Utf8JsonWriter) (redact : bool) (dv : Dval) : unit =
  let writeValue = unsafeDvalToJsonValueV0 w redact
  let writeField = unsafeDvalToJsonFieldV0 w redact
  let rec wrapStrValue (typ : DType) (str : string) =
    w.WriteStartObject ()
    w.WriteString("type", unsafeDTypeToJson typ)
    w.WriteString("value", str)
    w.WriteEndObject ()
  and wrapNestedDvalValue (typ : DType) (dv : Dval) =
    w.WriteStartObject ()
    w.WriteString("type", unsafeDTypeToJson  typ)
    writeField "value" dv
    w.WriteEndObject ()
  match dv with
  (* basic types *)
  | DInt i -> w.WriteNumberValue (decimal i)
  | DFloat f -> writeFloat w f
  | DBool b -> w.WriteBooleanValue b
  | DNull -> w.WriteNullValue ()
  | DStr s -> w.WriteStringValue s
  | DList l ->
      w.WriteStartArray ()
      List.iter writeValue l
      w.WriteEndArray ()
  | DObj o ->
      w.WriteStartObject ()
      Map.iter (fun k v -> writeField k v) o
      w.WriteEndObject ()
  | DFnVal _ ->
      (* See docs/dblock-serialization.ml *)
      w.WriteNullValue ()
  | DIncomplete _ -> w.WriteNullValue ()
  | DChar c -> w.WriteStringValue c
  | DError (_, msg) ->
      w.WriteStartObject ()
      writeField "Error" (DStr msg)
      w.WriteEndObject ()
  | DHttpResponse (h, hdv : Dval) ->
      w.WriteStartArray ()
      match h with
      | Redirect str ->
          w.WriteStartArray ()
          w.WriteStringValue "Redirect"
          w.WriteStringValue str
          w.WriteEndArray ()
      | Response (code, headers) ->
          w.WriteStartArray ()
          w.WriteStringValue "Response"
          w.WriteNumberValue code
          w.WriteStartArray ()
          List.iter (fun (k : string, v : string) ->
            w.WriteStartArray ()
            w.WriteStringValue k
            w.WriteStringValue v
            w.WriteEndArray ()) headers
          w.WriteEndArray ()
          w.WriteEndArray ()
      // value object
      wrapNestedDvalValue (Dval.toType dv) hdv
      w.WriteEndArray ()
  | DDB dbname -> wrapStrValue (TDB TAny) dbname
  | DDate date -> wrapStrValue TDate (date.toIsoString ())
  // | DPassword hashed ->
  //     if redact then
  //       wrap_user_type J.nil
  //     else
  //       hashed |> Bytes.to_string |> B64.encode |> wrap_user_str
  | DUuid uuid -> wrapStrValue TUuid (uuid.ToString())
  | DOption opt ->
      (match opt with
       | None -> wrapStrValue (Dval.toType dv) "null"
       | Some ndv -> wrapNestedDvalValue (Dval.toType dv) ndv)
  | DErrorRail erdv ->
      wrapNestedDvalValue (Dval.toType dv) erdv
  | DResult res ->
      (match res with
       | Ok rdv ->
          w.WriteStartObject ()
          w.WriteString("type", dv |> Dval.toType |> unsafeDTypeToJson)
          w.WriteString("constructor", "Ok")
          w.WriteStartArray "values"
          writeValue rdv
          w.WriteEndArray ()
       | Error rdv ->
          w.WriteStartObject ()
          w.WriteString("type", dv |> Dval.toType |> unsafeDTypeToJson)
          w.WriteString("constructor", "Error")
          w.WriteStartArray "values"
          writeValue rdv
          w.WriteEndArray ())
  | DBytes bytes -> wrapStrValue TBytes (System.Convert.ToBase64String bytes)


and unsafeDvalToJsonFieldV0 (w : Utf8JsonWriter) (redact : bool) (field : string) (dv : Dval) : unit =
  let writeValue = unsafeDvalToJsonValueV0 w redact
  let writeField = unsafeDvalToJsonFieldV0 w redact
  let wrapStrInObj (field : string) (typ : DType) (str : string) =
    w.WriteStartObject field
    w.WriteString("type", unsafeDTypeToJson typ)
    w.WriteString("value", str)
    w.WriteEndObject ()
  let wrapNestedDvalInObj (field : string) (typ : DType) (dv : Dval) =
    w.WriteStartObject field
    w.WriteString("type", unsafeDTypeToJson typ)
    writeField "value" dv
    w.WriteEndObject ()
  let wrapNestedDvalValue (typ : DType) (dv : Dval) =
    w.WriteStartObject ()
    w.WriteString("type", unsafeDTypeToJson typ)
    writeField "value" dv
    w.WriteEndObject ()
  match dv with
  (* basic types *)
  | DInt i -> w.WriteNumber(field, (decimal i))
  | DFloat f -> writeFloatProperty w field f
  | DBool b -> w.WriteBoolean(field, b)
  | DNull -> w.WriteNull field
  | DStr s -> w.WriteString(field, s)
  | DList l ->
      w.WriteStartArray field
      List.iter writeValue l
      w.WriteEndArray ()
  | DObj o ->
      w.WriteStartObject field
      Map.iter (fun k v -> writeField k v) o
      w.WriteEndObject ()
  | DFnVal _ ->
      (* See docs/dblock-serialization.ml *)
      w.WriteNull field
  | DIncomplete _ -> w.WriteNull field
  | DChar c -> w.WriteString(field, c)
  | DError (_, msg) ->
      w.WriteStartObject field
      writeField "Error" (DStr msg)
      w.WriteEndObject ()
  | DHttpResponse (h, hdv : Dval) ->
      w.WriteStartArray field
      match h with
      | Redirect str ->
          w.WriteStartArray ()
          w.WriteStringValue "Redirect"
          w.WriteStringValue str
          w.WriteEndArray ()
      | Response (code, headers) ->
          w.WriteStartArray ()
          w.WriteStringValue "Response"
          w.WriteNumberValue code
          w.WriteStartArray ()
          List.iter (fun (k : string, v : string) ->
            w.WriteStartArray ()
            w.WriteStringValue k
            w.WriteStringValue v
            w.WriteEndArray ()) headers
          w.WriteEndArray ()
          w.WriteEndArray ()
      // value object
      wrapNestedDvalValue (Dval.toType dv) hdv
      w.WriteEndArray ()
  | DDB dbname -> wrapStrInObj field (TDB TAny) dbname
  | DDate date -> wrapStrInObj field TDate (date.toIsoString ())
  // | DPassword hashed ->
  //     if redact then
  //       wrap_user_type J.nil
  //     else
  //       hashed |> Bytes.to_string |> B64.encode |> wrap_user_str
  | DUuid uuid -> wrapStrInObj field TUuid (uuid.ToString())
  | DOption opt ->
      (match opt with
       | None -> wrapStrInObj field (Dval.toType dv) "null"
       | Some ndv -> wrapNestedDvalInObj field (Dval.toType dv) ndv)
  | DErrorRail erdv ->
      wrapNestedDvalInObj field (Dval.toType dv) erdv
  | DResult res ->
      (match res with
       | Ok rdv ->
          w.WriteStartObject field
          w.WriteString("type", dv |> Dval.toType |> unsafeDTypeToJson)
          w.WriteString("constructor", "Ok")
          w.WriteStartArray "values"
          writeValue rdv
          w.WriteEndArray ()
       | Error rdv ->
          w.WriteStartObject field
          w.WriteString("type", dv |> Dval.toType |> unsafeDTypeToJson)
          w.WriteString("constructor", "Error")
          w.WriteStartArray "values"
          writeValue rdv
          w.WriteEndArray ())
  | DBytes bytes -> wrapStrInObj field TBytes (System.Convert.ToBase64String bytes)


let unsafeDvalToJsonValueV1 (w : Utf8JsonWriter) (redact : bool) (dv : Dval) : unit =
  unsafeDvalToJsonValueV0 w redact dv

(* ------------------------- *)
(* Roundtrippable - for events and traces *)
(* ------------------------- *)
let toInternalRoundtrippableV0 (dval : Dval) : string =
  writeJson (fun w -> unsafeDvalToJsonValueV1 w false dval)


let ofInternalRoundtrippableJsonV0 (j : JsonElement) : Result<Dval, string> =
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
  writeJson (fun w ->
    w.WriteStartObject ()
    dvalMap
    |> Map.toList
    |> List.iter (fun (k, dval) -> (unsafeDvalToJsonFieldV0 w false k dval))
    w.WriteEndObject ()
   )

let ofInternalQueryableV1 (str : string) : Dval =
  // The first level _must_ be an object at the moment
  let rec convertTopLevel (json : JsonElement) : Dval =
    match json.ValueKind with
    | JsonValueKind.Object -> convert json
    | _ -> failwith "Value that isn't an object"

  and convert (json : JsonElement) : Dval =
    match json.ValueKind with
    | JsonValueKind.Number ->
        let str = json.GetRawText ()
        if String.includes "." str then
          str |> System.Double.Parse |> DFloat
        else
          str |> parseBigint |> DInt
    | JsonValueKind.True -> DBool true
    | JsonValueKind.False -> DBool false
    | JsonValueKind.Null -> DNull
    | JsonValueKind.Undefined -> DNull
    | JsonValueKind.String -> DStr (json.GetString ())
    | JsonValueKind.Array ->
       // We shouldnt have saved dlist that have incompletes or error rails but we might have
        seq (json.EnumerateArray()) |> Seq.toList  |> List.map convert |> Dval.list
    | JsonValueKind.Object ->
        let fields =
          seq (json.EnumerateObject()) |> Seq.toList  |> List.map (fun jp -> (jp.Name, convert jp.Value)) |> List.sortBy (fun (k, _) -> k)
        // These are the only types that are allowed in the queryable
        // representation. We may allow more in the future, but the real thing to
        // do is to use the DB's type and version to encode/decode them correctly
        match fields with
        | [ ("type", DStr "date"); ("value", DStr v) ] -> DDate(System.DateTime.ofIsoString v)
        | [ ("type", DStr "password"); ("value", DStr v) ] -> fstodo "password"
        // v |> B64.decode |> Bytes.of_string |> DPassword
        | [ ("type", DStr "uuid"); ("value", DStr v) ] -> DUuid(System.Guid v)
        | _ -> fields |> Map.ofList |> DObj
    | _ -> DNull // enums make take values outside known cases
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
