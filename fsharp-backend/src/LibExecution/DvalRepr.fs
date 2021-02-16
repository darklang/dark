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

module WriteJson =
  open FSharp.Data
  // This code is copied from
  // https://github.com/fsprojects/FSharp.Data/blob/4697bfdf4e5ee9d761695f8432a944ae92947143/src/Json/JsonValue.fs

  // The default FSharp.Data.Json serializer prints doubles as ints when they
  // have a 0 fractional. this breaks our roundtripping as we'll then read
  // those as ints. Since it feels like this is a bug in FSharp.Data.Json, the
  // code is copied and hacked (seee "HACK" below).

  let writeJson (x : JsonValue, w : System.IO.TextWriter, saveOptions) =

    let newLine =
      if saveOptions = JsonSaveOptions.None then
        fun indentation plus ->
          w.WriteLine()
          System.String(' ', indentation + plus) |> w.Write
      else
        fun _ _ -> ()

    let propSep = if saveOptions = JsonSaveOptions.None then "\": " else "\":"


    // Encode characters that are not valid in JS string. The implementation is based
    // on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
    let JsonStringEncodeTo (w : System.IO.TextWriter) (value : string) =
      if not (System.String.IsNullOrEmpty value) then
        for i = 0 to value.Length - 1 do
          let c = value.[i]
          let ci = int c

          if ci >= 0 && ci <= 7 || ci = 11 || ci >= 14 && ci <= 31 then
            w.Write("\\u{0:x4}", ci) |> ignore
          else
            match c with
            | '\b' -> w.Write "\\b"
            | '\t' -> w.Write "\\t"
            | '\n' -> w.Write "\\n"
            | '\f' -> w.Write "\\f"
            | '\r' -> w.Write "\\r"
            | '"' -> w.Write "\\\""
            | '\\' -> w.Write "\\\\"
            | _ -> w.Write c

    let rec serialize indentation : JsonValue -> unit =
      function
      | JsonValue.Null -> w.Write "null"
      | JsonValue.Boolean b -> w.Write((if b then "true" else "false"))
      | JsonValue.Number number -> w.Write number
      | JsonValue.Float v when System.Double.IsInfinity v || System.Double.IsNaN v ->
          w.Write "null"

      // HACK This is the only thing that differs from the default HACK
      | JsonValue.Float number ->
          w.Write(number.ToString("0.0#####################"))
      | JsonValue.String s ->
          w.Write "\""
          JsonStringEncodeTo w s
          w.Write "\""
      | JsonValue.Record properties ->
          w.Write "{"

          for i = 0 to properties.Length - 1 do
            let k, v = properties.[i]
            if i > 0 then w.Write ","
            newLine indentation 2
            w.Write "\""
            JsonStringEncodeTo w k
            w.Write propSep
            serialize (indentation + 2) v

          newLine indentation 0
          w.Write "}"
      | JsonValue.Array elements ->
          w.Write "["

          for i = 0 to elements.Length - 1 do
            if i > 0 then w.Write ","
            newLine indentation 2
            serialize (indentation + 2) elements.[i]

          if elements.Length > 0 then newLine indentation 0
          w.Write "]"

    serialize 0 x


module J =
  open FSharp.Data
  type JsonValue = FSharp.Data.JsonValue

  let bigint (i : bigint) : JsonValue = JsonValue.Number(decimal i)
  let string (s : string) : JsonValue = JsonValue.String s
  let int (i : int) : JsonValue = JsonValue.Number(decimal i)
  let int64 (i : int64) : JsonValue = JsonValue.Number(decimal i)
  let float (f : float) : JsonValue = JsonValue.Float f
  let bool (b : bool) : JsonValue = JsonValue.Boolean b
  let nil = JsonValue.Null
  let list (l : JsonValue list) : JsonValue = l |> List.toArray |> JsonValue.Array
  let array (a : JsonValue array) : JsonValue = a |> JsonValue.Array
  let variant (name : string) (args : JsonValue list) = (string name :: args) |> list

  let object (r : (string * JsonValue) list) : JsonValue =
    r |> List.toArray |> JsonValue.Record

  let toString (j : JsonValue) : string =
    let w = new System.IO.StringWriter()
    WriteJson.writeJson (j, w, JsonSaveOptions.DisableFormatting)
    w.ToString()

  let toPrettyString (j : JsonValue) = j.ToString(JsonSaveOptions.None)
  let parse (str : string) : JsonValue = JsonValue.Parse str

// -------------------------
// Runtime Types
// -------------------------
let rec dtypeToString (t : DType) : string =
  // TODO CLEANUP its unclear the full scope of what we use these for, and the
  // impact of cleaning up the terrible names here
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
    | DFloat f -> f.ToString()
    | DChar c -> c
    | DNull -> "null"
    | DDate d -> d.toIsoString ()
    | DUuid uuid -> uuid.ToString()
    | DDB dbname -> $"<DB: {dbname}>"
    | DFakeVal (DError _) ->
        // FSTODO make this a string again
        "Error: TODO: print message"
    | DFakeVal (DIncomplete _) -> "<Incomplete>"
    | DFnVal _ ->
        // See docs/dblock-serialization.ml
        "<Block>"
    // | DPassword _ ->
    //     (* redacting, do not unredact *)
    //     "<Password>"
    | DObj _
    | DList _ -> toNestedString nestedreprfn dv
    | DFakeVal (DErrorRail d) ->
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

let toPrettyMachineJsonValueV1 dval : FSharp.Data.JsonValue =
  let rec r dv =
    match dv with
    (* basic types *)
    | DInt i -> J.bigint i
    | DFloat f -> J.float f
    | DBool b -> J.bool b
    | DNull -> J.nil
    | DStr s -> J.string s
    | DList l -> J.list (List.map r l)
    | DObj o -> o |> Map.toList |> List.map (fun (k, v) -> (k, r v)) |> J.object
    | DFnVal _ ->
        (* See docs/dblock-serialization.ml *)
        J.nil
    | DFakeVal (DIncomplete _) -> J.nil
    | DChar c -> J.string c
    | DFakeVal (DError _) ->
        // FSTODO
        J.object [ "Error", J.string "TODO: error" ]
    | DHttpResponse (h, response) -> r response
    | DDB dbname -> J.string dbname
    | DDate date -> J.string (date.toIsoString ())
    // | DPassword hashed ->
    //     `Assoc [("Error", `String "Password is redacted")]
    | DUuid uuid -> J.string (uuid.ToString())
    | DOption opt -> Option.map r opt |> Option.defaultValue J.nil
    | DFakeVal (DErrorRail dv) -> r dv
    | DResult res ->
        (match res with
         | Ok dv -> r dv
         | Error dv -> J.object [ "Error", r dv ])
    | DBytes bytes ->
        // FSTODO is this the right b64 encoding
        bytes |> System.Convert.ToBase64String |> J.string

  r dval

// member this.toJSON() : J.JsonValue =
//   let rec encodeDval (dv : Dval) : J.JsonValue =
//     let encodeWithType name value = J.object [ name, J.string value ]
//     match dv with
//     | DInt i -> J.bigint i
//     | DChar c -> J.string c
//     | DFloat d -> J.float d
//     | DStr str -> J.string str
//     | DNull -> J.nil
//     | DList l -> l |> List.map encodeDval |> J.list
//     | DBool b -> J.bool b
//     | DBytes bytes -> bytes |> System.Text.Encoding.ASCII.GetString |> J.string
//     | DUuid uuid -> uuid.ToString() |> J.string
//     | DFnVal _ -> J.nil
//     | DFakeVal (DError (e)) -> J.object [ "error", J.string (e.ToString()) ]
//     | DFakeVal (DIncomplete (_)) -> J.object [ "incomplete", J.nil ]
//     | DFakeVal (DErrorRail (value)) -> J.object [ "errorrail", encodeDval value ]
//     | DObj obj ->
//         obj |> Map.toList |> List.map (fun (k, v) -> k, encodeDval v) |> J.object
//     | DDB name -> encodeWithType "db" name
//     | DHttpResponse _ -> J.string "FSTODO: DResp"
//     | DOption (Some dv) -> encodeDval dv
//     | DOption (None) -> J.nil
//     | DResult (Ok dv) -> encodeDval dv
//     | DResult (Error dv) -> J.object [ "error", encodeDval dv ]
//
//   encodeDval this
//
//


let toPrettyMachineJsonV1 dval : string =
  dval |> toPrettyMachineJsonValueV1 |> J.toPrettyString



// (* The "unsafe" variations here are bad. They encode data ambiguously, and
//  * though we mostly have the decoding right, it's brittle and unsafe.  This
//  * should be considered append only. There's a ton of dangerous things in this,
//  * and we really need to move off it, but for now we're here. Do not change
//  * existing encodings - this will break everything.
//  *)
// let rec unsafe_dval_of_yojson_v0 (json : Yojson.Safe.t) : dval =
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

let rec unsafeDvalOfJsonV1 (json : J.JsonValue) : Dval =
  (* sort so this isn't key-order-dependent. *)
  fstodo "sort"
  // let json = Yojson.Safe.sort json in
  match json with
  | J.JsonValue.Number d ->
      let str = d.ToString()

      if String.includes "." str then
        str |> System.Double.Parse |> DFloat
      else
        str |> parseBigint |> DInt
  | J.JsonValue.Float b -> DFloat b
  | J.JsonValue.Boolean b -> DBool b
  | J.JsonValue.Null -> DNull
  | J.JsonValue.String s -> DStr s
  | J.JsonValue.Array l ->
      // We shouldnt have saved dlist that have incompletes or error rails but we might have
      l |> Array.map unsafeDvalOfJsonV1 |> Array.toList |> Dval.list
  | J.JsonValue.Record [| ("type", J.JsonValue.String "response");
                          ("value", J.JsonValue.Array [| a; b |]) |] ->
      let dhttp =
        match a with
        | J.JsonValue.Array [| J.JsonValue.String "Redirect"; J.JsonValue.String url |] ->
            Redirect url
        | J.JsonValue.Array [| J.JsonValue.String "Response"; J.JsonValue.Number i;
                               J.JsonValue.Array headers |] ->
            let headers =
              headers
              |> Array.map
                   (function
                   | J.JsonValue.Array [| J.JsonValue.String k; J.JsonValue.String v |] ->
                       (k, v)
                   | _ -> failwith "invalid dresponse header")
              |> Array.toList

            Response(int i, headers)
        | _ -> failwith "invalid response json"

      DHttpResponse(dhttp, unsafeDvalOfJsonV1 b)
  | J.JsonValue.Record [| ("constructor", J.JsonValue.String constructor);
                          ("type", J.JsonValue.String tipe);
                          ("values", J.JsonValue.Array vs) |] ->
      let expectOne f vs =
        match vs with
        | [| v |] -> f v
        | _ -> DObj(unsafeDvalmapOfJsonV1 json)

      (match (tipe, constructor) with
       | "result", "Ok" ->
           vs |> expectOne (fun v -> DResult(Ok(unsafeDvalOfJsonV1 v)))
       | "result", "Error" ->
           vs |> expectOne (fun v -> DResult(Error(unsafeDvalOfJsonV1 v)))
       | _ -> DObj(unsafeDvalmapOfJsonV1 json))
  | J.JsonValue.Record [| ("type", J.JsonValue.String "incomplete");
                          ("value", J.JsonValue.Null) |] ->
      DFakeVal(DIncomplete SourceNone)
  | J.JsonValue.Record [| ("type", J.JsonValue.String "option"); ("value", dv) |] ->
      if dv = J.JsonValue.Null then
        DOption None
      else
        DOption(Some(unsafeDvalOfJsonV1 dv))
  | J.JsonValue.Record [| ("type", J.JsonValue.String "block");
                          ("value", J.JsonValue.Null) |] ->
      (* See docs/dblock-serialization.ml *)
      DFnVal(
        Lambda { body = EBlank(id 23456); symtable = Map.empty; parameters = [] }
      )
  | J.JsonValue.Record [| ("type", J.JsonValue.String "errorrail"); ("value", dv) |] ->
      DFakeVal(DErrorRail(unsafeDvalOfJsonV1 dv))
  | J.JsonValue.Record [| ("type", J.JsonValue.String "date");
                          ("value", J.JsonValue.String v) |] ->
      DDate(System.DateTime.ofIsoString v)
  | J.JsonValue.Record [| ("type", J.JsonValue.String "error");
                          ("value", J.JsonValue.String v) |] -> Dval.errStr v
  | J.JsonValue.Record [| ("type", J.JsonValue.String "password");
                          ("value", J.JsonValue.String v) |] ->
      fstodo "support password"
  // v |> base64Decode |> toBytes |> DPassword
  | J.JsonValue.Record [| ("type", J.JsonValue.String "datastore");
                          ("value", J.JsonValue.String v) |] -> DDB v
  | J.JsonValue.Record [| ("type", J.JsonValue.String "uuid");
                          ("value", J.JsonValue.String v) |] -> DUuid(System.Guid v)
  | J.JsonValue.Record [| ("type", J.JsonValue.String "char");
                          ("value", J.JsonValue.String v) |]
  | J.JsonValue.Record [| ("type", J.JsonValue.String "character");
                          ("value", J.JsonValue.String v) |] ->
      v |> String.toEgcSeq |> Seq.head |> DChar
  | J.JsonValue.Record [| ("type", J.JsonValue.String "bytes");
                          ("value", J.JsonValue.String v) |] ->
      v |> base64Decode |> toBytes |> DBytes
  | J.JsonValue.Record _ -> DObj(unsafeDvalmapOfJsonV1 json)

and unsafeDvalmapOfJsonV1 (j : J.JsonValue) : DvalMap =
  match j with
  | J.JsonValue.Record records ->
      Array.fold
        Map.empty
        (fun m (k, v) -> Map.add k (unsafeDvalOfJsonV1 v) m)
        records
  | _ -> failwith "Not a json object"

let rec unsafeDvalToJsonV0 (redact : bool) (dv : Dval) : J.JsonValue =
  let tipe = dv |> dtypeName |> J.string in
  let wrapUserType value = J.object [ ("type", tipe); ("value", value) ] in

  let wrapConstructedType cons values =
    J.object [ ("type", tipe); ("constructor", cons); ("values", J.list values) ]

  let wrapUserStr value = wrapUserType (J.string value) in

  match dv with
  (* basic types *)
  | DInt i -> J.bigint i
  | DFloat f -> J.float f
  | DBool b -> J.bool b
  | DNull -> J.nil
  | DStr s -> J.string s
  | DList l -> J.list (List.map (unsafeDvalToJsonV0 redact) l)
  | DObj o ->
      o
      |> Map.toList
      |> List.map (fun (k, v) -> (k, unsafeDvalToJsonV0 redact v))
      |> fun x -> J.object x
  | DFnVal _ ->
      (* See docs/dblock-serialization.ml *)
      wrapUserType J.nil
  | DFakeVal (DIncomplete _) -> wrapUserType J.nil
  | DChar c -> wrapUserStr (c.ToString())
  | DFakeVal (DError (msg, _)) -> wrapUserStr (msg.ToString())
  | DHttpResponse (h, hdv) ->
      let http =
        match h with
        | Redirect str -> J.list [ J.string "Redirect"; J.string str ]
        | Response (code, headers) ->
            J.list [ J.string "Response"
                     J.int code
                     J.list (
                       List.map
                         (fun (k, v) -> J.list [ J.string k; J.string v ])
                         headers
                     ) ]

      wrapUserType (J.list [ http; unsafeDvalToJsonV0 redact hdv ])
  | DDB dbname -> wrapUserStr dbname
  | DDate date -> wrapUserStr (date.toIsoString ())
  // | DPassword hashed ->
  //     if redact then
  //       wrap_user_type J.nil
  //     else
  //       hashed |> Bytes.to_string |> B64.encode |> wrap_user_str
  | DUuid uuid -> wrapUserStr (uuid.ToString())
  | DOption opt ->
      (match opt with
       | None -> wrapUserType J.nil
       | Some dv -> wrapUserType (unsafeDvalToJsonV0 redact dv))
  | DFakeVal (DErrorRail dv) -> wrapUserType (unsafeDvalToJsonV0 redact dv)
  | DResult res ->
      (match res with
       | Ok dv ->
           wrapConstructedType (J.string "Ok") [ unsafeDvalToJsonV0 redact dv ]
       | Error dv ->
           wrapConstructedType (J.string "Error") [ unsafeDvalToJsonV0 redact dv ])
  | DBytes bytes -> bytes |> ofBytes |> base64Encode |> wrapUserStr

let unsafeDvalToJsonV1 (redact : bool) (dv : Dval) : J.JsonValue =
  unsafeDvalToJsonV0 redact dv

(* ------------------------- *)
(* Roundtrippable - for events and traces *)
(* ------------------------- *)
let toInternalRoundtrippableV0 (dval : Dval) : string =
  unsafeDvalToJsonV1 false dval |> J.toString


let ofInternalRoundtrippableJsonV0 (j : J.JsonValue) : Result<Dval, string> =
  (* Switched to v1 cause it was a bug fix *)
  try
    unsafeDvalOfJsonV1 j |> Ok
  with e -> Error(e.ToString())


let ofInternalRoundtrippableV0 str : Dval = str |> J.parse |> unsafeDvalOfJsonV1


// -------------------------
// Queryable - for the DB *)
// -------------------------
//
let toInternalQueryableV0 (dval : Dval) : string =
  dval |> unsafeDvalToJsonV0 false |> J.toString


let ofInternalQueryableV0 (str : string) : Dval =
  str |> J.parse |> unsafeDvalOfJsonV1


let toInternalQueryableFieldJsonV1 (dval : Dval) : J.JsonValue =
  dval |> unsafeDvalToJsonV0 false


let toInternalQueryableFieldV1 (dval : Dval) : string =
  dval |> toInternalQueryableFieldJsonV1 |> J.toString


let toInternalQueryableV1 (dvalMap : DvalMap) : string =
  dvalMap
  |> Map.toList
  |> List.map (fun (k, dval) -> (k, dval |> toInternalQueryableFieldJsonV1))
  |> fun l -> J.object l |> J.toString


let ofInternalQueryableV1 (str : string) : Dval =
  // The first level _must_ be an object at the moment
  let rec convertTopLevel (json : J.JsonValue) : Dval =
    match json with
    | J.JsonValue.Record rows ->
        DObj(Array.fold Map.empty (fun m (k, v) -> Map.add k (convert v) m) rows)
    | _ -> failwith "Value that isn't an object"

  and convert (json : J.JsonValue) : Dval =
    // sort so this isn't key-order-dependent.
    // FSTODO
    // let json = Yojson.Safe.sort json in
    match json with
    | J.JsonValue.Number d ->
        let str = d.ToString()

        if String.includes "." str then
          str |> System.Double.Parse |> DFloat
        else
          str |> parseBigint |> DInt
    | J.JsonValue.Float b -> DFloat b
    | J.JsonValue.Boolean b -> DBool b
    | J.JsonValue.Null -> DNull
    | J.JsonValue.String s -> DStr s
    | J.JsonValue.Array l ->
        // We shouldnt have saved dlist that have incompletes or error rails but we might have
        l |> Array.map convert |> Array.toList |> Dval.list
    // These are the only types that are allowed in the queryable
    // representation. We may allow more in the future, but the real thing to
    // do is to use the DB's type and version to encode/decode them correctly
    | J.JsonValue.Record [| ("type", J.JsonValue.String "date");
                            ("value", J.JsonValue.String v) |] ->
        DDate(System.DateTime.ofIsoString v)
    | J.JsonValue.Record [| ("type", J.JsonValue.String "password");
                            ("value", J.JsonValue.String v) |] ->
        fstodo "support password"
    // v |> B64.decode |> Bytes.of_string |> DPassword
    | J.JsonValue.Record [| ("type", J.JsonValue.String "uuid");
                            ("value", J.JsonValue.String v) |] ->
        DUuid(System.Guid v)
    | J.JsonValue.Record _ as json -> convertTopLevel json

  str |> J.parse |> convertTopLevel

//
// (* ------------------------- *)
// (* Other formats *)
// (* ------------------------- *)
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
    | DFloat f -> f.ToString()
    | DNull -> "null"
    | DFnVal _ ->
        (* See docs/dblock-serialization.ml *)
        justtipe
    | DFakeVal (DIncomplete _) -> justtipe
    | DFakeVal (DError (_, msg)) -> wrap msg
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
    | DFakeVal (DErrorRail dv) -> "ErrorRail: " + toRepr_ indent dv
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
