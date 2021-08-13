module LibBackend.HttpClient

// HttpClient used by standard libraries

open Prelude
open LibExecution

module RT = RuntimeTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Http
open System.Net.Http.Json
open System.Net.Http.Headers

type KeyValuePair<'k, 'v> = System.Collections.Generic.KeyValuePair<'k, 'v>
type StringValues = Microsoft.Extensions.Primitives.StringValues


type Headers = (string * string) list

type HttpResult =
  { body : string
    code : int
    headers : Headers
    error : string
    httpVersion : string
    httpStatusMessage : string }

type ClientError = { url : string; error : string; code : int }

(* Given ~regex, return Err if it doesn't match, or list of captures if
 * it does. First elem of the list is the first capture, not the whole
 * match. *)
// let stringMatch (regex : string) (str : string) : Result<string list, string> =
// let reg = Re2.create_exn regex in

// str
// |> Re2.find_submatches reg
// |> Result.map Array.to_list
// |> Result.map List.tl_exn
// (* skip full match *)
// |> Result.map (List.map (Option.value ""))

type Charset =
  | Latin1
  | Utf8
  | Other


// let charset (headers : (string * string) list) : Charset =
//   let canonicalize s = s |> String.strip |> String.toLower in

//   headers
//   |> List.map (Tuple.T2.map_fst canonicalize)
//   |> List.map (Tuple.T2.map_snd canonicalize)
//   |> List.filter_map
//        (function
//        | "content-type", v ->
//          (match stringMatch ".*;\\s*charset=(.*)$" v with
//           | Result.Ok [ "utf-8" ] -> Some Utf8
//           | Result.Ok [ "utf8" ] -> Some Utf8
//           | Result.Ok [ "us-ascii" ] -> Some Latin1 (* should work *)
//           | Result.Ok [ "iso-8859-1" ] -> Some Latin1
//           | Result.Ok [ "iso_8859-1" ] -> Some Latin1
//           | Result.Ok [ "latin1" ] -> Some Latin1
//           | _ -> None)
//        | _ -> None)
//   |> List.head
//   |> Option.value Other

// -------------------------
// Forms and queries Functions
// -------------------------

// For putting into URLs as query params
// FSTODO: fuzz against OCaml
let rec dvalToUrlStringExn (dv : RT.Dval) : string =
  let r = dvalToUrlStringExn
  match dv with
  | RT.DFnVal _ ->
    (* See docs/dblock-serialization.ml *)
    "<block>"
  | RT.DIncomplete _ -> "<incomplete>"
  | RT.DPassword _ -> "<password>"
  | RT.DInt i -> string i
  | RT.DBool true -> "true"
  | RT.DBool false -> "false"
  | RT.DStr s -> s
  | RT.DFloat f -> DvalRepr.ocamlStringOfFloat f
  | RT.DChar c -> c
  | RT.DNull -> "null"
  | RT.DDate d -> d.toIsoString ()
  | RT.DDB dbname -> dbname
  | RT.DErrorRail d -> r d
  | RT.DError (_, msg : string) -> $"error={msg}"
  | RT.DUuid uuid -> string uuid
  | RT.DHttpResponse (RT.Redirect _) -> "null"
  | RT.DHttpResponse (RT.Response (_, _, hdv)) -> r hdv
  | RT.DList l -> "[ " + String.concat ", " (List.map r l) + " ]"
  | RT.DObj o ->
    let strs = Map.fold (fun l key value -> (key + ": " + r value) :: l) [] o
    "{ " + (String.concat ", " strs) + " }"
  | RT.DOption None -> "none"
  | RT.DOption (Some v) -> r v
  | RT.DResult (Error v) -> "error=" + r v
  | RT.DResult (Ok v) -> r v
  | RT.DBytes bytes -> base64Encode bytes

// FSTODO: fuzz this against OCAML
let dvalToQuery (dv : RT.Dval) : (string * string list) list =
  match dv with
  | RT.DObj kvs ->
    kvs
    |> Map.toList
    |> List.map
         (fun (k, value) ->
           match value with
           | RT.DNull -> (k, [])
           | RT.DList l -> (k, List.map dvalToUrlStringExn l)
           | _ -> (k, [ dvalToUrlStringExn value ]))
  | _ -> failwith "attempting to use non-object as query param" // CODE exception

// FSTODO: fuzz against OCaml
// https://secretgeek.net/uri_enconding
let dvalToFormEncoding (dv : RT.Dval) : string =
  dvalToQuery dv
  |> List.map (fun (k, v) -> $"{k}={v}")
  |> List.map System.Uri.EscapeDataString
  |> String.concat "&"

let queryToDval (queryString : string) : RT.Dval =
  let nvc = System.Web.HttpUtility.ParseQueryString queryString
  nvc.AllKeys
  |> Seq.map
       (fun key ->
         let values = nvc.GetValues key
         let value =
           let split =
             values.[values.Length - 1]
             |> FSharpPlus.String.split [| "," |]
             |> Seq.toList

           match split with
           | [] -> RT.DNull
           | [ "" ] -> RT.DNull // CLEANUP this should be a string
           | [ v ] -> RT.DStr v
           | list -> RT.DList(List.map RT.DStr list)

         if isNull key then
           // All the values with no key are by GetValues, so make each one a value
           values |> Array.toList |> List.map (fun k -> (k, RT.DNull))
         else
           [ (key, value) ])
  |> List.concat
  |> RT.Dval.obj

(* Servers should default to ISO-8859-1 (aka Latin-1) if nothing
 * provided. We ask for UTF-8, but might not get it. If we get
 * ISO-8859-1 we can transcode it using Uutf. Uutf supports more recent
 * unicode than camomile (10, vs 3.2). However, camomile supports many
 * more transcoding formats. So we should default to Uutf, and fallback
 * to camomile if needs be. *)
// let recodeLatin1 (src : string) =
//   let recodebuf = Buffer.create 16384 in
//   let rec loop d e =
//     match Uutf.decode d with
//     | Uchar _ as u ->
//       ignore (Uutf.encode e u)
//       loop d e
//     | _End -> ignore (Uutf.encode e End)
//     | Malformed _ ->
//       ignore (Uutf.encode e (Uchar Uutf.u_rep))
//       loop d e
//     | _Await -> assert false
//   let d = Uutf.decoder ISO_8859_1 (String src) in
//   let e = Uutf.encoder UTF_8 (Buffer recodebuf) in
//   loop d e
//   Buffer.contents recodebuf
let _socketsHandler =
  let socketsHandler = new SocketsHttpHandler()
  socketsHandler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
  socketsHandler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0
  socketsHandler

let httpClient () : HttpClient = new HttpClient(_socketsHandler)

let getHeader (headerKey : string) (headers : Headers) : string option =
  headers
  |> List.tryFind
       (fun ((k : string), (_ : string)) -> String.equalsCaseInsensitive headerKey k)
  |> Option.map (fun (k, v) -> v)

let formContentType = "application/x-www-form-urlencoded"
let jsonContentType = "application/json"
let textContentType = "text/plain"

let hasFormHeader (headers : Headers) : bool =
  // CLEANUP: this doesn't work properly if a charset is included. But also, this was
  // always false in OCaml because the string we compared against wasn't properly
  // trimmed.
  // getHeader "content-type" headers = Some formContentType
  false

let hasJsonHeader (headers : Headers) : bool =
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains jsonContentType)
  |> Option.defaultValue false

let hasTextHeader (headers : Headers) : bool =
  getHeader "content-type" headers
  |> Option.map (fun s -> s.Contains textContentType)
  |> Option.defaultValue false

// Convert strings into queryParams. This matches the OCaml Uri.query function. Note that keys and values use slightly different encodings
let toQueryString (queryParams : (List<string * List<string>>)) : string =
  let urlEncodeValue (s : string) : string =
    let encodeByte (b : byte) : byte array =
      // FSTODO: fuzz this against OCaml version
      // CLEANUP make a nicer version of this that's designed for this use case
      // We do want to escape the following: []+&^%#@"<>/;
      // We don't want to escape the following: *$@!:?,.-_'
      match b with
      | b when b >= (byte 'a') && b <= (byte 'z') -> [| b |]
      | b when b >= (byte '0') && b <= (byte '9') -> [| b |]
      | b when b >= (byte 'A') && b <= (byte 'Z') -> [| b |]
      | b when b = (byte '*') -> [| b |]
      | b when b = (byte '$') -> [| b |]
      | b when b = (byte '@') -> [| b |]
      | b when b = (byte '!') -> [| b |]
      | b when b = (byte ':') -> [| b |]
      | b when b = (byte '(') -> [| b |]
      | b when b = (byte ')') -> [| b |]
      | b when b = (byte '~') -> [| b |]
      | b when b = (byte '?') -> [| b |]
      | b when b = (byte '/') -> [| b |]
      | b when b = (byte '.') -> [| b |]
      | b when b = (byte '-') -> [| b |]
      | b when b = (byte '_') -> [| b |]
      | b when b = (byte '=') -> [| b |] // not the same for key
      | b when b = (byte '\'') -> [| b |]
      | _ -> toBytes ("%" + b.ToString("X2"))
    s |> toBytes |> Array.collect encodeByte |> ofBytes

  let urlEncodeKey (s : string) : string =
    let encodeByte (b : byte) : byte array =
      // FSTODO: fuzz this against OCaml version
      // CLEANUP make a nicer version of this that's designed for this use case
      // We do want to escape the following: []+&^%#@"<>/;
      // We don't want to escape the following: *$@!:?,.-_'
      match b with
      | b when b >= (byte 'a') && b <= (byte 'z') -> [| b |]
      | b when b >= (byte '0') && b <= (byte '9') -> [| b |]
      | b when b >= (byte 'A') && b <= (byte 'Z') -> [| b |]
      | b when b = (byte '*') -> [| b |]
      | b when b = (byte '$') -> [| b |]
      | b when b = (byte '@') -> [| b |]
      | b when b = (byte '!') -> [| b |]
      | b when b = (byte ':') -> [| b |]
      | b when b = (byte '(') -> [| b |]
      | b when b = (byte ')') -> [| b |]
      | b when b = (byte '~') -> [| b |]
      | b when b = (byte '?') -> [| b |]
      | b when b = (byte '/') -> [| b |]
      | b when b = (byte '.') -> [| b |]
      | b when b = (byte ',') -> [| b |] // only in keys
      | b when b = (byte '-') -> [| b |]
      | b when b = (byte '_') -> [| b |]
      | b when b = (byte '\'') -> [| b |]

      | _ -> toBytes ("%" + b.ToString("X2"))
    s |> toBytes |> Array.collect encodeByte |> ofBytes

  match queryParams with
  | [ key, [] ] -> "?" + urlEncodeKey key
  | _ ->
    queryParams
    |> List.map
         (fun (k, vs) ->
           let k = k |> urlEncodeKey
           vs
           |> List.map urlEncodeValue
           |> String.concat ","
           |> fun vs -> $"{k}={vs}")
    |> String.concat "&"
    |> fun s -> if s = "" then "" else "?" + s

// Convert .NET HttpHeaders into Dark-style headers
let convertHeaders (headers : HttpHeaders) : List<string * string> =
  headers
  |> Seq.map
       (fun (kvp : KeyValuePair<string, seq<string>>) ->
         (kvp.Key, kvp.Value |> Seq.toList |> String.concat ","))
  |> Seq.toList



// The [body] parameter is optional to force us to actually treat its
// presence/non-presence correctly between different requests. Naively using
// the empty string to stand-in for "no body" was a pattern that bubbled up
// too far and lead to us passing `""` to a function that then JSON encoded
// the empty string, leading to the string `"\"\""` being passed here. By
// making this an `option` here, and bubbling this optionality the whole way
// up the callstack, we hopefully make it clear that a request has an optional
// body
let httpCallWithCode
  (rawBytes : bool)
  (url : string)
  (queryParams : (string * string list) list)
  (method : HttpMethod)
  (reqHeaders : Headers)
  (reqBody : string option)
  : Task<Result<HttpResult, ClientError>> =
  task {
    try
      // FSTODO: check clients dont share cookies or other state (apart from DNS cache)
      // FSTODO: ensure the proxy is used
      let uri = System.Uri(url, System.UriKind.Absolute)
      // FSTODO get the query, combine it with the other query, then re-add it
      if uri.Scheme <> "https" && uri.Scheme <> "http" then
        return Error { url = url; code = 0; error = "Unsupported protocol" }
      elif uri.IsLoopback then
        return Error { url = url; code = 0; error = "Loopback is not allowed" }
      else
        let req = new HttpRequestMessage(method, url + toQueryString queryParams)

        // username and password - note that an actual auth header will overwrite this
        if uri.UserInfo <> "" then
          let authString =
            // UserInfo is escaped during parsing, but shouldn't actually isn't
            // useful here, so unescape it.
            let userInfo = System.Uri.UnescapeDataString uri.UserInfo
            // Handle usernames with no colon
            if userInfo.Contains(":") then userInfo else userInfo + ":"
          req.Headers.Authorization <-
            AuthenticationHeaderValue(
              "Basic",
              System.Convert.ToBase64String(
                System.Text.ASCIIEncoding.UTF8.GetBytes(authString)
              )
            )

        // content
        let body =
          match reqBody with
          | Some body -> toBytes body
          | None -> [||]
        req.Content <- new ByteArrayContent(body)

        // headers
        List.iter
          (fun (k, v) ->
            if v = "" then
              // CLEANUP: OCaml doesn't send empty headers, but no reason not to
              ()
            elif String.equalsCaseInsensitive k "content-type" then
              req.Content.Headers.ContentType <- MediaTypeHeaderValue.Parse(v)
            else
              // FSTODO test headers that are gibberish
              // Dark headers can only be added once, as they use a Dict. Remove them
              // so they don't get added twice (eg via Authorization headers above)
              req.Headers.Remove(k) |> ignore<bool>
              let added = req.Headers.TryAddWithoutValidation(k, v)
              // Headers are split between req.Headers and req.Content.Headers so just try both
              if not added then
                req.Content.Headers.Remove(k) |> ignore<bool>
                req.Content.Headers.Add(k, v))
          reqHeaders

        // send request
        let client = httpClient ()
        let! response = client.SendAsync req
        let! respBody = response.Content.ReadAsByteArrayAsync()
        let result =
          { body = respBody |> ofBytes
            code = int response.StatusCode
            headers =
              convertHeaders response.Headers
              @ convertHeaders response.Content.Headers
            error = ""
            httpVersion = string response.Version
            httpStatusMessage = response.ReasonPhrase }
        return Ok result
    with
    | :? TaskCanceledException -> // only timeouts
      return Error { url = url; code = 0; error = "Timeout" }
    | :? System.ArgumentException as e -> // incorrect protocol, possibly more
      return Error { url = url; code = 0; error = e.Message }
    | :? System.UriFormatException ->
      return Error { url = url; code = 0; error = "Invalid URI" }
    | :? HttpRequestException as e ->
      let code = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
      return Error { url = url; code = code; error = e.Message }
  }


// try
//   let queryParams = url |> Uri.of_string |> Uri.query |> List.append query_params
//   let url =
//     url |> Uri.of_string |> Uri.with_uri (Some queryParams) |> Uri.to_string
//   let headers = headers |> List.map (fun (k, v) -> k ^ ": " ^ v) in
//   let errorbuf = ref "" in
//   let responsebuf = Buffer.create 16384 in
//   (* uploads *)
//   (* let bodybuffer = ref body in *)
//   (* let putfn (count: int) : string = *)
//   (*   let len = String.length !bodybuffer in *)
//   (*   let this_body = !bodybuffer in *)
//   (*   if count < len *)
//   (*   then (bodybuffer := ""; this_body) *)
//   (*   else *)
//   (*     let result = String.sub ~pos:0 ~len:count this_body in *)
//   (*     let save = String.sub ~pos:count ~len:(len-count) this_body in *)
//   (*     bodybuffer := save; *)
//   (*     result *)
//   (*   in *)
//   let responsefn str : int =
//     Buffer.add_string responsebuf str
//     String.length str
//   (* headers *)
//   let result_headers = ref [] in
//   let headerfn (h : string) : int =
//     (* See comment about responsebody below before changing this. *)
//     let split = String.lsplit2 ':' h in

//     match split with
//     | Some (l, r) ->
//       result_headers := List.cons (l, r) !result_headers
//       String.length h
//     | None ->
//       result_headers := List.cons (h, "") !result_headers
//       String.length h
//   let debug_bufs = new_debug_bufs () in
//   let code, error, body =
//     let c = C.init () in
//     C.set_url c url
//     C.set_verbose c true
//     C.set_debugfunction c (debugfn debug_bufs)
//     C.set_errorbuffer c errorbuf
// FSTODO followlocation
//     C.set_followlocation c true
//     C.set_failonerror c false
//     C.set_writefunction c responsefn
//     C.set_httpheader c headers
//     C.set_headerfunction c headerfn
// FSTODO timeout
//     C.setopt c (Curl.CURLOPT_TIMEOUT 30) (* timeout is infinite by default *)
//     (* This tells CURL to send an Accept-Encoding header including all
//       * of the encodings it supports *and* tells it to automagically decode
//       * responses in those encodings. This works even if someone manually specifies
//       * the encoding in the header, as libcurl will still appropriately decode it
//       *
//       * https://curl.haxx.se/libcurl/c/CURLOPT_ACCEPT_ENCODING.html
//       * *)
// FSTODO rawbytes
//     if not raw_bytes then C.set_encoding c C.CURL_ENCODING_ANY
//     (* Don't let users curl to e.g. file://; just HTTP and HTTPs. *)
// FSTODO allowed protocols
//     C.set_protocols c [ C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS ]
//     (* Seems like redirects can be used to get around the above list... *)
//     C.set_redirprotocols c [ C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS ]
// FSTODO proxy
//     C.setopt c (Curl.CURLOPT_PROXY Config.curl_tunnel_url)
//     (match verb with
//      | PUT ->
//        (match body with
//         | Some body ->
//           C.set_postfields c body
//           C.set_postfieldsize c (String.length body)
//           C.set_customrequest c "PUT"
//         | None ->
//           C.set_postfields c ""
//           C.set_postfieldsize c 0
//           C.set_customrequest c "PUT")
//      | POST ->
//        (match body with
//         | Some body ->
//           C.set_post c true
//           C.set_postfields c body
//           C.set_postfieldsize c (String.length body)
//         | None ->
//           C.set_postfields c ""
//           C.set_postfieldsize c 0
//           C.set_customrequest c "POST")
//      | PATCH ->
//        (match body with
//         | Some body ->
//           C.set_postfields c body
//           C.set_postfieldsize c (String.length body)
//           C.set_customrequest c "PATCH"
//         | None ->
//           C.set_postfields c ""
//           C.set_postfieldsize c 0
//           C.set_customrequest c "PATCH")
//      | DELETE ->
//        C.set_followlocation c false
//        C.set_customrequest c "DELETE"
//      | OPTIONS -> C.set_customrequest c "OPTIONS"
//      | HEAD ->
//        C.set_nobody c true
//        C.set_customrequest c "HEAD"
//      | GET -> ())
//     (* Actually do the request *)
//     C.perform c
//     (* If we get a redirect back, then we may see the content-type
//       * header twice. Fortunately, because we push headers to the front
//       * above, and take the first in charset, we get the right
//       * answer. Whew. To do this correctly, we'd have to implement our
//       * own follow logic which would clear the header ref, which seems
//       * straightforward in theory but likely not practice.
//       * Alternatively, we could clear the headers ref when we receive a
//       * new `ok` header. *)
// FSTODO latin1 translation
//     let responsebody =
//       if charset !result_headers = Latin1 then
//         recode_latin1 (Buffer.contents responsebuf)
//       else
//         Buffer.contents responsebuf
//     let response = (C.get_responsecode c, !errorbuf, responsebody)
//     let primaryip = C.get_primaryip c
//     C.cleanup c
//     log_debug_info debug_bufs (Some primaryip)
//     response
//   let obj = { body = body; code = code; headers = !result_headers; error = error }
//   Ok obj
// with
// | Curl.CurlException (curl_code, code, s) ->
//   let info =
//     url
//     error = Curl.strerror curl_code
//     code
//   Error info


let httpCall
  (rawBytes : bool)
  (url : string)
  (queryParams : (string * string list) list)
  (verb : HttpMethod)
  (headers : (string * string) list)
  (body : string option)
  =
  httpCallWithCode rawBytes url queryParams verb headers body
