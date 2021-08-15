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
open System.IO.Compression
open System.IO

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
let dvalToFormEncoding (dv : RT.Dval) : HttpContent =
  dvalToQuery dv
  |> List.map (fun (k, v) -> KeyValuePair(k, String.concat "," v))
  |> (fun pairs -> new FormUrlEncodedContent(pairs))
  :> HttpContent

// FSTODO: fuzz against OCaml
let queryStringToParams (queryString : string) : List<string * List<string>> =
  let nvc = System.Web.HttpUtility.ParseQueryString queryString
  nvc.AllKeys
  |> Array.map
       (fun key ->
         let values = nvc.GetValues key
         let split =
           values.[values.Length - 1]
           |> FSharpPlus.String.split [| "," |]
           |> Seq.toList

         if isNull key then
           // All the values with no key are by GetValues, so make each one a value
           values |> Array.toList |> List.map (fun k -> (k, []))
         else
           [ (key, split) ])
  |> List.concat


// FSTODO: fuzz against OCaml
let queryToDval (queryString : string) : RT.Dval =
  queryString
  |> queryStringToParams
  |> List.map
       (fun (k, v) ->
         match v with
         | [] -> k, RT.DNull
         | [ "" ] -> k, RT.DNull // CLEANUP this should be a string
         | [ v ] -> k, RT.DStr v
         | list -> k, RT.DList(List.map RT.DStr list))
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
let _httpMessageHandler : HttpMessageHandler =
  let handler = new SocketsHttpHandler()
  handler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
  handler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0

  // Note, do not do automatic decompression, see decompression code later for details
  handler.AutomaticDecompression <- System.Net.DecompressionMethods.None

  handler.AllowAutoRedirect <- false

  // CLEANUP rename CurlTunnelUrl
  // CLEANUP add port into config var
  // This port is assumed by Curl in the OCaml version, but not by .NET
  handler.UseProxy <- true
  handler.Proxy <- System.Net.WebProxy($"{Config.curlTunnelUrl}:1080", false)

  handler.UseCookies <- false // FSTODO test
  handler :> HttpMessageHandler

let httpClient () : HttpClient =
  let client = new HttpClient(_httpMessageHandler)
  client.Timeout <- System.TimeSpan.FromSeconds 30.0
  // Can't find what this was in Curl, but this seems a reasonable default
  client.MaxResponseContentBufferSize <- 1024L * 1024L * 100L
  client

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



exception InvalidEncodingException of int

let httpCall
  (rawBytes : bool)
  (url : string)
  (queryParams : (string * string list) list)
  (method : HttpMethod)
  (reqHeaders : Headers)
  (reqBody : HttpContent)
  : Task<Result<HttpResult, ClientError>> =
  task {
    try
      // FSTODO: check clients dont share cookies or other state (apart from DNS cache)
      let uri = System.Uri(url, System.UriKind.Absolute)
      if uri.Scheme <> "https" && uri.Scheme <> "http" then
        return Error { url = url; code = 0; error = "Unsupported protocol" }
      else
        // Remove the parts of the existing Uri that are duplicated or handled in
        // other ways
        let reqUri = System.UriBuilder()
        reqUri.Scheme <- uri.Scheme
        reqUri.Host <- uri.Host
        reqUri.Port <- uri.Port
        reqUri.Path <- uri.AbsolutePath
        reqUri.Query <- toQueryString (queryParams @ queryStringToParams uri.Query)
        let req = new HttpRequestMessage(method, string reqUri)

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
              System.Convert.ToBase64String(toBytes authString)
            )

        // content
        req.Content <- reqBody

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

        // We do not do automatic decompression, because if we did, we would lose the
        // content-Encoding header, which the automatic decompression removes for
        // some reason.
        // From http://www.west-wind.com/WebLog/posts/102969.aspx
        let! responseStream = response.Content.ReadAsStreamAsync()
        let encoding = response.Content.Headers.ContentEncoding.ToString()
        let contentStream =
          // The version of Curl we used in OCaml does not support zstd, so omitting
          // that won't break anything.
          if (String.equalsCaseInsensitive "br" encoding) then
            new BrotliStream(responseStream, CompressionMode.Decompress) :> Stream
          elif (String.equalsCaseInsensitive "gzip" encoding) then
            new GZipStream(responseStream, CompressionMode.Decompress) :> Stream
          elif (String.equalsCaseInsensitive "deflate" encoding) then
            new DeflateStream(responseStream, CompressionMode.Decompress) :> Stream
          else if encoding = "" then
            responseStream
          else
            raise (InvalidEncodingException(int response.StatusCode))

        let! respBody = (new StreamReader(contentStream)).ReadToEndAsync()
        let result =
          { body = respBody
            code = int response.StatusCode
            headers =
              convertHeaders response.Headers
              @ convertHeaders response.Content.Headers
            error = ""
            httpVersion = string response.Version
            httpStatusMessage = response.ReasonPhrase }
        return Ok result
    with
    | InvalidEncodingException code ->
      let error = "Unrecognized or bad HTTP Content or Transfer-Encoding"
      return Error { url = url; code = code; error = error }
    | :? TaskCanceledException -> // only timeouts
      return Error { url = url; code = 0; error = "Timeout" }
    | :? System.ArgumentException as e -> // incorrect protocol, possibly more
      return Error { url = url; code = 0; error = e.Message }
    | :? System.UriFormatException ->
      return Error { url = url; code = 0; error = "Invalid URI" }
    | :? IOException as e -> return Error { url = url; code = 0; error = e.Message }
    | :? HttpRequestException as e ->
      let code = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
      return Error { url = url; code = code; error = e.Message }
  }


// FSTODO followlocation
//     C.set_followlocation c true
// FSTODO allowed protocols on redirect
//     (* Seems like redirects can be used to get around the above list... *)
//     C.set_redirprotocols c [ C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS ]
// FSTODO - don't follow on DELETE
//      | DELETE ->
//        C.set_followlocation c false
// FSTODO rawbytes
//     if not raw_bytes then C.set_encoding c C.CURL_ENCODING_ANY
// FSTODO - test for the following
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
