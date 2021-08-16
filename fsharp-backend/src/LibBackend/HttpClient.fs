module LibBackend.HttpClient

// HttpClient used by standard libraries

open Prelude
open LibExecution

module RT = RuntimeTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Net.Http
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


// There has been quite a history of HTTPClient having problems in previous versions
// of .NET, including socket exhaustion and DNS results not expiring. The history is
// handled quite well in
// https://www.stevejgordon.co.uk/httpclient-connection-pooling-in-dotnet-core
//
// As of today (using .NET6) it seems we no longer need to worry about either socket
// exhaustion or DNS issues, so long as we use a single HttpClient.
let httpClient : HttpClient =
  let handler = new SocketsHttpHandler()
  handler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
  handler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0

  // Note, do not do automatic decompression, see decompression code later for details
  handler.AutomaticDecompression <- System.Net.DecompressionMethods.None

  // 50 is the default too. The OCaml implementation used infinite, but that's
  // probably not a good default.
  handler.AllowAutoRedirect <- true
  handler.MaxAutomaticRedirections <- 50

  // CLEANUP rename CurlTunnelUrl
  // CLEANUP add port into config var
  // This port is assumed by Curl in the OCaml version, but not by .NET
  handler.UseProxy <- true
  handler.Proxy <- System.Net.WebProxy($"{Config.curlTunnelUrl}:1080", false)

  // Users share the HttpClient, don't let them share cookies!
  handler.UseCookies <- false


  let client = new HttpClient(handler)
  client.Timeout <- System.TimeSpan.FromSeconds 30.0
  // Can't find what this was in OCaml/Curl, but 100MB seems a reasonable default
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

        // CLEANUP We could use 3.0. This uses 2.0 as that's what was supported in
        // OCaml/Curl, and we don't want to change behaviour. The potential behaviour
        // is that we know the behaviour of headers in 2.0 (our OCaml code lowercases
        // them in 2.0 only, but we don't want a surprise with 3.0 when they're
        // dynamically upgraded)
        req.Version <- System.Net.HttpVersion.Version20

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
        let! response = httpClient.SendAsync req

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

        use memoryStream = new MemoryStream()
        do! contentStream.CopyToAsync(memoryStream)
        let respBody = memoryStream.ToArray()

        let respString =
          // CLEANUP we can support any encoding that .NET supports, which I bet is a
          // lot
          let latin1 =
            try
              let charset = response.Content.Headers.ContentType.CharSet
              match charset with
              | "latin1"
              | "us-ascii"
              | "iso-8859-1"
              | "iso_8859-1" -> true
              | _ -> false
            with
            | _ -> false
          if latin1 then
            System.Text.Encoding.Latin1.GetString respBody
          else
            ofBytes respBody

        let result =
          { body = respString
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
      let message =
        if e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')" then
          "Unsupported protocol"
        else
          e.Message
      return Error { url = url; code = 0; error = message }
    | :? System.UriFormatException ->
      return Error { url = url; code = 0; error = "Invalid URI" }
    | :? IOException as e -> return Error { url = url; code = 0; error = e.Message }
    | :? HttpRequestException as e ->
      let code = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
      return Error { url = url; code = code; error = e.Message }
  }

// FSTODO rawbytes
//     if not raw_bytes then C.set_encoding c C.CURL_ENCODING_ANY
