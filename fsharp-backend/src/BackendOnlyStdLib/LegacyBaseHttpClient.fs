module BackendOnlyStdLib.LegacyBaseHttpClient

// Provides the basic http client used by LibHttpClients 0-4. Superceded by
// HttpClient which supports LibHttpClient5.

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.IO
open System.IO.Compression
open System.Net.Http

type AspHeaders = System.Net.Http.Headers.HttpHeaders

open Prelude
open LibExecution
open LibBackend
open VendoredTablecloth
open LibExecution.RuntimeTypes

module Telemetry = LibService.Telemetry

module RT = RuntimeTypes

type HttpResult = { body : byte []; code : int; headers : HttpHeaders.T }

type ClientError = { url : string; error : string; code : int }



// -------------------------
// Forms and queries Functions
// -------------------------
module MediaType =
  type T =
    | Form
    | Xml
    | Json
    | Text
    | Html
    | Other of string

    override this.ToString() : string =
      match this with
      | Form -> "application/x-www-form-urlencoded"
      | Xml -> "application/xml"
      | Json -> "application/json"
      | Text -> "text/plain"
      | Html -> "text/html"
      | Other s -> s

  let parse (str : string) : T =
    match String.trim str with
    | "application/x-www-form-urlencoded" -> Form
    | "application/xml" -> Xml
    | "application/json" -> Json
    | "text/plain" -> Text
    | "text/html" -> Html
    | _ -> Other str

module Charset =
  type T =
    | Utf8
    | NotUtf8 of string

    override this.ToString() : string =
      match this with
      | Utf8 -> "utf-8"
      | NotUtf8 s -> s

  let parse (str : string) : T =
    match String.trim str with
    | "utf-8" -> Utf8
    | _ -> NotUtf8 str

module ContentType =
  type T =
    | Known of MediaType.T * Charset.T
    | KnownNoCharset of MediaType.T
    | Unknown of string // don't parse out charset or boundary or anything

    override this.ToString() : string =
      match this with
      | Known (mt, cs) -> $"{mt}; charset={cs}"
      | KnownNoCharset (mt) -> string mt
      | Unknown s -> s

  let toMediaType (ct : T) : Option<MediaType.T> =
    match ct with
    | Known (mt, _) -> Some mt
    | KnownNoCharset (mt) -> Some mt
    | Unknown s -> None

  let toHttpHeader (ct : T) : HttpHeaders.Header = "Content-Type", string ct

  let parse (str : string) : T =
    match String.split ";" str |> List.map String.trim with
    | [ mt; cs ] ->
      match String.split "=" cs |> List.map String.trim with
      | [ "charset"; cs ] -> Known(MediaType.parse mt, Charset.parse cs)
      | _ -> Unknown(str)
    | [ mt ] -> KnownNoCharset(MediaType.parse mt)
    | _ -> Unknown str

  let text = (Known(MediaType.Text, Charset.Utf8))
  let json = (Known(MediaType.Json, Charset.Utf8))

  let textHeader : HttpHeaders.Header = toHttpHeader text

  let getContentType (headers : HttpHeaders.T) : Option<T> =
    headers |> HttpHeaders.get "Content-type" |> Option.map parse

  let getMediaType (headers : HttpHeaders.T) : Option<MediaType.T> =
    headers |> getContentType |> Option.bind toMediaType

  let hasNoContentType (headers : HttpHeaders.T) : bool =
    headers |> getContentType |> Option.isNone

  let hasJsonHeader (headers : HttpHeaders.T) : bool =
    // CLEANUP: don't use contains for this
    HttpHeaders.get "content-type" headers
    |> Option.map (fun s -> s.Contains "application/json")
    |> Option.defaultValue false

  let hasFormHeaderWithoutCharset (headers : HttpHeaders.T) : bool =
    // CLEANUP: don't use contains for this
    HttpHeaders.get "content-type" headers
    |> Option.map (fun s -> s = "application/x-www-form-urlencoded")
    |> Option.defaultValue false



// includes an implicit content-type
type Content =
  // OCaml's impl. used cURL under the hood.
  // cURL is special in that it will assume that
  // the request is a _form_ request if unspecified,
  // when POST/PUTing
  | FakeFormContentToMatchCurl of string
  | FormContent of string
  | StringContent of string
  | NoContent

// There has been quite a history of HTTPClient having problems in previous versions
// of .NET, including socket exhaustion and DNS results not expiring. The history is
// handled quite well in
// https://www.stevejgordon.co.uk/httpclient-connection-pooling-in-dotnet-core
//
// As of today (using .NET6) it seems we no longer need to worry about either socket
// exhaustion or DNS issues. It appears that we can use either multiple HTTP clients
// or just one, we use just one for efficiency.
// See https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests?view=aspnetcore-6.0#alternatives-to-ihttpclientfactory
//
// Note that I manually verified by hand the number of sockets, which you can do with
//   sudo netstat -apn | grep _WAIT
let socketHandler : HttpMessageHandler =
  let handler = new SocketsHttpHandler()

  // Avoid DNS problems
  handler.PooledConnectionIdleTimeout <- System.TimeSpan.FromMinutes 5.0
  handler.PooledConnectionLifetime <- System.TimeSpan.FromMinutes 10.0

  // Note, do not do automatic decompression, see decompression code later for details
  handler.AutomaticDecompression <- System.Net.DecompressionMethods.None

  // If we use auto-redirect, we can't limit the protocols or massage the headers, so
  // we're going to have to implement this manually
  handler.AllowAutoRedirect <- false

  handler.UseProxy <- true
  handler.Proxy <- System.Net.WebProxy(Config.httpclientProxyUrl, false)

  // Don't add a RequestId header for opentelemetry
  handler.ActivityHeadersPropagator <- null

  // Users share the HttpClient, don't let them share cookies!
  handler.UseCookies <- false
  handler


let httpClient : HttpClient =
  let client = new HttpClient(socketHandler, disposeHandler = false)
  client.Timeout <- System.TimeSpan.FromSeconds 30.0
  // 100MB seems a reasonable default
  client.MaxResponseContentBufferSize <- 1024L * 1024L * 100L
  client

// Convert .NET HttpHeaders into Dark-style headers
let convertHeaders (headers : AspHeaders) : HttpHeaders.T =
  headers
  |> Seq.map Tuple2.fromKeyValuePair
  |> Seq.map (fun (k, v) -> (k, v |> Seq.toList |> String.concat ","))
  |> Seq.toList

exception InvalidEncodingException of int

let prependInternalErrorMessage errorMessage =
  $"Internal HTTP-stack exception: {errorMessage}"

let makeHttpCall
  (url : string)
  (queryParams : (string * string list) list)
  (method : HttpMethod)
  (reqHeaders : HttpHeaders.T)
  (reqBody : Content)
  : Task<Result<HttpResult, ClientError>> =
  task {
    use _ = Telemetry.child "LegacyHttpClient.call" [ "url", url; "method", method ]
    try
      let uri = System.Uri(url, System.UriKind.Absolute)
      if uri.Scheme <> "https" && uri.Scheme <> "http" then
        return
          Error
            { url = url
              code = 0
              error = prependInternalErrorMessage "Unsupported protocol" }
      else
        // Remove the parts of the existing Uri that are duplicated or handled in
        // other ways
        let reqUri = System.UriBuilder()
        reqUri.Scheme <- uri.Scheme
        reqUri.Host <- uri.Host
        reqUri.Port <- uri.Port
        reqUri.Path <- uri.AbsolutePath
        // Remove the question mark
        let query =
          if uri.Query.Length > 0 then String.dropLeft 1 uri.Query else uri.Query
        reqUri.Query <- HttpQueryEncoding.createQueryString query queryParams
        use req = new HttpRequestMessage(method, string reqUri)

        // CLEANUP We could use Http3. This uses Http2 as that's what was supported in
        // OCaml/Curl, and we don't want to change behaviour. The potential behaviour
        // is that we know the behaviour of headers in Http2 (our OCaml code lowercases
        // them in Http2 only, but we don't want a surprise with Http3 when they're
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
            Headers.AuthenticationHeaderValue(
              "Basic",
              Base64.defaultEncodeToString (UTF8.toBytes authString)
            )

        // If the user set the content-length, then we want to try to set the content
        // length of the data. Don't let it be set too large though, as that isn't
        // allowed in .NET.
        let contentLengthHeader : Option<int> =
          reqHeaders
          |> List.find (fun (k, v) -> String.equalsCaseInsensitive k "content-length")
          // Note: in ocaml it would send nonsense headers, .NET doesn't allow it
          |> Option.bind (fun (k, v) -> parseInt v)

        // content
        let utf8 = System.Text.Encoding.UTF8
        match reqBody with
        | FormContent s ->
          let s =
            match contentLengthHeader with
            | None -> s
            | Some count when count >= s.Length -> s
            | Some count -> s.Substring(0, count)
          req.Content <-
            new StringContent(s, utf8, "application/x-www-form-urlencoded")
        | StringContent str ->
          let str =
            match contentLengthHeader with
            | None -> str
            | Some count when count >= str.Length -> str
            | Some count -> str.Substring(0, count)
          req.Content <- new StringContent(str, utf8, "text/plain")
        | NoContent -> req.Content <- new ByteArrayContent [||]
        | FakeFormContentToMatchCurl s ->
          let s =
            match contentLengthHeader with
            | None -> s
            | Some count when count >= s.Length -> s
            | Some count -> s.Substring(0, count)
          req.Content <-
            new StringContent(s, utf8, "application/x-www-form-urlencoded")
          req.Content.Headers.ContentType.CharSet <- System.String.Empty

        // headers
        let defaultHeaders =
          Map [ "Accept", "*/*"; "Accept-Encoding", "deflate, gzip, br" ]
        Map reqHeaders
        |> Map.mergeFavoringRight defaultHeaders
        |> Map.iter (fun k v ->
          if v = "" then
            // CLEANUP: OCaml doesn't send empty headers, but no reason not to
            ()
          elif String.equalsCaseInsensitive k "content-type" then
            try
              req.Content.Headers.ContentType <-
                Headers.MediaTypeHeaderValue.Parse(v)
            with
            | :? System.FormatException ->
              Exception.raiseCode "Invalid content-type header"
          elif String.equalsCaseInsensitive k "content-length" then
            // Handled above
            ()
          else
            // Dark headers can only be added once, as they use a Dict. Remove them
            // so they don't get added twice (eg via Authorization headers above)
            req.Headers.Remove(k) |> ignore<bool>
            let added = req.Headers.TryAddWithoutValidation(k, v)
            // Headers are split between req.Headers and req.Content.Headers so just try both
            if not added then
              req.Content.Headers.Remove(k) |> ignore<bool>
              req.Content.Headers.Add(k, v))

        // send request
        Telemetry.addTag "request.content_type" req.Content.Headers.ContentType
        Telemetry.addTag "request.content_length" req.Content.Headers.ContentLength
        use! response = httpClient.SendAsync req

        // We do not do automatic decompression, because if we did, we would lose the
        // content-Encoding header, which the automatic decompression removes for
        // some reason.
        // From http://www.west-wind.com/WebLog/posts/102969.aspx
        let encoding = response.Content.Headers.ContentEncoding.ToString()
        Telemetry.addTags [ "response.encoding", encoding
                            "response.status_code", response.StatusCode
                            "response.version", response.Version ]
        use! responseStream = response.Content.ReadAsStreamAsync()
        use contentStream : Stream =
          let decompress = CompressionMode.Decompress
          // CLEANUP support zstd
          match String.toLowercase encoding with
          | "br" -> new BrotliStream(responseStream, decompress)
          | "gzip" -> new GZipStream(responseStream, decompress)
          | "deflate" -> new DeflateStream(responseStream, decompress)
          | "" -> responseStream
          | _ -> raise (InvalidEncodingException(int response.StatusCode))

        use memoryStream = new MemoryStream()
        do! contentStream.CopyToAsync(memoryStream)
        let respBody = memoryStream.ToArray()

        let respBody =
          // CLEANUP we can support any encoding that .NET supports, which I bet is a
          // lot
          let latin1 =
            try
              let charset = response.Content.Headers.ContentType.CharSet.ToLower()
              Telemetry.addTag
                "response.content_type"
                response.Content.Headers.ContentType
              match charset with
              | "latin1"
              | "us-ascii"
              | "iso-8859-1"
              | "iso_8859-1" -> true
              | _ -> false
            with
            | _ -> false
          if latin1 then
            System.Text.Encoding.Latin1.GetString respBody |> UTF8.toBytes
          else
            respBody

        let code = int response.StatusCode
        let isHttp2 = (response.Version = System.Net.HttpVersion.Version20)

        // CLEANUP: For some reason, the OCaml version includes a header with the HTTP
        // status line the response and each redirect.
        let statusHeader =
          if isHttp2 then
            $"HTTP/2 {code}"
          else
            $"HTTP/{response.Version} {code} {response.ReasonPhrase}"

        let headers =
          convertHeaders response.Headers @ convertHeaders response.Content.Headers

        // CLEANUP The OCaml version automatically made this lowercase for
        // http2. That's a weird experience for users, as they don't have
        // control over this, so make this lowercase by default
        let headers =
          if isHttp2 then
            List.map (fun (k : string, v) -> (String.toLowercase k, v)) headers
          else
            headers

        let result =
          { body = respBody; code = code; headers = [ statusHeader, "" ] @ headers }
        return Ok result
    with
    | InvalidEncodingException code ->
      let error = "Unrecognized or bad HTTP Content or Transfer-Encoding"
      Telemetry.addTags [ "error", true; "error.msg", error ]
      return
        Error { url = url; code = code; error = prependInternalErrorMessage error }
    | :? TaskCanceledException -> // only timeouts
      Telemetry.addTags [ "error", true; "error.msg", "Timeout" ]
      return
        Error { url = url; code = 0; error = prependInternalErrorMessage "Timeout" }
    | :? System.ArgumentException as e -> // incorrect protocol, possibly more
      let message =
        if e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')" then
          prependInternalErrorMessage "Unsupported protocol"
        else
          prependInternalErrorMessage e.Message
      Telemetry.addTags [ "error", true; "error.msg", message ]
      return
        Error { url = url; code = 0; error = prependInternalErrorMessage message }
    | :? System.UriFormatException ->
      Telemetry.addTags [ "error", true; "error.msg", "Invalid URI" ]
      return
        Error
          { url = url; code = 0; error = prependInternalErrorMessage "Invalid URI" }
    | :? IOException as e ->
      Telemetry.addTags [ "error", true; "error.msg", e.Message ]
      return
        Error { url = url; code = 0; error = prependInternalErrorMessage e.Message }
    | :? HttpRequestException as e ->
      let code = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
      Telemetry.addException [ "error.status_code", code ] e
      let message = e |> Exception.getMessages |> String.concat " "
      return
        Error { url = url; code = code; error = prependInternalErrorMessage message }
  }

// Encodes [body] as a UTF-8 string, safe for sending across the internet! Uses
// the `Content-Type` header provided by the user in [headers] to make ~magic~ decisions about
// how to encode said body. Returns a tuple of the encoded body, and the passed headers that
// have potentially had a Content-Type added to them based on the magic decision we've made.
let encodeRequestBody
  jsonFn
  (headers : HttpHeaders.T)
  (body : RT.Dval option)
  : Content =
  match body with
  | Some dv ->
    match dv with
    | RT.DObj _ when ContentType.hasFormHeaderWithoutCharset headers ->
      match HttpQueryEncoding.toFormEncoding dv with
      | Ok content -> FormContent(content)
      | Error msg -> Exception.raiseCode msg
    | _ when ContentType.hasNoContentType headers ->
      FakeFormContentToMatchCurl(jsonFn dv)
    | _ -> StringContent(jsonFn dv)
  | None -> NoContent

let rec httpCall
  (count : int)
  (url : string)
  (queryParams : (string * string list) list)
  (method : HttpMethod)
  (reqHeaders : HttpHeaders.T)
  (reqBody : Content)
  : Task<Result<HttpResult, ClientError>> =
  task {
    // The OCaml version of these functions handled a number of things differently
    // which can only be done with our own redirect logic:
    //   1) when redirecting, it stored all headers along the way
    //   2) when redirecting, it kept the Authorization header (which HTTPClient drops)
    //   3) when redirecting, it failed appropriately when the scheme was not http/https
    // Each of these was a breaking change, and not great, but considering all three
    // it felt worth implementing redirects ourselves. Note that we did not use
    // recursion within makeHttpCall, to ensure that the HTTP objects/streams were
    // all cleaned up before the redirect happened. We do keep more data in this case
    // than we would like but they're all redirects and so unlikely to have bodies.
    if (count > 50) then
      return Error { url = url; code = 0; error = "Too many redirects" }
    else
      let! response = makeHttpCall url queryParams method reqHeaders reqBody

      match response with
      | Ok result when result.code >= 300 && result.code < 400 ->
        let location =
          result.headers
          |> List.tryFind (fun (k, _) -> String.equalsCaseInsensitive "location" k)

        match location with
        | Some (_, locationUrl) when method <> HttpMethod.Delete ->
          let newCount = count + 1

          // It might be a relative URL. If the location is absolute, the location will win over the last URL
          let newUrl = System.Uri(System.Uri(url), locationUrl).ToString()

          // Match curls default redirect behaviour: if it's a POST with content, redirect to GET
          let method, reqBody =
            match reqBody with
            | StringContent body when body <> "" ->
              if method = HttpMethod.Post then
                HttpMethod.Get, NoContent
              else
                method, NoContent
            | FakeFormContentToMatchCurl body when body <> "" ->
              if method = HttpMethod.Post then
                HttpMethod.Get, NoContent
              else
                method, NoContent
            | _ -> method, reqBody

          // Unlike HttpClient, do not drop the authorization header
          let! newResponse =
            // Query params are part of the client's creation of the url. Once the
            // server redirects, it gives us a new url and we shouldn't append the
            // query param to it.
            // Consider: http://redirect.to?url=xyz.com/path
            httpCall newCount newUrl [] method reqHeaders reqBody

          return
            Result.map
              (fun redirectResult ->
                // Keep all headers along the way, mirroring the OCaml version. The
                // first request's headers win when there are duplicates.
                // CLEANUP: really the redirect target should win
                { redirectResult with
                    headers = redirectResult.headers @ result.headers })
              newResponse
        | _ -> return response
      | _ -> return response
  }

// Converts an object to (string,string) pairs. Raises an exception if not an object
let toStringPairs (dv : Dval) : Result<List<string * string>, string> =
  match dv with
  | DObj obj ->
    obj
    |> Map.toList
    |> List.map (fun pair ->
      match pair with
      | (k, DStr v) -> Ok(k, v)
      | (k, v) ->
        // CLEANUP: this was just to keep the error messages the same with OCaml. It's safe to change the error message
        // Error $"Expected a string, but got: {toDeveloperReprV0 v}"
        Error "expecting str")
    |> Tablecloth.Result.values
  | _ ->
    // CLEANUP As above
    // $"Expected a string, but got: {toDeveloperReprV0 dv}"
    Error "expecting str"
