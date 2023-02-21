/// HttpClient used by LibHttpClient5 StdLib functions
module BackendOnlyStdLib.HttpClient

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.IO
open System.IO.Compression
open System.Net.Http

type AspHeaders = System.Net.Http.Headers.HttpHeaders

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
open LibBackend
open VendoredTablecloth

module Telemetry = LibService.Telemetry

module Errors = LibExecution.Errors

module RT = RuntimeTypes

let incorrectArgs = Errors.incorrectArgs

// Converts an object to (string,string) pairs. Raises an exception if not an object
let toStringPairs (dv : Dval) : Result<List<string * string>, string> =
  match dv with
  | DObj obj ->
    obj
    |> Map.toList
    |> List.map (fun pair ->
      match pair with
      | (k, DStr v) -> Ok(k, v)
      | (_, _) ->
        // CLEANUP: this was just to keep the error messages the same with OCaml. It's safe to change the error message
        // Error $"Expected a string, but got: {toDeveloperReprV0 v}"
        Error "expecting str")
    |> Tablecloth.Result.values
  | _ ->
    // CLEANUP As above
    // $"Expected a string, but got: {toDeveloperReprV0 dv}"
    Error "expecting str"

type HttpResult =
  { body : byte []
    code : int
    headers : HttpHeaders.T
    error : string }

type ClientError = { url : string; error : string; code : int }

// -------------------------
// Forms and queries Functions
// -------------------------

// includes an implicit content-type
type Content =
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

exception InvalidEncodingException of int

let httpCall'
  (url : string)
  (queryParams : (string * string list) list)
  (method : HttpMethod)
  (reqHeaders : HttpHeaders.T)
  (reqBody : Content)
  : Task<Result<HttpResult, ClientError>> =
  task {
    use _ =
      Telemetry.child
        "HttpClient.call"
        [ "request.url", url; "request.method", method ]
    try
      let uri = System.Uri(url, System.UriKind.Absolute)

      // currently we only support http(s) requests
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
        // length of the data. Don't let it be set too large though, as that allows
        // isn't allowed in .NET.
        let contentLengthHeader : Option<int> =
          reqHeaders
          |> List.find (fun (k, _) -> String.equalsCaseInsensitive k "content-length")
          // Note: in ocaml it would send nonsense headers, .NET doesn't allow it
          |> Option.bind (fun (_, v) -> parseInt v)

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

        // headers - get them before content so we know what to do with content-length
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
              // CLEANUP: always use lowercase headers
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

        let headers = HttpHeaders.headersForAspNetResponse response

        // CLEANUP The OCaml version automatically made this lowercase for
        // http2. That's a weird experience for users, as they don't have
        // control over this, so make this lowercase by default
        let headers =
          if isHttp2 then
            List.map (fun (k : string, v) -> (String.toLowercase k, v)) headers
          else
            headers

        let result =
          { body = respBody
            code = code
            headers = [ statusHeader, "" ] @ headers
            error = "" }
        return Ok result
    with
    | InvalidEncodingException code ->
      let error = "Unrecognized or bad HTTP Content or Transfer-Encoding"
      Telemetry.addTags [ "error", true; "error.msg", error ]
      return Error { url = url; code = code; error = error }
    | :? TaskCanceledException -> // only timeouts
      Telemetry.addTags [ "error", true; "error.msg", "Timeout" ]
      return Error { url = url; code = 0; error = "Timeout" }
    | :? System.ArgumentException as e -> // incorrect protocol, possibly more
      let message =
        if e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')" then
          "Unsupported protocol"
        else
          e.Message
      Telemetry.addTags [ "error", true; "error.msg", message ]
      return Error { url = url; code = 0; error = message }
    | :? System.UriFormatException ->
      Telemetry.addTags [ "error", true; "error.msg", "Invalid URI" ]
      return Error { url = url; code = 0; error = "Invalid URI" }
    | :? IOException as e -> return Error { url = url; code = 0; error = e.Message }
    | :? HttpRequestException as e ->
      let code = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
      Telemetry.addException [ "error.status_code", code ] e
      let message = e |> Exception.getMessages |> String.concat " "
      return Error { url = url; code = code; error = message }
  }

/// Uses an internal .NET HttpClient to make a request
/// and process response into an HttpResult
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
    // it felt worth implementing redirects outselves. Note that we did not use
    // recursion within makeHttpCall, to ensure that the HTTP objects/streams were
    // all cleaned up before the redirect happened. We do keep more data in this case
    // than we would like but they're all redirects and so unlikely to have bodies.
    if (count > 50) then
      return Error { url = url; code = 0; error = "Too many redirects" }
    else
      let! response = httpCall' url queryParams method reqHeaders reqBody

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

          // CLEANUP no reason to do this
          // Match curls default redirect behaviour: if it's a POST with content, redirect to GET
          let method, reqBody =
            match reqBody with
            | StringContent body when body <> "" ->
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

// Header utility functions, deliberately kept separate from the Http
// Middleware, as we want to be able to change them separately.

let hasFormHeader (headers : HttpHeaders.T) : bool =
  headers
  |> HttpHeaders.get "content-type"
  |> Option.map Tablecloth.String.toLowercase = Some
                                                  "application/x-www-form-urlencoded"

let hasJsonHeader (headers : HttpHeaders.T) : bool =
  // CLEANUP: don't use contains for this
  HttpHeaders.get "content-type" headers
  |> Option.map (fun s -> s.Contains "application/json")
  |> Option.defaultValue false

let hasTextHeader (headers : HttpHeaders.T) : bool =
  // CLEANUP: don't use contains for this
  HttpHeaders.get "content-type" headers
  |> Option.map (fun s -> s.Contains "text/plain")
  |> Option.defaultValue false


let guessContentType (body : Dval option) : string =
  match body with
  | Some dv ->
    match dv with
    (* TODO: DBytes? *)
    // Do nothing to strings; users can set the header if they have opinions
    | DStr _ -> "text/plain; charset=utf-8"
    // Otherwise, jsonify (this is the 'easy' API afterall), regardless of
    // headers passed. This makes a little more sense than you might think on
    // first glance, due to the interaction with the above `DStr` case. Note that
    // this handles all non-DStr dvals.
    | _ -> "application/json; charset=utf-8"
  // If we were passed an empty body, we need to ensure a Content-Type was set, or
  // else helpful intermediary load balancers will set the Content-Type to something
  // they've plucked out of the ether, which is distinctfully non-helpful and also
  // non-deterministic *)
  | None -> "text/plain; charset=utf-8"


// Encodes [body] as a UTF-8 string, safe for sending across the internet! Uses
// the `Content-Type` header provided by the user in [headers] to make ~magic~ decisions about
// how to encode said body. Returns a tuple of the encoded body, and the passed headers that
// have potentially had a Content-Type added to them based on the magic decision we've made.
let encodeRequestBody (body : Dval option) (headers : HttpHeaders.T) : Content =
  match body with
  | Some dv ->
    match dv with
    // CLEANUP support DBytes
    | DStr s ->
      // Do nothing to strings, ever. The reasoning here is that users do not
      // expect any magic to happen to their raw strings. It's also the only real
      // way (barring Bytes) to support users doing their _own_ encoding (say,
      // jsonifying themselves and passing the Content-Type header manually).
      //
      // CLEANUP find a place for all the notion links
      // See:
      // https://www.notion.so/darklang/Httpclient-Empty-Body-2020-03-10-5fa468b5de6c4261b5dc81ff243f79d9
      // for more information. *)
      StringContent s
    // CLEANUP if there is a charset here, it uses json encoding
    | DObj _ when hasFormHeader headers ->
      match HttpQueryEncoding.toFormEncoding dv with
      | Ok content -> FormContent(content)
      | Error msg -> Exception.raiseCode msg
    | dv when hasTextHeader headers ->
      StringContent(DvalReprLegacyExternal.toEnduserReadableTextV0 dv)
    | _ -> // hasJsonHeader
      StringContent(DvalReprLegacyExternal.toPrettyMachineJsonStringV1 dv)
  | None -> NoContent

/// Used in both Ok and Error cases
let responseType =
  TRecord [ "body", TVariable "responseBody"
            "headers", TDict TStr
            "raw", TStr
            "code", TInt
            "error", TStr ]

let sendRequest
  (uri : string)
  (verb : HttpMethod)
  (reqBody : Dval option)
  (query : Dval)
  (reqHeaders : Dval)
  : Ply<Dval> =
  uply {
    let query = HttpQueryEncoding.toQuery query |> Exception.unwrapResultCode

    // Headers
    let encodedReqHeaders = toStringPairs reqHeaders |> Exception.unwrapResultCode
    let contentType =
      HttpHeaders.get "content-type" encodedReqHeaders
      |> Option.defaultValue (guessContentType reqBody)
    let reqHeaders =
      Map encodedReqHeaders |> Map.add "Content-Type" contentType |> Map.toList

    let encodedReqBody = encodeRequestBody reqBody reqHeaders

    match! httpCall 0 uri query verb reqHeaders encodedReqBody with
    | Ok response ->
      let body = UTF8.ofBytesOpt response.body
      let parsedResponseBody =
        if hasJsonHeader response.headers then
          try
            body
            |> Exception.unwrapOptionInternal "invalid json string" []
            |> DvalReprLegacyExternal.unsafeOfUnknownJsonV0
          with
          | _ -> DStr "json decoding error"
        else
          body |> Option.defaultValue "utf-8 decoding error" |> DStr

      let parsedResponseHeaders =
        response.headers
        |> List.map (fun (k, v) -> (String.trim k, DStr(String.trim v)))
        |> List.filter (fun (k, _) -> String.length k > 0)
        |> Map.ofList
        |> DObj // in old version, this was Dval.obj, however we want to allow duplicates

      let obj =
        Dval.obj [ ("body", parsedResponseBody)
                   ("headers", parsedResponseHeaders)
                   ("raw", body |> Option.defaultValue "utf-8 decoding error" |> DStr)
                   ("code", DInt(int64 response.code))
                   ("error", DStr response.error) ]
      if response.code >= 200 && response.code <= 299 then
        return DResult(Ok obj)
      else
        return DResult(Error obj)
    | Error err -> return DResult(Error(DStr err.error))
  }


let call (method : HttpMethod) =
  (function
  | _, [ DStr uri; body; query; headers ] ->
    sendRequest uri method (Some body) query headers
  | _ -> incorrectArgs ())

let callNoBody (method : HttpMethod) : BuiltInFnSig =
  (function
  | _, [ DStr uri; query; headers ] -> sendRequest uri method None query headers
  | _ -> incorrectArgs ())
