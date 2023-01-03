/// StdLib functions in the HttpBaseClient module
module BackendOnlyStdLib.LibHttpBaseClient

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
open LibBackend
open VendoredTablecloth


module HttpBaseClient =
  module Telemetry = LibService.Telemetry

  type HttpResult = { code : int; headers : HttpHeaders.T; body : byte [] }

  // forked from Elm's HttpError type
  // https://package.elm-lang.org/packages/elm/http/latest/Http#Error
  type HttpRequestError =
    | BadUrl of details : string
    | Timeout
    | NetworkError
    | Other of details : string

  type HttpRequestResult = Result<HttpResult, HttpRequestError>

  // There has been quite a history of .NET's HttpClient having problems,
  // including socket exhaustion and DNS results not expiring.
  // The history is outlined well here:
  // https://www.stevejgordon.co.uk/httpclient-connection-pooling-in-dotnet-core
  //
  // As of .NET 6 it seems we no longer need to worry about either socket
  // exhaustion or DNS issues. It appears that we can use either multiple HTTP
  // clients or just one, we use just one for efficiency.
  // See https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests?view=aspnetcore-6.0#alternatives-to-ihttpclientfactory
  //
  // Note that the number of sockets was verified manually, with:
  // `sudo netstat -apn | grep _WAIT`
  // TODO: I don't see where "the number of sockets" is actually configured?
  let private socketHandler : HttpMessageHandler =
    new SocketsHttpHandler(
      // Avoid DNS problems
      PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
      PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,

      // HttpBaseClientTODO avail functions to compress/decompress with common
      // compression algorithms (gzip, brottli, deflate)
      //
      // HttpBaseClientTODO consider: is there any reason to think that ASP.NET
      // does something fancy such that automatic .net httpclient -level
      // decompression would be notably more efficient than doing so 'manually'
      // via some function? There will certainly be more bytes passed around -
      // probably not a big deal?
      AutomaticDecompression = System.Net.DecompressionMethods.None,

      // HttpBaseClientTODO avail function that handles redirect behaviour
      AllowAutoRedirect = false,

      UseProxy = true,
      Proxy = System.Net.WebProxy(Config.httpclientProxyUrl, false),

      // Don't add a RequestId header for opentelemetry
      ActivityHeadersPropagator = null,

      // Users share the HttpClient, don't let them share cookies!
      UseCookies = false
    )

  let private httpClient : HttpClient =
    new HttpClient(
      socketHandler,
      disposeHandler = false,

      // HttpBaseClientTODO are these appropriate defaults?
      // Should users be able to override these somehow?
      Timeout = System.TimeSpan.FromSeconds 30.0,
      MaxResponseContentBufferSize = 1024L * 1024L * 100L // 100MB
    )

  // HttpBaseClientTODO test what happens when user credentials are included
  // in the URL - adjust according to the results.
  let private httpCall
    (url : string)
    (method : HttpMethod)
    (reqHeaders : List<string * string>)
    (reqBody : byte array)
    : Task<HttpRequestResult> =
    task {
      use _ =
        Telemetry.child
          "HttpBaseClient.call"
          [ "request.url", url; "request.method", method ]
      try
        let uri = System.Uri(url, System.UriKind.Absolute)

        // currently we only support http(s) requests
        if uri.Scheme <> "https" && uri.Scheme <> "http" then
          return Error(BadUrl "Unsupported Protocol")
        else
          let reqUri =
            System.UriBuilder(
              Scheme = uri.Scheme,
              Host = uri.Host,
              Port = uri.Port,
              Path = uri.AbsolutePath
            )

          use req =
            new HttpRequestMessage(
              method,
              string reqUri,
              // HttpBaseClientTODO does this mean 'use exactly 3.0'?
              Version = System.Net.HttpVersion.Version30,
              Content = new ByteArrayContent(reqBody)
            )

          // headers
          reqHeaders
          |> List.iter (fun (k, v) ->
            // .NET handles "content headers" separately from other headers.
            // They're put into `req.Content.Headers` rather than `req.Headers`
            // https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.httpcontentheaders?view=net-6.0
            if String.equalsCaseInsensitive k "content-type" then
              try
                req.Content.Headers.ContentType <-
                  Headers.MediaTypeHeaderValue.Parse(v)
              with
              | :? System.FormatException ->
                Exception.raiseCode "Invalid content-type header"
            else
              let added = req.Headers.TryAddWithoutValidation(k, v)

              // Headers are split between req.Headers and req.Content.Headers so just try both
              if not added then req.Content.Headers.Add(k, v))

          // send request
          Telemetry.addTag "request.content_type" req.Content.Headers.ContentType
          Telemetry.addTag "request.content_length" req.Content.Headers.ContentLength
          use! response = httpClient.SendAsync req

          // HttpBaseClientTODO: errors after an HTTP response is returned
          // should include the status code. (right now they don't always).

          Telemetry.addTags [ "response.status_code", response.StatusCode
                              "response.version", response.Version ]
          use! responseStream = response.Content.ReadAsStreamAsync()
          use memoryStream = new MemoryStream()
          do! responseStream.CopyToAsync(memoryStream)
          let respBody = memoryStream.ToArray()

          let headers =
            response
            |> HttpHeaders.headersForAspNetResponse
            |> List.map (fun (k, v) -> (String.toLowercase k, String.toLowercase v))

          return
            { code = int response.StatusCode; headers = headers; body = respBody }
            |> Ok

      with
      | :? TaskCanceledException ->
        Telemetry.addTags [ "error", true; "error.msg", "Timeout" ]
        return Error Timeout

      | :? System.ArgumentException as e -> // We know of one specific case indicating Unsupported Protocol
        if e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')" then
          Telemetry.addTags [ "error", true; "error.msg", "Unsupported Protocol" ]
          return Error(BadUrl "Unsupported Protocol")
        else
          Telemetry.addTags [ "error", true; "error.msg", e.Message ]
          return Error(Other e.Message)

      | :? System.UriFormatException ->
        Telemetry.addTags [ "error", true; "error.msg", "Invalid URI" ]
        return Error(BadUrl "Invalid URI")

      | :? IOException as e -> return Error(Other e.Message)

      | :? HttpRequestException as e ->
        // This is a bit of an awkward case. I'm unsure how it fits into our model.
        // We've made a request, and _potentially_ (according to .NET) have a status
        // code. That should return some sort of Error - but our Error case type
        // doesn't have a good slot to include the status code. We could have a new
        // case of `| ErrorHandlingResponse of statusCode: int` but that feels wrong.
        let code = if e.StatusCode.HasValue then int e.StatusCode.Value else 0

        Telemetry.addException [ "error.status_code", code ] e

        return Error(Other(Exception.getMessages e |> String.concat " "))
    }

  let sendRequest
    (verb : HttpMethod)
    (uri : string)
    (reqHeaders : List<string * string>)
    (reqBody : byte array)
    : Ply<Dval> =
    uply {
      match! httpCall uri verb reqHeaders reqBody with
      | Ok response ->
        let responseHeaders =
          response.headers
          |> List.map (fun (k, v) ->
            DTuple(DStr(String.toLowercase k), DStr(String.toLowercase v), []))
          |> DList

        return
          [ ("code", DInt(int64 response.code))
            ("headers", responseHeaders)
            ("body", DBytes response.body) ]
          |> Dval.obj
          |> Ok
          |> DResult

      | Error err ->
        let errorMsg =
          match err with
          | BadUrl details -> $"Bad URL: {details}"
          | Timeout -> "Request timed out"
          | NetworkError -> "Network error"
          | Other details -> details

        return DResult(Error(DStr errorMsg))
    }


module Errors = LibExecution.Errors

let incorrectArgs = Errors.incorrectArgs

let fn = FQFnName.stdlibFnName

let headersType = TList(TTuple(TStr, TStr, []))

let fns : List<BuiltInFn> =
  [ // Note: although this is a non-internal function, it is 'hidden' behind a
    // 'preview' setting in the editor.
    //
    // HttpBaseClientTODO thorough testing
    { name = fn "HttpBaseClient" "request" 0
      parameters =
        [ Param.make "method" TStr ""
          Param.make "uri" TStr ""
          Param.make "headers" headersType ""
          Param.make "body" TBytes "" ]
      returnType =
        TResult(
          TRecord [ "code", TInt; "headers", headersType; "body", TBytes ],
          TStr
        )
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
        the response is wrapped in {{ Ok }} if a response was successfully
        received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        (function
        | _, [ DStr method; DStr uri; DList reqHeaders; DBytes reqBody ] ->
          let method =
            // Note: this only seems to fail if `method` is a blank string
            try
              Some(HttpMethod method)
            with
            | _ -> None

          let reqHeaders =
            reqHeaders
            |> List.map (fun pair ->
              match pair with
              | DTuple (DStr k, DStr v, []) -> Ok(k, v)
              | other ->
                Error
                  $"Expected a (string * string), but got: {DvalReprDeveloper.toRepr other}")
            |> Tablecloth.Result.values

          // TODO: type error when method is None (probably just blank)
          match reqHeaders, method with
          | Ok reqHeaders, Some method ->
            HttpBaseClient.sendRequest method uri reqHeaders reqBody
          | _ -> incorrectArgs ()
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
