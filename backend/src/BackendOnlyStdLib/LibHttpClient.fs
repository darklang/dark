/// StdLib functions in the HttpClient module
module BackendOnlyStdLib.LibHttpClient

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
open LibBackend
open VendoredTablecloth

module Telemetry = LibService.Telemetry


module HttpClient =
  type Method = HttpMethod

  module Headers =
    type Header = string * string
    type T = List<Header>

  type Body = byte array

  type HttpRequest =
    { url : string
      method : Method
      headers : Headers.T
      body : Body }

  type HttpResult = { statusCode : int; headers : Headers.T; body : Body }

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
  // See https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests?view=aspnetcore-7.0#alternatives-to-ihttpclientfactory
  //
  // Note that the number of sockets was verified manually, with:
  // `sudo netstat -apn | grep _WAIT`
  // TODO: I don't see where "the number of sockets" is actually configured?
  let private socketHandler : HttpMessageHandler =
    new SocketsHttpHandler(
      // Avoid DNS problems
      PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
      PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,

      // HttpClientTODO avail functions to compress/decompress with common
      // compression algorithms (gzip, brottli, deflate)
      //
      // HttpClientTODO consider: is there any reason to think that ASP.NET
      // does something fancy such that automatic .net httpclient -level
      // decompression would be notably more efficient than doing so 'manually'
      // via some function? There will certainly be more bytes passed around -
      // probably not a big deal?
      AutomaticDecompression = System.Net.DecompressionMethods.None,

      // HttpClientTODO avail function that handles redirect behaviour
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
      Timeout = System.TimeSpan.FromSeconds 30.0,
      MaxResponseContentBufferSize = 1024L * 1024L * 100L // 100MB
    )

  let request (httpRequest : HttpRequest) : Task<HttpRequestResult> =
    task {
      use _ =
        Telemetry.child
          "HttpClient.call"
          [ "request.url", httpRequest.url; "request.method", httpRequest.method ]
      try
        let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)

        // currently we only support http(s) requests
        if uri.Scheme <> "https" && uri.Scheme <> "http" then
          return Error(BadUrl "Unsupported Protocol")
        else
          let reqUri =
            System.UriBuilder(
              Scheme = uri.Scheme,
              Host = uri.Host,
              Port = uri.Port,
              Path = uri.AbsolutePath,
              Query = uri.Query
            )
            |> string

          use req =
            new HttpRequestMessage(
              httpRequest.method,
              reqUri,
              Content = new ByteArrayContent(httpRequest.body),

              // Support both Http 2.0 and 3.0
              // https://learn.microsoft.com/en-us/dotnet/api/system.net.http.httpversionpolicy?view=net-7.0
              // TODO: test this (against requestbin or something that allows us to control the HTTP protocol version)
              Version = System.Net.HttpVersion.Version30,
              VersionPolicy = System.Net.Http.HttpVersionPolicy.RequestVersionOrLower
            )

          // headers
          httpRequest.headers
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
            { statusCode = int response.StatusCode
              headers = headers
              body = respBody }
            |> Ok

      with
      | :? TaskCanceledException ->
        Telemetry.addTags [ "error", true; "error.msg", "Timeout" ]
        return Error Timeout

      | :? System.ArgumentException as e ->
        // We know of one specific case indicating Unsupported Protocol
        // If we get this otherwise, return generic error
        //
        // TODO: would this be better as a guard (i.e. `when e.Message = ...`),
        //   leaving other cases un-caught?
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
        let statusCode = if e.StatusCode.HasValue then int e.StatusCode.Value else 0

        Telemetry.addException [ "error.status_code", statusCode ] e

        return Error(Other(Exception.getMessages e |> String.concat " "))
    }


module Errors = LibExecution.Errors

let incorrectArgs = Errors.incorrectArgs

let fn = FQFnName.stdlibFnName

let headersType = TList(TTuple(TStr, TStr, []))



let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "request" 0
      parameters =
        [ Param.make "method" TStr ""
          Param.make "uri" TStr ""
          Param.make "headers" headersType ""
          Param.make "body" TBytes "" ]
      returnType =
        TResult(
          TRecord [ "statusCode", TInt; "headers", headersType; "body", TBytes ],
          TStr
        )
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
        the response is wrapped in {{ Ok }} if a response was successfully
        received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        (function
        | _, [ DStr method; DStr uri; DList reqHeaders; DBytes reqBody ] ->
          let reqHeaders : Result<List<string * string>, string> =
            reqHeaders
            |> List.fold (Ok []) (fun agg item ->
              match agg, item with
              | (Error err, _) -> Error err
              | (Ok pairs, DTuple (DStr k, DStr v, [])) ->
                // TODO: what about whitespace? What else can break?
                if k = "" then
                  Error "Empty request header key provided"
                else
                  Ok((k, v) :: pairs)

              | (_, notAPair) ->
                // this should be a DError, not a "normal" error
                Error
                  $"Expected request headers to be a List of (string * string), but got: {DvalReprDeveloper.toRepr notAPair}")
            |> Result.map (fun pairs -> List.rev pairs)

          let method =
            try
              Some(HttpMethod method)
            with
            | _ -> None

          match reqHeaders, method with
          | Ok reqHeaders, Some method ->
            uply {
              let request : HttpClient.HttpRequest =
                { url = uri; method = method; headers = reqHeaders; body = reqBody }

              let! (response : HttpClient.HttpRequestResult) =
                HttpClient.request request

              match response with
              | Ok response ->
                let responseHeaders =
                  response.headers
                  |> List.map (fun (k, v) ->
                    DTuple(
                      DStr(String.toLowercase k),
                      DStr(String.toLowercase v),
                      []
                    ))
                  |> DList

                return
                  [ ("statusCode", DInt(int64 response.statusCode))
                    ("headers", responseHeaders)
                    ("body", DBytes response.body) ]
                  |> Dval.obj
                  |> Ok
                  |> DResult

              | Error (HttpClient.BadUrl details) ->
                // TODO: include a DvalSource rather than SourceNone
                return DError(SourceNone, $"Bad URL: {details}")

              | Error (HttpClient.Timeout) ->
                return DResult(Error(DStr $"Request timed out"))

              | Error (HttpClient.NetworkError) ->
                return DResult(Error(DStr $"Network error"))

              | Error (HttpClient.Other details) ->
                return DResult(Error(DStr details))
            }

          | Error reqHeadersErr, _ ->
            uply { return DError(SourceNone, reqHeadersErr) }

          | _, None ->
            let error = "Expected valid HTTP method (e.g. 'get' or 'POST')"
            uply { return DError(SourceNone, error) }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
