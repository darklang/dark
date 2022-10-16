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

  type private HttpResult = { body : byte []; code : int; headers : HttpHeaders.T }
  type private ClientError = { error : string; code : int option }

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
  // TODO: I don't see where "the nunber of sockets" is actually configured?
  let private socketHandler : HttpMessageHandler =
    new SocketsHttpHandler(
      // Avoid DNS problems
      PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
      PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,

      // HttpBaseClientTODO avail functions to compress/decompress with common
      // compression algorithms (gzip, brottli, deflate)
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
    : Task<Result<HttpResult, ClientError>> =
    task {
      use _ =
        Telemetry.child
          "HttpBaseClient.call"
          [ "request.url", url; "request.method", method ]
      try
        let uri = System.Uri(url, System.UriKind.Absolute)

        // currently we only support http(s) requests
        if uri.Scheme <> "https" && uri.Scheme <> "http" then
          return Error { code = None; error = "Unsupported protocol" }
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
            { body = respBody; code = int response.StatusCode; headers = headers }
            |> Ok
      with
      | :? TaskCanceledException -> // only timeouts
        Telemetry.addTags [ "error", true; "error.msg", "Timeout" ]
        return Error { code = None; error = "Timeout" }
      | :? System.ArgumentException as e -> // incorrect protocol, possibly more
        let message =
          if e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')" then
            "Unsupported protocol"
          else
            e.Message
        Telemetry.addTags [ "error", true; "error.msg", message ]
        return Error { code = None; error = message }
      | :? System.UriFormatException ->
        Telemetry.addTags [ "error", true; "error.msg", "Invalid URI" ]
        return Error { code = None; error = "Invalid URI" }
      | :? IOException as e -> return Error { code = None; error = e.Message }
      | :? HttpRequestException as e ->
        let code = if e.StatusCode.HasValue then int e.StatusCode.Value else 0
        Telemetry.addException [ "error.status_code", code ] e
        let message = e |> Exception.getMessages |> String.concat " "
        return Error { code = Some code; error = message }
    }

  let sendRequest
    (uri : string)
    (verb : HttpMethod)
    (reqBody : byte array)
    (reqHeaders : List<string * string>)
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
          [ ("body", DBytes response.body)
            ("headers", responseHeaders)
            ("code", DInt(int64 response.code)) ]
          |> Dval.obj
          |> Ok
          |> DResult
      | Error err -> return DResult(Error(DStr err.error))
    }


module Errors = LibExecution.Errors

let incorrectArgs = Errors.incorrectArgs

let fn = FQFnName.stdlibFnName

let headersType = TList(TTuple(TStr, TStr, []))

let parameters =
  [ // HttpBaseClientTODO consider method being a new type (DU).
    // Alternatively, leave it as a string, and don't try to parse it.
    // (instead, just do `new HttpMethod(userInputMethod)`)
    Param.make "method" TStr ""

    // HttpBaseClientTODO consider URI being a new type (complex type)
    Param.make "uri" TStr ""

    Param.make "body" TBytes ""

    Param.make "headers" headersType "" ]

let returnType =
  TResult(TRecord [ "body", TBytes; "headers", headersType; "code", TInt ], TStr)

// HttpBaseClientTODO maybe the errors should be in the form of a 'custom' DU?
// Elm's http response type is well thought out and could provide a good model
// for us. https://package.elm-lang.org/packages/elm/http/latest/Http.
// In other words, `returnType` above may be adjusted significantly.

let fns : List<BuiltInFn> =
  [ // HttpBaseClientTODO expose as non-internal fn
    // HttpBaseClientTODO better name than 'call'?
    // HttpBaseClientTODO thorough testing
    { name = fn "DarkInternal" "baseHttpClientCall" 0
      parameters = parameters
      returnType = returnType
      description =
        // HttpBaseClientTODO better description
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
        the response is wrapped in <code Ok> if a response was successfully
        received and parsed, and is wrapped in <code Error> otherwise"
      fn =
        // HttpBaseClientTODO expose as non-internal fn
        LibDarkInternal.internalFn (function
          | _, [ DStr method; DStr uri; DBytes body; DList headers ] ->
            let method =
              match String.toLowercase method with
              | "get" -> Some HttpMethod.Get
              | "post" -> Some HttpMethod.Post
              | "put" -> Some HttpMethod.Put
              | "patch" -> Some HttpMethod.Patch
              | "delete" -> Some HttpMethod.Delete
              | "head" -> Some HttpMethod.Head
              | "options" -> Some HttpMethod.Options
              | _ -> None

            let headers =
              headers
              |> List.map (fun pair ->
                match pair with
                | DTuple (DStr k, DStr v, []) -> Ok(k, v)
                | other ->
                  Error
                    $"Expected a (string * string), but got: {DvalReprDeveloper.toRepr other}")
              |> Tablecloth.Result.values

            // HttpBaseClientTODO return better error messages
            match headers, method with
            | Ok headers, Some method ->
              HttpBaseClient.sendRequest uri method body headers
            | _ -> incorrectArgs ()
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
