/// Builtin functions in the HttpClient module
module BuiltinExecution.Libs.HttpClient

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes
module VT = ValueType

type Method = HttpMethod

module Headers =
  type Header = string * string
  type T = List<Header>

type Body = byte array

type Request = { url : string; method : Method; headers : Headers.T; body : Body }

type Response = { statusCode : int; headers : Headers.T; body : Body }


module BadHeader =
  type BadHeader =
    | EmptyKey
    | InvalidContentType

  let toDT (err : BadHeader) : Dval =
    let (caseName, fields) =
      match err with
      | EmptyKey -> "EmptyKey", []
      | InvalidContentType -> "InvalidContentType", []
    let typeName =
      FQTypeName.fqPackage "Darklang" [ "Stdlib"; "HttpClient" ] "BadHeader"
    DEnum(typeName, typeName, [], caseName, fields)

module BadUrl =
  type BadUrlDetails =
    | UnsupportedProtocol
    | InvalidHost
    | InvalidUri
    | InvalidRequest

  let toDT (err : BadUrlDetails) : Dval =
    let (caseName, fields) =
      match err with
      | UnsupportedProtocol -> "UnsupportedProtocol", []
      | InvalidHost -> "InvalidHost", []
      | InvalidUri -> "InvalidUri", []
      | InvalidRequest -> "InvalidRequest", []

    let typeName =
      FQTypeName.fqPackage "Darklang" [ "Stdlib"; "HttpClient" ] "BadUrlDetails"
    Dval.DEnum(typeName, typeName, [], caseName, fields)

module RequestError =
  // inspired by Elm's HttpError type
  // https://package.elm-lang.org/packages/elm/http/latest/Http#Error
  type RequestError =
    | BadUrl of BadUrl.BadUrlDetails
    | Timeout
    | BadHeader of BadHeader.BadHeader
    | NetworkError
    | BadMethod

  let toDT (err : RequestError) : Dval =
    let (caseName, fields) =
      match err with
      | BadUrl details ->
        let details = BadUrl.toDT details
        "BadUrl", [ details ]
      | Timeout -> "Timeout", []
      | BadHeader err ->
        let err = BadHeader.toDT err
        "BadHeader", [ err ]
      | NetworkError -> "NetworkError", []
      | BadMethod -> "BadMethod", []

    let typeName =
      FQTypeName.fqPackage "Darklang" [ "Stdlib"; "HttpClient" ] "RequestError"
    DEnum(typeName, typeName, [], caseName, fields)


type RequestResult = Result<Response, RequestError.RequestError>

type Configuration =
  { timeoutInMs : int
    allowedIP : System.Net.IPAddress -> bool
    allowedHost : string -> bool
    allowedScheme : string -> bool
    allowedHeaders : Headers.T -> bool
    // telemetryInitialize allows us wrap the code with a span
    telemetryInitialize : (unit -> Task<RequestResult>) -> Task<RequestResult>
    telemetryAddTag : string -> obj -> unit
    telemetryAddException : Metadata -> System.Exception -> unit }

module BaseClient =
  // There are a number of different configuration options we want to enable:
  // WASM:
  //   when using Blazor/WASM, dotnet doesn't allow using a SocketsHttpHandler
  //   (errors at runtime). So we need to use a HttpClientHandler instead.
  // Cloud:
  //   when in the cloud, we want to include telemetry, as well as security measures
  //   to prevent access to local infrastructure (this is defense-in-depth: obvi we
  //   also use a firewall)
  // Local:
  //   when running locally, we want to use SocketsHttpHandler and no cloud
  //   features/restrictions.
  //
  // We enable these in two ways:
  // - if SocketsHttpHandler is avaiable (cloud and local), we use that
  // - we provide a Configuration record when initializing, that allows
  //   telemetry/etc. (emptyConfig can be used for no telemetry/etc)


  module SocketBasedHandler =
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
    let handler (config : Configuration) : HttpMessageHandler =
      let connectionFilter
        (context : SocketsHttpConnectionContext)
        (cancellationToken : System.Threading.CancellationToken)
        : ValueTask<Stream> =
        vtask {
          try
            // While this DNS call is expensive, it should be cached
            let ips = System.Net.Dns.GetHostAddresses context.DnsEndPoint.Host

            if not (Array.forall config.allowedIP ips) then
              // Use this to hide more specific errors when looking at loopback
              Exception.raiseInternal "Could not connect" []

            let socket =
              new System.Net.Sockets.Socket(
                System.Net.Sockets.SocketType.Stream,
                System.Net.Sockets.ProtocolType.Tcp
              )
            socket.NoDelay <- true

            do! socket.ConnectAsync(context.DnsEndPoint, cancellationToken)
            return new System.Net.Sockets.NetworkStream(socket, true)
          with :? System.ArgumentException ->
            return Exception.raiseInternal "Could not connect" []
        }
      new SocketsHttpHandler(
        // Avoid DNS problems
        PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
        PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,
        ConnectTimeout = System.TimeSpan.FromSeconds 10.0,

        // HttpClientTODO avail function that handles redirect behaviour
        AllowAutoRedirect = false,

        // Users share the HttpClient, don't let them share cookies!
        UseCookies = false,

        // HttpClientTODO avail functions to compress/decompress with common
        // compression algorithms (gzip, brottli, deflate)
        //
        // HttpClientTODO consider: is there any reason to think that ASP.NET
        // does something fancy such that automatic .net httpclient -level
        // decompression would be notably more efficient than doing so 'manually'
        // via some function? There will certainly be more bytes passed around -
        // probably not a big deal?
        AutomaticDecompression = System.Net.DecompressionMethods.None,

        // Don't add a RequestId header for opentelemetry
        ActivityHeadersPropagator = null,

        // Don't allow access to local stuff
        ConnectCallback = connectionFilter
      )

  module WasmHandler =
    let handler (_config : Configuration) : HttpMessageHandler =
      new HttpClientHandler(
        // These settings are also enabled in SocketBasedHandler - see comments above for discussion
        AllowAutoRedirect = false
      // These can't be set in WASM, even though they exist (PlatformNotSupportedException)
      // UseCookies = false,
      // AutomaticDecompression = System.Net.DecompressionMethods.None
      )

  let create (config : Configuration) : HttpClient =
    let handler =
      if SocketsHttpHandler.IsSupported then
        SocketBasedHandler.handler config
      else
        // For Blazor
        WasmHandler.handler config

    new HttpClient(
      handler,
      disposeHandler = false,
      Timeout = System.TimeSpan.FromSeconds 30.0,
      MaxResponseContentBufferSize = 1024L * 1024L * 100L // 100MB
    )


/// This configuration has no limits, and so is only suitable for trusted
/// environments (the command line), and is not suitable for untrusted environments
/// (eg the cloud)
let defaultConfig =
  { timeoutInMs = 30000
    allowedIP = fun _ -> true
    allowedHost = fun _ -> true
    allowedScheme = fun _ -> true
    allowedHeaders = fun _ -> true
    telemetryInitialize = fun f -> f ()
    telemetryAddTag = fun _ _ -> ()
    telemetryAddException = fun _ _ -> () }

let makeRequest
  (config : Configuration)
  (httpClient : HttpClient)
  (httpRequest : Request)
  : Task<RequestResult> =
  config.telemetryInitialize (fun () ->
    task {
      config.telemetryAddTag "request.url" httpRequest.url
      config.telemetryAddTag "request.method" httpRequest.method
      config.telemetryAddTag "request.method" httpRequest.method
      try
        let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)

        let host = uri.Host.Trim().ToLower()
        if not (config.allowedHost host) then
          return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidHost)
        else if not (config.allowedHeaders httpRequest.headers) then
          return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidRequest)
        else if not (config.allowedScheme uri.Scheme) then
          return Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)
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
              VersionPolicy =
                System.Net.Http.HttpVersionPolicy.RequestVersionOrLower
            )

          // headers
          let headerResults =
            httpRequest.headers
            |> List.map (fun (k, v) ->
              // .NET handles "content headers" separately from other headers.
              // They're put into `req.Content.Headers` rather than `req.Headers`
              // https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.httpcontentheaders?view=net-6.0
              if String.equalsCaseInsensitive k "content-type" then
                try
                  req.Content.Headers.ContentType <-
                    Headers.MediaTypeHeaderValue.Parse(v)
                  Ok()
                with :? System.FormatException ->
                  Error BadHeader.InvalidContentType
              else
                let added = req.Headers.TryAddWithoutValidation(k, v)

                // Headers are split between req.Headers and req.Content.Headers so just try both
                if not added then req.Content.Headers.Add(k, v)
                Ok())


          match Result.collect headerResults with
          | Ok _ ->
            config.telemetryAddTag
              "request.content_type"
              req.Content.Headers.ContentType
            config.telemetryAddTag
              "request.content_length"
              req.Content.Headers.ContentLength

            // Allow timeout
            let source =
              new System.Threading.CancellationTokenSource(config.timeoutInMs)
            let cancellationToken = source.Token

            use! response = httpClient.SendAsync(req, cancellationToken)

            config.telemetryAddTag "response.status_code" response.StatusCode
            config.telemetryAddTag "response.version" response.Version
            use! responseStream = response.Content.ReadAsStreamAsync()
            use memoryStream = new MemoryStream()
            do! responseStream.CopyToAsync(memoryStream)
            let respBody = memoryStream.ToArray()

            let headersForAspNetResponse
              (response : HttpResponseMessage)
              : List<string * string> =
              let fromAspNetHeaders
                (headers : Headers.HttpHeaders)
                : List<string * string> =
                headers
                |> Seq.map Tuple2.fromKeyValuePair
                |> Seq.map (fun (k, v) -> (k, v |> Seq.toList |> String.concat ","))
                |> Seq.toList
              fromAspNetHeaders response.Headers
              @ fromAspNetHeaders response.Content.Headers


            let headers =
              response
              |> headersForAspNetResponse
              |> List.map (fun (k, v) ->
                (String.toLowercase k, String.toLowercase v))

            return
              { statusCode = int response.StatusCode
                headers = headers
                body = respBody }
              |> Ok

          | Error e -> return Error(RequestError.RequestError.BadHeader e)

      with
      | :? TaskCanceledException ->
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Timeout"
        return Error RequestError.RequestError.Timeout

      | :? System.ArgumentException as e when
        e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
        ->
        // We know of one specific case indicating Unsupported Protocol
        // If we get this otherwise, return generic error
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Unsupported Protocol"
        return Error(RequestError.BadUrl BadUrl.BadUrlDetails.UnsupportedProtocol)

      | :? System.UriFormatException ->
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Invalid URI"
        return Error(RequestError.BadUrl BadUrl.BadUrlDetails.InvalidUri)
      | :? IOException -> return Error(RequestError.NetworkError)
      | :? HttpRequestException as e ->
        // This is a bit of an awkward case. I'm unsure how it fits into our model.
        // We've made a request, and _potentially_ (according to .NET) have a status
        // code. That should return some sort of Error - but our Error case type
        // doesn't have a good slot to include the status code. We could have a new
        // case of `| ErrorHandlingResponse of statusCode: int` but that feels wrong.
        let statusCode = if e.StatusCode.HasValue then int e.StatusCode.Value else 0

        config.telemetryAddException [ "error.status_code", statusCode ] e

        return Error(RequestError.NetworkError)
    })


let headersType = TList(TTuple(TString, TString, []))


open LibExecution.Builtin.Shortcuts


let fns (config : Configuration) : List<BuiltInFn> =
  let httpClient = BaseClient.create config
  [ { name = fn "httpClientRequest" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" (TList TUInt8) "" ]
      returnType =
        TypeReference.result
          (TCustomType(
            Ok(FQTypeName.fqPackage "Darklang" [ "Stdlib"; "HttpClient" ] "Response"),
            []
          ))
          (TCustomType(
            Ok(
              FQTypeName.fqPackage
                "Darklang"
                [ "Stdlib"; "HttpClient" ]
                "RequestError"
            ),
            []
          ))
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
        the response is wrapped in {{ Ok }} if a response was successfully
        received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        let typ =
          FQTypeName.fqPackage "Darklang" [ "Stdlib"; "HttpClient" ] "Response"

        let responseType = KTCustomType(typ, [])
        let resultOk = Dval.resultOk responseType KTString
        let typeName = RuntimeError.name [ "HttpClient" ] "RequestError"
        let resultError = Dval.resultError responseType (KTCustomType(typeName, []))
        (function
        | state,
          _,
          [ DString method; DString uri; DList(_, reqHeaders); DList(_, reqBody) ] ->
          uply {
            let! (reqHeaders : Result<List<string * string>, BadHeader.BadHeader>) =
              reqHeaders
              |> Ply.List.mapSequentially (fun item ->
                uply {
                  match item with
                  | DTuple(DString k, DString v, []) ->
                    let k = String.trim k
                    if k = "" then
                      return Error BadHeader.BadHeader.EmptyKey
                    else
                      return Ok((k, v))

                  | notAPair ->
                    let context =
                      TypeChecker.Context.FunctionCallParameter(
                        (FQFnName.fqPackage
                          "Darklang"
                          [ "Stdlib"; "HttpClient" ]
                          "request"),
                        ({ name = "headers"; typ = headersType }),
                        2
                      )
                    return!
                      TypeChecker.raiseValueNotExpectedType
                        state.tracing.callStack
                        notAPair
                        (TList(TTuple(TString, TString, [])))
                        context
                })
              |> Ply.map (Result.collect)

            let method =
              try
                Some(HttpMethod method)
              with _ ->
                None

            let! (result : Result<Dval, RequestError.RequestError>) =
              uply {
                match reqHeaders, method with
                | Ok reqHeaders, Some method ->
                  let request =
                    { url = uri
                      method = method
                      headers = reqHeaders
                      body = Dval.DlistToByteArray reqBody }

                  let! response = makeRequest config httpClient request

                  match response with
                  | Ok response ->
                    let responseHeaders =
                      response.headers
                      |> List.map (fun (k, v) ->
                        DTuple(
                          DString(String.toLowercase k),
                          DString(String.toLowercase v),
                          []
                        ))
                      |> Dval.list (KTTuple(VT.string, VT.string, []))

                    let typ =
                      FQTypeName.fqPackage
                        "Darklang"
                        [ "Stdlib"; "HttpClient" ]
                        "Response"

                    let fields =
                      [ ("statusCode", DInt64(int64 response.statusCode))
                        ("headers", responseHeaders)
                        ("body", Dval.byteArrayToDvalList response.body) ]

                    return Ok(DRecord(typ, typ, [], Map fields) |> resultOk)

                  | Error err -> return Error err

                | Error reqHeadersErr, _ ->
                  let reqHeadersErr = reqHeadersErr
                  return Error(RequestError.RequestError.BadHeader reqHeadersErr)

                | _, None ->
                  let error = RequestError.RequestError.BadMethod
                  return Error error
              }
            match result with
            | Ok result -> return result
            | Error err ->
              let err = RequestError.toDT err
              return resultError err
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins config = Builtin.make [] (fns config)
