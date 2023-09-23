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


module HeaderError =
  type HeaderError = | EmptyKey

  let toDT (err : HeaderError) : Dval =
    let caseName, fields =
      match err with
      | EmptyKey -> "EmptyKey", []

    let typeName =
      TypeName.fqPackage "Darklang" [ "Stdlib"; "HttpClient" ] "HeaderError" 0
    Dval.enum typeName typeName (Some []) caseName fields

module RequestError =
  // forked from Elm's HttpError type
  // https://package.elm-lang.org/packages/elm/http/latest/Http#Error
  type RequestError =
    | BadUrl of details : string
    | Timeout
    | NetworkError
    | HeaderError of HeaderError.HeaderError
    | Other of details : string

  let toDT (err : RequestError) : Dval =
    let caseName, fields =
      match err with
      | BadUrl details -> "BadUrl", [ DString details ]
      | Timeout -> "Timeout", []
      | NetworkError -> "NetworkError", []
      | HeaderError err -> "HeaderError", [ HeaderError.toDT err ]
      | Other details -> "Other", [ DString details ]

    let typeName =
      TypeName.fqPackage "Darklang" [ "Stdlib"; "HttpClient" ] "RequestError" 0
    Dval.enum typeName typeName (Some []) caseName fields


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
          return Error(RequestError.RequestError.BadUrl "Invalid host")
        else if not (config.allowedHeaders httpRequest.headers) then
          return Error(RequestError.RequestError.BadUrl "Invalid request")
        else if not (config.allowedScheme uri.Scheme) then
          return Error(RequestError.RequestError.BadUrl "Unsupported Protocol")
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
          httpRequest.headers
          |> List.iter (fun (k, v) ->
            // .NET handles "content headers" separately from other headers.
            // They're put into `req.Content.Headers` rather than `req.Headers`
            // https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.httpcontentheaders?view=net-6.0
            if String.equalsCaseInsensitive k "content-type" then
              try
                req.Content.Headers.ContentType <-
                  Headers.MediaTypeHeaderValue.Parse(v)
              with :? System.FormatException ->
                raiseString "Invalid content-type header"
            else
              let added = req.Headers.TryAddWithoutValidation(k, v)

              // Headers are split between req.Headers and req.Content.Headers so just try both
              if not added then req.Content.Headers.Add(k, v))

          // send request
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
            |> List.map (fun (k, v) -> (String.toLowercase k, String.toLowercase v))

          return
            { statusCode = int response.StatusCode
              headers = headers
              body = respBody }
            |> Ok

      with
      | :? TaskCanceledException ->
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Timeout"
        return Error RequestError.RequestError.Timeout

      | :? System.ArgumentException as e ->
        // We know of one specific case indicating Unsupported Protocol
        // If we get this otherwise, return generic error
        //
        // TODO: would this be better as a guard (i.e. `when e.Message = ...`),
        //   leaving other cases un-caught?
        if
          e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
        then
          config.telemetryAddTag "error" true
          config.telemetryAddTag "error.msg" "Unsupported Protocol"
          return Error(RequestError.RequestError.BadUrl "Unsupported Protocol")
        else
          config.telemetryAddTag "error" true
          config.telemetryAddTag "error.msg" e.Message
          return Error(RequestError.RequestError.Other e.Message)

      | :? System.UriFormatException ->
        config.telemetryAddTag "error" true
        config.telemetryAddTag "error.msg" "Invalid URI"
        return Error(RequestError.RequestError.BadUrl "Invalid URI")

      | :? IOException as e ->
        return Error(RequestError.RequestError.Other e.Message)

      | :? HttpRequestException as e ->
        // This is a bit of an awkward case. I'm unsure how it fits into our model.
        // We've made a request, and _potentially_ (according to .NET) have a status
        // code. That should return some sort of Error - but our Error case type
        // doesn't have a good slot to include the status code. We could have a new
        // case of `| ErrorHandlingResponse of statusCode: int` but that feels wrong.
        let statusCode = if e.StatusCode.HasValue then int e.StatusCode.Value else 0

        config.telemetryAddException [ "error.status_code", statusCode ] e

        return
          Error(
            RequestError.RequestError.Other(
              Exception.getMessages e |> String.concat " "
            )
          )
    })


let headersType = TList(TTuple(TString, TString, []))


open LibExecution.Builtin.Shortcuts

let types : List<BuiltInType> = []

let fns (config : Configuration) : List<BuiltInFn> =
  let httpClient = BaseClient.create config
  [ { name = fn [ "HttpClient" ] "request" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" TBytes "" ]
      returnType =
        TypeReference.result
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "HttpClient" ]
                  name = TypeName.TypeName "Response"
                  version = 0 }
            ),
            []
          ))
          (TCustomType(
            Ok(
              FQName.Package
                { owner = "Darklang"
                  modules = [ "Stdlib"; "HttpClient" ]
                  name = TypeName.TypeName "RequestError"
                  version = 0 }
            ),
            []
          ))
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
        the response is wrapped in {{ Ok }} if a response was successfully
        received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        let responseType =
          KTCustomType(FQName.BuiltIn(typ [ "HttpClient" ] "Response" 0), [])
        let resultOk = Dval.resultOk responseType KTString
        let resultErrorStr str = Dval.resultError responseType KTString (DString str)
        let typeName = RuntimeError.name [ "HttpClient" ] "RequestError" 0
        let resultError =
          Dval.resultError VT.unknownTODO VT.unknownTODO
        let resultErrorStr str =
          Dval.resultError VT.unknownTODO VT.string (DString str)
        (function
        | _, _, [ DString method; DString uri; DList(_, reqHeaders); DBytes reqBody ] ->
          let reqHeaders : Result<List<string * string>, HeaderError.HeaderError> =
            reqHeaders
            |> List.fold
              (fun agg item ->
                match agg, item with
                | (Error err, _) -> Error err
                | (Ok pairs, DTuple(DString k, DString v, [])) ->
                  // TODO: what about whitespace? What else can break?
                  if k = "" then
                    Error HeaderError.HeaderError.EmptyKey
                  else
                    Ok((k, v) :: pairs)

                | (_, notAPair) ->
                  raiseUntargetedRTE (
                    RuntimeError.oldError
                      $"Expected request headers to be a `List<String*String>`, but got: {DvalReprDeveloper.toRepr notAPair}"
                  ))
              (Ok [])
            |> Result.map (fun pairs -> List.rev pairs)

          let method =
            try
              Some(HttpMethod method)
            with _ ->
              None

          match reqHeaders, method with
          | Ok reqHeaders, Some method ->
            uply {
              let request =
                { url = uri; method = method; headers = reqHeaders; body = reqBody }

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
                  FQName.Package
                    { owner = "Darklang"
                      modules = [ "Stdlib"; "HttpClient" ]
                      name = TypeName.TypeName "Response"
                      version = 0 }

                let fields =
                  [ ("statusCode", DInt(int64 response.statusCode))
                    ("headers", responseHeaders)
                    ("body", DBytes response.body) ]

                return DRecord(typ, typ, [], Map fields) |> resultOk

              // TODO: include a DvalSource rather than SourceNone

              | Error(err) -> return (err |> RequestError.toDT |> resultError)

            }

          | Error reqHeadersErr, _ ->
            uply { return reqHeadersErr |> HeaderError.toDT |> resultError }

          | _, None ->
            let error = "Expected valid HTTP method (e.g. 'get' or 'POST')"
            uply { return resultErrorStr error }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []

let contents config = (fns config, types, constants)
