/// StdLib functions in the HttpClient module
module StdLibCloudExecution.Libs.HttpClient

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes

open LibCloud
open VendoredTablecloth

module Telemetry = LibService.Telemetry

// For security, we want to prevent access to internal IP address ranges or
// Instance Metadata service or localhost
// 1. via hostname
// 2. via IP address in the connectionCallback
// 3. via IP tables on the container (see TODO)
// 4. via header for Instance Metadata service
// 5. By removing all access for the cloud run service account (see iam.tf)
module LocalAccess =
  let bannedIPv4Strings (ipStr : string) : bool =
    // This from ChatGPT so verify this before using
    // let bytes = ipAddress.GetAddressBytes() |> Array.rev
    // let ipAsInt = System.BitConverter.ToUInt32(bytes, 0)
    // // Check the following private IP ranges:
    // 10.0.0.0 - 10.255.255.255 (10.0.0.0/8)
    // 172.16.0.0 - 172.31.255.255 (172.16.0.0/12)
    // 192.168.0.0 - 192.168.255.255 (192.168.0.0/16)
    // 169.254.0.0 - 169.254.255.255 (169.254.0.0/16, link-local addresses)
    // todo 127.0.0.1
    // todo 0.0.0.0
    // (ipAsInt >= 0x0A000000u && ipAsInt <= 0x0AFFFFFFu) ||
    // (ipAsInt >= 0xAC100000u && ipAsInt <= 0xAC1FFFFFu) ||
    // (ipAsInt >= 0xC0A80000u && ipAsInt <= 0xC0A8FFFFu) ||
    // (ipAsInt >= 0xA9FE0000u && ipAsInt <= 0xA9FEFFFFu)

    // Slower version
    ipStr.StartsWith("10.0.0.")
    || ipStr.StartsWith("172.16.")
    || ipStr.StartsWith("192.168.")
    || ipStr.StartsWith("169.254.") // covers Instance Metadata service
    || ipStr.StartsWith("127.")
    || ipStr = "0.0.0.0"
    || ipStr = "0"

  let bannedHost (host : string) =
    let host = host.Trim().ToLower()
    // Internal network addresses
    // Localhost
    host = "localhost"
    || host = "metadata"
    || host = "metadata.google.internal"
    || bannedIPv4Strings host


  let bannedIp (ip : System.Net.IPAddress) : bool =
    let bannedIPv4 (ip : System.Net.IPAddress) : bool =
      System.Net.IPAddress.IsLoopback ip // 127.*
      || bannedIPv4Strings (string ip)

    if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6 then
      if ip.IsIPv4MappedToIPv6 then
        bannedIPv4 (ip.MapToIPv4())
      else
        ip.IsIPv6LinkLocal // ipv6 equivalent of 169.254.*
        || ip.IsIPv6SiteLocal // ipv6 equivalent of 10.*.*.*, 172.16.*.* and 192.168.*.*
        || System.Net.IPAddress.IsLoopback ip // 127.*
    else if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork then
      bannedIPv4 ip
    else
      true // not ipv4 or ipv6, so banned


  // Disallow headers that access the Instance Metadata service
  let hasInstanceMetadataHeader (headers : List<string * string>) =
    let eq = String.equalsCaseInsensitive
    headers
    |> List.tryFind (fun (k, v) ->
      let (k, v) = (String.trim k, String.trim v)
      (eq k "Metadata-Flavor" && eq v "Google")
      // Old but allowed https://cloud.google.com/compute/docs/metadata/overview#querying
      || (eq k "X-Google-Metadata-Request" && eq v "True"))
    |> Option.isSome

  let connectionFilter
    (context : SocketsHttpConnectionContext)
    (cancellationToken : System.Threading.CancellationToken)
    : ValueTask<Stream> =
    vtask {
      try
        // While this DNS call is expensive, it should be cached
        let ips = System.Net.Dns.GetHostAddresses context.DnsEndPoint.Host
        ips
        |> Array.iter (fun ip ->
          if bannedIp ip then Exception.raiseInternal "Could not connect" [])

        let socket =
          new System.Net.Sockets.Socket(
            System.Net.Sockets.SocketType.Stream,
            System.Net.Sockets.ProtocolType.Tcp
          )
        socket.NoDelay <- true

        do! socket.ConnectAsync(context.DnsEndPoint, cancellationToken)
        return new System.Net.Sockets.NetworkStream(socket, true)
      with :? System.ArgumentException ->
        // Use this to hide more specific errors when looking at loopback
        return Exception.raiseInternal "Could not connect" []
    }


module HttpClient =
  type Method = HttpMethod

  module Headers =
    type Header = string * string
    type T = List<Header>

  type Body = byte array

  type HttpRequest =
    { url : string; method : Method; headers : Headers.T; body : Body }

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
  let socketHandler (allowLocalConnections : bool) : HttpMessageHandler =
    let handler =
      new SocketsHttpHandler(
        // Avoid DNS problems
        PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
        PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,
        ConnectTimeout = System.TimeSpan.FromSeconds 10.0,

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

        // Don't add a RequestId header for opentelemetry
        ActivityHeadersPropagator = null,

        // Users share the HttpClient, don't let them share cookies!
        UseCookies = false
      )
    if not allowLocalConnections then
      handler.ConnectCallback <- LocalAccess.connectionFilter
    handler


  let private makeHttpClient (allowLocalConnections : bool) : HttpClient =
    new HttpClient(
      socketHandler allowLocalConnections,
      disposeHandler = false,
      Timeout = System.TimeSpan.FromSeconds 30.0,
      MaxResponseContentBufferSize = 1024L * 1024L * 100L // 100MB
    )

  let private localAllowedHttpClient = makeHttpClient true
  let private localDisallowedHttpClient = makeHttpClient false

  let request
    (localAccessAllowed : bool)
    (timeoutInMs : int)
    (httpRequest : HttpRequest)
    : Task<HttpRequestResult> =
    task {
      use _ =
        Telemetry.child
          "HttpClient.call"
          [ "request.url", httpRequest.url; "request.method", httpRequest.method ]
      try
        let uri = System.Uri(httpRequest.url, System.UriKind.Absolute)


        let host = uri.Host.Trim().ToLower()
        if not localAccessAllowed && LocalAccess.bannedHost host then
          return Error(BadUrl "Invalid host")
        else if
          not localAccessAllowed
          && LocalAccess.hasInstanceMetadataHeader httpRequest.headers
        then
          return Error(BadUrl "Invalid request")

        // currently we only support http(s) requests
        else if uri.Scheme <> "https" && uri.Scheme <> "http" then
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
              with :? System.FormatException ->
                Exception.raiseCode "Invalid content-type header"
            else
              let added = req.Headers.TryAddWithoutValidation(k, v)

              // Headers are split between req.Headers and req.Content.Headers so just try both
              if not added then req.Content.Headers.Add(k, v))

          // send request
          Telemetry.addTag "request.content_type" req.Content.Headers.ContentType
          Telemetry.addTag "request.content_length" req.Content.Headers.ContentLength

          // Allow timeout
          let cancellationToken =
            (new System.Threading.CancellationTokenSource(timeoutInMs)).Token

          use! response =
            if localAccessAllowed then
              localAllowedHttpClient.SendAsync(req, cancellationToken)
            else
              localDisallowedHttpClient.SendAsync(req, cancellationToken)

          Telemetry.addTags
            [ "response.status_code", response.StatusCode
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
        if
          e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
        then
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


let headersType = TList(TTuple(TString, TString, []))

type HeaderError =
  | BadInput of string
  | TypeMismatch of string

open LibExecution.StdLib.Shortcuts

let types : List<BuiltInType> =
  [ { name = typ [ "HttpClient" ] "Response" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              NEList.ofList
                { name = "statusCode"; typ = TInt }
                [ { name = "headers"; typ = headersType }
                  { name = "body"; typ = TBytes } ]
            ) }
      description = "The response from a HTTP request"
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn [ "HttpClient" ] "request" 0
      typeParams = []
      parameters =
        [ Param.make "method" TString ""
          Param.make "uri" TString ""
          Param.make "headers" headersType ""
          Param.make "body" TBytes "" ]
      returnType =
        TypeReference.result
          (TCustomType(FQName.BuiltIn(typ [ "HttpClient" ] "Response" 0), []))
          TString
      description =
        "Make blocking HTTP call to <param uri>. Returns a <type Result> where
        the response is wrapped in {{ Ok }} if a response was successfully
        received and parsed, and is wrapped in {{ Error }} otherwise"
      fn =
        (function
        | state,
          _,
          [ DString method; DString uri; DList(t, reqHeaders); DBytes reqBody ] ->
          let reqHeaders : Result<List<string * string>, HeaderError> =
            reqHeaders
            |> List.fold (Ok []) (fun agg item ->
              match agg, item with
              | (Error err, _) -> Error err
              | (Ok pairs, DTuple(DString k, DString v, [])) ->
                // TODO: what about whitespace? What else can break?
                if k = "" then
                  BadInput "Empty request header key provided" |> Error
                else
                  Ok((k, v) :: pairs)

              | (_, notAPair) ->
                // this should be a DError, not a "normal" error
                TypeMismatch
                  $"Expected request headers to be a `List<String*String>`, but got: {DvalReprDeveloper.toRepr notAPair}"
                |> Error)
            |> Result.map (fun pairs -> List.rev pairs)

          let method =
            try
              Some(HttpMethod method)
            with _ ->
              None

          match reqHeaders, method with
          | Ok reqHeaders, Some method ->
            uply {
              let request : HttpClient.HttpRequest =
                { url = uri; method = method; headers = reqHeaders; body = reqBody }

              let! (response : HttpClient.HttpRequestResult) =
                HttpClient.request
                  state.config.allowLocalHttpAccess
                  state.config.httpclientTimeoutInMs
                  request

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
                  |> Dval.list (Known(KTTuple(Known KTString, Known KTString, [])))

                let typ =
                  FQName.BuiltIn(TypeName.builtIn [ "HttpClient" ] "Response" 0)

                return
                  [ ("statusCode", DInt(int64 response.statusCode))
                    ("headers", responseHeaders)
                    ("body", DBytes response.body) ]
                  |> Dval.record typ
                  |> Dval.resultOk

              | Error(HttpClient.BadUrl details) ->
                // TODO: include a DvalSource rather than SourceNone
                return Dval.resultError (DString $"Bad URL: {details}")

              | Error(HttpClient.Timeout) ->
                return Dval.resultError (DString $"Request timed out")

              | Error(HttpClient.NetworkError) ->
                return Dval.resultError (DString $"Network error")

              | Error(HttpClient.Other details) ->
                return Dval.resultError (DString details)
            }

          | Error reqHeadersErr, _ ->
            uply {
              match reqHeadersErr with
              | BadInput details -> return Dval.resultError (DString details)
              | TypeMismatch details -> return DError(SourceNone, details)
            }

          | _, None ->
            let error = "Expected valid HTTP method (e.g. 'get' or 'POST')"
            uply { return Dval.resultError (DString error) }

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []

let contents = (fns, types, constants)
