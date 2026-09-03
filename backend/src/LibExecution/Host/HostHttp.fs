/// Host-owned HTTP: client configuration, SSRF guards, the send machinery,
/// and the streaming-response table.
///
/// Moved from the HTTP client builtins so the checked boundary owns the
/// network. Two configurations exist: the guest one (SSRF-guarded by default;
/// a host may replace it with `setGuestConfig`) and the trusted sync one. An
/// `Operation.HttpRequest` names which it wants with `HostTypes.HttpProfile`,
/// so timeouts and allow-lists stay in host-owned state.
module LibExecution.HostHttp

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open LibExecution.HostTypes

type private Headers = List<string * string>

/// The per-profile knobs: the request timeout and the SSRF guards. Scheme
/// (http/https only) and the metadata-header ban are not configurable.
type Configuration =
  { timeoutInMs : int
    allowedIP : System.Net.IPAddress -> bool
    allowedHost : string -> bool }

module private BaseClient =
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
  // - we provide a Configuration record when initializing, that carries the
  //   allow-lists and timeout


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
            // While this DNS call is expensive, it should be cached.
            //
            // Connect against the resolved IPs, not the hostname: the OS
            // would otherwise re-resolve at connect time and a malicious
            // resolver can return public-then-private within ms,
            // bypassing the allow-list (DNS-rebinding TOCTOU).
            let ips = System.Net.Dns.GetHostAddresses context.DnsEndPoint.Host

            if Array.isEmpty ips || not (Array.forall config.allowedIP ips) then
              // Use this to hide more specific errors when looking at loopback
              Exception.raiseInternal "Could not connect" []

            // TRY EVERY resolved address, not just the first. A name routinely
            // resolves to more than one (`localhost` is ::1 AND 127.0.0.1), only
            // some of which have anything listening, and the order is the resolver's
            // to choose. Connecting to ips[0] alone makes `http://localhost:<port>`
            // fail against a server bound to IPv4, and report it as a flat "network
            // error".
            //
            // Every address was already checked against the allow-list above, so
            // trying the rest widens nothing: the DNS-rebinding guard is that we
            // connect to a resolved ADDRESS rather than let the OS re-resolve the
            // name, and that still holds for each one.
            //
            // The socket's address family has to match the address it dials. The
            // 2-arg ctor defaults to IPv6, which only reaches an IPv4 literal when
            // the host has dual-mode, and containers often don't.
            let mutable stream : Stream = null
            let mutable lastError : exn = null
            let mutable i = 0

            while isNull stream && i < ips.Length do
              let ip = ips[i]
              i <- i + 1

              let socket =
                new System.Net.Sockets.Socket(
                  ip.AddressFamily,
                  System.Net.Sockets.SocketType.Stream,
                  System.Net.Sockets.ProtocolType.Tcp
                )
              socket.NoDelay <- true

              try
                let endpoint = System.Net.IPEndPoint(ip, context.DnsEndPoint.Port)
                do! socket.ConnectAsync(endpoint, cancellationToken)
                stream <- new System.Net.Sockets.NetworkStream(socket, true)
              with e ->
                socket.Dispose()
                lastError <- e
                // A cancelled request is the caller giving up, not this address
                // being wrong. Trying the next one would ignore the timeout and dial
                // again on a token that's already done.
                if cancellationToken.IsCancellationRequested then i <- ips.Length

            match stream with
            | null ->
              return
                Exception.raiseInternal
                  "Could not connect"
                  [ "reason",
                    (if isNull lastError then "no address" else lastError.Message) ]
            | s -> return s
          with :? System.ArgumentException ->
            return Exception.raiseInternal "Could not connect" []
        }
      new SocketsHttpHandler(
        // Avoid DNS problems
        PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
        PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,
        ConnectTimeout = System.TimeSpan.FromSeconds 10.0,

        // Redirects are disabled; callers must opt into each target explicitly.
        AllowAutoRedirect = false,

        // Users share the HttpClient, don't let them share cookies!
        UseCookies = false,

        // Compression is disabled so response handling stays explicit.
        AutomaticDecompression = System.Net.DecompressionMethods.None,

        // Don't add a RequestId header for opentelemetry
        ActivityHeadersPropagator = null,

        // Don't allow access to local stuff
        ConnectCallback = connectionFilter
      )

  module WasmHandler =
    let handler (_config : Configuration) : HttpMessageHandler =
      new HttpClientHandler(
        // These settings are also enabled in SocketBasedHandler - see comments above
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


/// SSRF guard: predicates that block access to internal IP ranges,
/// loopback, link-local, and well-known cloud-provider metadata
/// endpoints.
///
/// Used by `defaultConfig` (below) to compose a configuration safe
/// for HTTP servers that execute untrusted handler code. Exposed
/// directly so callers can build a custom configuration that mixes
/// these predicates with their own allow-lists.
module LocalAccess =

  // RFC 1918 private ranges
  // https://datatracker.ietf.org/doc/html/rfc1918#section-3
  let private ten = System.Net.IPNetwork.Parse "10.0.0.0/8"
  let private oneSevenTwo = System.Net.IPNetwork.Parse "172.16.0.0/12"
  let private oneNineTwo = System.Net.IPNetwork.Parse "192.168.0.0/16"

  // RFC 6598 carrier-grade NAT
  // https://datatracker.ietf.org/doc/html/rfc6598#section-7
  let private oneHundred = System.Net.IPNetwork.Parse "100.64.0.0/10"

  // Google Cloud Run private endpoints (199.36.153.4/30, 199.36.153.8/30)
  let private oneNineNineFour = System.Net.IPNetwork.Parse "199.36.153.4/30"
  let private oneNineNineEight = System.Net.IPNetwork.Parse "199.36.153.8/30"

  // 169.254.0.0/16 — link-local addresses (incl. cloud metadata IPs)
  let private oneSixNine = System.Net.IPNetwork.Parse "169.254.0.0/16"

  let private zero = System.Net.IPAddress.Parse "0.0.0.0"
  // IPv6 unique-local addresses, the IPv6 equivalent of RFC 1918 space.
  let private uniqueLocal = System.Net.IPNetwork.Parse "fc00::/7"

  let bannedIPv4 (ip : System.Net.IPAddress) : bool =
    System.Net.IPAddress.IsLoopback ip // 127.*
    || ten.Contains ip
    || oneSevenTwo.Contains ip
    || oneNineTwo.Contains ip
    || oneHundred.Contains ip
    || oneNineNineFour.Contains ip
    || oneNineNineEight.Contains ip
    || oneSixNine.Contains ip
    || zero = ip

  let bannedIp (ip : System.Net.IPAddress) : bool =
    if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6 then
      if ip.IsIPv4MappedToIPv6 then
        bannedIPv4 (ip.MapToIPv4())
      else
        uniqueLocal.Contains ip
        || ip.IsIPv6LinkLocal // ipv6 equivalent of 169.254.*
        || ip.IsIPv6SiteLocal // ipv6 equivalent of 10/172.16/192.168
        || System.Net.IPAddress.IsLoopback ip
    else if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork then
      bannedIPv4 ip
    else
      true // not ipv4 or ipv6, so banned

  /// Ranges that remain blocked for trusted tailnet sync pulls: link-local,
  /// cloud-metadata, GCP private endpoints, and 0.0.0.0. Loopback, RFC-1918,
  /// and Tailscale's CGN range stay reachable so LAN peers can be used.
  let private metadataOrLinkLocalV4 (ip : System.Net.IPAddress) : bool =
    oneSixNine.Contains ip
    || oneNineNineFour.Contains ip
    || oneNineNineEight.Contains ip
    || zero = ip

  let metadataOrLinkLocal (ip : System.Net.IPAddress) : bool =
    if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6 then
      if ip.IsIPv4MappedToIPv6 then
        metadataOrLinkLocalV4 (ip.MapToIPv4())
      else
        ip.IsIPv6LinkLocal // ipv6 link-local / metadata
    else if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork then
      metadataOrLinkLocalV4 ip
    else
      true

  let bannedHost (host : string) : bool =
    let host = host.Trim().ToLower()
    let badIP =
      let mutable ip = null
      if System.Net.IPAddress.TryParse(host, &ip) then bannedIp ip else false
    badIP
    || host = "localhost"
    || host = "metadata"
    || host = "metadata.google.internal"

  /// Disallow headers that would request the GCP Instance Metadata service.
  let hasInstanceMetadataHeader (headers : Headers) : bool =
    let eq = String.equalsCaseInsensitive
    headers
    |> List.find (fun (k, v) ->
      let (k, v) = (String.trim k, String.trim v)
      (eq k "Metadata-Flavor" && eq v "Google")
      // Old but allowed
      // https://cloud.google.com/compute/docs/metadata/overview#querying
      || (eq k "X-Google-Metadata-Request" && eq v "True"))
    |> Option.isSome


/// Default guest configuration. SSRF guards on (no internal IPs, no
/// loopback, no link-local, no cloud-metadata endpoints). Safe to wire into
/// anything that runs untrusted Dark code (e.g. `darklang serve` handlers,
/// package initialisation evals).
let defaultConfig : Configuration =
  { timeoutInMs = 30000
    allowedIP = fun ip -> not (LocalAccess.bannedIp ip)
    allowedHost = fun host -> not (LocalAccess.bannedHost host) }

/// Config for trusted tailnet SYNC pulls (`httpGetUnsafeBytes`). Unlike
/// `defaultConfig` it reaches loopback / RFC-1918 / the Tailscale range so a
/// peer's server is reachable — but it still blocks cloud-metadata +
/// link-local, so even a sync pull can't be aimed at 169.254.169.254.
let private syncConfig : Configuration =
  { timeoutInMs = 30000
    allowedIP = fun ip -> not (LocalAccess.metadataOrLinkLocal ip)
    allowedHost = fun _ -> true }


// ── profiles ──────────────────────────────────────────────────────────────────

/// A configuration and the client built for it. Host-side only: operations
/// name a profile, they never carry one.
type Profile = { config : Configuration; client : HttpClient }

let private profileOf (config : Configuration) : Profile =
  { config = config; client = BaseClient.create config }

let mutable private guest : Profile = profileOf defaultConfig

let private sync : Profile = profileOf syncConfig

/// Replace the guest configuration; the client is rebuilt and shared for the
/// configuration's lifetime. A host that needs loopback reachable (a test
/// harness with an in-process server) sets it here before guest execution.
let setGuestConfig (config : Configuration) : unit = guest <- profileOf config

let profileFor (profile : HttpProfile) : Profile =
  match profile with
  | HttpProfile.Guest -> guest
  | HttpProfile.Sync -> sync


// ── request prep ──────────────────────────────────────────────────────────────

/// Run the configured allow-lists over the request and return the canonical
/// request URI. Malformed input becomes a typed error, never an exception and
/// never a wildcard.
let private validateAndBuildUri
  (config : Configuration)
  (url : string)
  (headers : Headers)
  : Result<string, HttpRequestError> =
  try
    let uri = System.Uri(url, System.UriKind.Absolute)
    let host = uri.Host.Trim().ToLower()
    if not (config.allowedHost host) then
      Error(HttpRequestError.BadUrl HttpBadUrl.InvalidHost)
    elif LocalAccess.hasInstanceMetadataHeader headers then
      Error(HttpRequestError.BadUrl HttpBadUrl.InvalidRequest)
    elif uri.Scheme <> "https" && uri.Scheme <> "http" then
      Error(HttpRequestError.BadUrl HttpBadUrl.UnsupportedProtocol)
    else
      System.UriBuilder(
        Scheme = uri.Scheme,
        Host = uri.Host,
        Port = uri.Port,
        Path = uri.AbsolutePath,
        Query = uri.Query
      )
      |> string
      |> Ok
  with
  | :? System.UriFormatException ->
    Error(HttpRequestError.BadUrl HttpBadUrl.InvalidUri)
  | :? System.ArgumentException ->
    Error(HttpRequestError.BadUrl HttpBadUrl.InvalidUri)

/// Validate method, URL, and the profile's allow-lists; return the canonical
/// URI the permission layer checks and the send uses, with the resolved
/// profile so the send need not look it up again. `Error` is a guest-visible
/// typed error.
let prepare
  (profile : HttpProfile)
  (method : string)
  (url : string)
  (headers : Headers)
  : Result<string * Profile, HttpRequestError> =
  let profile = profileFor profile
  let method =
    try
      Some(HttpMethod method)
    with _ ->
      None
  match method with
  | None -> Error HttpRequestError.BadMethod
  | Some _ ->
    validateAndBuildUri profile.config url headers
    |> Result.map (fun uri -> (uri, profile))


/// Build the request message used for both the buffered and the
/// streaming path. Header version policy is the same in both cases.
let private buildHttpRequestMessage
  (method : HttpMethod)
  (reqUri : string)
  (body : byte[])
  : HttpRequestMessage =
  new HttpRequestMessage(
    method,
    reqUri,
    Content = new ByteArrayContent(body),
    // Support both Http 2.0 and 3.0
    // https://learn.microsoft.com/en-us/dotnet/api/system.net.http.httpversionpolicy?view=net-7.0
    Version = System.Net.HttpVersion.Version30,
    VersionPolicy = HttpVersionPolicy.RequestVersionOrLower
  )


/// Apply the caller-supplied headers to `req`. Content-Type goes on
/// `req.Content.Headers`; everything else lives on `req.Headers`
/// unless .NET rejects it (then we fall back to Content.Headers).
/// Returns `Error InvalidContentType` if Content-Type can't be
/// parsed; otherwise `Ok ()`.
let private applyRequestHeaders
  (req : HttpRequestMessage)
  (headers : Headers)
  : Result<unit, HttpBadHeader> =
  headers
  |> List.map (fun (k, v) ->
    // .NET handles "content headers" separately from other headers.
    // They're put into `req.Content.Headers` rather than `req.Headers`
    // https://docs.microsoft.com/en-us/dotnet/api/system.net.http.headers.httpcontentheaders?view=net-6.0
    if String.equalsCaseInsensitive k "content-type" then
      try
        req.Content.Headers.ContentType <- Headers.MediaTypeHeaderValue.Parse(v)
        Ok()
      with :? System.FormatException ->
        Error HttpBadHeader.InvalidContentType
    else
      let added = req.Headers.TryAddWithoutValidation(k, v)
      // Headers are split between req.Headers and req.Content.Headers
      // so just try both.
      if not added then req.Content.Headers.Add(k, v)
      Ok())
  |> Result.collect
  |> Result.map (fun _ -> ())


/// Flatten the response and content header collections into a flat
/// list, joining repeated values with ',' as ASP.NET surfaces them.
let private headersForAspNetResponse
  (response : HttpResponseMessage)
  : List<string * string> =
  let fromAspNetHeaders (headers : Headers.HttpHeaders) : List<string * string> =
    headers
    |> Seq.map Tuple2.fromKeyValuePair
    |> Seq.map (fun (k, v) -> (k, v |> Seq.toList |> String.concat ","))
    |> Seq.toList
  fromAspNetHeaders response.Headers @ fromAspNetHeaders response.Content.Headers


/// Lowercase keys + values across a response-header list. Both the
/// buffered and streaming paths surface the same flattened shape.
let private normalizeResponseHeaders
  (headers : List<string * string>)
  : List<string * string> =
  headers |> List.map (fun (k, v) -> (String.toLowercase k, String.toLowercase v))


/// Match a send-time exception to the typed error the guest sees; anything
/// else propagates as a host failure. Shared by the buffered and streaming
/// paths.
let private (|SendFailure|_|) (e : exn) : Option<HttpRequestError> =
  match e with
  | :? TaskCanceledException -> Some HttpRequestError.Timeout
  | :? System.ArgumentException as e when
    e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
    ->
    // We know of one specific case indicating Unsupported Protocol; any
    // other ArgumentException is a genuine host failure.
    Some(HttpRequestError.BadUrl HttpBadUrl.UnsupportedProtocol)
  | :? System.UriFormatException ->
    Some(HttpRequestError.BadUrl HttpBadUrl.InvalidUri)
  | :? IOException -> Some HttpRequestError.NetworkError
  // We've made a request and _potentially_ (according to .NET) have a status
  // code, but the error type has no good slot for it.
  | :? HttpRequestException -> Some HttpRequestError.NetworkError
  | _ -> None


// ── buffered send ─────────────────────────────────────────────────────────────

/// Send one validated request and buffer the whole response. `canonicalUri`
/// and `profile` come from `prepare`; the URI has already passed the
/// permission check.
let send
  (profile : Profile)
  (method : string)
  (canonicalUri : string)
  (headers : Headers)
  (body : byte[])
  : Task<Result<HttpResponse, HttpRequestError>> =
  task {
    try
      use req = buildHttpRequestMessage (HttpMethod method) canonicalUri body

      match applyRequestHeaders req headers with
      | Error e -> return Error(HttpRequestError.BadHeader e)
      | Ok() ->
        // Allow timeout
        let source =
          new System.Threading.CancellationTokenSource(profile.config.timeoutInMs)

        use! response = profile.client.SendAsync(req, source.Token)

        use! responseStream = response.Content.ReadAsStreamAsync()
        use memoryStream = new MemoryStream()
        do! responseStream.CopyToAsync(memoryStream)

        let headers =
          response |> headersForAspNetResponse |> normalizeResponseHeaders
        return
          Ok
            { statusCode = int response.StatusCode
              headers = headers
              body = memoryStream.ToArray() }
    with SendFailure typed ->
      return Error typed
  }


// ── streaming ─────────────────────────────────────────────────────────────────

/// Live streaming responses, keyed by opaque handle. Guest code never holds
/// the response object; it reads chunks and closes via the handle.
let private streams =
  System.Collections.Concurrent.ConcurrentDictionary<int64, HttpResponseMessage *
  Stream>()

let mutable private nextStreamId = 1L

/// Open a streaming request: headers arrive now, the body is read lazily via
/// `readChunk`. `canonicalUri` and `profile` come from `prepare`; the URI has
/// already passed the permission check.
let openStream
  (profile : Profile)
  (method : string)
  (canonicalUri : string)
  (headers : Headers)
  : Task<Result<HttpStreamHead, HttpRequestError>> =
  task {
    let mutable responseToDispose : HttpResponseMessage option = None
    try
      use req = buildHttpRequestMessage (HttpMethod method) canonicalUri [||]

      match applyRequestHeaders req headers with
      | Error e -> return Error(HttpRequestError.BadHeader e)
      | Ok() ->
        use source =
          new System.Threading.CancellationTokenSource(profile.config.timeoutInMs)
        // No `use!` — the response must outlive this function; it is owned
        // by the stream table until `closeStream`.
        let! response =
          profile.client.SendAsync(
            req,
            HttpCompletionOption.ResponseHeadersRead,
            source.Token
          )
        responseToDispose <- Some response

        let! responseStream = response.Content.ReadAsStreamAsync()

        let handle = System.Threading.Interlocked.Increment(&nextStreamId)
        streams[handle] <- (response, responseStream)

        let headers =
          response |> headersForAspNetResponse |> normalizeResponseHeaders
        return
          Ok
            { handle = handle
              statusCode = int response.StatusCode
              headers = headers }
    with
    | SendFailure typed ->
      responseToDispose |> Option.iter (fun response -> response.Dispose())
      return Error typed
    | e ->
      responseToDispose |> Option.iter (fun response -> response.Dispose())
      return raise e
  }

/// Pull up to `maxBytes` from an open stream; None at EOF or on an unknown
/// handle. No fresh permission check: the transfer was authorized when the
/// stream was opened, and the handle is host-issued.
let readChunk (handle : int64) (maxBytes : int) : Task<Option<byte[]>> =
  task {
    match streams.TryGetValue handle with
    | false, _ -> return None
    | true, (_, stream) ->
      let cap = max 1 maxBytes
      let buf = Array.zeroCreate<byte> cap
      let! n = stream.ReadAsync(buf, 0, cap)
      if n = 0 then
        return None
      elif n = cap then
        return Some buf
      else
        let trimmed = Array.zeroCreate<byte> n
        System.Array.Copy(buf, 0, trimmed, 0, n)
        return Some trimmed
  }

/// Release an open stream. Idempotent; safe from finalizers.
let closeStream (handle : int64) : unit =
  match streams.TryRemove handle with
  | true, (response, stream) ->
    // Ordered response-stream first so the stream is closed before the
    // message — Dispose chains naturally either way, but this mirrors
    // idiomatic .NET cleanup.
    try
      stream.Dispose()
    with _ ->
      ()
    try
      response.Dispose()
    with _ ->
      ()
  | false, _ -> ()
