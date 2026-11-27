/// Host-owned HTTP: client configuration, SSRF guards, the send machinery,
/// and the streaming-response table.
///
/// The checked boundary owns the network. Guest requests run under one
/// SSRF-guarded configuration, which a host may replace with `setGuestConfig`.
/// An `Operation.HttpRequest` names it with `HostTypes.HttpProfile`, so
/// timeouts and allow-lists stay in host-owned state.
module LibExecution.HostHttp

open System.IO
open System.Net.Http
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates

open System.Threading.Tasks

open Prelude

open LibExecution.HostTypes

type private Headers = List<string * string>

/// The per-profile knobs: the request timeout, the connection cap, how long a stream may sit
/// unread, and the SSRF guards. Scheme (http/https only) and the metadata-header ban are not
/// configurable.
type Configuration =
  {
    timeoutInMs : int
    /// How many connections one host gets at a time (`SocketsHttpHandler.MaxConnectionsPerServer`).
    maxConnectionsPerHost : int
    /// How long an open stream may go unread before its connection is closed; 0 is no limit.
    streamIdleTimeoutMs : int
    allowedIP : System.Net.IPAddress -> bool
    allowedHost : string -> bool
  }

/// Preserve typed failures through .NET exception wrapping.
type private ConnectFailed(failure : HttpNetworkFailure, inner : exn) =
  inherit System.Exception("HTTP connection failed", inner)
  member _.Failure = failure

/// The connection cap per host when none is configured (`http.maxConnectionsPerHost`).
let defaultMaxConnectionsPerHost = 50

/// How long a stream may go unread when none is configured: a stream left undrained and
/// unclosed otherwise holds its connection, against the host's cap, until it is collected.
let defaultStreamIdleTimeoutMs = 60_000

/// Start with the first address returned by DNS, then alternate IPv4 and IPv6.
/// Keep the original order within each address family.
let alternateIPv4AndIPv6 (addresses : System.Net.IPAddress array) =
  let addresses = Array.distinct addresses
  if Array.isEmpty addresses then
    addresses
  else
    let preferred, other =
      addresses
      |> Array.partition (fun ip -> ip.AddressFamily = addresses[0].AddressFamily)
    [| for i in 0 .. max preferred.Length other.Length - 1 do
         if i < preferred.Length then yield preferred[i]
         if i < other.Length then yield other[i] |]

/// Start connection attempts a short delay apart. Attempts can overlap.
/// Keep the first successful connection, cancel pending attempts, and close any extras.
let connectToFirstAvailableAddress
  (staggerMs : int)
  (addresses : System.Net.IPAddress array)
  (connect :
    System.Net.IPAddress -> System.Threading.CancellationToken -> Task<Stream>)
  (cancellationToken : System.Threading.CancellationToken)
  : Task<Stream> =
  task {
    cancellationToken.ThrowIfCancellationRequested()
    if Array.isEmpty addresses then invalidArg "addresses" "No resolved addresses"
    if staggerMs < 0 then invalidArg "staggerMs" "Negative connection attempt delay"
    let addresses = alternateIPv4AndIPv6 addresses
    use attemptCancellation =
      System.Threading.CancellationTokenSource.CreateLinkedTokenSource
        cancellationToken
    let winner =
      TaskCompletionSource<Stream>(
        TaskCreationOptions.RunContinuationsAsynchronously
      )
    // Cancellation may arrive before all the attempts have started.
    use _cancelled =
      cancellationToken.Register(fun () ->
        winner.TrySetCanceled cancellationToken |> ignore<bool>)
    let mutable remaining = addresses.Length
    let attemptTasks = ResizeArray<Task<unit>>()
    let startAttempt ip =
      task {
        try
          let! stream = connect ip attemptCancellation.Token
          if cancellationToken.IsCancellationRequested then
            stream.Dispose()
            winner.TrySetCanceled cancellationToken |> ignore<bool>
          elif not (winner.TrySetResult stream) then
            stream.Dispose()
        with e ->
          // Report a connection failure only after every address has failed.
          if System.Threading.Interlocked.Decrement(&remaining) = 0 then
            if cancellationToken.IsCancellationRequested then
              winner.TrySetCanceled cancellationToken |> ignore<bool>
            else
              winner.TrySetException e |> ignore<bool>
      }
    let launchAttempts =
      task {
        try
          let mutable index = 0
          while index < addresses.Length && not winner.Task.IsCompleted do
            if index > 0 then do! Task.Delay(staggerMs, attemptCancellation.Token)
            attemptCancellation.Token.ThrowIfCancellationRequested()
            if not winner.Task.IsCompleted then
              attemptTasks.Add(startAttempt addresses[index])
              index <- index + 1
        with
        | :? System.OperationCanceledException when
          attemptCancellation.IsCancellationRequested
          ->
          ()
        | e -> winner.TrySetException e |> ignore<bool>
      }
    let! outcome =
      task {
        try
          let! stream = winner.Task
          return Ok stream
        with e ->
          return Error e
      }
    // Wait for cleanup before disposing the attempts' cancellation source.
    attemptCancellation.Cancel()
    do! launchAttempts
    let! _ = Task.WhenAll(attemptTasks.ToArray())
    match outcome with
    | Ok stream -> return stream
    | Error e -> return raise e
  }

module private BaseClient =
  // SocketsHttpHandler wherever it runs; Blazor/WASM refuses it at runtime, so the browser
  // gets an HttpClientHandler instead. The allow-lists and timeouts come from `Configuration`.

  module SocketBasedHandler =
    // One shared client: SocketsHttpHandler's pooled-connection lifetime keeps sockets from
    // piling up and DNS answers from going stale.
    // https://learn.microsoft.com/en-us/aspnet/core/fundamentals/http-requests#alternatives-to-ihttpclientfactory
    let handler
      (config : Configuration)
      (proxy : Option<System.Uri * System.Net.ICredentials>)
      : HttpMessageHandler =
      let connectionFilter
        (context : SocketsHttpConnectionContext)
        (cancellationToken : System.Threading.CancellationToken)
        : ValueTask<Stream> =
        let connect : Task<Stream> =
          task {
            let uri, target =
              match proxy with
              | Some(uri, _) -> uri, HttpNetworkTarget.Proxy
              | None ->
                context.InitialRequestMessage.RequestUri,
                HttpNetworkTarget.Destination
            let connectFailed (inner : exn) =
              let detail =
                match inner with
                | :? System.Net.Sockets.SocketException as s -> Some s.Message
                | _ -> None
              ConnectFailed(
                HttpNetworkFailure.ConnectionFailure(target uri.Authority, detail),
                inner
              )
            try
              // Connect against the resolved IPs, not the hostname. A second DNS lookup
              // could return a private IP after a public IP passed the allow-list check
              // (DNS rebinding).
              let! ips =
                System.Net.Dns.GetHostAddressesAsync(
                  context.DnsEndPoint.Host,
                  cancellationToken
                )

              // This handler serves either direct requests or one configured proxy
              // selected by the host. Never widen the direct handler's IP policy.
              let isProxyConnection =
                match proxy with
                | Some(uri, _) ->
                  String.equalsCaseInsensitive uri.IdnHost context.DnsEndPoint.Host
                  && uri.Port = context.DnsEndPoint.Port
                | None -> false

              if
                Array.isEmpty ips
                || (not isProxyConnection && not (Array.forall config.allowedIP ips))
              then
                // Do not expose details about blocked addresses.
                raise (connectFailed null)

              // All direct addresses passed the allow-list check. Configured proxies
              // are trusted and skip that check. If an address is slow to connect,
              // try the next one while the earlier attempt is still pending.
              let connect (ip : System.Net.IPAddress) token =
                task {
                  let socket =
                    new System.Net.Sockets.Socket(
                      ip.AddressFamily,
                      System.Net.Sockets.SocketType.Stream,
                      System.Net.Sockets.ProtocolType.Tcp
                    )
                  try
                    socket.NoDelay <- true
                    let endpoint =
                      System.Net.IPEndPoint(ip, context.DnsEndPoint.Port)
                    do! socket.ConnectAsync(endpoint, token)
                    return
                      new System.Net.Sockets.NetworkStream(socket, true) :> Stream
                  with e ->
                    socket.Dispose()
                    return raise e
                }
              try
                return!
                  connectToFirstAvailableAddress 250 ips connect cancellationToken
              with
              | :? System.OperationCanceledException as e when
                cancellationToken.IsCancellationRequested
                ->
                return raise e
              | e -> return raise (connectFailed e)
            with
            | :? System.ArgumentException as e -> return raise (connectFailed e)
            | :? System.Net.Sockets.SocketException as e ->
              match e.SocketErrorCode with
              | System.Net.Sockets.SocketError.HostNotFound
              | System.Net.Sockets.SocketError.NoData
              | System.Net.Sockets.SocketError.TryAgain ->
                return
                  raise (
                    ConnectFailed(HttpNetworkFailure.DnsFailure(target uri.Host), e)
                  )
              | _ -> return raise (connectFailed e)
          }
        ValueTask<Stream>(connect)
      new SocketsHttpHandler(
        UseProxy = Option.isSome proxy,
        Proxy =
          (match proxy with
           | Some(uri, credentials) ->
             new System.Net.WebProxy(uri, false, [||], credentials)
           | None -> null),
        // Avoid DNS problems
        PooledConnectionIdleTimeout = System.TimeSpan.FromMinutes 5.0,
        // A cap per host, as Go's `MaxConnsPerHost` and undici's pool have: `List.map(urls, get)`
        // over one host opens 50 connections, and the rest wait for one inside their deadline.
        // 50 is curl's `--parallel-max` default; browsers use 6, which is too few for an API.
        MaxConnectionsPerServer = config.maxConnectionsPerHost,
        PooledConnectionLifetime = System.TimeSpan.FromMinutes 10.0,
        ConnectTimeout = System.TimeSpan.FromSeconds 10.0,

        // Redirects are disabled; callers must opt into each target explicitly.
        AllowAutoRedirect = false,

        // Users share the HttpClient, don't let them share cookies!
        UseCookies = false,

        // Decompression is the request's to ask for (`decoderFor`), so `client([])` stays bare.
        AutomaticDecompression = System.Net.DecompressionMethods.None,

        // Don't add a RequestId header for opentelemetry
        ActivityHeadersPropagator = null,

        // Direct requests remain guarded; only the selected proxy is trusted.
        ConnectCallback = connectionFilter
      )

  /// Select the proxy before choosing a connection pool. Separate handlers keep
  /// a trusted proxy connection from becoming an exception for direct requests
  /// (including NO_PROXY and names resolving to the proxy's address).
  type private ProxyRoutingHandler
    (config : Configuration, proxy : System.Net.IWebProxy) =
    inherit HttpMessageHandler()

    let direct = new HttpMessageInvoker(SocketBasedHandler.handler config None)
    let proxied =
      System.Collections.Generic.Dictionary<System.Uri, HttpMessageInvoker>()

    override _.SendAsync(request, cancellationToken) =
      let uri = request.RequestUri
      let selected = if proxy.IsBypassed uri then null else proxy.GetProxy uri
      let transport =
        if isNull selected || selected = uri then
          direct
        else
          lock proxied (fun () ->
            match proxied.TryGetValue selected with
            | true, transport -> transport
            | false, _ ->
              // Without its user and password: .NET names the proxy in its errors, and the
              // credentials reach the proxy through `proxy.Credentials` anyway.
              let address =
                System.UriBuilder(selected, UserName = "", Password = "").Uri
              let transport =
                new HttpMessageInvoker(
                  SocketBasedHandler.handler
                    config
                    (Some(address, proxy.Credentials))
                )
              proxied.Add(selected, transport)
              transport)
      transport.SendAsync(request, cancellationToken)

    override _.Dispose(disposing) =
      if disposing then
        direct.Dispose()
        for transport in proxied.Values do
          transport.Dispose()
      base.Dispose(disposing)

  module WasmHandler =
    let handler (_config : Configuration) : HttpMessageHandler =
      new HttpClientHandler(
        // These settings are also enabled in SocketBasedHandler - see comments above
        AllowAutoRedirect = false
      // These can't be set in WASM, even though they exist (PlatformNotSupportedException)
      // UseCookies = false,
      // AutomaticDecompression = System.Net.DecompressionMethods.None
      )

  let create (config : Configuration) (proxy : System.Net.IWebProxy) : HttpClient =
    let handler =
      if SocketsHttpHandler.IsSupported then
        new ProxyRoutingHandler(config, proxy) :> HttpMessageHandler
      else
        // For Blazor
        WasmHandler.handler config

    new HttpClient(
      handler,
      disposeHandler = true,
      // No timeout of the client's own: every send runs under a deadline of its own
      // (`deadlineFor`), the request's when it names one, so this one could only cut a
      // longer request short.
      Timeout = System.Threading.Timeout.InfiniteTimeSpan,
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

  // RFC 1122 "this network"; Linux connects 0.0.0.0 to the local host.
  let private zero = System.Net.IPNetwork.Parse "0.0.0.0/8"
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
    || zero.Contains ip

  let bannedIp (ip : System.Net.IPAddress) : bool =
    if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6 then
      if ip.IsIPv4MappedToIPv6 then
        bannedIPv4 (ip.MapToIPv4())
      else
        uniqueLocal.Contains ip
        || ip.IsIPv6LinkLocal // ipv6 equivalent of 169.254.*
        || ip.IsIPv6SiteLocal // ipv6 equivalent of 10/172.16/192.168
        || System.Net.IPAddress.IsLoopback ip
        || ip.Equals System.Net.IPAddress.IPv6Any // `::`, which Linux connects to the local host
    else if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork then
      bannedIPv4 ip
    else
      true // not ipv4 or ipv6, so banned

  let bannedHost (host : string) : bool =
    let host = host.Trim().ToLower()
    let badIP =
      let mutable ip = null
      if System.Net.IPAddress.TryParse(host, &ip) then bannedIp ip else false
    badIP
    || host = "localhost"
    || host = "metadata"
    || host = "metadata.google.internal"

  /// Loopback, including the IPv4-mapped-IPv6 spelling, which `IsLoopback` alone does not
  /// recognise. Only used to WIDEN a guard, so missing a spelling would under-permit rather
  /// than under-protect, but `::ffff:127.0.0.1` is reachable and so should answer true.
  let isLoopback (ip : System.Net.IPAddress) : bool =
    System.Net.IPAddress.IsLoopback ip
    || (ip.IsIPv4MappedToIPv6 && System.Net.IPAddress.IsLoopback(ip.MapToIPv4()))

  /// `localhost`, or any spelling of a loopback address.
  let isLoopbackHost (host : string) : bool =
    let host = host.Trim().ToLower()
    if host = "localhost" then
      true
    else
      let mutable ip = null
      System.Net.IPAddress.TryParse(host, &ip) && isLoopback ip

  /// The non-public ranges the guest default bans, minus the ones no opt-in widens: RFC 1918,
  /// RFC 6598 shared space (`100.64.0.0/10`, which carrier NAT and mesh VPNs both assign from),
  /// and IPv6 unique-local and site-local. Link-local, cloud-metadata and 0.0.0.0/8 stay banned.
  let rec isPrivateNetwork (ip : System.Net.IPAddress) : bool =
    if ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetworkV6 then
      if ip.IsIPv4MappedToIPv6 then
        isPrivateNetwork (ip.MapToIPv4())
      else
        uniqueLocal.Contains ip || ip.IsIPv6SiteLocal
    elif ip.AddressFamily = System.Net.Sockets.AddressFamily.InterNetwork then
      bannedIPv4 ip
      && not (System.Net.IPAddress.IsLoopback ip)
      && not (oneSixNine.Contains ip)
      && not (zero.Contains ip)
    else
      false

  /// A private-range address literal. There is no name half to widen: `bannedHost` refuses
  /// `localhost` and otherwise only literals, so a LAN name is decided by `allowedIP` on
  /// whatever it resolves to.
  let isPrivateNetworkHost (host : string) : bool =
    let mutable ip = null
    System.Net.IPAddress.TryParse(host.Trim(), &ip) && isPrivateNetwork ip

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
  { timeoutInMs = defaultHttpTimeoutMs
    maxConnectionsPerHost = defaultMaxConnectionsPerHost
    streamIdleTimeoutMs = defaultStreamIdleTimeoutMs
    allowedIP = fun ip -> not (LocalAccess.bannedIp ip)
    allowedHost = fun host -> not (LocalAccess.bannedHost host) }

// ── profiles ──────────────────────────────────────────────────────────────────

/// A configuration and the client built for it. Host-side only: operations
/// name a profile, they never carry one.
type Profile = { config : Configuration; client : HttpClient }

/// Build a host-owned profile with an explicit proxy selector. The selector is
/// trusted configuration, never guest input; URL permission checks still apply.
let profileWithProxy
  (config : Configuration)
  (proxy : System.Net.IWebProxy)
  : Profile =
  { config = config; client = BaseClient.create config proxy }

let private profileOf (config : Configuration) : Profile =
  // Browser hosts use WasmHandler; DefaultProxy is unsupported there.
  let proxy =
    if SocketsHttpHandler.IsSupported then HttpClient.DefaultProxy else null
  profileWithProxy config proxy

let mutable private guest : Profile = profileOf defaultConfig

/// Replace the guest configuration; the client is rebuilt and shared for the
/// configuration's lifetime. A host that needs loopback reachable (a test
/// harness with an in-process server) sets it here before guest execution.
let setGuestConfig (config : Configuration) : unit = guest <- profileOf config

/// The guest configuration with another connection cap per host (`http.maxConnectionsPerHost`):
/// higher for a big fan-out, lower to be polite to one host.
let setMaxConnectionsPerHost (n : int) : unit =
  setGuestConfig { guest.config with maxConnectionsPerHost = n }

/// The pair of predicates a widening moves together: an IP, and the host literal that names it.
/// Widening the IP alone leaves what a person types refused by `allowedHost`.
type Widening = { ip : System.Net.IPAddress -> bool; host : string -> bool }

let loopbackWidening : Widening =
  { ip = LocalAccess.isLoopback; host = LocalAccess.isLoopbackHost }

let privateNetworkWidening : Widening =
  { ip = LocalAccess.isPrivateNetwork; host = LocalAccess.isPrivateNetworkHost }

/// `defaultConfig` with the named ranges widened and nothing else.
let guestConfigWidenedBy (widenings : List<Widening>) : Configuration =
  { defaultConfig with
      allowedIP =
        fun ip ->
          List.exists (fun w -> w.ip ip) widenings || defaultConfig.allowedIP ip
      allowedHost =
        fun host ->
          List.exists (fun w -> w.host host) widenings
          || defaultConfig.allowedHost host }

/// `defaultConfig` with loopback reachable and NOTHING else widened: RFC-1918, Tailscale's CGN
/// range, GCP private endpoints, link-local, cloud-metadata and 0.0.0.0 all stay blocked. Both
/// predicates compose with the default rather than restating its ban list, so anything added
/// there is inherited here.
///
/// For an explicit, named opt-in only. The guest default is unchanged, so nothing that ships
/// reaches loopback unless a person turns this on: `Cli.fs` installs it from the `http.loopback`
/// setting, and a browser would install it the same way if that is ever wanted.
///
/// Both halves have to widen. `allowedIP` alone leaves the NAME `localhost` refused by
/// `allowedHost`, which is what a person actually types; `TestUtils.testHttpConfig` widens only
/// the IP and its callers write `127.0.0.1`.
///
/// Why an opt-in rather than honouring the instance policy, which can already say
/// `http LOCALHOST *`: a policy may say `all`, and guest code includes `darklang serve` handlers
/// and package initialisation, which is what these guards exist to contain. A guard a broad grant
/// switches off is not a guard.
let devLoopbackConfig : Configuration = guestConfigWidenedBy [ loopbackWidening ]

let profileFor (profile : HttpProfile) : Profile =
  match profile with
  | HttpProfile.Guest -> guest


// ── request prep ──────────────────────────────────────────────────────────────

/// Why the guard refused <param host>. The ranges a person can open get a case of their own.
let private refusedHost (host : string) : HttpBadUrl =
  if LocalAccess.isLoopbackHost host then HttpBadUrl.Loopback
  elif LocalAccess.isPrivateNetworkHost host then HttpBadUrl.PrivateNetwork
  else HttpBadUrl.InvalidHost

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
      Error(HttpRequestError.BadUrl(refusedHost host))
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

/// RFC 9110 token characters, the only ones a header name may hold.
let private isTokenChar (c : char) : bool =
  System.Char.IsAsciiLetterOrDigit c || "!#$%&'*+-.^_`|~".Contains c

/// Header text reaches the wire unvalidated (`TryAddWithoutValidation`), so a CR or LF
/// would end the header line and start another. Refuse rather than repair.
let private validateHeaders (headers : Headers) : Result<unit, HttpBadHeader> =
  headers
  |> List.tryPick (fun (k, v) ->
    // Use the host and port from the URL that passed the permission check.
    if String.equalsCaseInsensitive k "host" then
      Some HttpBadHeader.InvalidName
    elif k = "" || not (Seq.forall isTokenChar k) then
      Some HttpBadHeader.InvalidName
    elif Seq.exists (fun c -> c = '\r' || c = '\n' || c = '\000') v then
      Some HttpBadHeader.InvalidValue
    else
      None)
  |> Option.map Error
  |> Option.defaultValue (Ok())

/// Validate method, headers, URL, and the profile's allow-lists; return the
/// canonical URI the permission layer checks and the send uses, with the
/// resolved profile so the send need not look it up again. Headers are checked
/// before the metadata guard reads them, so one can't hide inside another's
/// value. `Error` is a guest-visible typed error.
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
    validateHeaders headers
    |> Result.mapError HttpRequestError.BadHeader
    |> Result.bind (fun () -> validateAndBuildUri profile.config url headers)
    |> Result.map (fun uri -> (uri, profile))


/// Build the request message for `sendForHeaders`, which both the buffered and
/// the streaming path go through.
let private buildHttpRequestMessage
  (method : HttpMethod)
  (reqUri : string)
  (body : byte[])
  : HttpRequestMessage =
  // The browser's fetch rejects a GET or HEAD that carries a body, even an empty one, and
  // Blazor's handler forwards any Content as a body. Everywhere else an empty body is fine.
  let content : HttpContent =
    if
      System.OperatingSystem.IsBrowser()
      && body.Length = 0
      && (method = HttpMethod.Get || method = HttpMethod.Head)
    then
      null
    else
      new ByteArrayContent(body)
  new HttpRequestMessage(
    method,
    reqUri,
    Content = content,
    // Dark checks destination IPs in the TCP ConnectCallback.
    // HTTP/3 uses QUIC, so it needs its own hook to run the same checks.
    // TODO: Restore HTTP/3 once Dark can enforce those checks on QUIC.
    // .NET's proposed QUIC hook: https://github.com/dotnet/runtime/issues/64449
    Version = System.Net.HttpVersion.Version20,
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
        // No content (a bodiless GET in the browser): nothing to type.
        if not (isNull req.Content) then
          req.Content.Headers.ContentType <- Headers.MediaTypeHeaderValue.Parse(v)
        Ok()
      with :? System.FormatException ->
        Error HttpBadHeader.InvalidContentType
    else
      let added = req.Headers.TryAddWithoutValidation(k, v)
      // Headers are split between req.Headers and req.Content.Headers
      // so just try both.
      if not added && not (isNull req.Content) then req.Content.Headers.Add(k, v)
      Ok())
  |> Result.collect
  |> Result.map (fun _ -> ())


/// The response and content header collections as a flat list, as the server sent them: names
/// and values unchanged, and a header sent twice is two pairs rather than one joined with `,`,
/// which would corrupt `Set-Cookie` (an `Expires` date holds a comma). `NonValidated` is the
/// raw lines, so .NET's parsing of known headers does not reshape or split a value either.
/// Normalizing names is Dark's (`Stdlib.HttpClient.Middleware.lowercaseNames`).
let private headersForAspNetResponse
  (response : HttpResponseMessage)
  : List<string * string> =
  let fromAspNetHeaders (headers : Headers.HttpHeaders) : List<string * string> =
    [ for KeyValue(name, values) in headers.NonValidated do
        for value in values do
          yield (name, value) ]
  fromAspNetHeaders response.Headers @ fromAspNetHeaders response.Content.Headers


/// Whether the system trust stores hold any root certificate. On Linux the
/// machine store is OpenSSL's default bundle, or SSL_CERT_FILE / SSL_CERT_DIR.
/// Read only after a certificate has failed to verify.
let private systemHasRootCAs () : bool =
  let count (location : StoreLocation) =
    use store = new X509Store(StoreName.Root, location)
    store.Open OpenFlags.ReadOnly
    store.Certificates.Count
  try
    count StoreLocation.LocalMachine + count StoreLocation.CurrentUser > 0
  with _ ->
    // Unknown is not empty: don't send someone to install what they may have.
    true

/// Why a TLS handshake failed. With no CA certificates at all, every server
/// fails the same way, so that case is named, with the fix.
///
/// The native build carries no resource strings, so .NET's message is either
/// "The remote certificate is invalid because of errors in the certificate
/// chain: PartialChain" or "net_ssl_io_cert_chain_validation, PartialChain".
/// Only fragments both share are matched, and the reason is what follows.
let tlsFailureMessage
  (hasRootCAs : unit -> bool)
  (e : AuthenticationException)
  : string =
  let message = e.Message
  let reason =
    match message.IndexOf ": ", message.IndexOf ", " with
    | i, _ when i >= 0 -> message.Substring(i + 2)
    | _, i when i >= 0 -> message.Substring(i + 2)
    | _ -> message
  let has (status : string) = reason.Contains status
  let unverified = "could not verify the server's TLS certificate"
  if not (message.Contains "cert") then
    $"TLS handshake with the server failed: {message}"
  else if not (hasRootCAs ()) then
    $"{unverified}: no CA certificates found; install your distro's "
    + "ca-certificates package or set SSL_CERT_FILE"
  else if has "UntrustedRoot" || has "PartialChain" then
    $"{unverified}: no CA certificate on this system trusts it ({reason})"
  else if has "NotTimeValid" then
    $"{unverified}: it has expired or is not yet valid ({reason})"
  else if has "RemoteCertificateNameMismatch" then
    $"{unverified}: it was issued for a different host name ({reason})"
  else
    $"{unverified} ({reason})"

/// .NET wraps the handshake failure in an HttpRequestException.
let rec private (|TlsFailure|_|) (e : exn) : Option<AuthenticationException> =
  match e with
  | :? AuthenticationException as e -> Some e
  | null -> None
  | e -> (|TlsFailure|_|) e.InnerException

/// Unwrap a connection failure or return the I/O error.
let rec private networkFailure
  (canonicalUri : string)
  (e : exn)
  : HttpNetworkFailure =
  match e with
  | :? ConnectFailed as c -> c.Failure
  | _ when not (isNull e.InnerException) ->
    networkFailure canonicalUri e.InnerException
  | _ -> HttpNetworkFailure.IoFailure(System.Uri(canonicalUri).Authority, e.Message)

/// Match a send-time exception to the typed error the guest sees; anything
/// else propagates as a host failure. Shared by the buffered and streaming
/// paths, and by the buffered path's read of the body.
let private (|SendFailure|_|)
  (canonicalUri : string)
  (e : exn)
  : Option<HttpRequestError> =
  match e with
  | TlsFailure e ->
    Some(HttpRequestError.TlsError(tlsFailureMessage systemHasRootCAs e))
  // A deadline that fires during the body's read cancels the read rather than the send,
  // which .NET reports as the base class.
  | :? System.OperationCanceledException -> Some HttpRequestError.Timeout
  | :? System.ArgumentException as e when
    e.Message = "Only 'http' and 'https' schemes are allowed. (Parameter 'value')"
    ->
    // We know of one specific case indicating Unsupported Protocol; any
    // other ArgumentException is a genuine host failure.
    Some(HttpRequestError.BadUrl HttpBadUrl.UnsupportedProtocol)
  | :? System.UriFormatException ->
    Some(HttpRequestError.BadUrl HttpBadUrl.InvalidUri)
  | :? IOException
  // A gzip or brotli body that does not decode.
  | :? InvalidDataException
  // We've made a request and _potentially_ (according to .NET) have a status
  // code, but the error type has no good slot for it.
  | :? HttpRequestException ->
    Some(HttpRequestError.NetworkError(networkFailure canonicalUri e))
  | _ -> None


// ── the send ──────────────────────────────────────────────────────────────────

/// Send one validated request and answer as soon as the response's headers are in, its body
/// still to be read. The one send both `send` and `openStream` make. `canonicalUri` and
/// `profile` come from `prepare`; the URI has already passed the permission check.
let private sendForHeaders
  (profile : Profile)
  (method : string)
  (canonicalUri : string)
  (headers : Headers)
  (body : byte[])
  (cancel : System.Threading.CancellationToken)
  : Task<Result<HttpResponseMessage, HttpRequestError>> =
  task {
    use req = buildHttpRequestMessage (HttpMethod method) canonicalUri body
    match applyRequestHeaders req headers with
    | Error e -> return Error(HttpRequestError.BadHeader e)
    | Ok() ->
      let! response =
        profile.client.SendAsync(
          req,
          HttpCompletionOption.ResponseHeadersRead,
          cancel
        )
      return Ok response
  }

/// The deadline a send runs under: the request's own when it names one, else the profile's. Zero
/// (or less) is no limit, as the server's `http.requestTimeoutMs` is.
let private deadlineFor (profile : Profile) (timeoutMs : Option<int>) =
  match timeoutMs with
  | Some ms when ms <= 0 -> new System.Threading.CancellationTokenSource()
  | Some ms -> new System.Threading.CancellationTokenSource(ms)
  | None -> new System.Threading.CancellationTokenSource(profile.config.timeoutInMs)

/// For a body with a gzip or brotli `Content-Encoding`: its headers without the two that
/// describe the encoded body, and the decoder to read it through. `None` for any other body,
/// which is left as it came.
let private decoderFor (headers : Headers) : Option<Headers * (Stream -> Stream)> =
  let isHeader name (k, _) = String.equalsCaseInsensitive k name
  let decoder : Option<Stream -> Stream> =
    match List.filter (isHeader "Content-Encoding") headers with
    | [ (_, encoding) ] ->
      match encoding.Trim().ToLowerInvariant() with
      | "gzip" ->
        Some(fun body ->
          new Compression.GZipStream(body, Compression.CompressionMode.Decompress))
      | "br" ->
        Some(fun body ->
          new Compression.BrotliStream(body, Compression.CompressionMode.Decompress))
      | _ -> None
    | _ -> None
  decoder
  |> Option.map (fun decode ->
    let headers =
      headers
      |> List.filter (fun h ->
        not (isHeader "Content-Encoding" h || isHeader "Content-Length" h))
    (headers, decode))

let rec private readInto
  (body : Stream)
  (into : MemoryStream)
  (buffer : byte[])
  (limit : int64)
  (cancel : System.Threading.CancellationToken)
  : Task<unit> =
  task {
    let! n = body.ReadAsync(buffer, 0, buffer.Length, cancel)
    if n > 0 then
      if into.Length + int64 n > limit then
        raise (HttpRequestException $"The decoded body is over {limit} bytes")
      into.Write(buffer, 0, n)
      return! readInto body into buffer limit cancel
  }

/// All of `body`, refusing past `limit` bytes as the client's own buffering does.
let private readAll
  (body : Stream)
  (limit : int64)
  (cancel : System.Threading.CancellationToken)
  : Task<byte[]> =
  task {
    use into = new MemoryStream()
    do! readInto body into (Array.zeroCreate<byte> 81920) limit cancel
    return into.ToArray()
  }

/// Send one validated request and read its whole response: the streaming send, drained here.
/// One deadline covers the exchange, body included, and a failure reading the body is the same
/// typed error as one sending, so a slow or broken body is a `Timeout` or a `NetworkError`.
let send
  (profile : Profile)
  (method : string)
  (canonicalUri : string)
  (headers : Headers)
  (body : byte[])
  (timeoutMs : Option<int>)
  (decompress : bool)
  : Task<Result<HttpResponse, HttpRequestError>> =
  task {
    use deadline = deadlineFor profile timeoutMs
    try
      match!
        sendForHeaders profile method canonicalUri headers body deadline.Token
      with
      | Error e -> return Error e
      | Ok response ->
        use response = response
        let headers = headersForAspNetResponse response
        let limit = profile.client.MaxResponseContentBufferSize
        match (if decompress then decoderFor headers else None) with
        | Some(headers, decode) ->
          let! raw = response.Content.ReadAsStreamAsync(deadline.Token)
          use stream = decode raw
          let! bytes = readAll stream limit deadline.Token
          return
            Ok
              { statusCode = int response.StatusCode
                headers = headers
                body = bytes }
        | None ->
          // The client's own buffering, as a buffered send does it, under the same deadline:
          // past the buffer limit it is an `HttpRequestException`, so a `NetworkError`.
          do! response.Content.LoadIntoBufferAsync(limit, deadline.Token)
          let! bytes = response.Content.ReadAsByteArrayAsync(deadline.Token)
          return
            Ok
              { statusCode = int response.StatusCode
                headers = headers
                body = bytes }
    with SendFailure canonicalUri typed ->
      return Error typed
  }


// ── streaming ─────────────────────────────────────────────────────────────────

/// An open stream: the response it belongs to, the body as read, and the timer that closes the
/// connection when the body goes unread for the profile's `streamIdleTimeoutMs`.
type private OpenStream =
  { response : HttpResponseMessage
    body : Stream
    idle : System.Threading.Timer
    idleMs : int }

/// Live streaming responses, keyed by opaque handle. Guest code never holds
/// the response object; it reads chunks and closes via the handle.
let private streams =
  System.Collections.Concurrent.ConcurrentDictionary<int64, OpenStream>()

let mutable private nextStreamId = 1L

let private release (s : OpenStream) : unit =
  // Ordered response-stream first so the stream is closed before the
  // message — Dispose chains naturally either way, but this mirrors
  // idiomatic .NET cleanup.
  try
    s.body.Dispose()
  with _ ->
    ()
  try
    s.response.Dispose()
  with _ ->
    ()

/// Release an open stream. Idempotent; safe from finalizers.
let closeStream (handle : int64) : unit =
  match streams.TryRemove handle with
  | true, s ->
    s.idle.Dispose()
    release s
  | false, _ -> ()

/// The connection of a stream left unread too long goes back to the pool. The handle stays until
/// the stream is closed, so a later read fails as one on a broken connection does, rather than
/// reading as the end of the body.
let private expire (handle : int64) : unit =
  match streams.TryGetValue handle with
  | true, s -> release s
  | false, _ -> ()

/// Restart (`Some`) or stop the idle clock. A stream closed meanwhile has no clock to change.
let private idleIn (s : OpenStream) (ms : Option<int>) : unit =
  let due = ms |> Option.defaultValue System.Threading.Timeout.Infinite
  try
    s.idle.Change(due, System.Threading.Timeout.Infinite) |> ignore<bool>
  with :? System.ObjectDisposedException ->
    ()

let private startIdle (s : OpenStream) : unit =
  if s.idleMs > 0 then idleIn s (Some s.idleMs)

let private stopIdle (s : OpenStream) : unit = idleIn s None

/// Open a streaming request: headers arrive now, the body is read lazily via
/// `readChunk`. `canonicalUri` and `profile` come from `prepare`; the URI has
/// already passed the permission check. The deadline covers the headers only, so
/// a long-lived body (server-sent events) is not cut off.
let openStream
  (profile : Profile)
  (method : string)
  (canonicalUri : string)
  (headers : Headers)
  (body : byte[])
  (timeoutMs : Option<int>)
  (decompress : bool)
  : Task<Result<HttpStreamHead, HttpRequestError>> =
  task {
    let mutable responseToDispose : HttpResponseMessage option = None
    try
      use deadline = deadlineFor profile timeoutMs
      match!
        sendForHeaders profile method canonicalUri headers body deadline.Token
      with
      | Error e -> return Error e
      | Ok response ->
        // Not disposed here: the response must outlive this function; it is owned
        // by the stream table until `closeStream`.
        responseToDispose <- Some response

        let! responseStream = response.Content.ReadAsStreamAsync()
        let headers = headersForAspNetResponse response
        let (headers, responseStream) =
          match (if decompress then decoderFor headers else None) with
          | Some(headers, decode) -> (headers, decode responseStream)
          | None -> (headers, responseStream)

        let handle = System.Threading.Interlocked.Increment(&nextStreamId)
        let opened =
          { response = response
            body = responseStream
            idle =
              new System.Threading.Timer(
                (fun _ -> expire handle),
                null,
                System.Threading.Timeout.Infinite,
                System.Threading.Timeout.Infinite
              )
            idleMs = profile.config.streamIdleTimeoutMs }
        streams[handle] <- opened
        startIdle opened

        return
          Ok
            { handle = handle
              statusCode = int response.StatusCode
              headers = headers }
    with
    | SendFailure canonicalUri typed ->
      responseToDispose |> Option.iter (fun response -> response.Dispose())
      return Error typed
    | e ->
      responseToDispose |> Option.iter (fun response -> response.Dispose())
      return raise e
  }

/// Pull up to `maxBytes` from an open stream; None at EOF or on an unknown
/// handle. No fresh permission check: the transfer was authorized when the
/// stream was opened, and the handle is host-issued. The idle clock stops while a read waits.
let readChunk (handle : int64) (maxBytes : int) : Task<Option<byte[]>> =
  task {
    match streams.TryGetValue handle with
    | false, _ -> return None
    | true, s ->
      stopIdle s
      try
        let cap = max 1 maxBytes
        let buf = Array.zeroCreate<byte> cap
        let! n = s.body.ReadAsync(buf, 0, cap)
        if n = 0 then
          return None
        elif n = cap then
          return Some buf
        else
          let trimmed = Array.zeroCreate<byte> n
          System.Array.Copy(buf, 0, trimmed, 0, n)
          return Some trimmed
      finally
        startIdle s
  }
