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
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates

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

/// Preserve typed failures through .NET exception wrapping.
type private ConnectFailed(failure : HttpNetworkFailure, inner : exn) =
  inherit System.Exception("HTTP connection failed", inner)
  member _.Failure = failure

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
    let handler
      (config : Configuration)
      (proxy : Option<System.Uri * System.Net.ICredentials>)
      : HttpMessageHandler =
      let connectionFilter
        (context : SocketsHttpConnectionContext)
        (cancellationToken : System.Threading.CancellationToken)
        : ValueTask<Stream> =
        vtask {
          let uri, target =
            match proxy with
            | Some(uri, _) -> uri, HttpNetworkTarget.Proxy
            | None ->
              context.InitialRequestMessage.RequestUri, HttpNetworkTarget.Destination
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
            // While this DNS call is expensive, it should be cached.
            //
            // Connect against the resolved IPs, not the hostname: the OS
            // would otherwise re-resolve at connect time and a malicious
            // resolver can return public-then-private within ms,
            // bypassing the allow-list (DNS-rebinding TOCTOU).
            let ips = System.Net.Dns.GetHostAddresses context.DnsEndPoint.Host

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

            // TRY EVERY resolved address, not just the first. A name routinely
            // resolves to more than one (`localhost` is ::1 AND 127.0.0.1), only
            // some of which have anything listening, and the order is the resolver's
            // to choose. Connecting to ips[0] alone makes `http://localhost:<port>`
            // fail against a server bound to IPv4, and report it as a flat "network
            // error".
            //
            // Every address passed the direct guard or belongs to the trusted proxy.
            // Trying another resolved address does not allow additional destinations:
            // we connect to that address rather than let the OS resolve the name again,
            // preserving the DNS-rebinding guard for each attempt.
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
            | null -> return raise (connectFailed lastError)
            | s -> return s
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
      new SocketsHttpHandler(
        UseProxy = Option.isSome proxy,
        Proxy =
          (match proxy with
           | Some(uri, credentials) ->
             new System.Net.WebProxy(uri, false, [||], credentials)
           | None -> null),
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
              let transport =
                new HttpMessageInvoker(
                  SocketBasedHandler.handler
                    config
                    (Some(selected, proxy.Credentials))
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
  { timeoutInMs = 30000
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

/// Send one validated request and read its whole response: the streaming send, drained here.
/// One deadline covers the exchange, body included, and a failure reading the body is the same
/// typed error as one sending, so a slow or broken body is a `Timeout` or a `NetworkError`.
let send
  (profile : Profile)
  (method : string)
  (canonicalUri : string)
  (headers : Headers)
  (body : byte[])
  : Task<Result<HttpResponse, HttpRequestError>> =
  task {
    use deadline =
      new System.Threading.CancellationTokenSource(profile.config.timeoutInMs)
    try
      match!
        sendForHeaders profile method canonicalUri headers body deadline.Token
      with
      | Error e -> return Error e
      | Ok response ->
        use response = response
        // The client's own buffering, as a buffered send does it, under the same deadline:
        // past the buffer limit it is an `HttpRequestException`, so a `NetworkError`.
        do!
          response.Content.LoadIntoBufferAsync(
            profile.client.MaxResponseContentBufferSize,
            deadline.Token
          )
        let! bytes = response.Content.ReadAsByteArrayAsync(deadline.Token)
        let headers =
          response |> headersForAspNetResponse |> normalizeResponseHeaders
        return
          Ok
            { statusCode = int response.StatusCode; headers = headers; body = bytes }
    with SendFailure canonicalUri typed ->
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
/// already passed the permission check. The deadline covers the headers only, so
/// a long-lived body (server-sent events) is not cut off.
let openStream
  (profile : Profile)
  (method : string)
  (canonicalUri : string)
  (headers : Headers)
  (body : byte[])
  : Task<Result<HttpStreamHead, HttpRequestError>> =
  task {
    let mutable responseToDispose : HttpResponseMessage option = None
    try
      use deadline =
        new System.Threading.CancellationTokenSource(profile.config.timeoutInMs)
      match!
        sendForHeaders profile method canonicalUri headers body deadline.Token
      with
      | Error e -> return Error e
      | Ok response ->
        // Not disposed here: the response must outlive this function; it is owned
        // by the stream table until `closeStream`.
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
    | SendFailure canonicalUri typed ->
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
