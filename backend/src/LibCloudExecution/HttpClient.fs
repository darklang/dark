module LibCloudExecution.HttpClient

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes

open VendoredTablecloth

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
