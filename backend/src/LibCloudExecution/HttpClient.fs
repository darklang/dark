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

let initializeTelemetry (f : unit -> Task<'a>) : Task<'a> =
  task {
    use _ = LibService.Telemetry.child "HttpClient.call" []
    return! f ()
  }

let configuration : StdLibExecution.Libs.HttpClient.Configuration =
  { timeoutInMs = 10000
    allowedIP = (fun ip -> not <| LocalAccess.bannedIp ip)
    allowedHost = (fun host -> not <| LocalAccess.bannedHost host)
    allowedScheme = (fun scheme -> scheme = "https" || scheme = "http")
    allowedHeaders =
      (fun headers -> not <| LocalAccess.hasInstanceMetadataHeader headers)
    telemetryInitialize = initializeTelemetry
    telemetryAddTag = LibService.Telemetry.addTag
    telemetryAddException = LibService.Telemetry.addException }
