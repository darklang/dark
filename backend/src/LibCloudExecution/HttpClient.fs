module LibCloudExecution.HttpClient

open System.IO
open System.Net.Http

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution
open LibExecution.RuntimeTypes


// For security, we want to prevent access to internal IP address ranges or
// Instance Metadata service or localhost, or anything Cloud Run routes internally
// 1. via hostname
// 2. via IP address in the connectionCallback
// 3. via IP tables on the container (see TODO)
// 4. via header for Instance Metadata service
// 5. By removing all access for the cloud run service account (see iam.tf)
module LocalAccess =

  let bannedHost (host : string) =
    let host = host.Trim().ToLower()
    host = "localhost" || host = "metadata" || host = "metadata.google.internal"



  // Cloud Run lists the IPs it routes internally here:
  // https://cloud.google.com/run/docs/configuring/vpc-connectors?#manage
  // Ban all of them

  // https://datatracker.ietf.org/doc/html/rfc1918#section-3
  let ten = System.Net.IPNetwork.Parse "10.0.0.0/8"
  let oneSevenTwo = System.Net.IPNetwork.Parse "172.16.0.0/12"
  let oneNineTwo = System.Net.IPNetwork.Parse "192.168.0.0/16"

  // https://datatracker.ietf.org/doc/html/rfc6598#section-7
  let oneHundred = System.Net.IPNetwork.Parse "100.64.0.0/10"

  // 199.36.153.4/30 and 199.36.153.8/30
  let oneNineNineFour = System.Net.IPNetwork.Parse "199.36.153.4/30"
  let oneNineNineEight = System.Net.IPNetwork.Parse "199.36.153.8/30"

  // 169.254.0.0 - 169.254.255.255 (169.254.0.0/16, link-local addresses)
  let oneSixNine = System.Net.IPNetwork.Parse "169.254.0.0/16"

  let zero = System.Net.IPAddress.Parse "0.0.0.0"


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
    |> List.find (fun (k, v) ->
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

let configuration : BuiltinExecution.Libs.HttpClient.Configuration =
  { timeoutInMs = LibCloud.Config.httpclientTimeoutInMs
    allowedIP = (fun ip -> not <| LocalAccess.bannedIp ip)
    allowedHost = (fun host -> not <| LocalAccess.bannedHost host)
    allowedScheme = (fun scheme -> scheme = "https" || scheme = "http")
    allowedHeaders =
      (fun headers -> not <| LocalAccess.hasInstanceMetadataHeader headers)
    telemetryInitialize = initializeTelemetry
    telemetryAddTag = LibService.Telemetry.addTag
    telemetryAddException = LibService.Telemetry.addException }
