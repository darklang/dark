module LibService.Kestrel

open Prelude

type KestrelServerOptions =
  Microsoft.AspNetCore.Server.Kestrel.Core.KestrelServerOptions

// Apply some simple options so things don't get out of control
let configureKestrel (opts : KestrelServerOptions) : unit =
  opts.Limits.MaxConcurrentConnections <- 1000L
  opts.Limits.MaxConcurrentUpgradedConnections <- 0L // don't support websockets
  opts.Limits.MaxRequestBodySize <- 10L * 1024L * 1024L // 10MB
  opts.Limits.RequestHeadersTimeout <- System.TimeSpan.FromSeconds 10.0
  opts.AllowSynchronousIO <- false // prevent deadlock of some kind
  opts.AddServerHeader <- false // Don't add kestrel
