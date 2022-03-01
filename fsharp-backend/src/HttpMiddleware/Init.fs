module HttpMiddleware.Init

open Prelude

/// Initialize HttpMiddleware
let init (serviceName : string) : unit =
  print $"Initing HttpMiddleware in {serviceName}"
  print $" Inited HttpMiddleware in {serviceName}"
