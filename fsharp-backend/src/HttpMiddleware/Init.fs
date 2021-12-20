module HttpMiddleware.Init

// Initialize HttpMiddleware

open Prelude

let init (serviceName : string) : unit =
  print $"Initing HttpMiddleware in {serviceName}"
  print $" Inited HttpMiddleware in {serviceName}"
