/// Initialize HttpMiddleware
module HttpMiddleware.Init

open Prelude

let init (serviceName : string) : unit =
  print $"Initing HttpMiddleware in {serviceName}"
  print $" Inited HttpMiddleware in {serviceName}"
