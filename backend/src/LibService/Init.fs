module LibService.Init

open Prelude

let init (serviceName : string) : unit =
  printTime $"Initing LibService in {serviceName}"
  printTime $" Inited LibService in {serviceName}"


let shutdown (serviceName : string) : unit =
  printTime $"Shutting down LibService in {serviceName}"
  printTime $"Shut down LibService in {serviceName}"
