/// Initialize LibCloudExecution
module LibCloudExecution.Init

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {
    print $"Initing LibCloudExecution in {serviceName}"
    do! CloudExecution.init ()
    print $" Inited LibCloudExecution in {serviceName}"
    return ()
  }
