/// Initialize LibCloudExecution
module LibCloudExecution.Init

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {
    printTime $"Initing LibCloudExecution in {serviceName}"
    do! CloudExecution.init ()
    printTime $" Inited LibCloudExecution in {serviceName}"
    return ()
  }
