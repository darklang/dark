/// Initialize LibRealExecution
module LibExperimentalExecution.Init

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {
    print $"Initing RealExperimentalExecution in {serviceName}"
    do! RealExperimentalExecution.init ()
    print $" Inited RealExperimentalExecution in {serviceName}"
    return ()
  }
