/// Initialize LibRealExecution
module LibRealExecution.Init

open FSharp.Control.Tasks
open System.Threading.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {
    print $"Initing LibRealExecution in {serviceName}"
    do! RealExecution.init ()
    print $" Inited LibRealExecution in {serviceName}"
  }
