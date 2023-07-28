module LibCliExecution.Init


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {
    print $"Initing LibCliExecution in {serviceName}"

    do! PackageManager.packageManager.init

    print $" Inited LibCliExecution in {serviceName}"
  }
