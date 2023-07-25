module LibCliExecution.Init


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {
    print $"Initing LibCliExecution in {serviceName}"

    // TODO: pre-fetch some packages?
    //let packageManager = PackageManager.packageManager
    //let! _fns = packageManager.getAllFns
    //let! _types = packageManager.getAllTypes

    print $" Inited LibCliExecution in {serviceName}"
  }
