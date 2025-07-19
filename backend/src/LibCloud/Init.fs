module LibCloud.Init

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

let init (serviceName : string) : Task<unit> =
  task {
    printTime $"Initing LibCloud in {serviceName}"

    let queueTask = Queue.init ()
    let! (_ : List<unit>) = Task.flatten [ queueTask ]

    printTime $" Inited LibCloud in {serviceName}"
  }
