namespace Wasm

open BlazorWorker.WorkerCore
open FSharp.Control.Tasks

#nowarn "988"

type EvalService(messageService : IWorkerMessageService) as this =
  do messageService.IncomingMessage.Add(this.OnMessage) |> ignore

  member this.OnMessage(message : string) =
    task {
      let! result = Eval.performAnalysis message
      do! messageService.PostMessageAsync result
    }
    |> ignore
