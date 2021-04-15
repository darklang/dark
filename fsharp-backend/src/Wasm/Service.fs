namespace Wasm

open BlazorWorker.WorkerCore
open FSharp.Control.Tasks

type EvalService(messageService : IWorkerMessageService) as this =
  do messageService.IncomingMessage.Add(this.OnMessage) |> ignore
  member this.Eval expr = ()
  static member EventsEval = $"Events.EvalRequested"
  static member ResultMessage = $"EvalResult"

  member this.OnMessage(message : string) =
    task {
      let! result = Eval.run 7
      do! messageService.PostMessageAsync(result)
    }
    |> ignore
