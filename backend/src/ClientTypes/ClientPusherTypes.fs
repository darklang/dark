/// Payloads that we send to the client via Pusher.com
module ClientTypes.Pusher

open Prelude

module Payload =
  type NewTrace = System.Guid * tlid list

  type New404 = string * string * string * NodaTime.Instant * System.Guid

  type AddOpV1 = { result : Ops.AddOpResultV1; ``params`` : Ops.AddOpParamsV1 }
  //type AddOpV1PayloadTooBig = { tlids : List<tlid> } // this is so-far unused

  type UpdateWorkerStates = Map<string, Worker.WorkerState>
