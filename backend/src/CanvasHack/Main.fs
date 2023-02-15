module CanvasHack.Main

open System
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

let initSerializers () =
  // Json.Vanilla.allow<pos> "Prelude"

  // one-off types used internally
  // Json.Vanilla.allow<LibExecution.ProgramTypes.Oplist> "Canvas.loadJsonFromDisk"
  // Json.Vanilla.allow<LibExecution.ProgramTypes.Position> "Canvas.saveTLIDs"
  // Json.Vanilla.allow<LibExecution.DvalReprInternalNew.RoundtrippableSerializationFormatV0.Dval>
  //   "RoundtrippableSerializationFormatV0.Dval"
  // Json.Vanilla.allow<LibBackend.Analytics.HeapIOMetadata> "heap.io metadata"
  // Json.Vanilla.allow<LibBackend.EventQueueV2.NotificationData> "eventqueue storage"
  // Json.Vanilla.allow<LibBackend.PackageManager.ParametersDBFormat> "PackageManager"
  // Json.Vanilla.allow<LibBackend.Session.JsonData> "LibBackend session db storage"

  // for API request/response payloads
  // Json.Vanilla.allow<CTApi.Workers.WorkerStats.Request> "ApiServer.Workers"
  // Json.Vanilla.allow<CTApi.Workers.WorkerStats.Response> "ApiServer.Workers"
  ()



[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()

    print "Running CanvasHack"

    0
  with
  | e ->
    System.Console.WriteLine $"Error starting CanvasHack: {{e}}"
    1
