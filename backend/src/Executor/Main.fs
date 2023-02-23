module Executor.Main

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth

let executeFiles (files : string list) : unit =
  let expr =
    files
    |> List.map (fun file ->
      if file = "-" then
        System.Console.In.ReadToEnd()
      else
        System.IO.File.ReadAllText file)
    |> String.concat "\n"
    |> Parser.parseRTExpr

  let result = Execute.execute expr Map.empty
  let dval = result.Result
  let output = LibExecution.DvalReprLegacyExternal.toEnduserReadableTextV0 dval
  System.Console.Out.WriteLine output


// Generally speaking, this should be a superset of BwdServer's list.
let initSerializers () =
  // universally-serializable types
  Json.Vanilla.allow<pos> "Prelude"

// one-off types used internally
// Json.Vanilla.allow<LibExecution.ProgramTypes.Oplist> "Canvas.loadJsonFromDisk"
// Json.Vanilla.allow<LibExecution.DvalReprInternalRoundtrippable.FormatV0.Dval>
//   "RoundtrippableSerializationFormatV0.Dval"
// Json.Vanilla.allow<LibBackend.Analytics.HeapIOMetadata> "heap.io metadata"
// Json.Vanilla.allow<LibBackend.EventQueueV2.NotificationData> "eventqueue storage"
// Json.Vanilla.allow<LibBackend.PackageManager.ParametersDBFormat> "PackageManager"
// Json.Vanilla.allow<LibBackend.Session.JsonData> "LibBackend session db storage"
// Json.Vanilla.allow<LibBackend.TraceCloudStorage.CloudStorageFormat>
//   "TraceCloudStorageFormat"
// Json.Vanilla.allow<LibService.Rollbar.HoneycombJson> "Rollbar"

// for API request/response payloads
// Json.Vanilla.allow<CTApi.Workers.WorkerStats.Request> "ApiServer.Workers"
// Json.Vanilla.allow<CTApi.Workers.WorkerStats.Response> "ApiServer.Workers"

[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()
    let (mode, config) = args |> List.fromArray |> Arguments.parse
    match mode with
    | Arguments.Serve (port, hcPort) -> WebServer.runServer config.debug port hcPort
    | Arguments.Execute files -> executeFiles files
    | Arguments.Help -> Arguments.printHelp ()
    | Arguments.Version -> Arguments.printVersion ()

    // LibService.Init.init name
    // (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
    // (LibRealExecution.Init.init name).Result

    0
  // LibService.Init.shutdown name 0
  with
  | e ->
    System.Console.WriteLine $"Error starting Executor: {e}"
    1
//  LibService.Rollbar.lastDitchBlockAndPage "Error starting ApiServer" e
