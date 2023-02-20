module CanvasHack.Main

open System
open System.Threading.Tasks
open FSharp.Control.Tasks

open Legivel.Serialization
open Legivel.Attributes

module PT = LibExecution.ProgramTypes

let initSerializers () = ()

let print (s : string) = System.Console.WriteLine(s)

module CommandNames =
  [<Literal>]
  let import = "load-from-disk"

  [<Literal>]
  let export = "save-to-disk"

type CanvasHackConfig =
  { [<YamlField("http-handlers")>]
    HttpHandlers : Map<string, CanvasHackHttpHandler> }
and CanvasHackHttpHandler =
  { [<YamlField("method")>]
    Method : string

    [<YamlField("path")>]
    Path : string }

let baseDir = $"{LibBackend.Config.backendDir}/src/CanvasHack/dark-editor"

[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()

    match args with
    | [||] ->
      print
        $"`canvas-hack {CommandNames.import}` to load dark-editor from disk or
          `canvas-hack {CommandNames.export}' to save dark-editor to disk"

    | [| CommandNames.import |] ->
      // Read+parse the config.yml file
      let configFileContents : string =
        $"{baseDir}/config.yml" |> System.IO.File.ReadAllText

      let config = Deserialize<CanvasHackConfig> configFileContents

      // TODO: better error-handling here
      let config : CanvasHackConfig =
        match List.head config with
        | Success s -> s.Data
        | _ -> failwith "couldn't parse config file for canvas"

      // Purge any existing canvas under the name
      print "TODO - purge dark-editor canvas"


      let canvasName =
        match Prelude.CanvasName.create "dark-editor" with
        | Ok (c) -> c
        | Result.Error (_) -> failwith "yolo"
      let c =
        LibBackend.Canvas.getMetaAndCreate canvasName
        |> Async.AwaitTask
        |> Async.RunSynchronously

      // For each of the handlers defined in `config.yml`,
      // produce a set of Ops to add.
      // TODO: I'm not sure why I can't seem to omit the
      // `TLSavepoint` or decrease the opCtr and still have this work.
      let createHttpHandlerOps =
        config.HttpHandlers
        |> Map.toList
        |> List.map (fun (name, details) ->
          let handlerId = Prelude.gid ()

          let handler : PT.Handler.T =
            { tlid = handlerId
              ast =
                System.IO.File.ReadAllText $"{baseDir}/{name}.dark"
                |> Parser.parsePTExpr
              spec =
                PT.Handler.Spec.HTTP(
                  details.Path,
                  details.Method,
                  { moduleID = 129952UL; nameID = 33052UL; modifierID = 10038562UL }
                ) }

          [ PT.Op.TLSavepoint(140418122UL); PT.Op.SetHandler(handlerId, handler) ])

      let addOpsParams : LibBackend.Op.AddOpParamsV1 =
        { ops = List.collect Prelude.identity createHttpHandlerOps
          opCtr = 2
          clientOpCtrID = "4803612a-cf8c-4aa0-8f87-0a82daa923c4" }

      let addOpsResponse =
        CanvasHack.AddOps.addOp c addOpsParams
        |> Async.AwaitTask
        |> Async.RunSynchronously

      ()

    | [| CommandNames.export |] ->
      // Find the canvas
      print "TODO: get context of the canvas"

      // Get the list of HTTP Handlers configured
      print "TODO: get list of http handlers, incl. code, path, method"

      // Replace the .dark files on disk

      // For each of the current HTTP handlers
      //    - serialize it (`let a = 1 + 2`)
      //      (write serializer in dark?)
      //    - save to disk
      //      (? how do we choose the name)

      // 5. Save to .dark files

      print "TODO"

    | _ ->
      print
        $"CanvasHack isn't sure what to do with these arguments.
        Currently expecting just '{CommandNames.import} [canvasName]'
                              or '{CommandNames.export} [canvasName]'"

    0
  with
  | e ->
    System.Console.WriteLine $"Error starting CanvasHack: {e}"
    1
