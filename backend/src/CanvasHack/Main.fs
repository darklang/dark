module CanvasHack.Main

open System
open System.Threading.Tasks
open FSharp.Control.Tasks

open Legivel.Serialization
open Legivel.Attributes

open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module Op = LibBackend.Op
module C = LibBackend.Canvas

let initSerializers () = ()

let print (s : string) = System.Console.WriteLine(s)

let parseExpr (name : string) (s : string) =
  try
    Parser.parsePTExpr s
  with
  | e -> Exception.raiseCode $"Couldn't parse expression {name}\n{s}\n{e.Message}"

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
        | Some (Success s) -> s.Data
        | _ -> Exception.raiseCode "couldn't parse config file for canvas"

      let canvasName =
        match Prelude.CanvasName.create "dark-editor" with
        | Ok (c) -> c
        | Result.Error (_) -> Exception.raiseInternal "yolo" []
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
                System.IO.File.ReadAllText $"{baseDir}/{name}.dark" |> parseExpr name
              spec =
                PT.Handler.Spec.HTTP(
                  details.Path,
                  details.Method,
                  { moduleID = 129952UL; nameID = 33052UL; modifierID = 10038562UL }
                ) }

          [ PT.Op.TLSavepoint(140418122UL); PT.Op.SetHandler(handlerId, handler) ])

      let ops = List.collect identity createHttpHandlerOps

      let canvasWithTopLevels = C.fromOplist c [] ops

      let oplists =
        ops
        |> Op.oplist2TLIDOplists
        |> List.filterMap (fun (tlid, oplists) ->
          match Map.get tlid (C.toplevels canvasWithTopLevels) with
          | Some tl -> Some(tlid, oplists, tl, C.NotDeleted)
          | None -> None)
      // let _addOpsResponse =
      //   CanvasHack.AddOps.addOp c addOpsParams
      //   |> Async.AwaitTask
      //   |> Async.RunSynchronously

      C.saveTLIDs c oplists |> Async.AwaitTask |> Async.RunSynchronously

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
        Currently expecting just '{CommandNames.import}' or '{CommandNames.export}'"

    0
  with
  | e ->
    System.Console.WriteLine $"Error starting CanvasHack: {e}"
    1
