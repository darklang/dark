module CanvasHack.Main

open System
open System.Threading.Tasks
open FSharp.Control.Tasks


open Prelude
open Tablecloth

module PT = LibExecution.ProgramTypes
module Op = LibBackend.Op
module C = LibBackend.Canvas

let initSerializers () = ()

module CommandNames =
  [<Literal>]
  let import = "load-from-disk"

  [<Literal>]
  let export = "save-to-disk"

type CanvasHackConfig =
  { [<Legivel.Attributes.YamlField("http-handlers")>]
    HttpHandlers : Map<string, CanvasHackHttpHandler> }

and CanvasHackHttpHandler =
  { [<Legivel.Attributes.YamlField("method")>]
    Method : string

    [<Legivel.Attributes.YamlField("path")>]
    Path : string }

let baseDir = $"dark-editor"

[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()

    task {
      // See if "dark" user exists and create them if not
      let username = UserName.create "dark"
      match! LibBackend.Account.getUser username with
      | None ->
        print "creating dark user"
        let! r = LibBackend.Account.upsertNonAdmin { username = username }
        Result.unwrap () r
      | Some _ -> print "Using existing dark user"


      match args with
      | [||] ->
        print
          $"`canvas-hack {CommandNames.import}` to load dark-editor from disk or
            `canvas-hack {CommandNames.export}' to save dark-editor to disk"

      | [| CommandNames.import |] ->
        // Read+parse the config.yml file
        let configFileContents : string =
          $"{baseDir}/config.yml" |> System.IO.File.ReadAllText

        let config =
          Legivel.Serialization.Deserialize<CanvasHackConfig> configFileContents

        // TODO: better error-handling here
        let config : CanvasHackConfig =
          match List.head config with
          | Some (Legivel.Serialization.Success s) -> s.Data
          | _ -> Exception.raiseCode "couldn't parse config file for canvas"

        let canvasName =
          match CanvasName.create "dark-editor" with
          | Ok (c) -> c
          | Error (_) -> Exception.raiseInternal "yolo" []
        let! c = LibBackend.Canvas.getMetaAndCreate canvasName

        // For each of the handlers defined in `config.yml`,
        // produce a set of Ops to add.
        // TODO: I'm not sure why I can't seem to omit the
        // `TLSavepoint` or decrease the opCtr and still have this work.
        let createHttpHandlerOps =
          config.HttpHandlers
          |> Map.toList
          |> List.map (fun (name, details) ->
            let handlerId = gid ()
            let ast =
              System.IO.File.ReadAllText $"{baseDir}/{name}.dark"
              |> Parser.parsePTExpr

            let handler : PT.Handler.T =
              { tlid = handlerId
                ast = ast
                spec =
                  PT.Handler.Spec.HTTP(
                    details.Path,
                    details.Method,
                    { moduleID = 129952UL
                      nameID = 33052UL
                      modifierID = 10038562UL }
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

        do! C.saveTLIDs c oplists

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

      return 0
    }
    |> fun x -> x.Result
  with
  | e ->
    printException "" [] e
    1
