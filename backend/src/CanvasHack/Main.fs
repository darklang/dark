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

let baseDir = $"canvases/dark-editor"

[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()

    task {
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

        // Create the canvas - expect this to be empty
        let domain = "dark-editor.dlio.localhost"
        let canvasID = LibBackend.Config.allowedDarkInternalCanvasID
        let! ownerID = LibBackend.Account.createUser ()
        do! LibBackend.Canvas.createWithExactID canvasID ownerID domain

        // Parse each file, extract the functions and types. The expressions will be
        // used for handlers according to the config file

        let ops =
          config.HttpHandlers
          |> Map.toList
          |> List.map (fun (name, details) ->
            let modul = Parser.parseModule [] $"{baseDir}/{name}.dark"

            let types = modul.types |> List.map PT.Op.SetType
            let fns = modul.fns |> List.map PT.Op.SetFunction
            let handler =
              match modul.exprs with
              | [ expr ] ->
                PT.Op.SetHandler(
                  { tlid = gid ()
                    ast = expr
                    spec =
                      PT.Handler.Spec.HTTP(
                        details.Path,
                        details.Method,
                        { moduleID = gid (); nameID = gid (); modifierID = gid () }
                      ) }
                )
              | _ -> Exception.raiseCode "expected exactly one expr in file"

            [ PT.Op.TLSavepoint(140418122UL) ] @ types @ fns @ [ handler ])
          |> List.flatten

        let canvasWithTopLevels = C.fromOplist canvasID [] ops

        let oplists =
          ops
          |> Op.oplist2TLIDOplists
          |> List.filterMap (fun (tlid, oplists) ->
            match Map.get tlid (C.toplevels canvasWithTopLevels) with
            | Some tl -> Some(tlid, oplists, tl, C.NotDeleted)
            | None -> None)

        do! C.saveTLIDs canvasID oplists

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
