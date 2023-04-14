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

let baseDir = $"canvases"

module CommandNames =
  [<Literal>]
  let import = "load-from-disk"

  [<Literal>]
  let export = "save-to-disk"

module CanvasHackConfig =
  type JustVersion =
    { [<Legivel.Attributes.YamlField("version")>]
      Version : string }

  // One file per HTTP handler
  type V1 =
    { [<Legivel.Attributes.YamlField("id")>]
      CanvasId : string

      [<Legivel.Attributes.YamlField("http-handlers")>]
      HttpHandlers : Map<string, V1HttpHandler> }

  and V1HttpHandler =
    { [<Legivel.Attributes.YamlField("method")>]
      Method : string

      [<Legivel.Attributes.YamlField("path")>]
      Path : string }

  // One .dark file to define the full canvas
  type V2 =
    { [<Legivel.Attributes.YamlField("id")>]
      CanvasId : string

      [<Legivel.Attributes.YamlField("main")>]
      Main : string }

let parseYamlExn<'a> (filename : string) : 'a =
  let contents = System.IO.File.ReadAllText filename
  let deserialized = Legivel.Serialization.Deserialize<'a> contents

  match List.head deserialized with
  | Some (Legivel.Serialization.Success s) -> s.Data
  | ex ->
    //Exception.raiseCode "couldn't parse config file for canvas"
    Exception.raiseCode $"couldn't parse {filename}" [ "error", ex ]

let seedCanvasV1 (canvasName : string) =
  task {
    let canvasDir = $"{baseDir}/{canvasName}"
    let config = parseYamlExn<CanvasHackConfig.V1> $"{canvasDir}/config.yml"

    // Create the canvas - expect this to be empty
    let domain = $"{canvasName}.dlio.localhost"
    let host = $"http://{domain}:{LibService.Config.bwdServerPort}"
    let canvasID = Guid.Parse config.CanvasId
    let! ownerID = LibBackend.Account.createUser ()
    do! LibBackend.Canvas.createWithExactID canvasID ownerID domain

    // Parse each file, extract the functions and types. The expressions will be
    // used for handlers according to the config file

    let ops =
      config.HttpHandlers
      |> Map.toList
      |> List.map (fun (name, details) ->
        let modul =
          Parser.CanvasV1.parseHandlerFromFile Map.empty $"{canvasDir}/{name}.dark"

        let types = modul.types |> List.map PT.Op.SetType

        let fns = modul.fns |> List.map PT.Op.SetFunction

        let handler =
          match modul.httpHandlers with
          | [ expr ] ->
            print $"Adding {details.Method} {details.Path} HTTP handler"
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

        [ PT.Op.TLSavepoint(gid ()) ] @ types @ fns @ [ handler ])
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
    print $"Success saved canvas - endpoints available at {host}"
  }

let seedCanvasV2 (canvasName : string) =
  task {
    let canvasDir = $"{baseDir}/{canvasName}"
    let config = parseYamlExn<CanvasHackConfig.V2> $"{canvasDir}/config.yml"

    // Create the canvas - expect this to be empty
    let domain = $"{canvasName}.dlio.localhost"
    let host = $"http://{domain}:{LibService.Config.bwdServerPort}"
    let experimentalHost = $"http://{domain}:{LibService.Config.bwdDangerServerPort}"
    let canvasID = Guid.Parse config.CanvasId

    let! ownerID = LibBackend.Account.createUser ()
    do! LibBackend.Canvas.createWithExactID canvasID ownerID domain

    let ops =
      let modul =
        Parser.CanvasV2.parseFromFile Map.empty $"{canvasDir}/{config.Main}.dark"

      let types = modul.types |> List.map PT.Op.SetType
      let fns = modul.fns |> List.map PT.Op.SetFunction

      let handlers =
        modul.handlers
        |> List.map (fun (spec, ast) ->
          PT.Op.SetHandler({ tlid = gid (); ast = ast; spec = spec }))

      let createSavepoint = PT.Op.TLSavepoint(gid ())

      [ createSavepoint ] @ types @ fns @ handlers

    let canvasWithTopLevels = C.fromOplist canvasID [] ops

    let oplists =
      ops
      |> Op.oplist2TLIDOplists
      |> List.filterMap (fun (tlid, oplists) ->
        match Map.get tlid (C.toplevels canvasWithTopLevels) with
        | Some tl -> Some(tlid, oplists, tl, C.NotDeleted)
        | None -> None)

    do! C.saveTLIDs canvasID oplists
    print
      $"Success saved canvas - endpoints available at {host} (bwdserver) and {experimentalHost} (bwd-danger-server)"
  }

[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()

    task {
      match args with
      | [||]
      | [| "--help" |] ->
        print
          $"`canvas-hack {CommandNames.import}` to load dark-editor from disk or
            `canvas-hack {CommandNames.export}' to save dark-editor to disk"

      | [| CommandNames.import; canvasName |] ->
        let config =
          parseYamlExn<CanvasHackConfig.JustVersion>
            $"{baseDir}/{canvasName}/config.yml"

        match config.Version with
        | "1" -> do! seedCanvasV1 canvasName
        | "2" -> do! seedCanvasV2 canvasName
        | _ -> Exception.raiseCode "unknown canvas import config version"

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

      NonBlockingConsole.wait ()
      return 0
    }
    |> fun x -> x.Result

  with
  | e ->
    printException "Exception running CanvasHack" [ "args", args ] e
    1
