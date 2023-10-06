module CanvasHack.Main

open System
open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module PT = LibExecution.ProgramTypes
module C = LibCloud.Canvas

let initSerializers () = ()

let baseDir = $"canvases"

module CommandNames =
  [<Literal>]
  let import = "load-from-disk"

type CanvasHackConfig =
  { [<Legivel.Attributes.YamlField("id")>]
    CanvasId : Option<string>

    [<Legivel.Attributes.YamlField("main")>]
    Main : string }

let parseYamlExn<'a> (filename : string) : 'a =
  let contents = System.IO.File.ReadAllText filename
  let deserialized = Legivel.Serialization.Deserialize<'a> contents

  match List.head deserialized with
  | Some(Legivel.Serialization.Success s) -> s.Data
  | ex -> raise (Exception($"couldn't parse {filename} with error {ex}"))

let packageManager = LibCloud.PackageManager.packageManager

let seedCanvas (canvasName : string) =
  task {
    let canvasDir = $"{baseDir}/{canvasName}"
    let config = parseYamlExn<CanvasHackConfig> $"{canvasDir}/config.yml"

    // Create the canvas - expect this to be empty
    let domain = $"{canvasName}.dlio.localhost"
    let host = $"http://{domain}:{LibService.Config.bwdServerPort}"
    let experimentalHost = $"http://{domain}:{LibService.Config.bwdDangerServerPort}"
    let canvasID =
      match config.CanvasId with
      | None -> Guid.NewGuid()
      | Some id -> Guid.Parse id

    let! ownerID = LibCloud.Account.createUser ()
    do! LibCloud.Canvas.createWithExactID canvasID ownerID domain

    let resolver =
      let builtIns =
        LibExecution.Builtin.combine
          [ BuiltinExecution.Builtin.contents
              BuiltinExecution.Libs.HttpClient.defaultConfig
            BuiltinCloudExecution.Builtin.contents
            BwdDangerServer.Builtin.contents
            BuiltinDarkInternal.Builtin.contents ]
          []
      LibParser.NameResolver.fromBuiltins builtIns
      |> fun nr -> { nr with packageManager = Some packageManager }

    let! tls =
      uply {
        let! modul =
          LibParser.Canvas.parseFromFile resolver $"{canvasDir}/{config.Main}.dark"

        let types = modul.types |> List.map PT.Toplevel.TLType
        let fns = modul.fns |> List.map PT.Toplevel.TLFunction

        let handlers =
          modul.handlers
          |> List.map (fun (spec, ast) ->
            PT.Toplevel.TLHandler { tlid = gid (); ast = ast; spec = spec })

        let dbs = modul.dbs |> List.map PT.Toplevel.TLDB

        return
          (types @ dbs @ fns @ handlers)
          |> List.map (fun tl -> tl, LibCloud.Serialize.NotDeleted)
      }

    do! C.saveTLIDs canvasID tls


    // now that the canvas has been seeded, load any secrets in a .secrets file
    let secretsFileLocation = $"{canvasDir}/.secrets"

    if System.IO.File.Exists secretsFileLocation then
      // read this file
      do!
        secretsFileLocation
        |> System.IO.File.ReadAllLines
        |> Array.filter (String.startsWith "#" >> not)
        |> Array.filter (String.isEmpty >> not)
        |> Array.map (fun line ->
          match line.Split('=') with
          | [| key; value |] -> (key, value)
          | _ -> Exception.raiseInternal "invalid .secrets file" [])
        |> Array.toList
        |> Task.iterSequentially (fun (k, v) ->
          LibCloud.Secret.insert canvasID k v 0)
    else
      // we don't have secrets to load - we're done
      ()

    print
      $"Success saved canvas - endpoints available
       at {host} (bwdserver)
      and {experimentalHost} (bwd-danger-server)"
  }

[<EntryPoint>]
let main (args : string[]) =
  try
    initSerializers ()

    task {
      match args with
      | [||]
      | [| "--help" |] ->
        print $"`canvas-hack {CommandNames.import}` to load dark-editor from disk"

      | [| canvasName |]
      | [| CommandNames.import; canvasName |] ->
        print $"Loading canvas {canvasName} from disk"
        do! seedCanvas canvasName

      | _ ->
        let args = args |> Array.toList |> String.concat " "
        print
          $"CanvasHack isn't sure what to do with these arguments: [{args}]
          Currently expecting just '{CommandNames.import}'"

      NonBlockingConsole.wait ()
      return 0
    }
    |> fun x -> x.Result

  with e ->
    printException "Exception running CanvasHack" [ "args", args ] e
    1
