module CanvasHack.Main

open System
open System.Threading.Tasks
open FSharp.Control.Tasks

open Legivel.Serialization
open Legivel.Attributes

let initSerializers () = ()

let print (s : string) = System.Console.WriteLine(s)

module CommandNames =
  [<Literal>]
  let import = "load-from-disk"

  [<Literal>]
  let export = "save-to-disk"

type CanvasHackConfig =
  { [<YamlField("http-handlers")>]
    HttpHandlers : List<CanvasHackHttpHandler> }
and CanvasHackHttpHandler =
  { [<YamlField("method")>]
    Method : string

    [<YamlField("path")>]
    Path : string }

[<EntryPoint>]
let main (args : string []) =
  try
    initSerializers ()

    match args with
    | [||] ->
      print
        $"`canvas-hack {CommandNames.import} [canvas-name]` or
          `canvas-hack {CommandNames.export} [canvas-name]`"

    | [| CommandNames.import; canvasName |] ->
      // 1. Make sure canvasName is OK
      print "TODO make sure canvas name is OK"

      // 2. Read+parse the config.yml file in the `canvas-bootstraps` dir
      //  - it should just list/describe `http-handlers` for now
      // todo: read the actual file
      let configFileContents : string =
        $"{LibBackend.Config.backendDir}/src/CanvasHack/canvas-bootstraps/{canvasName}/config.yml"
        |> System.IO.File.ReadAllText

      let config = Deserialize<CanvasHackConfig> configFileContents

      // TODO: better error-handling here
      let config : CanvasHackConfig =
        match List.head config with
        | Success s -> s.Data
        | _ -> failwith "couldn't parse config file for canvas"

      print (sprintf "%A" config)

      // 3. Purge any existing canvas under the name
      print "TODO - purge canvas"



      // 4. For each of the handlers defined in `config.yml`,

      //   a. Read+parse the corresponding .dark file from the `canvas-bootstraps` dir
      //     (get to an AST)

      //   b. create the corresponding http handler (so it's live)
      //     (AddOp)

      print "TODO"

    | [| CommandNames.export; _canvasName |] ->
      // 1. Find the canvas

      // 2. Get the list of HTTP Handlers configured
      //   (later, we can expand this to REPLs and other things)
      //   (maybe for now, we can at least raise an error if other things somehow exist (via an AddOp somehow))

      // 3. purge .dark files from disk
      //    (keep the config.yml)

      // 4. For each of the current HTTP handlers
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
