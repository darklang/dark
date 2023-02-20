module Executor.Arguments

open Prelude
open Tablecloth

type Mode =
  | Serve of port : int * healthCheckPort : int
  | Execute of List<string>
  | Help
  | Version

type Config = { debug : bool }
let defaultConfig = { debug = false }


let printHelp () : unit =
  System.Console.Out.WriteLine
    "Usage: darklang-executor [serve [--port=3275] [--healthCheckPort=3276]] [--debug] ...files"
  System.Console.Out.WriteLine
    "
  [files] Execute the given files, printing the result to stdout
          Use '-' for stdin

  serve [--port=3275] [--healthCheckPort=3276] Run a server, accepting requests on the given port:

    POST /api/v0/execute-text
      Request Body (json): { code: string, symtable: Map<string, Dval> }
      Response body (json): Dval

    POST /api/v0/execute-json
      Request Body (json): { expr: Expr, symtable: Map<string, Dval> }
      Response body (json): Dval

    GET /api/v0/version
      Response body (json): { version: string, date: datetime-string, inDevelopment: bool }
"
  System.Console.Out.WriteLine "  --debug  Enable debug logging"
  System.Console.Out.WriteLine "  --help  Print this help message"
  System.Console.Out.WriteLine "  --version  Print version information"

let printVersion () : unit =
  let i = VersionInfo.info ()
  System.Console.Out.WriteLine
    $"darklang-executor\nhash: {i.hash}\nbuild-date: {i.buildDate}\ndevelopment version? : {i.inDevelopment}"

let parse (cliArgs : List<string>) : (Mode * Config) =
  let result =
    List.fold
      (None, defaultConfig)
      (fun ((mode, config) : Option<Mode> * Config) (cliArg : string) ->
        match mode, cliArg |> String.split "=" with
        // help
        | Some Help, _ -> (Some Help, config)
        | _, [ "--help" ] -> (Some Help, config)

        // version
        | Some Version, _ -> (Some Version, config)
        | _, [ "--version" ] -> (Some Version, config)

        // debug
        | mode, [ "--debug" ] -> (mode, { config with debug = true })

        // serve
        | None, [ "serve" ] ->
          (Some(Serve(port = 3275, healthCheckPort = 3276)), config)
        // serve --port
        | Some (Serve (_, hcPort)), [ "--port"; port ] ->
          (Some(Serve(port = int port, healthCheckPort = hcPort)), config)
        // serve --healthCheckPort
        | Some (Serve (port, _)), [ "--healthCheckPort"; hcPort ] ->
          (Some(Serve(port = port, healthCheckPort = int hcPort)), config)

        // file list
        | None, [ file ] -> (Some(Execute [ file ]), config)
        | Some (Execute files), [ file ] -> (Some(Execute(files @ [ file ])), config)

        // other
        | _ ->
          print $"Invalid argument {cliArg}, in mode {mode}"
          (Some Help, config))
      cliArgs
  match result with
  | (None, _) -> (Help, defaultConfig)
  | (Some mode, config) -> (mode, config)
