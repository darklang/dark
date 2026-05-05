module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

let httpClientConfig = Builtins.Execution.Libs.HttpClient.defaultConfig
let ptPM = LibExecution.ProgramTypes.PackageManager.empty

/// for parsing packages, which may reference _any_ builtin
let all () : RT.Builtins =
  LibExecution.Builtin.combine
    [ Builtins.Execution.Builtin.builtins httpClientConfig
      Builtins.Cli.Builtin.builtins ()
      Builtins.PM.Builtin.builtins ptPM
      Builtins.CliHost.Builtin.builtins ()
      Builtins.HttpServer.Builtin.builtins ()
      Builtins.DB.Builtin.builtins ()
      Builtins.DarkInternal.Builtin.builtins ()
      Builtins.Tracing.Builtin.builtins ()
      TestUtils.LibTest.builtins () ]
    []

let accessibleByCanvas () : RT.Builtins =
  LibExecution.Builtin.combine
    [ Builtins.Execution.Builtin.builtins httpClientConfig
      Builtins.PM.Builtin.builtins ptPM
      Builtins.HttpServer.Builtin.builtins ()
      Builtins.DB.Builtin.builtins ()
      //?Builtins.DarkInternal.Builtin.builtins
      ]
    []
