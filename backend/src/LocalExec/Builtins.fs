module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

let httpClientConfig = Builtins.Http.Client.Libs.HttpClient.defaultConfig
let ptPM = LibExecution.ProgramTypes.PackageManager.empty

/// for parsing packages, which may reference _any_ builtin
let all () : RT.Builtins =
  LibExecution.Builtin.combine
    [ Builtins.Pure.Builtin.builtins ()
      Builtins.Http.Client.Builtin.builtins httpClientConfig
      Builtins.Language.Builtin.builtins ()
      Builtins.Cli.Builtin.builtins ()
      Builtins.Time.Builtin.builtins ()
      Builtins.Random.Builtin.builtins ()
      Builtins.Matter.Builtin.builtins ptPM
      Builtins.CliHost.Builtin.builtins ()
      Builtins.Http.Server.Builtin.builtins ()
      TestUtils.LibTest.builtins () ]
    []
