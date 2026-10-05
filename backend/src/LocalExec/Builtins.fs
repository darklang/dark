module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

let ptPM = LibExecution.ProgramTypes.PackageManager.empty

/// The same set, with the package builtins answering from <param pm>.
let allWith (pm : LibExecution.ProgramTypes.PackageManager) : RT.Builtins =
  LibExecution.Builtin.combine
    [ Builtins.Pure.Builtin.builtins ()
      Builtins.Http.Client.Builtin.builtins ()
      Builtins.Language.Builtin.builtins ()
      Builtins.Cli.Builtin.builtins ()
      Builtins.Time.Builtin.builtins ()
      Builtins.Random.Builtin.builtins ()
      Builtins.Matter.Builtin.builtins pm
      Builtins.CliHost.Builtin.builtins ()
      Builtins.Http.Server.Builtin.builtins ()
      TestUtils.LibTest.builtins () ]
    []

/// for parsing packages, which may reference _any_ builtin
let all () : RT.Builtins = allWith ptPM
