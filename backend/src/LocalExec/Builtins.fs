module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

let ptPM = LibExecution.ProgramTypes.PackageManager.empty

/// for parsing packages, which may reference _any_ builtin
let all () : RT.Builtins =
  LibExecution.Builtin.combine
    [ Builtins.Pure.Builtin.builtins ()
      Builtins.Http.Client.Builtin.builtins ()
      Builtins.Language.Builtin.builtins ()
      Builtins.Cli.Builtin.builtins ()
      Builtins.Time.Builtin.builtins ()
      Builtins.Random.Builtin.builtins ()
      Builtins.Matter.Builtin.builtins ptPM
      Builtins.CliHost.Builtin.builtins ()
      Builtins.Http.Server.Builtin.builtins ()
      TestUtils.LibTest.builtins ()
#if DARK_WITH_COMPILER
      // 10th entry: the airlifted compiler extension. Present only in
      // -p:DarkWithCompiler=true builds; the core links without it.
      Builtins.Compiler.Builtin.builtins ()
#endif
      ]
    []
