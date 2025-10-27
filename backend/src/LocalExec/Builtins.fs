module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

let httpClientConfig = BuiltinExecution.Libs.HttpClient.defaultConfig
let ptPM = LibExecution.ProgramTypes.PackageManager.empty

/// for parsing packages, which may reference _any_ builtin
let all : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins httpClientConfig
      BuiltinCli.Builtin.builtins
      BuiltinPM.Builtin.builtins ptPM
      BuiltinCliHost.Builtin.builtins
      BuiltinCloudExecution.Builtin.builtins // TODO: do we need this?
      TestUtils.LibTest.builtins ] // TODO: or this?
    []

let accessibleByCanvas : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins httpClientConfig
      BuiltinPM.Builtin.builtins ptPM
      BuiltinCloudExecution.Builtin.builtins
      //?BuiltinDarkInternal.Builtin.builtins
      ]
    []
