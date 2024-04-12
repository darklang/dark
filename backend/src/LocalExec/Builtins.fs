module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

/// for parsing packages, which may reference _any_ builtin
let all : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins
        BuiltinExecution.Libs.HttpClient.defaultConfig
      BuiltinCli.Builtin.builtins
      BuiltinCliHost.Builtin.builtins
      BuiltinCloudExecution.Builtin.builtins // TODO: do we need this?
      TestUtils.LibTest.builtins ] // TODO: or this?
    []

let accessibleByCanvas : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.builtins
        BuiltinExecution.Libs.HttpClient.defaultConfig
      BuiltinCloudExecution.Builtin.builtins
      //?BuiltinDarkInternal.Builtin.builtins
      ]
    []
