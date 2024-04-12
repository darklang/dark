module LocalExec.Builtins

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes

/// for parsing packages, which may reference _any_ builtin
let all : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.contents
        BuiltinExecution.Libs.HttpClient.defaultConfig
      BuiltinCli.Builtin.contents
      BuiltinCliHost.Builtin.contents
      BuiltinCloudExecution.Builtin.contents // TODO: do we need this?
      BuiltinPackagesOnDisk.Builtin.contents
      TestUtils.LibTest.contents ] // TODO: or this?
    []

let accessibleByCanvas : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.contents
        BuiltinExecution.Libs.HttpClient.defaultConfig
      BuiltinCloudExecution.Builtin.contents
      //?BuiltinDarkInternal.Builtin.contents
      ]
    []
