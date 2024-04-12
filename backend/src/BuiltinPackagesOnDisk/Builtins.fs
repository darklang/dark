module BuiltinPackagesOnDisk.Builtins

open Prelude

module RT = LibExecution.RuntimeTypes

/// for parsing packages, which may reference _any_ builtin
let all : RT.Builtins =
  LibExecution.Builtin.combine
    [ BuiltinExecution.Builtin.contents
        BuiltinExecution.Libs.HttpClient.defaultConfig
      BuiltinCli.Builtin.contents
      BuiltinCloudExecution.Builtin.contents // TODO: do we need this?
      TestUtils.LibTest.contents ] // TODO: or this?
    []
