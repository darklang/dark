/// Standard libraries for Directories
module Builtins.Cli.Libs.Directory

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Host = LibExecution.Host
module PermissionCheck = LibExecution.PermissionCheck
open Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "directoryCurrent" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TString
      description = "Returns the current working directory"
      fn =
        (function
        | state, vm, _, [| DUnit |] ->
          uply {
            let op = Host.Operation.DirectoryCurrent
            match! PermissionCheck.performHost state vm op with
            | Ok response -> return DString(Host.expectPath response)
            | Error failure ->
              return
                Exception.raiseInternal
                  "reading the working directory failed"
                  [ "message", failure.message ]
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "directoryList" 0
      typeParams = []
      parameters = [ Param.make "path" TString "" ]
      returnType = TList TString
      description = "Returns the directory at <param path>"
      fn =
        (function
        | state, vm, _, [| DString path |] ->
          uply {
            let op = Host.Operation.DirectoryList(Host.expandHome path)
            match! PermissionCheck.performHost state vm op with
            | Ok response ->
              let entries = Host.expectEntries response
              return DList(VT.string, List.map DString entries)
            | Error _ -> return DList(VT.string, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated }


    { name = fn "getCurrentExecutablePath" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the full path to the currently running executable"
      fn =
        (function
        | state, vm, _, [| DUnit |] ->
          uply {
            match!
              PermissionCheck.performHost
                state
                vm
                Host.Operation.CurrentExecutablePath
            with
            | Ok response -> return DString(Host.expectPath response)
            | Error failure ->
              return
                Exception.raiseInternal
                  "current executable path failed"
                  [ "message", failure.message ]
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      // The answer is a path, so it is a read of that path.
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
