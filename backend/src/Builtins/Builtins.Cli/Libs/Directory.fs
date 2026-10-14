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
module Interpreter = LibExecution.Interpreter
open Builtin.Shortcuts


let fns () : List<BuiltInFn> =
  [ { name = fn "runtimeExecutablePath" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the full path to the currently running executable"
      fn =
        (function
        | _, vm, _, [| DUnit |] ->
          Interpreter.requestHost
            vm
            Host.Operation.CurrentExecutablePath
            (fun outcome ->
              match outcome with
              | Ok response -> Ply(DString(Host.expectPath response))
              | Error failure ->
                Exception.raiseInternal
                  "current executable path failed"
                  [ "message", failure.message ])
        | _ -> incorrectArgs ())
      previewable = Impure
      // The answer is a path, so it is a read of that path.
      callEffects = set [ Effect.FileRead ]
      deprecated = NotDeprecated } ]


let builtins () : Builtins = Builtin.make [] (fns ())
