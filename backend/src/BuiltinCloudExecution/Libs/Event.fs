/// Builtin functions for emitting events
///
/// Those events are handled by Workers
module BuiltinCloudExecution.Libs.Event

open Prelude
open LibExecution.RuntimeTypes

module Queue = LibCloud.Queue

open LibExecution.Builtin.Shortcuts

let varA = TVariable "a"

let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn [] "emit" 0
      typeParams = []
      parameters = [ Param.make "event" varA ""; Param.make "name" TString "" ]
      returnType = varA
      description = "Emit a <param event> to the <param name> worker"
      fn =
        (function
        | state, _, [ data; DString name ] ->
          uply {
            let canvasID = state.program.canvasID

            do!
              // the "_" exists because handlers in the DB have 3 fields (eg Http, /path, GET),
              // but we don't need a 3rd one for workers
              Queue.enqueueNow canvasID "WORKER" name "_" data

            return data
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, constants)
