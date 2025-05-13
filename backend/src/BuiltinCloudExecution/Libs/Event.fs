/// Builtin functions for emitting events
///
/// Those events are handled by Workers
module BuiltinCloudExecution.Libs.Event

open Prelude
open LibExecution.RuntimeTypes

module Queue = LibCloud.Queue

open LibExecution.Builtin.Shortcuts

let tvar v = TVariable v

let fns : List<BuiltInFn> =
  [ { name = fn "emit" 0
      typeParams = []
      parameters = [ Param.make "event" (tvar "a") ""; Param.make "name" TString "" ]
      returnType = tvar "a"
      description = "Emit a <param event> to the <param name> worker"
      fn =
        (function
        | exeState, _, _, [ data; DString name ] ->
          uply {
            let canvasID = exeState.program.canvasID

            do!
              // Handlers in our Sqlite DB are all stored in the same table.
              // Typically they have 3 fields (e.g. `Http`, `/path`, `GET`).
              // The "_" exists here to fit that shape, even though, workers don't need it.
              Queue.enqueueNow canvasID "WORKER" name "_" data

            return data
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
