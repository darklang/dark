/// Builtin functions for emitting events
///
/// Note: Workers/QueueWorker have been removed. emit is now a no-op that returns the event.
module BuiltinCloudExecution.Libs.Event

open Prelude
open LibExecution.RuntimeTypes

open LibExecution.Builtin.Shortcuts

let tvar v = TVariable v

let fns : List<BuiltInFn> =
  [ { name = fn "emit" 0
      typeParams = []
      parameters = [ Param.make "event" (tvar "a") ""; Param.make "name" TString "" ]
      returnType = tvar "a"
      description = "Emit a <param event> to the <param name> worker (no-op, workers removed)"
      fn =
        (function
        | _, _, _, [ data; DString _ ] ->
          // Workers have been removed, so emit is now a no-op
          Ply data
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
