/// Library functions for emitting events
///
/// Those events are handled by Workers
module BackendOnlyStdLib.LibEvent

open LibExecution.RuntimeTypes
open Prelude

module EventQueue = LibBackend.EventQueue
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"

let fns : List<BuiltInFn> =
  [ { name = fn "" "emit" 0
      parameters =
        // CLEANUP lowercase names
        [ Param.make "Data" varA ""
          Param.make "Space" TStr ""
          Param.make "Name" TStr "" ]
      returnType = varA
      description =
        "Emit event `name` in `space`, passing along `data` as a parameter"
      fn =
        (function
        | state, [ data; DStr space; DStr name ] ->
          uply {
            // See client/src/Entry.ml for the "_"
            let canvasID = state.program.canvasID
            let canvasName = state.program.canvasName
            let accountID = state.program.accountID
            do! EventQueue.enqueue canvasName canvasID accountID space name "_" data
            return data
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "" "emit" 1) }

    { name = fn "" "emit" 1
      parameters = [ Param.make "event" varA ""; Param.make "Name" TStr "" ]
      returnType = varA
      description = "Emit a `event` to the `name` worker"
      fn =
        (function
        | state, [ data; DStr name ] ->
          uply {
            let canvasID = state.program.canvasID
            let canvasName = state.program.canvasName
            let accountID = state.program.accountID

            do!
              // WHATISTHIS See client/src/Entry.ml for context about the "_"
              EventQueue.enqueue canvasName canvasID accountID "WORKER" name "_" data

            return data
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
