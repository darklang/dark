// Functions which are not part of the Dark standard library, but which are
// useful for testing

module Backend.Tests.LibExecution.LibTest

open System.Threading.Tasks
open FSharp.Control.Tasks
open LibExecution.Runtime
open FSharpPlus
open Prelude

let fn = FnDesc.stdFnDesc

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "Test" "errorRailNothing" 0
      parameters = [ Param.make "value" varA "" ]
      returnType = TOption varA
      description = "Return an errorRail wrapping nothing."
      fn =
        (function
        | state, [ v ] -> Plain(DFakeVal(DErrorRail(DOption None)))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
