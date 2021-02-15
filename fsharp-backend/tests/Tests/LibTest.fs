module Tests.LibTest

// Functions which are not part of the Dark standard library, but which are
// useful for testing

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

let fn = FQFnName.stdlibName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

// FSTODO: this is going cause race conditions - we should move this into state
let sideEffectCount : int ref = ref 0

let fns : List<BuiltInFn> =
  [ { name = fn "Test" "errorRailNothing" 0
      parameters = []
      returnType = TOption varA
      description = "Return an errorRail wrapping nothing."
      fn =
        (function
        | state, [] -> Value(DFakeVal(DErrorRail(DOption None)))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "typeError" 0
      parameters = [ Param.make "errorString" TStr "" ]
      returnType = TInt
      description = "Return a value representing a type error"
      fn =
        (function
        | state, [ DStr errorString ] ->
            Value(DFakeVal(DError(SourceNone, errorString)))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "incrementSideEffectCounter" 0
      parameters =
        [ Param.make "passThru" (TVariable "a") "Value which will be returned" ]
      returnType = TVariable "a"
      description =
        "Increases the side effect counter by one, to test real-world side-effects. Returns its argument."
      fn =
        (function
        | state, [ arg ] ->
            sideEffectCount := !sideEffectCount + 1
            Value(arg)
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "sideEffectCount" 0
      parameters = []
      returnType = TInt
      description = "Return the value of the side-effect counter"
      fn =
        (function
        | state, [] -> Value(Dval.int !sideEffectCount)
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
