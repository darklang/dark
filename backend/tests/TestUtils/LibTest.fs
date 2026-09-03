module TestUtils.LibTest

// Functions which are not part of the Dark standard library, but which are
// useful for testing

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes
module Dval = LibExecution.Dval
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PackageRefs = LibExecution.PackageRefs


let values : List<BuiltInValue> =
  [ { name = value "testNan" 0
      typ = TFloat
      description = "Return a NaN"
      body = DFloat(System.Double.NaN)
      deprecated = NotDeprecated }

    { name = value "testInfinity" 0
      typ = TFloat
      description = "Returns positive infitity"
      body = DFloat(System.Double.PositiveInfinity)
      deprecated = NotDeprecated }

    { name = value "testNegativeInfinity" 0
      typ = TFloat
      description = "Returns negative infinity"
      body = DFloat(System.Double.NegativeInfinity)
      deprecated = NotDeprecated } ]

let fns () : List<BuiltInFn> =
  [ { name = fn "testRuntimeError" 0
      typeParams = []
      parameters = [ Param.make "errorString" TString "" ]
      returnType = TInt64
      description = "Return a value representing a type error"
      fn =
        (function
        | _, _, _, [| DString errorString |] ->
          raiseUntargetedRTE (RuntimeError.UncaughtException(errorString, []))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "testToChar" 0
      typeParams = []
      parameters = [ Param.make "c" TString "" ]
      returnType = TypeReference.option TChar
      description = "Turns a string of length 1 into a character"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          let chars = String.toEgcSeq s

          if Seq.length chars = 1 then
            chars
            |> Seq.toList
            |> (fun l -> l[0])
            |> DChar
            |> Dval.optionSome KTChar
            |> Ply
          else
            Dval.optionNone KTChar |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "testIncrementSideEffectCounter" 0
      typeParams = []
      parameters =
        [ Param.make "passThru" (TVariable "a") "Ply which will be returned" ]
      returnType = TVariable "a"
      description =
        "Increases the side effect counter by one, to test real-world side-effects. Returns its argument."
      fn =
        (function
        | state, _, _, [| arg |] ->
          state.test.sideEffectCount <- state.test.sideEffectCount + 1
          Ply(arg)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "testSideEffectCount" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description = "Return the value of the side-effect counter"
      fn =
        (function
        | state, _, _, [| DUnit |] -> Ply(Dval.int64 state.test.sideEffectCount)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "testRaiseException" 0
      typeParams = []
      parameters = [ Param.make "message" TString "" ]
      returnType = TVariable "a"
      description = "A function that raises an F# exception"
      fn =
        (function
        | _, _, _, [| DString message |] -> raise (System.Exception message)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "testSetExpectedExceptionCount" 0
      typeParams = []
      parameters = [ Param.make "count" TInt64 "" ]
      returnType = TUnit
      description = "Set the expected exception count for the current test"
      fn =
        (function
        | state, _, _, [| DInt64 count |] ->
          uply {
            state.test.expectedExceptionCount <- int count
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make values (fns ())
