module LibTest

// Functions which are not part of the Dark standard library, but which are
// useful for testing

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "Test" "errorRailNothing" 0
      parameters = []
      returnType = TOption varA
      description = "Return an errorRail wrapping nothing."
      fn =
        (function
        | state, [] -> Value(DErrorRail(DOption None))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "typeError" 0
      parameters = [ Param.make "errorString" TStr "" ]
      returnType = TInt
      description = "Return a value representing a type error"
      fn =
        (function
        | state, [ DStr errorString ] -> Value(DError(SourceNone, errorString))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "sqlError" 0
      parameters = [ Param.make "errorString" TStr "" ]
      returnType = TInt
      description = "Return a value that matches errors thrown by the SqlCompiler"
      fn =
        (function
        | state, [ DStr errorString ] ->
          let msg = LibExecution.Errors.queryCompilerErrorTemplate ++ errorString
          Value(DError(SourceNone, msg))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "nan" 0
      parameters = []
      returnType = TFloat
      description = "Return a NaN"
      fn =
        (function
        | _, [] -> Value(DFloat(System.Double.NaN))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "infinity" 0
      parameters = []
      returnType = TFloat
      description = "Returns positive infitity"
      fn =
        (function
        | _, [] -> Value(DFloat(System.Double.PositiveInfinity))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "toChar" 0
      parameters = [ Param.make "c" TStr "" ]
      returnType = TOption TChar
      description = "Turns a string of length 1 into a character"
      fn =
        (function
        | _, [ DStr s ] ->
          let chars = String.toEgcSeq s

          if Seq.length chars = 1 then
            chars
            |> Seq.toList
            |> fun l -> l.[0] |> DChar |> Some |> DOption |> Value
          else
            Value(DOption None)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "negativeInfinity" 0
      parameters = []
      returnType = TFloat
      description = "Returns negative infinity"
      fn =
        (function
        | _, [] -> Value(DFloat(System.Double.NegativeInfinity))
        | _ -> incorrectArgs ())
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
          state.test.sideEffectCount <- state.test.sideEffectCount + 1
          Value(arg)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "sideEffectCount" 0
      parameters = []
      returnType = TInt
      description = "Return the value of the side-effect counter"
      fn =
        (function
        | state, [] -> Value(Dval.int state.test.sideEffectCount)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "inspect" 0
      parameters = [ Param.make "var" varA ""; Param.make "msg" TStr "" ]
      returnType = varA
      description = "Prints the value into stdout"
      fn =
        (function
        | state, [ v; DStr msg ] ->
          printfn $"{msg}: {v}"
          Value v
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "justWithTypeError" 0
      parameters = [ Param.make "msg" TStr "" ]
      returnType = TOption varA
      description = "Returns a DError in a Just"
      fn =
        (function
        | _, [ DStr msg ] -> Value(DOption(Some(DError(SourceNone, msg))))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "okWithTypeError" 0
      parameters = [ Param.make "msg" TStr "" ]
      returnType = TResult(varA, varB)
      description = "Returns a DError in an OK"
      fn =
        (function
        | _, [ DStr msg ] -> Value(DResult(Ok(DError(SourceNone, msg))))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "errorWithTypeError" 0
      parameters = [ Param.make "msg" TStr "" ]
      returnType = TResult(varA, varB)
      description = "Returns a DError in a Result.Error"
      fn =
        (function
        | _, [ DStr msg ] -> Value(DResult(Ok(DError(SourceNone, msg))))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
