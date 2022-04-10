module TestUtils.LibTest

// Functions which are not part of the Dark standard library, but which are
// useful for testing

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open LibExecution.RuntimeTypes
open Prelude

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "Test" "errorRailValue" 0
      parameters = [ Param.make "value" varA "" ]
      returnType = varA
      description = "Return an errorRail wrapping a value."
      fn =
        (function
        | state, [ value ] -> Ply(DErrorRail(value))
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
        | state, [ DStr errorString ] -> Ply(DError(SourceNone, errorString))
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
        | _, [ DStr errorString ] ->
          let msg = LibBackend.SqlCompiler.errorTemplate + errorString
          Ply(DError(SourceNone, msg))
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
        | _, [] -> Ply(DFloat(System.Double.NaN))
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
        | _, [] -> Ply(DFloat(System.Double.PositiveInfinity))
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
            chars |> Seq.toList |> fun l -> l[0] |> DChar |> Some |> DOption |> Ply
          else
            Ply(DOption None)
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
        | _, [] -> Ply(DFloat(System.Double.NegativeInfinity))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "resetSideEffectCounter" 0
      parameters = [ Param.make "counterName" TStr "Name of the counter (unused)" ]
      returnType = TNull
      description =
        "Reset the side effect counter to zero, to test real-world side-effects."
      fn =
        (function
        | state, [ _; arg ] ->
          // CLEANUP this function is no longer needed once we remove ocaml
          state.test.sideEffectCount <- 0
          Ply(arg)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "incrementSideEffectCounter" 0
      parameters =
        // CLEANUP This parameter is only needed for OCaml, which doesn't have test
        // state and uses a global instead
        [ Param.make "counterName" TStr "Name of the counter (unused)"
          Param.make "passThru" (TVariable "a") "Ply which will be returned" ]
      returnType = TVariable "a"
      description =
        "Increases the side effect counter by one, to test real-world side-effects. Returns its argument."
      fn =
        (function
        | state, [ _; arg ] ->
          state.test.sideEffectCount <- state.test.sideEffectCount + 1
          Ply(arg)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "sideEffectCount" 0
      parameters = [ Param.make "counterName" (TStr) "Name of the counter (unused)" ]
      returnType = TInt
      description = "Return the value of the side-effect counter"
      fn =
        (function
        | state, [ _ ] -> Ply(Dval.int state.test.sideEffectCount)
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
          print $"{msg}: {v}"
          Ply v
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
        | _, [ DStr msg ] -> Ply(DOption(Some(DError(SourceNone, msg))))
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
        | _, [ DStr msg ] -> Ply(DResult(Ok(DError(SourceNone, msg))))
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
        | _, [ DStr msg ] -> Ply(DResult(Ok(DError(SourceNone, msg))))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "deleteUser" 0
      parameters = [ Param.make "username" TStr "" ]
      returnType = TResult(TNull, varB)
      description = "Delete a user (test only)"
      fn =
        (function
        | _, [ DStr username ] ->
          uply {
            do!
              // This is unsafe. A user has canvases, and canvases have traces. It
              // will either break or cascade (haven't checked)
              Sql.query "DELETE from ACCOUNTS WHERE username = @username"
              |> Sql.parameters [ "username", Sql.string (string username) ]
              |> Sql.executeStatementAsync
            return DNull
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Test" "getQueue" 0
      parameters = [ Param.make "eventName" TStr "" ]
      returnType = TList TStr
      description = "Fetch a queue (test only)"
      fn =
        (function
        | state, [ DStr eventName ] ->
          uply {
            let canvasID = state.program.canvasID
            let! results = LibBackend.EventQueue.testingGetQueue canvasID eventName
            return DList results
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "Test" "raiseException" 0
      parameters = [ Param.make "message" TStr "" ]
      returnType = TVariable "a"
      description = "A function that raises an F# exception"
      fn =
        (function
        | _, [ DStr message ] -> raise (System.Exception(message))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "Test" "intArrayToBytes" 0
      parameters = [ Param.make "bytes" (TList TInt) "" ]
      returnType = TBytes
      description = "Create a bytes structure from an array of ints"
      fn =
        (function
        | _, [ DList bytes ] ->
          bytes
          |> List.toArray
          |> Array.map (function
            | DInt i -> byte i
            | other -> Exception.raiseCode "Expected int" [ "actual", other ])
          |> DBytes
          |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "Test" "regexReplace" 0
      parameters =
        [ Param.make "subject" TStr ""
          Param.make "pattern" TStr ""
          Param.make "replacement" TStr "" ]
      returnType = TStr
      description = "Replaces regex patterns in a string"
      fn =
        (function
        | _, [ DStr str; DStr pattern; DStr replacement ] ->
          FsRegEx.replace pattern replacement str |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    ]
