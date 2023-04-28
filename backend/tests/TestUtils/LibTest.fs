module TestUtils.LibTest

// Functions which are not part of the Dark standard library, but which are
// useful for testing

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

open LibBackend.Db


let varA = TVariable "a"
let varB = TVariable "b"

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =

  [ { name = fn "Test" "typeError" 0
      typeParams = []
      parameters = [ Param.make "errorString" TString "" ]
      returnType = TInt
      description = "Return a value representing a type error"
      fn =
        (function
        | _, _, [ DString errorString ] -> Ply(DError(SourceNone, errorString))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Test" "incomplete" 0
      typeParams = []
      parameters = []
      returnType = TVariable "a"
      description = "Return a DIncomplet"
      fn =
        (function
        | _, _, [] -> Ply(DIncomplete(SourceNone))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "sqlError" 0
      typeParams = []
      parameters = [ Param.make "errorString" TString "" ]
      returnType = TInt
      description = "Return a value that matches errors thrown by the SqlCompiler"
      fn =
        (function
        | _, _, [ DString errorString ] ->
          let msg = LibBackend.SqlCompiler.errorTemplate + errorString
          Ply(DError(SourceNone, msg))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "nan" 0
      typeParams = []
      parameters = []
      returnType = TFloat
      description = "Return a NaN"
      fn =
        (function
        | _, _, [] -> Ply(DFloat(System.Double.NaN))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "infinity" 0
      typeParams = []
      parameters = []
      returnType = TFloat
      description = "Returns positive infitity"
      fn =
        (function
        | _, _, [] -> Ply(DFloat(System.Double.PositiveInfinity))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "toChar" 0
      typeParams = []
      parameters = [ Param.make "c" TString "" ]
      returnType = TOption TChar
      description = "Turns a string of length 1 into a character"
      fn =
        (function
        | _, _, [ DString s ] ->
          let chars = String.toEgcSeq s

          if Seq.length chars = 1 then
            chars |> Seq.toList |> (fun l -> l[0] |> DChar |> Some |> DOption |> Ply)
          else
            Ply(DOption None)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "negativeInfinity" 0
      typeParams = []
      parameters = []
      returnType = TFloat
      description = "Returns negative infinity"
      fn =
        (function
        | _, _, [] -> Ply(DFloat(System.Double.NegativeInfinity))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "incrementSideEffectCounter" 0
      typeParams = []
      parameters =
        [ Param.make "passThru" (TVariable "a") "Ply which will be returned" ]
      returnType = TVariable "a"
      description =
        "Increases the side effect counter by one, to test real-world side-effects. Returns its argument."
      fn =
        (function
        | state, _, [ arg ] ->
          state.test.sideEffectCount <- state.test.sideEffectCount + 1
          Ply(arg)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "sideEffectCount" 0
      typeParams = []
      parameters = []
      returnType = TInt
      description = "Return the value of the side-effect counter"
      fn =
        (function
        | state, _, [] -> Ply(Dval.int state.test.sideEffectCount)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "inspect" 0
      typeParams = []
      parameters = [ Param.make "var" varA ""; Param.make "msg" TString "" ]
      returnType = varA
      description = "Prints the value into stdout"
      fn =
        (function
        | _, _, [ v; DString msg ] ->
          print $"{msg}: {v}"
          Ply v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "justWithTypeError" 0
      typeParams = []
      parameters = [ Param.make "msg" TString "" ]
      returnType = TOption varA
      description = "Returns a DError in a Just"
      fn =
        (function
        | _, _, [ DString msg ] -> Ply(DOption(Some(DError(SourceNone, msg))))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "okWithTypeError" 0
      typeParams = []
      parameters = [ Param.make "msg" TString "" ]
      returnType = TResult(varA, varB)
      description = "Returns a DError in an OK"
      fn =
        (function
        | _, _, [ DString msg ] -> Ply(DResult(Ok(DError(SourceNone, msg))))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "errorWithTypeError" 0
      typeParams = []
      parameters = [ Param.make "msg" TString "" ]
      returnType = TResult(varA, varB)
      description = "Returns a DError in a Result.Error"
      fn =
        (function
        | _, _, [ DString msg ] -> Ply(DResult(Ok(DError(SourceNone, msg))))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "deleteUser" 0
      typeParams = []
      parameters = [ Param.make "username" TString "" ]
      returnType = TResult(TUnit, varB)
      description = "Delete a user (test only)"
      fn =
        (function
        | _, _, [ DString username ] ->
          uply {
            do!
              // This is unsafe. A user has canvases, and canvases have traces. It
              // will either break or cascade (haven't checked)
              Sql.query "DELETE FROM accounts_v0 WHERE username = @username"
              |> Sql.parameters [ "username", Sql.string (string username) ]
              |> Sql.executeStatementAsync
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "getQueue" 0
      typeParams = []
      parameters = [ Param.make "eventName" TString "" ]
      returnType = TList TString
      description = "Fetch a queue (test only)"
      fn =
        (function
        | state, _, [ DString eventName ] ->
          uply {
            let canvasID = state.program.canvasID
            let! results =
              LibBackend.Queue.Test.loadEvents canvasID ("WORKER", eventName, "_")
            let results =
              results
              |> List.map (fun x -> DString(LibExecution.DvalReprDeveloper.toRepr x))
            return DList results
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Test" "asBytes" 0
      typeParams = []
      parameters = [ Param.make "list" (TList TInt) "" ]
      returnType = TBytes
      description = "Turns a list of ints into bytes"
      fn =
        (function
        | _, _, [ DList l ] ->
          l
          |> List.map (fun x ->
            match x with
            | DInt x -> byte x
            | _ -> incorrectArgs ())
          |> Array.ofList
          |> DBytes
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "raiseException" 0
      typeParams = []
      parameters = [ Param.make "message" TString "" ]
      returnType = TVariable "a"
      description = "A function that raises an F# exception"
      fn =
        (function
        | _, _, [ DString message ] -> raise (System.Exception(message))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "intArrayToBytes" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TInt) "" ]
      returnType = TBytes
      description = "Create a bytes structure from an array of ints"
      fn =
        (function
        | _, _, [ DList bytes ] ->
          bytes
          |> List.toArray
          |> Array.map (fun dval ->
            match dval with
            | DInt i -> byte i
            | other -> Exception.raiseCode "Expected int" [ "actual", other ])
          |> DBytes
          |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "regexReplace" 0
      typeParams = []
      parameters =
        [ Param.make "subject" TString ""
          Param.make "pattern" TString ""
          Param.make "replacement" TString "" ]
      returnType = TString
      description = "Replaces regex patterns in a string"
      fn =
        (function
        | _, _, [ DString str; DString pattern; DString replacement ] ->
          FsRegEx.replace pattern replacement str |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "getCanvasID" 0
      typeParams = []
      parameters = []
      returnType = TUuid
      description = "Get the name of the canvas that's running"
      fn =
        (function
        | state, _, [] -> state.program.canvasID |> DUuid |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TOption(TVariable "a")) "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or a DError if Nothing"
      fn =
        (function
        | _, _, [ DOption opt ] ->
          uply {
            match opt with
            | Some value -> return value
            | None -> return (DError(SourceNone, "Nothing"))
          }
        | _, _, [ DResult res ] ->
          uply {
            match res with
            | Ok value -> return value
            | Error e ->
              return
                (DError(
                  SourceNone,
                  ("Error: " + LibExecution.DvalReprDeveloper.toRepr e)
                ))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Test" "setExpectedExceptionCount" 0
      typeParams = []
      parameters = [ Param.make "count" TInt "" ]
      returnType = TUnit
      description = "Set the expected exception count for the current test"
      fn =
        (function
        | state, _, [ DInt count ] ->
          uply {
            state.test.expectedExceptionCount <- int count
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
