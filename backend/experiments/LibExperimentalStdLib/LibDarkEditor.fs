module LibExperimentalStdLib.LibDarkEditor

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "DarkEditor" "parseAndExecuteExpr" 0
      typeParams = []
      parameters =
        [ Param.make "code" TString ""; Param.make "userInputs" (TDict TString) "" ]
      returnType = TResult(TString, TString)
      description =
        "Parses and executes arbitrary Dark code in the context of the current canvas."
      fn =
        // I had these tests in internal.tests for a bit, but that test runner
        // no longer has 'access' to the fn, so removed them. If we move them
        // out of 'LibExperimentalStdLib' then we can add them back in.

        //module ParseAndExecuteExpr =
        //  DarkEditor.parseAndExecuteExpr "1 + 2" Dict.empty_v0 = Ok "3"
        //  DarkEditor.parseAndExecuteExpr "a" { a = 3 } = Ok "3"
        //  DarkEditor.parseAndExecuteExpr """let a = 3 in a + b""" { b = 2 } = Ok "5"
        //  //DarkEditor.parseAndExecuteExpr """(HttpClient.request "get" "https://example.com" [] Bytes.empty) |> Test.unwrap |> (fun response -> response.statusCode)""" Dict.empty = 200

        function
        | state, _, [ DString code; DDict userInputs ] ->
          uply {
            // TODO: return an appropriate error if this fails
            let expr = Parser.RuntimeTypes.parseExprWithTypes Map.empty code

            let symtable = LibExecution.Interpreter.withGlobals state userInputs

            // TODO: return an appropriate error if this fails
            let! evalResult = LibExecution.Interpreter.eval state symtable expr

            return
              LibExecution.DvalReprDeveloper.toRepr evalResult
              |> DString
              |> Ok
              |> DResult
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "DarkEditor" "parseAndSerializeExpr" 0
      typeParams = []
      parameters = [ Param.make "code" TString "" ]
      returnType = TResult(TString, TString)
      description = "Parses Dark code and serializes the result to JSON."
      fn =
        function
        | _, _, [ DString code ] ->
          uply {
            let expr = Parser.RuntimeTypes.parseExprWithTypes Map.empty code
            let serializedExpr = Json.Vanilla.serialize expr
            return serializedExpr |> DString |> Ok |> DResult
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
