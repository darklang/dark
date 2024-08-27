module TestUtils.LibTest

// Functions which are not part of the Dark standard library, but which are
// useful for testing

open System.Threading.Tasks
open FSharp.Control.Tasks

// open Npgsql.FSharp
// open Npgsql

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module PT = LibExecution.ProgramTypes
module Dval = LibExecution.Dval
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PackageIDs = LibExecution.PackageIDs

//open LibCloud.Db


// let varA = TVariable "a"
//let varB = TVariable "b"


let constants : List<BuiltInConstant> =
  [ { name = constant "testNan" 0
      typ = TFloat
      description = "Return a NaN"
      body = DFloat(System.Double.NaN)
      deprecated = NotDeprecated }

    { name = constant "testInfinity" 0
      typ = TFloat
      description = "Returns positive infitity"
      body = DFloat(System.Double.PositiveInfinity)
      deprecated = NotDeprecated }

    { name = constant "testNegativeInfinity" 0
      typ = TFloat
      description = "Returns negative infinity"
      body = DFloat(System.Double.NegativeInfinity)
      deprecated = NotDeprecated } ]

let fns : List<BuiltInFn> =
  [
  // { name = fn "testDerrorMessage" 0
  //   typeParams = []
  //   parameters = [ Param.make "errorMessage" TString "" ]
  //   returnType =
  //     TCustomType(
  //       Ok(
  //         FQTypeName.Package
  //           PackageIDs.Type.LanguageTools.RuntimeError.Error.errorMessage
  //       ),
  //       []
  //     )
  //   description = "Return a value representing a runtime type error"
  //   fn =
  //     (function
  //     | _, _, [ DString error ] ->
  //       let typeName =
  //         FQTypeName.Package
  //           PackageIDs.Type.LanguageTools.RuntimeError.Error.errorMessage
  //       DEnum(typeName, typeName, [], "ErrorString", [ DString error ]) |> Ply
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }

  // // CLEANUP consider renaming to `oldError` or something more clear
  // { name = fn "testRuntimeError" 0
  //   typeParams = []
  //   parameters = [ Param.make "errorString" TString "" ]
  //   returnType = TInt64
  //   description = "Return a value representing a type error"
  //   fn =
  //     (function
  //     | _, _, [ DString errorString ] ->
  //       raiseUntargetedRTE (RuntimeError.oldError errorString)
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }

  // { name = fn "testDerrorSqlMessage" 0
  //   typeParams = []
  //   parameters = [ Param.make "errorString" TString "" ]
  //   returnType =
  //     TCustomType(
  //       Ok(
  //         FQTypeName.Package
  //           PackageIDs.Type.LanguageTools.RuntimeError.Error.errorMessage
  //       ),
  //       []
  //     )
  //   description = "Return a value that matches errors thrown by the SqlCompiler"
  //   fn =
  //     (function
  //     | _, _, [ DString errorString ] ->
  //       let msg = LibCloud.SqlCompiler.errorTemplate + errorString
  //       let typeName =
  //         FQTypeName.Package
  //           PackageIDs.Type.LanguageTools.RuntimeError.Error.errorMessage
  //       DEnum(typeName, typeName, [], "ErrorString", [ DString msg ]) |> Ply
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }

  // { name = fn "testToChar" 0
  //   typeParams = []
  //   parameters = [ Param.make "c" TString "" ]
  //   returnType = TypeReference.option TChar
  //   description = "Turns a string of length 1 into a character"
  //   fn =
  //     (function
  //     | _, _, [ DString s ] ->
  //       let chars = String.toEgcSeq s

  //       if Seq.length chars = 1 then
  //         chars
  //         |> Seq.toList
  //         |> (fun l -> l[0])
  //         |> DChar
  //         |> Dval.optionSome KTChar
  //         |> Ply
  //       else
  //         Dval.optionNone KTChar |> Ply
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }


  // { name = fn "testIncrementSideEffectCounter" 0
  //   typeParams = []
  //   parameters =
  //     [ Param.make "passThru" (TVariable "a") "Ply which will be returned" ]
  //   returnType = TVariable "a"
  //   description =
  //     "Increases the side effect counter by one, to test real-world side-effects. Returns its argument."
  //   fn =
  //     (function
  //     | state, _, [ arg ] ->
  //       state.test.sideEffectCount <- state.test.sideEffectCount + 1
  //       Ply(arg)
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }


  // { name = fn "testSideEffectCount" 0
  //   typeParams = []
  //   parameters = [ Param.make "unit" TUnit "" ]
  //   returnType = TInt64
  //   description = "Return the value of the side-effect counter"
  //   fn =
  //     (function
  //     | state, _, [ DUnit ] -> Ply(Dval.int64 state.test.sideEffectCount)
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }


  // { name = fn "testInspect" 0
  //   typeParams = []
  //   parameters = [ Param.make "var" varA ""; Param.make "msg" TString "" ]
  //   returnType = varA
  //   description = "Prints the value into stdout"
  //   fn =
  //     (function
  //     | _, _, [ v; DString msg ] ->
  //       print $"{msg}: {v}"
  //       Ply v
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }


  // { name = fn "testDeleteUser" 0
  //   typeParams = []
  //   parameters = [ Param.make "username" TString "" ]
  //   returnType = TypeReference.result TUnit varB
  //   description = "Delete a user (test only)"
  //   fn =
  //     (function
  //     | _, _, [ DString username ] ->
  //       uply {
  //         do!
  //           // This is unsafe. A user has canvases, and canvases have traces. It
  //           // will either break or cascade (haven't checked)
  //           Sql.query "DELETE FROM accounts_v0 WHERE username = @username"
  //           |> Sql.parameters [ "username", Sql.string (string username) ]
  //           |> Sql.executeStatementAsync
  //         return DUnit
  //       }
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }


  // { name = fn "testGetQueue" 0
  //   typeParams = []
  //   parameters = [ Param.make "eventName" TString "" ]
  //   returnType = TList TString
  //   description = "Fetch a queue (test only)"
  //   fn =
  //     (function
  //     | state, _, [ DString eventName ] ->
  //       uply {
  //         let canvasID = state.program.canvasID
  //         let! results =
  //           LibCloud.Queue.Test.loadEvents canvasID ("WORKER", eventName, "_")
  //         let results =
  //           results
  //           |> List.map LibExecution.DvalReprDeveloper.toRepr
  //           |> List.map DString
  //         return DList(VT.string, results)
  //       }
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Impure
  //   deprecated = NotDeprecated }


  // { name = fn "testRaiseException" 0
  //   typeParams = []
  //   parameters = [ Param.make "message" TString "" ]
  //   returnType = TVariable "a"
  //   description = "A function that raises an F# exception"
  //   fn =
  //     (function
  //     | _, _, [ DString message ] -> raise (System.Exception(message))
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }


  // { name = fn "testGetCanvasID" 0
  //   typeParams = []
  //   parameters = [ Param.make "unit" TUnit "" ]
  //   returnType = TUuid
  //   description = "Get the name of the canvas that's running"
  //   fn =
  //     (function
  //     | state, _, [ DUnit ] -> state.program.canvasID |> DUuid |> Ply
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }


  // { name = fn "testSetExpectedExceptionCount" 0
  //   typeParams = []
  //   parameters = [ Param.make "count" TInt64 "" ]
  //   returnType = TUnit
  //   description = "Set the expected exception count for the current test"
  //   fn =
  //     (function
  //     | state, _, [ DInt64 count ] ->
  //       uply {
  //         state.test.expectedExceptionCount <- int count
  //         return DUnit
  //       }
  //     | _ -> incorrectArgs ())
  //   sqlSpec = NotQueryable
  //   previewable = Pure
  //   deprecated = NotDeprecated }
  ]

let builtins = LibExecution.Builtin.make constants fns
