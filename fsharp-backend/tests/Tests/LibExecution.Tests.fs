module Tests.LibExecution

// Create test cases from .tests files in the tests/stdlib dir

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibBackend.ProgramSerialization.ProgramTypes
module Exe = LibExecution.Execution

open TestUtils

// Remove random things like IDs to make the tests stable
let normalizeDvalResult (dv : RT.Dval) : RT.Dval =
  match dv with
  | RT.DFakeVal (RT.DError (source, str)) ->
      RT.DFakeVal(RT.DError(RT.SourceNone, str))
  | RT.DFakeVal (RT.DIncomplete source) -> RT.DFakeVal(RT.DIncomplete(RT.SourceNone))
  | dv -> dv

open LibExecution.RuntimeTypes

let rec dvalEquals (left : Dval) (right : Dval) (msg : string) : unit =
  let de l r = dvalEquals l r msg

  match left, right with
  | DFloat l, DFloat r -> Expect.floatClose Accuracy.veryHigh l r msg
  | DResult (Ok l), DResult (Ok r) -> de l r
  | DResult (Error l), DResult (Error r) -> de l r
  | DOption (Some l), DOption (Some r) -> de l r
  | DList ls, DList rs -> List.iter2 de ls rs
  | DObj ls, DObj rs ->
      List.iter2
        (fun (k1, v1) (k2, v2) ->
          Expect.equal k1 k2 msg
          de v1 v2)
        (Map.toList ls)
        (Map.toList rs)
  | DHttpResponse (Response (sc1, h1), b1), DHttpResponse (Response (sc2, h2), b2) ->
      Expect.equal sc1 sc2 msg
      Expect.equal h1 h2 msg
      de b1 b2
  | DHttpResponse (Redirect u1, b1), DHttpResponse (Redirect u2, b2) ->
      Expect.equal u1 u2 msg
      de b1 b2
  | DFakeVal (DIncomplete _), DFakeVal (DIncomplete _) ->
      Expect.equal true true "two incompletes"
  // Keep for exhaustiveness checking
  | DHttpResponse _, _
  | DObj _, _
  | DList _, _
  | DResult _, _
  | DOption _, _
  // All others can be directly compared
  | DInt _, _
  | DDate _, _
  | DBool _, _
  | DFloat _, _
  | DNull, _
  | DStr _, _
  | DChar _, _
  | DFnVal _, _
  | DFakeVal _, _
  | DDB _, _
  | DUuid _, _
  | DBytes _, _ -> Expect.equal left right msg

let fns =
  lazy
    (LibExecution.StdLib.StdLib.fns
     @ LibBackend.StdLib.StdLib.fns @ Tests.LibTest.fns
     |> Map.fromListBy (fun fn -> fn.name))

type UserInfo = LibBackend.Account.UserInfo

let t (comment : string) (code : string) (dbInfo : Option<string * string>) : Test =
  let name = $"{comment} ({code})"

  if code.StartsWith "//" then
    ptestTask name { return (Expect.equal "skipped" "skipped" "") }
  else
    testTask name {
      try
        let! owner = testOwner.Force()
        let ownerID : UserID = (owner : UserInfo).id

        // Performance optimization: don't touch the DB if you don't use the DB
        let! canvasID =
          match dbInfo with
          | Some _ ->
              task {
                let hash = sha1digest name |> System.Convert.ToBase64String
                let canvasName = CanvasName.create $"test-{hash}"
                do! TestUtils.clearCanvasData canvasName

                let! canvasID =
                  LibBackend.Canvas.canvasIDForCanvasName ownerID canvasName

                return canvasID
              }
          | None -> task { return! testCanvasID.Force() }

        let (dbs : List<RT.DB.T>) =
          match dbInfo with
          | Some (name, json) ->
              let dict = Json.AutoSerialize.deserialize<Map<string, string>> json

              [ { tlid = id 8
                  name = name
                  version = 0
                  cols =
                    dict
                    |> Map.toList
                    |> List.map
                         (fun (k, v) -> (k, LibExecution.DvalRepr.dtypeOfString v)) } ]
          | None -> []

        let source = FSharpToExpr.parse code
        let actualProg, expectedResult = FSharpToExpr.convertToTest source
        let tlid = id 7

        let state = Exe.createState ownerID canvasID tlid (fns.Force()) dbs [] [] []

        let! actual = Exe.run state Map.empty actualProg
        let! expected = Exe.run state Map.empty expectedResult
        let actual = normalizeDvalResult actual
        //let str = $"{source} => {actualProg} = {expectedResult}"
        let astMsg = $"{actualProg} = {expectedResult} ->"
        let dataMsg = $"\n\nActual:\n{actual}\n = \n{expected}"
        let str = astMsg + dataMsg
        return (dvalEquals actual expected str)
      with e ->
        printfn "Exception thrown in test: %s" (e.ToString())
        return (Expect.equal "Exception thrown in test" (e.ToString()) "")
    }


// Read all test files. Test file format is as follows:
//
// Lines with just comments or whitespace are ignored
// Tests are made up of code and comments, comments are used as names
//
// Test indicators:
//   [tests.name] denotes that the following lines (until the next test
//   indicator) are single line tests, that are all part of the test group
//   named "name". Single line tests should evaluate to true, and may have a
//   comment at the end, which will be the test name
//
//   [test.name] indicates that the following lines, up until the next test
//   indicator, are all a single test named "name", and should be parsed as
//   one.
let fileTests () : Test =
  let dir = "tests/testfiles/"

  System.IO.Directory.GetFiles(dir, "*")
  |> Array.map
       (fun file ->
         let filename = System.IO.Path.GetFileName file
         let currentTestName = ref ""
         let currentTestDB = ref None
         let currentTests = ref []
         let singleTestMode = ref false
         let currentTestString = ref "" // keep track of the current [test]
         let allTests = ref []

         let finish () =
           // Add the current work to allTests, and clear
           let newTestCase =
             if !singleTestMode then
               // Add a single test case
               t !currentTestName !currentTestString !currentTestDB
             else
               // Put currentTests in a group and add them
               testList !currentTestName !currentTests

           allTests := !allTests @ [ newTestCase ]

           // Clear settings
           currentTestName := ""
           currentTestDB := None
           singleTestMode := false
           currentTestString := ""
           currentTests := []

         (dir + filename)
         |> System.IO.File.ReadLines
         |> Seq.iteri
              (fun i line ->
                let i = i + 1

                match line with
                // [tests] indicator
                | Regex "^\[tests\.(.*)\]$" [ name ] ->
                    finish ()
                    currentTestDB := None
                    currentTestName := name
                // [test] with DB indicator
                | Regex @"^\[test\.(.*)\](?: with DB (\w+) (.*))$"
                        [ name; dbName; dbJson ] ->
                    finish ()
                    singleTestMode := true
                    currentTestDB := Some(dbName, dbJson)
                    currentTestName := name
                // [test] indicator (no DB)
                | Regex @"^\[test\.(.*)\]$" [ name ] ->
                    finish ()
                    singleTestMode := true
                    currentTestDB := None
                    currentTestName := name
                // Append to the current test string
                | _ when !singleTestMode ->
                    currentTestString := !currentTestString + line
                // Skip whitespace lines
                | Regex "^\s*$" [] -> ()
                // 1-line test
                | Regex "^(.*)\s*$" [ code ] ->
                    currentTests := !currentTests @ [ t $"line {i}" code None ]
                // 1-line test w/ comment or commented out lines
                | Regex "^(.*)\s*//\s*(.*)$" [ code; comment ] ->
                    currentTests
                    := !currentTests @ [ t $"{comment} (line {i})" code None ]
                | _ -> raise (System.Exception $"can't parse line {i}: {line}"))

         finish ()
         testList $"Tests from {filename}" !allTests)
  |> Array.toList
  |> testList "All files"

let fqFnName =
  testMany
    "FQFnName.ToString"
    (fun (name : FQFnName.T) -> name.ToString())
    [ (FQFnName.stdlibName "" "++" 0), "++_v0"
      (FQFnName.stdlibName "" "!=" 0), "!=_v0"
      (FQFnName.stdlibName "" "&&" 0), "&&_v0"
      (FQFnName.stdlibName "" "toString" 0), "toString_v0"
      (FQFnName.stdlibName "String" "append" 1), "String::append_v1" ]

// TODO parsing function names from OCaml

let backendFqFnName =
  testMany
    "ProgramTypes.FQFnName.ToString"
    (fun (name : PT.FQFnName.T) -> name.ToString())
    [ (PT.FQFnName.stdlibName "" "++" 0), "++_v0"
      (PT.FQFnName.stdlibName "" "!=" 0), "!=_v0"
      (PT.FQFnName.stdlibName "" "&&" 0), "&&_v0"
      (PT.FQFnName.stdlibName "" "toString" 0), "toString_v0"
      (PT.FQFnName.stdlibName "String" "append" 1), "String::append_v1" ]


let tests = testList "LibExecution" [ fqFnName; backendFqFnName; fileTests () ]
