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
  | RT.DFakeVal (RT.DError (_, str)) -> RT.DFakeVal(RT.DError(RT.SourceNone, str))
  | RT.DFakeVal (RT.DIncomplete _) -> RT.DFakeVal(RT.DIncomplete(RT.SourceNone))
  | dv -> dv

let fns =
  lazy
    (LibExecution.StdLib.StdLib.fns @ LibBackend.StdLib.StdLib.fns @ LibTest.fns
     |> Map.fromListBy (fun fn -> fn.name))

let t (comment : string) (code : string) (dbs : List<RT.DB.T>) : Test =
  let name = $"{comment} ({code})"

  if code.StartsWith "//" then
    ptestTask name { return (Expect.equal "skipped" "skipped" "") }
  else
    testTask name {
      try
        let! owner = testOwner.Force()
        let ownerID : UserID = (owner : LibBackend.Account.UserInfo).id

        // Performance optimization: don't touch the DB if you don't use the DB
        let! canvasID =
          if List.length dbs > 0 then
            task {
              let hash = sha1digest name |> System.Convert.ToBase64String
              let canvasName = CanvasName.create $"test-{hash}"
              do! TestUtils.clearCanvasData canvasName

              let! canvasID =
                LibBackend.Canvas.canvasIDForCanvasName ownerID canvasName

              return canvasID
            }
          else
            task { return! testCanvasID.Force() }

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
         let mutable currentTestName = ""
         let mutable currentTestDBs = []
         let mutable currentTests = []
         let mutable singleTestMode = false
         let mutable currentTestString = "" // keep track of the current [test]
         let mutable allTests = ref []
         let mutable functions : Map<string, RT.UserFunction.T> = Map.empty
         let mutable dbs : Map<string, RT.DB.T> = Map.empty

         let finish () =
           // Add the current work to allTests, and clear
           let newTestCase =
             if singleTestMode then
               // Add a single test case
               t currentTestName currentTestString currentTestDBs
             else
               // Put currentTests in a group and add them
               testList currentTestName currentTests

           allTests := !allTests @ [ newTestCase ]

           // Clear settings
           currentTestName <- ""
           currentTestDBs <- []
           singleTestMode <- false
           currentTestString <- ""
           currentTests <- []

         (dir + filename)
         |> System.IO.File.ReadLines
         |> Seq.iteri
              (fun i line ->
                let i = i + 1

                match line with
                // [tests] indicator
                | Regex @"^\[tests\.(.*)\]$" [ name ] ->
                    finish ()
                    currentTestName <- name
                // [db] declaration
                | Regex @"^\[db.(.*) (\{.*\})\]\s*$" [ name; definition ] ->
                    let (db : RT.DB.T) =
                      { tlid = id i
                        name = name
                        version = 0
                        cols =
                          definition
                          |> Json.AutoSerialize.deserialize<Map<string, string>>
                          |> Map.map LibExecution.DvalRepr.dtypeOfString
                          |> Map.toList }

                    dbs <- Map.add name db dbs
                | Regex @"^\[fn.\s+(.*)\]$" [ name; definition ] ->
                    let (db : RT.DB.T) =
                      { tlid = id i
                        name = name
                        version = 0
                        cols =
                          definition
                          |> Json.AutoSerialize.deserialize<Map<string, string>>
                          |> Map.toList
                          |> List.map
                               (fun (k, v) ->
                                 (k, LibExecution.DvalRepr.dtypeOfString v)) }

                    dbs <- Map.add name db dbs
                // [function] declaration
                // [test] with DB indicator
                | Regex @"^\[test\.(.*)\] with DB (.*)$" [ name; dbName ] ->
                    finish ()

                    match Map.get dbName dbs with
                    | Some db -> currentTestDBs <- [ db ]
                    | None -> failwith $"No DB named {dbName} found"

                    singleTestMode <- true
                    currentTestName <- name
                // [test] indicator (no DB)
                | Regex @"^\[test\.(.*)\]$" [ name ] ->
                    finish ()
                    singleTestMode <- true
                    currentTestName <- name
                // Append to the current test string
                | _ when singleTestMode ->
                    currentTestString <- currentTestString + line
                // Skip whitespace lines
                | Regex "^\s*$" [] -> ()
                // 1-line test
                | Regex "^(.*)\s*$" [ code ] ->
                    currentTests <- currentTests @ [ t $"line {i}" code [] ]
                // 1-line test w/ comment or commented out lines
                | Regex "^(.*)\s*//\s*(.*)$" [ code; comment ] ->
                    currentTests <-
                      currentTests @ [ t $"{comment} (line {i})" code [] ]
                | _ -> raise (System.Exception $"can't parse line {i}: {line}"))

         finish ()
         testList $"Tests from {filename}" !allTests)
  |> Array.toList
  |> testList "All files"

let fqFnName =
  testMany
    "FQFnName.ToString"
    (fun (name : RT.FQFnName.T) -> name.ToString())
    [ (RT.FQFnName.stdlibName "" "++" 0), "++_v0"
      (RT.FQFnName.stdlibName "" "!=" 0), "!=_v0"
      (RT.FQFnName.stdlibName "" "&&" 0), "&&_v0"
      (RT.FQFnName.stdlibName "" "toString" 0), "toString_v0"
      (RT.FQFnName.stdlibName "String" "append" 1), "String::append_v1" ]

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


let tests =
  lazy (testList "LibExecution" [ fqFnName; backendFqFnName; fileTests () ])
