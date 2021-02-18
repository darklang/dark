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

let t
  (comment : string)
  (code : string)
  (dbs : List<RT.DB.T>)
  (functions : Map<string, RT.UserFunction.T>)
  : Test =
  let name = $"{comment} ({code})"

  if matches "^\s*//" code then
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

        let state =
          Exe.createState
            ownerID
            canvasID
            tlid
            (fns.Force())
            Map.empty
            (dbs |> List.map (fun db -> db.name, db) |> Map.ofList)
            functions
            Map.empty
            []

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


type TestInfo =
  { name : string
    recording : bool
    code : string
    dbs : List<RT.DB.T> }

type TestGroup = { name : string; tests : List<Test>; dbs : List<RT.DB.T> }

type FnInfo =
  { name : string
    recording : bool
    code : string
    tlid : tlid
    parameters : List<RT.UserFunction.Parameter> }

// Read all test files. The test file format is described in README.md
let fileTests () : Test =
  let dir = "tests/testfiles/"

  System.IO.Directory.GetFiles(dir, "*")
  |> Array.map
       (fun file ->
         let filename = System.IO.Path.GetFileName file
         let emptyTest = { recording = false; name = ""; dbs = []; code = "" }

         let emptyFn =
           { recording = false; name = ""; parameters = []; code = ""; tlid = id 0 }

         let emptyGroup = { name = ""; tests = []; dbs = [] }

         // for recording a multiline test
         let mutable currentTest = emptyTest
         // for recording a multiline function
         let mutable currentFn = emptyFn
         // for recording a bunch if single-line tests grouped together
         let mutable currentGroup = emptyGroup
         let mutable allTests = []
         let mutable functions : Map<string, RT.UserFunction.T> = Map.empty
         let mutable dbs : Map<string, RT.DB.T> = Map.empty

         let finish () =
           if currentTest.recording then
             let newTestCase =
               t currentTest.name currentTest.code currentTest.dbs functions

             allTests <- allTests @ [ newTestCase ]

           if List.length currentGroup.tests > 0 then
             let newTestCase = testList currentGroup.name currentGroup.tests
             allTests <- allTests @ [ newTestCase ]

           if currentFn.recording then
             let (fn : RT.UserFunction.T) =
               { tlid = currentFn.tlid
                 name = currentFn.name
                 returnType = RT.TAny
                 description = "test function"
                 infix = false
                 body = (FSharpToExpr.parseRTExpr currentFn.code)
                 parameters = currentFn.parameters }

             functions <- Map.add currentFn.name fn functions

           // Clear settings
           currentTest <- emptyTest
           currentFn <- emptyFn
           currentGroup <- emptyGroup

         (dir + filename)
         |> System.IO.File.ReadLines
         |> Seq.iteri
              (fun i line ->
                let i = i + 1

                // This format is described in testfiles/README.md. If you make
                // any changes, update that file.
                match line with
                // [tests] indicator
                | Regex @"^\[tests\.(.*)\] with DB (.*)$" [ name; dbName ] ->
                    finish ()

                    match Map.get dbName dbs with
                    | Some db -> currentGroup <- { currentGroup with dbs = [ db ] }
                    | None -> failwith $"No DB named {dbName} found"

                    currentGroup <- { currentGroup with name = name }
                | Regex @"^\[tests\.(.*)\]$" [ name ] ->
                    finish ()
                    currentGroup <- { currentGroup with name = name }
                // [db] declaration
                | Regex @"^\[db.(.*) (\{.*\})\]\s*$" [ name; definition ] ->
                    finish ()

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
                // [function] declaration
                | Regex @"^\[fn\.(\S+) (.*)\]$" [ name; definition ] ->
                    finish ()

                    let parameters : List<RT.UserFunction.Parameter> =
                      definition
                      |> String.split " "
                      |> List.map
                           (fun name ->
                             { name = name; description = ""; typ = RT.TAny })

                    currentFn <-
                      { tlid = id i
                        recording = true
                        name = name
                        parameters = parameters
                        code = "" }
                // [test] with DB indicator
                | Regex @"^\[test\.(.*)\] with DB (.*)$" [ name; dbName ] ->
                    finish ()

                    match Map.get dbName dbs with
                    | Some db -> currentTest <- { currentTest with dbs = [ db ] }
                    | None -> failwith $"No DB named {dbName} found"

                    currentTest <- { currentTest with name = name; recording = true }
                // [test] indicator (no DB)
                | Regex @"^\[test\.(.*)\]$" [ name ] ->
                    finish ()
                    currentTest <- { currentTest with name = name; recording = true }
                // Skip whitespace lines
                | Regex @"^\s*$" [] -> ()
                // Skip whole-line comments
                | Regex @"^\s*//.*$" [] when
                  currentTest.recording || currentFn.recording -> ()
                // Append to the current test string
                | _ when currentTest.recording ->
                    currentTest <-
                      { currentTest with code = currentTest.code + line }
                | _ when currentFn.recording ->
                    currentFn <- { currentFn with code = currentFn.code + line }
                // 1-line test
                | Regex "^(.*)\s*$" [ code ] ->
                    let test = t $"line {i}" code currentGroup.dbs functions

                    currentGroup <-
                      { currentGroup with tests = currentGroup.tests @ [ test ] }
                | Regex "^(.*)\s*//\s*(.*)$" [ code; comment ] ->
                    let test =
                      t $"{comment} (line {i})" code currentGroup.dbs functions

                    currentGroup <-
                      { currentGroup with tests = currentGroup.tests @ [ test ] }
                | _ -> raise (System.Exception $"can't parse line {i}: {line}"))

         finish ()
         testList $"Tests from {filename}" allTests)
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
