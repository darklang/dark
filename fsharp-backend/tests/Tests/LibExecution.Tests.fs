module Tests.LibExecution

// Create test cases from .tests files in the tests/stdlib dir

open Expecto
open Prelude

let t (comment : string) (code : string) : Test =
  let name = $"{comment} ({code})"
  if code.StartsWith "//" then
    ptestTask name { return (Expect.equal "skipped" "skipped" "") }
  else
    testTask name {
      try
        let fns = LibExecution.StdLib.fns @ LibBackend.StdLib.fns @ Tests.LibTest.fns

        let source = FSharpToExpr.parse code
        let actualProg, expectedResult = FSharpToExpr.convertToTest source
        let! actual = LibExecution.Execution.run [] fns actualProg
        let! expected = LibExecution.Execution.run [] fns expectedResult

        return (Expect.equal
                  actual
                  expected
                  // $"{source} => {actualProg} = {expectedResult}")
                  $"{actualProg} = {expectedResult}")
      with e -> return (Expect.equal "" e.Message "Error message")
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
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.map (fun filename ->
       if filename.Contains "language" then // tweak to run subset of tests
         let currentTestName = ref ""
         let currentTests = ref [] // keep track of tests within a [tests]
         let singleTestMode = ref false
         let currentTestString = ref "" // keep track of the current [test]
         let allTests = ref [] // finished test and suites

         let finish () =
           // Add the current work to allTests, and clear
           let newTestCase =
             if !singleTestMode then
               // Add a single test case
               t !currentTestName !currentTestString
             else
               // Put currentTests in a group and add them
               testList !currentTestName !currentTests

           allTests := !allTests @ [ newTestCase ]

           // Clear settings
           currentTestName := ""
           singleTestMode := false
           currentTestString := ""
           currentTests := []

         (dir + filename)
         |> System.IO.File.ReadLines
         |> Seq.iteri (fun i line ->
              let i = i + 1
              match line with
              // [tests] indicator
              | Regex "^\[tests\.(.*)\]$" [ name ] ->
                  finish ()
                  currentTestName := name
              // [test] indicator
              | Regex "^\[test\.(.*)\]$" [ name ] ->
                  finish ()
                  singleTestMode := true
                  currentTestName := name
              // Skip comment-only lines
              | Regex "^\s*//.*" [] -> ()
              // Append to the current test string
              | _ when !singleTestMode ->
                  currentTestString := !currentTestString + line
              // Skip whitespace lines
              | Regex "^\s*$" [] -> ()
              // 1-line test
              | Regex "^(.*)\s*$" [ code ] ->
                  currentTests := !currentTests @ [ t $"line {i}" code ]
              // 1-line test w/ comment
              | Regex "^(.*)\s*//\s*(.*)$" [ code; comment ] ->
                  currentTests := !currentTests @ [ t $"{comment} (line {i})" code ]
              | _ -> raise (System.Exception $"can't parse line {i}: {line}"))

         finish ()
         testList $"Tests from {filename}" !allTests
       else
         testList $"Tests from {filename}" [])
  |> Array.toList
  |> testList "All files"

let tests = testList "StdLib" [ fileTests () ]
