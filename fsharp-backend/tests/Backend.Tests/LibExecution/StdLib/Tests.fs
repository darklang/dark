module LibExecution.StdLib.Tests

open Expecto

// Read all test files. Each file of a file represents a test with a comment which is the test name
let fileTests () : Test =
  let dir = "tests/stdlib/"
  System.IO.Directory.GetFiles(dir, "*")
  |> Array.map (System.IO.Path.GetFileName)
  |> Array.map (fun filename ->
       if filename.Contains "" then // tweak to run subset of tests
         (dir + filename)
         |> System.IO.File.ReadLines
         |> Seq.map (fun (line : string) ->
              let split = line.Split(" // ")
              let comment = if split.Length = 1 then "" else split.[1]
              ExecUtils.t split.[0] comment)
         |> Seq.toList
         |> testList $"Tests from {filename}"
       else
         testList $"Tests from {filename}" [])
  |> Array.toList
  |> testList "All files"

let tests = testList "StdLib" [ fileTests () ]
