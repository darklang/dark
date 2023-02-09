/// <summary>
/// Parses a file as F# and executes it against a new test canvas
/// </summary>
///
/// <remarks>
/// This isn't really for tests, it's for utilities to tests things in FSI.
/// There just wasn't a better place for this.
/// </remarks>
module TestUtils.FSI

open System.Threading.Tasks
open FSharp.Control.Tasks

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module DvalReprLegacyExternal = LibExecution.DvalReprLegacyExternal
module Exe = LibExecution.Execution

open Prelude

let execute (code : string) : RT.Dval =
  let t =
    task {
      let! meta = TestUtils.initializeTestCanvas (TestUtils.Exact "fsi")
      let! state = TestUtils.executionStateFor meta Map.empty Map.empty
      let prog = Parser.parseRTExpr code
      return! Exe.executeExpr state Map.empty prog
    }

  Task.WaitAll [| t :> Task |]
  t.Result
