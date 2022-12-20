module Tests.AnalysisTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes

let testTraceIDOrdering =
  testTask "test traceIDs are sortable in most recent order" {
    let traceID1 = LibExecution.AnalysisTypes.TraceID.create ()
    do! Task.Delay(10)
    let traceID2 = LibExecution.AnalysisTypes.TraceID.create ()
    do! Task.Delay(10)
    let traceID3 = LibExecution.AnalysisTypes.TraceID.create ()
    do! Task.Delay(10)
    let traceID4 = LibExecution.AnalysisTypes.TraceID.create ()
    do! Task.Delay(10)
    let traceID5 = LibExecution.AnalysisTypes.TraceID.create ()
    do! Task.Delay(10)
    let traceID6 = LibExecution.AnalysisTypes.TraceID.create ()
    do! Task.Delay(10)
    let expected =
      [ traceID6; traceID5; traceID4; traceID3; traceID2; traceID1 ]
      |> List.map string
    let actual =
      [ traceID4; traceID1; traceID5; traceID2; traceID3; traceID6 ]
      |> List.map string
      |> List.sort
    Expect.equal actual expected ""
  }

let tests = testList "AnalysisTypes" [ testTraceIDOrdering ]
