module Tests.AnalysisTypes

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
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

let testTraceIDTimestampRoundtrip =
  testMany
    "roundtrip traceID timestamps"
    (fun (ts : int64) ->
      ts
      |> NodaTime.Instant.FromUnixTimeMilliseconds
      |> LibExecution.AnalysisTypes.TraceID.fromTimestamp
      |> LibExecution.AnalysisTypes.TraceID.toTimestamp
      |> fun ts -> ts.ToUnixTimeMilliseconds())
    // Only test positive values since we throw away the important bytes for negative
    // values, and we only care about positive values anyway (since we are creating
    // the timestamps in 2022 or later)
    [ NodaTime.Instant.MaxValue.ToUnixTimeMilliseconds(),
      NodaTime.Instant.MaxValue.ToUnixTimeMilliseconds()
      0L, 0L
      1L, 1L
      2L, 2L
      1671731279596L, 1671731279596L
      1671731278547L, 1671731278547L ]

let tests =
  testList "AnalysisTypes" [ testTraceIDOrdering; testTraceIDTimestampRoundtrip ]
