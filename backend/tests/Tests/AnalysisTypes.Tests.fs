module Tests.AnalysisTypes

open Expecto
open Prelude
open TestUtils.TestUtils

/// A trace id is the id a person types at `traces resume`, and the CLI takes the shortest
/// unique prefix, so what has to hold is that a SHORT prefix is unique too.
let testTraceIDsAreDistinctEarly =
  test "trace ids differ in their first eight characters" {
    let ids =
      List.init 200 (fun _ ->
        (string (LibExecution.AnalysisTypes.TraceID.create ())).Substring(0, 8))
    Expect.equal
      (List.length (List.distinct ids))
      (List.length ids)
      "two hundred ids, two hundred distinct prefixes"
  }

let tests = testList "AnalysisTypes" [ testTraceIDsAreDistinctEarly ]
