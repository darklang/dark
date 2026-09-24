module Tests.AnalysisTypes

open Expecto
open Prelude
open TestUtils.TestUtils

/// A trace id used to carry an inverted timestamp so that sorting ids sorted traces
/// newest-first, which Google Cloud Storage needed and SQLite does not. What is left to say
/// about one is that it is unique and that a short prefix of it is too: it is the id a person
/// types at `traces resume`.
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
