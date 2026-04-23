/// Phase 0 — baseline measurement harness.
///
/// See thinking/blobs-and-streams/10-phase-0.md. Each chunk (0.2–0.5)
/// will add one or more scenarios here. Results go to per-scenario
/// files under rundir/measurements/phase-0/ and get consolidated into
/// thinking/blobs-and-streams/baseline.md by chunk 0.6.
///
/// Run with:
///   ./scripts/run-backend-tests --filter-test-list Measurement
module Tests.Measurement

open Expecto

open Prelude

open Tests.MeasurementHelpers


/// Dummy scenario that exercises the full harness path (measure +
/// appendRow) without doing any real work. Lets later chunks assume
/// the plumbing is wired.
let harnessSelfTest =
  test "harness self-test: measure and append a row" {
    let result, sample = measure (fun () -> Array.zeroCreate<byte> 1_000_000)
    Expect.equal result.Length 1_000_000 "allocated the expected buffer"
    Expect.isGreaterThan
      sample.allocatedBytesDelta
      500_000L
      "should have allocated at least ~1MB (noise-tolerant lower bound)"
    Expect.isGreaterThan
      sample.peakWorkingSet
      0L
      "working-set reading should be non-zero"

    appendRow
      "harness.txt"
      $"self-test alloc={sample.allocatedBytesDelta} ws={sample.peakWorkingSet} ms={sample.elapsedMs}"
  }


let tests = testList "measurement" [ harnessSelfTest ]
