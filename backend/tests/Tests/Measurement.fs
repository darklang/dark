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

module Dval = LibExecution.Dval


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


/// Scenario 1 — fileRead memory/allocation profile.
///
/// Mirrors what [BuiltinCli.Libs.File.fileRead] does today:
/// [System.IO.File.ReadAllBytesAsync] followed by
/// [Dval.byteArrayToDvalList]. The second step is where the boxed
/// `DList(DUInt8)` representation blows up — post-Phase-1 this row
/// should drop to a single `DBlob` allocation.
///
/// Writes one row per file-size target. Each row includes allocation
/// delta, peak working set, wall time, and the Dval node count
/// (reachable allocations, not bytes). 38 MB may OOM; we catch
/// [OutOfMemoryException] and log it as such.
let fileReadProfile =
  test "fileRead memory profile" {
    let tempDir = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "dark-phase0")
    System.IO.Directory.CreateDirectory(tempDir) |> ignore<System.IO.DirectoryInfo>

    // Write the row header first so the file is legible even if a
    // later case crashes the runner.
    appendRow
      "fileRead.txt"
      "size_bytes,alloc_bytes,peak_ws_bytes,elapsed_ms,dval_nodes,note"

    // 38_000_000 omitted: would allocate ~50 GB and risk OOM-killing
    // the test runner. The observation is recorded as an explicit row
    // below, sourced from thinking/bug-fileread-oom.md.
    let sizes = [ 1_024; 100_000; 1_000_000; 10_000_000 ]

    let rng = System.Random(0)

    for size in sizes do
      let path = System.IO.Path.Combine(tempDir, $"measure-{size}.bin")
      if not (System.IO.File.Exists(path)) then
        let buf = Array.zeroCreate<byte> size
        rng.NextBytes(buf)
        System.IO.File.WriteAllBytes(path, buf)

      let row =
        try
          let dval, sample =
            measure (fun () ->
              let bytes = System.IO.File.ReadAllBytes(path)
              Dval.byteArrayToDvalList bytes)
          let nodes = countDvalNodes dval
          $"{size},{sample.allocatedBytesDelta},{sample.peakWorkingSet},{sample.elapsedMs},{nodes},ok"
        with
        | :? System.OutOfMemoryException -> $"{size},-1,-1,-1,-1,oom"
        | e ->
          let msg = e.Message.Replace(",", ";").Replace("\n", " ")
          $"{size},-1,-1,-1,-1,err:{msg}"

      appendRow "fileRead.txt" row

    // Explicit anecdotal row for the OOM repro; not measured here.
    appendRow
      "fileRead.txt"
      "38000000,-1,-1,-1,-1,anecdote:bug-fileread-oom.md reports ~49GB RSS then OOM-kill"
  }


let tests = testList "measurement" [ harnessSelfTest; fileReadProfile ]
