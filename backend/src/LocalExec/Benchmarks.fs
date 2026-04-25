/// Allocation/timing benchmarks for the Blob and Stream code paths.
///
/// Run via `./scripts/run-local-exec bench`. Writes a JSON snapshot to
/// `backend/benchmarks/results/latest.json` (and appends to history.jsonl).
/// View via `backend/benchmarks/viewer.html`.
module LocalExec.Benchmarks

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval


type Sample = { allocBytes : int64; peakWorkingSet : int64; elapsedMs : int64 }


/// Force GC, time + measure allocation around `f`.
let measure<'a> (f : unit -> 'a) : 'a * Sample =
  System.GC.Collect()
  System.GC.WaitForPendingFinalizers()
  System.GC.Collect()

  let proc = System.Diagnostics.Process.GetCurrentProcess()
  let beforeAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
  let beforeWs = proc.WorkingSet64
  let sw = System.Diagnostics.Stopwatch.StartNew()

  let result = f ()

  sw.Stop()
  proc.Refresh()
  let afterAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
  let afterWs = proc.WorkingSet64

  let sample =
    { allocBytes = afterAlloc - beforeAlloc
      peakWorkingSet = max beforeWs afterWs
      elapsedMs = sw.ElapsedMilliseconds }

  result, sample


let rec private countDvalNodes (dv : RT.Dval) : int =
  match dv with
  | RT.DList(_, items) -> 1 + List.sumBy countDvalNodes items
  | RT.DTuple(a, b, rest) ->
    1 + countDvalNodes a + countDvalNodes b + List.sumBy countDvalNodes rest
  | RT.DDict(_, entries) -> 1 + (entries |> Map.values |> Seq.sumBy countDvalNodes)
  | RT.DRecord(_, _, _, fields) ->
    1 + (fields |> Map.values |> Seq.sumBy countDvalNodes)
  | RT.DEnum(_, _, _, _, fields) -> 1 + List.sumBy countDvalNodes fields
  | _ -> 1


type Result =
  { scenario : string
    inputBytes : int64
    allocBytes : int64
    elapsedMs : int64
    dvalNodes : int
    note : string }


let private jsonEscape (s : string) : string =
  s.Replace("\\", "\\\\").Replace("\"", "\\\"")

let private serializeResult (r : Result) : string =
  $"""    {{ "scenario": "{jsonEscape r.scenario}", "inputBytes": {r.inputBytes}, "allocBytes": {r.allocBytes}, "elapsedMs": {r.elapsedMs}, "dvalNodes": {r.dvalNodes}, "note": "{jsonEscape r.note}" }}"""


/// Make a fresh ExecutionState for measurement runs. Uses the empty
/// PackageManager (benchmarks don't need the real PM) and no tracing.
let private freshState () : RT.ExecutionState =
  let builtins = TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT
  LibExecution.Execution.createState
    builtins
    TestUtils.TestUtils.pmRT
    LibExecution.Execution.noTracing
    (fun _ _ _ _ -> uply { return () })
    (fun _ _ _ _ -> uply { return () })
    LibExecution.ProgramTypes.mainBranchId
    { canvasID = System.Guid.NewGuid()
      internalFnsAllowed = false
      dbs = Map.empty
      secrets = [] }


let private fileReadScenario (state : RT.ExecutionState) : List<Result> =
  let tempDir = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "dark-bench")
  System.IO.Directory.CreateDirectory(tempDir) |> ignore<System.IO.DirectoryInfo>

  let sizes = [ 1_024; 100_000; 1_000_000; 10_000_000; 38_000_000 ]
  let rng = System.Random(0)

  sizes
  |> List.map (fun size ->
    let path = System.IO.Path.Combine(tempDir, $"bench-{size}.bin")
    if not (System.IO.File.Exists(path)) then
      let buf = Array.zeroCreate<byte> size
      rng.NextBytes(buf)
      System.IO.File.WriteAllBytes(path, buf)

    try
      let dval, sample =
        measure (fun () ->
          let bytes = System.IO.File.ReadAllBytes(path)
          Dval.newEphemeralBlob state bytes)
      { scenario = "fileRead"
        inputBytes = int64 size
        allocBytes = sample.allocBytes
        elapsedMs = sample.elapsedMs
        dvalNodes = countDvalNodes dval
        note = "ok" }
    with :? System.OutOfMemoryException ->
      { scenario = "fileRead"
        inputBytes = int64 size
        allocBytes = -1L
        elapsedMs = -1L
        dvalNodes = -1
        note = "oom" })


let private httpBodyScenario (state : RT.ExecutionState) : List<Result> =
  let sizes = [ 100_000; 1_000_000; 10_000_000 ]

  sizes
  |> List.map (fun size ->
    let payload = Array.zeroCreate<byte> size
    System.Random(0).NextBytes(payload)

    let handler =
      { new System.Net.Http.HttpMessageHandler() with
          member _.SendAsync(_req, _ct) =
            let resp =
              new System.Net.Http.HttpResponseMessage(System.Net.HttpStatusCode.OK)
            resp.Content <- new System.Net.Http.ByteArrayContent(payload)
            System.Threading.Tasks.Task.FromResult(resp) }
    use client = new System.Net.Http.HttpClient(handler)

    let dval, sample =
      measure (fun () ->
        let resp =
          client.GetAsync("http://fake.local/body").GetAwaiter().GetResult()
        let bytes = resp.Content.ReadAsByteArrayAsync().GetAwaiter().GetResult()
        Dval.newEphemeralBlob state bytes)

    { scenario = "httpBody"
      inputBytes = int64 size
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = countDvalNodes dval
      note = "simulated-handler" })


let private hexEncodeScenario (state : RT.ExecutionState) : List<Result> =
  let size = 1_000_000
  let raw = Array.zeroCreate<byte> size
  System.Random(0).NextBytes(raw)

  let _, sample =
    measure (fun () ->
      // Mirror the Builtin.blobToHex path: mint blob, deref bytes, hex-encode.
      let _ = Dval.newEphemeralBlob state raw
      System.Convert.ToHexString(raw))

  [ { scenario = "hexEncode"
      inputBytes = int64 size
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = 1
      note = "ok" } ]


/// Captures run identity for the JSON output.
let private gitCommit () : string =
  try
    let psi =
      System.Diagnostics.ProcessStartInfo(
        FileName = "git",
        Arguments = "rev-parse HEAD",
        RedirectStandardOutput = true,
        UseShellExecute = false
      )
    use proc = System.Diagnostics.Process.Start(psi)
    let out = proc.StandardOutput.ReadToEnd().Trim()
    proc.WaitForExit()
    out
  with _ ->
    "unknown"


let runAll () : Ply<Result<unit, string>> =
  uply {
    let state = freshState ()

    let results =
      [ yield! fileReadScenario state
        yield! httpBodyScenario state
        yield! hexEncodeScenario state ]

    let commit = gitCommit ()
    let timestamp = System.DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ")

    let resultsJson = results |> List.map serializeResult |> String.concat ",\n"

    let json =
      $"""{{
  "timestamp": "{timestamp}",
  "commit": "{commit}",
  "results": [
{resultsJson}
  ]
}}
"""

    // runDir = `<repo>/rundir`; the results live under `<repo>/backend/benchmarks/results`.
    let outDir =
      System.IO.Path.Combine(
        LibConfig.Config.runDir,
        "..",
        "backend",
        "benchmarks",
        "results"
      )
      |> System.IO.Path.GetFullPath
    System.IO.Directory.CreateDirectory(outDir) |> ignore<System.IO.DirectoryInfo>

    System.IO.File.WriteAllText(System.IO.Path.Combine(outDir, "latest.json"), json)

    let historyEntry = json.Replace("\n", "") + "\n"
    System.IO.File.AppendAllText(
      System.IO.Path.Combine(outDir, "history.jsonl"),
      historyEntry
    )

    print $"\nWrote {results.Length} results to {outDir}/latest.json"
    print "scenario        input bytes      overhead"
    print "--------        -----------      --------"
    for r in results do
      let overhead =
        if r.inputBytes <= 0L then
          "—"
        else
          $"{(float r.allocBytes) / (float r.inputBytes):F2}x"
      let scenarioPad = r.scenario.PadRight(15)
      let inputPad = (string r.inputBytes).PadLeft(11)
      print $"{scenarioPad} {inputPad}      {overhead}"

    return Ok()
  }
