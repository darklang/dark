/// Allocation/timing benchmarks for the Blob and Stream code paths.
///
/// Two subcommands:
///   `bench`        — run all scenarios; append a snapshot to the
///                    tracked `benchmarks/results/history.jsonl`.
///                    Also writes a local-only `results/latest.json`
///                    (gitignored) for piping into other tools.
///   `bench-render` — read `history.jsonl` and rewrite
///                    `benchmarks/results.md`. Latest run is the
///                    headline; older runs become a per-run table.
///
/// Local hacking: drop a snapshot in
/// `benchmarks/results/local-*.json` (gitignored) if you want a
/// machine-local file. There's no `bench-promote` — committing a
/// `bench` run via `history.jsonl` is the promotion mechanism.
module LocalExec.Benchmarks

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module Dval = LibExecution.Dval
module Blob = LibExecution.Blob
module Stream = LibExecution.Stream
module VT = LibExecution.ValueType


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


/// runDir = `<repo>/rundir`; benchmarks live under `<repo>/benchmarks/results`.
let private resultsDir () : string =
  System.IO.Path.Combine(LibConfig.Config.runDir, "..", "benchmarks", "results")
  |> System.IO.Path.GetFullPath


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
          Blob.newEphemeral state bytes)
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
        Blob.newEphemeral state bytes)

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
      let _ = Blob.newEphemeral state raw
      System.Convert.ToHexString(raw))

  [ { scenario = "hexEncode"
      inputBytes = int64 size
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = 1
      note = "ok" } ]


/// Base64 encode a blob via the builtin code path.
let private base64Scenario (state : RT.ExecutionState) : List<Result> =
  let size = 1_000_000
  let raw = Array.zeroCreate<byte> size
  System.Random(0).NextBytes(raw)

  let _, sample =
    measure (fun () ->
      let _ = Blob.newEphemeral state raw
      System.Convert.ToBase64String(raw))

  [ { scenario = "base64Encode"
      inputBytes = int64 size
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = 1
      note = "ok" } ]


/// Many small ephemeral blobs in one scope, each with distinct bytes.
/// Stresses the ConcurrentDictionary/GUID-gen/Dval-wrapper hot path
/// that the fileRead bench amortises away. inputBytes = sum of all
/// blob sizes.
let private manyBlobsScenario (state : RT.ExecutionState) : List<Result> =
  let scenarios = [ 100, 1_024; 1_000, 1_024; 10_000, 256 ]
  let rng = System.Random(1)

  scenarios
  |> List.map (fun (count, perBlobBytes) ->
    // Pre-allocate distinct payloads outside the measure so the
    // measurement only captures the blob-creation hot path itself.
    let payloads =
      Array.init count (fun _ ->
        let p = Array.zeroCreate<byte> perBlobBytes
        rng.NextBytes(p)
        p)

    let _, sample =
      measure (fun () ->
        for i in 0 .. count - 1 do
          ignore<RT.Dval> (Blob.newEphemeral state payloads[i]))

    let totalBytes = int64 (count * perBlobBytes)
    { scenario = $"manyBlobs/{count}x{perBlobBytes}B"
      inputBytes = totalBytes
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = count
      note = "intra-scope; per-blob overhead amortised over count" })


/// Equality over two independently-built ephemeral blobs with the
/// same bytes. Exercises promoteBlobs+noopInsert+sha256 path.
/// inputBytes is the size of a single side; alloc reflects the work
/// done on both sides combined.
let private blobEqualityScenario (state : RT.ExecutionState) : List<Result> =
  let sizes = [ 100_000; 1_000_000; 10_000_000 ]

  sizes
  |> List.map (fun size ->
    let payload = Array.zeroCreate<byte> size
    System.Random(0).NextBytes(payload)

    let blobA = Blob.newEphemeral state payload
    let blobB = Blob.newEphemeral state payload

    let _, sample =
      measure (fun () ->
        let resultPly =
          uply { return! BuiltinExecution.Libs.NoModule.equals state blobA blobB }
        resultPly |> Ply.toTask |> _.Result)

    { scenario = "blobEqualityEphemeral"
      inputBytes = int64 size
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = 1
      note = "promote+hash both sides" })


/// Drain a Stream<UInt8> into a Blob via streamToBlob's chunked path.
/// This is the scenario the L.7 chunked-drain optimization addresses.
/// inputBytes = the byte stream length.
let private streamToBlobScenario (state : RT.ExecutionState) : List<Result> =
  let sizes = [ 100_000; 1_000_000; 10_000_000 ]

  sizes
  |> List.map (fun size ->
    let payload = Array.zeroCreate<byte> size
    System.Random(0).NextBytes(payload)

    let _, sample =
      measure (fun () ->
        // Build a chunked-source Stream that yields the payload as one
        // chunk, then drain via readStreamChunk in 64KB increments —
        // mirrors what `streamToBlob` does today.
        let mutable yielded = false
        let nextChunk (_max : int) : Ply<Option<byte[]>> =
          uply {
            if yielded then
              return None
            else
              yielded <- true
              return Some payload
          }
        let stream = Stream.newChunked VT.uint8 nextChunk None
        let collected = new System.IO.MemoryStream()
        let rec drain () : Ply<unit> =
          uply {
            let! chunk = Stream.readChunk 65536 stream
            match chunk with
            | Some bs ->
              collected.Write(bs, 0, bs.Length)
              return! drain ()
            | None -> return ()
          }
        drain () |> Ply.toTask |> _.Wait()
        let bytes = collected.ToArray()
        ignore<RT.Dval> (Blob.newEphemeral state bytes))

    { scenario = "streamToBlob"
      inputBytes = int64 size
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = 1
      note = "chunked-drain" })


/// Multipart-style body construction: build a Blob by concatenating
/// many text and binary parts via Stdlib.Blob.concat. Mirrors the
/// openai/audio.dark MultipartRequest pattern. inputBytes is the
/// total of all parts.
let private multipartScenario (state : RT.ExecutionState) : List<Result> =
  let configs = [ 10, 1_024; 100, 10_240; 50, 100_000 ]

  configs
  |> List.map (fun (parts, perPartBytes) ->
    let part = Array.zeroCreate<byte> perPartBytes
    System.Random(0).NextBytes(part)

    let _, sample =
      measure (fun () ->
        // Build a List<Blob> of parts; concat via MemoryStream (the
        // shape Blob.concat ultimately uses on the F# side).
        use buf = new System.IO.MemoryStream()
        for _ in 1..parts do
          // Each "part" mints an ephemeral.
          let _ = Blob.newEphemeral state part
          buf.Write(part, 0, part.Length)
        ignore<RT.Dval> (Blob.newEphemeral state (buf.ToArray())))

    let total = int64 (parts * perPartBytes)
    { scenario = $"multipart/{parts}x{perPartBytes}B"
      inputBytes = total
      allocBytes = sample.allocBytes
      elapsedMs = sample.elapsedMs
      dvalNodes = parts + 1
      note = "concat" })


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
        yield! hexEncodeScenario state
        yield! base64Scenario state
        yield! manyBlobsScenario state
        yield! blobEqualityScenario state
        yield! streamToBlobScenario state
        yield! multipartScenario state ]

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

    let outDir = resultsDir ()
    System.IO.Directory.CreateDirectory(outDir) |> ignore<System.IO.DirectoryInfo>

    System.IO.File.WriteAllText(System.IO.Path.Combine(outDir, "latest.json"), json)

    let historyEntry = json.Replace("\n", "") + "\n"
    System.IO.File.AppendAllText(
      System.IO.Path.Combine(outDir, "history.jsonl"),
      historyEntry
    )

    print $"\nWrote {results.Length} results to {outDir}/latest.json"
    print "scenario                       input bytes    alloc bytes  overhead   ms"
    print "--------                       -----------    -----------  --------  ---"
    for r in results do
      let overhead =
        if r.inputBytes <= 0L then
          "-"
        else
          $"{(float r.allocBytes) / (float r.inputBytes):F2}x"
      let scenarioPad = r.scenario.PadRight(30)
      let inputPad = (string r.inputBytes).PadLeft(11)
      let allocPad = (string r.allocBytes).PadLeft(11)
      let overheadPad = overhead.PadLeft(8)
      let msPad = (string r.elapsedMs).PadLeft(4)
      print $"{scenarioPad} {inputPad}    {allocPad}  {overheadPad} {msPad}"

    return Ok()
  }


/// Parse one JSONL row and return (timestamp, commit, results).
/// Best-effort: returns `None` if anything's malformed.
let private readSnapshotJson
  (json : string)
  : Option<string * string * List<Result>> =
  try
    use doc = System.Text.Json.JsonDocument.Parse(json)
    let root = doc.RootElement
    let timestamp = root.GetProperty("timestamp").GetString()
    let commit = root.GetProperty("commit").GetString()
    let results =
      root.GetProperty("results").EnumerateArray()
      |> Seq.map (fun el ->
        { scenario = el.GetProperty("scenario").GetString()
          inputBytes = el.GetProperty("inputBytes").GetInt64()
          allocBytes = el.GetProperty("allocBytes").GetInt64()
          elapsedMs = el.GetProperty("elapsedMs").GetInt64()
          dvalNodes = el.GetProperty("dvalNodes").GetInt32()
          note = el.GetProperty("note").GetString() })
      |> Seq.toList
    Some(timestamp, commit, results)
  with _ ->
    None


let private formatBytes (n : int64) : string =
  System.String.Format(
    System.Globalization.CultureInfo.InvariantCulture,
    "{0:N0}",
    n
  )


let private overheadStr (alloc : int64) (input : int64) : string =
  if input <= 0L then "-" else $"{(float alloc) / (float input):F2}x"


/// Render the markdown table block for one snapshot, grouped by scenario.
let private renderSnapshotBody (results : List<Result>) : System.Text.StringBuilder =
  let sb = System.Text.StringBuilder()
  // Stable grouping by scenario family (prefix before '/'), preserving
  // the input order for both the family list and the rows within.
  let grouped =
    results
    |> List.fold
      (fun acc r ->
        let group = r.scenario.Split('/')[0]
        match acc |> List.tryFindIndex (fun (g, _) -> g = group) with
        | Some i ->
          acc
          |> List.mapi (fun j (g, rs) -> if j = i then (g, rs @ [ r ]) else (g, rs))
        | None -> acc @ [ (group, [ r ]) ])
      []
  for (group, rows) in grouped do
    sb.AppendLine($"### {group}") |> ignore<System.Text.StringBuilder>
    sb.AppendLine() |> ignore<System.Text.StringBuilder>
    sb.AppendLine(
      "| scenario | input bytes | alloc bytes | overhead | ms | dval nodes |"
    )
    |> ignore<System.Text.StringBuilder>
    sb.AppendLine(
      "| -------- | ----------: | ----------: | -------: | -: | ---------: |"
    )
    |> ignore<System.Text.StringBuilder>
    for r in rows do
      sb.AppendLine(
        $"| {r.scenario} | {formatBytes r.inputBytes} | {formatBytes r.allocBytes} | {overheadStr r.allocBytes r.inputBytes} | {r.elapsedMs} | {r.dvalNodes} |"
      )
      |> ignore<System.Text.StringBuilder>
    sb.AppendLine() |> ignore<System.Text.StringBuilder>
  sb


/// Read every JSONL row in `history.jsonl` and rewrite
/// `benchmarks/results.md`. The most recent run becomes the headline
/// (full per-scenario tables); every prior run gets a one-line entry
/// in a "Run history" table.
let render () : Ply<Result<unit, string>> =
  uply {
    let dir = resultsDir ()
    let historyPath = System.IO.Path.Combine(dir, "history.jsonl")
    if not (System.IO.File.Exists(historyPath)) then
      return Error $"No {historyPath} — run `bench` to record at least one snapshot."
    else
      let snapshots =
        System.IO.File.ReadAllLines(historyPath)
        |> Array.filter (fun line -> line.Trim() <> "")
        |> Array.choose readSnapshotJson
        |> Array.toList

      match List.tryLast snapshots with
      | None -> return Error $"{historyPath} contained no readable snapshots."
      | Some(latestTs, latestCommit, latestResults) ->
        let sb = System.Text.StringBuilder()
        sb.AppendLine("# Benchmark results") |> ignore<System.Text.StringBuilder>
        sb.AppendLine() |> ignore<System.Text.StringBuilder>
        sb.AppendLine(
          "Generated by `./scripts/run-local-exec bench-render`. Don't edit by hand."
        )
        |> ignore<System.Text.StringBuilder>
        sb.AppendLine() |> ignore<System.Text.StringBuilder>
        sb.AppendLine($"## Latest — {latestTs} @ `{latestCommit.Substring(0, 7)}`")
        |> ignore<System.Text.StringBuilder>
        sb.AppendLine() |> ignore<System.Text.StringBuilder>
        sb.Append(renderSnapshotBody latestResults)
        |> ignore<System.Text.StringBuilder>

        let priorRuns = snapshots |> List.take (snapshots.Length - 1)
        if not priorRuns.IsEmpty then
          sb.AppendLine("## Run history") |> ignore<System.Text.StringBuilder>
          sb.AppendLine() |> ignore<System.Text.StringBuilder>
          sb.AppendLine("| timestamp | commit | rows |")
          |> ignore<System.Text.StringBuilder>
          sb.AppendLine("| --------- | ------ | ---: |")
          |> ignore<System.Text.StringBuilder>
          // Newest first.
          for (ts, commit, results) in List.rev priorRuns do
            sb.AppendLine(
              $"| {ts} | `{commit.Substring(0, 7)}` | {results.Length} |"
            )
            |> ignore<System.Text.StringBuilder>
          sb.AppendLine() |> ignore<System.Text.StringBuilder>

        let outPath =
          System.IO.Path.Combine(dir, "..", "results.md")
          |> System.IO.Path.GetFullPath
        System.IO.File.WriteAllText(outPath, sb.ToString())
        print $"Wrote {outPath}"
        return Ok()
  }
