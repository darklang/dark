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

    resetOutput "fileRead.txt"
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


/// Scenario 2 — HttpClient response-body memory profile.
///
/// Exercises the `HttpResponseMessage -> ReadAsByteArrayAsync ->
/// byteArrayToDvalList` path that today's `HttpClient.request`
/// builtin drives into. We use an in-process fake
/// `HttpMessageHandler` so there is no real socket; the dominant
/// cost — list boxing — shows up identically. The note column flags
/// the simulation so the baseline reader isn't misled.
let httpBodyProfile =
  test "httpclient body memory profile" {
    resetOutput "httpBody.txt"
    appendRow
      "httpBody.txt"
      "size_bytes,alloc_bytes,peak_ws_bytes,elapsed_ms,dval_nodes,note"

    let sizes = [ 100_000; 1_000_000; 10_000_000 ]

    for size in sizes do
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

      let row =
        try
          let dval, sample =
            measure (fun () ->
              let resp =
                client.GetAsync("http://fake.local/body").GetAwaiter().GetResult()
              let bytes =
                resp.Content.ReadAsByteArrayAsync().GetAwaiter().GetResult()
              Dval.byteArrayToDvalList bytes)
          let nodes = countDvalNodes dval
          $"{size},{sample.allocatedBytesDelta},{sample.peakWorkingSet},{sample.elapsedMs},{nodes},simulated-handler"
        with
        | :? System.OutOfMemoryException -> $"{size},-1,-1,-1,-1,oom"
        | e ->
          let msg = e.Message.Replace(",", ";").Replace("\n", " ")
          $"{size},-1,-1,-1,-1,err:{msg}"

      appendRow "httpBody.txt" row
  }


/// Stream that yields [chunkCount] chunks of [chunkSize] bytes each,
/// sleeping [delayMs] between chunks. Simulates a slow chunked HTTP
/// response body for scenario 3. Content is zero-filled — we're
/// measuring allocation, not entropy.
type private SlowChunkedStream(chunkSize : int, chunkCount : int, delayMs : int) =
  inherit System.IO.Stream()

  let mutable bufferRemaining = 0
  let mutable chunksEmitted = 0
  let buffer = Array.zeroCreate<byte> chunkSize

  override _.CanRead = true
  override _.CanSeek = false
  override _.CanWrite = false
  override _.Length = int64 (chunkSize * chunkCount)
  override _.Position
    with get () = 0L
    and set _ = raise (System.NotSupportedException())
  override _.Flush() = ()
  override _.Seek(_, _) = raise (System.NotSupportedException())
  override _.SetLength(_) = raise (System.NotSupportedException())
  override _.Write(_, _, _) = raise (System.NotSupportedException())

  override _.Read(dest, offset, count) =
    if bufferRemaining = 0 && chunksEmitted < chunkCount then
      System.Threading.Thread.Sleep(delayMs)
      bufferRemaining <- chunkSize
      chunksEmitted <- chunksEmitted + 1
    if bufferRemaining = 0 then
      0
    else
      let bytes = min count bufferRemaining
      System.Array.Copy(buffer, 0, dest, offset, bytes)
      bufferRemaining <- bufferRemaining - bytes
      bytes


/// Scenario 3 — streaming-http chunk behaviour.
///
/// Drives a slow chunked response through the same
/// `ReadAsStreamAsync -> 8KB-buffer ReadAsync loop` path that
/// `StreamingHttpClient.fs` uses today, and converts each read into a
/// `Dval.byteArrayToDvalList` (what the streaming builtin surfaces as
/// `StreamChunk.Data`). Records time-to-first-chunk, time-to-last,
/// and total allocation from which we derive per-chunk average.
let streamingHttpProfile =
  test "streaming-http chunk behaviour" {
    resetOutput "streaming.txt"
    appendRow
      "streaming.txt"
      "chunks,chunk_bytes,delay_ms,total_alloc,time_to_first_ms,time_to_last_ms,per_chunk_alloc,note"

    // scenario spec: 100 × 100KB chunks, 100ms apart.
    let chunkSize = 100_000
    let chunkCount = 100
    let delayMs = 100

    let handler =
      { new System.Net.Http.HttpMessageHandler() with
          member _.SendAsync(_req, _ct) =
            let resp =
              new System.Net.Http.HttpResponseMessage(System.Net.HttpStatusCode.OK)
            let stream =
              new SlowChunkedStream(chunkSize, chunkCount, delayMs)
              :> System.IO.Stream
            resp.Content <- new System.Net.Http.StreamContent(stream)
            System.Threading.Tasks.Task.FromResult(resp) }

    use client = new System.Net.Http.HttpClient(handler)

    let row =
      try
        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()
        System.GC.Collect()
        let beforeAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
        let sw = System.Diagnostics.Stopwatch.StartNew()

        let resp =
          client
            .GetAsync(
              "http://fake.local/stream",
              System.Net.Http.HttpCompletionOption.ResponseHeadersRead
            )
            .GetAwaiter()
            .GetResult()

        use responseStream =
          resp.Content.ReadAsStreamAsync().GetAwaiter().GetResult()

        // 8KB read buffer matches StreamingHttpClient.fs:266.
        let buffer = Array.zeroCreate<byte> 8192
        let mutable firstChunkMs = -1L
        let mutable chunksSeen = 0

        let rec pump () =
          let bytesRead = responseStream.Read(buffer, 0, buffer.Length)
          if bytesRead > 0 then
            if firstChunkMs < 0L then firstChunkMs <- sw.ElapsedMilliseconds
            let bytes = Array.sub buffer 0 bytesRead
            // mirror what StreamingHttpClient does per-chunk: wrap in
            // a DList(DUInt8). This is the measured cost.
            let _dval = Dval.byteArrayToDvalList bytes
            chunksSeen <- chunksSeen + 1
            pump ()

        pump ()

        let lastMs = sw.ElapsedMilliseconds
        let afterAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
        let totalAlloc = afterAlloc - beforeAlloc
        let perChunk = if chunksSeen > 0 then totalAlloc / int64 chunksSeen else 0L
        $"{chunksSeen},{chunkSize},{delayMs},{totalAlloc},{firstChunkMs},{lastMs},{perChunk},ok"
      with
      | :? System.OutOfMemoryException -> "0,0,0,-1,-1,-1,-1,oom"
      | e ->
        let msg = e.Message.Replace(",", ";").Replace("\n", " ")
        $"0,0,0,-1,-1,-1,-1,err:{msg}"

    appendRow "streaming.txt" row
  }


let tests =
  testList
    "measurement"
    [ harnessSelfTest; fileReadProfile; httpBodyProfile; streamingHttpProfile ]
