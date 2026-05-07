/// Allocation/timing scenarios for the Blob and Stream code paths.
/// Each scenario emits a list of `Result` rows; `run` concatenates them.
/// The orchestration (JSON I/O, markdown render, CLI wiring) lives in
/// `Benchmarks.fs`.
module LocalExec.BenchmarkScenarios

open Prelude

module RT = LibExecution.RuntimeTypes
module Blob = LibExecution.Blob
module Stream = LibExecution.Stream
module VT = LibExecution.ValueType


type Sample = { allocBytes : int64; elapsedMs : int64 }

type Result =
  { scenario : string
    inputBytes : int64
    allocBytes : int64
    elapsedMs : int64
    dvalNodes : int
    note : string }


/// Force GC, time + measure allocation around `f`.
let measure<'a> (f : unit -> 'a) : 'a * Sample =
  System.GC.Collect()
  System.GC.WaitForPendingFinalizers()
  System.GC.Collect()
  let beforeAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
  let sw = System.Diagnostics.Stopwatch.StartNew()
  let result = f ()
  sw.Stop()
  let afterAlloc = System.GC.GetTotalAllocatedBytes(precise = false)
  result,
  { allocBytes = afterAlloc - beforeAlloc; elapsedMs = sw.ElapsedMilliseconds }


let rec private countDvalNodes (dv : RT.Dval) : int =
  match dv with
  | RT.DList(_, xs) -> 1 + List.sumBy countDvalNodes xs
  | RT.DTuple(a, b, rest) ->
    1 + countDvalNodes a + countDvalNodes b + List.sumBy countDvalNodes rest
  | RT.DDict(_, m) -> 1 + (m |> Map.values |> Seq.sumBy countDvalNodes)
  | RT.DRecord(_, _, _, m) -> 1 + (m |> Map.values |> Seq.sumBy countDvalNodes)
  | RT.DEnum(_, _, _, _, fs) -> 1 + List.sumBy countDvalNodes fs
  | _ -> 1


let private mkResult
  (scenario : string)
  (inputBytes : int)
  (dvalNodes : int)
  (note : string)
  (sample : Sample)
  : Result =
  { scenario = scenario
    inputBytes = int64 inputBytes
    allocBytes = sample.allocBytes
    elapsedMs = sample.elapsedMs
    dvalNodes = dvalNodes
    note = note }


let private randomBytes (seed : int) (size : int) : byte[] =
  let buf = Array.zeroCreate<byte> size
  System.Random(seed).NextBytes(buf)
  buf


/// Fresh ExecutionState for measurement runs. Uses TestUtils' canonical
/// PMs (benchmarks share that wiring with the test infra).
let freshState () : RT.ExecutionState =
  let builtins = TestUtils.TestUtils.localBuiltIns TestUtils.TestUtils.pmPT
  LibExecution.Execution.createState
    builtins
    TestUtils.TestUtils.pmRT
    LibExecution.Execution.noTracing
    (fun _ _ _ _ -> uply { return () })
    (fun _ _ _ _ -> uply { return () })
    LibExecution.ProgramTypes.mainBranchId
    { dbs = Map.empty }


// ─── Scenarios ───────────────────────────────────────────────────

let private fileRead (state : RT.ExecutionState) : List<Result> =
  let tempDir = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "dark-bench")
  System.IO.Directory.CreateDirectory(tempDir) |> ignore<System.IO.DirectoryInfo>
  let rng = System.Random(0)

  [ 1_024; 100_000; 1_000_000; 10_000_000; 38_000_000 ]
  |> List.map (fun size ->
    let path = System.IO.Path.Combine(tempDir, $"bench-{size}.bin")
    if not (System.IO.File.Exists(path)) then
      let buf = Array.zeroCreate<byte> size
      rng.NextBytes(buf)
      System.IO.File.WriteAllBytes(path, buf)
    try
      let dv, sample =
        measure (fun () ->
          Blob.newEphemeral state (System.IO.File.ReadAllBytes(path)))
      mkResult "fileRead" size (countDvalNodes dv) "ok" sample
    with :? System.OutOfMemoryException ->
      mkResult "fileRead" size -1 "oom" { allocBytes = -1L; elapsedMs = -1L })


let private httpBody (state : RT.ExecutionState) : List<Result> =
  [ 100_000; 1_000_000; 10_000_000 ]
  |> List.map (fun size ->
    let payload = randomBytes 0 size
    let handler =
      { new System.Net.Http.HttpMessageHandler() with
          member _.SendAsync(_req, _ct) =
            let resp =
              new System.Net.Http.HttpResponseMessage(System.Net.HttpStatusCode.OK)
            resp.Content <- new System.Net.Http.ByteArrayContent(payload)
            System.Threading.Tasks.Task.FromResult(resp) }
    use client = new System.Net.Http.HttpClient(handler)
    let dv, sample =
      measure (fun () ->
        let resp =
          client.GetAsync("http://fake.local/body").GetAwaiter().GetResult()
        let bytes = resp.Content.ReadAsByteArrayAsync().GetAwaiter().GetResult()
        Blob.newEphemeral state bytes)
    mkResult "httpBody" size (countDvalNodes dv) "simulated-handler" sample)


let private hexEncode (state : RT.ExecutionState) : List<Result> =
  // Mirror Builtin.blobToHex: mint blob, deref bytes, hex-encode.
  let size = 1_000_000
  let raw = randomBytes 0 size
  let _, sample =
    measure (fun () ->
      let _ = Blob.newEphemeral state raw
      System.Convert.ToHexString(raw))
  [ mkResult "hexEncode" size 1 "ok" sample ]


let private base64Encode (state : RT.ExecutionState) : List<Result> =
  let size = 1_000_000
  let raw = randomBytes 0 size
  let _, sample =
    measure (fun () ->
      let _ = Blob.newEphemeral state raw
      System.Convert.ToBase64String(raw))
  [ mkResult "base64Encode" size 1 "ok" sample ]


/// Many small ephemeral blobs in one scope, each with distinct bytes.
/// Stresses the ConcurrentDictionary/GUID-gen/Dval-wrapper hot path
/// that fileRead amortises away.
let private manyBlobs (state : RT.ExecutionState) : List<Result> =
  let rng = System.Random(1)
  [ 100, 1_024; 1_000, 1_024; 10_000, 256 ]
  |> List.map (fun (count, perBlobBytes) ->
    let payloads =
      Array.init count (fun _ ->
        let p = Array.zeroCreate<byte> perBlobBytes
        rng.NextBytes(p)
        p)
    let _, sample =
      measure (fun () ->
        for i in 0 .. count - 1 do
          ignore<RT.Dval> (Blob.newEphemeral state payloads[i]))
    mkResult
      $"manyBlobs/{count}x{perBlobBytes}B"
      (count * perBlobBytes)
      count
      "intra-scope; per-blob overhead amortised over count"
      sample)


/// Drain a Stream<UInt8> into a Blob via the chunked path — the bulk
/// path that pulls 64 KB buffers instead of boxing one DUInt8 per byte.
let private streamToBlob (state : RT.ExecutionState) : List<Result> =
  [ 100_000; 1_000_000; 10_000_000 ]
  |> List.map (fun size ->
    let payload = randomBytes 0 size
    let _, sample =
      measure (fun () ->
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
        ignore<RT.Dval> (Blob.newEphemeral state (collected.ToArray())))
    mkResult "streamToBlob" size 1 "chunked-drain" sample)


/// Multipart-style body construction: build a Blob by concatenating
/// many text/binary parts. Mirrors the openai/audio.dark
/// MultipartRequest pattern.
let private multipart (state : RT.ExecutionState) : List<Result> =
  [ 10, 1_024; 100, 10_240; 50, 100_000 ]
  |> List.map (fun (parts, perPartBytes) ->
    let part = randomBytes 0 perPartBytes
    let _, sample =
      measure (fun () ->
        use buf = new System.IO.MemoryStream()
        for _ in 1..parts do
          let _ = Blob.newEphemeral state part
          buf.Write(part, 0, part.Length)
        ignore<RT.Dval> (Blob.newEphemeral state (buf.ToArray())))
    mkResult
      $"multipart/{parts}x{perPartBytes}B"
      (parts * perPartBytes)
      (parts + 1)
      "concat"
      sample)


/// Run every scenario; concatenate their Result rows.
let run (state : RT.ExecutionState) : List<Result> =
  [ yield! fileRead state
    yield! httpBody state
    yield! hexEncode state
    yield! base64Encode state
    yield! manyBlobs state
    yield! streamToBlob state
    yield! multipart state ]
