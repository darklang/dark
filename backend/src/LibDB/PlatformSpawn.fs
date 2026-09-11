/// Running a platform that lives in another process.
///
/// The wire, proven by the spike before any of this was written: a four-byte little-endian length
/// and then that many bytes, both directions. Inside a frame comes `PlatformWire`'s blob table,
/// then the payload: a request is the builtin's name, a varint argument count, and each argument
/// as a `Dval`; a response is a status byte and a `Dval`.
///
/// `Dval` on the wire and not something new, because `Serializers.RT.Dval` already exists, already
/// refuses what cannot cross (streams, ephemeral blobs), and is what a plugin in another language
/// has to implement either way. The encoding it has to speak is small: a tag byte, .NET's 7-bit
/// varint for lengths, little-endian integers.
module LibDB.PlatformSpawn

open System
open System.Diagnostics
open System.IO
open Prelude

module RT = LibExecution.RuntimeTypes
module Platform = LibExecution.Platform
module DvalWire = LibSerialization.Binary.Serializers.RT.Dval
module Wire = LibDB.PlatformWire
module Varint = LibSerialization.Binary.Serializers.Common.Varint

/// One running platform process, and the lock that makes it usable.
///
/// A single pipe pair is not concurrency-safe: two calls in flight interleave their frames and
/// both get nonsense. The lock is per PROCESS rather than global, so two platforms do not block
/// each other, and it is held across the whole request and response rather than around each write.
type private Running =
  { proc : Process
    writer : BinaryWriter
    reader : BinaryReader
    gate : obj }

/// A platform that has been described but not necessarily started.
///
/// Lazy on purpose, and this is the part of the design that makes out-of-process affordable at all:
/// activation already means nothing spawns until code reaches for it, so an installed platform
/// nobody calls costs one record and no process.
type Handle =
  private
    { executable : string
      platformName : string
      types : List<string * RT.FQTypeName.FQTypeName>
      mutable running : Option<Running>
      startGate : obj }

let handleFor
  (platformName : string)
  (executable : string)
  (types : List<string * RT.FQTypeName.FQTypeName>)
  : Handle =
  { executable = executable
    platformName = platformName
    types = types
    running = None
    startGate = obj () }

/// What this side speaks. A plugin that answers with a different number is refused at startup
/// rather than allowed to produce frames neither side can read.
[<Literal>]
let protocolVersion = 1

/// Tell a freshly started platform what it needs to know before the first call.
///
/// One frame out, one frame back. The frame out is the protocol version, the platform's name, and
/// what this instance's store made of every type name the manifest used. The frame back is the
/// version the plugin speaks.
///
/// The type table is the point. A record or an enum on the wire carries the type's CONTENT HASH,
/// and a plugin cannot know a hash: that is the same reason a manifest names types symbolically.
/// Without this a platform can only return primitives, so no platform could return a `Result`.
///
/// A round trip at startup rather than a one-way announcement, because a version mismatch found
/// here is one sentence, and found later is a corrupt frame in the middle of somebody's program.
let private handshake (handle : Handle) (running : Running) : Result<unit, string> =
  try
    use body = new MemoryStream()
    use bw = new BinaryWriter(body)
    Varint.write bw protocolVersion
    LibSerialization.Binary.Serializers.Common.String.write bw handle.platformName
    Varint.write bw (List.length handle.types)
    for (name, typeName) in handle.types do
      LibSerialization.Binary.Serializers.Common.String.write bw name
      let (RT.FQTypeName.Package(RT.Hash hash)) = typeName
      LibSerialization.Binary.Serializers.Common.String.write bw hash
    bw.Flush()
    let bytes = body.ToArray()
    running.writer.Write(int32 bytes.Length)
    running.writer.Write bytes
    running.writer.Flush()

    let length = running.reader.ReadInt32()
    let response = running.reader.ReadBytes length
    use rs = new MemoryStream(response)
    use br = new BinaryReader(rs)
    let theirs = Varint.read br
    if theirs = protocolVersion then
      Ok()
    else
      Error
        $"the {handle.platformName} platform speaks wire version {theirs}, and this build speaks {protocolVersion}"
  with
  | :? EndOfStreamException ->
    Error
      $"the {handle.platformName} platform exited during startup, without saying what it speaks"
  | :? IOException as e ->
    Error $"lost contact with the {handle.platformName} platform at startup: {e.Message}"

let private start (handle : Handle) : Result<Running, string> =
  try
    let psi = ProcessStartInfo(handle.executable)
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    let proc = Process.Start psi
    Ok
      { proc = proc
        writer = new BinaryWriter(proc.StandardInput.BaseStream)
        reader = new BinaryReader(proc.StandardOutput.BaseStream)
        gate = obj () }
  with e ->
    Error $"could not start the {handle.platformName} platform: {e.Message}"

/// Start it if it is not running, or hand back the one that is.
let private ensureRunning (handle : Handle) : Result<Running, string> =
  lock handle.startGate (fun () ->
    match handle.running with
    | Some r when not r.proc.HasExited -> Ok r
    | _ ->
      match start handle with
      | Error e -> Error e
      | Ok r ->
        match handshake handle r with
        | Error e ->
          // Never recorded as running, so the next call starts fresh rather than talking to a
          // process this one has already decided it cannot understand.
          (try r.proc.Kill() with _ -> ())
          Error e
        | Ok() ->
          handle.running <- Some r
          Ok r)

/// Stop it, if it is running. Closing stdin is the shutdown signal: the plugin's read loop ends.
let stop (handle : Handle) : unit =
  lock handle.startGate (fun () ->
    match handle.running with
    | None -> ()
    | Some r ->
      handle.running <- None
      try
        r.proc.StandardInput.Close()
        if not (r.proc.WaitForExit 2000) then r.proc.Kill()
      with _ ->
        ())

/// One call, framed.
///
/// A crashed plugin is a CLOSED PIPE rather than any response, which the spike found and its
/// harness did not handle. That case lives here rather than in the message vocabulary: a read that
/// ends early is an error with the platform's name on it, not a hang and not an internal exception.
let private call
  (handle : Handle)
  (running : Running)
  (name : string)
  (blobs : Wire.Table)
  (args : List<RT.Dval>)
  : Result<RT.Dval * Wire.Table, string> =
  lock running.gate (fun () ->
    try
      use body = new MemoryStream()
      use bw = new BinaryWriter(body)
      Wire.writeTable bw blobs
      LibSerialization.Binary.Serializers.Common.String.write bw name
      Varint.write bw (List.length args)
      for arg in args do
        DvalWire.writeDval bw arg
      bw.Flush()
      let bytes = body.ToArray()
      running.writer.Write(int32 bytes.Length)
      running.writer.Write bytes
      running.writer.Flush()

      let length = running.reader.ReadInt32()
      let response = running.reader.ReadBytes length
      if response.Length <> length then
        Error $"the {handle.platformName} platform stopped mid-answer"
      else
        use rs = new MemoryStream(response)
        use br = new BinaryReader(rs)
        let returned = Wire.readTable br
        let status = br.ReadByte()
        let dval = DvalWire.readDval br
        if status = 0uy then
          Ok(dval, returned)
        else
          match dval with
          | RT.DString message -> Error $"{handle.platformName}: {message}"
          | other -> Error $"{handle.platformName} failed: {other}"
    with
    | :? EndOfStreamException ->
      // The pipe closed. Nothing is coming, so say so rather than waiting for it.
      Error
        $"the {handle.platformName} platform exited without answering. It may have crashed."
    | :? IOException as e ->
      Error $"lost contact with the {handle.platformName} platform: {e.Message}")

/// The `Invoke` a described platform runs on.
///
/// Errors come back as a Dark runtime error rather than an F# exception, because a platform
/// falling over is an ordinary thing for a program to see: it is somebody else's executable and it
/// is allowed to be broken.
let invoke (handle : Handle) : Platform.External.Invoke =
  fun state name args ->
    uply {
      match ensureRunning handle with
      | Error e -> return RT.RuntimeError.UncaughtException(e, []) |> RT.raiseUntargetedRTE
      | Ok running ->
        // Bytes leave the arguments here and travel beside them, because the at-rest encoding has
        // nowhere to put an ephemeral blob and the far side has no store to look a reference up in.
        let! (blobs, args) = Wire.collect state args
        match call handle running name blobs args with
        | Ok(dval, returned) -> return! Wire.rehydrate returned dval
        | Error e ->
          // A broken pipe leaves the process useless, so drop it. The next call starts a fresh one
          // rather than talking into a socket nobody is holding.
          stop handle
          return RT.RuntimeError.UncaughtException(e, []) |> RT.raiseUntargetedRTE
    }
