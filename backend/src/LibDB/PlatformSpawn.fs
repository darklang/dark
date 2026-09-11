/// Running a platform that lives in another process.
///
/// The wire, proven by the spike before any of this was written: a four-byte little-endian length
/// and then that many bytes, both directions. A request is a varint builtin index, a varint
/// argument count, and each argument as a `Dval`. A response is a status byte and a `Dval`.
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
      mutable running : Option<Running>
      startGate : obj }

let handleFor (platformName : string) (executable : string) : Handle =
  { executable = executable
    platformName = platformName
    running = None
    startGate = obj () }

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
  (args : List<RT.Dval>)
  : Result<RT.Dval, string> =
  lock running.gate (fun () ->
    try
      use body = new MemoryStream()
      use bw = new BinaryWriter(body)
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
        let status = br.ReadByte()
        let dval = DvalWire.readDval br
        if status = 0uy then
          Ok dval
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
  fun name args ->
    uply {
      match ensureRunning handle with
      | Error e -> return RT.RuntimeError.UncaughtException(e, []) |> RT.raiseUntargetedRTE
      | Ok running ->
        match call handle running name args with
        | Ok dval -> return dval
        | Error e ->
          // A broken pipe leaves the process useless, so drop it. The next call starts a fresh one
          // rather than talking into a socket nobody is holding.
          stop handle
          return RT.RuntimeError.UncaughtException(e, []) |> RT.raiseUntargetedRTE
    }
