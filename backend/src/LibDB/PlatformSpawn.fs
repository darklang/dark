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
/// How long to wait for a platform to answer before deciding it never will.
///
/// NOT a service level. It is deliberately far longer than any call should take, because its only
/// job is to break a deadlock: a platform that crashes closes its pipe and is handled, and a
/// platform that reads the request and goes silent used to block the caller forever. A number
/// short enough to be a useful SLA would also cancel legitimate slow work, and the runtime has no
/// way to tell a slow platform from a stuck one.
[<Literal>]
let defaultDeadlineMs = 120_000

/// The largest frame this host will read from a platform.
///
/// The length prefix is chosen by the PLATFORM, and everything after it is allocated on its say
/// so. Unbounded, a platform announcing two gigabytes gets two gigabytes allocated before anybody
/// checks what it actually sent, and a NEGATIVE length is worse than large: it is not a size at
/// all, and what happens next is whatever the reader does with nonsense.
///
/// Sixty-four megabytes is far above any sensible answer and far below anything that hurts. A
/// platform with more to say than this should be handing back a blob reference, which is what the
/// table is for.
[<Literal>]
let maxFrameBytes = 64 * 1024 * 1024

type Handle =
  private
    { plan : PlatformSandbox.Plan
      platformName : string
      types : List<string * RT.FQTypeName.FQTypeName>
      deadlineMs : int
      mutable running : Option<Running>
      startGate : obj }

/// The effects are not decoration here: they decide how the process is CONFINED, so a platform
/// that never mentioned the network is started somewhere it does not have one.
let handleFor
  (platformName : string)
  (executable : string)
  (effects : Set<LibExecution.Effects.Effect>)
  (types : List<string * RT.FQTypeName.FQTypeName>)
  : Handle =
  { plan = PlatformSandbox.plan effects executable
    platformName = platformName
    types = types
    deadlineMs = defaultDeadlineMs
    running = None
    startGate = obj () }

/// The same handle with a different deadline. For tests, which should not wait two minutes to
/// prove that waiting ends.
let withDeadline (ms : int) (handle : Handle) : Handle = { handle with deadlineMs = ms }

/// One sentence about how confined this platform is, for a person reading about it.
let confinement (handle : Handle) : string = handle.plan.confinement

/// What this side speaks. A plugin that answers with a different number is refused at startup
/// rather than allowed to produce frames neither side can read.
[<Literal>]
let protocolVersion = 1

/// Read one framed response, or give up.
///
/// The read itself is blocking and cannot be cancelled: it is a pipe, and on Unix a pending read
/// on one does not observe a token. So the wait happens on a task, and when the deadline passes the
/// PROCESS is killed, which closes the pipe and lets the stranded read fail on its own rather than
/// leaking a thread for the life of the CLI.
///
/// Killing is the whole remedy. A platform that has not answered in this long has nothing left to
/// say that anybody is still waiting for, and the handle is dropped, so the next call starts a
/// fresh process.
let private readFramed
  (handle : Handle)
  (running : Running)
  : Result<byte[], string> =
  let read =
    System.Threading.Tasks.Task.Run(fun () ->
      let length = running.reader.ReadInt32()
      // Checked BEFORE it is used to allocate. The platform chose this number and it is under no
      // obligation to have chosen a sane one.
      if length < 0 then
        Error $"announced a frame of {length} bytes, which is not a size"
      elif length > maxFrameBytes then
        Error
          $"announced a frame of {length} bytes, past the {maxFrameBytes / (1024 * 1024)}MB limit"
      else
        // `ReadBytes` stops at end of stream without complaining, so a platform that dies
        // mid-answer returns a short buffer rather than raising. Compared here, where the promised
        // length is still in hand.
        let body = running.reader.ReadBytes length
        if body.Length = length then Ok body else Error "stopped mid-answer")
  if read.Wait handle.deadlineMs then
    match read.Result with
    | Ok response -> Ok response
    | Error why -> Error $"the {handle.platformName} platform {why}"
  else
    try
      running.proc.Kill()
    with _ ->
      ()
    Error
      $"the {handle.platformName} platform did not answer within {handle.deadlineMs / 1000} seconds, so it was stopped"

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

    match readFramed handle running with
    | Error e -> Error e
    | Ok response ->

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

/// One line a platform wrote to its stderr, made safe to show and impossible to mistake for ours.
///
/// Two things happen to it. Control characters go, which kills ANSI escapes: a platform that can
/// paint the terminal can clear it, recolour it, or draw something that looks like a prompt from
/// this program. And it is PREFIXED with the platform's name, so nothing it writes can be read as
/// coming from Dark.
///
/// Not discarded, which was the other option. A platform author debugging a plugin has nowhere
/// else to look, and silence would be paid for by exactly the person the feature is for.
let private escapeSequence =
  // ESC, then a CSI or OSC body, then its final byte. Removed WHOLE rather than by dropping the
  // ESC alone: stripping just the escape byte defangs the sequence and leaves its tail behind as
  // literal text, so a coloured line arrives as `[31mhello[0m` and reads like a bug in this code.
  System.Text.RegularExpressions.Regex(
    @"\u001b(?:\[[0-9;?]*[ -/]*[@-~]|\][^\u0007\u001b]*(?:\u0007|\u001b\\)|[@-Z\\-_])",
    System.Text.RegularExpressions.RegexOptions.Compiled
  )

/// Make one line a platform wrote safe to show.
///
/// Escape sequences go whole, then any remaining control character, so nothing a platform writes
/// can move the cursor, recolour the terminal, clear the screen, or draw something that looks like
/// this program asking a question. Tabs survive, because a plugin's diagnostics are often columns.
let sanitizeDiagnostic (line : string) : string =
  escapeSequence.Replace(line, "")
  |> String.filter (fun c -> c = '\t' || not (System.Char.IsControl c))

let private reportDiagnostic (platformName : string) (line : string) : unit =
  if not (isNull line) then
    eprintfn $"[{platformName}] {sanitizeDiagnostic line}"

let private start (handle : Handle) : Result<Running, string> =
  try
    let psi = ProcessStartInfo(handle.plan.executable)
    for arg in handle.plan.arguments do
      psi.ArgumentList.Add arg
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    // Redirected so it cannot reach the terminal unattributed. A platform declaring no `stdout`
    // could still write there, because stderr was never ours to begin with: the effect system
    // describes what its BUILTINS may do, and a process has a file descriptor either way.
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false
    let proc = Process.Start psi
    proc.ErrorDataReceived.Add(fun args ->
      reportDiagnostic handle.platformName args.Data)
    proc.BeginErrorReadLine()
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
/// A plugin can fail in three ways and each has to look like a value, not like a hang or an
/// internal exception:
///
///   - it CRASHES, which closes the pipe, and the read ends early
///   - it dies MID-ANSWER, which returns a short buffer without raising
///   - it goes SILENT, alive and never answering, which is the one that used to block forever
///
/// The first was found by the spike. The third was found by writing a platform that sleeps: a
/// crash is loud and a hang is not, so the case that never announces itself is the one to build a
/// deadline for.
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

      match readFramed handle running with
      | Error e -> Error e
      | Ok response ->

      use rs = new MemoryStream(response)
      use br = new BinaryReader(rs)
      let returned = Wire.readTable br
      let status = br.ReadByte()
      let dval = DvalWire.readDval br
      if status = 0uy then
        // Before anything else looks at it. A well-formed frame can still carry a value that is
        // not data but a handle into this runtime, and the type checker cannot see the
        // difference for all of them.
        match Wire.refuseForgedHandles handle.platformName dval with
        | Error e -> Error e
        | Ok() -> Ok(dval, returned)
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
