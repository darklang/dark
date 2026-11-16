/// The clock a run reads and sleeps on: the real one, or a virtual one for tests.
///
/// A virtual clock moves only when the scheduler running on it has nothing else to do and no
/// host operation of the run is still in flight. Then it jumps to the earliest deadline, so a
/// sleep of an hour finishes at once and every reading of the clock is exact.
module LibExecution.HostClock

open System.Threading
open System.Threading.Tasks

open Prelude

/// 100 ns ticks, as `NodaTime` and `TimeSpan` count them.
type Ticks = int64

type VirtualClock(start : Ticks) =
  let sync = obj ()
  // keyed by (deadline, arm order): the order due waiters fire in
  let waiting =
    System.Collections.Generic.SortedDictionary<struct (Ticks * int64), unit -> unit>()
  let mutable now = start
  let monotonicStart = System.Diagnostics.Stopwatch.GetElapsedTime(0L).Ticks
  let mutable armed = 0L
  let mutable inFlight = 0

  /// Unix time.
  member _.Now : Ticks = lock sync (fun () -> now)

  /// The monotonic readings: the real one when the clock started, moved as this clock moves.
  member this.Monotonic : Ticks = monotonicStart + this.Now - start

  /// Call `onDue` once the clock reaches `after` from now, on the thread that advances it.
  /// Disposing first means it never fires.
  member _.Arm(after : Ticks, onDue : unit -> unit) : System.IDisposable =
    let key =
      lock sync (fun () ->
        armed <- armed + 1L
        let key = struct (now + max 0L after, armed)
        waiting.Add(key, onDue)
        key)
    { new System.IDisposable with
        member _.Dispose() = lock sync (fun () -> waiting.Remove key |> ignore<bool>) }

  member this.Delay(after : Ticks) : Task =
    if after <= 0L then
      Task.CompletedTask
    else
      let done' = TaskCompletionSource()
      this.Arm(after, (fun () -> done'.TrySetResult() |> ignore<bool>))
      |> ignore<System.IDisposable>
      done'.Task

  /// A host operation of this run started or landed. The clock stands still while any is out.
  member _.Begin() : unit = Interlocked.Increment &inFlight |> ignore<int>
  member _.End() : unit = Interlocked.Decrement &inFlight |> ignore<int>
  member _.Busy : bool = Volatile.Read &inFlight > 0

  /// Jump to the earliest deadline and fire everything due then, in the order it was armed.
  /// False when nothing waits or a host operation is in flight.
  member this.Advance() : bool =
    let due =
      lock sync (fun () ->
        if this.Busy || waiting.Count = 0 then
          []
        else
          let struct (deadline, _) = (Seq.head waiting).Key
          now <- max now deadline
          let due =
            waiting
            |> Seq.takeWhile (fun kv ->
              let struct (d, _) = kv.Key
              d <= now)
            |> List.ofSeq
          for kv in due do
            waiting.Remove kv.Key |> ignore<bool>
          due |> List.map (fun kv -> kv.Value))
    for onDue in due do
      onDue ()
    not due.IsEmpty

type Clock =
  | Real
  | Virtual of VirtualClock

/// The width of one timespec field: 32-bit ARM's are 4 bytes.
let private field =
  if
    System.Runtime.InteropServices.RuntimeInformation.ProcessArchitecture = System.Runtime.InteropServices.Architecture.Arm
  then
    4
  else
    8

let private pair (high : int64) (low : int64) : byte[] =
  let bytes = Array.zeroCreate<byte> (field * 2)
  let write (at : int) (n : int64) =
    if field = 4 then
      System.Buffers.Binary.BinaryPrimitives.WriteInt32LittleEndian(
        System.Span(bytes, at, 4),
        int32 n
      )
    else
      System.Buffers.Binary.BinaryPrimitives.WriteInt64LittleEndian(
        System.Span(bytes, at, 8),
        n
      )
  write 0 high
  write field low
  bytes

let private timespec (ticks : Ticks) : byte[] =
  pair (ticks / 10_000_000L) (ticks % 10_000_000L * 100L)

/// The clock ids a virtual clock reads for: Linux's. The kernel is asked for any other, so one
/// it does not have fails as it would.
let private clockIds : Set<int> = set [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 11 ]

/// Whether a virtual clock answers this operation itself: a clock read or a sleep.
let answers (op : HostTypes.PosixOp) : bool =
  match op with
  | HostTypes.PosixOp.ClockGettime id -> Set.contains id clockIds
  | HostTypes.PosixOp.Gettimeofday
  | HostTypes.PosixOp.MachAbsoluteTime
  | HostTypes.PosixOp.MachTimebaseInfo
  | HostTypes.PosixOp.Nanosleep _ -> true
  | _ -> false

/// What a virtual clock answers for an operation `answers` takes, in the kernel's shape.
let answer (v : VirtualClock) (op : HostTypes.PosixOp) : Task<HostTypes.Response> =
  let bytes (b : byte[]) = Task.FromResult(HostTypes.Response.Bytes b)
  match op with
  // CLOCK_REALTIME, _COARSE and _ALARM; the rest are monotonic
  | HostTypes.PosixOp.ClockGettime(0 | 5 | 8) -> bytes (timespec v.Now)
  | HostTypes.PosixOp.ClockGettime _ -> bytes (timespec v.Monotonic)
  | HostTypes.PosixOp.Gettimeofday ->
    let ticks = v.Now
    bytes (pair (ticks / 10_000_000L) (ticks % 10_000_000L / 10L))
  | HostTypes.PosixOp.MachAbsoluteTime ->
    bytes (System.BitConverter.GetBytes(v.Monotonic * 100L))
  | HostTypes.PosixOp.MachTimebaseInfo ->
    bytes (
      Array.append (System.BitConverter.GetBytes 1) (System.BitConverter.GetBytes 1)
    )
  | HostTypes.PosixOp.Nanosleep(seconds, nanos) ->
    let remaining = Array.zeroCreate<byte> (field * 2)
    if seconds < 0L || nanos < 0L || nanos >= 1_000_000_000L then
      Task.FromResult(HostTypes.Response.KernelSleep(-22, remaining))
    else
      task {
        do! v.Delay(seconds * 10_000_000L + nanos / 100L)
        return HostTypes.Response.KernelSleep(0, remaining)
      }
  | _ -> Exception.raiseInternal "not a clock operation" []

/// A virtual clock that starts at `start`.
let startingAt (start : NodaTime.Instant) : Clock =
  Virtual(VirtualClock(start.ToUnixTimeTicks()))
