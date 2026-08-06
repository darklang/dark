/// Lightweight telemetry for profiling CLI startup and runtime.
/// Writes JSON-lines to a log file. Each line is one span or event.
///
/// Usage:
///   use _span = Telemetry.span "phase" [("key", "value")]
///   // ... work ...
///   // span is closed and logged on Dispose
///
///   Telemetry.event "marker" [("key", "value")]
///
/// The output format matches the Dark-side Telemetry module so both
/// F# and Dark traces appear in the same file and can be analyzed together.
module Telemetry

open System.Diagnostics

/// Global mutable log path. Set early in startup.
/// Empty string = telemetry disabled.
let mutable private logPath : string = ""

/// Set the telemetry output file path. Call once at startup.
let init (path : string) : unit =
  logPath <- path
  // Ensure directory exists
  let dir = System.IO.Path.GetDirectoryName(path)
  if dir <> "" && not (System.IO.Directory.Exists(dir)) then
    System.IO.Directory.CreateDirectory(dir) |> ignore<System.IO.DirectoryInfo>

let isEnabled () : bool = logPath <> ""

/// Write a raw JSON line to the telemetry log (thread-safe via lock).
let private writeLock = obj ()

let private writeLine (line : string) : unit =
  if logPath <> "" then
    lock writeLock (fun () ->
      try
        System.IO.File.AppendAllText(logPath, line + "\n")
      with _ ->
        ())

/// Convert a Stopwatch-tick DELTA to microseconds.
///
/// Deltas, never absolute timestamps. `GetTimestamp() * 1_000_000L` overflows int64 once the tick count
/// passes ~9.2e12, which on a 1 GHz-tick clocksource is **2.56 hours of uptime**. Past that the product
/// wraps, and any span whose start and end straddle a wrap boundary records a wildly wrong duration,
/// including large negative ones. A delta is small, so scaling it is safe: an hour-long span is 3.6e18
/// after scaling, still inside int64.
let private toUs (ticks : int64) : int64 = ticks * 1_000_000L / Stopwatch.Frequency

/// Get wall-clock ISO 8601 timestamp.
let private wallClock () : string =
  System.DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")

/// Escape a string for JSON.
let private jsonEscape (s : string) : string =
  s.Replace("\\", "\\\\").Replace("\"", "\\\"").Replace("\n", "\\n")

/// Format context pairs as JSON object string.
let private formatCtx (ctx : (string * string) list) : string =
  match ctx with
  | [] -> "{}"
  | pairs ->
    pairs
    |> List.map (fun (k, v) -> $"\"{jsonEscape k}\":\"{jsonEscape v}\"")
    |> String.concat ","
    |> fun s -> "{" + s + "}"

/// Counters for things that happen many times per run, where a span each would cost more than the thing
/// being measured and would drown the log. Read them at exit with `counterSnapshot`.
///
/// Here to answer one question: how many package items does a single command actually decode? "The CLI is
/// slow because it loads its package closure" is only actionable once you know whether that closure is
/// hundreds of items or thousands.
let private counters =
  System.Collections.Concurrent.ConcurrentDictionary<string, int64 ref>()

let count (name : string) : unit =
  if logPath <> "" then
    let r = counters.GetOrAdd(name, (fun _ -> ref 0L))
    System.Threading.Interlocked.Increment(r) |> ignore<int64>

/// Accumulated microseconds for a repeated operation. Same reasoning as `count`: a span per call would cost
/// more than the call. Pair them -- a count without a total can't tell you whether 96 loads are the problem.
let private timers =
  System.Collections.Concurrent.ConcurrentDictionary<string, int64 ref>()

let addUs (name : string) (us : int64) : unit =
  if logPath <> "" then
    let r = timers.GetOrAdd(name, (fun _ -> ref 0L))
    System.Threading.Interlocked.Add(r, us) |> ignore<int64>

let timerSnapshot () : (string * int64) list =
  timers
  |> Seq.map (fun kv -> kv.Key, kv.Value.Value)
  |> Seq.sortBy fst
  |> Seq.toList


let counterSnapshot () : (string * int64) list =
  counters
  |> Seq.map (fun kv -> kv.Key, kv.Value.Value)
  |> Seq.sortBy fst
  |> Seq.toList


/// Log a point-in-time event (no duration).
let event (name : string) (ctx : (string * string) list) : unit =
  if logPath <> "" then
    let wall = wallClock ()
    let ctxJson = formatCtx ctx
    writeLine
      $"{{\"event\":\"{jsonEscape name}\",\"wall\":\"{wall}\",\"ctx\":{ctxJson}}}"

/// A span that measures elapsed time and logs on Dispose.
type Span(name : string, ctx : (string * string) list) =
  let startTicks = Stopwatch.GetTimestamp()
  let wall = wallClock ()

  interface System.IDisposable with
    member _.Dispose() =
      if logPath <> "" then
        let elapsedUs = toUs (Stopwatch.GetTimestamp() - startTicks)
        let ms = elapsedUs / 1000L
        let ctxJson = formatCtx ctx
        writeLine
          $"{{\"event\":\"{jsonEscape name}\",\"ms\":{ms},\"us\":{elapsedUs},\"wall\":\"{wall}\",\"ctx\":{ctxJson}}}"

/// Create a span that logs its duration when disposed.
/// Use with `use`: `use _s = Telemetry.span "name" []`
let span (name : string) (ctx : (string * string) list) : Span = new Span(name, ctx)

/// Convenience: time a synchronous function and log the result.
let time (name : string) (ctx : (string * string) list) (f : unit -> 'a) : 'a =
  use _s = span name ctx
  f ()

/// Convenience: time an async task and log the result.
let timeTask
  (name : string)
  (ctx : (string * string) list)
  (f : unit -> System.Threading.Tasks.Task<'a>)
  : System.Threading.Tasks.Task<'a> =
  task {
    use _s = span name ctx
    return! f ()
  }
