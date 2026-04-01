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

/// Get monotonic timestamp in microseconds.
let private nowUs () : int64 =
  Stopwatch.GetTimestamp() * 1_000_000L / Stopwatch.Frequency

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

/// Log a point-in-time event (no duration).
let event (name : string) (ctx : (string * string) list) : unit =
  if logPath <> "" then
    let wall = wallClock ()
    let ctxJson = formatCtx ctx
    writeLine
      $"{{\"event\":\"{jsonEscape name}\",\"wall\":\"{wall}\",\"ctx\":{ctxJson}}}"

/// A span that measures elapsed time and logs on Dispose.
type Span(name : string, ctx : (string * string) list) =
  let startUs = nowUs ()
  let wall = wallClock ()

  interface System.IDisposable with
    member _.Dispose() =
      if logPath <> "" then
        let elapsedUs = nowUs () - startUs
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
