/// Allocation/timing benchmarks — bench command + render command.
///
/// Two subcommands:
///   `bench`        — run every scenario; append a snapshot to the
///                    tracked `benchmarks/results/history.jsonl`.
///                    Also writes a local-only `results/latest.json`
///                    (gitignored) for piping into other tools.
///   `bench-render` — read `history.jsonl` and rewrite
///                    `benchmarks/results.md`. Latest run is the
///                    headline; older runs become a per-run table.
///
/// Local hacking: drop a snapshot in
/// `benchmarks/results/local-*.json` (gitignored) for a machine-local
/// file. There's no `bench-promote` — committing a `bench` run via
/// `history.jsonl` is the promotion mechanism.
///
/// Scenarios live in `BenchmarkScenarios.fs`.
module LocalExec.Benchmarks

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module Scenarios = LocalExec.BenchmarkScenarios


// ─── Snapshot I/O ────────────────────────────────────────────────

let private resultsDir () : string =
  System.IO.Path.Combine(LibConfig.Config.runDir, "..", "benchmarks", "results")
  |> System.IO.Path.GetFullPath


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


let private jsonEscape (s : string) : string =
  s.Replace("\\", "\\\\").Replace("\"", "\\\"")

let private serializeResult (r : Scenarios.Result) : string =
  $"""    {{ "scenario": "{jsonEscape r.scenario}", "inputBytes": {r.inputBytes}, "allocBytes": {r.allocBytes}, "elapsedMs": {r.elapsedMs}, "dvalNodes": {r.dvalNodes}, "note": "{jsonEscape r.note}" }}"""


let private serializeSnapshot
  (timestamp : string)
  (commit : string)
  (results : List<Scenarios.Result>)
  : string =
  let resultsJson = results |> List.map serializeResult |> String.concat ",\n"
  $"""{{
  "timestamp": "{timestamp}",
  "commit": "{commit}",
  "results": [
{resultsJson}
  ]
}}
"""


let private parseSnapshot
  (json : string)
  : Option<string * string * List<Scenarios.Result>> =
  try
    use doc = System.Text.Json.JsonDocument.Parse(json)
    let root = doc.RootElement
    let timestamp = root.GetProperty("timestamp").GetString()
    let commit = root.GetProperty("commit").GetString()
    let results : List<Scenarios.Result> =
      root.GetProperty("results").EnumerateArray()
      |> Seq.map (fun el ->
        ({ scenario = el.GetProperty("scenario").GetString()
           inputBytes = el.GetProperty("inputBytes").GetInt64()
           allocBytes = el.GetProperty("allocBytes").GetInt64()
           elapsedMs = el.GetProperty("elapsedMs").GetInt64()
           dvalNodes = el.GetProperty("dvalNodes").GetInt32()
           note = el.GetProperty("note").GetString() }
        : Scenarios.Result))
      |> Seq.toList
    Some(timestamp, commit, results)
  with _ ->
    None


// ─── bench command ───────────────────────────────────────────────

let private overhead (alloc : int64) (input : int64) : string =
  if input <= 0L then "-" else $"{(float alloc) / (float input):F2}x"

let private printSummary (results : List<Scenarios.Result>) : unit =
  print "scenario                       input bytes    alloc bytes  overhead   ms"
  print "--------                       -----------    -----------  --------  ---"
  for r in results do
    let line =
      $"{r.scenario.PadRight(30)} {(string r.inputBytes).PadLeft(11)}    "
      + $"{(string r.allocBytes).PadLeft(11)}  "
      + $"{(overhead r.allocBytes r.inputBytes).PadLeft(8)} "
      + $"{(string r.elapsedMs).PadLeft(4)}"
    print line


let runAll () : Ply<Result<unit, string>> =
  uply {
    let results = Scenarios.run (Scenarios.freshState ())
    let timestamp = System.DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ")
    let json = serializeSnapshot timestamp (gitCommit ()) results

    let outDir = resultsDir ()
    System.IO.Directory.CreateDirectory(outDir) |> ignore<System.IO.DirectoryInfo>
    System.IO.File.WriteAllText(System.IO.Path.Combine(outDir, "latest.json"), json)
    System.IO.File.AppendAllText(
      System.IO.Path.Combine(outDir, "history.jsonl"),
      json.Replace("\n", "") + "\n"
    )

    print $"\nWrote {results.Length} results to {outDir}/latest.json"
    printSummary results
    return Ok()
  }


// ─── bench-render command ────────────────────────────────────────

let private formatBytes (n : int64) : string =
  System.String.Format(
    System.Globalization.CultureInfo.InvariantCulture,
    "{0:N0}",
    n
  )


/// Group results by scenario family (prefix before '/'); preserves
/// first-occurrence order for both the family list and the rows.
/// (Prelude's `List.groupBy` returns a Map, which sorts by key.)
let private groupByFamily
  (results : List<Scenarios.Result>)
  : List<string * List<Scenarios.Result>> =
  let dict =
    System.Collections.Generic.Dictionary<string, ResizeArray<Scenarios.Result>>()
  let order = ResizeArray<string>()
  for r in results do
    let group = r.scenario.Split('/')[0]
    if not (dict.ContainsKey(group)) then
      order.Add(group)
      dict[group] <- ResizeArray()
    dict[group].Add(r)
  order |> Seq.map (fun g -> (g, List.ofSeq dict[g])) |> Seq.toList


let private renderSnapshotBody (results : List<Scenarios.Result>) : List<string> =
  groupByFamily results
  |> List.collect (fun (group, rows) ->
    [ $"### {group}"
      ""
      "| scenario | input bytes | alloc bytes | overhead | ms | dval nodes |"
      "| -------- | ----------: | ----------: | -------: | -: | ---------: |"
      yield!
        rows
        |> List.map (fun r ->
          $"| {r.scenario} | {formatBytes r.inputBytes} | {formatBytes r.allocBytes} | "
          + $"{overhead r.allocBytes r.inputBytes} | {r.elapsedMs} | {r.dvalNodes} |")
      "" ])


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
        |> Array.choose parseSnapshot
        |> Array.toList

      match List.tryLast snapshots with
      | None -> return Error $"{historyPath} contained no readable snapshots."
      | Some(latestTs, latestCommit, latestResults) ->
        let header =
          [ "# Benchmark results"
            ""
            "Generated by `./scripts/run-local-exec bench-render`. Don't edit by hand."
            ""
            $"## Latest — {latestTs} @ `{latestCommit.Substring(0, 7)}`"
            "" ]

        let priorRuns = snapshots |> List.take (snapshots.Length - 1)
        let history =
          if priorRuns.IsEmpty then
            []
          else
            [ "## Run history"
              ""
              "| timestamp | commit | rows |"
              "| --------- | ------ | ---: |"
              yield!
                priorRuns
                |> List.rev // newest first
                |> List.map (fun (ts, commit, rs) ->
                  $"| {ts} | `{commit.Substring(0, 7)}` | {rs.Length} |")
              "" ]

        let outPath =
          System.IO.Path.Combine(dir, "..", "results.md")
          |> System.IO.Path.GetFullPath
        let content =
          header @ renderSnapshotBody latestResults @ history |> String.concat "\n"
        System.IO.File.WriteAllText(outPath, content)
        print $"Wrote {outPath}"
        return Ok()
  }
