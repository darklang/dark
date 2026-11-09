[<RequireQualifiedAccess>]
module LocalExec.LoadPackagesFromDisk

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.ProgramTypes

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module NR = LibParser.NameResolver
module HS = LibDB.HashStabilization
module PackageLocation = LibDB.PackageLocation

open Utils


// A parse failure surfaces loudly rather than being silently masked.
// Every package file is expected to parse cleanly
let private parsePackageFile
  (builtins : RT.Builtins)
  (pm : PT.PackageManager)
  (onMissing : NR.OnMissing)
  (path : string)
  (contents : string)
  : Ply<List<PT.PackageOp>> =
  uply {
    match! LibParser.Package.parse builtins pm onMissing contents with
    | Ok ops -> return ops
    | Error diagnostics ->
      let rendered = String.concat "\n" diagnostics
      return
        Exception.raiseInternal
          $"Failed to parse package file {path}:\n{rendered}"
          [ "path", path ]
  }


/// How many files are parsed at once. Past eight the gain is small: on a quiet
/// 48-core box the parse took 9.6s one at a time, 4.2s at 4, 3.0s at 8 and 2.6s
/// at 16. A reload often shares the box with a build or a test run.
let private parseWorkers : int = min 8 System.Environment.ProcessorCount

/// <param parseOne> over every file, `parseWorkers` at a time on the thread
/// pool. The ops come back in file order, so they and their hashes are the same
/// as a one-by-one parse. Nothing is shared between files: the parser keeps only
/// local state, and the PM it reads is never written after it is built.
let private parseAll
  (parseOne : string -> string -> Ply<List<PT.PackageOp>>)
  (files : List<string * string>)
  : Task<List<PT.PackageOp>> =
  files
  |> Task.mapWithConcurrency parseWorkers (fun (path, contents) ->
    System.Threading.Tasks.Task.Run<List<PT.PackageOp>>(fun () ->
      parseOne path contents |> Ply.toTask))
  |> Task.map List.flatten


/// Every package source a reload reads, as (path, contents).
let files () : List<string * string> =
  System.IO.Path.Combine(
    LibConfig.ConfigDsl.absoluteDir "DARK_CONFIG_ROOT_DIR",
    "packages"
  )
  |> listDirectoryRecursive
  |> List.filter (String.contains "_" >> not)
  |> List.filter (fun x -> x |> String.endsWith ".dark")
  |> List.map (fun fileName -> (fileName, System.IO.File.ReadAllText fileName))


/// Parse <param filesWithContents> (`files`) and fail on parse or name-resolution errors.
let load
  (builtins : RT.Builtins)
  (filesWithContents : List<string * string>)
  : Ply<List<PT.PackageOp>> =
  uply {

    // -- Phase 1: Initial parse (unresolved names allowed) --
    let fileCount = List.length filesWithContents
    debuG "phase 1" $"parsing {fileCount} files (unresolved names allowed)"
    let parseTimer = System.Diagnostics.Stopwatch()
    parseTimer.Start()
    let! (firstPassOps : List<PT.PackageOp>) =
      filesWithContents
      |> parseAll (fun path contents ->
        try
          debuG "  parsing" path
          parsePackageFile
            builtins
            PT.PackageManager.empty
            NR.OnMissing.Allow
            path
            contents
        with _ex ->
          debuG "  FAILED to parse" path
          reraise ())
    parseTimer.Stop()
    debuG "phase 1" $"done, {List.length firstPassOps} ops"

    // -- Phase 2: Iterative re-parse until hashes converge --
    // Each pass resolves names using the previous pass's PM, then
    // remaps Set*Name IDs and computes SCC-aware hashes.
    // Typically converges in 2-3 passes. Cap at 50.
    debuG "phase 2" "starting hash convergence loop"
    let rec converge
      (iteration : int)
      (currentOps : List<PT.PackageOp>)
      (prevHashes : List<Hash>)
      : Ply<int * List<PT.PackageOp>> =
      uply {
        if iteration >= 50 then
          return (iteration, currentOps)
        else
          let iteration = iteration + 1
          debuG
            $"  pass {iteration}"
            $"re-parsing {fileCount} files, resolving names, computing hashes"
          let pm =
            LibDB.PackageManager.withExtraOps PT.PackageManager.empty currentOps
          parseTimer.Start()
          let! newRawOps =
            filesWithContents
            |> parseAll (parsePackageFile builtins pm NR.OnMissing.ThrowError)
          parseTimer.Stop()
          let remapped = HS.remapSetNames newRawOps currentOps
          let newOps = HS.computeRealHashes remapped
          let newHashes = HS.extractAllHashes newOps
          let converged = newHashes = prevHashes
          if not converged && prevHashes <> [] then
            let prevSet = prevHashes |> Set.ofList
            let newSet = newHashes |> Set.ofList
            let onlyInPrev = Set.difference prevSet newSet |> Set.count
            let onlyInNew = Set.difference newSet prevSet |> Set.count
            let shared = Set.intersect prevSet newSet |> Set.count
            debuG
              $"  pass {iteration} diff"
              $"shared={shared}, onlyInPrev={onlyInPrev}, onlyInNew={onlyInNew}"
            let itemLocs =
              newOps
              |> List.choose (function
                | PT.PackageOp.SetName(loc, target, _) ->
                  Some(
                    target.hash,
                    target.kind.toString (),
                    PackageLocation.toFQN loc
                  )
                | _ -> None)
            let changes =
              List.zip prevHashes newHashes
              |> List.mapi (fun i (prev, next) -> (i, prev, next))
              |> List.filter (fun (_, prev, next) -> prev <> next)
            let firstChanges = changes |> List.truncate 5
            for (i, (Hash prev), (Hash next)) in firstChanges do
              let (_, kind, name) = List.item i itemLocs
              let prevShort = prev.Substring(0, 7)
              let nextShort = next.Substring(0, 7)
              debuG
                $"    changed {i}"
                $"{kind} {name}: {prevShort}... -> {nextShort}..."
          debuG
            $"  pass {iteration} result"
            $"converged={converged}, hashCount={List.length newHashes}"
          if converged then
            return (iteration, newOps)
          else
            return! converge iteration newOps newHashes
      }
    let! (iteration, currentOps) = converge 0 (HS.computeRealHashes firstPassOps) []

    debuG "phase 2" $"converged after {iteration} passes"
    let passes = iteration + 1
    let secs = parseTimer.Elapsed.TotalSeconds
    print
      $"Parsed {fileCount} files {passes} times, {parseWorkers} at once: {secs:F1}s"
    return currentOps
  }
