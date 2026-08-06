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


/// Reads and parses all .dark files in `packages` dir,
/// failing upon any individual failure
let load (builtins : RT.Builtins) : Ply<List<PT.PackageOp>> =
  uply {
    let filesWithContents =
      "/home/dark/app/packages"
      |> listDirectoryRecursive
      |> List.filter (String.contains "_" >> not)
      |> List.filter (fun x -> x |> String.endsWith ".dark")
      |> List.map (fun fileName -> (fileName, System.IO.File.ReadAllText fileName))

    // -- Phase 1: Initial parse (unresolved names allowed) --
    let fileCount = List.length filesWithContents
    debuG "phase 1" $"parsing {fileCount} files (unresolved names allowed)"
    let! (firstPassOps : List<PT.PackageOp>) =
      filesWithContents
      // TODO: parallelize
      |> Ply.List.mapSequentially (fun (path, contents) ->
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
      |> Ply.map List.flatten
    debuG "phase 1" $"done, {List.length firstPassOps} ops"

    // -- Phase 2: Iterative re-parse until hashes converge --
    // Each pass resolves names using the previous pass's PM, then
    // remaps Set*Name IDs and computes SCC-aware hashes.
    // Typically converges in 2-3 passes. Cap at 50.
    debuG "phase 2" "starting hash convergence loop"
    let mutable currentOps = HS.computeRealHashes firstPassOps
    let mutable prevHashes = []
    let mutable converged = false
    let mutable iteration = 0
    while not converged && iteration < 50 do
      iteration <- iteration + 1
      debuG
        $"  pass {iteration}"
        $"re-parsing {fileCount} files, resolving names, computing hashes"
      let pm = LibDB.PackageManager.withExtraOps PT.PackageManager.empty currentOps
      let! newRawOps =
        filesWithContents
        |> Ply.List.mapSequentially (fun (path, contents) ->
          parsePackageFile builtins pm NR.OnMissing.ThrowError path contents)
        |> Ply.map List.flatten
      let remapped = HS.remapSetNames newRawOps currentOps
      let newOps = HS.computeRealHashes remapped
      let newHashes = HS.extractAllHashes newOps
      converged <- newHashes = prevHashes
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
              Some(target.hash, target.kind.toString (), PackageLocation.toFQN loc)
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
          debuG $"    changed {i}" $"{kind} {name}: {prevShort}... -> {nextShort}..."
      debuG
        $"  pass {iteration} result"
        $"converged={converged}, hashCount={List.length newHashes}"
      prevHashes <- newHashes
      currentOps <- newOps

    debuG "phase 2" $"converged after {iteration} passes"
    return currentOps
  }
