[<RequireQualifiedAccess>]
module LocalExec.LoadPackagesFromDisk

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module NR = LibParser.NameResolver
module HS = LibPackageManager.HashStabilization

open Utils


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

    // First pass, parse all the packages, allowing unresolved names
    // (other package items won't be available yet)
    let! (firstPassOps : List<PT.PackageOp>) =
      filesWithContents
      // TODO: parallelize
      |> Ply.List.mapSequentially (fun (path, contents) ->
        try
          debuG "about to parse" path
          LibParser.Parser.parsePackageFile
            builtins
            PT.PackageManager.empty
            NR.OnMissing.Allow
            path
            contents
        with _ex ->
          debuG "failed to parse" path
          reraise ())
      |> Ply.map List.flatten

    // Iteratively re-parse until all content hashes converge.
    // Each pass resolves names using the previous pass's PM.
    // After parsing, remapSetNames aligns the Set*Name IDs with the PM's
    // hashes (so the dependency graph is correctly connected), then
    // computeRealHashes computes SCC-aware content hashes.
    // Typically converges in 2-3 passes. Cap at 50.
    let mutable currentOps = HS.computeRealHashes firstPassOps
    let mutable prevHashes = []
    let mutable converged = false
    let mutable iteration = 0
    while not converged && iteration < 50 do
      iteration <- iteration + 1
      let pm =
        LibPackageManager.PackageManager.withExtraOps
          PT.PackageManager.empty
          currentOps
      let! newRawOps =
        filesWithContents
        |> Ply.List.mapSequentially (fun (path, contents) ->
          LibParser.Parser.parsePackageFile
            builtins
            pm
            NR.OnMissing.ThrowError
            path
            contents)
        |> Ply.map List.flatten
      // Remap Set*Name IDs to match the PM (previous iteration's hashes),
      // then compute real content hashes using SCC-aware hashing
      let remapped = HS.remapSetNames newRawOps currentOps
      let newOps = HS.computeRealHashes remapped
      // Extract all content hashes from Set*Name ops for convergence check.
      let newHashes = HS.extractAllHashes newOps
      converged <- newHashes = prevHashes
      // Detailed diff logging for convergence debugging
      if not converged && prevHashes <> [] then
        let prevSet = prevHashes |> Set.ofList
        let newSet = newHashes |> Set.ofList
        let onlyInPrev = Set.difference prevSet newSet |> Set.count
        let onlyInNew = Set.difference newSet prevSet |> Set.count
        let shared = Set.intersect prevSet newSet |> Set.count
        debuG
          $"iteration {iteration} diff"
          $"shared={shared}, onlyInPrev={onlyInPrev}, onlyInNew={onlyInNew}"
        // Log first few changed items with their locations
        let itemLocs =
          newOps
          |> List.choose (function
            | PT.PackageOp.SetTypeName(hash, loc) ->
              let mods = String.concat "." loc.modules
              Some(hash, "type", $"{loc.owner}.{mods}.{loc.name}")
            | PT.PackageOp.SetFnName(hash, loc) ->
              let mods = String.concat "." loc.modules
              Some(hash, "fn", $"{loc.owner}.{mods}.{loc.name}")
            | PT.PackageOp.SetValueName(hash, loc) ->
              let mods = String.concat "." loc.modules
              Some(hash, "value", $"{loc.owner}.{mods}.{loc.name}")
            | _ -> None)
        // Find items whose hash changed from prev iteration
        let changes =
          List.zip prevHashes newHashes
          |> List.mapi (fun i (prev, next) -> (i, prev, next))
          |> List.filter (fun (_, prev, next) -> prev <> next)
        let firstChanges = changes |> List.truncate 5
        for (i, (ContentHash prev), (ContentHash next)) in firstChanges do
          let (_, kind, name) = List.item i itemLocs
          let prevShort = prev.Substring(0, 7)
          let nextShort = next.Substring(0, 7)
          debuG $"  changed {i}" $"{kind} {name}: {prevShort}... -> {nextShort}..."
      debuG
        $"iteration {iteration}"
        $"converged={converged}, hashCount={List.length newHashes}"
      prevHashes <- newHashes
      currentOps <- newOps

    return currentOps
  }
