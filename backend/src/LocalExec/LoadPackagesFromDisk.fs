[<RequireQualifiedAccess>]
module LocalExec.LoadPackagesFromDisk

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module NR = LibParser.NameResolver

open Utils

/// Result type for incremental loading
type LoadResult =
  | NoChanges
  | ParsedOps of List<PT.PackageOp>

/// Package directory path
let packageDir = "/home/dark/app/packages"

/// Full load - parses all files (original behavior)
let private loadFullInternal
  (builtins : RT.Builtins)
  (filesWithContents : List<string * string>)
  : Ply<List<PT.PackageOp>> =
  uply {
    let accountID = None
    let branchId = None

    // First pass, parse all the packages in parallel, allowing unresolved names
    // (other package items won't be available yet)
    let! (firstPassOps : List<PT.PackageOp>) =
      filesWithContents
      |> Ply.List.mapWithConcurrency 8 (fun (path, contents) ->
        try
          debuG "about to parse" path
          LibParser.Parser.parsePackageFile
            accountID
            branchId
            builtins
            PT.PackageManager.empty
            NR.OnMissing.Allow
            path
            contents
        with _ex ->
          debuG "failed to parse" path
          reraise ())
      |> Ply.map List.flatten

    // Re-parse the packages in parallel, this time with strict name resolution
    // (any package references that may have been unresolved a second ago should now be OK)
    let secondPassPM =
      LibPackageManager.PackageManager.withExtraOps PT.PackageManager.empty firstPassOps

    let! reParsedOps =
      filesWithContents
      |> Ply.List.mapWithConcurrency 8 (fun (path, contents) ->
        LibParser.Parser.parsePackageFile
          accountID
          branchId
          builtins
          secondPassPM
          NR.OnMissing.ThrowError
          path
          contents)
      |> Ply.map List.flatten

    // Build PM from first pass for ID stabilization
    let firstPassPM = LibPackageManager.PackageManager.createInMemory firstPassOps

    // Adjust IDs in second pass to match first pass (ID stabilization)
    let! adjustedOps =
      LibPackageManager.PackageManager.stabilizeOpsAgainstPM firstPassPM reParsedOps

    return adjustedOps
  }

/// Reads and parses all .dark files in `packages` dir,
/// failing upon any individual failure.
/// Returns NoChanges if no files have changed since last reload.
let loadIncremental (builtins : RT.Builtins) : Ply<LoadResult> =
  uply {
    // Quick mtime check - doesn't read file contents
    let (changedPaths, unchangedPaths, deletedPaths) =
      FileTracking.getChangedFilePaths packageDir

    // If nothing changed and nothing deleted, skip reload entirely
    if List.isEmpty changedPaths && List.isEmpty deletedPaths then
      debuG "No files changed based on mtime" (List.length unchangedPaths)
      return NoChanges
    else
      debuG "Files to reload based on mtime" (List.length changedPaths)
      debuG "Files deleted" (List.length deletedPaths)

      // For now, if anything changed, do full reload
      // TODO: Implement true incremental - only parse changed files
      let filesWithContents =
        packageDir
        |> listDirectoryRecursive
        |> List.filter (String.contains "_" >> not)
        |> List.filter (fun x -> x |> String.endsWith ".dark")
        |> List.map (fun fileName -> (fileName, System.IO.File.ReadAllText fileName))

      let! ops = loadFullInternal builtins filesWithContents

      // Update tracking with current state
      FileTracking.updateTracking filesWithContents

      return ParsedOps ops
  }

/// Backward-compatible load function (always does full load)
let load (builtins : RT.Builtins) : Ply<List<PT.PackageOp>> =
  uply {
    let filesWithContents =
      packageDir
      |> listDirectoryRecursive
      |> List.filter (String.contains "_" >> not)
      |> List.filter (fun x -> x |> String.endsWith ".dark")
      |> List.map (fun fileName -> (fileName, System.IO.File.ReadAllText fileName))

    return! loadFullInternal builtins filesWithContents
  }
