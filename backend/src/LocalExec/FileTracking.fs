/// Track file modification times and content hashes to enable incremental package reloading
module LocalExec.FileTracking

open System
open System.IO
open Prelude

/// Path to store file tracking data (simple text format: mtime<tab>hash<tab>path per line)
/// Uses relative path so it works in both docker and local environments
let trackingFilePath =
  System.IO.Path.Combine(
    System.IO.Directory.GetCurrentDirectory(),
    "rundir",
    "package-file-tracking.txt")

/// Compute SHA256 hash of file content
let computeFileHash (content : string) : string =
  use sha256 = System.Security.Cryptography.SHA256.Create()
  let bytes = System.Text.Encoding.UTF8.GetBytes(content)
  let hashBytes = sha256.ComputeHash(bytes)
  Convert.ToHexString(hashBytes).ToLowerInvariant()

/// Tracked file info
type TrackedFile =
  { mtime : int64  // File.GetLastWriteTimeUtc().Ticks
    hash : string }

/// Load tracked files from disk (path -> TrackedFile)
let loadTracking () : Map<string, TrackedFile> =
  try
    if File.Exists(trackingFilePath) then
      File.ReadAllLines(trackingFilePath)
      |> Array.choose (fun line ->
        let parts = line.Split('\t')
        if parts.Length = 3 then
          let mtime = Int64.TryParse(parts[0]) |> function | true, v -> v | _ -> 0L
          Some(parts[2], { mtime = mtime; hash = parts[1] })
        else None)
      |> Map.ofArray
    else
      Map.empty
  with _ ->
    Map.empty

/// Save tracked files to disk
let saveTracking (tracking : Map<string, TrackedFile>) : unit =
  try
    let dir = Path.GetDirectoryName(trackingFilePath)
    if not (Directory.Exists(dir)) then
      Directory.CreateDirectory(dir) |> ignore<DirectoryInfo>
    let lines =
      tracking
      |> Map.toSeq
      |> Seq.map (fun (path, info) -> $"{info.mtime}\t{info.hash}\t{path}")
      |> Seq.toArray
    File.WriteAllLines(trackingFilePath, lines)
  with ex ->
    debuG "Failed to save file tracking" ex.Message

/// Quick check using mtimes only - returns list of potentially changed files
/// This is fast because it only reads file metadata, not content
let getChangedFilePaths (packageDir : string) : List<string> * List<string> * List<string> =
  let tracked = loadTracking ()

  // Get all current .dark files with their mtimes
  let currentFiles =
    packageDir
    |> Utils.listDirectoryRecursive
    |> List.filter (String.contains "_" >> not)
    |> List.filter (fun x -> x |> String.endsWith ".dark")
    |> List.map (fun path ->
      let mtime = File.GetLastWriteTimeUtc(path).Ticks
      (path, mtime))

  let currentPaths = currentFiles |> List.map fst |> Set.ofList
  let trackedPaths = tracked |> Map.keys |> Set.ofSeq

  // Files that were deleted
  let deletedPaths = Set.difference trackedPaths currentPaths |> Set.toList

  // Check which files might have changed (different mtime)
  let mutable changed = []
  let mutable unchanged = []

  for (path, currentMtime) in currentFiles do
    match Map.tryFind path tracked with
    | Some info when info.mtime = currentMtime ->
      // Same mtime, assume unchanged
      unchanged <- path :: unchanged
    | _ ->
      // Different mtime or new file
      changed <- path :: changed

  debuG "Quick mtime check - changed" (List.length changed)
  debuG "Quick mtime check - unchanged" (List.length unchanged)

  (List.rev changed, List.rev unchanged, deletedPaths)

/// Update tracking with current file states
let updateTracking (files : List<string * string>) : unit =
  let tracking =
    files
    |> List.map (fun (path, content) ->
      let mtime = File.GetLastWriteTimeUtc(path).Ticks
      let hash = computeFileHash content
      (path, { mtime = mtime; hash = hash }))
    |> Map.ofList
  saveTracking tracking

/// Clear all tracking data (forces full reload next time)
let clearTracking () : unit =
  try
    if File.Exists(trackingFilePath) then
      File.Delete(trackingFilePath)
  with _ -> ()
