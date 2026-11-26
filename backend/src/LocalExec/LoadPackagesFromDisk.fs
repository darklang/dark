[<RequireQualifiedAccess>]
module LocalExec.LoadPackagesFromDisk

open System.Threading.Tasks

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
  : List<PT.PackageOp> =
  match LibParser.Package.parse builtins pm onMissing contents with
  | Ok ops -> ops
  | Error diagnostics ->
    let rendered = String.concat "\n" diagnostics
    Exception.raiseInternal
      $"Failed to parse package file {path}:\n{rendered}"
      [ "path", path ]


/// How many files are parsed at once. Past eight the gain is small, and a reload
/// often shares the box with a build or a test run.
let private parseWorkers : int = min 8 System.Environment.ProcessorCount

/// <param parseOne> over every file, `parseWorkers` at a time on the thread
/// pool. The ops come back in file order, so they and their hashes are the same
/// as a one-by-one parse. Nothing is shared between files: the parser keeps only
/// local state, and the PM it reads is never written after it is built.
let private parseAll
  (parseOne : string -> string -> List<PT.PackageOp>)
  (files : List<string * string>)
  : List<PT.PackageOp> =
  LibExecution.HostEvents.runHere (fun () ->
    files
    |> Task.mapWithConcurrency parseWorkers (fun (path, contents) ->
      System.Threading.Tasks.Task.Run<List<PT.PackageOp>>(fun () ->
        parseOne path contents))
    |> Task.map List.flatten)


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


/// The error message for a name declared more than once. <param declarations>
/// lists each place it is declared: the file, and the kind (such as "fn").
///
/// The message is one of:
///   fn A.f is declared twice in x.dark
///   fn A.f is declared in both x.dark and y.dark
///   A.f is declared as a fn and a val in x.dark
let private describeDuplicate
  (name : string)
  (declarations : List<string * string>)
  : string =
  let files = declarations |> List.map fst |> List.distinct |> List.sort
  let kinds = declarations |> List.map snd |> List.distinct |> List.sort
  let where =
    match files with
    | [ file ] -> $"in {file}"
    | [ one; two ] -> $"in both {one} and {two}"
    | _ ->
      let joined = String.concat ", " files
      $"in {List.length files} files: {joined}"
  match kinds, files with
  | [ kind ], [ _ ] ->
    let count = List.length declarations
    let times = if count = 2 then "twice" else $"{count} times"
    $"{kind} {name} is declared {times} {where}"
  | [ kind ], _ -> $"{kind} {name} is declared {where}"
  | _ ->
    let listed = kinds |> List.map (fun kind -> $"a {kind}") |> String.concat " and "
    $"{name} is declared as {listed} {where}"


/// Parse <param filesWithContents> (`files`) and fail on parse or name-resolution errors.
let load
  (builtins : RT.Builtins)
  (filesWithContents : List<string * string>)
  : List<PT.PackageOp> =

  // -- Phase 1: Initial parse (unresolved names allowed) --
  let fileCount = List.length filesWithContents
  debuG "phase 1" $"parsing {fileCount} files (unresolved names allowed)"
  let parseTimer = System.Diagnostics.Stopwatch()
  parseTimer.Start()
  let (firstPassOps : List<PT.PackageOp>) =
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
    : int * List<PT.PackageOp> =
    if iteration >= 50 then
      (iteration, currentOps)
    else
      let iteration = iteration + 1
      debuG
        $"  pass {iteration}"
        $"re-parsing {fileCount} files, resolving names, computing hashes"
      let pm = LibDB.PackageManager.withExtraOps PT.PackageManager.empty currentOps
      parseTimer.Start()
      let newRawOps =
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
      if converged then (iteration, newOps) else converge iteration newOps newHashes
  let (iteration, currentOps) = converge 0 (HS.computeRealHashes firstPassOps) []

  debuG "phase 2" $"converged after {iteration} passes"
  match LibParser.Package.duplicateDeclarations currentOps with
  | [] -> ()
  | duplicates ->
    // The ops don't say which file each came from, so parse the files again to find
    // out. This runs only when there is a duplicate to report.
    let root = LibConfig.ConfigDsl.absoluteDir "DARK_CONFIG_ROOT_DIR"
    let declared : List<string * string * string> =
      filesWithContents
      |> List.collect (fun (path, contents) ->
        let file = System.IO.Path.GetRelativePath(root, path)
        parsePackageFile
          builtins
          PT.PackageManager.empty
          NR.OnMissing.Allow
          path
          contents
        |> LibParser.Package.declarations
        |> List.map (fun (kind, name) -> (file, kind, name)))
    let describe (name : string) : string =
      declared
      |> List.choose (fun (file, kind, declaredName) ->
        if declaredName = name then Some(file, kind) else None)
      |> describeDuplicate name
    // one line: the build shows only the last lines of the reload log
    let listed = duplicates |> List.map describe |> String.concat "; "
    Exception.raiseInternal listed [ "count", List.length duplicates ]
  match LibParser.Package.unresolvedCalls currentOps with
  | [] -> ()
  | missing ->
    // one line: the build shows only the last lines of the reload log
    let listed = String.concat "; " missing
    Exception.raiseInternal
      $"Package code calls names that do not exist: {listed}"
      [ "count", List.length missing ]
  let passes = iteration + 1
  let secs = parseTimer.Elapsed.TotalSeconds
  print
    $"Parsed {fileCount} files {passes} times, {parseWorkers} at once: {secs:F1}s"
  currentOps
