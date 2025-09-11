module LocalExec.Utils

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


/// Util function to create a package manager from an in-memory bag of package items
let inMemPackageManagerFromPackages (p : PT.Packages) : PT.PackageManager =
  { findType =
      fun name ->
        p.types |> List.find (fun t -> t.name = name) |> Option.map _.hash |> Ply
    findFn =
      fun name ->
        p.fns |> List.find (fun f -> f.name = name) |> Option.map _.hash |> Ply
    findValue =
      fun name ->
        p.values |> List.find (fun c -> c.name = name) |> Option.map _.hash |> Ply

    getType = fun hash -> p.types |> List.find (fun t -> t.hash = hash) |> Ply
    getFn = fun hash -> p.fns |> List.find (fun f -> f.hash = hash) |> Ply
    getValue = fun hash -> p.values |> List.find (fun v -> v.hash = hash) |> Ply

    search =
      fun _ ->
        uply { return { submodules = [ [] ]; fns = []; types = []; values = [] } }

    init = uply { return () } }


let isNormalFile (path : string) : bool =
  try
    let attrs = System.IO.File.GetAttributes(path)
    let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
    let exists = System.IO.File.Exists(path) || System.IO.Directory.Exists(path)
    exists && not isDir
  with e ->
    false


let rec listDirectoryRecursive (dir : string) : List<string> =
  let contents = System.IO.Directory.EnumerateFileSystemEntries dir |> Seq.toList
  let (files, dirs) = contents |> List.partition (fun x -> isNormalFile x)
  let nested = dirs |> List.map (fun d -> listDirectoryRecursive d) |> List.flatten
  dirs |> List.append files |> List.append nested
