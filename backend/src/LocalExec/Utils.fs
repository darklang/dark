module LocalExec.Utils

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


/// Util function to create a package manager from an in-memory bag of package items
let inMemPackageManagerFromPackages (p : PT.Packages) : PT.PackageManager =
  { findType =
      fun name ->
        p.types |> List.find (fun t -> t.name = name) |> Option.map _.id |> Ply
    findFn =
      fun name ->
        p.fns |> List.find (fun f -> f.name = name) |> Option.map _.id |> Ply
    findConstant =
      fun name ->
        p.constants |> List.find (fun c -> c.name = name) |> Option.map _.id |> Ply

    search =
      fun _ ->
        uply {
          return
            { submodules = [ [] ]
              fns = p.fns
              types = p.types
              constants = p.constants }
        }

    getType = fun id -> p.types |> List.find (fun t -> t.id = id) |> Ply
    getFn = fun id -> p.fns |> List.find (fun f -> f.id = id) |> Ply
    getConstant = fun id -> p.constants |> List.find (fun c -> c.id = id) |> Ply

    getAllFnNames =
      fun _ ->
        p.fns
        |> List.map (fun f ->
          let modules = f.name.modules |> String.concat "."
          $"{modules}.{f.name.name}")
        |> Ply

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
