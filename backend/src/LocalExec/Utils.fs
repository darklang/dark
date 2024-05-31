module LocalExec.Utils

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

/// Util function to create a package manager from an in-memory bag of package items
let inMemPackageManagerFromPackages (p : PT.Packages) : RT.PackageManager =
  let types = p.types |> List.map PT2RT.PackageType.toRT
  let fns = p.fns |> List.map PT2RT.PackageFn.toRT
  let consts = p.constants |> List.map PT2RT.PackageConstant.toRT

  { getType = fun name -> types |> List.find (fun t -> t.name = name) |> Ply
    getFn = fun name -> fns |> List.find (fun f -> f.name = name) |> Ply
    getFnByID = fun id -> fns |> List.find (fun f -> f.id = id) |> Ply
    getConstant = fun name -> consts |> List.find (fun c -> c.name = name) |> Ply

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
