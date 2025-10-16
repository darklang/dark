module LocalExec.Utils

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes


/// Create an in-memory package manager from PackageOps
let inMemPackageManagerFromOps (ops : List<PT.PackageOp>) : PT.PackageManager =
  // Extract items and location mappings from ops
  let types =
    ops
    |> List.choose (function
      | PT.PackageOp.AddType t -> Some t
      | _ -> None)

  let values =
    ops
    |> List.choose (function
      | PT.PackageOp.AddValue v -> Some v
      | _ -> None)

  let fns =
    ops
    |> List.choose (function
      | PT.PackageOp.AddFn f -> Some f
      | _ -> None)

  // Build location→ID maps
  let typeLocToId =
    ops
    |> List.choose (function
      | PT.PackageOp.SetTypeName(id, loc) -> Some(loc, id)
      | _ -> None)
    |> Map.ofList

  let valueLocToId =
    ops
    |> List.choose (function
      | PT.PackageOp.SetValueName(id, loc) -> Some(loc, id)
      | _ -> None)
    |> Map.ofList

  let fnLocToId =
    ops
    |> List.choose (function
      | PT.PackageOp.SetFnName(id, loc) -> Some(loc, id)
      | _ -> None)
    |> Map.ofList

  { findType = fun (_, loc) -> Map.tryFind loc typeLocToId |> Ply
    findValue = fun (_, loc) -> Map.tryFind loc valueLocToId |> Ply
    findFn = fun (_, loc) -> Map.tryFind loc fnLocToId |> Ply

    getType = fun id -> types |> List.find (fun t -> t.id = id) |> Ply
    getValue = fun id -> values |> List.find (fun v -> v.id = id) |> Ply
    getFn = fun id -> fns |> List.find (fun f -> f.id = id) |> Ply

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
