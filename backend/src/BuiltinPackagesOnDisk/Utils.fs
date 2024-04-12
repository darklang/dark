module BuiltinPackagesOnDisk.Utils

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
    getFnByTLID = fun tlid -> fns |> List.find (fun f -> f.tlid = tlid) |> Ply
    getConstant = fun name -> consts |> List.find (fun c -> c.name = name) |> Ply

    init = uply { return () } }
