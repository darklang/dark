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

    // Re-parse the packages, though this time we don't allow unresolved names
    // (any package references that may have been unresolved a second ago should now be OK)
    let! reParsedOps =
      filesWithContents
      |> Ply.List.mapSequentially (fun (path, contents) ->
        LibParser.Parser.parsePackageFile
          builtins
          (inMemPackageManagerFromOps firstPassOps)
          NR.OnMissing.ThrowError
          path
          contents)
      |> Ply.map List.flatten

    // Build location→ID maps from first pass for ID stabilization
    let firstPassTypeLocToId =
      firstPassOps
      |> List.choose (function
        | PT.PackageOp.SetTypeName(id, loc) -> Some(loc, id)
        | _ -> None)
      |> Map.ofList

    let firstPassValueLocToId =
      firstPassOps
      |> List.choose (function
        | PT.PackageOp.SetValueName(id, loc) -> Some(loc, id)
        | _ -> None)
      |> Map.ofList

    let firstPassFnLocToId =
      firstPassOps
      |> List.choose (function
        | PT.PackageOp.SetFnName(id, loc) -> Some(loc, id)
        | _ -> None)
      |> Map.ofList

    // Adjust IDs in second pass to match first pass (ID stabilization)
    let adjustedOps : List<PT.PackageOp> =
      reParsedOps
      |> List.map (function
        | PT.PackageOp.SetTypeName(_, loc) ->
          let stableId = firstPassTypeLocToId |> Map.tryFind loc |> Option.defaultWith (fun () -> System.Guid.NewGuid())
          PT.PackageOp.SetTypeName(stableId, loc)

        | PT.PackageOp.AddType typ ->
          // Find this type's location to get stable ID
          let typLoc =
            reParsedOps
            |> List.tryPick (function
              | PT.PackageOp.SetTypeName(id, loc) when id = typ.id -> Some loc
              | _ -> None)
          let stableId =
            typLoc
            |> Option.bind (fun loc -> Map.tryFind loc firstPassTypeLocToId)
            |> Option.defaultValue typ.id
          PT.PackageOp.AddType { typ with id = stableId }

        | PT.PackageOp.SetValueName(_, loc) ->
          let stableId = firstPassValueLocToId |> Map.tryFind loc |> Option.defaultWith (fun () -> System.Guid.NewGuid())
          PT.PackageOp.SetValueName(stableId, loc)

        | PT.PackageOp.AddValue value ->
          let valueLoc =
            reParsedOps
            |> List.tryPick (function
              | PT.PackageOp.SetValueName(id, loc) when id = value.id -> Some loc
              | _ -> None)
          let stableId =
            valueLoc
            |> Option.bind (fun loc -> Map.tryFind loc firstPassValueLocToId)
            |> Option.defaultValue value.id
          PT.PackageOp.AddValue { value with id = stableId }

        | PT.PackageOp.SetFnName(_, loc) ->
          let stableId = firstPassFnLocToId |> Map.tryFind loc |> Option.defaultWith (fun () -> System.Guid.NewGuid())
          PT.PackageOp.SetFnName(stableId, loc)

        | PT.PackageOp.AddFn fn ->
          let fnLoc =
            reParsedOps
            |> List.tryPick (function
              | PT.PackageOp.SetFnName(id, loc) when id = fn.id -> Some loc
              | _ -> None)
          let stableId =
            fnLoc
            |> Option.bind (fun loc -> Map.tryFind loc firstPassFnLocToId)
            |> Option.defaultValue fn.id
          PT.PackageOp.AddFn { fn with id = stableId })

    return adjustedOps
  }
