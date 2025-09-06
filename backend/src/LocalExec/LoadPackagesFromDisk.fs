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
let load (builtins : RT.Builtins) : Ply<PT.Packages> =
  uply {
    let filesWithContents =
      "/home/dark/app/packages"
      |> listDirectoryRecursive
      |> List.filter (String.contains "_" >> not)
      |> List.filter (fun x -> x |> String.endsWith ".dark")
      |> List.map (fun fileName -> (fileName, System.IO.File.ReadAllText fileName))

    // First pass, parse all the packages, allowing unresolved names
    // (other package items won't be available yet)
    let! (packages : PT.Packages) =
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
      |> Ply.map PT.Packages.combine

    // Re-parse the packages, though this time we don't allow unresolved names
    // (any package references that may have been unresolved a second ago should now be OK)
    let! reParsed =
      filesWithContents
      |> Ply.List.mapSequentially (fun (path, contents) ->
        LibParser.Parser.parsePackageFile
          builtins
          (inMemPackageManagerFromPackages packages) // should have the packages from the first pass, with some IDs... that should be used when parsing other things
          NR.OnMissing.ThrowError
          path
          contents)
      |> Ply.map PT.Packages.combine

    // The IDs that weren't locked-in have changed - let's fix that now.
    let adjusted : PT.Packages =
      { types =
          reParsed.types
          |> List.map (fun typ ->
            { typ with
                hash =
                  packages.types
                  |> List.find (fun original -> original.name = typ.name)
                  |> Option.map _.hash
                  |> Option.defaultValue typ.hash })
        values =
          reParsed.values
          |> List.map (fun c ->
            { c with
                hash =
                  packages.values
                  |> List.find (fun original -> original.name = c.name)
                  |> Option.map _.hash
                  |> Option.defaultValue c.hash })
        fns =
          reParsed.fns
          |> List.map (fun fn ->
            { fn with
                hash =
                  packages.fns
                  |> List.find (fun original -> original.name = fn.name)
                  |> Option.map _.hash
                  |> Option.defaultValue fn.hash }) }

    return adjusted
  }
