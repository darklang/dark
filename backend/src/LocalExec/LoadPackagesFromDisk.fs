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
      |> List.filter (fun x -> x |> String.endsWith ".dark")
      |> List.map (fun fileName -> (fileName, System.IO.File.ReadAllText fileName))

    // First pass, parse all the packages, allowing unresolved names
    // (other package items won't be available yet)
    let! (packages : PT.Packages) =
      filesWithContents
      // TODO: parallelize
      |> Ply.List.mapSequentially (fun (path, contents) ->
        //debuG "parsing" path
        LibParser.Parser.parsePackageFile
          builtins
          RT.PackageManager.empty
          NR.OnMissing.Allow
          path
          contents)
      |> Ply.map PT.Packages.combine

    // Re-parse the packages, though this time we don't allow unresolved names
    // (any package references that may have been unresolved a second ago should now be OK)
    return!
      filesWithContents
      |> Ply.List.mapSequentially (fun (path, contents) ->
        LibParser.Parser.parsePackageFile
          builtins
          (inMemPackageManagerFromPackages packages)
          NR.OnMissing.ThrowError
          path
          contents)
      |> Ply.map PT.Packages.combine
  }
