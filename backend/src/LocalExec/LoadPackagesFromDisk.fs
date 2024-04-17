[<RequireQualifiedAccess>]
module LocalExec.LoadPackagesFromDisk

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes

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
    let nameResolver =
      LibParser.NameResolver.fromBuiltins builtins
      |> fun nr -> { nr with allowError = true }

    let! (packagesParsedWithUnresolvedNamesAllowed : PT.Packages) =
      filesWithContents
      // TODO: parallelize
      |> Ply.List.mapSequentially (fun (path, contents) ->
        //print $"Parsing {path}, allowing unresolved names"
        LibParser.Parser.parsePackageFile nameResolver path contents)
      |> Ply.map PT.Packages.combine

    // Re-parse the packages, though this time we don't allow unresolved names
    // (any package references that may have been unresolved a second ago should now be OK)
    let nameResolver =
      { nameResolver with
          allowError = false
          packageManager =
            inMemPackageManagerFromPackages packagesParsedWithUnresolvedNamesAllowed }

    return!
      filesWithContents
      |> Ply.List.mapSequentially (fun (path, contents) ->
        //print $"Parsing {path}, not allowing unresolved names"
        LibParser.Parser.parsePackageFile nameResolver path contents)
      |> Ply.map PT.Packages.combine
  }
