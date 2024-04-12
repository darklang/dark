module BuiltinPackagesOnDisk.Libs.Packages

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes


open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

open BuiltinPackagesOnDisk.Utils

let constants : List<BuiltInConstant> = []

let rec listDirectoryRecursive (dir : string) : List<string> =
  let isNormalFile (path : string) : bool =
    try
      let attrs = System.IO.File.GetAttributes(path)
      let isDir = attrs.HasFlag(System.IO.FileAttributes.Directory)
      let exists = System.IO.File.Exists(path) || System.IO.Directory.Exists(path)
      exists && not isDir
    with e ->
      false

  let contents = System.IO.Directory.EnumerateFileSystemEntries dir |> Seq.toList
  let (files, dirs) = contents |> List.partition (fun x -> isNormalFile x)
  let nested = dirs |> List.map (fun d -> listDirectoryRecursive d) |> List.flatten
  dirs |> List.append files |> List.append nested


/// Reads and parses all .dark files in `packages` dir,
/// failing upon any individual failure
let loadFromDisk (builtins : RT.Builtins) : Ply<PT.Packages> =
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
      |> Ply.List.mapSequentially (fun (path, contents) ->
        LibParser.Parser.parsePackageFile nameResolver path contents)
      |> Ply.map PT.Packages.combine

    // Re-parse the packages, though this time we don't allow unresolved names
    // (any package references that may have been unresolved a second ago should now be OK)
    let nameResolver =
      { nameResolver with
          allowError = false
          packageManager =
            Some(
              inMemPackageManagerFromPackages
                packagesParsedWithUnresolvedNamesAllowed
            ) }

    return!
      filesWithContents
      |> Ply.List.mapSequentially (fun (path, contents) ->
        LibParser.Parser.parsePackageFile nameResolver path contents)
      |> Ply.map PT.Packages.combine
  }


let fns : List<BuiltInFn> =
  [ { name = fn "getPackagesFromDisk" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok PT2DT.Packages.typeName, [])
      description = "Gets all Packages (from disk)."
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            let! packages = loadFromDisk BuiltinPackagesOnDisk.Builtins.all
            return PT2DT.Packages.toDT packages
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : Builtins = Builtin.fromContents constants fns
