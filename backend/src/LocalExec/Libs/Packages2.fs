/// Package functions that require access to other types/fns in this module
module LocalExec.Libs.Packages2

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module PT2DT = LibExecution.ProgramTypesToDarkTypes

let packageManager = LibCloud.PackageManager.packageManager

let resolver : LibParser.NameResolver.NameResolver =
  let builtinResolver =
    // CLEANUP we need a better way to determine what builtins should be
    // available to the name resolver, as this currently assumes builtins
    // from _all_ environments are available
    LibExecution.Builtin.combine
      // We are missing the builtins that contain this function (and all associated ones)
      [ BuiltinExecution.Builtin.contents
          BuiltinExecution.Libs.HttpClient.defaultConfig
        BuiltinCli.Builtin.contents
        Packages.contents
        Cli.contents
        TestUtils.LibTest.contents
        BuiltinCloudExecution.Builtin.contents
        BuiltinCliHost.Builtin.contents ]
      []
      []
    |> LibParser.NameResolver.fromBuiltins

  let thisResolver =
    { LibParser.NameResolver.empty with
        builtinFns =
          Set
            [ LibExecution.ProgramTypes.FnName.builtIn
                [ "LocalExec"; "Packages" ]
                "parseAndSave"
                0
              LibExecution.ProgramTypes.FnName.builtIn
                [ "LocalExec"; "Packages" ]
                "parse"
                0 ] }

  LibParser.NameResolver.merge builtinResolver thisResolver (Some packageManager)



let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "Packages" ] "parse" 0
      typeParams = []
      parameters =
        [ Param.make "package source" TString "The source code of the package"
          Param.make "filename" TString "Used for error message" ]
      returnType =
        TypeReference.result
          (TCustomType(
            Ok(FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Package" 0)),
            []
          ))
          TString
      description = "Parse a package"
      fn =
        function
        | _, _, [ DString contents; DString path ] ->
          uply {
            let resolver = { resolver with allowError = false }

            let! (fns, types, constants) =
              LibParser.Parser.parsePackageFile resolver path contents

            let! packagesFns =
              fns |> Ply.List.mapSequentially (fun fn -> PT2DT.PackageFn.toDT fn)
            let! packagesTypes =
              types |> Ply.List.mapSequentially PT2DT.PackageType.toDT
            let! packagesConstants =
              constants |> Ply.List.mapSequentially PT2DT.PackageConstant.toDT

            return!
              Dval.record
                (FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Package" 0))
                (Some [])
                [ ("fns", Dval.list VT.unknownTODO packagesFns)
                  ("types", Dval.list VT.unknownTODO packagesTypes)
                  ("constants", Dval.list VT.unknownTODO packagesConstants) ]
              |> Ply.map (Dval.resultOk VT.unknownTODO VT.string)

          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : LibExecution.Builtin.Contents = (fns, [], [])
