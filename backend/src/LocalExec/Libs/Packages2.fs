/// Package functions that require access to other types/fns in this module
module LocalExec.Libs.Packages2

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

module PT2DT = LibExecution.ProgramTypesToDarkTypes

let packageManager = LibCloud.PackageManager.packageManager

let resolver : LibParser.NameResolver.NameResolver =
  let stdlibResolver =
    // CLEANUP we need a better way to determine what builtins should be
    // available to the name resolver, as this currently assumes builtins
    // from _all_ environments are available
    LibExecution.StdLib.combine
      // We are missing the builtins that contain this function (and all associated ones)
      [ BuiltinExecution.Builtin.contents
          BuiltinExecution.Libs.HttpClient.defaultConfig
        StdLibCli.StdLib.contents
        Packages.contents
        Cli.contents
        TestUtils.LibTest.contents
        StdLibCloudExecution.StdLib.contents
        StdLibCliHost.StdLib.contents ]
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

  LibParser.NameResolver.merge stdlibResolver thisResolver (Some packageManager)



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

            let packagesFns = fns |> List.map (fun fn -> PT2DT.PackageFn.toDT fn)
            let packagesTypes = types |> List.map PT2DT.PackageType.toDT
            let packagesConstants = constants |> List.map PT2DT.PackageConstant.toDT

            return
              Dval.resultOk (
                Dval.record
                  (FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Package" 0))
                  [ ("fns", DList packagesFns)
                    ("types", DList packagesTypes)
                    ("constants", DList packagesConstants) ]
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : LibExecution.StdLib.Contents = (fns, [], [])
