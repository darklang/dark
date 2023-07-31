/// Package functions that require access to other types/fns in this module
module LocalExec.Libs.Packages2

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Tablecloth
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

module PT2DT = StdLibDarkInternal.Helpers.ProgramTypesToDarkTypes

let resolver =
  let stdlibResolver =
    // CLEANUP we need a better way to determine what builtins should be
    // available to the name resolver, as this currently assumes builtins
    // from _all_ environments are available
    LibExecution.StdLib.combine
      // We are missing the builtins that contain this function (and all associated ones)
      [ StdLibExecution.StdLib.contents
        StdLibCli.StdLib.contents
        Packages.contents
        Cli.contents
        TestUtils.LibTest.contents
        StdLibCloudExecution.StdLib.contents ]
      []
      []
    // TODO: this may need more builtins, and packages
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

  LibParser.NameResolver.merge stdlibResolver thisResolver



let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "Packages" ] "parse" 0
      typeParams = []
      parameters =
        [ Param.make "package source" TString "The source code of the package"
          Param.make "filename" TString "Used for error message" ]
      returnType =
        TypeReference.result
          (TCustomType(
            FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Package" 0),
            []
          ))
          TString
      description = "Parse a package"
      fn =
        function
        | _, _, [ DString contents; DString path ] ->
          uply {
            let (fns, types, constants) =
              LibParser.Parser.parsePackage resolver path contents

            let packagesFns = fns |> List.map (fun fn -> PT2DT.PackageFn.toDT fn)

            let packagesTypes =
              types |> List.map (fun typ -> PT2DT.PackageType.toDT typ)

            let packagesConstants =
              constants
              |> List.map (fun constant -> PT2DT.PackageConstant.toDT constant)

            return
              Dval.resultOk (
                DRecord(
                  FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Package" 0),
                  Map(
                    [ ("fns", DList packagesFns)
                      ("types", DList packagesTypes)
                      ("constants", DList packagesConstants) ]
                  )
                )
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn [ "LocalExec"; "Packages" ] "parseAndSave" 0
      typeParams = []
      parameters =
        [ Param.make "package source" TString "The source code of the package"
          Param.make "filename" TString "Used for error message" ]
      returnType = TUnit
      description = "Parse a package and save it to the database"
      fn =
        function
        | _, _, [ DString contents; DString path ] ->
          uply {
            let (fns, types, constants) =
              LibParser.Parser.parsePackage resolver path contents
            do! LibCloud.PackageManager.savePackageFunctions fns
            do! LibCloud.PackageManager.savePackageTypes types
            do! LibCloud.PackageManager.savePackageConstants constants

            return DUnit
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let contents : LibExecution.StdLib.Contents = (fns, [], [])
