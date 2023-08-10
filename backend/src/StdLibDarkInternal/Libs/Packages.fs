/// StdLib functions for building Dark functionality via Dark packages
module StdLibDarkInternal.Libs.Packages

open Prelude
open Tablecloth

open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module PT = LibExecution.ProgramTypes
module Canvas = LibCloud.Canvas
module Serialize = LibCloud.Serialize
module PT2DT = LibExecution.ProgramTypesToDarkTypes


let stdlibPackageTyp
  (submodules : List<string>)
  (name : string)
  (version : int)
  : TypeName.T =
  TypeName.fqPackage "Darklang" ("Stdlib" :: submodules) name version

let modules = [ "DarkInternal"; "Packages" ]
let fn = fn modules


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []
let fns : List<BuiltInFn> =
  [ { name = fn "all" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TCustomType(Ok(stdlibPackageTyp [] "Packages" 0), [])
      description = "List all package types and functions"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! types = LibCloud.PackageManager.allTypes
            let types = List.map PT2DT.PackageType.toDT types

            let! fns = LibCloud.PackageManager.allFunctions
            let fns = List.map PT2DT.PackageFn.toDT fns

            let! constants = LibCloud.PackageManager.allConstants
            let constants = List.map PT2DT.PackageConstant.toDT constants

            return
              Dval.record
                (stdlibPackageTyp [] "Packages" 0)
                [ "types", DList types


                  "fns", DList fns
                  "constants", DList constants ]
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
