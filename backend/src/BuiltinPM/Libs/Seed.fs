module BuiltinPM.Libs.Seed

open Prelude
open LibExecution.RuntimeTypes

module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin

open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "pmSeedExport" 0
      typeParams = []
      parameters = [ Param.make "outputPath" TString "" ]
      returnType = TypeReference.result TUnit TString
      description = "Export a minimal seed.db from the current database"
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, _, [ DString outputPath ] ->
          uply {
            try
              do! LibPackageManager.Seed.export outputPath
              return resultOk DUnit
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
