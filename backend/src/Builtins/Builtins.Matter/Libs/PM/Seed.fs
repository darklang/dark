module Builtins.Matter.Libs.PM.Seed

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

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
        | state, vm, _, [| DString outputPath |] ->
          uply {
            try
              let outputPath = LibExecution.Host.normalizeFilePath outputPath
              LibExecution.PermissionCheck.requireFileWrite state vm outputPath
              do! LibDB.Seed.export outputPath
              return resultOk DUnit
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      // `Native` plus scoped write: export copies the package store and opens
      // SQLite inside LibDB, so a path rule alone cannot confine it.
      callEffects = set [ Effect.PackageRead; Effect.FileWrite; Effect.Native ]
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
