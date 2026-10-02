module Builtins.Matter.Libs.PM.Seed

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module C2DT = LibExecution.CommonToDarkTypes
module D = LibExecution.DvalDecoder

open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "pmSeedExport" 0
      typeParams = []
      parameters =
        [ Param.make "outputPath" TString ""
          Param.make
            "upToCommit"
            (TypeReference.option TString)
            "cut the seed at this commit and its ancestors, so the same commit yields the same ops however far the store has moved since; `None` takes everything committed" ]
      returnType = TypeReference.result TUnit TString
      description = "Export a minimal seed.db from the current database"
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | state, vm, _, [| DString outputPath; upToCommit |] ->
          uply {
            try
              let outputPath = LibExecution.Host.normalizeFilePath outputPath
              LibExecution.PermissionCheck.requireFileWrite state vm outputPath
              let upToCommit = C2DT.Option.fromDT D.string upToCommit
              do! LibDB.Seed.exportAt outputPath upToCommit
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
