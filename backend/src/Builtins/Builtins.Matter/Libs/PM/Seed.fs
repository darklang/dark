module Builtins.Matter.Libs.PM.Seed

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
              do! LibDB.Seed.export outputPath
              return resultOk DUnit
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmRebuildProjections" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TInt64
      description =
        "Drop every regenerable projection table and re-fold the entire `package_ops` log to
         rebuild them (the ops⊥projections recovery path — `dark branch rebuild`). Projections
         are non-authoritative: losing one costs only the CPU to re-fold from the canonical op
         log. Returns the number of ops re-folded."
      fn =
        (function
        | exeState, _, _, [ DUnit ] ->
          uply {
            let! refolded = LibDB.Seed.rebuildProjections ()
            // The structural fold restores `package_values` rows with NULL `rt_dval`
            // (evaluation is a separate phase that normally runs at startup over unapplied
            // ops). Re-evaluate here so values are readable right after a rebuild — otherwise
            // reading any value hits a NULL rt_dval. Mirrors `pmAddOps`' post-insert eval.
            let builtins : Builtins =
              { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
            let! _ = LibDB.Seed.evaluateAllValues builtins LibDB.PackageManager.rt
            return DInt64 refolded
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmProjectionStatus" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TTuple(TInt64, TInt64, [])
      description =
        "Projection-currency counters for `dark status`: `(opsCount, foldedThrough)` — total ops
         in the canonical `package_ops` log vs how many are folded into the projections. Equal
         means the cache is current; a gap means ops await folding (`branch rebuild` / restart)."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! (total, folded) = LibDB.Seed.projectionStatus ()
            return DTuple(DInt64 total, DInt64 folded, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
