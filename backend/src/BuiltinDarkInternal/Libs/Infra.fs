/// Builtin functions for looking at Dark infra
module BuiltinDarkInternal.Libs.Infra

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs


let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalInfraGetAndLogTableSizes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TDict(
          TCustomType(
            Ok(FQTypeName.Package PackageIDs.Type.Internal.Infra.tableSize),
            []
          )
        )
      description =
        "Query the SQLite database for the current row count of all tables. Disk size is approximated via PRAGMA page_count * page_size."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! tableStats = LibDB.Db.tableStats ()

            let typeName =
              FQTypeName.Package PackageIDs.Type.Internal.Infra.tableSize

            return
              tableStats
              |> List.map (fun ts ->
                let fields =
                  [ ("disk", DInt64(ts.diskBytes))
                    ("rows", DInt64(ts.rows))
                    ("diskHuman", DString ts.diskHuman)
                    ("rowsHuman", DString ts.rowsHuman) ]

                (ts.relation, DRecord(typeName, typeName, [], Map fields)))
              |> Dval.dict (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalInfraServerBuildHash" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the git hash of the server's current deploy"
      fn =
        (function
        | _, _, _, [ DUnit ] -> LibService.Config.buildHash |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
