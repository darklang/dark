/// The package manager allows user-defined functions to be shared with other
/// users. Currently only enabled for admins.
module LibBackend.PackageManager

open System.Threading.Tasks
open FSharp.Control.Tasks
open Npgsql.FSharp
open Npgsql

open Prelude
open Tablecloth
open Db

module BinarySerialization = LibBinarySerialization.BinarySerialization
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT = LibExecution.RuntimeTypes

// ------------------
// Uploading
// ------------------

// let max_version
//     (function_name : string)
//     (username : string)
//     (package : string)
//     (module_ : string)
//     (fnname : string) : int option =
//   Db.run
//     ~name:"add_package_management_function abort txn if exn"
//     "ABORT"
//     ~params:[] ;
//   Db.fetch_one
//     ~name:"add_package_management_function get_latest_version_for_error"
//     "SELECT MAX(version) FROM packages_v0 JOIN accounts ON user_id = accounts.id
//                   WHERE username = $1 AND package = $2 AND module = $3 AND fnname = $4"
//     ~subject:function_name
//     ~params:
//       [ Db.String username
//       ; Db.String package
//       ; Db.String module_
//       ; Db.String fnname ]
//     ~result:TextResult
//   |> List.hd
//   |> Option.bind ~f:(fun v -> if v = "" then None else Some (int_of_string v))
let writeBody (tlid : tlid) (expr : PT.Expr) : Task<unit> =
  task {
    let binary = BinarySerialization.serializeExpr tlid expr
    return!
      Sql.query "UPDATE package_functions_v0 SET body = @body where tlid = @tlid"
      |> Sql.parameters [ "body", Sql.bytea binary; "tlid", Sql.tlid tlid ]
      |> Sql.executeStatementAsync
  }

let savePackages (packages : List<PT.PackageFn.T>) : Task<Unit> =
  task {
    do!
      packages
      |> Task.iterInParallel (fun fn ->
        Sql.query
          "INSERT INTO package_functions_v0 (tlid, id, owner, modules, fnname, version, definition)
           VALUES (@tlid, @id, @owner, @modules, @fnname, @version, @definition)"
        |> Sql.parameters [ "tlid", Sql.tlid fn.tlid
                            "id", Sql.uuid fn.id
                            "owner", Sql.string fn.name.owner
                            "modules",
                            Sql.string (fn.name.modules |> String.concat ".")
                            "fnname", Sql.string fn.name.function_
                            "version", Sql.int fn.name.version
                            "definition",
                            Sql.bytea (BinarySerialization.serializePackageFn fn) ]
        |> Sql.executeStatementAsync)
    return ()
  }



// ------------------
// Fetching functions
// ------------------

let allFunctions () : Task<List<PT.PackageFn.T>> =
  task {
    let! fns =
      Sql.query "SELECT id, definition FROM package_functions_v0"
      |> Sql.parameters []
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.bytea "definition"))

    return
      fns
      |> List.map (fun (id, def) -> BinarySerialization.deserializePackageFn id def)

  }
