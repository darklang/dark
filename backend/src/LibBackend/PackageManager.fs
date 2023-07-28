/// The package manager allows types and functions to be shared with other users
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

let savePackageFunctions (fns : List<PT.PackageFn.T>) : Task<Unit> =
  fns
  |> Task.iterInParallel (fun fn ->
    let (PT.FnName.FnName name) = fn.name.name
    Sql.query
      "INSERT INTO package_functions_v0 (tlid, id, owner, modules, fnname, version, definition)
       VALUES (@tlid, @id, @owner, @modules, @fnname, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid fn.tlid
        "id", Sql.uuid fn.id
        "owner", Sql.string fn.name.owner
        "modules", Sql.string (fn.name.modules |> String.concat ".")
        "fnname", Sql.string name
        "version", Sql.int fn.name.version
        "definition", Sql.bytea (BinarySerialization.serializePackageFn fn) ]
    |> Sql.executeStatementAsync)

let savePackageTypes (types : List<PT.PackageType.T>) : Task<Unit> =
  types
  |> Task.iterInParallel (fun typ ->
    let (PT.TypeName.TypeName name) = typ.name.name
    Sql.query
      "INSERT INTO package_types_v0 (tlid, id, owner, modules, typename, version, definition)
       VALUES (@tlid, @id, @owner, @modules, @typename, @version, @definition)"
    |> Sql.parameters
      [ "tlid", Sql.tlid typ.tlid
        "id", Sql.uuid typ.id
        "owner", Sql.string typ.name.owner
        "modules", Sql.string (typ.name.modules |> String.concat ".")
        "typename", Sql.string name
        "version", Sql.int typ.name.version
        "definition", Sql.bytea (BinarySerialization.serializePackageType typ) ]
    |> Sql.executeStatementAsync)



// ------------------
// Fetching
// ------------------

let allFunctions : Task<List<PT.PackageFn.T>> =
  task {
    let! fns =
      Sql.query "SELECT id, definition FROM package_functions_v0"
      |> Sql.parameters []
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.bytea "definition"))

    return
      fns
      |> List.map (fun (id, def) -> BinarySerialization.deserializePackageFn id def)
  }

let allTypes : Task<List<PT.PackageType.T>> =
  task {
    let! types =
      Sql.query "SELECT id, definition FROM package_types_v0"
      |> Sql.parameters []
      |> Sql.executeAsync (fun read -> (read.uuid "id", read.bytea "definition"))

    return
      types
      |> List.map (fun (id, def) ->
        BinarySerialization.deserializePackageType id def)
  }

open System

let cachedTask (expiration: TimeSpan) (f: Task<'a>): Task<'a> =
  let mutable value = None
  let mutable lastUpdate = DateTime.MinValue

  task {
    match value, DateTime.Now - lastUpdate > expiration with
    | Some value, false -> return value
    | _ ->
      let! newValue = f
      value <- Some newValue
      lastUpdate <- DateTime.Now
      return newValue
  }

let packageManager : RT.PackageManager =
  let allTypes = cachedTask (TimeSpan.FromMinutes 1.) allTypes
  let allFunctions = cachedTask (TimeSpan.FromMinutes 1.) allFunctions

  { getType =
      fun typ ->
        uply {
          let! types = allTypes

          let types =
            types
            |> List.map (fun (t : PT.PackageType.T) ->
              (t.name |> PT2RT.TypeName.Package.toRT, PT2RT.PackageType.toRT t))
            |> Map.ofList

          return types.TryFind typ
        }

    getFn =
      fun fn ->
        uply {
          let! fns = allFunctions

          let fns =
            fns
            |> List.map (fun (f : PT.PackageFn.T) ->
              (f.name |> PT2RT.FnName.Package.toRT, PT2RT.PackageFn.toRT f))
            |> Map.ofList

          return fns.TryFind fn
        }

    init = uply { return () } }
