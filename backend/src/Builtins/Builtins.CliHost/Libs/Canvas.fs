/// Builtin functions for app and DB operations in the CLI
module Builtins.CliHost.Libs.Canvas

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibSqlite.Db

module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module VT = LibExecution.ValueType
module NR = LibExecution.RuntimeTypes.NameResolution
module App = LibCloud.App
module Serialize = LibCloud.Serialize
module Account = LibCloud.Account
module PackageLocation = LibDB.PackageLocation


let fns () : List<BuiltInFn> =
  [ { name = fn "appDBCreate" 0
      typeParams = []
      parameters =
        [ Param.make "appID" TUuid "The app to add the DB to"
          Param.make "dbName" TString "Name of the database"
          Param.make
            "typeHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Hash of the type stored in this DB" ]
      returnType = TypeReference.result TUInt64 TString
      description = "Creates a new database in the specified app"
      fn =
        (function
        | _, _, _, [ DUuid appID; DString dbName; typeHashDval ] ->
          let typeHash = PT2DT.Hash.fromDT typeHashDval
          uply {
            // Check for existing DB with the same name
            let! existing =
              Sql.query
                "SELECT COUNT(*) as cnt FROM toplevels_v0
                 WHERE app_id = @appID
                   AND tipe = 'db'
                   AND name = @name
                   AND deleted = 0"
              |> Sql.parameters
                [ "appID", Sql.uuid appID; "name", Sql.string dbName ]
              |> Sql.executeRowAsync (fun read -> read.int "cnt")

            if existing > 0 then
              return
                Dval.resultError
                  KTUInt64
                  KTString
                  (DString $"A database named '{dbName}' already exists")
            else
              let tlid = gid ()
              let db : PT.DB.T =
                { tlid = tlid
                  name = dbName
                  version = 0
                  typ =
                    PT.TypeReference.TCustomType(
                      { originalName = []
                        location = None
                        resolved = Ok(PT.FQTypeName.Package typeHash) },
                      []
                    ) }

              let toplevel = PT.Toplevel.TLDB db
              do! App.saveTLIDs appID [ (toplevel, Serialize.NotDeleted) ]
              return Dval.resultOk KTUInt64 KTString (DUInt64 tlid)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "appGetOrCreateForAccount" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "The account ID" ]
      returnType = TUuid
      description =
        "Gets the app for an account, creating one if it doesn't exist"
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! appID = App.getOrCreateForAccount accountID
            return DUuid appID
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "appDBListAll" 0
      typeParams = []
      parameters =
        [ Param.make "appID" TUuid "The app to list DBs from"
          Param.make "branchId" TUuid "Branch for resolving type names" ]
      returnType = TList(TTuple(TString, TString, []))
      description =
        "Returns a list of (name, typeName) tuples for all DBs in the app"
      fn =
        (function
        | _, _, _, [ DUuid appID; DUuid branchId ] ->
          uply {
            let! app = App.loadAllDBs appID
            let pm = LibDB.PackageManager.pt
            let! dbs =
              app.dbs
              |> Map.values
              |> Ply.List.mapSequentially (fun (db : PT.DB.T) ->
                uply {
                  let! typeName =
                    match db.typ with
                    | PT.TypeReference.TCustomType({ resolved = Ok(PT.FQTypeName.Package typeID) },
                                                   _) ->
                      uply {
                        let! locs = pm.getTypeLocations branchId typeID
                        match locs with
                        | location :: _ -> return PackageLocation.toFQN location
                        | [] -> return typeID.ToString()
                      }
                    | _ -> Ply "unknown"
                  return DTuple(DString db.name, DString typeName, [])
                })
            return Dval.list (KTTuple(VT.string, VT.string, [])) dbs
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "appDBDrop" 0
      typeParams = []
      parameters =
        [ Param.make "appID" TUuid "The app containing the DB"
          Param.make "dbName" TString "Name of the database to drop" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Drops (deletes) all databases with the given name from the specified app"
      fn =
        (function
        | _, _, _, [ DUuid appID; DString dbName ] ->
          uply {
            let! matchingTlids =
              Sql.query
                "SELECT tlid FROM toplevels_v0
                 WHERE app_id = @appID
                   AND tipe = 'db'
                   AND name = @name
                   AND deleted = 0"
              |> Sql.parameters
                [ "appID", Sql.uuid appID; "name", Sql.string dbName ]
              |> Sql.executeAsync (fun read -> read.tlid "tlid")

            match matchingTlids with
            | [] ->
              return
                Dval.resultError
                  KTUnit
                  KTString
                  (DString $"Database not found: {dbName}")
            | _ ->
              do!
                matchingTlids
                |> Task.iterInParallel (fun tlid ->
                  App.deleteToplevelForever appID tlid)
              // Also delete any user data for these DBs
              do!
                matchingTlids
                |> Task.iterInParallel (fun tlid ->
                  Sql.query
                    "DELETE FROM user_data_v0
                     WHERE app_id = @appID AND table_tlid = @tlid"
                  |> Sql.parameters
                    [ "appID", Sql.uuid appID; "tlid", Sql.id tlid ]
                  |> Sql.executeStatementAsync)
              return Dval.resultOk KTUnit KTString DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    { name = fn "userGetByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "The account name to look up" ]
      returnType = TypeReference.option TUuid
      description =
        "Looks up a user by name and returns their ID, or None if not found."
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! result = Account.getUserByName name
            match result with
            | Some userID -> return Dval.optionSome KTUuid (DUuid userID)
            | None -> return Dval.optionNone KTUuid
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated
      accessibility = Any }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
