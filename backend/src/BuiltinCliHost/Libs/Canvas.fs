/// Builtin functions for canvas and DB operations in the CLI
module BuiltinCliHost.Libs.Canvas

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open Fumble
open LibDB.Db

module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes
module VT = LibExecution.ValueType
module Canvas = LibCloud.Canvas
module Serialize = LibCloud.Serialize
module Account = LibCloud.Account


let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalCanvasDBCreate" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid "The canvas to add the DB to"
          Param.make "dbName" TString "Name of the database"
          Param.make "typeID" TUuid "ID of the type stored in this DB" ]
      returnType = TypeReference.result TUInt64 TString
      description = "Creates a new database in the specified canvas"
      fn =
        (function
        | _, _, _, [ DUuid canvasID; DString dbName; DUuid typeID ] ->
          uply {
            // Check for existing DB with the same name
            let! existing =
              Sql.query
                "SELECT COUNT(*) as cnt FROM toplevels_v0
                 WHERE canvas_id = @canvasID
                   AND tipe = 'db'
                   AND name = @name
                   AND deleted = 0"
              |> Sql.parameters
                [ "canvasID", Sql.uuid canvasID; "name", Sql.string dbName ]
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
                      Ok(PT.FQTypeName.Package typeID),
                      []
                    ) }

              let toplevel = PT.Toplevel.TLDB db
              do! Canvas.saveTLIDs canvasID [ (toplevel, Serialize.NotDeleted) ]
              return Dval.resultOk KTUInt64 KTString (DUInt64 tlid)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasGetOrCreateForAccount" 0
      typeParams = []
      parameters =
        [ Param.make "accountID" TUuid "The account ID"
          Param.make "domain" TString "Domain for the canvas if created" ]
      returnType = TUuid
      description =
        "Gets the canvas for an account, creating one if it doesn't exist"
      fn =
        (function
        | _, _, _, [ DUuid accountID; DString domain ] ->
          uply {
            let! canvasID = Canvas.getOrCreateForAccount accountID domain
            return DUuid canvasID
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasDBListAll" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid "The canvas to list DBs from"
          Param.make "branchId" TUuid "Branch for resolving type names" ]
      returnType = TList(TTuple(TString, TString, []))
      description =
        "Returns a list of (name, typeName) tuples for all DBs in the canvas"
      fn =
        (function
        | _, _, _, [ DUuid canvasID; DUuid branchId ] ->
          uply {
            let! canvas = Canvas.loadAllDBs canvasID
            let pm = LibPackageManager.PackageManager.pt
            let! dbs =
              canvas.dbs
              |> Map.values
              |> Ply.List.mapSequentially (fun (db : PT.DB.T) ->
                uply {
                  let! typeName =
                    match db.typ with
                    | PT.TypeReference.TCustomType(Ok(PT.FQTypeName.Package typeID),
                                                   _) ->
                      uply {
                        let! loc = pm.getTypeLocation branchId typeID
                        match loc with
                        | Some location ->
                          let parts =
                            [ location.owner ]
                            @ location.modules
                            @ [ location.name ]
                          return String.concat "." parts
                        | None -> return typeID.ToString()
                      }
                    | _ -> Ply "unknown"
                  return DTuple(DString db.name, DString typeName, [])
                })
            return Dval.list (KTTuple(VT.string, VT.string, [])) dbs
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalCanvasDBDrop" 0
      typeParams = []
      parameters =
        [ Param.make "canvasID" TUuid "The canvas containing the DB"
          Param.make "dbName" TString "Name of the database to drop" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Drops (deletes) all databases with the given name from the specified canvas"
      fn =
        (function
        | _, _, _, [ DUuid canvasID; DString dbName ] ->
          uply {
            let! matchingTlids =
              Sql.query
                "SELECT tlid FROM toplevels_v0
                 WHERE canvas_id = @canvasID
                   AND tipe = 'db'
                   AND name = @name
                   AND deleted = 0"
              |> Sql.parameters
                [ "canvasID", Sql.uuid canvasID; "name", Sql.string dbName ]
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
                  Canvas.deleteToplevelForever canvasID tlid)
              // Also delete any user data for these DBs
              do!
                matchingTlids
                |> Task.iterInParallel (fun tlid ->
                  Sql.query
                    "DELETE FROM user_data_v0
                     WHERE canvas_id = @canvasID AND table_tlid = @tlid"
                  |> Sql.parameters
                    [ "canvasID", Sql.uuid canvasID; "tlid", Sql.id tlid ]
                  |> Sql.executeStatementAsync)
              return Dval.resultOk KTUnit KTString DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalUserGetByName" 0
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
      deprecated = NotDeprecated }


    ]


let builtins = LibExecution.Builtin.make [] fns
