/// StdLib functions for building Dark functionality via Dark canvases
module BackendOnlyStdLib.LibDarkInternal

open System.Threading.Tasks

open Npgsql.FSharp
open LibBackend.Db

open Prelude

open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module Errors = LibExecution.Errors
module Telemetry = LibService.Telemetry

open LibBackend
module SchedulingRules = LibBackend.QueueSchedulingRules

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

// TODO: publish Dark types, and use them rather than these
// anonymous-ish types
module Types =
  module Canvas =
    let meta = TRecord([ "id", TUuid; "name", TStr ])

    let dbMeta = TRecord([ "tlid", TStr; "name", TStr ])
    let httpHandlerMeta = TRecord([ "tlid", TStr; "method", TStr; "route", TStr ])

    let program =
      TRecord([ "dbs", TList(dbMeta); "httpHandlers", TList(httpHandlerMeta) ])

/// Wraps an internal Lib function
/// and ensures that the appropriate permissions are in place
///
/// Also reports usage to telemetry
let internalFn (f : BuiltInFnSig) : BuiltInFnSig =
  (fun (state, args) ->
    uply {
      match! state.program.accountID |> Account.usernameForUserID with
      | None ->
        Exception.raiseInternal $"User not found" [ "id", state.program.accountID ]
        return DUnit
      | Some username ->
        let! canAccess = Account.canAccessOperations username
        if canAccess then
          let fnName =
            state.executingFnName
            |> Option.map FQFnName.toString
            |> Option.defaultValue "unknown"
          use _span =
            Telemetry.child
              "internal_fn"
              [ "canvas", state.program.canvasName
                "user", username
                "fnName", fnName ]
          return! f (state, args)
        else
          return
            Exception.raiseInternal
              "User executed an internal function but isn't an admin"
              [ "username", username ]
    })

// only accessible to the `dark-editor canvas`
let darkEditorFn (f : BuiltInFnSig) : BuiltInFnSig =
  (fun (state, args) ->
    uply {
      if state.program.canvasName.ToString() = "dark-editor" then
        return! f (state, args)
      else
        return
          Exception.raiseInternal
            "dark-editor-only internal function attempted to be used in another canvas"
            [ "canavasId", state.program.canvasID ]
    })

let modifySchedule (fn : CanvasID -> string -> Task<unit>) =
  internalFn (function
    | _, [ DUuid canvasID; DStr handlerName ] ->
      uply {
        do! fn canvasID handlerName
        let! s = SchedulingRules.getWorkerSchedules canvasID
        Pusher.push
          ClientTypes2BackendTypes.Pusher.eventSerializer
          canvasID
          (Pusher.UpdateWorkerStates s)
          None
        return DUnit
      }
    | _ -> incorrectArgs ())


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "endUsers" 0
      parameters = []
      returnType = TList TStr
      description =
        "Return a <type list> of all user email addresses for non-admins and not in @darklang.com or @example.com"
      fn =
        internalFn (function
          | _, [] ->
            uply {
              let! result =
                Sql.query
                  "SELECT email FROM accounts WHERE admin IS FALSE AND email NOT
                     LIKE '%@darklang.com' AND email NOT LIKE '%@example.com'"
                |> Sql.executeAsync (fun read -> read.string "email")
              return result |> List.map DStr |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "insertUser" 2
      parameters =
        [ Param.make "username" TStr ""
          Param.make "email" TStr ""
          Param.make "name" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Add a user. Returns a result containing an empty string. Usernames are unique; if you try to add a username
that's already taken, returns an error."
      fn =
        internalFn (function
          | _, [ DStr username; DStr email; DStr name ] ->
            uply {
              let username =
                Exception.catchError (fun () ->
                  if username.Contains "_" then
                    Exception.raiseCode "Underscores not allowed in usernames"
                  UserName.create username)
              match username with
              | Ok username ->
                let! _user = Account.insertUser username email name
                Analytics.identifyUser username
                let toCanvasName =
                  $"{username}-{LibService.Config.gettingStartedCanvasName}"
                let fromCanvasName = LibService.Config.gettingStartedCanvasSource
                do!
                  CanvasClone.cloneCanvas
                    (CanvasName.createExn fromCanvasName)
                    (CanvasName.createExn toCanvasName)
                    // Don't preserve history here, it isn't useful and
                    // we don't currently have visibility into canvas
                    // history, so we'd rather not share unknown sample-
                    // history with users in case it contains
                    // sensitive information like access keys.
                    false
                return DResult(Ok(DStr ""))
              | Error msg -> return DResult(Error(DStr msg))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getUser" 1
      parameters = [ Param.make "username" TStr "" ]
      returnType =
        TOption(
          TRecord [ "username", TStr; "name", TStr; "email", TStr; "admin", TBool ]
        )
      description = "Return a user for the username. Does not include passwords"
      fn =
        internalFn (function
          | _, [ DStr username ] ->
            uply {
              let! info = Account.getUser (UserName.create username)
              return
                info
                |> Option.map (fun user ->
                  Dval.obj [ ("username", DStr(string user.username))
                             ("name", DStr user.name)
                             ("email", DStr user.email)
                             ("admin", DBool user.admin) ])
                |> DOption
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getUserByEmail" 0
      parameters = [ Param.make "email" TStr "" ]
      returnType =
        TOption(
          TRecord [ "username", TStr; "name", TStr; "email", TStr; "admin", TBool ]
        )
      description = "Return a user for the email. Does not include passwords"
      fn =
        internalFn (function
          | _, [ DStr email ] ->
            uply {
              let! info = Account.getUserByEmail email
              return
                info
                |> Option.map (fun user ->
                  Dval.obj [ ("username", DStr(string user.username))
                             ("name", DStr user.name)
                             ("email", DStr user.email)
                             ("admin", DBool user.admin) ])
                |> DOption
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getUserID" 0
      parameters = [ Param.make "username" TStr "" ]
      returnType = TOption(TUuid)
      description = "Return a user's userID"
      fn =
        internalFn (function
          | _, [ DStr username ] ->
            uply {
              let! info = Account.getUser (UserName.create username)
              return info |> Option.map (fun user -> DUuid user.id) |> DOption
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getUsers" 0
      parameters = []
      returnType = TList TStr
      description = "Return a list of username of all the accounts in Dark"
      fn =
        internalFn (function
          | _, [] ->
            uply {
              let! users = Account.getUsers ()
              return users |> List.map string |> List.map DStr |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getAllCanvases" 0
      parameters = []
      returnType = TList TStr
      description = "Get a list of all canvas names"
      fn =
        internalFn (fun _ ->
          uply {
            let! hosts = Serialize.currentHosts ()
            return hosts |> List.map DStr |> DList
          })
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "canvasesFor" 0
      parameters = [ Param.make "account" TStr "" ]
      returnType = TList TStr
      description =
        "Returns a list of all canvases owned by a particular account (user OR org)"
      fn =
        internalFn (function
          | _, [ DStr username ] ->
            uply {
              let owner = UserName.create username
              let! userID = Account.userIDForUserName owner
              let! cs = Account.ownedCanvases userID
              return cs |> List.map string |> List.map DStr |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "dbs" 0
      parameters = [ Param.make "canvasName" TStr "" ]
      returnType = TList TStr
      description = "Returns a list of toplevel ids of dbs in <param canvasName>"
      fn =
        internalFn (function
          | _, [ DStr canvasName ] ->
            uply {
              let! dbTLIDs =
                Sql.query
                  "SELECT tlid
                     FROM toplevel_oplists
                     JOIN canvases ON canvases.id = canvas_id
                    WHERE canvases.name = @name AND tipe = 'db'"
                |> Sql.parameters [ "name", Sql.string canvasName ]
                |> Sql.executeAsync (fun read -> read.tlid "tlid")
              return dbTLIDs |> List.map int64 |> List.map DInt |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "canvasIDOfCanvasName" 0
      parameters = [ Param.make "canvasName" TStr "" ]
      returnType = TResult(TUuid, TStr)
      description = "Gives canvasID for a canvasName"
      fn =
        internalFn (function
          | _, [ DStr canvasName ] ->
            uply {
              try
                match! Canvas.getMeta (CanvasName.createExn canvasName) with
                | Some meta -> return DResult(Ok(DUuid meta.id))
                | None -> return DResult(Error(DStr "Canvas not found"))
              with
              | e -> return DResult(Error(DStr e.Message))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "canvasNameOfCanvasID" 0
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TResult(TStr, TStr)
      description = "Returns the name of canvas"
      fn =
        internalFn (function
          | _, [ DUuid canvasID ] ->
            uply {
              try
                let! meta = Canvas.getMetaFromID canvasID
                return meta.name |> string |> DStr |> Ok |> DResult
              with
              | _ -> return DResult(Error(DStr "Canvas not found"))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "usernameToUserInfo" 0
      parameters = [ Param.make "username" TStr "" ]
      returnType = TOption varA
      description = "Gives userinfo {username, name, admin, email} for a username"
      fn =
        internalFn (function
          | _, [ DStr username ] ->
            uply {
              let username = UserName.create username
              match! Account.getUser username with
              | None -> return DOption None
              | Some userInfo ->
                return
                  Map [ ("username", DStr(string userInfo.username))
                        ("email", DStr userInfo.email)
                        ("name", DStr userInfo.name)
                        ("admin", DBool userInfo.admin) ]
                  |> DObj
                  |> Some
                  |> DOption
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "log" 0
      parameters =
        [ Param.make "level" TStr ""
          Param.make "name" TStr ""
          Param.make "log" (TDict TStr) "" ]
      returnType = TDict TStr
      description =
        "Write the log object to a honeycomb log, along with whatever enrichment the backend provides. Returns its input"
      fn =
        internalFn (function
          | _, [ DStr level; DStr name; DObj log as result ] ->
            let args =
              log
              |> Map.toList
              // We could just leave the dval vals as strings and use params, but
              // then we can't do numeric things (MAX, AVG, >, etc) with these
              // logs
              |> List.map (fun (k, v) -> (k, DvalReprDeveloper.toRepr v :> obj))
            Telemetry.addEvent name (("level", level) :: args)
            Ply result
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "allFunctions" 0
      parameters = []
      returnType = TList varA
      description =
        "Returns a list of objects, representing the functions available in the standard library. Does not return DarkInternal functions"
      fn =
        let rec typeName (t : DType) : string =
          match t with
          | TInt -> "int"
          | TFloat -> "float"
          | TBool -> "bool"
          | TUnit -> "unit"
          | TChar -> "character"
          | TStr -> "string"
          | TList _ -> "list"
          | TTuple _ -> "tuple"
          | TDict _ -> "dict"
          | TRecord _ -> "dict"
          | TFn _ -> "block"
          | TVariable _ -> "any"
          | TIncomplete -> "incomplete"
          | TError -> "error"
          | THttpResponse _ -> "response"
          | TDB _ -> "datastore"
          | TDateTime -> "date"
          | TPassword -> "password"
          | TUuid -> "uuid"
          | TOption _ -> "option"
          | TResult _ -> "result"
          | TUserType t -> t.type_
          | TBytes -> "bytes"

        internalFn (function
          | state, [] ->
            state.libraries.stdlib
            |> Map.toList
            |> List.filter (fun (key, data) ->
              (not (FQFnName.isInternalFn key)) && data.deprecated = NotDeprecated)
            |> List.map (fun (key, data) ->
              let alist =
                let returnType = typeName data.returnType
                let parameters =
                  data.parameters
                  |> List.map (fun p ->
                    Dval.obj [ ("name", DStr p.name)
                               ("type", DStr(typeName p.typ)) ])
                [ ("name", DStr(FQFnName.toString key))
                  ("documentation", DStr data.description)
                  ("parameters", DList parameters)
                  ("returnType", DStr returnType) ]
              Dval.obj alist)
            |> DList
            |> Ply
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getAndLogTableSizes" 0
      parameters = []
      returnType = TDict(varA)
      // returnType = varA CLEANUP
      description =
        "Query the postgres database for the current size (disk + rowcount) of all
tables. This uses pg_stat, so it is fast but imprecise. This function is logged
in OCaml; its primary purpose is to send data to honeycomb, but also gives
human-readable data."
      fn =
        internalFn (function
          | _, [] ->
            uply {
              let! tableStats = Db.tableStats ()
              // Send events to honeycomb. We could save some events by sending
              // these all as a single event - tablename.disk = 1, etc - but
              // by having an event per table, it's easier to query and graph:
              // `VISUALIZE MAX(disk), MAX(rows);  GROUP BY relation`.
              // (Also, if/when we add more tables, the graph-query doesn't need
              // to be updated)
              //
              // There are ~40k minutes/month, and 20 tables, so a 1/min cron
              // would consume 80k of our 1.5B monthly events. That seems
              // reasonable.
              tableStats
              |> List.iter (fun ts ->
                Telemetry.addEvent
                  "postgres_table_sizes"
                  [ ("relation", ts.relation)
                    ("disk_bytes", ts.diskBytes)
                    ("rows", ts.rows)
                    ("disk_human", ts.diskHuman)
                    ("rows_human", ts.rowsHuman) ])
              // Reformat a bit for human-usable dval output.
              // - Example from my local dev: {
              //     Total: {
              //       disk: 835584,
              //       diskHuman: "816 kB",
              //       rows: 139,
              //       rowsHuman: 139
              //     },
              //     access: {...},
              //     ...
              // }
              return
                tableStats
                |> List.map (fun ts ->
                  (ts.relation,
                   [ ("disk_bytes", DInt(ts.diskBytes))
                     ("rows", DInt(ts.rows))
                     ("disk_human", DStr ts.diskHuman)
                     ("rows_human", DStr ts.rowsHuman) ]
                   |> Map
                   |> DObj))
                |> Map
                |> DObj
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "raiseInternalException" 0
      parameters = [ Param.make "argument" varA "Added as a tag" ]
      returnType = TUnit
      description =
        "Raise an internal exception inside Dark. This is intended to test exceptions
        and exception tracking, not for any real use."
      fn =
        internalFn (function
          | _, [ arg ] ->
            Exception.raiseInternal
              "DarkInternal::raiseInternalException"
              [ "arg", arg ]
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "serverBuildHash" 0
      parameters = []
      returnType = TStr
      description = "Returns the git hash of the server's current deploy"
      fn =
        internalFn (function
          | _, [] -> uply { return DStr LibService.Config.buildHash }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Apis - 404s
    // ---------------------
    { name = fn "DarkInternal" "delete404" 0
      parameters =
        [ Param.make "canvasID" TUuid ""
          Param.make "space" TStr ""
          Param.make "path" TStr ""
          Param.make "modifier" TStr "" ]
      returnType = TUnit
      description = "Deletes a specific 404 for a canvas"
      fn =
        internalFn (function
          | _, [ DUuid canvasID; DStr space; DStr path; DStr modifier ] ->
            uply {
              Telemetry.addTags [ "space", space
                                  "path", path
                                  "modifier", modifier ]
              do! TraceInputs.delete404s canvasID space path modifier
              return DUnit
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getRecent404s" 0
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType =
        TList(
          TRecord [ "space", TStr
                    "path", TStr
                    "modifier", TStr
                    "timestamp", TDateTime
                    "traceID", TUuid ]
        )
      description = "Fetch a list of recent 404s"
      fn =
        internalFn (function
          | _, [ DUuid canvasID ] ->
            uply {
              let! f404s = TraceInputs.getRecent404s canvasID
              return
                f404s
                |> List.map (fun (space, path, modifier, instant, traceID) ->
                  [ "space", DStr space
                    "path", DStr path
                    "modifier", DStr modifier
                    "timestamp", DDateTime(DarkDateTime.fromInstant instant)
                    "traceID",
                    DUuid(LibExecution.AnalysisTypes.TraceID.toUUID traceID) ]
                  |> Map
                  |> DObj)
                |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Apis - secrets
    // ---------------------
    { name = fn "DarkInternal" "getSecrets" 0
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TDict TStr
      description = "Get list of secrets in the canvas"
      fn =
        internalFn (function
          | _, [ DUuid canvasID ] ->
            uply {
              let! secrets = Secret.getCanvasSecrets canvasID
              return
                secrets |> List.map (fun s -> (s.name, DStr s.value)) |> Map |> DObj
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "deleteSecret" 0
      parameters =
        [ Param.make "canvasID" TUuid ""; Param.make "secretName" TStr "" ]
      returnType = TUnit
      description = "Delete a secret"
      fn =
        internalFn (function
          | _, [ DUuid canvasID; DStr secretName ] ->
            uply {
              do! Secret.delete canvasID secretName
              return DUnit
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "insertSecret" 0
      parameters =
        [ Param.make "canvasID" TUuid ""
          Param.make "secretName" TStr ""
          Param.make "secretValue" TStr "" ]
      returnType = TResult(TUnit, TStr)
      description = "Add a secret"
      fn =
        internalFn (function
          | _, [ DUuid canvasID; DStr secretName; DStr secretValue ] ->
            uply {
              try
                do! Secret.insert canvasID secretName secretValue
                return DResult(Ok DUnit)
              with
              | _ -> return DResult(Error(DStr "Error inserting secret"))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Apis - toplevels
    // ---------------------
    { name = fn "DarkInternal" "deleteToplevelForever" 0
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TInt "" ]
      returnType = TBool
      description =
        "Delete a toplevel forever. Requires that the toplevel already by deleted. If so, deletes and returns true. Otherwise returns false"
      fn =
        internalFn (function
          | _, [ DUuid canvasID; DInt tlid ] ->
            uply {
              let! meta = Canvas.getMetaFromID canvasID
              let tlid = uint64 tlid
              let! c =
                Canvas.loadFrom Serialize.IncludeDeletedToplevels meta [ tlid ]
              if Map.containsKey tlid c.deletedHandlers
                 || Map.containsKey tlid c.deletedDBs
                 || Map.containsKey tlid c.deletedUserTypes
                 || Map.containsKey tlid c.deletedUserFunctions then
                do! Canvas.deleteToplevelForever meta tlid
                return DBool true
              else
                return DBool false
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Apis - DBs
    // ---------------------
    { name = fn "DarkInternal" "unlockedDBs" 0
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList TInt
      description = "Get a list of unlocked DBs"
      fn =
        internalFn (function
          | _, [ DUuid canvasID ] ->
            uply {
              let! meta = Canvas.getMetaFromID canvasID
              let! unlocked = UserDB.unlocked meta.owner meta.id
              return unlocked |> List.map int64 |> List.map DInt |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Apis - workers
    // ---------------------
    { name = fn "DarkInternal" "getQueueCount" 0
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TInt "" ]
      returnType = TList TInt
      description = "Get count of how many events are in the queue for this tlid"
      fn =
        internalFn (function
          | _, [ DUuid canvasID; DInt tlid ] ->
            uply {
              let tlid = uint64 tlid
              let! count = Stats.workerStats canvasID tlid
              return DInt count
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getAllSchedulingRules" 0
      parameters = []
      returnType = TList varA
      description = "Returns a list of all queue scheduling rules"
      fn =
        internalFn (function
          | _, [] ->
            uply {
              let! rules = SchedulingRules.getAllSchedulingRules ()
              return rules |> List.map SchedulingRules.SchedulingRule.toDval |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getSchedulingRulesForCanvas" 0
      parameters = [ Param.make "canvasID" TUuid "" ]
      returnType = TList varA
      description =
        "Returns a list of all queue scheduling rules for the specified canvasID"
      fn =
        internalFn (function
          | _, [ DUuid canvasID ] ->
            uply {
              let! rules = SchedulingRules.getSchedulingRules canvasID
              return rules |> List.map SchedulingRules.SchedulingRule.toDval |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "addWorkerSchedulingBlock" 0
      parameters =
        [ Param.make "canvasID" TUuid ""; Param.make "handlerName" TStr "" ]
      returnType = TUnit
      description =
        "Add a worker scheduling 'block' for the given canvas and handler. This prevents any events for that handler from being scheduled until the block is manually removed."
      fn = modifySchedule EventQueueV2.blockWorker
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "removeWorkerSchedulingBlock" 0
      parameters =
        [ Param.make "canvasID" TUuid ""; Param.make "handlerName" TStr "" ]
      returnType = TUnit
      description =
        "Removes the worker scheduling block, if one exists, for the given canvas and handler. Enqueued events from this job will immediately be scheduled."
      fn = modifySchedule EventQueueV2.unblockWorker
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // ---------------------
    // Apis - canvases and org metadata
    // ---------------------
    { name = fn "DarkInternal" "getCanvasList" 0
      parameters = [ Param.make "userID" TUuid "" ]
      returnType = TList TStr
      description = "Returns all canvases owned by a user"
      fn =
        internalFn (function
          | _, [ DUuid userID ] ->
            uply {
              let! canvasList = Account.ownedCanvases userID
              return canvasList |> List.map string |> List.map DStr |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }



    { name = fn "DarkInternal" "getOpsForToplevel" 0
      parameters = [ Param.make "canvasID" TUuid ""; Param.make "tlid" TInt "" ]
      returnType = TList TStr
      description = "Returns all ops for a tlid in the given canvas"
      fn =
        internalFn (function
          | _, [ DUuid canvasID; DInt tlid ] ->
            uply {
              let tlid = uint64 tlid
              let! ops =
                let loadAmount = Serialize.LoadAmount.IncludeDeletedToplevels
                Serialize.loadOplists loadAmount canvasID [ tlid ]

              match ops with
              | [ (_tlid, ops) ] -> return ops |> List.map (string >> DStr) |> DList
              | _ -> return DList []
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Fns available only to the `dark-editor` canvas.
    // Slowly, functions above (only available to admins) should be moved here,
    // with the darkEditorFn wrapper used in place of the internalFn wrapper.

    // TODO: any tlids below should somehow really be uint64s

    { name = fn "DarkInternal" "darkEditorCanvas" 0
      parameters = []
      returnType = Types.Canvas.meta
      description = "Returns basic details of the dark-editor canvas"
      fn =
        darkEditorFn (function
          | state, [] ->
            uply {
              return
                [ "id", DUuid(state.program.canvasID)
                  "name", DStr(state.program.canvasName.ToString()) ]
                |> Map
                |> DObj // TODO: DRecord
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // TODO: avail a `currentCanvas` fn, and remove the above fn

    // TODO: this name is bad?
    { name = fn "DarkInternal" "canvasProgram" 0
      parameters = [ Param.make "canvasId" TUuid "" ]
      returnType = TResult(Types.Canvas.program, TStr)
      description =
        "Returns a list of toplevel ids of http handlers in canvas <param canvasId>"
      fn =
        darkEditorFn (function
          | _, [ DUuid canvasId ] ->
            uply {
              let! meta = Canvas.getMetaFromID canvasId
              let! canvas = Canvas.loadAll meta

              let dbs =
                Map.values canvas.dbs
                |> Seq.toList
                |> List.map (fun db ->
                  [ "tlid", DStr(db.tlid.ToString()); "name", DStr db.name ]
                  |> Map
                  |> DObj)
                |> DList

              let httpHandlers =
                Map.values canvas.handlers
                |> Seq.toList
                |> List.choose (fun handler ->
                  match handler.spec with
                  | PT.Handler.Worker _
                  | PT.Handler.Cron _
                  | PT.Handler.REPL _ -> None
                  | PT.Handler.HTTP (route, method, _ids) ->
                    [ "tlid", DStr(handler.tlid.ToString())
                      "method", DStr method
                      "route", DStr route ]
                    |> Map
                    |> DObj
                    |> Some)
                |> DList

              return
                DResult(Ok(DObj(Map [ "dbs", dbs; "httpHandlers", httpHandlers ])))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
