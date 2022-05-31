/// StdLib functions for building Dark functionality via Dark canvases
/// instead of the ApiServer or other means
module BackendOnlyStdLib.LibDarkInternal

open System.Threading.Tasks

open Npgsql.FSharp
open LibBackend.Db

open Prelude

open LibExecution.RuntimeTypes

module DvalReprInternalDeprecated = LibExecution.DvalReprInternalDeprecated
module DvalReprExternal = LibExecution.DvalReprExternal
module Errors = LibExecution.Errors
module Telemetry = LibService.Telemetry

open LibBackend
module SchedulingRules = LibBackend.QueueSchedulingRules

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

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
        return DNull
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


let modifySchedule (fn : CanvasID -> string -> Task<unit>) =
  internalFn (function
    | state, [ DUuid canvasID; DStr handlerName ] ->
      uply {
        do! fn canvasID handlerName
        let! s = SchedulingRules.getWorkerSchedules canvasID
        Pusher.pushWorkerStates canvasID s
        return DNull
      }
    | _ -> incorrectArgs ())


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "endUsers" 0
      parameters = []
      returnType = TList varA
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
          Param.make "name" TStr ""
          Param.make "analytics_metadata" (TDict TStr) "" ]
      returnType = TResult(varA, TStr)
      description =
        "Add a user. Returns a result containing the password for the user,
which was randomly generated. Usernames are unique; if you try to add a username
that's already taken, returns an error."
      fn =
        internalFn (function
          | state, [ DStr username; DStr email; DStr name; DObj analyticsMetadata ] ->
            uply {
              let username = UserName.create username
              let! _user =
                Account.insertUser username email name (Some analyticsMetadata)
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
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getUser" 1
      parameters = [ Param.make "username" TStr "" ]
      returnType = TOption varA
      description = "Return a user for the username. Does not include passwords."
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
      returnType = TOption varA
      description = "Return a user for the email. Does not include passwords."
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


    { name = fn "DarkInternal" "setAdmin" 0
      parameters = [ Param.make "username" TStr ""; Param.make "admin" TBool "" ]
      returnType = TNull
      description = "Set whether a user is an admin. Returns null."
      fn =
        internalFn (function
          | state, [ DStr username; DBool admin ] ->
            uply {
              let username = UserName.create username
              do! Account.setAdmin admin username
              LibService.Rollbar.notify
                "setAdmin called"
                [ "username", username; "admin", admin ]
              Analytics.identifyUser username
              return DNull
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "getUsers" 0
      parameters = []
      returnType = TList TStr
      description = "Return a list of username of all the accounts in Dark."
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
      description = "TODO"
      // CLEANUP description = "Get a list of all canvas names"
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
      parameters = [ Param.make "host" TStr "" ]
      returnType = TList TStr
      description = "Returns a list of toplevel ids of dbs in `host`"
      fn =
        internalFn (function
          | _, [ DStr host ] ->
            uply {
              let! dbTLIDs =
                // CLEANUP stop calling things host
                Sql.query
                  "SELECT tlid
                     FROM toplevel_oplists
                     JOIN canvases ON canvases.id = canvas_id
                    WHERE canvases.name = @name AND tipe = 'db'"
                |> Sql.parameters [ "name", Sql.string host ]
                |> Sql.executeAsync (fun read -> read.string "tlid")
              return dbTLIDs |> List.map DStr |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "pushStrollerEvent" 1
      parameters =
        [ Param.make "canvas_id" TStr ""
          Param.make "event" TStr ""
          Param.make "payload" varA "" ]
      returnType = TResult(varB, TStr)
      description = "Pushes an event to Stroller"
      fn =
        internalFn (function
          | state, [ DStr canvasID; DStr event; payload ] ->
            (try
              Pusher.push
                (canvasID |> System.Guid.Parse)
                event
                (payload |> DvalReprInternalDeprecated.toInternalRoundtrippableV0)
              Ply(DResult(Ok payload))
             with
             | e -> Ply(DResult(Error(e |> string |> DStr))))
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "sessionKeyToUsername" 0
      parameters = [ Param.make "sessionKey" TStr "" ]
      returnType = TOption varA
      description = "Looks up the username for a session_key"
      fn =
        internalFn (function
          | _, [ DStr sessionKey ] ->
            uply {
              match! Session.getNoCSRF sessionKey with
              | None -> return DResult(Error(DStr "No session for cookie"))
              | Some session -> return DResult(Ok(DStr(string session.username)))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "canvasIdOfCanvasName" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TOption varA
      description = "Gives canvasId for a canvasName/host"
      fn =
        internalFn (function
          | _, [ DStr host ] ->
            uply {
              try
                let! meta = Canvas.getMetaExn (CanvasName.createExn host)
                return DOption(Some(DStr(string meta.id)))
              with
              | e -> return DOption None
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


    { name = fn "DarkInternal" "grant" 0
      parameters =
        [ Param.make "username" TStr ""
          Param.make "org" TStr ""
          Param.make "permission" TStr "" ]
      returnType = TResult(varA, TStr)
      description = "Set a user's permissions for a particular auth_domain."
      fn =
        internalFn (function
          | _, [ DStr username; DStr org; DStr permission ] ->
            uply {
              let resultToDval r =
                match r with
                | Ok x -> DResult(Ok x)
                | Error x -> DResult(Error(DStr x))
              let! _userID = username |> UserName.create |> Account.userIDForUserName
              let! _orgID = org |> UserName.create |> Account.userIDForUserName
              let permission =
                match permission with
                | "rw" -> Ok(Some Authorization.ReadWrite)
                | "r" -> Ok(Some Authorization.Read)
                | "" -> Ok None
                | _ -> Error "can't decode permission string"
              let! result =
                match permission with
                | Ok permission ->
                  uply {
                    do!
                      Authorization.setUserAccess
                        (UserName.create username)
                        (OwnerName.create org)
                        permission
                    return Ok(DStr "success!")
                  }
                | Error e -> Ply(Error e)
              return result |> resultToDval
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "grantsFor" 0
      parameters = [ Param.make "org" TStr "" ]
      returnType = TDict(varA)
      // CLEANUP returnType = varA
      description =
        "Returns a dict mapping username->permission of users who have been granted permissions for a given auth_domain"
      fn =
        internalFn (function
          | _, [ DStr org ] ->
            uply {
              let! grants = Authorization.grantsFor (OwnerName.create org)
              return
                grants
                |> List.map (fun (user, perm) ->
                  (string user, perm |> string |> DStr))
                |> Map
                |> DObj
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "orgsFor" 0
      parameters = [ Param.make "username" TStr "" ]
      returnType = TDict TStr
      // returnType = varA // CLEANUP
      description =
        "Returns a dict mapping orgs->permission to which the given `username` has been given permission"
      fn =
        internalFn (function
          | _, [ DStr username ] ->
            uply {
              let! orgs = Authorization.orgsFor (UserName.create username)
              return
                orgs
                |> List.map (fun (org, perm) -> (string org, perm |> string |> DStr))
                |> Map
                |> DObj
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "checkPermission" 0
      parameters = [ Param.make "username" TStr ""; Param.make "canvas" TStr "" ]
      // CLEANUP: should be TStr
      returnType = TBool
      description = "Check a user's permissions for a particular canvas."
      fn =
        internalFn (function
          | _, [ DStr username; DStr canvas ] ->
            uply {
              let owner =
                Account.ownerNameFromCanvasName (CanvasName.createExn canvas)
              match! Authorization.permission owner (UserName.create username) with
              | Some perm -> return DStr(string perm)
              | None -> return DStr ""
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
      returnType = TDict(TStr)
      // returnType = varA
      description =
        "Write the log object to a honeycomb log, along with whatever enrichment the backend provides."
      fn =
        internalFn (function
          | _, [ DStr level; DStr name; DObj log as result ] ->
            let args =
              log
              |> Map.toList
              // We could just leave the dval vals as strings and use params, but
              // then we can't do numeric things (MAX, AVG, >, etc) with these
              // logs
              |> List.map (fun (k, v) ->
                (k, DvalReprExternal.toDeveloperReprV0 v :> obj))
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
        internalFn (function
          | state, [] ->
            state.libraries.stdlib
            |> Map.toList
            |> List.filter (fun (key, data) ->
              (not (FQFnName.isInternalFn key)) && data.deprecated = NotDeprecated)
            |> List.map (fun (key, data) ->
              let alist =
                let returnType = DvalReprExternal.typeToBCTypeName data.returnType
                let parameters =
                  data.parameters
                  |> List.map (fun p ->
                    Dval.obj [ ("name", DStr p.name)
                               ("type", DStr(DvalReprExternal.typeToBCTypeName p.typ)) ])
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


    { name = fn "DarkInternal" "getAllSchedulingRules" 0
      parameters = []
      returnType = TList varA
      description = "Returns a list of all queue scheduling rules."
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
      parameters = [ Param.make "canvas_id" TUuid "" ]
      returnType = TList varA
      description =
        "Returns a list of all queue scheduling rules for the specified canvas_id"
      // CLEANUP "Returns a list of all queue scheduling rules for the specified canvasID"
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
        [ Param.make "canvas_id" TUuid ""; Param.make "handler_name" TStr "" ]
      returnType = TNull
      description =
        "Add a worker scheduling 'block' for the given canvas and handler. This prevents any events for that handler from being scheduled until the block is manually removed."
      fn = modifySchedule EventQueueV2.blockWorker
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "removeWorkerSchedulingBlock" 0
      parameters =
        [ Param.make "canvas_id" TUuid ""; Param.make "handler_name" TStr "" ]
      returnType = TNull
      description =
        "Removes the worker scheduling block, if one exists, for the given canvas and handler. Enqueued events from this job will immediately be scheduled."
      fn = modifySchedule EventQueueV2.unblockWorker
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "newSessionForUsername" 0
      parameters = [ Param.make "username" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "If username is an existing user, puts a new session in the DB and returns the new sessionKey."
      fn =
        internalFn (function
          | state, [ DStr username ] ->
            uply {
              try
                // This is used by the login.darklang.com/dark-cli callback
                let username = UserName.create username
                let! session = Session.insert username
                return DResult(Ok(DStr session.sessionKey))
              with
              | e ->
                let metadata =
                  [ "username", username :> obj
                    "fn", "DarkInternal::newSessionForUserName"
                    "error", "failed to create session" ]
                state.reportException state metadata e
                return DResult(Error(DStr "Failed to create session"))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "DarkInternal" "newSessionForUsername" 1) }


    { name = fn "DarkInternal" "newSessionForUsername" 1
      parameters = [ Param.make "username" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        (* We need the csrf token for dark-cli to use *)
        "If username is an existing user, puts a new session in the DB and returns the new sessionKey and csrfToken."
      fn =
        internalFn (function
          | state, [ DStr username ] ->
            uply {
              try
                let username = UserName.create username
                let! session = Session.insert username
                return
                  DResult(
                    Ok(
                      DObj(
                        Map [ ("sessionKey", DStr session.sessionKey)
                              ("csrfToken", DStr session.csrfToken) ]
                      )
                    )
                  )
              with
              | e ->
                let metadata =
                  [ "username", username :> obj
                    "fn", "DarkInternal::newSessionForUserName"
                    "error", "failed to create session" ]
                state.reportException state metadata e
                return DResult(Error(DStr "Failed to create session"))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "deleteSession" 0
      parameters = [ Param.make "session_key" TStr "" ]
      returnType = TInt
      description =
        "Delete session by session_key; return number of sessions deleted."
      fn =
        internalFn (function
          | _, [ DStr sessionKey ] ->
            uply {
              let! count =
                Sql.query "DELETE FROM session WHERE session_key = @key"
                |> Sql.parameters [ "key", Sql.string sessionKey ]
                |> Sql.executeNonQueryAsync
              return DInt count
            }
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
          | state, [] ->
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
      returnType = TNull
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
      deprecated = NotDeprecated } ]
