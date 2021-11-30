module BackendOnlyStdLib.LibDarkInternal

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude

open LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr
module Errors = LibExecution.Errors

open LibBackend

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

// Apply this to function to wrap that function in an  that checks
// permissions for the dark internal functions and logs status.
let internalFn (f : BuiltInFnSig) : BuiltInFnSig =
  (fun (state, args) ->
    uply {
      match! state.program.accountID |> Account.usernameForUserID with
      | None ->
        Exception.raiseInternal $"User not found with id: {state.program.accountID}"
        return DNull
      | Some username ->
        let! canAccess = Account.canAccessOperations username
        if canAccess then
          // (Log.infO "internalFn" [ ("user", username); ("status", "starting") ]
          // FSTODO:
          let! result = f (state, args)
          // FSTODO
          //  Log.infO "internalFn" [ ("user", username); ("status", "finished") ]
          return result
        else
          return
            Exception.raiseInternal
              $"User executed an internal function but isn't an admin: {username}"
    })


let modifySchedule (fn : CanvasID -> string -> Task<unit>) =
  internalFn (function
    | state, [ DUuid canvasID; DStr handlerName ] ->
      uply {
        do! fn canvasID handlerName
        let! s = EventQueue.getWorkerSchedules canvasID
        Pusher.pushWorkerStates state.executionID canvasID s
        return DNull
      }
    | _ -> incorrectArgs ())


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "checkAccess" 0
      parameters = []
      returnType = TNull
      description = "TODO"
      fn = internalFn (fun _ -> Ply DNull)
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "endUsers" 0
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "checkAllCanvases" 0
      parameters = []
      returnType = TNull
      description = "TODO"
      fn = internalFn (fun _ -> Ply DNull)
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DarkInternal" "migrateAllCanvases" 0
      parameters = []
      returnType = TNull
      description = "REMOVED"
      fn = internalFn (fun _ -> Ply DNull)
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DarkInternal" "cleanupOldTraces" 0
      parameters = []
      returnType = TNull
      description = "Deprecated, use v1"
      fn = internalFn (fun _ -> Ply DNull)
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DarkInternal" "cleanupOldTraces" 1
      parameters = []
      returnType = TFloat
      description = "Cleanup the old traces from a canvas"
      fn =
        internalFn (function
          | state, [] -> Ply(DFloat 0.0)
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DarkInternal" "cleanupOldTracesForCanvas" 1
      parameters = [ Param.make "canvas_id" TUuid "" ]
      returnType = TFloat
      description =
        "Cleanup the old traces for a specific canvas. Returns elapsed time in ms."
      fn =
        internalFn (function
          | state, [ DUuid canvas_id ] -> Ply(DFloat 0.0)
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DarkInternal" "checkCanvas" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TBool
      description = "Validate the canvas' opcodes"
      fn =
        internalFn (function
          | state, [ DStr host ] -> Ply DNull
          // FSTODO
          // (match Canvas.validate_host host with
          //  | Ok _ -> Ply(DBool true)
          //  | Error _ -> Ply(DBool false))
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "migrateCanvas" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TResult(varA, TStr)
      description = "Migrate a canvas' opcodes"
      fn =
        internalFn (function
          | state, [ DStr host ] -> Ply DNull
          // FSTODO
          // (match Canvas.migrate_host (Unicode_string.to_string host) with
          //  | Ok () -> DResult(Ok DNull)
          //  | Error msg -> DResult(Error(DStr msg)))
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "upsertUser" 0
      parameters =
        [ Param.make "username" TStr ""
          Param.make "email" TStr ""
          Param.make "name" TStr "" ]
      returnType = TStr
      description =
        "Add a user. Returns a password for the user, which was randomly generated. Usernames are unique: if you add the same username multiple times, it will overwrite the old settings (useful for changing password)."
      fn =
        internalFn (function
          | _, [ DStr username; DStr email; DStr name ] ->
            uply {
              let! result =
                Account.upsertNonAdmin
                  { username = UserName.create username
                    email = email
                    name = name
                    password = Password.invalid }
              match result with
              | Ok () -> return DStr ""
              | Error msg -> return Exception.raiseGrandUser msg
            }

          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DarkInternal" "insertUser" 1
      parameters =
        [ Param.make "username" TStr ""
          Param.make "email" TStr ""
          Param.make "name" TStr "" ]
      returnType = TResult(varA, TStr)
      description =
        "Add a user. Returns a result containing the password for the user,
which was randomly generated. Usernames are unique; if you try to add a username
that's already taken, returns an error."
      fn =
        internalFn (function
          | _, [ DStr username; DStr email; DStr name ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
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
          | _, [ DStr username; DStr email; DStr name; DObj analytics_metadata ] ->
            uply {
              let result =
                Account.insertUser username email name analytics_metadata ()
                |> Result.map (fun () ->
                  LibBackend.Analytics.identifyUser state.executionID username)
                |> Result.bind (fun () ->
                  let toCanvasName =
                    $"{username}-{LibService.Config.gettingStartedCanvasName}"
                  let fromCanvasName = LibService.Config.gettingStartedCanvasSource
                  do!
                    LibBackend.CanvasClone.cloneCanvas
                      (CanvasName.create fromCanvasName)
                      (CanvasName.create toCanvasName)
                      // Don't preserve history here, it isn't useful and
                      // we don't currently have visibility into canvas
                      // history, so we'd rather not share unknown sample-
                      // history with users in case it contains
                      // sensitive information like access keys.
                      false)
              return
                match result with
                | Ok () -> DResult(Ok(DStr ""))
                | Error msg -> DResult(Error(DStr msg))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "upsertUser" 1
      parameters =
        [ Param.make "username" TStr ""
          Param.make "email" TStr ""
          Param.make "name" TStr "" ]
      returnType = varA
      description =
        "Update a username's email or (human) name. WARNING: email must be kept in sync (manually, for now) with auth0!"
      fn =
        internalFn (function
          | _, [ DStr username; DStr email; DStr name ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "getUser" 0
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
                             ("email", DStr user.email) ])
                |> DOption
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
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
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "setAdmin" 0
      parameters = [ Param.make "username" TStr ""; Param.make "admin" TBool "" ]
      returnType = TNull
      description = "Set whether a user is an admin. Returns null."
      fn =
        internalFn (function
          | _, [ DStr username; DBool admin ] ->
            uply {
              do! Account.setAdmin admin (UserName.create username)
              // FSTODO: report to rollbar
              do! LibBackend.Analytics.identifyUser state.executionID username
              return DNull
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "schema" 0
      parameters = [ Param.make "host" TStr ""; Param.make "dbid" TStr "" ]
      returnType = varA
      description = "Return a schema for the db"
      fn =
        internalFn (function
          | _, [ DStr canvas_name; DStr tlid ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "canvasAsText" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TStr
      description = "TODO"
      fn =
        internalFn (function
          | _, [ DStr host ] ->
            (* Removed, no longer useful now that you can copy from Fluid *)
            Ply(DStr "")
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "handlers" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TList varA
      description = "Returns a list of toplevel ids of handlers in `host`"
      fn =
        internalFn (function
          | _, [ DStr host ] -> Ply DNull
          // let c =
          //   Canvas.load_all (Unicode_string.to_string host) []
          //   |> Result.map_error (String.concat ", ")
          //   |> Prelude.Result.ok_or_internal_exception "Canvas load error"
          // !c.handlers
          // |> IDMap.data
          // |> Ply.List.filterMapSequentially Libexecution.Toplevel.as_handler
          // |> List.map (fun h -> DStr(Libexecution.Types.string_of_id h.tlid))
          // |> fun l -> DList l
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "functions" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TList varA
      description = "Returns a list of toplevel ids of the functions in `host`"
      fn =
        internalFn (function
          | _, [ DStr host ] -> Ply DNull
          // let c =
          //   Canvas.load_all (Unicode_string.to_string host) []
          //   |> Result.map_error (String.concat ", ")
          //   |> Prelude.Result.ok_or_internal_exception "Canvas load error"
          // !c.user_functions
          // |> IDMap.data
          // |> List.map (fun fn -> DStr(Libexecution.Types.string_of_id fn.tlid))
          // |> fun l -> DList l
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "canLoadTraces" 0
      parameters = [ Param.make "host" TStr ""; Param.make "tlid" TStr "" ]
      returnType = TBool
      description =
        "Takes a <var host> and a <var tlid> and returns {{true}} iff we can load and parse traces for the handler identified by <var tlid>, and {{false}} otherwise."
      fn =
        internalFn (function
          | _, [ DStr host; DStr tlid ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "getCORSSetting" 0
      parameters = [ Param.make "canvas" TStr "" ]
      returnType = TOption(varA)
      description =
        "Given the full canvas name (including the username), get that canvas' global CORS setting."
      fn =
        internalFn (function
          | _, [ DStr host ] ->
            uply {
              let corsSettingToDval (setting : Canvas.CorsSetting option) : Dval =
                match setting with
                | None -> DOption None
                | Some Canvas.AllOrigins -> "*" |> DStr |> Some |> DOption
                | Some (Canvas.Origins os) ->
                  os |> List.map DStr |> DList |> Some |> DOption
              let! c = Canvas.getMeta (CanvasName.create host)
              let! cors = Canvas.fetchCORSSetting c.id
              return corsSettingToDval cors
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "setCORSSetting" 0
      parameters =
        [ Param.make "canvas" TStr ""; Param.make "origins" (TOption varA) "" ]
      returnType = TResult(varA, TStr)
      description =
        "Given the full canvas name (including the username) and an Option of either \"*\" or a list of string origins, set that value to that canvas' global CORS setting, so that it will be used in Access-Control-Allow-Origin response headers. Returns true if it worked and false if it didn't (likely meaning: the Dark value you passed in was invalid)."
      fn =
        internalFn (function
          | _, [ DStr host; DOption s ] ->
            uply {
              let corsSetting
                (opt : Option<Dval>)
                : Result<Option<Canvas.CorsSetting>, string> =
                // Error: error converting the dval to a cors setting.
                // Ok None: the dval is "unset the cors value"
                // Ok (Some cs): the dval is "set the cors setting to cs" *)
                try
                  match opt with
                  | None -> Ok None
                  | Some (DStr "*") -> Ok(Some Canvas.AllOrigins)
                  | Some (DList os) ->
                    os
                    |> List.map (function
                      | DStr v -> v
                      | _ -> Exception.raiseGrandUser "Invalid origin string")
                    |> Canvas.Origins
                    |> Some
                    |> Ok
                  | Some dv ->
                    Error(
                      "Received something other than an Nothing, Just [...], or Just \"*\": "
                      + DvalRepr.toDeveloperReprV0 dv
                    )
                with
                | e -> Error(string e)
              match corsSetting s with
              | Error e -> return e |> DStr |> Error |> DResult
              | Ok settings ->
                let! c = Canvas.getMeta (CanvasName.create host)
                do! Canvas.updateCorsSetting c.id settings
                return s |> DOption |> Ok |> DResult
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
                Sql.query
                  "SELECT tlid
                     FROM toplevel_oplists
                     JOIN canvases ON canvases.idescription = canvas_id
                    WHERE canvases.name = @name AND tipe = 'db'"
                |> Sql.parameters [ "name", Sql.string host ]
                |> Sql.executeAsync (fun read -> read.string "tlid")
              return dbTLIDs |> List.map DStr |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "oplistInfo" 0
      parameters = [ Param.make "host" TStr ""; Param.make "tlid" TStr "" ]
      returnType = varA
      description =
        "Returns the information from the toplevel_oplists table for the (host, tlid)"
      fn =
        internalFn (function
          | _, [ DStr host; DStr tlid_str ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "storedEvents" 0
      parameters = [ Param.make "host" TStr ""; Param.make "tlid" TStr "" ]
      returnType = TOption varA
      description =
        "Returns {{Just <var events>}}, where <var events> is the most recent stored events for the <param tlid> if it is a handler or {{Nothing}} if it is not."
      fn = internalFn (fun (_, _) -> Ply DNull)
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "pushStrollerEvent" 0
      parameters =
        [ Param.make "canvas_id" TStr ""
          Param.make "event" TStr ""
          Param.make "payload" varA "" ]
      returnType = TResult(varA, TStr)
      description = "Pushes an event to Stroller"
      fn = internalFn (fun _ -> Ply DNull)
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
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
                state.program.executionID
                (canvasID |> System.Guid.Parse)
                event
                (payload |> DvalRepr.toInternalRoundtrippableV0)
              Ply(DResult(Ok payload))
             with
             | e -> Ply(DResult(Error(e |> string |> DStr))))
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
              match Auth.SessionSync.username_of_key sessionKey with
              | None -> return DResult(Error(DStr "No session for cookie"))
              | Some username -> return DResult(Ok(DStr username))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "canvasIdOfCanvasName" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TOption varA
      description = "Gives canvasId for a canvasName/host"
      fn =
        internalFn (function
          | _, [ DStr host ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated

    }
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "grantsFor" 0
      parameters = [ Param.make "org" TStr "" ]
      returnType = TObj
      description =
        "Returns a dict mapping username->permission of users who have been granted permissions for a given auth_domain"
      fn =
        internalFn (function
          | _, [ DStr org ] ->
            let grants = Authorization.grants_for (Unicode_string.to_string org)
            grants
            |> List.fold DvalMap.empty (fun map (user, perm) ->
              DvalMap.insert
                user

                (perm |> Authorization.permission_to_string |> DStr)
                map)
            |> fun obj -> DObj obj
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "orgsFor" 0
      parameters = [ Param.make "username" TStr "" ]
      returnType = TObj
      description =
        "Returns a dict mapping orgs->permission to which the given `username` has been given permission"
      fn =
        internalFn (function
          | _, [ DStr username ] ->
            let orgs = Authorization.orgs_for (Unicode_string.to_string username)
            orgs
            |> List.fold DvalMap.empty (fun map (org, perm) ->
              DvalMap.insert
                org

                (perm |> Authorization.permission_to_string |> DStr)
                map)
            |> fun obj -> DObj obj
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "checkPermission" 0
      parameters = [ Param.make "username" TStr ""; Param.make "canvas" TStr "" ]
      returnType = TBool
      description = "Check a user's permissions for a particular canvas."
      fn =
        internalFn (function
          | _, [ DStr username; DStr canvas ] ->
            let auth_domain =
              Account.auth_domain_for (Unicode_string.to_string canvas)
            let username = Unicode_string.to_string username in
            (match Authorization.permission auth_domain username with
             | Some perm -> Authorization.permission_to_string perm
             | None -> "")
            |> DStr
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "log" 0
      parameters =
        [ Param.make "level" TStr ""
          Param.make "name" TStr ""
          Param.make "log" TObj "" ]
      returnType = TObj
      description =
        "Write the log object to a honeycomb log, along with whatever enrichment the backend provides."
      fn =
        internalFn (function
          | _, [ DStr level; DStr name; DObj log ] ->
            let name = name |> Unicode_string.to_string in
            (* Logs are important; if we get a level we can't parse, fall back to
             * `Info and also error log *)
            let levelStr = level |> Unicode_string.to_string in
            let level =
              levelStr
              |> Log.string_to_level_opt
              |> function
                | Some level -> level
                | None ->
                  Log.erroR
                    "DarkInternal::log no match"
                    [ ("input_level", levelStr); ("log_name", name) ]
                  Info
            (* We could just leave the dval vals as strings and use params, but
             * then we can't do numeric things (MAX, AVG, >, etc) with these
             * logs *)
            let jsonparams =
              log
              |> DvalMap.to_yojson (fun v ->
                v |> Dval.to_pretty_machine_json_v1 |> Yojson.Safe.from_string)
              |> function
                | Assoc jsonparams -> jsonparams
                | _ -> Exception.raiseInternal "Can't happen, bad log call"
            let log =
              log
              |> DvalMap.insert_no_override
                   "level"

                   (level |> Log.level_to_string |> DStr)
              |> DvalMap.insert_no_override "name" (name |> DStr)
            Log.pP level name jsonparams
            DObj log
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "fnsUsed" 0
      parameters = [ Param.make "host" TStr ""; Param.make "tlid" TStr "" ]
      returnType = TList varA
      description =
        "Iterates through all ops of the AST, returning for each op a list of the functions used in that op. The last value will be the functions currently used."
      fn =
        internalFn (function
          | _, [ DStr host; DStr tlid ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "fieldNamesUsed" 0
      parameters = [ Param.make "host" TStr ""; Param.make "tlid" TStr "" ]
      returnType = TList varA
      description =
        "Iterates through all ops of the AST, returning for each op a list of the field names used in that op. The last value will be the fieldnames in the current code."
      fn =
        internalFn (function
          | _, [ DStr host; DStr tlid ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "fnMetadata" 0
      parameters = [ Param.make "name" TStr "" ]
      returnType = TResult(varA, TStr)
      description =
        "Returns an object with the metadata of the built-in function name"
      fn =
        internalFn (function
          | _, [ DStr fnname ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
              (not (key.isInternalFn ())) && data.deprecated = NotDeprecated)
            |> List.map (fun (key, data) ->
              let alist =
                let returnType = DvalRepr.typeToBCTypeName data.returnType
                let parameters =
                  data.parameters
                  |> List.map (fun p ->
                    Dval.obj [ ("name", DStr p.name)
                               ("type", DStr(DvalRepr.typeToBCTypeName p.typ)) ])
                [ ("name", DStr(string key))
                  ("documentation", DStr data.description)
                  ("parameters", DList parameters)
                  ("returnType", DStr returnType) ]
              Dval.obj alist)
            |> DList
            |> Ply
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "clearStaticAssets" 0
      parameters = [ Param.make "host" TStr "" ]
      returnType = TNull
      description =
        "Deletes our record of static assets for a handler. Does not delete the data from the bucket. This is a hack for making Ellen's demo easier and should not be used for other uses in this form."
      fn =
        internalFn (function
          | _, [ DStr host ] -> Ply DNull
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
              let! rules = EventQueue.getAllSchedulingRules ()
              return rules |> List.map EventQueue.SchedulingRule.toDval |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "getSchedulingRulesForCanvas" 0
      parameters = [ Param.make "canvas_id" TUuid "" ]
      returnType = TList varA
      description =
        "Returns a list of all queue scheduling rules for the specified canvasID"
      fn =
        internalFn (function
          | _, [ DUuid canvasID ] ->
            uply {
              let! rules = EventQueue.getSchedulingRules canvasID
              return rules |> List.map EventQueue.SchedulingRule.toDval |> DList
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "addWorkerSchedulingBlock" 0
      parameters =
        [ Param.make "canvas_id" TUuid ""; Param.make "handler_name" TStr "" ]
      returnType = TNull
      description =
        "Add a worker scheduling 'block' for the given canvas and handler. This prevents any events for that handler from being scheduled until the block is manually removed."
      fn = modifySchedule EventQueue.blockWorker
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "removeWorkerSchedulingBlock" 0
      parameters =
        [ Param.make "canvas_id" TUuid ""; Param.make "handler_name" TStr "" ]
      returnType = TNull
      description =
        "Removes the worker scheduling block, if one exists, for the given canvas and handler. Enqueued events from this job will immediately be scheduled."
      fn = modifySchedule EventQueue.unblockWorker
      sqlSpec = NotYetImplementedTODO
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
              let username = UserName.create username
              let userID = Account.userIDForUserName username
              let session_key =
                try
                  Auth.SessionSync.new_for_username username |> Result.Ok
                with
                | e -> Result.fail e
              match session_key with
              | Ok session_key -> return DResult(Ok(DStr session_key))
              | Error e ->
                (* If session creation fails, log and rollbar *)
                // let err = Libexecution.Exception.exn_to_string e
                // FSTODO
                // Log.erroR
                //   "DarkInternal::newSessionForUsername"
                //   [ ("username", username); ("exception", err) ]
                // let bt = Libexecution.Exception.get_backtrace ()
                match
                  Rollbar.report e bt (Other "Darklang") (string state.execution_id)
                  with
                | Success
                | Disabled -> ()
                | Failure ->
                  Log.erroR "rollbar.report at DarkInternal::newSessionForUsername"
                return DResult(Error(DStr "Failed to create session"))
            }
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "DarkInternal" "newSessionForUsername" 1
      parameters = [ Param.make "username" TStr "" ]
      returnType = TResult
      description =
        (* We need the csrf token for dark-cli to use *)
        "If username is an existing user, puts a new session in the DB and returns the new sessionKey and csrfToken."
      fn =
        internalFn (function
          | exec_state, [ DStr username ] ->
            let username = Unicode_string.to_string username in

            (match Account.userIDForUserName username with
             | None -> DResult(Error(DStr("No user '" ^ username ^ "'")))
             | Some _user_id ->
               let session_key_and_csrf_token =
                 try
                   Auth.SessionSync.new_for_username_with_csrf_token username
                   |> Result.Ok
                 with
                 | e -> Result.fail e
               (match session_key_and_csrf_token with
                | Ok session ->
                  DResult(
                    Ok(
                      DObj(
                        DvalMap.from_list [ ("sessionKey", DStr sessionKey)
                                            ("csrfToken", DStr csrfToken) ]
                      )
                    )
                  )
                | Error e ->
                  (* If session creation fails, log and rollbar *)
                  let err = Libexecution.Exception.exn_to_string e
                  Log.erroR
                    "DarkInternal::newSessionForUsername_v1"
                    [ ("username", username); ("exception", err) ]
                  let bt = Libexecution.Exception.get_backtrace ()
                  (match
                    Rollbar.report
                      e
                      bt
                      (Other "Darklang")
                      (exec_state.execution_id |> Types.string_of_id)
                     with
                   | Success
                   | Disabled -> ()
                   | Failure ->
                     Log.erroR
                       "rollbar.report at DarkInternal::newSessionForUsername")
                  DResult(Error(DStr "Failed to create session"))))
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    { name = fn "DarkInternal" "getAndLogTableSizes" 0
      parameters = []
      returnType = TObj
      description =
        "Query the postgres database for the current size (disk + rowcount) of all
tables. This uses pg_stat, so it is fast but imprecise. This function is logged
in OCaml; its primary purpose is to send data to honeycomb, but also gives
human-readable data."
      fn =
        internalFn (function
          | exec_state, [] ->
            let table_stats = Db.table_stats ()
            (* Send logs to honeycomb. We could save some events by sending
                 * these all as a single event - tablename.disk = 1, etc - but
                 * by having an event per table, it's easier to query and graph:
                 * `VISUALIZE MAX(disk), MAX(rows);  GROUP BY relation`. (Also,
                 * if/when we add more tables, the graph-query doesn't need to
                 * be updated,)
                 *
                 * There are ~40k minutes/month, and 20 tables, so a 1/min cron
                 * would consume 80k of our 1.5B monthly events. That seems
                 * reasonable.
                 *
                 * The log statements all look like:
                 * {"timestamp":"2020-05-29T00:20:08.769420000Z","level":"INFO","name":"postgres_table_sizes","relation":"Total","disk_bytes":835584,"rows":139,"disk_human":"816 kB","rows_human":"139"}
                 * *)
            table_stats
            |> List.iter (fun ts ->
              Log.infO
                "postgres_table_sizes"
                [ ("relation", String ts.relation)
                  ("disk_bytes", Int ts.disk_bytes)
                  ("rows", Int ts.rows)
                  ("disk_human", String ts.disk_human)
                  ("rows_human", String ts.rows_human) ])
            (* Reformat a bit for human-usable dval output.
                 * Example from my local dev: {
                 *   Total: {
                 *     disk: 835584,
                 *     disk_human: "816 kB",
                 *     rows: 139,
                 *     rows_human: 139
                 *   },
                 *   access: {...},
                 *   ...
                 * } *)
            let table_stats_for_dobj =
              table_stats
              |> List.map (fun ts ->
                (ts.relation,
                 [ ("disk_bytes", DInt(Dint.of_int ts.disk_bytes))
                   ("rows", DInt(Dint.of_int ts.rows))
                   ("disk_human", DStr ts.disk_human)
                   ("rows_human", DStr ts.rows_human) ]
                 |> DvalMap.from_list
                 |> DObj))
            table_stats_for_dobj |> DvalMap.from_list |> DObj
          | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
