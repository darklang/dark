open Core_kernel
open Libcommon
open Libexecution
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
open Types
module Unicode = Libexecution.Unicode_string

(* Apply this to function to wrap that function in an InProcess that checks
 * permissions for the dark internal functions and logs status. *)
let internal_fn (f : exec_state * dval list -> dval) =
  InProcess
    (fun (es, params) ->
      match es.account_id |> Account.username_of_id with
      | None ->
          es.account_id
          |> Uuidm.to_string
          |> Format.sprintf "User not found with id: %s"
          |> Exception.code
      | Some username ->
          if Account.can_access_operations ~username
          then (
            Log.infO
              "internal_fn"
              ~params:[("user", username); ("status", "starting")] ;
            let result = f (es, params) in
            Log.infO
              "internal_fn"
              ~params:[("user", username); ("status", "finished")] ;
            result )
          else
            username
            |> Format.sprintf
                 "User executed an internal function but isn't an admin: %s"
            |> Exception.code )


let fns : Lib.shortfn list =
  [ { pns = ["DarkInternal::checkAccess"]
    ; ins = []
    ; p = []
    ; r = TNull
    ; d = "TODO"
    ; f = internal_fn (fun _ -> DNull)
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::endUsers"]
    ; ins = []
    ; p = []
    ; r = TList
    ; d =
        "Return a list of all user email addresses for non-admins and not in
@darklang.com or @example.com"
    ; f =
        internal_fn (function
            | _, [] ->
                Db.fetch
                  ~name:"fetch_end_users"
                  "SELECT email FROM accounts WHERE admin IS FALSE AND email NOT
LIKE '%@darklang.com' AND email NOT LIKE '%@example.com'"
                  ~params:[]
                |> List.map ~f:(function
                       | [email] ->
                           Dval.dstr_of_string_exn email
                       | _ ->
                           Exception.internal
                             "Wrong number of fields from db query" )
                |> DList
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::checkAllCanvases"]
    ; ins = []
    ; p = []
    ; r = TNull
    ; d = "TODO"
    ; f = internal_fn (fun _ -> DNull)
    ; ps = false
    ; dep = true }
  ; { pns = ["DarkInternal::migrateAllCanvases"]
    ; ins = []
    ; p = []
    ; r = TNull
    ; d = "TODO"
    ; f =
        internal_fn (fun _ ->
            Canvas.migrate_all_hosts () ;
            DNull )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::cleanupOldTraces"]
    ; ins = []
    ; p = []
    ; r = TNull
    ; d = "Deprecated, use v1"
    ; f = internal_fn (fun _ -> DNull)
    ; ps = false
    ; dep = true }
  ; { pns = ["DarkInternal::cleanupOldTraces_v1"]
    ; ins = []
    ; p = []
    ; r = TFloat
    ; d = "Cleanup the old traces from a canvas"
    ; f =
        internal_fn (function
            | state, [] ->
                DFloat (Canvas.cleanup_old_traces ())
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::checkCanvas"]
    ; ins = []
    ; p = [par "host" TStr]
    ; r = TBool
    ; d = "TODO"
    ; f =
        internal_fn (function
            | state, [DStr host] ->
              ( try
                  Canvas.validate_host (Unicode_string.to_string host) ;
                  DBool true
                with _ -> DBool false )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::upsertUser"]
    ; ins = []
    ; p = [par "username" TStr; par "email" TStr; par "name" TStr]
    ; r = TStr
    ; d =
        "Add a user. Returns a password for the user, which was randomly generated. Usernames are unique: if you add the same username multiple times, it will overwrite the old settings (useful for changing password)."
    ; f =
        internal_fn (function
            | _, [DStr username; DStr email; DStr name] ->
                let username = Unicode_string.to_string username in
                let email = Unicode_string.to_string email in
                let name = Unicode_string.to_string name in
                let result = Account.upsert_user ~username ~email ~name () in
                ( match result with
                | Ok password ->
                    Dval.dstr_of_string_exn password
                | Error msg ->
                    Exception.code msg )
            | args ->
                fail args )
    ; ps = false
    ; dep = true }
  ; { pns = ["DarkInternal::upsertUser_v1"]
    ; ins = []
    ; p = [par "username" TStr; par "email" TStr; par "name" TStr]
    ; r = TResult
    ; d =
        "Add a user. Returns a result containing the password for the user, which was randomly generated. Usernames are unique: if you add the same username multiple times, it will overwrite the old settings (useful for changing password)."
    ; f =
        internal_fn (function
            | _, [DStr username; DStr email; DStr name] ->
                let username = Unicode_string.to_string username in
                let email = Unicode_string.to_string email in
                let name = Unicode_string.to_string name in
                let result = Account.upsert_user ~username ~email ~name () in
                ( match result with
                | Ok password ->
                    DResult (ResOk (Dval.dstr_of_string_exn password))
                | Error msg ->
                    DResult (ResError (Dval.dstr_of_string_exn msg)) )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::getUser"]
    ; ins = []
    ; p = [par "username" TStr]
    ; r = TOption
    ; d = "Return a user for the username. Does not include passwords."
    ; f =
        internal_fn (function
            | _, [DStr username] ->
                let info =
                  Account.get_user (Unicode_string.to_string username)
                in
                ( match info with
                | None ->
                    DOption OptNothing
                | Some {username; name; email} ->
                    DOption
                      (OptJust
                         (Dval.to_dobj_exn
                            [ ("username", Dval.dstr_of_string_exn username)
                            ; ("name", Dval.dstr_of_string_exn name)
                            ; ("email", Dval.dstr_of_string_exn email) ])) )
            | args ->
                fail args )
    ; ps = false
    ; dep = true }
  ; { pns = ["DarkInternal::getUser_v1"]
    ; ins = []
    ; p = [par "username" TStr]
    ; r = TOption
    ; d = "Return a user for the username. Does not include passwords."
    ; f =
        internal_fn (function
            | _, [DStr username] ->
                let info =
                  Account.get_user (Unicode_string.to_string username)
                in
                ( match info with
                | None ->
                    DOption OptNothing
                | Some {username; name; admin; email} ->
                    DOption
                      (OptJust
                         (Dval.to_dobj_exn
                            [ ("username", Dval.dstr_of_string_exn username)
                            ; ("name", Dval.dstr_of_string_exn name)
                            ; ("email", Dval.dstr_of_string_exn email)
                            ; ("admin", DBool admin) ])) )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::setAdmin"]
    ; ins = []
    ; p = [par "username" TStr; par "admin" TBool]
    ; r = TNull
    ; d = "Set whether a user is an admin. Returns null."
    ; f =
        internal_fn (function
            | _, [DStr username; DBool admin] ->
                Account.set_admin (Unicode_string.to_string username) admin ;
                DNull
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::getUsers"]
    ; ins = []
    ; p = []
    ; r = TList
    ; d = "Return a list of username of all the accounts in Dark."
    ; f =
        internal_fn (function
            | _, [] ->
                Account.get_users ()
                |> List.map ~f:Dval.dstr_of_string_exn
                |> DList
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::getAllCanvases"]
    ; ins = []
    ; p = []
    ; r = TList
    ; d = "TODO"
    ; f =
        internal_fn (fun _ ->
            Serialize.current_hosts ()
            |> List.map ~f:Dval.dstr_of_string_exn
            |> DList )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::schema"]
    ; ins = []
    ; p = [par "host" TStr; par "dbid" TStr]
    ; r = TObj
    ; d = "Return a schema for the db"
    ; f =
        internal_fn (function
            | _, [DStr canvas_name; DStr tlid] ->
                let tlid = Unicode_string.to_string tlid in
                let canvas_name = Unicode_string.to_string canvas_name in
                let c =
                  Canvas.load_only_tlids
                    ~tlids:[Types.id_of_string tlid]
                    canvas_name
                    []
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception
                       "Canvas load error"
                in
                let db =
                  !c.dbs
                  |> IDMap.data
                  |> List.filter_map ~f:Libexecution.Toplevel.as_db
                  |> List.find ~f:(fun d ->
                         Libexecution.Types.string_of_id d.tlid = tlid )
                in
                ( match db with
                | Some db ->
                    User_db.cols_for db
                    |> List.map ~f:(fun (k, v) ->
                           ( canvas_name
                             ^ "-"
                             ^ Ast.blank_to_string db.name
                             ^ "-"
                             ^ k
                           , Dval.dstr_of_string_exn (Dval.tipe_to_string v) )
                       )
                    |> Dval.to_dobj_exn
                | None ->
                    Dval.to_dobj_exn [] )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::canvasAsText"]
    ; ins = []
    ; p = [par "host" TStr]
    ; r = TStr
    ; d = "TODO"
    ; f =
        internal_fn (function
            | _, [DStr host] ->
                Dval.dstr_of_string_exn
                  (Canvas.to_string (Unicode_string.to_string host))
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::handlers"]
    ; ins = []
    ; p = [par "host" TStr]
    ; r = TList
    ; d = "Returns a list of toplevel ids of handlers in `host`"
    ; f =
        internal_fn (function
            | _, [DStr host] ->
                let c =
                  Canvas.load_all (Unicode_string.to_string host) []
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception
                       "Canvas load error"
                in
                !c.handlers
                |> IDMap.data
                |> List.filter_map ~f:Libexecution.Toplevel.as_handler
                |> List.map ~f:(fun h ->
                       Dval.dstr_of_string_exn
                         (Libexecution.Types.string_of_id h.tlid) )
                |> fun l -> DList l
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::functions"]
    ; ins = []
    ; p = [par "host" TStr]
    ; r = TList
    ; d = "Returns a list of toplevel ids of the functions in `host`"
    ; f =
        internal_fn (function
            | _, [DStr host] ->
                let c =
                  Canvas.load_all (Unicode_string.to_string host) []
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception
                       "Canvas load error"
                in
                !c.user_functions
                |> IDMap.data
                |> List.map ~f:(fun fn ->
                       Dval.dstr_of_string_exn
                         (Libexecution.Types.string_of_id fn.tlid) )
                |> fun l -> DList l
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::canLoadTraces"]
    ; ins = []
    ; p = [par "host" TStr; par "tlid" TStr]
    ; r = TBool
    ; d =
        "Takes a `host` and a `tlid` and returns true iff. we can load+parse traces for the handler identified by `tlid`, and false otherwise"
    ; f =
        internal_fn (function
            | _, [DStr host; DStr tlid] ->
              ( try
                  let open Libexecution in
                  let tlid = Unicode_string.to_string tlid in
                  let c =
                    Canvas.load_only_tlids
                      (Unicode_string.to_string host)
                      ~tlids:[Types.id_of_string tlid]
                      []
                    |> Result.map_error ~f:(String.concat ~sep:", ")
                    |> Prelude.Result.ok_or_internal_exception
                         "Canvas load error"
                  in
                  let handler =
                    !c.handlers
                    |> IDMap.data
                    |> List.map ~f:Toplevel.as_handler
                    |> List.map ~f:(fun h -> Option.value_exn h)
                    |> List.find_exn ~f:(fun h ->
                           h.tlid = Types.id_of_string tlid )
                  in
                  Analysis.traceids_for_handler !c handler
                  |> List.map ~f:(Analysis.handler_trace !c handler)
                  |> ignore ;
                  DBool true
                with _ -> DBool false )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::getCORSSetting"]
    ; ins = []
    ; p = [par "canvas" TStr]
    ; r = TOption
    ; d =
        "Given the full canvas name (including the username), get that canvas' global CORS setting."
    ; f =
        internal_fn (function
            | _, [DStr host] ->
                let cors_setting_to_dval (setting : Canvas.cors_setting option)
                    : dval =
                  match setting with
                  | None ->
                      DOption OptNothing
                  | Some AllOrigins ->
                      "*" |> Dval.dstr_of_string_exn |> OptJust |> DOption
                  | Some (Origins os) ->
                      os
                      |> List.map ~f:Dval.dstr_of_string_exn
                      |> DList
                      |> OptJust
                      |> DOption
                in
                let canvas =
                  Canvas.load_without_tls (Unicode.to_string host)
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception
                       "Canvas load error"
                in
                !canvas.cors_setting |> cors_setting_to_dval
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::setCORSSetting"]
    ; ins = []
    ; p = [par "canvas" TStr; par "origins" TOption]
    ; r = TResult
    ; d =
        "Given the full canvas name (including the username) and an Option of either \"*\" or a list of string origins, set that value to that canvas' global CORS setting, so that it will be used in Access-Control-Allow-Origin response headers. Returns true if it worked and false if it didn't (likely meaning: the Dark value you passed in was invalid)."
    ; f =
        internal_fn (function
            | _, [DStr host; DOption s] ->
                let cors_setting (opt : optionT) :
                    (Canvas.cors_setting option, string) result =
                  (* Error: error converting the dval to a cors setting.
                   * Ok None: the dval is "unset the cors value"
                   * Ok (Some cs): the dval is "set the cors setting to cs" *)
                  try
                    match opt with
                    | OptNothing ->
                        Ok None
                    | OptJust (DStr s) when Unicode.to_string s = "*" ->
                        Ok (Some Canvas.AllOrigins)
                    | OptJust (DList os) ->
                        os
                        |> List.map ~f:Dval.to_string_exn
                        |> Canvas.Origins
                        |> Some
                        |> Ok
                    | OptJust dv ->
                        Error
                          ( "Received something other than an Nothing, Just [...], or Just \"*\": "
                          ^ Dval.to_developer_repr_v0 dv )
                  with e -> Error (Exception.exn_to_string e)
                in
                ( match cors_setting s with
                | Error e ->
                    e |> Dval.dstr_of_string_exn |> ResError |> DResult
                | Ok settings ->
                    let canvas =
                      Canvas.load_without_tls (Unicode.to_string host)
                      |> Result.map_error ~f:(String.concat ~sep:", ")
                      |> Prelude.Result.ok_or_internal_exception
                           "Canvas load error"
                    in
                    Canvas.update_cors_setting canvas settings ;
                    s |> DOption |> ResOk |> DResult )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::dbs"]
    ; ins = []
    ; p = [par "host" TStr]
    ; r = TList
    ; d = "Returns a list of toplevel ids of dbs in `host`"
    ; f =
        internal_fn (function
            | _, [DStr host] ->
                let db_tlids =
                  Db.fetch
                    ~name:"dbs_in_canvas"
                    "SELECT tlid
                     FROM toplevel_oplists
                     JOIN canvases ON canvases.id = canvas_id
                     WHERE canvases.name = $1 AND tipe = 'db'"
                    ~params:[String (Unicode_string.to_string host)]
                  |> List.fold ~init:[] ~f:(fun acc e -> e @ acc)
                in
                db_tlids
                |> List.map ~f:(fun s -> DStr (Unicode_string.of_string_exn s))
                |> fun l -> DList l
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::oplistInfo"]
    ; ins = []
    ; p = [par "host" TStr; par "tlid" TStr]
    ; r = TObj
    ; d =
        "Returns the information from the toplevel_oplists table for the (host, tlid)"
    ; f =
        internal_fn (function
            | _, [DStr host; DStr tlid_str] ->
                let account =
                  Account.for_host_exn (Unicode_string.to_string host)
                in
                let canvas_id =
                  Serialize.fetch_canvas_id
                    account
                    (Unicode_string.to_string host)
                in
                let tlid =
                  Types.id_of_string (Unicode_string.to_string tlid_str)
                in
                let strings =
                  Db.fetch
                    ~name:"toplevel_metadata"
                    "SELECT canvas_id, account_id, tlid, tipe, name, module, modifier, created_at, updated_at
                     FROM toplevel_oplists
                     WHERE canvas_id = $1 AND tlid = $2"
                    ~params:[Uuid canvas_id; ID tlid]
                in
                let zipped =
                  strings
                  |> List.hd_exn
                  |> List.map ~f:Dval.dstr_of_string_exn
                  |> List.zip_exn
                       [ "canvas_id"
                       ; "account_id"
                       ; "tlid"
                       ; "tipe"
                       ; "name"
                       ; "module"
                       ; "modifier"
                       ; "created_at"
                       ; "updated_at" ]
                  |> DvalMap.from_list
                in
                let convert_to_date key obj =
                  DvalMap.update obj ~key ~f:(fun v ->
                      match v with
                      | Some (DStr s) ->
                          s
                          |> Unicode_string.to_string
                          |> Db.date_of_sqlstring
                          |> fun d -> Some (DDate d)
                      | _ ->
                          None )
                in
                zipped
                |> convert_to_date "created_at"
                |> convert_to_date "updated_at"
                |> fun o -> DObj o
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::storedEvents"]
    ; ins = []
    ; p = [par "host" TStr; par "tlid" TStr]
    ; r = TOption
    ; d =
        "Returns Just most recent stored events for the tlid if it is a handleror Nothing if it is not"
    ; f =
        internal_fn (function
            | _, [DStr host; DStr tlid_str] ->
                let tlid =
                  Types.id_of_string (Unicode_string.to_string tlid_str)
                in
                let canvas : Canvas.canvas ref =
                  Canvas.load_only_tlids
                    ~tlids:[tlid]
                    (Unicode_string.to_string host)
                    []
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception
                       "Canvas load error"
                in
                let desc =
                  !canvas.handlers
                  |> IDMap.data
                  |> List.filter_map ~f:Toplevel.as_handler
                  |> List.filter ~f:(fun h -> h.tlid = tlid)
                  |> List.hd
                  |> Option.bind ~f:Handler.event_desc_for
                in
                ( match desc with
                | None ->
                    DOption OptNothing
                | Some d ->
                    let events = Stored_event.load_events !canvas.id d in
                    let event_list =
                      events
                      |> List.map ~f:(fun (path, traceid, time, data) ->
                             [ ("path", Dval.dstr_of_string_exn path)
                             ; ("traceid", DUuid traceid)
                             ; ("time", DDate time)
                             ; ("event", data) ]
                             |> DvalMap.from_list
                             |> fun o -> DObj o )
                      |> fun l -> DList l
                    in
                    DOption (OptJust event_list) )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::pushStrollerEvent"]
    ; ins = []
    ; p = [par "canvas_id" TStr; par "event" TStr; par "payload" TObj]
    ; r = TResult
    ; d = "Pushes an event to Stroller"
    ; f =
        internal_fn (function
            | exec_state, [DStr canvas_id; DStr event; DObj payload] ->
              ( try
                  Stroller.push_new_event
                    ~execution_id:exec_state.execution_id
                    ~canvas_id:
                      ( canvas_id
                      |> Unicode_string.to_string
                      |> Uuidm.of_string
                      |> Option.value_exn )
                    ~event:(event |> Unicode_string.to_string)
                    (payload |> DObj |> Dval.to_internal_roundtrippable_v0) ;
                  DResult (ResOk (DObj payload))
                with e ->
                  DResult
                    (ResError
                       (e |> Exception.to_string |> Dval.dstr_of_string_exn))
              )
            | args ->
                fail args )
    ; ps = false
    ; dep = true }
  ; { pns = ["DarkInternal::pushStrollerEvent_v1"]
    ; ins = []
    ; p = [par "canvas_id" TStr; par "event" TStr; par "payload" TAny]
    ; r = TResult
    ; d = "Pushes an event to Stroller"
    ; f =
        internal_fn (function
            | exec_state, [DStr canvas_id; DStr event; payload] ->
              ( try
                  Stroller.push_new_event
                    ~execution_id:exec_state.execution_id
                    ~canvas_id:
                      ( canvas_id
                      |> Unicode_string.to_string
                      |> Uuidm.of_string
                      |> Option.value_exn )
                    ~event:(event |> Unicode_string.to_string)
                    (payload |> Dval.to_internal_roundtrippable_v0) ;
                  DResult (ResOk payload)
                with e ->
                  DResult
                    (ResError
                       (e |> Exception.to_string |> Dval.dstr_of_string_exn))
              )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::sessionKeyToUsername"]
    ; ins = []
    ; p = [par "sessionKey" TStr]
    ; r = TOption
    ; d = "Looks up the username for a session_key"
    ; f =
        internal_fn (function
            | _, [DStr sessionKey] ->
                let sessionKey = sessionKey |> Unicode_string.to_string in
                ( match Auth.Session.username_of_key sessionKey with
                | None ->
                    DResult
                      (ResError
                         (Dval.dstr_of_string_exn "No session for cookie"))
                | Some username ->
                    DResult (ResOk (Dval.dstr_of_string_exn username)) )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::canvasIdOfCanvasName"]
    ; ins = []
    ; p = [par "host" TStr]
    ; r = TOption
    ; d = "Gives canvasId for a canvasName/host"
    ; f =
        internal_fn (function
            | _, [DStr host] ->
                let host = Unicode_string.to_string host in
                Db.fetch_one_option
                  ~name:"canvas_id_of_canvas_name"
                  "SELECT id FROM canvases WHERE name = $1"
                  ~params:[Db.String host]
                |> (function
                | Some [s] ->
                    DOption (OptJust (Dval.dstr_of_string_exn s))
                | None | _ ->
                    DOption OptNothing)
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::usernameToUserInfo"]
    ; ins = []
    ; p = [par "username" TStr]
    ; r = TOption
    ; d = "Gives userinfo {username, name, admin, email} for a username"
    ; f =
        internal_fn (function
            | _, [DStr username] ->
                let username = Unicode_string.to_string username in
                ( match Account.get_user username with
                | None ->
                    DOption OptNothing
                | Some user_info ->
                    DvalMap.from_list
                      [ ("username", Dval.dstr_of_string_exn user_info.username)
                      ; ("email", Dval.dstr_of_string_exn user_info.email)
                      ; ("name", Dval.dstr_of_string_exn user_info.name)
                      ; ("admin", DBool user_info.admin) ]
                    |> DObj
                    |> OptJust
                    |> DOption )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::grant"]
    ; ins = []
    ; p = [par "username" TStr; par "org" TStr; par "permission" TStr]
    ; r = TResult
    ; d = "Set a user's permissions for a particular auth_domain."
    ; f =
        internal_fn (function
            | _, [DStr username; DStr org; DStr permission] ->
                let result_to_dval r =
                  match r with
                  | Ok x ->
                      DResult (ResOk x)
                  | Error x ->
                      DResult (ResError (Dval.dstr_of_string_exn x))
                in
                let user_id =
                  username
                  |> Unicode_string.to_string
                  |> Account.id_of_username
                  |> Result.of_option ~error:"no such user?"
                in
                let org_id =
                  org
                  |> Unicode_string.to_string
                  |> Account.id_of_username
                  |> Result.of_option ~error:"no such org?"
                in
                let permission =
                  match Unicode_string.to_string permission with
                  | "rw" ->
                      Ok (Some Authorization.ReadWrite)
                  | "r" ->
                      Ok (Some Authorization.Read)
                  | "" ->
                      Ok None
                  | _ ->
                      Error "can't decode permission string"
                in
                ( match (user_id, org_id, permission) with
                | Ok user_id, Ok org_id, Ok permission ->
                    Authorization.set_user_access user_id org_id permission ;
                    Ok (Dval.dstr_of_string_exn "success!")
                | Error e, _, _ | _, Error e, _ | _, _, Error e ->
                    Error e )
                |> result_to_dval
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::checkPermission"]
    ; ins = []
    ; p = [par "username" TStr; par "canvas" TStr]
    ; r = TBool
    ; d = "Check a user's permissions for a particular canvas."
    ; f =
        internal_fn (function
            | _, [DStr username; DStr canvas] ->
                let auth_domain =
                  Account.auth_domain_for (Unicode_string.to_string canvas)
                in
                let username = Unicode_string.to_string username in
                ( match Authorization.permission ~auth_domain ~username with
                | Some ReadWrite ->
                    "rw"
                | Some Read ->
                    "r"
                | None ->
                    "" )
                |> Dval.dstr_of_string_exn
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::log"]
    ; ins = []
    ; p = [par "level" TStr; par "name" TStr; par "log" TObj]
    ; r = TObj
    ; d =
        "Write the log object to a honeycomb log, along with whatever enrichment the backend provides."
    ; f =
        internal_fn (function
            | _, [DStr level; DStr name; DObj log] ->
                let name = name |> Unicode_string.to_string in
                (* Logs are important; if we get a level we can't parse, fall back to
             * `Info and also error log *)
                let levelStr = level |> Unicode_string.to_string in
                let level =
                  levelStr
                  |> Log.string_to_level_opt
                  |> function
                  | Some level ->
                      level
                  | None ->
                      Log.erroR
                        "DarkInternal::log no match"
                        ~params:[("input_level", levelStr); ("log_name", name)] ;
                      `Info
                in
                (* We could just leave the dval vals as strings and use ~params, but
             * then we can't do numeric things (MAX, AVG, >, etc) with these
             * logs *)
                let jsonparams =
                  log
                  |> DvalMap.to_yojson (fun v ->
                         v
                         |> Dval.to_pretty_machine_json_v1
                         |> Yojson.Safe.from_string )
                  |> function
                  | `Assoc jsonparams ->
                      jsonparams
                  | _ ->
                      Exception.internal "Can't happen, bad log call"
                in
                let log =
                  log
                  |> DvalMap.insert_no_override
                       ~key:"level"
                       ~value:
                         ( level
                         |> Log.level_to_string
                         |> Dval.dstr_of_string_exn )
                  |> DvalMap.insert_no_override
                       ~key:"name"
                       ~value:(name |> Dval.dstr_of_string_exn)
                in
                Log.pP ~level name ~jsonparams ;
                DObj log
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::fnsUsed"]
    ; ins = []
    ; p = [par "host" TStr; par "tlid" TStr]
    ; r = TList
    ; d =
        "Iterates through all ops of the AST, returning for each op a list of the functions used in that op. The last value will be the functions currently used."
    ; f =
        internal_fn (function
            | _, [DStr host; DStr tlid] ->
                let host = Unicode_string.to_string host in
                let owner = Account.for_host_exn host in
                let canvas_id = Serialize.fetch_canvas_id owner host in
                let tlids = [Unicode_string.to_string tlid |> id_of_string] in
                let ops =
                  Serialize.load_only_tlids ~tlids ~host ~canvas_id ()
                  |> List.hd_exn
                  |> Tablecloth.Tuple2.second
                in
                ops
                |> List.filter_map ~f:Op.ast_of
                |> List.filter_map ~f:(fun ast ->
                       ast
                       |> Internal_analysis.find_functions
                       |> List.map ~f:Dval.dstr_of_string_exn
                       |> DList
                       |> Some )
                |> DList
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::fieldNamesUsed"]
    ; ins = []
    ; p = [par "host" TStr; par "tlid" TStr]
    ; r = TList
    ; d =
        "Iterates through all ops of the AST, returning for each op a list of the field names used in that op. The last value will be the fieldnames in the current code."
    ; f =
        internal_fn (function
            | _, [DStr host; DStr tlid] ->
                let host = Unicode_string.to_string host in
                let owner = Account.for_host_exn host in
                let canvas_id = Serialize.fetch_canvas_id owner host in
                let tlids = [Unicode_string.to_string tlid |> id_of_string] in
                let ops =
                  Serialize.load_only_tlids ~tlids ~host ~canvas_id ()
                  |> List.hd_exn
                  |> Tablecloth.Tuple2.second
                in
                ops
                |> List.filter_map ~f:Op.ast_of
                |> List.filter_map ~f:(fun ast ->
                       ast
                       |> Internal_analysis.find_fields
                       |> List.map ~f:Dval.dstr_of_string_exn
                       |> DList
                       |> Some )
                |> DList
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::fnMetadata"]
    ; ins = []
    ; p = [par "name" TStr]
    ; r = TResult
    ; d = "Returns an object with the metadata of the built-in function name"
    ; f =
        internal_fn (function
            | _, [DStr fnname] ->
                let fnname = Unicode_string.to_string fnname in
                let fn =
                  Prelude.StrDict.get ~key:fnname !Libexecution.Libs.static_fns
                in
                ( match fn with
                | Some fn ->
                    [ ("name", Dval.dstr_of_string_exn fnname)
                    ; ("deprecated", DBool fn.deprecated) ]
                    |> DvalMap.from_list
                    |> DObj
                    |> ResOk
                    |> DResult
                | None ->
                    DResult
                      (ResError (Dval.dstr_of_string_exn "function not found"))
                )
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::allFunctions"]
    ; ins = []
    ; p = []
    ; r = TList
    ; d =
        "Returns a list of objects, representing the functions available in the standard library. Does not return DarkInternal functions"
    ; f =
        internal_fn (function
            | _, [] ->
                let fns =
                  String.Map.fold
                    ~init:[]
                    ~f:(fun ~key ~data acc ->
                      if String.is_prefix ~prefix:"DarkInternal::" key
                         || data.deprecated
                      then acc
                      else
                        let alist =
                          let returnType =
                            Dval.tipe_to_string data.return_type
                          in
                          let parameters =
                            data.parameters
                            |> List.map ~f:(fun p ->
                                   Dval.to_dobj_exn
                                     [ ("name", Dval.dstr_of_string_exn p.name)
                                     ; ( "type"
                                       , Dval.dstr_of_string_exn
                                           (Dval.tipe_to_string p.tipe) ) ] )
                          in
                          [ ("name", Dval.dstr_of_string_exn key)
                          ; ( "documentation"
                            , Dval.dstr_of_string_exn data.description )
                          ; ("parameters", DList parameters)
                          ; ("returnType", Dval.dstr_of_string_exn returnType)
                          ]
                        in
                        Dval.to_dobj_exn alist :: acc )
                    !Libexecution.Libs.static_fns
                in
                DList fns
            | args ->
                fail args )
    ; ps = false
    ; dep = false }
  ; { pns = ["DarkInternal::clearStaticAssets"]
    ; ins = []
    ; p = []
    ; r = TNull
    ; d =
        "Deletes our record of static assets for a handler. Does not delete the data from the bucket. This is a hack for making Ellen's demo easier and should not be used for other uses in this form."
    ; f =
        internal_fn (function
            | _, [DStr host] ->
                let host = Unicode_string.to_string host in
                let owner = Account.for_host_exn host in
                let canvas_id = Serialize.fetch_canvas_id owner host in
                Static_assets.delete_assets_for_ellens_demo canvas_id ;
                DNull
            | args ->
                fail args )
    ; ps = false
    ; dep = false } ]
