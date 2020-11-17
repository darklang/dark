open Core_kernel
open Libcommon
open Libexecution
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
open Types
module Op = Libserialize.Op
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
            |> Exception.code)


let modify_schedule fn =
  internal_fn (function
      | state, [DUuid canvas_id; DStr handler_name] ->
          Unicode_string.to_string handler_name |> fn canvas_id ;
          let s = Event_queue.get_worker_schedules_for_canvas canvas_id in
          Stroller.push_worker_states
            ~execution_id:state.execution_id
            ~canvas_id
            s ;
          DNull
      | args ->
          fail args)


let fns : fn list =
  [ { prefix_names = ["DarkInternal::checkAccess"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TNull
    ; description = "TODO"
    ; func = internal_fn (fun _ -> DNull)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::endUsers"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TList
    ; description =
        "Return a <type list> of all user email addresses for non-admins and not in @darklang.com or @example.com"
    ; func =
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
                             "Wrong number of fields from db query")
                |> DList
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::checkAllCanvases"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TNull
    ; description = "TODO"
    ; func = internal_fn (fun _ -> DNull)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::migrateAllCanvases"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TNull
    ; description = "REMOVED"
    ; func = internal_fn (fun _ -> DNull)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::cleanupOldTraces"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TNull
    ; description = "Deprecated, use v1"
    ; func = internal_fn (fun _ -> DNull)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::cleanupOldTraces_v1"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TFloat
    ; description = "Cleanup the old traces from a canvas"
    ; func =
        internal_fn (function state, [] -> DFloat 0.0 | args -> fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::cleanupOldTracesForCanvas_v1"]
    ; infix_names = []
    ; parameters = [par "canvas_id" TUuid]
    ; return_type = TFloat
    ; description =
        "Cleanup the old traces for a specific canvas. Returns elapsed time in ms."
    ; func =
        internal_fn (function
            | state, [DUuid canvas_id] ->
                DFloat 0.0
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::checkCanvas"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TBool
    ; description = "Validate the canvas' opcodes"
    ; func =
        internal_fn (function
            | state, [DStr host] ->
                let open Prelude.Result in
                ( match Canvas.validate_host (Unicode_string.to_string host) with
                | Ok _ ->
                    DBool true
                | Error _ ->
                    DBool false )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::migrateCanvas"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TResult
    ; description = "Migrate a canvas' opcodes"
    ; func =
        internal_fn (function
            | state, [DStr host] ->
                let open Prelude.Result in
                ( match Canvas.migrate_host (Unicode_string.to_string host) with
                | Ok () ->
                    DResult (ResOk DNull)
                | Error msg ->
                    DResult (ResError (Dval.dstr_of_string_exn msg)) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::upsertUser"]
    ; infix_names = []
    ; parameters = [par "username" TStr; par "email" TStr; par "name" TStr]
    ; return_type = TStr
    ; description =
        "Add a user. Returns a password for the user, which was randomly generated. Usernames are unique: if you add the same username multiple times, it will overwrite the old settings (useful for changing password)."
    ; func =
        internal_fn (function
            | _, [DStr username; DStr email; DStr name] ->
                let username = Unicode_string.to_string username in
                let email = Unicode_string.to_string email in
                let name = Unicode_string.to_string name in
                let result = Account.upsert_user ~username ~email ~name () in
                ( match result with
                | Ok () ->
                    Dval.dstr_of_string_exn ""
                | Error msg ->
                    Exception.code msg )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::insertUser_v1"]
    ; infix_names = []
    ; parameters = [par "username" TStr; par "email" TStr; par "name" TStr]
    ; return_type = TResult
    ; description =
        "Add a user. Returns a result containing the password for the user,
which was randomly generated. Usernames are unique; if you try to add a username
that's already taken, returns an error."
    ; func =
        internal_fn (function
            | _, [DStr username; DStr email; DStr name] ->
                let username = Unicode_string.to_string username in
                let email = Unicode_string.to_string email in
                let name = Unicode_string.to_string name in
                let result =
                  Account.insert_user ~username ~email ~name ()
                  |> Result.map ~f:(fun r ->
                         Stroller.heapio_identify_user username ;
                         r)
                in
                ( match result with
                | Ok () ->
                    DResult (ResOk (Dval.dstr_of_string_exn ""))
                | Error msg ->
                    DResult (ResError (Dval.dstr_of_string_exn msg)) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::insertUser_v2"]
    ; infix_names = []
    ; parameters =
        [ par "username" TStr
        ; par "email" TStr
        ; par "name" TStr
        ; par "analytics_metadata" TObj ]
    ; return_type = TResult
    ; description =
        "Add a user. Returns a result containing the password for the user,
which was randomly generated. Usernames are unique; if you try to add a username
that's already taken, returns an error."
    ; func =
        internal_fn (function
            | _, [DStr username; DStr email; DStr name; DObj analytics_metadata]
              ->
                let username = Unicode_string.to_string username in
                let email = Unicode_string.to_string email in
                let name = Unicode_string.to_string name in
                let result =
                  Account.insert_user
                    ~username
                    ~email
                    ~name
                    ~analytics_metadata
                    ()
                  |> Result.map ~f:(fun () ->
                         Stroller.heapio_identify_user username)
                  |> Result.bind ~f:(fun () ->
                         let to_canvas_name =
                           username
                           ^ "-"
                           ^ Libservice.Config.getting_started_canvas_name
                         in
                         let from_canvas_name =
                           Libservice.Config.getting_started_canvas_source
                         in
                         Canvas_clone.clone_canvas
                           ~from_canvas_name
                           ~to_canvas_name
                             (* Don't preserve history here, it isn't useful and
                              * we don't currently have visibility into canvas
                              * history, so we'd rather not share unknown sample-
                              * history with users in case it contains
                              * sensitive information like access keys. *)
                           ~preserve_history:false)
                in
                ( match result with
                | Ok () ->
                    DResult (ResOk (Dval.dstr_of_string_exn ""))
                | Error msg ->
                    DResult (ResError (Dval.dstr_of_string_exn msg)) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::upsertUser_v1"]
    ; infix_names = []
    ; parameters = [par "username" TStr; par "email" TStr; par "name" TStr]
    ; return_type = TResult
    ; description =
        "Update a username's email or (human) name. WARNING: email must be kept in sync (manually, for now) with auth0!"
    ; func =
        internal_fn (function
            | _, [DStr username; DStr email; DStr name] ->
                let username = Unicode_string.to_string username in
                let email = Unicode_string.to_string email in
                let name = Unicode_string.to_string name in
                let result =
                  Account.upsert_user ~username ~email ~name ()
                  |> Result.map ~f:(fun r ->
                         Stroller.heapio_identify_user username ;
                         r)
                in
                ( match result with
                | Ok () ->
                    DResult (ResOk (Dval.dstr_of_string_exn ""))
                | Error msg ->
                    DResult (ResError (Dval.dstr_of_string_exn msg)) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getUser"]
    ; infix_names = []
    ; parameters = [par "username" TStr]
    ; return_type = TOption
    ; description =
        "Return a user for the username. Does not include passwords."
    ; func =
        internal_fn (function
            | _, [DStr username] ->
                let info =
                  Account.get_user (Unicode_string.to_string username)
                in
                ( match info with
                | None ->
                    DOption OptNothing
                | Some {username; name; email; admin = _; id = _} ->
                    DOption
                      (OptJust
                         (Dval.to_dobj_exn
                            [ ("username", Dval.dstr_of_string_exn username)
                            ; ("name", Dval.dstr_of_string_exn name)
                            ; ("email", Dval.dstr_of_string_exn email) ])) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::getUser_v1"]
    ; infix_names = []
    ; parameters = [par "username" TStr]
    ; return_type = TOption
    ; description =
        "Return a user for the username. Does not include passwords."
    ; func =
        internal_fn (function
            | _, [DStr username] ->
                let info =
                  Account.get_user (Unicode_string.to_string username)
                in
                ( match info with
                | None ->
                    DOption OptNothing
                | Some {username; name; admin; email; id = _} ->
                    DOption
                      (OptJust
                         (Dval.to_dobj_exn
                            [ ("username", Dval.dstr_of_string_exn username)
                            ; ("name", Dval.dstr_of_string_exn name)
                            ; ("email", Dval.dstr_of_string_exn email)
                            ; ("admin", DBool admin) ])) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getUserByEmail"]
    ; infix_names = []
    ; parameters = [par "email" TStr]
    ; return_type = TOption
    ; description = "Return a user for the email. Does not include passwords."
    ; func =
        internal_fn (function
            | _, [DStr email] ->
                let info =
                  Account.get_user_by_email (Unicode_string.to_string email)
                in
                ( match info with
                | None ->
                    DOption OptNothing
                | Some {username; name; admin; email; id = _} ->
                    DOption
                      (OptJust
                         (Dval.to_dobj_exn
                            [ ("username", Dval.dstr_of_string_exn username)
                            ; ("name", Dval.dstr_of_string_exn name)
                            ; ("email", Dval.dstr_of_string_exn email)
                            ; ("admin", DBool admin) ])) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::setAdmin"]
    ; infix_names = []
    ; parameters = [par "username" TStr; par "admin" TBool]
    ; return_type = TNull
    ; description = "Set whether a user is an admin. Returns null."
    ; func =
        internal_fn (function
            | _, [DStr username; DBool admin] ->
                let username = Unicode_string.to_string username in
                Account.set_admin username admin ;
                Stroller.heapio_identify_user username ;
                DNull
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getUsers"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TList
    ; description = "Return a list of username of all the accounts in Dark."
    ; func =
        internal_fn (function
            | _, [] ->
                Account.get_users ()
                |> List.map ~f:Dval.dstr_of_string_exn
                |> DList
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getAllCanvases"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TList
    ; description = "TODO"
    ; func =
        internal_fn (fun _ ->
            Serialize.current_hosts ()
            |> List.map ~f:Dval.dstr_of_string_exn
            |> DList)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::canvasesFor"]
    ; infix_names = []
    ; parameters = [par "account" TStr]
    ; return_type = TList
    ; description =
        "Returns a list of all canvases owned by a particular account (user OR org)"
    ; func =
        internal_fn (function
            | _, [DStr account] ->
                Serialize.hosts_for (Unicode_string.to_string account)
                |> List.map ~f:Dval.dstr_of_string_exn
                |> DList
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::schema"]
    ; infix_names = []
    ; parameters = [par "host" TStr; par "dbid" TStr]
    ; return_type = TObj
    ; description = "Return a schema for the db"
    ; func =
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
                  |> Prelude.Result.ok_or_internal_exception "Canvas load error"
                in
                let db =
                  !c.dbs
                  |> IDMap.data
                  |> List.filter_map ~f:Libexecution.Toplevel.as_db
                  |> List.find ~f:(fun d ->
                         Libexecution.Types.string_of_id d.tlid = tlid)
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
                           , Dval.dstr_of_string_exn (Dval.tipe_to_string v) ))
                    |> Dval.to_dobj_exn
                | None ->
                    Dval.to_dobj_exn [] )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::canvasAsText"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TStr
    ; description = "TODO"
    ; func =
        internal_fn (function
            | _, [DStr host] ->
                (* Removed, no longer useful now that you can copy from Fluid *)
                Dval.dstr_of_string_exn ""
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::handlers"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TList
    ; description = "Returns a list of toplevel ids of handlers in `host`"
    ; func =
        internal_fn (function
            | _, [DStr host] ->
                let c =
                  Canvas.load_all (Unicode_string.to_string host) []
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception "Canvas load error"
                in
                !c.handlers
                |> IDMap.data
                |> List.filter_map ~f:Libexecution.Toplevel.as_handler
                |> List.map ~f:(fun h ->
                       Dval.dstr_of_string_exn
                         (Libexecution.Types.string_of_id h.tlid))
                |> fun l -> DList l
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::functions"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TList
    ; description = "Returns a list of toplevel ids of the functions in `host`"
    ; func =
        internal_fn (function
            | _, [DStr host] ->
                let c =
                  Canvas.load_all (Unicode_string.to_string host) []
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception "Canvas load error"
                in
                !c.user_functions
                |> IDMap.data
                |> List.map ~f:(fun fn ->
                       Dval.dstr_of_string_exn
                         (Libexecution.Types.string_of_id fn.tlid))
                |> fun l -> DList l
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::canLoadTraces"]
    ; infix_names = []
    ; parameters = [par "host" TStr; par "tlid" TStr]
    ; return_type = TBool
    ; description =
        "Takes a <var host> and a <var tlid> and returns {{true}} iff we can load and parse traces for the handler identified by <var tlid>, and {{false}} otherwise."
    ; func =
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
                           h.tlid = Types.id_of_string tlid)
                  in
                  Analysis.traceids_for_handler !c handler
                  |> List.map ~f:(Analysis.handler_trace !c handler)
                  |> ignore ;
                  DBool true
                with _ -> DBool false )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getCORSSetting"]
    ; infix_names = []
    ; parameters = [par "canvas" TStr]
    ; return_type = TOption
    ; description =
        "Given the full canvas name (including the username), get that canvas' global CORS setting."
    ; func =
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
                  |> Prelude.Result.ok_or_internal_exception "Canvas load error"
                in
                !canvas.cors_setting |> cors_setting_to_dval
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::setCORSSetting"]
    ; infix_names = []
    ; parameters = [par "canvas" TStr; par "origins" TOption]
    ; return_type = TResult
    ; description =
        "Given the full canvas name (including the username) and an Option of either \"*\" or a list of string origins, set that value to that canvas' global CORS setting, so that it will be used in Access-Control-Allow-Origin response headers. Returns true if it worked and false if it didn't (likely meaning: the Dark value you passed in was invalid)."
    ; func =
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
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::dbs"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TList
    ; description = "Returns a list of toplevel ids of dbs in `host`"
    ; func =
        internal_fn (function
            | _, [DStr host] ->
                let db_tlids =
                  Db.fetch
                    ~name:"dbs_in_canvas"
                    "SELECT tlid
                     FROM toplevel_oplists
                     JOIN canvases ON canvases.idescription = canvas_id
                     WHERE canvases.name = $1 AND tipe = 'db'"
                    ~params:[String (Unicode_string.to_string host)]
                  |> List.fold ~init:[] ~f:(fun acc e -> e @ acc)
                in
                db_tlids
                |> List.map ~f:(fun s -> DStr (Unicode_string.of_string_exn s))
                |> fun l -> DList l
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::oplistInfo"]
    ; infix_names = []
    ; parameters = [par "host" TStr; par "tlid" TStr]
    ; return_type = TObj
    ; description =
        "Returns the information from the toplevel_oplists table for the (host, tlid)"
    ; func =
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
                          None)
                in
                zipped
                |> convert_to_date "created_at"
                |> convert_to_date "updated_at"
                |> fun o -> DObj o
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::storedEvents"]
    ; infix_names = []
    ; parameters = [par "host" TStr; par "tlid" TStr]
    ; return_type = TOption
    ; description =
        "Returns {{Just <var events>}}, where <var events> is the most recent stored events for the <param tlid> if it is a handler or {{Nothing}} if it is not."
    ; func =
        internal_fn (function
            | _, [DStr host; DStr tlid_str] ->
                let tlid =
                  Types.id_of_string (Unicode_string.to_string tlid_str)
                in
                let canvas =
                  Canvas.load_only_tlids
                    ~tlids:[tlid]
                    (Unicode_string.to_string host)
                    []
                  |> Result.map_error ~f:(String.concat ~sep:", ")
                  |> Prelude.Result.ok_or_internal_exception "Canvas load error"
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
                             |> fun o -> DObj o)
                      |> fun l -> DList l
                    in
                    DOption (OptJust event_list) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::pushStrollerEvent"]
    ; infix_names = []
    ; parameters = [par "canvas_id" TStr; par "event" TStr; par "payload" TObj]
    ; return_type = TResult
    ; description = "Pushes an event to Stroller"
    ; func =
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
                       (e |> Exception.to_string |> Dval.dstr_of_string_exn)) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::pushStrollerEvent_v1"]
    ; infix_names = []
    ; parameters = [par "canvas_id" TStr; par "event" TStr; par "payload" TAny]
    ; return_type = TResult
    ; description = "Pushes an event to Stroller"
    ; func =
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
                       (e |> Exception.to_string |> Dval.dstr_of_string_exn)) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::sessionKeyToUsername"]
    ; infix_names = []
    ; parameters = [par "sessionKey" TStr]
    ; return_type = TOption
    ; description = "Looks up the username for a session_key"
    ; func =
        internal_fn (function
            | _, [DStr sessionKey] ->
                let sessionKey = sessionKey |> Unicode_string.to_string in
                ( match Auth.SessionSync.username_of_key sessionKey with
                | None ->
                    DResult
                      (ResError
                         (Dval.dstr_of_string_exn "No session for cookie"))
                | Some username ->
                    DResult (ResOk (Dval.dstr_of_string_exn username)) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::canvasIdOfCanvasName"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TOption
    ; description = "Gives canvasId for a canvasName/host"
    ; func =
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
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::usernameToUserInfo"]
    ; infix_names = []
    ; parameters = [par "username" TStr]
    ; return_type = TOption
    ; description =
        "Gives userinfo {username, name, admin, email} for a username"
    ; func =
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
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::grant"]
    ; infix_names = []
    ; parameters = [par "username" TStr; par "org" TStr; par "permission" TStr]
    ; return_type = TResult
    ; description = "Set a user's permissions for a particular auth_domain."
    ; func =
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
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::grantsFor"]
    ; infix_names = []
    ; parameters = [par "org" TStr]
    ; return_type = TObj
    ; description =
        "Returns a dict mapping username->permission of users who have been granted permissions for a given auth_domain"
    ; func =
        internal_fn (function
            | _, [DStr org] ->
                let grants =
                  Authorization.grants_for
                    ~auth_domain:(Unicode_string.to_string org)
                in
                grants
                |> List.fold ~init:DvalMap.empty ~f:(fun map (user, perm) ->
                       DvalMap.insert
                         ~key:user
                         ~value:
                           ( perm
                           |> Authorization.permission_to_string
                           |> Dval.dstr_of_string_exn )
                         map)
                |> fun obj -> DObj obj
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::orgsFor"]
    ; infix_names = []
    ; parameters = [par "username" TStr]
    ; return_type = TObj
    ; description =
        "Returns a dict mapping orgs->permission to which the given `username` has been given permission"
    ; func =
        internal_fn (function
            | _, [DStr username] ->
                let orgs =
                  Authorization.orgs_for
                    ~username:(Unicode_string.to_string username)
                in
                orgs
                |> List.fold ~init:DvalMap.empty ~f:(fun map (org, perm) ->
                       DvalMap.insert
                         ~key:org
                         ~value:
                           ( perm
                           |> Authorization.permission_to_string
                           |> Dval.dstr_of_string_exn )
                         map)
                |> fun obj -> DObj obj
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::checkPermission"]
    ; infix_names = []
    ; parameters = [par "username" TStr; par "canvas" TStr]
    ; return_type = TBool
    ; description = "Check a user's permissions for a particular canvas."
    ; func =
        internal_fn (function
            | _, [DStr username; DStr canvas] ->
                let auth_domain =
                  Account.auth_domain_for (Unicode_string.to_string canvas)
                in
                let username = Unicode_string.to_string username in
                ( match Authorization.permission ~auth_domain ~username with
                | Some perm ->
                    Authorization.permission_to_string perm
                | None ->
                    "" )
                |> Dval.dstr_of_string_exn
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::log"]
    ; infix_names = []
    ; parameters = [par "level" TStr; par "name" TStr; par "log" TObj]
    ; return_type = TObj
    ; description =
        "Write the log object to a honeycomb log, along with whatever enrichment the backend provides."
    ; func =
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
                         |> Yojson.Safe.from_string)
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
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::fnsUsed"]
    ; infix_names = []
    ; parameters = [par "host" TStr; par "tlid" TStr]
    ; return_type = TList
    ; description =
        "Iterates through all ops of the AST, returning for each op a list of the functions used in that op. The last value will be the functions currently used."
    ; func =
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
                       |> Some)
                |> DList
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::fieldNamesUsed"]
    ; infix_names = []
    ; parameters = [par "host" TStr; par "tlid" TStr]
    ; return_type = TList
    ; description =
        "Iterates through all ops of the AST, returning for each op a list of the field names used in that op. The last value will be the fieldnames in the current code."
    ; func =
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
                       |> Some)
                |> DList
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::fnMetadata"]
    ; infix_names = []
    ; parameters = [par "name" TStr]
    ; return_type = TResult
    ; description =
        "Returns an object with the metadata of the built-in function name"
    ; func =
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
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::allFunctions"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TList
    ; description =
        "Returns a list of objects, representing the functions available in the standard library. Does not return DarkInternal functions"
    ; func =
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
                                           (Dval.tipe_to_string p.tipe) ) ])
                          in
                          [ ("name", Dval.dstr_of_string_exn key)
                          ; ( "documentation"
                            , Dval.dstr_of_string_exn data.description )
                          ; ("parameters", DList parameters)
                          ; ("returnType", Dval.dstr_of_string_exn returnType)
                          ]
                        in
                        Dval.to_dobj_exn alist :: acc)
                    !Libexecution.Libs.static_fns
                in
                DList fns
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::clearStaticAssets"]
    ; infix_names = []
    ; parameters = [par "host" TStr]
    ; return_type = TNull
    ; description =
        "Deletes our record of static assets for a handler. Does not delete the data from the bucket. This is a hack for making Ellen's demo easier and should not be used for other uses in this form."
    ; func =
        internal_fn (function
            | _, [DStr host] ->
                let host = Unicode_string.to_string host in
                let owner = Account.for_host_exn host in
                let canvas_id = Serialize.fetch_canvas_id owner host in
                Static_assets.delete_assets_for_ellens_demo canvas_id ;
                DNull
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getAllSchedulingRules"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TList
    ; description = "Returns a list of all queue scheduling rules."
    ; func =
        internal_fn (function
            | _, [] ->
                Event_queue.get_all_scheduling_rules ()
                |> List.map ~f:Event_queue.Scheduling_rule.to_dval
                |> DList
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getSchedulingRulesForCanvas"]
    ; infix_names = []
    ; parameters = [par "canvas_id" TUuid]
    ; return_type = TList
    ; description =
        "Returns a list of all queue scheduling rules for the specified canvas_id"
    ; func =
        internal_fn (function
            | _, [DUuid canvas_id] ->
                Event_queue.get_scheduling_rules_for_canvas canvas_id
                |> List.map ~f:Event_queue.Scheduling_rule.to_dval
                |> DList
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::addWorkerSchedulingBlock"]
    ; infix_names = []
    ; parameters = [par "canvas_id" TUuid; par "handler_name" TStr]
    ; return_type = TNull
    ; description =
        "Add a worker scheduling 'block' for the given canvas and handler. This prevents any events for that handler from being scheduled until the block is manually removed."
    ; func = modify_schedule Event_queue.block_worker
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::removeWorkerSchedulingBlock"]
    ; infix_names = []
    ; parameters = [par "canvas_id" TUuid; par "handler_name" TStr]
    ; return_type = TNull
    ; description =
        "Removes the worker scheduling block, if one exists, for the given canvas and handler. Enqueued events from this job will immediately be scheduled."
    ; func = modify_schedule Event_queue.unblock_worker
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::newSessionForUsername"]
    ; infix_names = []
    ; parameters = [par "username" TStr]
    ; return_type = TResult
    ; description =
        "If username is an existing user, puts a new session in the DB and returns the new sessionKey."
    ; func =
        internal_fn (function
            | exec_state, [DStr username] ->
                let username = Unicode_string.to_string username in
                ( match Account.id_of_username username with
                | None ->
                    DResult
                      (ResError
                         (Dval.dstr_of_string_exn
                            ("No user '" ^ username ^ "'")))
                | Some _user_id ->
                    let session_key =
                      try
                        Auth.SessionSync.new_for_username username
                        |> Result.return
                      with e -> Result.fail e
                    in
                    ( match session_key with
                    | Ok session_key ->
                        DResult (ResOk (Dval.dstr_of_string_exn session_key))
                    | Error e ->
                        (* If session creation fails, log and rollbar *)
                        let err = Libexecution.Exception.exn_to_string e in
                        Log.erroR
                          "DarkInternal::newSessionForUsername"
                          ~params:[("username", username); ("exception", err)] ;
                        let bt = Libexecution.Exception.get_backtrace () in
                        ( match
                            Rollbar.report
                              e
                              bt
                              (Other "Darklang")
                              (exec_state.execution_id |> Types.string_of_id)
                          with
                        | `Success | `Disabled ->
                            ()
                        | `Failure ->
                            Log.erroR
                              "rollbar.report at DarkInternal::newSessionForUsername"
                        ) ;
                        DResult
                          (ResError
                             (Dval.dstr_of_string_exn
                                "Failed to create session")) ) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["DarkInternal::newSessionForUsername_v1"]
    ; infix_names = []
    ; parameters = [par "username" TStr]
    ; return_type = TResult
    ; description =
        (* We need the csrf token for dark-cli to use *)
        "If username is an existing user, puts a new session in the DB and returns the new sessionKey and csrfToken."
    ; func =
        internal_fn (function
            | exec_state, [DStr username] ->
                let username = Unicode_string.to_string username in
                ( match Account.id_of_username username with
                | None ->
                    DResult
                      (ResError
                         (Dval.dstr_of_string_exn
                            ("No user '" ^ username ^ "'")))
                | Some _user_id ->
                    let session_key_and_csrf_token =
                      try
                        Auth.SessionSync.new_for_username_with_csrf_token
                          username
                        |> Result.return
                      with e -> Result.fail e
                    in
                    ( match session_key_and_csrf_token with
                    | Ok {sessionKey; csrfToken} ->
                        DResult
                          (ResOk
                             (DObj
                                (DvalMap.from_list
                                   [ ( "sessionKey"
                                     , Dval.dstr_of_string_exn sessionKey )
                                   ; ( "csrfToken"
                                     , Dval.dstr_of_string_exn csrfToken ) ])))
                    | Error e ->
                        (* If session creation fails, log and rollbar *)
                        let err = Libexecution.Exception.exn_to_string e in
                        Log.erroR
                          "DarkInternal::newSessionForUsername_v1"
                          ~params:[("username", username); ("exception", err)] ;
                        let bt = Libexecution.Exception.get_backtrace () in
                        ( match
                            Rollbar.report
                              e
                              bt
                              (Other "Darklang")
                              (exec_state.execution_id |> Types.string_of_id)
                          with
                        | `Success | `Disabled ->
                            ()
                        | `Failure ->
                            Log.erroR
                              "rollbar.report at DarkInternal::newSessionForUsername"
                        ) ;
                        DResult
                          (ResError
                             (Dval.dstr_of_string_exn
                                "Failed to create session")) ) )
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::deleteSession"]
    ; infix_names = []
    ; parameters = [par "session_key" TStr]
    ; return_type = TInt
    ; description =
        "Delete session by session_key; return number of sessions deleted."
    ; func =
        internal_fn (function
            | exec_state, [DStr session_key] ->
                let session_key = Unicode_string.to_string session_key in
                Db.delete
                  ~subject:session_key
                  ~name:"delete session by session_key"
                  "DELETE FROM session WHERE session_key = $1"
                  ~params:[String session_key]
                |> Dval.dint
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["DarkInternal::getAndLogTableSizes"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TObj
    ; description =
        "Query the postgres database for the current size (disk + rowcount) of all
tables. This uses pg_stat, so it is fast but imprecise. This function is logged
in OCaml; its primary purpose is to send data to honeycomb, but also gives
human-readable data."
    ; func =
        internal_fn (function
            | exec_state, [] ->
                let table_stats = Db.table_stats () in
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
                |> List.iter ~f:(fun ts ->
                       Log.infO
                         "postgres_table_sizes"
                         ~jsonparams:
                           [ ("relation", `String ts.relation)
                           ; ("disk_bytes", `Int ts.disk_bytes)
                           ; ("rows", `Int ts.rows)
                           ; ("disk_human", `String ts.disk_human)
                           ; ("rows_human", `String ts.rows_human) ]) ;
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
                  |> List.map ~f:(fun ts ->
                         ( ts.relation
                         , [ ("disk_bytes", DInt (Dint.of_int ts.disk_bytes))
                           ; ("rows", DInt (Dint.of_int ts.rows))
                           ; ( "disk_human"
                             , Dval.dstr_of_string_exn ts.disk_human )
                           ; ( "rows_human"
                             , Dval.dstr_of_string_exn ts.rows_human ) ]
                           |> DvalMap.from_list
                           |> DObj ))
                in
                table_stats_for_dobj |> DvalMap.from_list |> DObj
            | args ->
                fail args)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
