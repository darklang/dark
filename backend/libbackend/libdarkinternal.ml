open Core_kernel
open Libcommon
open Libexecution
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
open Types
module Unicode = Libexecution.Unicode_string

(* Apply this to a name, function tuple to wrap that function
   in an InProcess that checks permissions for the dark internal
   functions and logs status. *)
let internal_fn ((name, f) : string * (exec_state * dval list -> dval)) =
  ( name
  , InProcess
      (fun (es, params) ->
        match es.account_id |> Account.username_of_id with
        | None ->
            es.account_id
            |> Uuidm.to_string
            |> Format.sprintf "User not found with id: %s"
            |> Exception.user
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
              |> Exception.user ) )


let replacements =
  List.map
    ~f:internal_fn
    [ ("DarkInternal::checkAccess", fun _ -> DNull)
    ; ("DarkInternal::checkAllCanvases", fun _ -> DNull)
    ; ( "DarkInternal::migrateAllCanvases"
      , fun _ ->
          Canvas.migrate_all_hosts () ;
          DNull )
    ; ("DarkInternal::cleanupOldTraces", fun _ -> DNull)
    ; ( "DarkInternal::cleanupOldTraces_v1"
      , function
        | state, [] ->
            DFloat (Canvas.cleanup_old_traces ())
        | args ->
            fail args )
    ; ( "DarkInternal::checkCanvas"
      , function
        | state, [DStr host] ->
          ( try
              Canvas.validate_host (Unicode_string.to_string host) ;
              DBool true
            with _ -> DBool false )
        | args ->
            fail args )
    ; ( "DarkInternal::getAllCanvases"
      , fun _ ->
          Serialize.current_hosts ()
          |> List.map ~f:Dval.dstr_of_string_exn
          |> DList )
    ; ( "DarkInternal::canvasAsText"
      , function
        | _, [DStr host] ->
            Dval.dstr_of_string_exn
              (Canvas.to_string (Unicode_string.to_string host))
        | args ->
            fail args )
    ; ( "DarkInternal::handlers"
      , function
        | _, [DStr host] ->
            let c = Canvas.load_all (Unicode_string.to_string host) [] in
            !c.handlers
            |> IDMap.data
            |> List.map ~f:Libexecution.Toplevel.as_handler
            |> List.map ~f:(fun h -> Option.value_exn h)
            |> List.map ~f:(fun h ->
                   Dval.dstr_of_string_exn
                     (Libexecution.Types.string_of_id h.tlid) )
            |> fun l -> DList l
        | args ->
            fail args )
    ; ( "DarkInternal::canLoadTraces"
      , function
        | _, [DStr host; DStr tlid] ->
          ( try
              let open Libexecution in
              let tlid = Unicode_string.to_string tlid in
              let c =
                Canvas.load_only_tlids
                  (Unicode_string.to_string host)
                  ~tlids:[Types.id_of_string tlid]
                  []
              in
              let handler =
                !c.handlers
                |> IDMap.data
                |> List.map ~f:Toplevel.as_handler
                |> List.map ~f:(fun h -> Option.value_exn h)
                |> List.find_exn ~f:(fun h -> h.tlid = Types.id_of_string tlid)
              in
              Analysis.traceids_for_handler !c handler
              |> List.map ~f:(Analysis.handler_trace !c handler)
              |> ignore ;
              DBool true
            with _ -> DBool false )
        | args ->
            fail args )
    ; ( "DarkInternal::endUsers"
      , function
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
    ; ( "DarkInternal::upsertUser"
      , function
        | _, [DStr username; DStr email; DStr name] ->
            let username = Unicode_string.to_string username in
            let email = Unicode_string.to_string email in
            let name = Unicode_string.to_string name in
            let password = Account.upsert_user ~username ~email ~name () in
            Dval.dstr_of_string_exn password
        | args ->
            fail args )
    ; ( "DarkInternal::getUser"
      , function
        | _, [DStr username] ->
            let info = Account.get_user (Unicode_string.to_string username) in
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
    ; ( "DarkInternal::getUser_v1"
      , function
        | _, [DStr username] ->
            let info = Account.get_user (Unicode_string.to_string username) in
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
    ; ( "DarkInternal::setAdmin"
      , function
        | _, [DStr username; DBool admin] ->
            Account.set_admin (Unicode_string.to_string username) admin ;
            DNull
        | args ->
            fail args )
    ; ( "DarkInternal::getUsers"
      , function
        | _, [] ->
            Account.get_users ()
            |> List.map ~f:Dval.dstr_of_string_exn
            |> DList
        | args ->
            fail args )
    ; ( "DarkInternal::getCORSSetting"
      , let cors_setting_to_dval (setting : Canvas.cors_setting option) : dval
            =
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
        function
        | _, [DStr host] ->
            let canvas = Canvas.load_without_tls (Unicode.to_string host) in
            !canvas.cors_setting |> cors_setting_to_dval
        | args ->
            fail args )
    ; ( "DarkInternal::setCORSSetting"
      , let cors_setting (opt : optionT) :
            (Canvas.cors_setting option, string) result =
          (* Error: error converting the dval to a cors setting
             Ok None: the dval is "unset the cors value"
             Ok (Some cs): the dval is "set the cors setting to cs" *)
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
        function
        | _, [DStr host; DOption s] ->
          ( match cors_setting s with
          | Error e ->
              e |> Dval.dstr_of_string_exn |> ResError |> DResult
          | Ok settings ->
              let canvas = Canvas.load_without_tls (Unicode.to_string host) in
              Canvas.update_cors_setting canvas settings ;
              s |> DOption |> ResOk |> DResult )
        | args ->
            fail args )
    ; ( "DarkInternal::dbs"
      , function
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
    ; ( "DarkInternal::schema"
      , function
        | _, [DStr canvas_name; DStr tlid] ->
            let tlid = Unicode_string.to_string tlid in
            let canvas_name = Unicode_string.to_string canvas_name in
            let c =
              Canvas.load_only_tlids
                ~tlids:[Types.id_of_string tlid]
                canvas_name
                []
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
                       , Dval.dstr_of_string_exn (Dval.tipe_to_string v) ) )
                |> Dval.to_dobj_exn
            | None ->
                Dval.to_dobj_exn [] )
        | args ->
            fail args )
    ; ( "DarkInternal::oplistInfo"
      , function
        | _, [DStr host; DStr tlid_str] ->
            let account =
              Account.for_host_exn (Unicode_string.to_string host)
            in
            let canvas_id =
              Serialize.fetch_canvas_id account (Unicode_string.to_string host)
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
              |> DvalMap.of_alist_exn
            in
            let convert_to_date k obj =
              DvalMap.change obj k ~f:(fun v ->
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
    ; ( "DarkInternal::storedEvents"
      , function
        | _, [DStr host; DStr tlid_str] ->
            let tlid =
              Types.id_of_string (Unicode_string.to_string tlid_str)
            in
            let canvas : Canvas.canvas ref =
              Canvas.load_only_tlids
                ~tlids:[tlid]
                (Unicode_string.to_string host)
                []
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
                         |> DvalMap.of_alist_exn
                         |> fun o -> DObj o )
                  |> fun l -> DList l
                in
                DOption (OptJust event_list) )
        | args ->
            fail args )
    ; ( "DarkInternal::pushStrollerEvent"
      , function
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
                (ResError (e |> Exception.to_string |> Dval.dstr_of_string_exn))
          )
        | args ->
            fail args )
    ; ( "DarkInternal::sessionKeyToUsername"
      , function
        | _, [DStr sessionKey] ->
            let sessionKey = sessionKey |> Unicode_string.to_string in
            ( match Auth.Session.username_of_key sessionKey with
            | None ->
                DOption OptNothing
            | Some username ->
                DOption (OptJust (Dval.dstr_of_string_exn username)) )
        | args ->
            fail args )
    ; ( "DarkInternal::canEditCanvas"
      , function
        | _, [DStr host; DStr username] ->
            let host = Unicode_string.to_string host in
            let username = Unicode_string.to_string username in
            Account.can_edit_canvas
              ~auth_domain:(Account.auth_domain_for host)
              ~username
            |> DBool
        | args ->
            fail args )
    ; ( "DarkInternal::usernameToUserInfo"
      , function
        | _, [DStr username] ->
            let username = Unicode_string.to_string username in
            ( match Account.get_user username with
            | None ->
                DOption OptNothing
            | Some user_info ->
                let dval_map =
                  user_info
                  |> Account.user_info_to_yojson
                  |> Yojson.Safe.Util.to_assoc
                  |> List.map ~f:(fun (k, v) ->
                         (k, Dval.dstr_of_string_exn (Yojson.Safe.to_string v))
                     )
                  |> DvalMap.of_alist_exn
                in
                DOption (OptJust (DObj dval_map)) )
        | args ->
            fail args )
    ; ( "DarkInternal::log"
      , function
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
              |> DvalMap.add
                   ~key:"level"
                   ~data:
                     (level |> Log.level_to_string |> Dval.dstr_of_string_exn)
              |> function
              | `Ok log ->
                  log
              | `Duplicate ->
                  log
                  |> DvalMap.add
                       ~key:"name"
                       ~data:(Dval.dstr_of_string_exn name)
                  |> (function `Ok log -> log | `Duplicate -> log)
            in
            Log.pP ~level name ~jsonparams ;
            DObj log
        | args ->
            fail args )
    ; ( "DarkInternal::sessionKeyToUsername"
      , function
        | _, [DStr sessionKey] ->
            let sessionKey = sessionKey |> Unicode_string.to_string in
            ( match Auth.Session.username_of_key sessionKey with
            | None ->
                DResult
                  (ResError (Dval.dstr_of_string_exn "No session for cookie"))
            | Some username ->
                DResult (ResOk (Dval.dstr_of_string_exn username)) )
        | args ->
            fail args )
    ; ( "DarkInternal::canEditCanvas"
      , function
        | _, [DStr host; DStr username] ->
            let host = Unicode_string.to_string host in
            let username = Unicode_string.to_string username in
            Account.can_edit_canvas
              ~auth_domain:(Account.auth_domain_for host)
              ~username
            |> DBool
        | args ->
            fail args )
    ; ( "DarkInternal::usernameToUserInfo"
      , function
        | _, [DStr username] ->
            let username = Unicode_string.to_string username in
            ( match Account.get_user username with
            | None ->
                DOption OptNothing
            | Some user_info ->
                let dval_map =
                  user_info
                  |> Account.user_info_to_yojson
                  |> Yojson.Safe.Util.to_assoc
                  |> List.map ~f:(fun (k, v) ->
                         (k, Dval.dstr_of_string_exn (Yojson.Safe.to_string v))
                     )
                  |> DvalMap.of_alist_exn
                in
                DOption (OptJust (DObj dval_map)) )
        | args ->
            fail args ) ]
