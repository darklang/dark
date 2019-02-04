open Core_kernel
open Libcommon
open Libexecution
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT
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
        | state, [DStr host] ->
            Canvas.cleanup_old_traces (Unicode_string.to_string host) ;
            DNull
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
            let open Libexecution in
            let tlid = Unicode_string.to_string tlid in
            let c =
              Canvas.load_only
                (Unicode_string.to_string host)
                ~tlids:[Types.id_of_string tlid]
                []
            in
            let handler =
              !c.handlers
              |> List.map ~f:Toplevel.as_handler
              |> List.map ~f:(fun h -> Option.value_exn h)
              |> List.find_exn ~f:(fun h -> h.tlid = Types.id_of_string tlid)
            in
            ( try
                ignore (Analysis.traces_for_handler !c handler) ;
                DBool true
              with _ -> DBool false )
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
                     (Dval.to_dobj
                        [ ("username", Dval.dstr_of_string_exn username)
                        ; ("name", Dval.dstr_of_string_exn name)
                        ; ("email", Dval.dstr_of_string_exn email) ])) )
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
            let canvas =
              Canvas.load_only ~tlids:[] (Unicode.to_string host) []
            in
            !canvas.cors_setting |> cors_setting_to_dval
        | args ->
            fail args )
    ; ( "DarkInternal::setCORSSetting"
      , let cors_setting_of_dval (dval : dval) :
            (Canvas.cors_setting option, string) result =
          (* Error: error converting the dval to a cors setting
             Ok None: the dval is "unset the cors value"
             Ok (Some cs): the dval is "set the cors setting to cs" *)
          try
            match dval with
            | DOption OptNothing ->
                Ok None
            | DOption (OptJust (DStr s)) when Unicode.to_string s = "*" ->
                Ok (Some Canvas.AllOrigins)
            | DOption (OptJust (DList os)) ->
                os
                |> List.map ~f:Dval.to_string_exn
                |> Canvas.Origins
                |> Some
                |> Ok
            | _ ->
                Error
                  "Received something other than an Nothing, Just [...], or Just \"*\""
          with e -> Error (Exception.exn_to_string e)
        in
        function
        | _, [DStr host; s] ->
          ( match cors_setting_of_dval s with
          | Error e ->
              e |> Dval.dstr_of_string_exn |> ResError |> DResult
          | Ok settings ->
              let canvas =
                Canvas.load_only ~tlids:[] (Unicode.to_string host) []
              in
              Canvas.update_cors_setting canvas settings ;
              s |> ResOk |> DResult )
        | args ->
            fail args ) ]
