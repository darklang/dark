open Core_kernel
open Libcommon
open Libexecution
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT

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
            Canvas.cleanup_old_traces host ;
            DNull
        | args ->
            fail args )
    ; ( "DarkInternal::checkCanvas"
      , function
        | state, [DStr host] ->
          ( try
              Canvas.validate_host host ;
              DBool true
            with _ -> DBool false )
        | args ->
            fail args )
    ; ( "DarkInternal::getAllCanvases"
      , fun _ ->
          Serialize.current_hosts () |> List.map ~f:(fun s -> DStr s) |> DList
      )
    ; ( "DarkInternal::canvasAsText"
      , function
        | _, [DStr host] -> DStr (Canvas.to_string host) | args -> fail args )
    ; ( "DarkInternal::handlers"
      , function
        | _, [DStr host] ->
            let c = Canvas.load_all host [] in
            !c.handlers
            |> List.map ~f:Libexecution.Toplevel.as_handler
            |> List.map ~f:(fun h -> Option.value_exn h)
            |> List.map ~f:(fun h ->
                   DStr (Libexecution.Types.string_of_id h.tlid) )
            |> fun l -> DList l
        | args ->
            fail args )
    ; ( "DarkInternal::canLoadTraces"
      , function
        | _, [DStr host; DStr tlid] ->
            let open Libexecution in
            let c =
              Canvas.load_only host ~tlids:[Types.id_of_string tlid] []
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
            let password = Account.upsert_user ~username ~email ~name () in
            DStr password
        | args ->
            fail args ) ]
