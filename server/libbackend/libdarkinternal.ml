open Core_kernel
open Libcommon
open Libexecution
open Libexecution.Lib
open Libexecution.Runtime
open Libexecution.Types.RuntimeT


(* Apply this to a name, function tuple to wrap that function
   in an InProcess that checks permissions for the dark internal
   functions and logs status. *)
let internal_fn  ((name, f) : string * (exec_state * dval list -> dval)) =
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
           then (Log.infO "internal_fn"
                   ~params:[ "user", username
                           ; "status", "access_denied"
                           ];
                 let result = f (es, params) in
                 (Log.infO "internal_fn"
                    ~params:[ "user", username
                            ; "status", "finished"
                            ];
                  result;))
           else
             username
             |> Format.sprintf "User executed an internal function but isn't an admin: %s"
             |> Exception.user)
  )

let replacements =
  List.map ~f:internal_fn
    [ "DarkInternal::checkAccess"
    , (fun _ -> DNull)

    ; "DarkInternal::checkAllCanvases"
    , (fun _ -> DNull)

    ; "DarkInternal::migrateAllCanvases"
    , (fun _ ->
      Canvas.migrate_all_hosts ();
      DNull)

    ; "DarkInternal::cleanupOldTraces"
    , (fun _ ->
      Canvas.cleanup_old_traces ();
      DNull)

    ; "DarkInternal::checkCanvas"
    , (function
        | (state, [DStr host]) ->
          (try
            Canvas.validate_host host;
            DBool true
           with
           | _ -> DBool false)
        | args -> fail args)

    ; "DarkInternal::getAllCanvases"
    , (fun _ ->
         Serialize.current_hosts ()
         |> List.map ~f:(fun s -> DStr s)
         |> DList)
  ]
