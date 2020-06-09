open Core_kernel
open Libcommon
open Libexecution
open Util
open Types
open Types.RuntimeT

(* We will later change it to get the entry with max(secret_version) *)
let secrets_in_canvas (canvas_id : Uuidm.t) : secret list =
  Db.fetch
    ~name:"all secrets by canvas"
    "SELECT secret_name, secret_value, secret_version FROM secrets WHERE canvas_id=$1"
    ~params:[Uuid canvas_id]
  |> List.map ~f:(function
         | [secret_name; secret_value; secret_version] ->
             { secret_name
             ; secret_value
             ; secret_version = int_of_string secret_version }
         | _ ->
             Exception.internal "Bad DB format for static assets deploys")
