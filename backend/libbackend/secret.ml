open Core_kernel
open Libcommon
open Libexecution
open Util
open Types
open Types.RuntimeT

let secrets_in_canvas (canvas_id : Uuidm.t) : secret list =
  Db.fetch
    ~name:"all secrets by canvas"
    "SELECT secret_name, secret_value FROM secrets WHERE canvas_id=$1 ORDER BY created_at DESC"
    ~params:[Uuid canvas_id]
  |> List.map ~f:(function
         | [secret_name; secret_value] ->
             {secret_name; secret_value}
         | _ ->
             Exception.internal "Bad DB format for secrets")


let insert_secret (canvas_id : Uuidm.t) (name : string) (value : string) : unit
    =
  Db.run
    "INSERT INTO secrets
    (canvas_id, secret_name, secret_value)
    VALUES ($1, $2, $3)"
    ~params:[Uuid canvas_id; String name; String value]
    ~name:"insert secret for canvas"
