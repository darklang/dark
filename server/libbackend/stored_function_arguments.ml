open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT

(* ------------------------- *)
(* External *)
(* ------------------------- *)

let store ~canvas_id ~trace_id tlid args =
  Db.run
    ~name:"stored_function_arguments.store"
    "INSERT INTO function_arguments
     (canvas_id, trace_id, tlid, timestamp, arguments_json)
     VALUES ($1, $2, $3, CURRENT_TIMESTAMP, $4)"
    ~params:[ Uuid canvas_id
            ; Uuid trace_id
            ; ID tlid
            ; String (args
                      |> Dval.unsafe_dvalmap_to_yojson ~redact:false
                      |> Yojson.Safe.to_string)
            ]

let load ~canvas_id tlid : (RTT.dval_map * Time.t) list =
  Db.fetch
    ~name:"stored_function_arguments.load"
    "SELECT arguments_json, timestamp
     FROM function_arguments
     WHERE canvas_id = $1
       AND tlid = $2
     ORDER BY timestamp DESC
       LIMIT 10"
    ~params:[ Db.Uuid canvas_id
            ; Db.ID tlid
            ]
  |> List.map ~f:(function
      | [args; ts] ->
        (args
         |> Yojson.Safe.from_string
         |> Dval.unsafe_dvalmap_of_yojson
        , Db.date_of_sqlstring ts)
      | _ -> Exception.internal "Bad DB format for stored_functions.load")


