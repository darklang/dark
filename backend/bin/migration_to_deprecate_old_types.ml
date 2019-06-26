open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend
open Libcommon

let usage () : unit =
  Format.printf
    "Usage: %s\n\n  Use DARK_CONFIG_DB_DBNAME=prodclone to check prodclone."
    Sys.argv.(0) ;
  exit 1


let () =
  ( match (Array.length Sys.argv, Array.to_list Sys.argv) with
  | 1, _ ->
      ()
  | 2, [_; "-h"] | _ ->
      usage () ) ;
  Db.iter_with_cursor
    ~name:"migrate id to string"
    ~params:[]
    "SELECT user_data.key, data FROM user_data CROSS JOIN LATERAL jsonb_each(data) sub WHERE
value @> '{\"type\": \"id\"}' LIMIT 100"
    ~f:(function
      | [key; data] ->
          let parsed_data = data |> Yojson.Safe.from_string in
          (* if a pair has an id value = `{"type": "id", ...}` - then make it a
           * string instead *)
          let transform_pair (pair : string * Yojson.Safe.t) :
              string * Yojson.Safe.t =
            let key, value = pair in
            let new_value =
              match value with
              | `Assoc [("type", `String "id"); ("value", `String value)] ->
                  `String value
              | _ ->
                  value
            in
            (key, new_value)
          in
          let transformed_data =
            match parsed_data with
            | `Assoc pairs ->
                `Assoc (pairs |> List.map ~f:transform_pair)
            | _ ->
                parsed_data
          in
          if parsed_data <> transformed_data
          then Log.infO "update_id_to_string" ~params:[("key", key)] ;
          Db.run
            ~name:"update ids in user_data to be strings"
            ~params:
              [ Db.Uuid
                  ( key
                  |> Uuidm.of_string
                  |> (Option.value_exn : Uuidm.t option -> Uuidm.t) )
              ; Db.String data
              ; Db.String (transformed_data |> Yojson.Safe.to_string) ]
            (* AND data = $2 to guard against race conditions *)
            "UPDATE user_data SET data = $3 WHERE key = $1 AND data = $2"
      | _ ->
          Exception.internal "Bad db result" ) ;
  ()
