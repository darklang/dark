open Core

open Types
open Types.RuntimeT

module FF = Feature_flag

type t = { id: int
         ; value: dval
         ; retries: int
         ; flag_context: feature_flag
         }

let status_to_enum status : string =
  match status with
  | `OK -> "'done'"
  | `Err -> "'error'"
  | `Incomplete -> "'error'"

let unlock_jobs ~(host:string) (dequeuer: int) ~status : unit =
  Printf.sprintf
    "UPDATE \"events\"
     SET status = %s
       WHERE dequeued_by = %s
         AND status = 'locked'"
    (status_to_enum status)
    (string_of_int dequeuer)
  |> Db.run_sql_in_ns ~host

(* ------------------------- *)
(* Public API *)
(* ------------------------- *)

let wrap s = "'" ^ s ^ "'"

let finalize ~(host:string)(dequeuer: int) ~status : unit =
  unlock_jobs ~host ~status dequeuer

let enqueue (state: exec_state) (space: string) (name: string) (data: dval) : unit =
  let serialized_data = Dval.dval_to_json_string data in
  let column_names =
    [ "status"
    ; "dequeued_by"
    ; "canvas"
    ; "space"
    ; "name"
    ; "value"
    ; "delay_until"
    ; "flag_context"
    ]
    |> String.concat ~sep:", "
  in
  let column_values =
    [ "'new'"
    ; "NULL"
    ; wrap state.host
    ; wrap space
    ; wrap name
    ; wrap serialized_data
    ; "CURRENT_TIMESTAMP"
    ; FF.to_sql state.ff
    ]
    |> String.concat ~sep:", "
  in
  (Printf.sprintf
     "INSERT INTO \"events\"
     (%s)
     VALUES (%s)"
     column_names column_values)
  |> Db.run_sql_in_ns ~host:state.host

(* This should soon enough do something like:
 * https://github.com/chanks/que/blob/master/lib/que/sql.rb#L4
 * but multiple queries will do fine for now
 *)
let dequeue ~(host:string) (execution_id: int) (space: string) (name: string) : t option =
  let fetched =
    Printf.sprintf
      "SELECT id, value, retries, flag_context from \"events\"
      WHERE space = %s
        AND name = %s
        AND canvas = %s
        AND status = 'new'
        AND delay_until < CURRENT_TIMESTAMP
      ORDER BY id DESC
             , retries ASC
      LIMIT 1"
      (wrap space)
      (wrap name)
      (wrap host)
    |> Db.fetch_via_sql_in_ns ~host
    |> List.hd
  in
  match fetched with
  | None -> None
  | Some [id; value; retries; flag_context] ->
    Db.run_sql_in_ns ~host
      (Printf.sprintf
         "UPDATE \"events\"
         SET status = 'locked'
           , dequeued_by = %s
         WHERE id = %s"
         (string_of_int execution_id)
         id);
    Some { id = int_of_string id
         ; value = Dval.parse value
         ; retries = int_of_string retries
         ; flag_context = FF.from_sql flag_context
         }
  | Some s ->
    Exception.internal
      ("Fetched seemingly impossible shape from Postgres"
       ^ ("[" ^ (String.concat ~sep:", " s) ^ "]"))

let put_back ~(host:string) (item: t) ~status : unit =
  let id = string_of_int item.id in
  let sql =
    match status with
    | `OK ->
      Printf.sprintf
        "UPDATE \"events\"
        SET status = 'done'
        WHERE id = %s"
        id
    | `Err ->
      if item.retries < 2
      then
        Printf.sprintf
          "UPDATE \"events\"
          SET status = 'new'
            , retries = %s
            , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
          WHERE id = %s"
        (string_of_int (item.retries + 1))
        id
      else
        Printf.sprintf
          "UPDATE \"events\"
          SET status = 'error'
          WHERE id = %s"
          id
    | `Incomplete ->
      Printf.sprintf
        "UPDATE \"events\"
        SET status = 'new'
          , delay_until = CURRENT_TIMESTAMP + INTERVAL '5 minutes'
        WHERE id = %s" id
  in
  Db.run_sql_in_ns ~host sql

let finish ~(host:string) (item: t) : unit =
  put_back ~host ~status:`OK item



let create_queue_status_type host : unit =
  Db.run_sql_in_ns ~host
    "DO $$
     BEGIN
       IF NOT EXISTS
         (SELECT 1
          FROM pg_type t
          LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
          WHERE (t.typrelid = 0
                 OR (SELECT c.relkind = 'c'
                     FROM pg_catalog.pg_class c
                     WHERE c.oid = t.typrelid))
          AND NOT EXISTS (SELECT 1
                          FROM pg_catalog.pg_type el
                          WHERE el.oid = t.typelem
                            AND el.typarray = t.oid)
          AND n.nspname NOT IN ('pg_catalog', 'information_schema')
          AND t.typname = 'queue_status'
          AND n.nspname <> '{SCHEMA}'
          )
       THEN
         CREATE TYPE queue_status AS
           ENUM ('new', 'locked', 'done', 'error');
       END IF;
     END $$;
    "

let create_events_table host =
  Db.run_sql_in_ns ~host
    "CREATE TABLE IF NOT EXISTS
          \"events\"
          (id SERIAL PRIMARY KEY
          , status queue_status
          , dequeued_by INT
          , canvas TEXT NOT NULL
          , space TEXT NOT NULL
          , name TEXT NOT NULL
          , value TEXT NOT NULL
          , retries INTEGER DEFAULT 0 NOT NULL
          , flag_content TEXT DEFAULT '' NOT NULL
          , delay_until TIMESTAMP
          )
          "
let initialize_queue host : unit =
  create_queue_status_type host;
  create_events_table host;
  ()


