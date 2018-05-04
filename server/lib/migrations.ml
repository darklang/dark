open Core

let migrations =
  (* user DB migrations *)
  [ `Global
    "CREATE TABLE IF NOT EXISTS
       migrations
       ( id BIGINT
       , host TEXT
       , sql TEXT
       , PRIMARY KEY (id, host))"

  (* user sessions *)
  (* https://github.com/inhabitedtype/ocaml-session/blob/master/backends/postgresql/lwt/session_postgresql_lwt.mli#L39 *)
  ; `Global
    "CREATE TABLE IF NOT EXISTS
       session
       ( session_key CHAR(40)
       , expire_date TIMESTAMP (2) WITH TIME ZONE
       , session_data TEXT
       )"

  (* user session indices *)
  ; `Global
    "CREATE INDEX IF NOT EXISTS
      session_key_idx
      ON \"session\"
      (session_key)"

  (* serialize canvases *)
  ; `Global
    "CREATE TABLE IF NOT EXISTS
      oplists
      ( host VARCHAR(64)
      , digest CHAR(32)
      , data BYTEA
      , PRIMARY KEY (host, digest))"

  (* type for queue state *)
    (* there's no CREATE TYPE IF NOT EXISTS :/ *)
  ; `Global
    "DO $$
       BEGIN
         IF NOT EXISTS
           (SELECT 1 FROM pg_type WHERE typname = 'queue_status')
         THEN
           CREATE TYPE queue_status AS
             ENUM ('new', 'locked', 'done', 'error');
         END IF;
       END$$;"

  (* queue table *)
  (* ensure_queue_table_exists *)
  ; `Global
    "CREATE TABLE IF NOT EXISTS
      \"events\"
      (id SERIAL PRIMARY KEY
      , status queue_status
      , dequeued_by INT
      , canvas TEXT NOT NULL
      , space TEXT NOT NULL
      , name TEXT NOT NULL
      , value TEXT NOT NULL)"


  (* queue index *)
  ; `Global
    "CREATE INDEX IF NOT EXISTS
       \"idx_dequeue\"
       ON \"events\"
       (space, name, canvas, status, id)"


  (* queue cleanup index *)
  ; `Global
    "CREATE INDEX IF NOT EXISTS \"idx_cleanup\"
      ON \"events\"
      (dequeued_by)"


  (* event retries *)
  ; `Global
    "ALTER TABLE \"events\"
      ADD COLUMN IF NOT EXISTS
        retries INTEGER DEFAULT 0 NOT NULL"


  (* event flag_context *)
  ; `Global
    "ALTER TABLE \"events\"
        ADD COLUMN IF NOT EXISTS
          flag_context TEXT DEFAULT '' NOT NULL"


  (* event delays *)
  ; `Global
    "ALTER TABLE \"events\"
       ADD COLUMN IF NOT EXISTS
         delay_until TIMESTAMP"

  (* make a namespace for each canvas *)
  ; `EachCanvas
      "CREATE SCHEMA IF NOT EXISTS \"{SCHEMA}\""

  (* make an events table for each canvas *)
  ; `EachCanvas
      "CREATE TABLE IF NOT EXISTS
        \"{SCHEMA}\".\"events\"
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
        )"

  ; `EachCanvas
    "CREATE INDEX IF NOT EXISTS \"idx_dequeue\"
       ON \"{SCHEMA}\".\"events\"
       (space, name, canvas, status, id)"

  ; `EachCanvas
    "CREATE INDEX IF NOT EXISTS \"idx_cleanup\"
      ON \"{SCHEMA}\".\"events\"
      (dequeued_by)"
  ]

(* TODO: couldn't figure out format stirngs in timebox allocated *)
let migrate_canvas (template: string) (canvas_name: string) =
  let schema_name = "dark_user_" ^ canvas_name in
  Util.string_replace "{SCHEMA}" schema_name template
  |> Db.run_sql


let run () : unit =
  try
    List.iter migrations ~f:(fun m ->
      match m with
      | `Global sql -> Db.run_sql sql
      | `EachCanvas template ->
        List.iter (Serialize.current_hosts ())
          ~f:(migrate_canvas template)
      )

  with
  | Postgresql.Error msg as e ->
    Log.erroR "sql error" msg;
    raise e



(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit  =
  run ()

