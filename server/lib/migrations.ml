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

  ; `EachCanvasRaw
      "CREATE SCHEMA IF NOT EXISTS \"{NS}\""

  ; `EachCanvas
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
          AND n.nspname = '{NS}'
          )
       THEN
         CREATE TYPE queue_status AS
           ENUM ('new', 'locked', 'done', 'error');
       END IF;
     END $$;
    "

  ; `EachCanvas
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


  ]

let migrate_canvas (template: string) (host: string) =
  Util.string_replace "{NS}" (Db.ns_name host) template
  |> Db.run_sql_in_ns ~host

(* don't run in schema *)
let migrate_canvas_raw (template: string) (host: string) =
  Util.string_replace "{NS}" (Db.ns_name host) template
  |> Db.run_sql

let run () : unit =
  try
    List.iter migrations ~f:(fun m ->
      match m with
      | `Global sql -> Db.run_sql sql
      | `EachCanvas template ->
        List.iter (Serialize.current_hosts ())
          ~f:(migrate_canvas template)
      | `EachCanvasRaw template ->
        List.iter (Serialize.current_hosts ())
          ~f:(migrate_canvas_raw template)
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

