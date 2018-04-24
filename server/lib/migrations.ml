open Core

let migrations =
  (* user DB migrations *)
  [ "CREATE TABLE IF NOT EXISTS
       migrations
       ( id BIGINT
       , host TEXT
       , sql TEXT
       , PRIMARY KEY (id, host))"

  (* user sessions *)
  (* https://github.com/inhabitedtype/ocaml-session/blob/master/backends/postgresql/lwt/session_postgresql_lwt.mli#L39 *)
  ; "CREATE TABLE IF NOT EXISTS
       session
       ( session_key CHAR(40)
       , expire_date TIMESTAMP (2) WITH TIME ZONE
       , session_data TEXT
       )"

  (* user session indices *)
  ; "CREATE INDEX IF NOT EXISTS
      session_key_idx
      ON \"session\"
      (session_key)"

  (* serialize canvases *)
  ; "CREATE TABLE IF NOT EXISTS
      oplists
      ( host VARCHAR(64)
      , digest CHAR(32)
      , data BYTEA
      , PRIMARY KEY (host, digest))"

  (* type for queue state *)
    (* there's no CREATE TYPE IF NOT EXISTS :/ *)
  ; "DO $$
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
  ; "CREATE TABLE IF NOT EXISTS
      \"events\"
      (id SERIAL PRIMARY KEY
      , status queue_status
      , dequeued_by INT
      , canvas TEXT NOT NULL
      , space TEXT NOT NULL
      , name TEXT NOT NULL
      , value TEXT NOT NULL)"


  (* queue index *)
  ; "CREATE INDEX IF NOT EXISTS
       \"idx_dequeue\"
       ON \"events\"
       (space, name, canvas, status, id)"


  (* queue cleanup index *)
  ; "CREATE INDEX IF NOT EXISTS \"idx_cleanup\"
      ON \"events\"
      (dequeued_by)"


  (* event retries *)
  ; "ALTER TABLE \"events\"
      ADD COLUMN IF NOT EXISTS
        retries INTEGER DEFAULT 0 NOT NULL"


  (* event flag_context *)
  ; "ALTER TABLE \"events\"
        ADD COLUMN IF NOT EXISTS
          flag_context TEXT DEFAULT '' NOT NULL"


  (* event delays *)
  ; "ALTER TABLE \"events\"
       ADD COLUMN IF NOT EXISTS
         delay_until TIMESTAMP"

  ]

let run () : unit =
  try
    List.iter migrations ~f:Db.run_sql
  with
  | Postgresql.Error msg as e ->
    Log.erroR "sql error" msg;
    raise e



(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit  =
  run ()

