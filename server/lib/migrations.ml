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

  (* moved to namespaces *)
  ; `Global
    "DROP TABLE IF EXISTS \"migrations\" CASCADE"

  (* timestamp triggers *)
  ; `Global
    "CREATE OR REPLACE FUNCTION trigger_set_timestamp()
     RETURNS TRIGGER AS $$
     BEGIN
       NEW.updated_at = NOW();
       RETURN NEW;
     END;
     $$ LANGUAGE plpgsql;"

  (* users *)
  ; `Global
    "CREATE TABLE IF NOT EXISTS
     accounts
     ( id UUID PRIMARY KEY
     , username VARCHAR(255) UNIQUE NOT NULL
     , name VARCHAR(255) NOT NULL
     , email VARCHAR(255) NOT NULL
     , admin BOOL NOT NULL DEFAULT FALSE
     , password VARCHAR(255) NOT NULL
     , created_at TIMESTAMP NOT NULL DEFAULT NOW()
     , updated_at TIMESTAMP NOT NULL DEFAULT NOW())"

  ; `Global
    "DROP TRIGGER IF EXISTS set_account_timestamp
     ON accounts;
     CREATE TRIGGER set_account_timestamp
     BEFORE UPDATE ON accounts
     FOR EACH ROW
     EXECUTE PROCEDURE trigger_set_timestamp();"

  (* canvas *)
   ; `Global
   "CREATE TABLE IF NOT EXISTS
    canvases
    ( id UUID PRIMARY KEY
    , account_id UUID REFERENCES accounts(id) NOT NULL
    , name VARCHAR(64) UNIQUE NOT NULL
    , created_at TIMESTAMP NOT NULL DEFAULT NOW()
    , updated_at TIMESTAMP NOT NULL DEFAULT NOW())"

  ; `Global
    "DROP TRIGGER IF EXISTS set_canvas_timestamp
     ON canvases;
     CREATE TRIGGER set_canvas_timestamp
     BEFORE UPDATE ON canvases
     FOR EACH ROW
     EXECUTE PROCEDURE trigger_set_timestamp();"

  (* events *)
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

  ; `Global
    "CREATE TABLE IF NOT EXISTS
     events
     (id SERIAL PRIMARY KEY
     , status queue_status NOT NULL
     , dequeued_by INT
     , canvas_id UUID REFERENCES canvases(id) NOT NULL
     , account_id UUID REFERENCES accounts(id) NOT NULL
     , space TEXT NOT NULL
     , name TEXT NOT NULL
     , value TEXT NOT NULL
     , retries INTEGER DEFAULT 0 NOT NULL
     , flag_context TEXT DEFAULT '' NOT NULL
     , delay_until TIMESTAMP NOT NULL DEFAULT NOW()
     )"

  (* queue cleanup index *)
  ; `Global
    "CREATE INDEX IF NOT EXISTS
     idx_cleanup
     ON events
     (dequeued_by, status)"

  (* queue index *)
  ; `Global
    "CREATE INDEX IF NOT EXISTS
     idx_dequeue
     ON events
     (account_id, canvas_id, space, name, status, delay_until, id)"

   ]


let run () : unit =
  List.iter migrations ~f:(fun m ->
    match m with
    | `Global sql -> Db.run_sql sql
    )


(* ------------------------- *)
(* Initialization *)
(* ------------------------- *)

let init () : unit  =
  run ()

