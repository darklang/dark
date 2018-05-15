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

  ; `EachCanvas
      "DO $$
         BEGIN
           BEGIN
             ALTER TABLE \"events\"
               RENAME COLUMN flag_content TO flag_context;
           EXCEPTION
             WHEN OTHERS
             THEN
               RAISE NOTICE 'ignoring error: %', SQLERRM;
           END;
         END;
       $$"

  ; `EachCanvas
    "CREATE TABLE IF NOT EXISTS
       migrations
       ( id BIGINT
       , sql TEXT
       , PRIMARY KEY (id))"

  ; `EachCanvas
    "INSERT INTO migrations (id, sql)
     SELECT id, sql
     FROM public.migrations
     WHERE host = '{HOST}'
     ON CONFLICT DO NOTHING"

  ; `ForEach ( "SELECT replace(table_name, '{HOST}_', '')
                FROM information_schema.tables
                WHERE table_schema = 'public'
                  AND table_name LIKE '{HOST}\\_%%'"
             , "ALTER TABLE \"{HOST}_{VALUE}\"
                  SET SCHEMA \"{NS}\";
                ALTER TABLE \"{NS}\".\"{HOST}_{VALUE}\"
                  RENAME TO \"user_{VALUE}\";
                "
             )

  ; `EachCanvas
    "CREATE INDEX IF NOT EXISTS
       \"idx_dequeue\"
       ON \"events\"
       (space, name, status, id)"


  ; `EachCanvas
    "CREATE INDEX IF NOT EXISTS \"idx_cleanup\"
      ON \"events\"
      (dequeued_by)"

  (* no longer necessary, since they're per-host *)
  ; `EachCanvas
    "ALTER TABLE \"events\"
       DROP COLUMN IF EXISTS \"canvas\""

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
     users
     ( id UUID PRIMARY KEY
     , username VARCHAR(255) NOT NULL
     , name VARCHAR(255) NOT NULL
     , email VARCHAR(255) NOT NULL
     , password VARCHAR(255) NOT NULL
     , created_at TIMESTAMP NOT NULL DEFAULT NOW()
     , updated_at TIMESTAMP NOT NULL DEFAULT NOW())"

  ; `Global
    "DROP TRIGGER IF EXISTS set_user_timestamp
     ON users;
     CREATE TRIGGER set_user_timestamp
     BEFORE UPDATE ON users
     FOR EACH ROW
     EXECUTE PROCEDURE trigger_set_timestamp();"

  (* orgs *)
  ; `Global
    "CREATE TABLE IF NOT EXISTS
     orgs
     ( id UUID PRIMARY KEY
     , name VARCHAR(255) NOT NULL
     , created_at TIMESTAMP NOT NULL DEFAULT NOW()
     , updated_at TIMESTAMP NOT NULL DEFAULT NOW())"

  ; `Global
    "DROP TRIGGER IF EXISTS set_org_timestamp
     ON orgs;
     CREATE TRIGGER set_org_timestamp
     BEFORE UPDATE ON orgs
     FOR EACH ROW
     EXECUTE PROCEDURE trigger_set_timestamp();"

  (* memberships *)
  ; `Global
    "CREATE TABLE IF NOT EXISTS
     org_memberships
     ( user_id UUID REFERENCES users(id)
     , org_id UUID REFERENCES orgs(id)
     , created_at TIMESTAMP NOT NULL DEFAULT NOW()
     , updated_at TIMESTAMP NOT NULL DEFAULT NOW()
     , PRIMARY KEY (user_id, org_id))"

  ; `Global
    "DROP TRIGGER IF EXISTS set_org_membership_timestamp
     ON org_memberships;
     CREATE TRIGGER set_org_membership_timestamp
     BEFORE UPDATE ON org_memberships
     FOR EACH ROW
     EXECUTE PROCEDURE trigger_set_timestamp();"

   ; `Global
   "CREATE TABLE IF NOT EXISTS
    canvases
    ( id UUID PRIMARY KEY
    , org_id UUID REFERENCES orgs(id) NOT NULL
    , name VARCHAR(40) NOT NULL
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
     , org_id UUID REFERENCES orgs(id) NOT NULL
     , space TEXT NOT NULL
     , name TEXT NOT NULL
     , value TEXT NOT NULL
     , retries INTEGER DEFAULT 0 NOT NULL
     , flag_context TEXT DEFAULT '' NOT NULL
     , delay_until TIMESTAMP NOT NULL DEFAULT NOW()
     )"

  (* queue index *)
  ; `Global
    "CREATE INDEX IF NOT EXISTS
       idx_dequeue
       ON events
       (org_id, canvas_id, space, name, status, delay_until, id)"


  (* queue cleanup index *)
  ; `Global
    "CREATE INDEX IF NOT EXISTS idx_cleanup
     ON events
     (dequeued_by, status)"

   ]

let migrate_canvas (template: string) (host: string) =
  template
  |> Util.string_replace "{NS}" (Db.ns_name host)
  |> Util.string_replace "{HOST}" host
  |> Db.run_sql_in_ns ~host

(* don't run in schema *)
let migrate_canvas_raw (template: string) (host: string) =
  template
  |> Util.string_replace "{NS}" (Db.ns_name host)
  |> Util.string_replace "{HOST}" host
  |> Db.run_sql

let migrate_foreach (search: string) (template: string) (host: string) =
  let all = search
            |> Util.string_replace "{NS}" (Db.ns_name host)
            |> Util.string_replace "{HOST}" host
            |> Db.fetch_via_sql
            |> List.concat
  in
  List.iter all
    ~f:(fun value ->
          template
          |> Util.string_replace "{NS}" (Db.ns_name host)
          |> Util.string_replace "{HOST}" host
          |> Util.string_replace "{VALUE}" value
          |> Db.run_sql)



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
      | `ForEach (search, template) ->
        List.iter (Serialize.current_hosts ())
          ~f:(migrate_foreach search template)
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

