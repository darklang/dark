-- https://github.com/inhabitedtype/ocaml-session/blob/master/backends/postgresql/lwt/session_postgresql_lwt.mli#L39

CREATE TABLE IF NOT EXISTS session
( session_key CHAR(40)
, expire_date TIMESTAMP (2) WITH TIME ZONE
, session_data TEXT
)
