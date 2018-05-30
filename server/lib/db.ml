open Core

module PG = Postgresql
open Dbprim

(* ------------------------- *)
(* Low-level API *)
(* ------------------------- *)

let escape = Dbprim.escape
let escapea = Dbprim.escapea

let conn = Dbconnection.conn

let literal_of_uuid (u: Uuid.t) : string =
  "'" ^ Uuid.to_string u ^ "'::uuid"

let run_sql ?(quiet=false) (sql: string) : unit =
  if quiet
  then ()
  else
    Log.infO "sql" sql ~stop:10000;
  try
    ignore (conn#exec ~expect:[PG.Command_ok] sql)
  with
  | Postgresql.Error pge as e ->
    Exception.reraise_after e
      (fun _ -> Log.erroR "Postgres error" pge)
  | e ->
    Exception.reraise e


let fetch_via_sql ?(quiet=false) (sql: string) : string list list =
  if quiet
  then
    ()
  else
    Log.infO "fetching via sql" sql;
  try
    sql
    |> conn#exec ~expect:PG.[Tuples_ok]
    |> (fun res -> res#get_all_lst)
  with
  | Postgresql.Error pge as e ->
    Exception.reraise_after e
      (fun _ -> Log.erroR "Postgres error" pge)
  | e ->
    Exception.reraise e



let exists_via_sql ?(quiet=false) (sql: string) : bool =
  fetch_via_sql ~quiet sql = [["1"]]


(* ------------------------- *)
(* Postgres schemas (namespacing) *)
(* ------------------------- *)
let ns_name host : string =
  Printf.sprintf
    "dark_user_%s"
    (escape host)

(* Sets the namespace. Technically, this happens within the
 * transaction, but every pg query uses autocommit by default in pg
 * >9.5, so we're good. *)
let in_ns ~(host:string) (sql: string) : string =
  let ns = ns_name host in
  Printf.sprintf
    " SET LOCAL search_path TO \"%s\";
       %s;
     "
    ns
    sql

let fetch_via_sql_in_ns ?(quiet=false) ~host sql =
  let sql = in_ns ~host sql in
  fetch_via_sql ~quiet sql

let run_sql_in_ns ?(quiet=false) ~host sql =
  let sql = in_ns ~host sql in
  run_sql ~quiet sql


(* ------------------------- *)
(* Schemas *)
(* ------------------------- *)
let create_namespace host : unit =
  Printf.sprintf
    "CREATE SCHEMA IF NOT EXISTS \"%s\""
    (ns_name host)
  |> run_sql

let all_schemas () : string list =
  "SELECT schema_name
   FROM information_schema.schemata
   WHERE schema_name LIKE 'dark\\_user\\_%%'"
  |> fetch_via_sql ~quiet:true
  |> List.concat
  |> List.map ~f:(String.chop_prefix_exn ~prefix:"dark_user_")

let delete_schema (host:string) : unit =
  Printf.sprintf
    "DROP SCHEMA IF EXISTS \"%s\" CASCADE;"
    (ns_name host)
  |> run_sql ~quiet:false

let delete_underscore_testdata () : unit =
  all_schemas ()
  |> List.filter ~f:(String.is_prefix ~prefix:"test_")
  |> List.iter ~f:delete_schema

let delete_hyphen_testdata () : unit =
  all_schemas ()
  |> List.filter ~f:(String.is_prefix ~prefix:"test_")
  |> List.iter ~f:delete_schema

(* ------------------------- *)
(* Schemas *)
(* ------------------------- *)
let save_oplists (host: string) (digest: string) (data: string) : unit =
  let data = escapea data in
  Printf.sprintf
    (* this is an upsert *)
    "INSERT INTO oplists
    (host, digest, data) VALUES ('%s', '%s', '%s')
    ON CONFLICT (host, digest) DO UPDATE
    SET data = '%s';"
    (escape host)
    digest
    data
    data
  |> run_sql ~quiet:true

let load_oplists (host: string) (digest: string) : string option =
  (* It's quite a bit of work to make a binary bytea go into the DB and
   * come out in the same shape:
   *
   * https://www.postgresql.org/docs/9.6/static/datatype-binary.html
   *
   * Postgres advices us to parse the hex format, above. We tried base64
   * as well, which is approx twice as slow. *)
  Printf.sprintf
    "SELECT encode(data, 'hex') FROM oplists
     WHERE host = '%s'
     AND digest = '%s';"
    (escape host)
    digest
  |> fetch_via_sql ~quiet:true
  |> List.hd
  |> Option.value_map ~default:None ~f:List.hd
  |> Option.map ~f:bytea_of_string_hex

let all_oplists (digest: string) : string list =
  "SELECT host
  FROM oplists
  WHERE digest = '%s'"
  |> fetch_via_sql ~quiet:true
  |> List.concat
  |> List.filter ~f:(fun h ->
      not (String.is_prefix ~prefix:"test-" h))

let delete_hyphen_test_oplists () : unit =
  "DELETE FROM oplists
  WHERE host like 'test-%%'"
  |> run_sql ~quiet:false


let delete_underscore_test_oplists () : unit =
  "DELETE FROM oplists
  WHERE host like 'test\\_%%'"
  |> run_sql ~quiet:false



let delete_testdata () : unit =
  delete_underscore_testdata ();
  delete_hyphen_testdata ();
  delete_hyphen_test_oplists ();
  delete_underscore_test_oplists ();



