open Core

module PG = Postgresql

(* globals *)
let rec rec_con depth =
  try
    let db = Config.postgres_settings in
    let c = new PG.connection ~host:db.host ~dbname:db.dbname
      ~user:db.user ~password:db.password ()  in
    c#set_notice_processor (Log.infO "postgres");
    c
  with
  | e ->
      Log.infO "Couldn't connect to postgres, attempt " depth;
      if depth < 10
      then
        (Unix.sleep 1;
         rec_con (depth+1))
      else
        raise e


let conn = rec_con 0

let escape (s: string) : string =
  conn#escape_string s

let escapea (s: string) : string =
  conn#escape_bytea s


(* Hex converter stolen from PGOcaml, with modifications because our
 * string does not start with "\\x" *)
let is_hex_digit = function '0'..'9' | 'a'..'f' | 'A'..'F' -> true
                                     | _ -> false

let hex_val c =
  let offset = match c with
    | '0'..'9' -> 0x30
    | 'a'..'f' -> 0x57
    | 'A'..'F' -> 0x37
    | _	       -> failwith "hex_val"
  in Char.to_int c - offset

(* Deserialiser for the new 'hex' format introduced in PostgreSQL 9.0. *)
let bytea_of_string_hex str =
  let len = String.length str in
  let buf = Buffer.create ((len)/2) in
  let i = ref 1 in
  while !i < len do
    let hi_nibble = str.[!i-1] in
    let lo_nibble = str.[!i] in
    i := !i+2;
    if is_hex_digit hi_nibble && is_hex_digit lo_nibble
    then begin
      let byte = ((hex_val hi_nibble) lsl 4) + (hex_val lo_nibble) in
      Buffer.add_char buf (Char.of_int_exn byte)
    end
  done;
  Buffer.contents buf

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

let create_namespace host : unit =
  Printf.sprintf
    "CREATE SCHEMA IF NOT EXISTS \"%s\""
    (ns_name host)
  |> run_sql

(* ------------------------- *)
(* Serializing canvases *)
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

let delete_test_oplists () : unit =
  "DELETE FROM oplists
  WHERE host like 'test-%%'"
  |> run_sql ~quiet:false


let delete_underscore_test_oplists () : unit =
  "DELETE FROM oplists
  WHERE host like 'test\\_%%'"
  |> run_sql ~quiet:false

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

let delete_underscope_testdata () : unit =
  all_schemas ()
  |> List.filter ~f:(String.is_prefix ~prefix:"test_")
  |> List.iter ~f:delete_schema



let delete_testdata () : unit =
  all_schemas ()
  |> List.filter ~f:(String.is_prefix ~prefix:"test-")
  |> List.iter ~f:delete_schema
  ;
  delete_test_oplists ();
  delete_underscore_test_oplists ();
  delete_test_oplists ()



