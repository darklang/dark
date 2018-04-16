open Core

open Types
open Types.DbT
open Types.RuntimeT

module RT = Runtime

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

let find_db tables table_name : DbT.db =
  match List.find ~f:(fun d -> d.display_name = String.capitalize table_name) tables with
   | Some d -> d
   | None -> failwith ("table not found: " ^ table_name)


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


(* ------------------------- *)
(* SQL Type Conversions; here placed to avoid OCaml circular dep issues *)
(* ------------------------- *)
let rec dval_to_sql (dv: dval) : string =
  match dv with
  | DInt i -> string_of_int i
  | DID i -> "'" ^ Uuid.to_string i ^ "'::uuid" (* needs a cast *)
  | DBool b -> if b then "TRUE" else "FALSE"
  | DChar c -> Char.to_string c
  | DStr s -> "'" ^ (escape s) ^ "'"
  | DFloat f -> string_of_float f
  | DNull -> "NULL"
  | DDate d ->
    "TIMESTAMP WITH TIME ZONE '"
    ^ Dval.sqlstring_of_date d
    ^ "'"
  | DList l ->
    "'{ "
    ^ (String.concat ~sep:", " (List.map ~f:dval_to_sql l))
    ^ " }'"
  | _ -> Exception.client ("We don't know how to convert a " ^ Dval.tipename dv ^ " into the DB format")

let escape_col (keyname: string) : string =
  keyname
  |> escape
  |> fun name -> "\"" ^ name ^ "\""

let col_names names : string =
  names
  |> List.map ~f:escape_col
  |> String.concat ~sep:", "

let key_names (vals: dval_map) : string =
  vals
  |> DvalMap.keys
  |> col_names

let val_names (vals: dval_map) : string =
  vals
  |> DvalMap.data
  |> List.map ~f:dval_to_sql
  |> String.concat ~sep:", "

(* Turn db rows into list of string/type pairs - removes elements with
 * holes, as they won't have been put in the DB yet *)
let cols_for (db: db) : (string * tipe) list =
  db.cols
  |> List.filter_map ~f:(fun c ->
    match c with
    | Filled (_, name), Filled (_, tipe) ->
      Some (name, tipe)
    | _ ->
      None)
  |> fun l -> ("id", TID) :: l

let fetch_via_sql ?(quiet=false) (sql: string) : string list list =
  if quiet
  then
    ()
  else
    Log.infO "fetching via sql" sql;
  sql
  |> conn#exec ~expect:PG.[Tuples_ok]
  |> (fun res -> res#get_all_lst)

(*
 * Dear god, OCaml this is the worst
 * *)
let rec sql_to_dval tables (tipe: tipe) (sql: string) : dval =
  match tipe with
  | TID -> sql |> Uuid.of_string |> DID
  | TInt -> sql |> int_of_string |> DInt
  | TTitle -> sql |> DTitle
  | TUrl -> sql |> DUrl
  | TStr -> sql |> DStr
  | TDate ->
    DDate (if sql = ""
           then Time.epoch
           else Dval.date_of_sqlstring sql)
  | TBelongsTo table ->
    (* fetch here for now *)
    let id = sql |> Uuid.of_string |> DID in
    let db = find_db tables table in
    (match (fetch_by ~tables db "id" id) with
    | DList l -> List.hd_exn l
    | _ -> failwith "should never happen, fetch_by returns a DList")
  | THasMany table ->
    (* we get the string "{ foo, bar, baz }" back *)
    let split =
      sql
      |> fun s -> String.drop_prefix s 1
      |> fun s -> String.drop_suffix s 1
      |> fun s -> String.split s ~on:','
    in
    let ids =
      if split = [""]
      then []
      else
        split
        |> List.map ~f:(fun s -> s |> String.strip |> Uuid.of_string |> DID)
    in
    let db = find_db tables table in
    (* TODO(ian): fix the N+1 here *)
    List.map
      ~f:(fun i ->
          (match (fetch_by ~tables db "id" i) with
           | DList l -> List.hd_exn l
           | _ -> failwith "should never happen, fetch_by returns a DList")
        ) ids
    |> DList
  | _ -> failwith ("type not yet converted from SQL: " ^ sql ^
                   (Dval.tipe_to_string tipe))
and
fetch_by ~tables db (col: string) (dv: dval) : dval =
  let (names, types) = cols_for db |> List.unzip in
  let colnames = col_names names in
  Printf.sprintf
    "SELECT %s FROM \"%s\" WHERE %s = %s"
    colnames (escape db.actual_name) (escape_col col) (dval_to_sql dv)
  |> fetch_via_sql
  |> List.map ~f:(to_obj tables names types)
  |> DList
and
(* PG returns lists of strings. This converts them to types using the
 * row info provided *)
to_obj tables (names : string list) (types: tipe list) (db_strings : string list)
  : dval =
  db_strings
  |> List.map2_exn ~f:(sql_to_dval tables) types
  |> List.zip_exn names
  |> Dval.to_dobj

let sql_tipe_for (tipe: tipe) : string =
  match tipe with
  | TAny -> failwith "todo sql type"
  | TInt -> "INT"
  | TFloat -> failwith "todo sql type"
  | TBool -> "BOOLEAN"
  | TNull -> failwith "todo sql type"
  | TChar -> failwith "todo sql type"
  | TStr -> "TEXT"
  | TList -> failwith "todo sql type"
  | TObj -> failwith "todo sql type"
  | TIncomplete -> failwith "todo sql type"
  | TError -> failwith "todo sql type"
  | TBlock -> failwith "todo sql type"
  | TResp -> failwith "todo sql type"
  | TDB -> failwith "todo sql type"
  | TID | TBelongsTo _ -> "UUID"
  | THasMany _ -> "integer ARRAY"
  | TDate -> "TIMESTAMP WITH TIME ZONE"
  | TTitle -> "TEXT"
  | TUrl -> "TEXT"


(* ------------------------- *)
(* frontend stuff *)
(* ------------------------- *)
let dbs_as_env (dbs: db list) : dval_map =
  dbs
  |> List.map ~f:(fun (db: db) -> (db.display_name, DDB db))
  |> DvalMap.of_alist_exn

let dbs_as_exe_env (dbs: db list) : dval_map =
  dbs_as_env dbs

(* ------------------------- *)
(* actual DB stuff *)
(* ------------------------- *)


let run_sql ?(quiet=false) (sql: string) : unit =
  if quiet
  then ()
  else
    Log.infO "sql" sql ~stop:10000;
  ignore (conn#exec ~expect:[PG.Command_ok] sql)


let with_postgres fn =
  try
    fn ()
  with
  | PG.Error e ->
    Exception.internal ("DB error with: " ^ (PG.string_of_error e))

let is_relation (valu: dval) : bool =
  match valu with
  | DObj _ -> true
  | DList l ->
    List.for_all ~f:Dval.is_obj l
  | _ -> false

let rec insert ~tables (db: db) (vals: dval_map) : Uuid.t =
  let id = Uuid.create () in
  let vals = DvalMap.set ~key:"id" ~data:(DID id) vals in
  (* split out complex objects *)
  let objs, normal =
    Map.partition_map
      ~f:(fun v -> if is_relation v then `Fst v else `Snd v) vals
  in
  let cols = cols_for db in
  (* insert complex objects into their own table, return the inserted ids *)
  let obj_id_map = Map.mapi ~f:(upsert_dependent_object tables cols) objs in
  (* merge the maps *)
  let merged = Util.merge_left normal obj_id_map in
  let _ = Printf.sprintf "INSERT into \"%s\" (%s) VALUES (%s)"
      (escape db.actual_name) (key_names merged) (val_names merged)
          |> run_sql
  in
    id
and update ~tables db (vals: dval_map) =
  let id = DvalMap.find_exn vals "id" in
  (* split out complex objects *)
  let objs, normal =
    Map.partition_map
      ~f:(fun v -> if is_relation v then `Fst v else `Snd v) vals
  in
  let cols = cols_for db in
  (* update complex objects *)
  let obj_id_map = Map.mapi ~f:(upsert_dependent_object tables cols) objs in
  let merged = Util.merge_left normal obj_id_map in
  let sets = merged
           |> DvalMap.to_alist
           |> List.map ~f:(fun (k,v) ->
               (escape_col k) ^ " = " ^ dval_to_sql v)
           |> String.concat ~sep:", " in
  Printf.sprintf "UPDATE \"%s\" SET %s WHERE id = %s"
    (escape db.actual_name) sets (dval_to_sql id)
  |> run_sql
and upsert_dependent_object tables cols ~key:relation ~data:obj : dval =
  (* find table via coltype *)
  let table_name =
    let (cname, ctype) = List.find_exn cols ~f:(fun (n, t) -> n = relation) in
    match ctype with
    | TBelongsTo t | THasMany t -> t
    | _ -> failwith ("Expected TBelongsTo/THasMany, got: " ^ (show_tipe_ ctype))
  in
  let db_obj = find_db tables table_name in
  match obj with
  | DObj m ->
    (match DvalMap.find m "id" with
     | Some existing -> update ~tables db_obj m; existing
     | None -> insert ~tables db_obj m |> DID)
  | DList l ->
    List.map ~f:(fun x -> upsert_dependent_object tables cols ~key:relation ~data:x) l |> DList
  | _ -> failwith ("Expected complex object (DObj), got: " ^ (Dval.to_repr obj))

let fetch_all ~tables (db: db) : dval =
  let (names, types) = cols_for db |> List.unzip in
  let colnames = col_names names in
  Printf.sprintf
    "SELECT %s FROM \"%s\""
    colnames (escape db.actual_name)
  |> fetch_via_sql
  |> List.map ~f:(to_obj tables names types)
  |> DList

let delete ~tables db (vals: dval_map) =
  let id = DvalMap.find_exn vals "id" in
  Printf.sprintf "DELETE FROM \"%s\" WHERE id = %s"
    (escape db.actual_name) (dval_to_sql id)
  |> run_sql

let count db =
  Printf.sprintf "SELECT COUNT(*) AS c FROM \"%s\""
    (escape db.actual_name)
  |> fetch_via_sql
  |> List.hd_exn
  |> List.hd_exn
  |> int_of_string

(* ------------------------- *)
(* run all db and schema changes as migrations *)
(* ------------------------- *)
let run_migration (host: string) (id: id) (sql:string) : unit =
  let host = escape host in
  Log.infO "sql" sql;
  Printf.sprintf
    "DO
       $do$
         BEGIN
           IF ((SELECT COUNT(*) FROM migrations WHERE id = %d
                                                  AND host = '%s') = 0)
           THEN
             %s;
             INSERT INTO migrations (id, host, sql)
               VALUES (%d, '%s', (quote_literal('%s')));
           END IF;
         END
       $do$;
     COMMIT;" id host sql id host (escape sql)
  |> run_sql

(* -------------------------
(* SQL for DB *)
 * TODO: all of the SQL here is very very easily SQL injectable.
 * This MUST be fixed before we go to production
 * ------------------------- *)

let create_table_sql (table_name: string) =
  Printf.sprintf
    "CREATE TABLE IF NOT EXISTS \"%s\" (id UUID PRIMARY KEY)"
    (escape table_name)

let add_col_sql (table_name: string) (name: string) (tipe: tipe) : string =
  Printf.sprintf
    "ALTER TABLE \"%s\" ADD COLUMN \"%s\" %s"
    (escape table_name) (escape name) (sql_tipe_for tipe)



(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let to_display_name (name: string) =
  if name
     |> String.to_list
     |> List.for_all ~f:Char.is_uppercase
  then name
       |> String.lowercase
       |> String.capitalize
  else String.capitalize name

let create_new_db (db: db) =
  run_migration db.host db.tlid (create_table_sql db.actual_name)

(* we only add this when it is complete, and we use the ID to mark the
   migration table to know whether it's been done before. *)
let maybe_add_to_actual_db (db: db) (id: id) (col: col) (do_db_ops: bool) : col =
  if do_db_ops
  then
    (match col with
    | Filled (_, name), Filled (_, tipe) ->
      run_migration db.host id (add_col_sql db.actual_name name tipe)
    | _ ->
      ())
  else ();
  col


let add_db_col colid typeid (db: db) =
  { db with cols = db.cols @ [(Blank colid, Blank typeid)]}

let set_col_name id name (do_db_ops: bool) db =
  let set col =
    match col with
    | (Blank hid, tipe) when hid = id ->
        maybe_add_to_actual_db db id (Filled (hid, name), tipe) do_db_ops
    | _ -> col in
  { db with cols = List.map ~f:set db.cols }

let set_db_col_type id tipe (do_db_ops: bool) db =
  let set col =
    match col with
    | (name, Blank hid) when hid = id ->
        maybe_add_to_actual_db db id (name, Filled (hid, tipe)) do_db_ops
    | _ -> col in
  { db with cols = List.map ~f:set db.cols }

(* ------------------------- *)
(* Serializing canvases *)
(* ------------------------- *)
let save_oplists (host: string) (data: string) : unit =
  let data = escapea data in
  Printf.sprintf
    (* this is an upsert *)
    "INSERT INTO oplists
    (host, data) VALUES ('%s', '%s')
    ON CONFLICT (host) DO UPDATE
    SET data = '%s';"
    (escape host)
    data
    data
  |> run_sql ~quiet:true

let load_oplists (host: string) : string option =
  (* It's quite a bit of work to make a binary bytea go into the DB and
   * come out in the same shape:
   *
   * https://www.postgresql.org/docs/9.6/static/datatype-binary.html
   *
   * Postgres advices us to parse the hex format, above. We tried base64
   * as well, which is approx twice as slow. *)
  Printf.sprintf
    "SELECT encode(data, 'hex') FROM oplists
     WHERE host = '%s';"
    (escape host)
  |> fetch_via_sql ~quiet:true
  |> List.hd
  |> Option.value_map ~default:None ~f:List.hd
  |> Option.map ~f:bytea_of_string_hex



(* ------------------------- *)
(* Some initialization *)
(* ------------------------- *)
let init () : unit  =
  run_sql "CREATE TABLE IF NOT EXISTS migrations (id BIGINT, host TEXT, sql TEXT, PRIMARY KEY (id, host))";
  (* https://github.com/inhabitedtype/ocaml-session/blob/master/backends/postgresql/lwt/session_postgresql_lwt.mli#L39 *)
  run_sql "CREATE TABLE IF NOT EXISTS session (session_key CHAR(40), expire_date TIMESTAMP (2) WITH TIME ZONE, session_data TEXT)";
  run_sql "CREATE INDEX IF NOT EXISTS session_key_idx ON \"session\" (session_key)";
  run_sql "CREATE TABLE IF NOT EXISTS oplists (host VARCHAR(64) PRIMARY KEY, data BYTEA)";
