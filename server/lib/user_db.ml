open Core

open Types
open Types.RuntimeT
open Types.RuntimeT.DbT

module RT = Runtime

module Dbp = Dbprim

open Db

let user_data_table = "user_data"

(* bump this if you make a breaking change to
 * the underlying data format, and are migrating
 * user data to the new version
 *
 * ! you should definitely notify the entire engineering
 * ! team about this
 *)
let current_dark_version = 0

let find_db tables table_name : db =
  match List.find ~f:(fun (d : db) -> d.display_name = String.capitalize table_name) tables with
   | Some d -> d
   | None -> failwith ("table not found: " ^ table_name)

let key_names (vals: dval_map) : string =
  vals
  |> DvalMap.keys
  |> Dbp.cols

let val_names (vals: dval_map) : string =
  vals
  |> DvalMap.data
  |> Dbp.dvals

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

(*
 * Dear god, OCaml this is the worst
 * *)
let rec sql_to_dval tables (tipe: tipe) (sql: string) : dval =
  match tipe with
  | TID -> sql |> Uuid.of_string |> DID
  | TInt -> sql |> int_of_string |> DInt
  | TFloat -> sql |> float_of_string |> DFloat
  | TTitle -> sql |> DTitle
  | TUrl -> sql |> DUrl
  | TStr -> sql |> DStr
  | TBool ->
    (match sql with
    | "f" -> DBool false
    | "t" -> DBool true
    | b -> failwith ("bool should be true or false: " ^ b))
  | TDate ->
    DDate (if sql = ""
           then Time.epoch
           else Dval.date_of_sqlstring sql)
  | TBelongsTo table ->
    (* fetch here for now *)
    let id = sql |> Uuid.of_string |> DID in
    let db = find_db tables table in
    (match (fetch_by ~tables db "id" id) with
     | DList (a :: _) -> a
     | DList _ -> DNull
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
  | TDbList tipe ->
    sql
    |> fun s -> String.drop_prefix s 1
    |> fun s -> String.drop_suffix s 1
    |> fun s -> String.split s ~on:','
    |> List.filter ~f:(fun s -> not (String.is_empty s))
    |> List.map ~f:(fun v -> sql_to_dval tables tipe v)
    |> DList
  | _ -> failwith ("type not yet converted from SQL: " ^ sql ^
                   (Dval.tipe_to_string tipe))
and
fetch_by ~tables db (col: string) (dv: dval) : dval =
  let (names, types) = cols_for db |> List.unzip in
  Printf.sprintf
    "SELECT %s FROM %s WHERE %s = %s"
    (Dbp.cols names)
    (Dbp.table db.actual_name)
    (Dbp.col col)
    (Dbp.dval dv)
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
  Printf.sprintf
    "INSERT into %s
     (%s)
     VALUES (%s)"
    (Dbp.table db.actual_name)
    (key_names merged)
    (val_names merged)
  |> run_sql;
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
               Printf.sprintf
                 "%s = %s"
                 (Dbp.col k)
                 (Dbp.dval v))
           |> String.concat ~sep:", " in
  Printf.sprintf
    "UPDATE %s
    SET %s
    WHERE id = %s"
    (Dbp.table db.actual_name)
    sets
    (Dbp.dval id)
  |> run_sql
and upsert_dependent_object tables cols ~key:relation ~data:obj : dval =
  (* find table via coltype *)
  let table_name =
    let (cname, ctype) =
      try
        List.find_exn cols ~f:(fun (n, t) -> n = relation)
      with e -> RT.error "Trying to create a relation that doesn't exist"
                  ~actual:(DStr relation)
                  ~expected:("one of" ^ Batteries.dump cols)
    in
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
  Printf.sprintf
    "SELECT %s FROM %s"
    (Dbp.cols names)
    (Dbp.table db.actual_name)
  |> fetch_via_sql
  |> List.map ~f:(to_obj tables names types)
  |> DList

let delete ~tables (db: db) (vals: dval_map) =
  let id = DvalMap.find_exn vals "id" in
  (* covered by composite PK index *)
  Printf.sprintf
    "DELETE
     FROM %s
     WHERE id = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.dval id)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> run_sql

let delete_all ~tables (db: db) =
  (* covered by idx_user_data_current_data_for_tlid *)
  Printf.sprintf
    "DELETE
     FROM %s
     WHERE table_tlid = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.tlid db.tlid)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> run_sql

let count (db: db) =
  (* covered by idx_user_data_current_data_for_tlid *)
  Printf.sprintf
    "SELECT COUNT(*) AS c
     FROM %s
     WHERE table_tlid = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.tlid db.tlid)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> fetch_via_sql
  |> List.hd_exn
  |> List.hd_exn
  |> int_of_string

(* ------------------------- *)
(* locked/unlocked (not _locking_) *)
(* ------------------------- *)

let db_locked (db: db) : bool =
  (count db) <> 0

let unlocked (dbs: db list) : db list =
  match dbs with
  | [] -> []
  | db :: _ ->
    let empties =
      (* covered by idx_user_data_current_data_for_tlid
       *
       * this is a little non-obvious, but this query
       * is the reason the index is ordered as
       * (user_version, dark_version, table_tlid)
       * as the `WHERE` is executed before the GROUP BY
       * and this allows the index to cover the full query
       * *)
      Printf.sprintf
        "SELECT table_tlid
         FROM %s
         WHERE user_version = %s
         AND dark_version = %s
         GROUP BY table_tlid
         HAVING COUNT(*) = 0"
        (Dbp.table user_data_table)
        (Dbp.int db.version)
        (Dbp.int current_dark_version)
      |> fetch_via_sql
    in
    dbs
    |> List.filter
      ~f:(fun db ->
          List.mem ~equal:(=) empties [db.tlid |> string_of_int])

(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let create (host:host) (name:string) (id: tlid) : db =
  { tlid = id
  ; host = host
  ; display_name = name
  ; actual_name = "user_" ^ name (* there's a schema too *)
  ; cols = []
  ; version = 0
  ; old_migrations = []
  ; active_migration = None
  }

let add_col colid typeid (db: db) =
  { db with cols = db.cols @ [(Blank colid, Blank typeid)]}

let set_col_name id name db =
  let set col =
    match col with
    | (Blank hid, tipe) when hid = id ->
      (Filled (hid, name), tipe)
    | _ -> col in
  let newcols = List.map ~f:set db.cols in
  { db with cols = newcols }

let change_col_name id name db =
  let change col =
    match col with
    | (Filled (hid, oldname), Filled (tipeid, tipename)) when hid = id ->
      (Filled (hid, name), Filled (tipeid, tipename))
    | _ -> col in
  { db with cols = List.map ~f:change db.cols }

let set_col_type id tipe  db =
  let set col =
    match col with
    | (name, Blank hid) when hid = id ->
      (name, Filled (hid, tipe))
    | _ -> col in
  let newcols = List.map ~f:set db.cols in
  { db with cols = newcols }

let change_col_type id newtipe db =
  let change col =
    match col with
    | (Filled (nameid, name), Filled (tipeid, oldtipe)) when tipeid = id ->
      (Filled (nameid, name), Filled (tipeid, newtipe))
    | _ -> col in
  { db with cols = List.map ~f:change db.cols }

let initialize_migration id rbid rfid kind (db : db) =
  if Option.is_some db.active_migration
  then
    Exception.internal
      ("Attempted to init a migration for a table with an active one: " ^ db.display_name);
  match kind with
  | ChangeColType ->
    let new_migration =
      { starting_version = db.version
      ; kind = kind
      ; rollback = Blank rbid
      ; rollforward = Blank rfid
      ; target = id
      }
    in
    { db with active_migration = Some new_migration }

