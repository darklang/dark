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
  tables
  |> List.find
    ~f:(fun (d : db) -> d.display_name = String.capitalize table_name)
  |> Option.value_exn ~message:("table not found " ^ table_name)

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

let rec fetch_by exec_state db (col: string) (dv: dval) : dval =
  Printf.sprintf
    "SELECT id, data
     FROM %s
     WHERE table_tlid = %s
     AND user_version = %s
     AND dark_version = %s
     AND (data->>%s)%s = %s"
    (Dbp.table user_data_table)
    (Dbp.tlid db.tlid)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
    (Dbp.string col)
    (Dbp.cast_expression_for dv
     |> Option.value ~default:"")
    (Dbp.dvaljson dv)
  |> fetch_via_sql
  |> List.map ~f:(to_obj exec_state db)
  |> DList
and
find exec_state db id  =
  Printf.sprintf
    "SELECT DISTINCT id, data
     FROM %s
     WHERE id = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.uuid id)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> fetch_via_sql
  |> List.concat
  |> to_obj exec_state db
and
find_many exec_state db ids =
  Printf.sprintf
    "SELECT DISTINCT id, data
     FROM %s
     WHERE id IN (%s)
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.list ~serializer:Dbp.uuid ids)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> fetch_via_sql
  |> List.map ~f:(to_obj exec_state db)
  |> DList
and
(* PG returns lists of strings. This converts them to types using the
 * row info provided *)
to_obj exec_state db (db_strings : string list)
  : dval =
  match db_strings with
  | [id; obj] ->
    let p_id =
      id |> Uuid.of_string |> DID
    in
    let p_obj =
      match Dval.dval_of_json_string obj with
      | DObj o -> o
      | x -> Exception.internal ("failed format, expected DObj got: " ^ (obj))
    in
    let merged =
      Map.set ~key:"id" ~data:p_id p_obj
    in
    let type_checked =
      type_check_and_fetch_dependents exec_state db merged
    in
    DObj type_checked
  | _ -> Exception.internal "Got bad format from db fetch"
and type_check_and_fetch_dependents exec_state (db: db) (obj: dval_map) : dval_map =
  let cols = cols_for db |> TipeMap.of_alist_exn in
  let same_keys =
    let tipe_keys = TipeMap.keys cols |> String.Set.of_list in
    let obj_keys = DvalMap.keys obj |> String.Set.of_list in
    String.Set.equal tipe_keys obj_keys
  in
  if same_keys
  then
    DvalMap.mapi
      ~f:(fun ~key ~data ->
          match (TipeMap.find_exn cols key, data) with
          | (TID, DID _) -> data
          | (TInt, DInt _) -> data
          | (TFloat, DFloat _) -> data
          | (TTitle, DTitle _) -> data
          | (TTitle, DStr s) -> DTitle s
          | (TUrl, DUrl _) -> data
          | (TUrl, DStr s) -> DUrl s
          | (TStr, DStr _) -> data
          | (TBool, DBool _) -> data
          | (TDate, DDate _) -> data
          | (TList, DList _) -> data
          | (TDbList _, DList _) -> data
          | (TBelongsTo table, DID id) ->
            let dep_table = find_db exec_state.dbs table in
            (match (fetch_by exec_state dep_table "id" (DID id)) with
            | DList (a :: _) -> a
            | DList _ -> DNull
            | _ -> failwith "should never happen, fetch_by returns a DList")
          | (THasMany table, DList ids) ->
            let dep_table = find_db exec_state.dbs table in
            (* TODO(ian): fix the N+1 here *)
            List.map
              ~f:(fun i ->
                  (match (fetch_by exec_state dep_table "id" i) with
                  | DList l -> List.hd_exn l
                  | _ -> failwith "should never happen, fetch_by returns a DList")
                ) ids
            |> DList
          | (_, error) -> failwith (Dval.dval_to_json_string error) (* TODO(ian) *)
        )
      obj
  else
    failwith "TODO(ian) missing key"

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

let rec insert exec_state (db: db) (vals: dval_map) : Uuid.t =
  let id = Uuid.create () in
  (* split out complex objects *)
  let objs, normal =
    Map.partition_map
      ~f:(fun v -> if is_relation v then `Fst v else `Snd v) vals
  in
  let cols = cols_for db in
  (* insert complex objects into their own table, return the inserted ids *)
  let obj_id_map = Map.mapi ~f:(upsert_dependent_object exec_state cols) objs in
  (* merge the maps *)
  let merged = Util.merge_left normal obj_id_map in
  Printf.sprintf
    "INSERT into %s
     (id, account_id, canvas_id, table_tlid, user_version, dark_version, data)
     VALUES (%s, %s, %s, %s, %s, %s, %s)"
    (Dbp.table user_data_table)
    (Dbp.uuid id)
    (Dbp.uuid exec_state.account_id)
    (Dbp.uuid exec_state.canvas_id)
    (Dbp.int db.tlid)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
    (Dbp.dvalmap_jsonb merged)
  |> run_sql;
  id
and update exec_state db (vals: dval_map) =
  let id =
    match DvalMap.find_exn vals "id" with
    | DID uuid -> uuid
    | _ -> Exception.client "error, id should be a uuid"
  in
  (* split out complex objects *)
  let objs, normal =
    Map.partition_map
      ~f:(fun v -> if is_relation v then `Fst v else `Snd v) vals
  in
  let cols = cols_for db in
  (* update complex objects *)
  let obj_id_map = Map.mapi ~f:(upsert_dependent_object exec_state cols) objs in
  let merged = Util.merge_left normal obj_id_map in
  Printf.sprintf
    "UPDATE %s
     SET data = %s
     WHERE id = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.dvalmap_jsonb merged)
    (Dbp.uuid id)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> run_sql
and upsert_dependent_object exec_state cols ~key:relation ~data:obj : dval =
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
  let db_obj = find_db exec_state.dbs table_name in
  match obj with
  | DObj m ->
    (match DvalMap.find m "id" with
     | Some existing -> update exec_state db_obj m; existing
     | None -> insert exec_state db_obj m |> DID)
  | DList l ->
    List.map ~f:(fun x -> upsert_dependent_object exec_state cols ~key:relation ~data:x) l |> DList
  | _ -> failwith ("Expected complex object (DObj), got: " ^ (Dval.to_repr obj))

let fetch_all exec_state (db: db) : dval =
  Printf.sprintf
    "SELECT id, data
     FROM %s
     WHERE table_tlid = %s
     AND account_id = %s
     AND canvas_id = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.tlid db.tlid)
    (Dbp.uuid exec_state.account_id)
    (Dbp.uuid exec_state.canvas_id)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> fetch_via_sql
  |> List.map ~f:(to_obj exec_state db)
  |> DList

let delete exec_state (db: db) (vals: dval_map) =
  let id =
    match DvalMap.find_exn vals "id" with
    | DID uuid -> uuid
    | _ -> Exception.client "error, id should be a uuid"
  in
  (* covered by composite PK index *)
  Printf.sprintf
    "DELETE
     FROM %s
     WHERE id = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.uuid id)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> run_sql

let delete_all exec_state (db: db) =
  (* covered by idx_user_data_current_data_for_tlid *)
  Printf.sprintf
    "DELETE
     FROM %s
     WHERE table_tlid = %s
     AND account_id = %s
     AND canvas_id = %s
     AND user_version = %s
     AND dark_version = %s"
    (Dbp.table user_data_table)
    (Dbp.tlid db.tlid)
    (Dbp.uuid exec_state.account_id)
    (Dbp.uuid exec_state.canvas_id)
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
    let existing = List.map ~f:(fun db -> db.tlid) dbs in
    let non_empties =
      Printf.sprintf
        "SELECT DISTINCT table_tlid
         FROM %s
         WHERE user_version = %s
         AND dark_version = %s
         AND table_tlid IN (%s)"
        (Dbp.table user_data_table)
        (Dbp.int db.version)
        (Dbp.int current_dark_version)
        (existing |> List.map ~f:Dbp.tlid |> String.concat ~sep:", ")
      |> fetch_via_sql
      |> List.concat
    in
    dbs
    |> List.filter
      ~f:(fun db ->
          not (List.mem ~equal:(=) non_empties (db.tlid |> string_of_int)))

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

