open Core_kernel
open Libexecution

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
let type_error_msg tipe dv : string =
  "Expected a value of type "
  ^ (Dval.tipe_to_string tipe)
  ^ " but got a "
  ^ (Dval.tipename dv)

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
let query_for (col: string) (dv: dval) : string =
  Printf.sprintf
    "AND (data->%s)%s = %s" (* compare against json in a string *)
    (Dbp.string col)
    (Dbp.cast_expression_for dv
     |> Option.value ~default:"")
    (Dbp.dvaljson dv)
let rec fetch_by ~state db (col: string) (dv: dval) : dval =
  Printf.sprintf
    "SELECT id, data
     FROM %s
     WHERE table_tlid = %s
     AND user_version = %s
     AND dark_version = %s
     %s"
    (Dbp.table user_data_table)
    (Dbp.tlid db.tlid)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
    (query_for col dv)
  |> fetch_via_sql
  |> List.map ~f:(to_obj ~state db)
  |> DList
and
fetch_by_many ~state db (pairs:(string*dval) list) : dval =
  let conds =
    pairs
    |> List.map ~f:(fun (k,v) -> query_for k v)
    |> String.concat ~sep:"\n     "
  in
  Printf.sprintf
    "SELECT id, data
     FROM %s
     WHERE table_tlid = %s
     AND user_version = %s
     AND dark_version = %s
     %s"
    (Dbp.table user_data_table)
    (Dbp.tlid db.tlid)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
    conds
  |> fetch_via_sql
  |> List.map ~f:(to_obj ~state db)
  |> DList
and
find ~state db id  =
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
  |> to_obj ~state db
and
find_many ~state db ids =
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
  |> List.map ~f:(to_obj ~state db)
  |> DList
and
(* PG returns lists of strings. This converts them to types using the
 * row info provided *)
to_obj ~state db (db_strings : string list)
  : dval =
  match db_strings with
  | [id; obj] ->
    let p_id =
      id |> Uuidm.of_string |> Option.value_exn |> DID
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
      type_check_and_fetch_dependents ~state db merged
    in
    DObj type_checked
  | _ -> Exception.internal "Got bad format from db fetch"
and type_check_and_map_dependents ~belongs_to ~has_many ~state (db: db) (obj: dval_map) : dval_map =
  let cols = cols_for db |> TipeMap.of_alist_exn in
  let tipe_keys = TipeMap.keys cols |> List.filter ~f:(fun k -> not (String.Caseless.equal k "id")) |> String.Set.of_list in
  let obj_keys = DvalMap.keys obj |> List.filter ~f:(fun k -> not (String.Caseless.equal k "id")) |> String.Set.of_list in
  let same_keys = String.Set.equal tipe_keys obj_keys in
  if same_keys
  then
    DvalMap.mapi
      ~f:(fun ~key ~data ->
          match (TipeMap.find_exn cols key, data) with
          | (TID, DID _) -> data
          | (TInt, DInt _) -> data
          | (TFloat, DFloat _) -> data
          | (TTitle, DTitle _) -> data
          | (TUrl, DUrl _) -> data
          | (TStr, DStr _) -> data
          | (TBool, DBool _) -> data
          | (TDate, DDate _) -> data
          | (TList, DList _) -> data
          | (TDbList _, DList _) -> data
          | (TBelongsTo table, any_dval) ->
            (* the belongs_to function needs to type check any_dval *)
            belongs_to table any_dval
          | (THasMany table, DList any_list) ->
            (* the has_many function needs to type check any_list *)
            has_many table any_list
          | (_, DNull) -> data (* allow nulls for now *)
          | (expected_type, value_of_actual_type) ->
            Exception.client (type_error_msg expected_type value_of_actual_type)
        )
      obj
  else
    let missing_keys = String.Set.diff tipe_keys obj_keys in
    let missing_msg = "Expected but did not find: ["
                      ^ (missing_keys |> String.Set.to_list |> String.concat ~sep:", ")
                      ^ "]"
    in
    let extra_keys  = String.Set.diff obj_keys tipe_keys in
    let extra_msg = "Found but did not expect: ["
                      ^ (extra_keys |> String.Set.to_list |> String.concat ~sep:", ")
                      ^ "]"
    in
    match (String.Set.is_empty missing_keys, String.Set.is_empty extra_keys) with
    | (false, false) ->
      Exception.client (missing_msg ^ " & " ^ extra_msg)
    | (false, true) ->
      Exception.client missing_msg
    | (true, false) ->
      Exception.client extra_msg
    | (true, true) ->
      Exception.internal
        "Type checker error! Deduced expected and actual did not unify, but could not find any examples!"
and type_check_and_fetch_dependents ~state db obj : dval_map =
  type_check_and_map_dependents
    ~belongs_to:(fun table dv ->
        let dep_table = find_db state.dbs table in
        (match dv with
         | DID id ->
           find ~state dep_table id
         | err -> Exception.client (type_error_msg TID err)))
    ~has_many:(fun table ids ->
        let dep_table = find_db state.dbs table in
        let uuids =
          List.map
            ~f:(fun id ->
                (match id with
                  | DID i -> i
                  | err -> Exception.client (type_error_msg TID err)))
            ids
        in
        find_many ~state dep_table uuids)
    ~state db obj
and type_check_and_upsert_dependents ~state db obj : dval_map =
  type_check_and_map_dependents
    ~belongs_to:(fun table dv ->
      let dep_table = find_db state.dbs table in
      (match dv with
       | DObj m ->
         (match DvalMap.find m "id" with
          | Some existing -> update ~state dep_table m; existing
          | None -> insert ~state dep_table m |> DID)
       | err -> Exception.client (type_error_msg TObj err)))
   ~has_many:(fun table dlist ->
        let dep_table = find_db state.dbs table in
        dlist
        |> List.map
          ~f:(fun o ->
              (match o with
               | DObj m -> type_check_and_upsert_dependents ~state dep_table m |> DObj
               | err -> Exception.client (type_error_msg TObj err)))
        |> DList)
    ~state db obj
and insert ~state (db: db) (vals: dval_map) : Uuidm.t =
  let id = Util.create_uuid () in
  let merged = type_check_and_upsert_dependents ~state db vals in
  Printf.sprintf
    "INSERT into %s
     (id, account_id, canvas_id, table_tlid, user_version, dark_version, data)
     VALUES (%s, %s, %s, %s, %s, %s, %s)"
    (Dbp.table user_data_table)
    (Dbp.uuid id)
    (Dbp.uuid state.account_id)
    (Dbp.uuid state.canvas_id)
    (Dbp.int db.tlid)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
    (Dbp.dvalmap_jsonb merged)
  |> run_sql;
  id
and update ~state db (vals: dval_map) =
  let id =
    match DvalMap.find_exn vals "id" with
    | DID uuid -> uuid
    | _ -> Exception.client "error, id should be a uuid"
  in
  let merged = type_check_and_upsert_dependents ~state db vals in
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

let fetch_all ~state (db: db) : dval =
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
    (Dbp.uuid state.account_id)
    (Dbp.uuid state.canvas_id)
    (Dbp.int db.version)
    (Dbp.int current_dark_version)
  |> fetch_via_sql
  |> List.map ~f:(to_obj ~state db)
  |> DList

let delete ~state (db: db) (vals: dval_map) =
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

let delete_all ~state (db: db) =
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
    (Dbp.uuid state.account_id)
    (Dbp.uuid state.canvas_id)
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

let unlocked canvas_id account_id (dbs: db list) : db list =
  match dbs with
  | [] -> []
  | db :: _ ->
    let non_empties =
      Printf.sprintf
        "SELECT DISTINCT table_tlid
         FROM %s
         WHERE user_version = %s
         AND dark_version = %s
         AND canvas_id = %s
         AND account_id = %s"
        (Dbp.table user_data_table)
        (Dbp.int db.version)
        (Dbp.int current_dark_version)
        (Dbp.uuid canvas_id)
        (Dbp.uuid account_id)
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

