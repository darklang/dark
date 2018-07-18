open Core_kernel
open Libexecution

open Types
open Types.RuntimeT
open Types.RuntimeT.DbT

module RT = Runtime

open Db

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
let type_error_msg col tipe dv : string =
  "Expected a value of type "
  ^ (Dval.tipe_to_string tipe)
  ^ " but got a "
  ^ (Dval.tipename dv)
  ^ " in column "
  ^ col

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
  if col = "id" (* the id is not stored in the jsonb *)
  then
    match dv with
    | DID id ->
      Printf.sprintf
        "AND id = %s"
        (Db.escape (Uuid id))
    | DStr str -> (* This is what you get for accidentally
                     implementating a dynamic language *)
      let uuid =
        match Uuidm.of_string str with
        | Some id -> Uuid id
        | None -> Exception.user "ID is not a UUID"
      in
      Printf.sprintf
        "AND id = %s"
        (Db.escape uuid)
    | _ -> Exception.client "Invalid id type"
  else
    Printf.sprintf
      "AND (data->%s)%s = %s" (* compare against json in a string *)
      (Db.escape (String col))
      (Db.cast_expression_for dv
       |> Option.value ~default:"")
      (Db.escape (DvalJson dv))

let rec fetch_by_many ~state db (pairs:(string*dval) list) : dval =
  let conds =
    pairs
    |> List.map ~f:(fun (k,v) -> query_for k v)
    |> String.concat ~sep:"\n     "
  in
  let sql =
    Printf.sprintf
      "SELECT id, data
       FROM user_data
       WHERE table_tlid = $1
       AND user_version = $2
       AND dark_version = $3
       %s"
      conds
  in
  Db.fetch
    ~name:"fetch_by"
    sql
    ~params:[ Int db.tlid
            ; Int db.version
            ; Int current_dark_version]
  |> List.map ~f:(to_obj ~state db)
  |> DList
and
fetch_by ~state db (col: string) (dv: dval) : dval =
  fetch_by_many ~state db [(col, dv)]
and
find ~state db id  =
  Db.fetch_one
    ~name:"find"
    "SELECT DISTINCT id, data
     FROM user_data
     WHERE id = $1
     AND user_version = $2
     AND dark_version = $3"
    ~params:[ Uuid id
            ; Int db.version
            ; Int current_dark_version]
  |> to_obj ~state db
and
find_many ~state db ids =
  let sql =
    Printf.sprintf
      "SELECT DISTINCT id, data
       FROM user_data
       WHERE id IN (%s)
       AND user_version = $1
       AND dark_version = $2"
      (Db.escape (List (List.map ~f:(fun u -> Uuid u) ids)))
  in
  if List.is_empty ids
  then DList []
  else
    Db.fetch
      ~name:"find_many"
      sql
      ~params:[ Int db.version
              ; Int current_dark_version]
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
    (* <HACK>: because it's hard to migrate at the moment, we need to
     * have default values when someone adds a col. We can remove this
     * when the migrations work properly. Structured like this so that
     * hopefully we only have to remove this small part. *)
    let default_keys =
        cols_for db
        |> List.map ~f:(fun (k, _) -> (k, DNull))
        |> DvalMap.of_alist_exn
    in
    let merged = Util.merge_left merged default_keys in
    (* </HACK> *)

    let type_checked =
      type_check_and_fetch_dependents ~state db merged
    in
    DObj type_checked
  | _ -> Exception.internal "Got bad format from db fetch"
and type_check_and_map_dependents ~belongs_to ~has_many ~state (db: db) (obj: dval_map) : dval_map =
  let cols = cols_for db |> TipeMap.of_alist_exn in
  let tipe_keys = cols
                  |> TipeMap.keys
                  |> List.filter
                    ~f:(fun k -> not (String.Caseless.equal k "id"))
                  |> String.Set.of_list
  in
  let obj_keys = obj
                 |> DvalMap.keys
                 |> List.filter
                   ~f:(fun k -> not (String.Caseless.equal k "id"))
                 |> String.Set.of_list
  in
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
            Exception.client (type_error_msg key expected_type value_of_actual_type)
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
           (* TODO: temporary, need to add this to coerce not found dependents to null. We should
            * probably propagate the deletion to the owning records, but this is very much a symptom
            * of modelling relationships parent->child rather than child->parent. child->parent
            * seems hard with our single-table, json blob approach though *)
           (try
              find ~state dep_table id
            with
            | Exception.DarkException e as original->
              (match e.tipe with
               | DarkStorage ->
                 DNull
               | _ ->
                 raise original)
            | other -> raise other)
         | DNull -> (* allow nulls for now *)
           DNull
         | err -> Exception.client (type_error_msg table TID err)))
    ~has_many:(fun table ids ->
        let dep_table = find_db state.dbs table in
        let uuids =
          List.map
            ~f:(fun id ->
                (match id with
                  | DID i -> i
                  | err -> Exception.client (type_error_msg table TID err)))
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
       | DNull -> (* allow nulls for now *)
         DNull
       | err -> Exception.client (type_error_msg table TObj err)))
   ~has_many:(fun table dlist ->
        let dep_table = find_db state.dbs table in
        dlist
        |> List.map
          ~f:(fun o ->
              (match o with
               | DObj m ->
                 type_check_and_upsert_dependents ~state dep_table m
                 |> fun o -> DvalMap.find o "id"
                 |> (function
                      | Some i -> i
                      | None -> Exception.internal "upsert returned id-less object")
               | err -> Exception.client (type_error_msg table TObj err)))
        |> DList)
    ~state db obj
and insert ~state (db: db) (vals: dval_map) : Uuidm.t =
  (* TODO: what if it has an id already *)
  let id = Util.create_uuid () in
  let merged = type_check_and_upsert_dependents ~state db vals in
  Db.run
    ~name:"user_insert"
    "INSERT into user_data
     (id, account_id, canvas_id, table_tlid, user_version, dark_version, data)
     VALUES ($1, $2, $3, $4, $5, $6, $7::jsonb)"
    ~params:[ Uuid id
            ; Uuid state.account_id
            ; Uuid state.canvas_id
            ; Int db.tlid
            ; Int db.version
            ; Int current_dark_version
            ; DvalmapJsonb merged];
  id
and update ~state db (vals: dval_map) =
  let id =
    match DvalMap.find_exn vals "id" with
    | DID uuid -> uuid
    | _ -> Exception.client "error, id should be a uuid"
  in
  let merged = type_check_and_upsert_dependents ~state db vals in
  Db.run
    ~name:"user_update"
    "UPDATE user_data
     SET data = $1
     WHERE id = $2
     AND account_id = $3
     AND canvas_id = $4
     AND table_tlid = $5
     AND user_version = $6
     AND dark_version = $7"
    ~params:[ DvalmapJsonb merged
            ; Uuid id
            ; Uuid state.account_id
            ; Uuid state.canvas_id
            ; Int db.tlid
            ; Int db.version
            ; Int current_dark_version]

let fetch_all ~state (db: db) : dval =
  Db.fetch
    ~name:"fetch_all"
    "SELECT id, data
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5"
    ~params:[ Int db.tlid
            ; Uuid state.account_id
            ; Uuid state.canvas_id
            ; Int db.version
            ; Int current_dark_version]
  |> List.map ~f:(to_obj ~state db)
  |> DList

let delete ~state (db: db) (vals: dval_map) =
  let id =
    match DvalMap.find_exn vals "id" with
    | DID uuid -> uuid
    | _ -> Exception.client "error, id should be a uuid"
  in
  (* covered by composite PK index *)
  Db.run
    ~name:"user_delete"
    "DELETE FROM user_data
     WHERE id = $1
     AND account_id = $2
     AND canvas_id = $3
     AND table_tlid = $4
     AND user_version = $5
     AND dark_version = $6"
    ~params:[ Uuid id
            ; Uuid state.account_id
            ; Uuid state.canvas_id
            ; Int db.tlid
            ; Int db.version
            ; Int current_dark_version]


let delete_all ~state (db: db) =
  (* covered by idx_user_data_current_data_for_tlid *)
  Db.run
    ~name:"user_delete_all"
    "DELETE FROM user_data
     WHERE account_id = $1
     AND canvas_id = $2
     AND table_tlid = $3
     AND user_version = $4
     AND dark_version = $5"
    ~params:[ Uuid state.account_id
            ; Uuid state.canvas_id
            ; Int db.tlid
            ; Int db.version
            ; Int current_dark_version]

let count (db: db) =
  (* covered by idx_user_data_current_data_for_tlid *)
  Db.fetch_one
    ~name:"count"
    "SELECT COUNT(*) AS c
     FROM user_data
     WHERE table_tlid = $1
     AND user_version = $2
     AND dark_version = $3"
    ~params:[ Int db.tlid
            ; Int db.version
            ; Int current_dark_version]
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
    (* this will need to be fixed when we allow migrations *)
    let locked =
      Db.fetch
        ~name:"unlocked"
        "SELECT DISTINCT table_tlid
         FROM user_data
         WHERE user_version = $1
         AND dark_version = $2
         AND canvas_id = $3
         AND account_id = $4"
        ~params:[ Int db.version
                ; Int current_dark_version
                ; Uuid canvas_id
                ; Uuid account_id]
      |> List.concat
      |> List.map ~f:int_of_string
    in
    List.filter dbs
      ~f:(fun db ->
          not (List.mem ~equal:(=) locked db.tlid))

(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let create (host:host) (name:string) (id: tlid) : db =
  { tlid = id
  ; host = host
  ; display_name = name
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

