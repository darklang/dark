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

let find_db (tables : db list) (table_name : string) : db option =
  List.find tables ~f:(fun (d : db) ->
      Ast.blank_to_string d.name = String.capitalize table_name )


(* ------------------------- *)
(* actual DB stuff *)
(* ------------------------- *)
let type_error_msg col tipe dv : string =
  "Expected a value of type "
  ^ Dval.tipe_to_string tipe
  ^ " but got a "
  ^ Dval.tipename dv
  ^ " in column "
  ^ col


(* Turn db rows into list of string/type pairs - removes elements with
 * holes, as they won't have been put in the DB yet *)
let cols_for (db : db) : (string * tipe) list =
  db.cols
  |> List.filter_map ~f:(fun c ->
         match c with
         | Filled (_, name), Filled (_, tipe) ->
             Some (name, tipe)
         | _ ->
             None )


let rec query ~state db query_obj : dval =
  let sql =
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = $1
     AND user_version = $2
     AND dark_version = $3
     AND canvas_id = $4
     AND data @> $5"
  in
  Db.fetch
    ~name:"fetch_by"
    sql
    ~params:
      [ ID db.tlid
      ; Int db.version
      ; Int current_dark_version
      ; Uuid state.canvas_id
      ; QueryableDval query_obj ]
  |> List.map ~f:(fun return_val ->
         match return_val with
         (* TODO(ian): change `to_obj` to just take a string *)
         | [key; data] ->
             DList [Dval.dstr_of_string_exn key; to_obj db [data]]
         | _ ->
             Exception.internal "bad format received in fetch_all" )
  |> DList


and query_by_one ~state db (col : string) (dv : dval) : dval =
  query ~state db (DObj (DvalMap.singleton col dv))


and (* PG returns lists of strings. This converts them to types using the
 * row info provided *)
    to_obj db (db_strings : string list) : dval =
  match db_strings with
  | [obj] ->
      let p_obj =
        match Dval.of_internal_queryable_v0 obj with
        | DObj o ->
            (* <HACK>: some legacy objects were allowed to be saved with `id` keys _in_ the
         * data object itself. they got in the database on the `update` of
         * an already present object as `update` did not remove the magic `id` field
         * which had been injected on fetch. we need to remove magic `id` if we fetch them
         * otherwise they will not type check on the way out any more and will not work.
         * if they are re-saved with `update` they will have their ids removed.
         * we consider an `id` key on the map to be a "magic" one if it is present in the map
         * but not in the schema of the object. this is a deliberate weakening of our schema
         * checker to deal with this case. *)
            if not
                 (List.exists
                    ~f:(( = ) "id")
                    (db |> cols_for |> List.map ~f:Tuple.T2.get1))
            then Map.remove o "id"
            else o
        (* </HACK> *)
        | x ->
            Exception.internal ("failed format, expected DObj got: " ^ obj)
      in
      (* <HACK>: because it's hard to migrate at the moment, we need to
     * have default values when someone adds a col. We can remove this
     * when the migrations work properly. Structured like this so that
     * hopefully we only have to remove this small part. *)
      let default_keys =
        cols_for db
        |> List.map ~f:(fun (k, _) -> (k, DNull))
        |> DvalMap.from_list_exn
      in
      let merged = Util.merge_left p_obj default_keys in
      (* </HACK> *)
      let type_checked = type_check db merged in
      DObj type_checked
  | _ ->
      Exception.internal "Got bad format from db fetch"


(* TODO: Unify with Type_checker.ml *)
and type_check db (obj : dval_map) : dval_map =
  let cols = cols_for db |> TipeMap.of_alist_exn in
  let tipe_keys = cols |> TipeMap.keys |> String.Set.of_list in
  let obj_keys = obj |> DvalMap.keys |> String.Set.of_list in
  let same_keys = String.Set.equal tipe_keys obj_keys in
  if same_keys
  then
    DvalMap.mapi
      ~f:(fun ~key ~value ->
        match (TipeMap.find_exn cols key, value) with
        | TInt, DInt _ ->
            value
        | TFloat, DFloat _ ->
            value
        | TStr, DStr _ ->
            value
        | TBool, DBool _ ->
            value
        | TDate, DDate _ ->
            value
        | TList, DList _ ->
            value
        | TDbList _, DList _ ->
            value
        | TPassword, DPassword _ ->
            value
        | TUuid, DUuid _ ->
            value
        | _, DNull ->
            value (* allow nulls for now *)
        | expected_type, value_of_actual_type ->
            Exception.code
              (type_error_msg key expected_type value_of_actual_type) )
      obj
  else
    let missing_keys = String.Set.diff tipe_keys obj_keys in
    let missing_msg =
      "Expected but did not find: ["
      ^ (missing_keys |> String.Set.to_list |> String.concat ~sep:", ")
      ^ "]"
    in
    let extra_keys = String.Set.diff obj_keys tipe_keys in
    let extra_msg =
      "Found but did not expect: ["
      ^ (extra_keys |> String.Set.to_list |> String.concat ~sep:", ")
      ^ "]"
    in
    match
      (String.Set.is_empty missing_keys, String.Set.is_empty extra_keys)
    with
    | false, false ->
        Exception.code (missing_msg ^ " & " ^ extra_msg)
    | false, true ->
        Exception.code missing_msg
    | true, false ->
        Exception.code extra_msg
    | true, true ->
        Exception.internal
          "Type checker error! Deduced expected and actual did not unify, but could not find any examples!"


and set ~state ~upsert (db : db) (key : string) (vals : dval_map) : Uuidm.t =
  let id = Util.create_uuid () in
  let merged = type_check db vals in
  let query =
    "INSERT INTO user_data
     (id, account_id, canvas_id, table_tlid, user_version, dark_version, key, data)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8::jsonb)"
    |> fun s ->
    if upsert
    then
      s
      ^ " ON CONFLICT ON CONSTRAINT user_data_key_uniq DO UPDATE SET data = EXCLUDED.data"
    else s
  in
  Db.run
    ~name:"user_set"
    query
    ~params:
      [ Uuid id
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; ID db.tlid
      ; Int db.version
      ; Int current_dark_version
      ; String key
      ; QueryableDvalmap merged ] ;
  id


and get ~state (db : db) (key : string) : dval =
  Db.fetch_one
    ~name:"get"
    "SELECT data
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5
     AND key = $6"
    ~params:
      [ ID db.tlid
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; Int db.version
      ; Int current_dark_version
      ; String key ]
  |> to_obj db


and get_many ~state (db : db) (keys : string list) : dval =
  Db.fetch
    ~name:"get_many"
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5
     AND key = ANY (string_to_array($6, $7)::text[])"
    ~params:
      [ ID db.tlid
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; Int db.version
      ; Int current_dark_version
      ; List (List.map ~f:(fun s -> String s) keys)
      ; String Db.array_separator ]
  |> List.map ~f:(fun return_val ->
         match return_val with
         (* TODO(ian): change `to_obj` to just take a string *)
         | [key; data] ->
             DList [Dval.dstr_of_string_exn key; to_obj db [data]]
         | _ ->
             Exception.internal "bad format received in get_many" )
  |> DList


and get_many_v2 ~state (db : db) (keys : string list) : dval =
  Db.fetch
    ~name:"get_many_v2"
    "SELECT key, data
    FROM user_data
    WHERE table_tlid = $1
    AND account_id = $2
    AND canvas_id = $3
    AND user_version = $4
    AND dark_version = $5
    AND key = ANY (string_to_array($6, $7)::text[])"
    ~params:
      [ ID db.tlid
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; Int db.version
      ; Int current_dark_version
      ; List (List.map ~f:(fun s -> String s) keys)
      ; String Db.array_separator ]
  |> List.map ~f:(fun return_val ->
         match return_val with
         (* TODO(ian): change `to_obj` to just take a string *)
         | [key; data] ->
             to_obj db [data]
         | _ ->
             Exception.internal "bad format received in get_many_v2" )
  |> DList


and get_many_with_keys ~state (db : db) (keys : string list) : dval =
  Db.fetch
    ~name:"get_many_with_keys"
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5
     AND key = ANY (string_to_array($6, $7)::text[])"
    ~params:
      [ ID db.tlid
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; Int db.version
      ; Int current_dark_version
      ; List (List.map ~f:(fun s -> String s) keys)
      ; String Db.array_separator ]
  |> List.map ~f:(fun return_val ->
         match return_val with
         (* TODO(ian): change `to_obj` to just take a string *)
         | [key; data] ->
             DList [Dval.dstr_of_string_exn key; to_obj db [data]]
         | _ ->
             Exception.internal "bad format received in get_many_with_keys" )
  |> DList


let get_all ~state (db : db) : dval =
  Db.fetch
    ~name:"get_all"
    "SELECT key, data
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5"
    ~params:
      [ ID db.tlid
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; Int db.version
      ; Int current_dark_version ]
  |> List.map ~f:(fun return_val ->
         match return_val with
         (* TODO(ian): change `to_obj` to just take a string *)
         | [key; data] ->
             DList [Dval.dstr_of_string_exn key; to_obj db [data]]
         | _ ->
             Exception.internal "bad format received in get_all" )
  |> DList


let count ~state (db : db) : int =
  Db.fetch
    ~name:"count"
    "SELECT count(*)
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5"
    ~params:
      [ ID db.tlid
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; Int db.version
      ; Int current_dark_version ]
  |> List.hd_exn
  |> List.hd_exn
  |> int_of_string


let delete ~state (db : db) (key : string) =
  (* covered by composite PK index *)
  Db.run
    ~name:"user_delete"
    "DELETE FROM user_data
     WHERE key = $1
     AND account_id = $2
     AND canvas_id = $3
     AND table_tlid = $4
     AND user_version = $5
     AND dark_version = $6"
    ~params:
      [ String key
      ; Uuid state.account_id
      ; Uuid state.canvas_id
      ; ID db.tlid
      ; Int db.version
      ; Int current_dark_version ]


let delete_all ~state (db : db) =
  (* covered by idx_user_data_current_data_for_tlid *)
  Db.run
    ~name:"user_delete_all"
    "DELETE FROM user_data
     WHERE account_id = $1
     AND canvas_id = $2
     AND table_tlid = $3
     AND user_version = $4
     AND dark_version = $5"
    ~params:
      [ Uuid state.account_id
      ; Uuid state.canvas_id
      ; ID db.tlid
      ; Int db.version
      ; Int current_dark_version ]


(* ------------------------- *)
(* stats/locked/unlocked (not _locking_) *)
(* ------------------------- *)
let stats_pluck ~account_id ~canvas_id (db : db) : dval option =
  Db.fetch
    ~name:"stats_pluck"
    "SELECT data
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5
     ORDER BY created_at DESC
     LIMIT 1"
    ~params:
      [ ID db.tlid
      ; Uuid account_id
      ; Uuid canvas_id
      ; Int db.version
      ; Int current_dark_version ]
  |> List.hd
  |> Option.map ~f:(to_obj db)


let stats_count ~account_id ~canvas_id (db : db) : int =
  Db.fetch
    ~name:"stats_count"
    "SELECT count(*)
     FROM user_data
     WHERE table_tlid = $1
     AND account_id = $2
     AND canvas_id = $3
     AND user_version = $4
     AND dark_version = $5"
    ~params:
      [ ID db.tlid
      ; Uuid account_id
      ; Uuid canvas_id
      ; Int db.version
      ; Int current_dark_version ]
  |> List.hd_exn
  |> List.hd_exn
  |> int_of_string


let unlocked canvas_id account_id (dbs : db list) : db list =
  match dbs with
  | [] ->
      []
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
          ~params:
            [ Int db.version
            ; Int current_dark_version
            ; Uuid canvas_id
            ; Uuid account_id ]
        |> List.concat
        |> List.map ~f:id_of_string
      in
      List.filter dbs ~f:(fun db -> not (List.mem ~equal:( = ) locked db.tlid))


(* ------------------------- *)
(* DB schema *)
(* ------------------------- *)

let create (name : string) (id : tlid) : db =
  { tlid = id
  ; name = Filled (id, name)
  ; cols = []
  ; version = 0
  ; old_migrations = []
  ; active_migration = None }


let create2 (name : string) (tlid : tlid) (name_id : id) : db =
  { tlid
  ; name = Filled (name_id, name)
  ; cols = []
  ; version = 0
  ; old_migrations = []
  ; active_migration = None }


let rename_db (n : string) (db : db) : db =
  let id =
    match db.name with Partial (i, _) | Blank i -> i | Filled (i, _) -> i
  in
  {db with name = Filled (id, n)}


let add_col colid typeid (db : db) =
  {db with cols = db.cols @ [(Blank colid, Blank typeid)]}


let set_col_name id name db =
  let set col =
    match col with
    | Blank hid, tipe when hid = id ->
        (Filled (hid, name), tipe)
    | _ ->
        col
  in
  let newcols = List.map ~f:set db.cols in
  {db with cols = newcols}


let change_col_name id name db =
  let change col =
    match col with
    | Filled (hid, oldname), Filled (tipeid, tipename) when hid = id ->
        (Filled (hid, name), Filled (tipeid, tipename))
    | _ ->
        col
  in
  {db with cols = List.map ~f:change db.cols}


let set_col_type id tipe db =
  let set col =
    match col with
    | name, Blank hid when hid = id ->
        (name, Filled (hid, tipe))
    | _ ->
        col
  in
  let newcols = List.map ~f:set db.cols in
  {db with cols = newcols}


let change_col_type id newtipe db =
  let change col =
    match col with
    | Filled (nameid, name), Filled (tipeid, oldtipe) when tipeid = id ->
        (Filled (nameid, name), Filled (tipeid, newtipe))
    | _ ->
        col
  in
  {db with cols = List.map ~f:change db.cols}


let delete_col id db =
  let newcols =
    List.filter db.cols ~f:(fun col ->
        match col with
        | Blank nid, _ when nid = id ->
            false
        | Filled (nid, _), _ when nid = id ->
            false
        | _ ->
            true )
  in
  {db with cols = newcols}


let create_migration rbid rfid cols db =
  match db.active_migration with
  | Some migration ->
      db
  | None ->
      let max_version =
        db.old_migrations
        |> List.map ~f:(fun m -> m.version)
        |> List.fold_left ~init:0 ~f:max
      in
      { db with
        active_migration =
          Some
            { starting_version = db.version
            ; version = max_version + 1
            ; cols
            ; state = DBMigrationInitialized
            ; rollback = Blank rbid
            ; rollforward = Blank rfid } }


let add_col_to_migration nameid typeid db =
  match db.active_migration with
  | None ->
      db
  | Some migration ->
      let mutated_migration =
        {migration with cols = migration.cols @ [(Blank nameid, Blank typeid)]}
      in
      {db with active_migration = Some mutated_migration}


let set_col_name_in_migration id name db =
  match db.active_migration with
  | None ->
      db
  | Some migration ->
      let set col =
        match col with
        | Blank hid, tipe when hid = id ->
            (Filled (hid, name), tipe)
        | Filled (nameid, oldname), tipe when nameid = id ->
            (Filled (nameid, name), tipe)
        | _ ->
            col
      in
      let newcols = List.map ~f:set migration.cols in
      let mutated_migration = {migration with cols = newcols} in
      {db with active_migration = Some mutated_migration}


let set_col_type_in_migration id tipe db =
  match db.active_migration with
  | None ->
      db
  | Some migration ->
      let set col =
        match col with
        | name, Blank blankid when blankid = id ->
            (name, Filled (blankid, tipe))
        | name, Filled (tipeid, oldtipe) when tipeid = id ->
            (name, Filled (tipeid, tipe))
        | _ ->
            col
      in
      let newcols = List.map ~f:set migration.cols in
      let mutated_migration = {migration with cols = newcols} in
      {db with active_migration = Some mutated_migration}


let abandon_migration db =
  match db.active_migration with
  | None ->
      db
  | Some migration ->
      let mutated_migration = {migration with state = DBMigrationAbandoned} in
      let db2 =
        {db with old_migrations = db.old_migrations @ [mutated_migration]}
      in
      {db2 with active_migration = None}


let delete_col_in_migration id db =
  match db.active_migration with
  | None ->
      db
  | Some migration ->
      let newcols =
        List.filter migration.cols ~f:(fun col ->
            match col with
            | Blank nid, _ when nid = id ->
                false
            | Filled (nid, _), _ when nid = id ->
                false
            | _ ->
                true )
      in
      let mutated_migration = {migration with cols = newcols} in
      {db with active_migration = Some mutated_migration}
