open Core
open Lib
open Runtime

module PG = Postgresql

let conn =
  new PG.connection ~host:"localhost" ~dbname:"proddb" ~user:"dark" ~password:"eapnsdc" ()

let create_table (name:string) (tipes : (string*string) list) : unit =
  let schema =
    tipes
    |> List.map ~f:(fun (name,tipe) -> name ^ " " ^ tipe)
    |> String.concat ~sep:", " in
  let cmd = "CREATE TABLE IF NOT EXISTS \"" ^ name ^ "\" (" ^ schema ^ ")" in
  ignore (conn#exec ~expect:[PG.Command_ok] cmd)

let kv_create name = create_table name [ "key", "VARCHAR PRIMARY KEY"
                                       ; "val", "VARCHAR"]

let kv_upsert (table: string) (key: string) (value: dval) : unit =
  let valstr = dval_to_json_string value in
  let cmd = "INSERT INTO \"" ^ table ^ "\" VALUES ('" ^ key ^ "', '" ^ valstr
            ^ "') ON CONFLICT (key) DO UPDATE SET val = '" ^ valstr ^ "'" in
  ignore (conn#exec ~expect:[PG.Command_ok] cmd)

let kv_fetch (table: string) (key: string) : dval =
  let cmd = "SELECT (val) FROM \"" ^ table ^ "\" WHERE key = '" ^ key ^ "' LIMIT 1" in
  let res = conn#exec cmd in
  if res#ntuples = 0 then DNull
  else if res#ntuples = 1 then (res#get_tuple 0).(0) |> parse
  else Exception.internal "more tuples than expected"

let kv_fetch_all (table: string) : dval =
  "SELECT * FROM \"" ^ table ^ "\""
  |> conn#exec
  |> (fun res -> res#get_all_lst)
  |> List.map ~f:(fun row ->
      match row with
      | [key; value] -> (key, value |> parse)
      | l -> Exception.internal ("Expected key,value list, got: " ^
                                 (String.concat ~sep:", " l)))
  |> to_dobj

let kv_keys (table: string) : dval =
  "SELECT (key) FROM \"" ^ table ^ "\""
  |> conn#exec
  |> (fun res -> res#get_all_lst)
  |> List.map ~f:(fun row ->
      match row with
      | [key] -> DStr key
      | l -> Exception.internal ("Expected key list, got: " ^
                                 (String.concat ~sep:", " l)))
  |> (fun l -> DList l)

let kv_delete (table: string) (key: string) : dval =
  "DELETE FROM \"" ^ table ^ "\"" ^ " WHERE key = '" ^ key ^ "'"
  |> conn#exec
  (* this returns an empty list, though postgres docs say should return the deletion count... *)
  |> fun x -> DNull




let with_postgres (table: opaque) fn =
  try
     let t = table#get in
     let _ = kv_create t in
     fn t
   with
   | PG.Error e ->
     Exception.internal ("DB error with: " ^ (PG.string_of_error e))


let fns : Lib.shortfn list = [
  { n = "DB::insert"
  ; o = []
  ; p = [par "table" TOpaque; par "key" TStr; par "val" TAny]
  ; r = TAny
  ; d = "Insert `val` into `table` indexed by `key`"
  ; f = InProcess
        (function
          | [DOpaque o; DStr key; value] ->
            with_postgres o (fun table -> kv_upsert table key value);
            DNull
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "DB::delete"
  ; o = []
  ; p = [par "table" TOpaque; par "key" TStr]
  ; r = TAny
  ; d = "Delete the value in `table` keyed by `key`"
  ; f = InProcess
        (function
          | [DOpaque o; DStr key] ->
            with_postgres o (fun table -> kv_delete table key)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "DB::fetch"
  ; o = []
  ; p = [par "table" TOpaque; par "key" TStr]
  ; r = TAny
  ; d = "Fetch the value in `table` keyed by `key`"
  ; f = InProcess
        (function
          | [DOpaque o; DStr key] ->
            with_postgres o (fun table -> kv_fetch table key)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "DB::keys"
  ; o = []
  ; p = [par "table" TOpaque]
  ; r = TAny
  ; d = "Fetch all the keys in `table`"
  ; f = InProcess
        (function
          | [DOpaque o] ->
            with_postgres o (fun table -> kv_keys table)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "DB::fetch_all"
  ; o = []
  ; p = [par "table" TOpaque]
  ; r = TList
  ; d = "Fetch all the values in `table`"
  ; f = InProcess
        (function
          | [DOpaque o] ->
            with_postgres o (fun table -> kv_fetch_all table)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


]

