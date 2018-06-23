open Core_kernel
open Libexecution

module PG = Postgresql
module Dbp = Dbprim

(* ------------------------- *)
(* Low-level API *)
(* ------------------------- *)

let conn = Dbconnection.conn
let escape = conn#escape_string

type param = Int of int
         | String of string
         | Uuid of Uuidm.t
         | Binary of string
         | Secret of string
         | DvalJson of Types.RuntimeT.dval
         | DvalmapJsonb of Types.RuntimeT.dval_map
         | Null

let to_binary_bool param : bool =
  match param with
  | Binary _ -> true
  | _ -> false

let to_sql param : string =
  match param with
  | Int i -> string_of_int i
  | String str -> str
  | Uuid uuid -> Uuidm.to_string uuid
  | Binary str -> str (* the to_binary_bool handled this *)
  | Secret str -> str
  | DvalJson dv -> Dval.dval_to_json_string dv
  | DvalmapJsonb dvm -> Dval.dvalmap_to_string dvm
  | Null -> Postgresql.null

let to_log param : string =
  let max_length = 600 in
  let abbrev s =
    if String.length s > max_length
    then (String.slice s 0 max_length) ^ "..."
    else s
  in
  match param with
  | Int i -> string_of_int i
  | String str -> abbrev str
  | Uuid uuid -> Uuidm.to_string uuid
  | Binary str -> "<binary>"
  | Secret str -> "<secret>"
  | DvalJson dv -> abbrev (Dval.dval_to_json_string dv)
  | DvalmapJsonb dvm -> abbrev (Dval.dvalmap_to_string dvm)
  | Null -> "NULL"

let execute2 ~name ~op ~params
    ~(f: params: string array ->
      binary_params : bool array ->
      string -> Postgresql.result)
    (sql : string) : Postgresql.result =
  let start = Unix.gettimeofday () in
  let time () =
    let finish = Unix.gettimeofday () in
    (finish -. start) *. 1000.0
  in
  let binary_params =
    params
    |> List.map ~f:to_binary_bool
    |> Array.of_list
  in
  let string_params =
    params
    |> List.map ~f:to_sql
    |> Array.of_list
  in
  let log_string =
    params
    |> List.map ~f:to_log
    |> String.concat ~sep:", "
  in
  try
    let result = f ~binary_params ~params:string_params sql in
    Log.succesS name ~params:["sql", op];
    result

  with e  ->
    let bt = Some (Caml.Printexc.get_raw_backtrace ()) in
    Log.erroR name ~params:[ "sql", op ; "params", log_string; "query", sql];

    let msg =
      match e with
      | Postgresql.Error (Unexpected_status (_, msg, _)) -> msg
      | Postgresql.Error pge -> Postgresql.string_of_error pge
      | pge -> (Exn.to_string pge)
    in
      Exception.storage
        msg
        ~bt
        ~info:[("time", time () |> string_of_float)]


let run ~(params: param list) ~(name:string) (sql: string) : unit =
  ignore
    (execute2 ~op:"run" ~params ~name sql
       ~f:(fun ~params ~binary_params sql ->
           conn#exec ~expect:[PG.Command_ok] ~params ~binary_params sql))

let fetch ~(params: param list) ~(name:string) (sql: string)
  : string list list =
  sql
  |> (execute2 ~op:"fetch" ~params ~name
      ~f:(fun ~params ~binary_params sql ->
          conn#exec ~expect:[PG.Tuples_ok] ~params ~binary_params sql))
  |> fun res -> res#get_all_lst

let fetch_one ~(params: param list) ~(name:string) (sql: string)
  : string list =
  sql
  |> execute2 ~op:"fetch_one" ~params ~name
      ~f:(fun ~params ~binary_params sql ->
          conn#exec ~expect:[PG.Tuples_ok] ~params ~binary_params sql)
  |> fun res ->
     match res#get_all_lst with
     | [single_result] -> single_result
     | [] -> Exception.storage "Expected one result, got none"
     | _ -> Exception.storage "Expected exactly one result, got many"

let fetch_one_option ~(params: param list) ~(name:string) (sql: string)
  : string list option =
  sql
  |> execute2 ~op:"fetch_one_option" ~params ~name
      ~f:(fun ~params ~binary_params sql ->
          conn#exec ~expect:[PG.Tuples_ok] ~params ~binary_params sql)
  |> fun res ->
     match res#get_all_lst with
     | [single_result] -> Some single_result
     | [] -> None
     | _ -> Exception.storage "Expected exactly one result, got many"


let exists ~(params: param list) ~(name:string) (sql: string)
  : bool =
  sql
  |> execute2 ~op:"exists" ~params ~name
      ~f:(fun ~params ~binary_params sql ->
          conn#exec ~expect:[PG.Tuples_ok] ~params ~binary_params sql)
  |> fun res ->
     match res#get_all_lst with
     | [["1"]] -> true
     | [] -> false
     | _ -> Exception.storage "Unexpected result"

(* ------------------------- *)
(* oplists *)
(* ------------------------- *)
let save_oplists ~(host: string) ~(digest: string) (data: string) : unit =
  run
    ~name:"save_oplists"
    "INSERT INTO oplists
    (host, digest, data)
    VALUES ($1, $2, $3)
    ON CONFLICT (host, digest) DO UPDATE
    SET data = $3;"
    ~params:[String host; String digest; Binary data]



let load_oplists ~(host: string) ~(digest: string) : string option =
  (* https://www.postgresql.org/docs/9.6/static/datatype-binary.html
   * Postgres advices us to parse the hex format. *)
  fetch_one_option
    ~name:"load_oplists"
    "SELECT data FROM oplists
     WHERE host = $1
     AND digest = $2;"
    ~params:[String host; String digest]
  |> Option.map ~f:List.hd_exn
  |> Option.map ~f:Dbp.binary_to_string (* slow but what alternative? *)

let load_json_oplists ~(host: string) : string option =
  fetch_one_option
    ~name:"load_json_oplists"
    "SELECT data FROM json_oplists
     WHERE host = $1"
    ~params:[String host]
  |> Option.map ~f:List.hd_exn

let save_json_oplists ~(host: string) ~(digest: string) (data: string) : unit =
    (* this is an upsert *)
  run
    ~name:"save_json_oplists"
    "INSERT INTO json_oplists
    (host, digest, data)
    VALUES ($1, $2, $3)
    ON CONFLICT (host) DO UPDATE
    SET data = $3;"
    ~params:[String host; String digest; String data]

(* TODO: this doesn't have json oplists *)
let all_oplists ~(digest: string) : string list =
  fetch
    ~name:"all_oplists"
    "SELECT host
    FROM oplists
    WHERE digest = $1"
    ~params:[String digest]
  |> List.concat
  |> List.filter ~f:(fun h ->
      not (String.is_prefix ~prefix:"test-" h))

let delete_benchmarking_data () : unit =
  run
    ~name:"delete_benchmarking_data"
    "DELETE FROM oplists WHERE host like 'benchmarking\\_%%';
     DELETE FROM json_oplists WHERE host like 'benchmarking\\_%%';"
    ~params:[]

