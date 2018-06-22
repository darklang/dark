open Core_kernel
open Libexecution

module PG = Postgresql
module Dbp = Dbprim

(* ------------------------- *)
(* Low-level API *)
(* ------------------------- *)

let conn = Dbconnection.conn
let escape = conn#escape_string

let execute ~op ~quiet ~f sql =
  let start = Unix.gettimeofday () in
  let time () =
    let finish = Unix.gettimeofday () in
    (finish -. start) *. 1000.0
  in
  let log msg =
    let t = time () in
    if quiet && t < 100.0
    then
      Log.infO
        ("sql (" ^ op ^ ", " ^ msg ^ "): " ^ sql)
        "" ~time:t ~stop:1000
  in

  try
    let result = f sql in
    log "success";
    result

  with e  ->
    let bt = Some (Caml.Printexc.get_raw_backtrace ()) in
    log "fail";

    let msg =
      match e with
      | Postgresql.Error (Unexpected_status (_, msg, _)) -> msg
      | Postgresql.Error pge -> Postgresql.string_of_error pge
      | pge -> (Exn.to_string pge)
    in
      Exception.storage
        msg
        ~bt
        ~info:[("sql", sql); ("time", time () |> string_of_float)]

type sql = Int of int
         | String of string
         | Uuid of Uuidm.t
         | Binary of string
         | Secret of string
         | DvalJson of Types.RuntimeT.dval
         | Null

let to_binary_bool sql : bool =
  match sql with
  | Binary _ -> true
  | _ -> false

let to_param sql : string =
  match sql with
  | Int i -> string_of_int i
  | String str -> str
  | Uuid uuid -> Uuidm.to_string uuid
  | Binary str -> str (* the to_binary_bool handled this *)
  | Secret str -> str
  | DvalJson dv -> Dval.dval_to_json_string dv
  | Null -> "null"

let to_log sql : string =
  let max_length = 600 in
  let abbrev s =
    if String.length s > max_length
    then (String.slice s 0 max_length) ^ "..."
    else s
  in
  match sql with
  | Int i -> string_of_int i
  | String str -> abbrev str
  | Uuid uuid -> Uuidm.to_string uuid
  | Binary str -> "<binary>"
  | Secret str -> "<secret>"
  | DvalJson dv -> abbrev (Dval.dval_to_json_string dv)
  | Null -> "null"

let execute2 ~name ~op ~params sql
    ~(f: params: string array ->
      binary_params : bool array ->
      string -> Postgresql.result) =
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
    |> List.map ~f:to_param
    |> Array.of_list
  in
  let log_string =
    params
    |> List.map ~f:to_log
    |> String.concat ~sep:", "
  in
  let log cond =
    let t = time () in
    Log.infO
      cond ("sql (" ^ op ^ ": " ^ name ^ "): [" ^ log_string ^ "]") ~time:t;
    Log.debuG sql (name ^ ": [" ^ log_string ^ "]") ~stop:1000;
  in
  try
    let result = f ~binary_params ~params:string_params (Log.pp "sql"
                                                           sql) in
    log "success";
    result

  with e  ->
    let bt = Some (Caml.Printexc.get_raw_backtrace ()) in
    log "fail";

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





let run_sql ?(quiet=false) (sql: string) : unit =
  ignore
    (execute sql ~op:"run" ~quiet
       ~f:(conn#exec ~expect:[PG.Command_ok]))


let run_sql2 ~(params: sql list) ~(name:string) (sql: string) : unit =
  ignore
    (execute2 ~op:"run" ~params ~name sql
       ~f:(fun ~params ~binary_params sql ->
           conn#exec ~expect:[PG.Command_ok] ~params ~binary_params sql))


let fetch_via_sql ?(quiet=false) (sql: string) : string list list =
  sql
  |> execute ~op:"fetch" ~quiet ~f:(conn#exec ~expect:PG.[Tuples_ok])
  |> fun res -> res#get_all_lst

let exists_via_sql ?(quiet=false) (sql: string) : bool =
  sql
  |> execute ~op:"exists" ~quiet ~f:(conn#exec ~expect:PG.[Tuples_ok])
  |> fun res -> res#get_all_lst
  |> (=) [["1"]]

(* ------------------------- *)
(* oplists *)
(* ------------------------- *)
let save_oplists ~(host: string) ~(digest: string) (data: string) : unit =
  run_sql2
    ~name:"save_oplists"
    "INSERT INTO oplists
    (host, digest, data)
    VALUES ($1, $2, $3)
    ON CONFLICT (host, digest) DO UPDATE
    SET data = $3;"
    ~params:[String host; String digest; Binary data]



let load_oplists ~(host: string) ~(digest: string) : string option =
  (* It's quite a bit of work to make a binary bytea go into the DB and
   * come out in the same shape:
   *
   * https://www.postgresql.org/docs/9.6/static/datatype-binary.html
   *
   * Postgres advices us to parse the hex format, above. We tried base64
   * as well, which is approx twice as slow. *)
  Printf.sprintf
    "SELECT encode(data, 'hex') FROM oplists
     WHERE host = %s
     AND digest = %s;"
    (Dbp.host host)
    (Dbp.string digest)
  |> fetch_via_sql ~quiet:true
  |> List.hd
  |> Option.value_map ~default:None ~f:List.hd
  |> Option.map ~f:Dbp.binary_to_string

let load_json_oplists ~(host: string) : string option =
  Printf.sprintf
    "SELECT data FROM json_oplists
     WHERE host = %s"
    (Dbp.host host)
  |> fetch_via_sql ~quiet:true
  |> List.hd
  |> Option.value_map ~default:None ~f:List.hd

let save_json_oplists ~(host: string) ~(digest: string) (data: string) : unit =
  Printf.sprintf
    (* this is an upsert *)
    "INSERT INTO json_oplists
    (host, digest, data)
    VALUES (%s, %s, %s)
    ON CONFLICT (host) DO UPDATE
    SET data = %s;"
    (Dbp.host host)
    (Dbp.string digest)
    (Dbp.string data)
    (Dbp.string data)
  |> run_sql ~quiet:true

(* TODO: this doesn't have json oplists *)
let all_oplists ~(digest: string) : string list =
  Printf.sprintf
    "SELECT host
    FROM oplists
    WHERE digest = '%s'"
    (Dbp.string digest)
  |> fetch_via_sql ~quiet:true
  |> List.concat
  |> List.filter ~f:(fun h ->
      not (String.is_prefix ~prefix:"test-" h))

let delete_benchmarking_data () : unit =
  "DELETE FROM oplists
  WHERE host like 'benchmarking\\_%%'"
  |> run_sql ~quiet:false;
  "DELETE FROM json_oplists
  WHERE host like 'benchmarking\\_%%'"
  |> run_sql ~quiet:false;
  ()

