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
        "" ~time:t ~stop:10000
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



let run_sql ?(quiet=false) (sql: string) : unit =
  ignore
    (execute sql ~op:"run" ~quiet
       ~f:(conn#exec ~expect:[PG.Command_ok]))

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
  let data = Dbp.binary data in
  Printf.sprintf
    (* this is an upsert *)
    "INSERT INTO oplists
    (host, digest, data)
    VALUES (%s, %s, %s)
    ON CONFLICT (host, digest) DO UPDATE
    SET data = %s;"
    (Dbp.host host)
    (Dbp.string digest)
    data
    data
  |> run_sql ~quiet:true

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

