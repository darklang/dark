open Core

open Types
module RTT = Types.RuntimeT

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)
let file_ext  = ".results.json"

let root = Config.Function_results

let dir_name (host: string) (tlid: tlid) : string =
  host ^ "-" ^ (string_of_int tlid)

let mkdir (host: string) (tlid: tlid) : unit =
  Util.mkdir ~root (dir_name host tlid)

(* By hashing the filename, it's cheap to know if anything has changed,
 * without security implication of saving passwords to disk *)
let hash (arglist : RTT.dval list) : string =
  arglist
  |> List.map ~f:Dval.to_internal_repr
  |> String.concat
  |> Util.hash

let basename (fnname: string) (id: id) (arglist : RTT.dval list)
 : string =
  let fnname = Util.string_replace ":" "_" fnname in
  fnname ^ "-" ^ (string_of_int id) ^ "-" ^ (hash arglist) ^ file_ext

let filename (host, tlid, fnname, id) arglist =
  dir_name host tlid ^ "/" ^ basename fnname id arglist




(* ------------------------- *)
(* External *)
(* ------------------------- *)

let store_old (host, tlid, fnname, id) arglist result =
  mkdir host tlid;
  let filename = filename (host, tlid, fnname, id) arglist in
  Util.writejsonfile ~root ~conv:Dval.dval_to_yojson ~value:result filename

let load_old (host, tlid, fnname, id) arglist =
  let fn = filename (host, tlid, fnname, id) arglist in
  if Util.file_exists ~root fn
  then
    fn
    |> Util.readjsonfile ~root ~conv:Dval.dval_of_yojson
    (* only do enough to satisfy the interface for now *)
    |> fun x -> Some (x, Time.now ())
  else None

let store_new (canvas_id, tlid, fnname, id) arglist result =
  let sql =
    Printf.sprintf
      "INSERT INTO function_results
      (canvas_id, tlid, fnname, id, hash, timestamp, value)
      VALUES ('%s':uuid, %d, '%s', %d, '%s', CURRENT_TIMESTAMP, '%s')"
      (Db.escape canvas_id)
      tlid
      (Db.escape fnname)
      id
      (Db.escape (hash arglist))
      (Db.escape (Dval.dval_to_json_string result))
  in
  Db.run_sql sql

let load_new (canvas_id, tlid, fnname, id) arglist
  : (RTT.dval * Time.t) option =
  let sql =
    Printf.sprintf
      "SELECT timestamp, value
      FROM function_results
      WHERE ( canvas_id = '%s':uuid
            , tlid = %d
            , fnname = '%s'
            , id = %d
            , hash = '%s')"
      (Db.escape canvas_id)
      tlid
      (Db.escape fnname)
      id
      (Db.escape (hash arglist))
  in
  sql
  |> Db.fetch_via_sql
  |> List.hd
  |> Option.map ~f:(function
      | [dval; ts] ->
        (Dval.dval_of_json_string dval, Time.of_string ts)
      | _ -> Exception.internal "Bad DB format for stored_functions")


let store = store_old
let load = load_old
