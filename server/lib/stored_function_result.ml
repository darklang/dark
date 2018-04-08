open Core

open Types
module RTT = Types.RuntimeT

(* ------------------------- *)
(* Internal *)
(* ------------------------- *)
let file_ext  = ".results.json"

let dir_name (host: string) (tlid: tlid) : string =
  Config.function_results_dir ^ host ^ "-" ^ (string_of_int tlid)

let mkdir (host: string) (tlid: tlid) : unit =
  Unix.mkdir_p (dir_name host tlid)

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

let store (host, tlid, fnname, id) arglist result =
  mkdir host tlid;
  let filename = filename (host, tlid, fnname, id) arglist in
  let s = Dval.dval_to_yojson result in
  Yojson.Safe.to_file filename s

let load (host, tlid, fnname, id) arglist =
  try
    filename (host, tlid, fnname, id) arglist
    |> Yojson.Safe.from_file
    |> Dval.dval_of_yojson
    |> Result.ok
  with e -> None

