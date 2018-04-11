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

let store (host, tlid, fnname, id) arglist result =
  mkdir host tlid;
  let filename = filename (host, tlid, fnname, id) arglist in
  Util.writejsonfile ~root ~conv:Dval.dval_to_yojson ~value:result filename

let load (host, tlid, fnname, id) arglist =
  let fn = filename (host, tlid, fnname, id) arglist in
  if Util.file_exists ~root fn
  then
    fn
    |> Util.readjsonfile ~root ~conv:Dval.dval_of_yojson
    |> fun x -> Some x
  else None

