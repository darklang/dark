open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libbackend
open Libcommon

let usage () : unit =
  Format.printf
    "Usage: %s\n\n  Use DARK_CONFIG_DB_DBNAME=prodclone to check prodclone."
    Sys.argv.(0) ;
  exit 1


(* Given an op, transform it if necessary to remove deprecated types *)
(* This list of ops is from logging errors in apply_op and running
 * roundtrip_stored_event.exe. (Specifically, the load_all part - no need to
 * check stored_events, function_arguments, function_results, we just care about
 * canvases right now) *)
let transform_op (op : Op.op) ~(params : (string * string) list) : Op.op =
  let transform_string old =
    let new_str =
      (* If it's a tipe we can't parse, make it either id or [id] *)
      try
        ignore (Libexecution.Dval.tipe_of_string old) ;
        old
      with e ->
        if String.is_prefix old "[" && String.is_suffix old "]"
        then "[id]"
        else "id"
    in
    if old <> new_str
    then
      Log.infO
        "transform_string"
        ~params:(params @ [("old", old); ("new", new_str)]) ;
    new_str
  in
  let transform_string_or_blank (old : string Types.or_blank) =
    match old with
    | Partial _ | Blank _ ->
        old
    | Filled (id, str) ->
        Filled (id, transform_string str)
  in
  let new_op =
    match op with
    | SetDBColType (tlid, id, tipe) ->
        (* Dunno why I need to specify Op here ... *)
        Op.SetDBColType (tlid, id, transform_string tipe)
    | ChangeDBColType (tlid, id, tipe) ->
        ChangeDBColType (tlid, id, transform_string tipe)
    | CreateDBMigration (tlid, rbid, rfid, oldCols) ->
        let newCols =
          oldCols
          |> List.map ~f:(fun (name, tipe) ->
                 (* Note: a previous version transformed both col name and tipe; we
               * just want to transform the tipe *)
                 (name, transform_string_or_blank tipe) )
        in
        CreateDBMigration (tlid, rbid, rfid, newCols)
    | CreateDBWithBlankOr (tlid, pos, id, name) ->
        op
        (* not actually impl'ing this; this popped up as an error in the
          roundtrip script on ellen-cto because of the Duplicate DB Name bug *)
    | _ ->
        op
  in
  if new_op <> op
  then
    Log.infO
      "transform_op"
      ~params:
        ( params
        @ [ ("new", new_op |> Op.op_to_yojson |> Yojson.Safe.to_string)
          ; ("old", op |> Op.op_to_yojson |> Yojson.Safe.to_string) ] ) ;
  new_op


(* given a row, deserialize the oplist, transform the ops, and if there's a
 * change, save it (and log) *)
let migrate_oplist_row (row : string list) : unit =
  let host, canvas_id, tlid =
    match row with
    | [host; canvas_id; tlid] ->
        (host, canvas_id, tlid)
    | _ ->
        Exception.internal "Bad db result"
  in
  let data =
    Db.fetch_one
      ~name:"fetch oplist"
      ~result:BinaryResult
      "SELECT data FROM toplevel_oplists WHERE canvas_id = $1 AND tlid = $2"
      ~params:
        [ Db.Uuid (Option.value_exn (Uuidm.of_string canvas_id))
        ; Db.Int (tlid |> Int.of_string) ]
    |> List.hd_exn
  in
  let old_oplist =
    try data |> Op.oplist_of_string with e ->
      Log.erroR
        "failed deserialize"
        ~params:
          [ ("host", host)
          ; ("msg", Exception.to_string e)
          ; ("canvas_id", canvas_id)
          ; ("tlid", tlid)
          ; ("data", string_of_int (String.length data)) ] ;
      []
  in
  if old_oplist <> []
  then (
    Log.infO
      "successful deserialize"
      ~params:[("host", host); ("canvas_id", canvas_id); ("tlid", tlid)] ;
    let newData =
      old_oplist
      |> List.map
           ~f:
             (transform_op
                ~params:
                  [("host", host); ("canvas_id", canvas_id); ("tlid", tlid)])
      |> Op.oplist_to_string
      (* compare and upsert *)
    in
    if data <> newData
    then (
      Log.infO
        "migrate_oplist"
        ~params:[("host", host); ("canvas_id", canvas_id); ("tlid", tlid)] ;
      Db.run
        ~name:"update_toplevel_oplist"
        ~params:
          [ Db.Binary newData
          ; Db.Uuid (Option.value_exn (Uuidm.of_string canvas_id))
          ; Db.Int (tlid |> Int.of_string)
          ; Db.Binary data ]
        (* data = ? is in the WHERE clause to avoid race conditions - if we've
         * identified this as an oplist that needs updating, but someone has
         * edited it in the meantime, we should not overwrite that. This may
         * mean needing to run the script repeatedly, until there are no
         * migrate_oplist logs *)
        "UPDATE toplevel_oplists
       SET data = $1
       WHERE canvas_id = $2 AND tlid = $3 AND data = $4" ) ) ;
  ()


let () =
  ( match (Array.length Sys.argv, Array.to_list Sys.argv) with
  | 1, _ ->
      ()
  | 2, [_; "-h"] | _ ->
      usage () ) ;
  Db.iter_with_cursor
    ~name:"migrate oplist"
    ~params:[]
    "SELECT canvases.name, canvas_id, tlid
     FROM toplevel_oplists
     JOIN canvases ON canvases.id = canvas_id"
    ~f:migrate_oplist_row ;
  ()
