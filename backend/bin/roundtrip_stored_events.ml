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


let validate_row (table : string) (values : string list) : string option =
  match values with
  | [canvas_name; value; trace_id] ->
      let params =
        [("table", table); ("canvas", canvas_name); ("trace_id", trace_id)]
      in
      ( try
          let _ : RTT.dval = Dval.of_internal_roundtrippable_v0 value in
          Log.infO "successful roundtrip" ~params ;
          None
        with _ ->
          let params = ("value", value) :: params in
          Log.erroR "Failed to roundtrip stored event" ~params ;
          Some trace_id )
  | _ ->
      Exception.internal "Impossible: wrong # of cols"


let () =
  ( match (Array.length Sys.argv, Array.to_list Sys.argv) with
  | 1, _ ->
      ()
  | 2, [_; "-h"] | _ ->
      usage () ) ;
  ignore
    ( Db.fetch
        ~name:"get stored_events"
        "SELECT canvases.name, value, trace_id
 FROM stored_events_v2
 JOIN canvases ON canvas_id = canvases.id"
        ~params:[]
    |> List.filter_map ~f:(validate_row "stored_events_v2") ) ;
  ignore
    ( Db.fetch
        ~name:"get function_results"
        "SELECT canvases.name, value, trace_id
 FROM function_results_v2
 JOIN canvases ON canvas_id = canvases.id"
        ~params:[]
    |> List.filter_map ~f:(validate_row "function_results_v2") ) ;
  ()
