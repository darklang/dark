open Core_kernel
open Libcommon
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT
module RT = Runtime
module PReq = Parsed_request

(* -------------------- *)
(* Symtable / input vars *)
(* -------------------- *)

module Symtable = DvalMap

type symtable = dval_map

let input_vars2symtable vars = Symtable.from_list vars

(* -------------------- *)
(* Dval store - save per-tl analysis results *)
(* -------------------- *)
let ht_to_json_dict ds ~f =
  let alist = Hashtbl.to_alist ds in
  `Assoc (List.map ~f:(fun (id, v) -> (string_of_id id, f v)) alist)


type dval_store = dval IDTable.t

let dval_store_to_yojson (ds : dval_store) : Yojson.Safe.t =
  ht_to_json_dict ds ~f:dval_to_yojson


(* -------------------- *)
(* Analysis result *)
(* -------------------- *)
type analysis = {live_values : dval_store} [@@deriving to_yojson]

type input_vars = (string * dval) list [@@deriving eq, show, yojson]

type function_arg_hash = string [@@deriving eq, show, yojson]

type fnname = string [@@deriving yojson]

type function_result = fnname * id * function_arg_hash * dval
[@@deriving eq, show, yojson]

type traceid = uuid [@@deriving show, yojson]

type trace_data =
  { input : input_vars
  ; timestamp : time
  ; function_results : function_result list }
[@@deriving eq, show, yojson]

type trace = traceid * trace_data option [@@deriving yojson]

type tlid_traces = tlid * trace list [@@deriving to_yojson]

type tlid_traceid = tlid * traceid [@@deriving to_yojson]

type traceid_tlids = traceid * tlid list [@@deriving to_yojson]
