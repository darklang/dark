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

type 'expr_type symtable = 'expr_type dval_map [@@deriving show]

let input_vars2symtable vars = Symtable.from_list vars

(* -------------------- *)
(* Dval store - save per-tl analysis results *)
(* -------------------- *)
let ht_to_json_dict ds ~f =
  let alist = Hashtbl.to_alist ds in
  `Assoc (List.map ~f:(fun (id, v) -> (string_of_id id, f v)) alist)


type 'expr_type intermediate_result_store =
  'expr_type execution_result IDTable.t

let intermediate_result_store_to_yojson
    (f : 'expr_type -> Yojson.Safe.t)
    (ds : 'expr_type intermediate_result_store) : Yojson.Safe.t =
  ht_to_json_dict ds ~f:(execution_result_to_yojson f)


(* -------------------- *)
(* Analysis result *)
(* -------------------- *)
type 'expr_type analysis = 'expr_type intermediate_result_store
[@@deriving to_yojson]

type 'expr_type input_vars = (string * 'expr_type dval) list
[@@deriving eq, show, yojson]

type function_arg_hash = string [@@deriving eq, show, yojson]

type hash_version = int [@@deriving eq, show, yojson]

type fnname = string [@@deriving yojson]

type 'expr_type function_result =
  fnname * id * function_arg_hash * hash_version * 'expr_type dval
[@@deriving eq, show, yojson]

type traceid = uuid [@@deriving show, yojson]

type 'expr_type trace_data =
  { input : 'expr_type input_vars
  ; timestamp : time
  ; function_results : 'expr_type function_result list }
[@@deriving eq, show, yojson]

type 'expr_type trace = traceid * 'expr_type trace_data option
[@@deriving yojson]

type 'expr_type tlid_traces = tlid * 'expr_type trace list
[@@deriving to_yojson]

type tlid_traceid = tlid * traceid [@@deriving to_yojson]

type traceid_tlids = traceid * tlid list [@@deriving to_yojson]
