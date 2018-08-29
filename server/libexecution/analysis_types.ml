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

let input_vars2symtable vars =
  Symtable.of_alist_exn vars


(* -------------------- *)
(* Live values *)
(* -------------------- *)
type livevalue = { value: string
                 ; tipe: string [@key "type"]
                 ; json: string
                 ; exc: Exception.exception_data option
                 } [@@deriving to_yojson, show]

let dval_to_livevalue (dv: dval) : livevalue =
  { value = Dval.to_livevalue_repr dv
  ; tipe = Dval.tipename dv
  ; json = dv
           |> Dval.dval_to_yojson ~livevalue:true
           |> Yojson.Safe.to_string
  ; exc = None
  }

let livevalue_dval_to_yojson v = v
                                 |> dval_to_livevalue
                                 |> livevalue_to_yojson


(* -------------------- *)
(* Dval store - save per-tl analysis results *)
(* -------------------- *)
let ht_to_json_dict ds ~f =
  let alist = Hashtbl.to_alist ds in
  `Assoc (
    List.map ~f:(fun (id, v) ->
        (string_of_id id, f v))
      alist)

type dval_store = dval IDTable.t

let dval_store_to_yojson (ds : dval_store) : Yojson.Safe.json =
  ht_to_json_dict ds ~f:livevalue_dval_to_yojson


(* -------------------- *)
(* Symstore - save available varnames at each point *)
(* -------------------- *)
module SymSet = String.Set
type sym_set = SymSet.t
type sym_store = sym_set IDTable.t

let sym_store_to_yojson (st : sym_store) : Yojson.Safe.json =
  ht_to_json_dict st ~f:(fun syms ->
      `List (syms
             |> SymSet.to_list
             |> List.map ~f:(fun s -> `String s)))




(* -------------------- *)
(* Sym lists - list of the input values *)
(* -------------------- *)
type sym_list = (string * livevalue) list
                [@@deriving to_yojson]

let sym_list_to_yojson (sl : sym_list) : Yojson.Safe.json =
  `Assoc (sl
          |> List.map ~f:(Tuple.T2.map_snd
                           ~f:livevalue_to_yojson))

let symtable_to_sym_list (st : symtable) : sym_list =
  st
  |> Map.to_alist
  |> List.map ~f:(Tuple.T2.map_snd ~f:dval_to_livevalue)


(* -------------------- *)
(* Analysis result *)
(* -------------------- *)
type analysis =
  { ast_value: livevalue
  ; live_values : dval_store
  ; available_varnames : sym_store
  ; input_values : sym_list
  } [@@deriving to_yojson]


type analysis_list = analysis list
                     [@@deriving to_yojson]

type analysis_result = tlid * analysis list

let analysis_result_to_yojson (id, results) =
  `Assoc [ ("id", id_to_yojson id)
         ; ("results", analysis_list_to_yojson results)
         ]

type analysis_result_list = analysis_result list
                          [@@deriving to_yojson]


