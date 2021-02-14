module LibExecution.AnalysisTypes

// Types used during program analysis/traces

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module RT = LibExecution.RuntimeTypes
// module PReq = Parsed_request

// (* -------------------- *)
// (* Dval store - save per-tl analysis results *)
// (* -------------------- *)
// let ht_to_json_dict ds ~f =
//   let alist = Hashtbl.to_alist ds in
//   `Assoc (List.map ~f:(fun (id, v) -> (string_of_id id, f v)) alist)
//
//
// type intermediate_result_store = execution_result IDTable.t
//
// let intermediate_result_store_to_yojson (ds : intermediate_result_store) :
//     Yojson.Safe.t =
//   ht_to_json_dict ds ~f:execution_result_to_yojson


// --------------------
// Analysis result
// --------------------
// type analysis = intermediate_result_store [@@deriving to_yojson]

type InputVars = List<string * RT.Dval>

type FunctionArgHash = string
type HashVersion = int
type FnName = string
type FunctionResult = FnName * id * FunctionArgHash * HashVersion * RT.Dval

type TraceID = System.Guid

type TraceData =
  { input : InputVars
    timestamp : System.DateTime
    function_results : List<FunctionResult> }

type Trace = TraceID * Option<TraceData>

// type tlid_traces = tlid * trace list [@@deriving to_yojson]
//
// type tlid_traceid = tlid * traceid [@@deriving to_yojson]
//
// type traceid_tlids = traceid * tlid list [@@deriving to_yojson]
//
