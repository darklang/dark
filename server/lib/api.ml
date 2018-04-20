open Core
open Types

type oplist = Op.op list [@@deriving yojson]

type executable_fns = (tlid * id) list
                    [@@deriving eq, show, yojson, sexp]

type rpc_params = { ops: oplist
                  ; executable_fns: executable_fns }
                  [@@deriving yojson]


let to_rpc_params (payload: string) : rpc_params =
  payload
  |> Yojson.Safe.from_string
  |> rpc_params_of_yojson
  |> Result.ok_or_failwith

let causes_any_changes (ps: rpc_params) : bool =
  List.exists ~f:Op.has_effect ps.ops


(*------------------*)
(* Functions *)
(*------------------*)
type param_metadata = { name: string
                      ; tipe: string
                      ; block_args : string list
                      ; optional: bool
                      ; description: string
                      } [@@deriving yojson]

type function_metadata = { name: string
                         ; parameters : param_metadata list
                         ; description : string
                         ; return_type : string
                         ; infix : bool
                         ; preview_execution_safe: bool
                         } [@@deriving yojson]

let functions =
  Libs.static_fns
  |> String.Map.to_alist
  |> List.map
    ~f:(fun (k,(v: RuntimeT.fn))
         -> { name = k
            ; parameters =
                List.map ~f:(fun p : param_metadata ->
                    { name = p.name
                    ; tipe = Dval.tipe_to_string p.tipe
                    ; block_args = p.block_args
                    ; optional = p.optional
                    ; description = p.description })
                  v.parameters
            ; description = v.description
            ; return_type = Dval.tipe_to_string v.return_type
            ; preview_execution_safe = v.preview_execution_safe
            ; infix = List.mem ~equal:(=) v.infix_names k
            })
  |> fun l -> `List (List.map ~f:function_metadata_to_yojson l)
  |> Yojson.Safe.pretty_to_string
