open Core
open Types

open Op
module RT = Runtime

type pos = Types.pos [@@deriving yojson]
type oplist = op list [@@deriving yojson]

let to_ops (payload: string) : op list =
  payload
  |> Yojson.Safe.from_string
  |> oplist_of_yojson
  |> Result.ok_or_failwith

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
                         } [@@deriving yojson]

let functions =
  Libs.static_fns
  |> String.Map.to_alist
  |> List.map ~f:(fun (k,(v: RuntimeT.fn))
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
                      ; infix = List.mem ~equal:(=) v.infix_names k
                      })
  |> fun l -> `List (List.map ~f:function_metadata_to_yojson l)
  |> Yojson.Safe.pretty_to_string
