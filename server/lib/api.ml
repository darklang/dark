open Core
open Types

open Op
module RT = Runtime
module F = Functions

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
type param_ = { name: string
              ; tipe: string
              ; block_args : string list
              ; optional: bool
              ; description: string
              } [@@deriving yojson]

type function_ = { name: string
                 ; parameters : param_ list
                 ; description : string
                 ; return_type : string} [@@deriving yojson]
type functionlist = function_ list [@@deriving yojson]

let functions =
  Libs.fns
  |> String.Map.to_alist
  |> List.map ~f:(fun (k,(v:F.fn))
                   -> { name = k
                      ; parameters =
                        List.map ~f:(fun p : param_ ->
                          { name = p.name
                          ; tipe = Dval.tipe_to_string p.tipe
                          ; block_args = p.block_args
                          ; optional = p.optional
                          ; description = p.description })
                        v.parameters
                      ; description = v.description
                      ; return_type = Dval.tipe_to_string v.return_type
                      })
  |> functionlist_to_yojson
  |> Yojson.Safe.pretty_to_string
