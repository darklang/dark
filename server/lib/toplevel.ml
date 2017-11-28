open Core

module RT = Runtime

(* --------------------- *)
(* Types *)
(* --------------------- *)
type handler_spec = { module_ : string [@key "module"]
                    ; name : string
                    ; modifier : string
                    } [@@deriving eq, show, yojson]


type toplevel = { id: Types.id
                ; pos: Types.pos
                ; ast: Ast.ast
                ; handler_spec : handler_spec option
                } [@@deriving eq, show, yojson]

(* --------------------- *)
(* API Types and Fns *)
(* --------------------- *)
type api_toplevel = { tlid: int [@key "id"]
                    ; pos: Types.pos
                    ; ast: Ast.api_ast
                    ; handler_spec: handler_spec option
                    } [@@deriving yojson]

(* --------------------- *)
(* to API *)
(* --------------------- *)
let toplevel2api_toplevel (tl: toplevel) : api_toplevel =
  { tlid = tl.id
  ; pos = tl.pos
  ; ast = Ast.ast2api_ast tl.ast
  ; handler_spec = tl.handler_spec
  }

let toplevel_to_frontend (tl: toplevel) : Yojson.Safe.json =
  tl
  |> toplevel2api_toplevel
  |> api_toplevel_to_yojson



