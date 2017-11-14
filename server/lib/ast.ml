open Core

module RT = Runtime

(* --------------------- *)
(* Types *)
(* --------------------- *)
type fnname = string [@@deriving eq, yojson, show]
type varname = string [@@deriving eq, yojson, show]

type expr = If of expr * expr * expr
          | FnCall of fnname * expr list
          | Variable of varname
          | Let of (varname * expr) list * expr
          | Lambda of varname list * expr
          | Value of string
          | Hole of int
          [@@deriving eq, yojson, show]

type ast = expr [@@deriving eq, yojson, show]

type toplevel = { id: Types.id
                ; pos: Types.pos
                ; ast: ast
                } [@@deriving eq, show, yojson]

(* --------------------- *)
(* API Types and Fns *)
(* --------------------- *)

type api_expr =
  { if_: api_if option [@key "if"] [@default None]
  ; fncall: api_fncall option [@default None]
  ; variable: api_variable option [@default None]
  ; let_: api_let option [@key "let"] [@default None]
  ; lambda: api_lambda option [@default None]
  ; value: api_value option [@default None]
  ; hole: api_hole option [@default None]
  }
and api_if = { cond: api_expr
             ; then_: api_expr [@key "then"]
             ; else_: api_expr [@key "else"]
             }
and api_fncall = { name : fnname
                 ; arguments: api_expr list
                 }
and api_variable = varname
and api_let = { bindings: api_let_binding list
              ; letbody: api_expr [@key "body"]
              }
and api_let_binding = { bname: varname [@key "name"]
                      ; bexpr: api_expr [@key "expr"]
                      }
and api_lambda = { vars: varname list
                 ; lambdabody: api_expr [@key "body"]
                 }
and api_value = string
and api_hole = { id: int }
[@@deriving yojson]

type api_ast = api_expr [@@deriving yojson]

type api_toplevel = { id: int
                    ; pos: Types.pos
                    ; ast: api_ast
                    } [@@deriving yojson]

(* --------------------- *)
(* from API *)
(* --------------------- *)
let rec api_expr2expr (e: api_expr) : expr =
  let a2e = api_expr2expr in
  match e with
  | { if_ = Some a } ->
    If (a2e a.cond, a2e a.then_, a2e a.else_)
  | { fncall = Some a } ->
    FnCall (a.name, List.map ~f:a2e a.arguments)
  | { variable = Some a } ->
    Variable a
  | { let_ = Some a } ->
    Let (List.map ~f:(fun b -> (b.bname, a2e b.bexpr)) a.bindings, a2e a.letbody)
  | { lambda = Some a } ->
    Lambda (a.vars, a2e a.lambdabody)
  | { value = Some a } ->
    Value a
  | { hole = Some a } ->
    Hole a.id
  | _ -> Exception.internal "Unexpected api_expr"

let api_ast2ast = api_expr2expr

(* --------------------- *)
(* to API *)
(* --------------------- *)

let rec expr2api_expr (e: expr) : api_expr =
  let empty = { if_ = None
              ; fncall = None
              ; variable = None
              ; let_ = None
              ; lambda = None
              ; value = None
              ; hole  = None } in
  let e2a = expr2api_expr in
  match e with
  | If (cond, then_, else_) ->
      { empty with if_ = Some { cond = e2a cond
                       ; then_ = e2a then_
                       ; else_ = e2a else_
                       }}
  | FnCall (name, args) ->
    { empty with fncall = Some { name; arguments = List.map ~f:e2a args } }
  | Variable v ->
    { empty with variable = Some v }
  | Let (binds, body) ->
    { empty with let_ = Some { bindings = List.map ~f:(fun (v,e) -> { bname = v; bexpr = e2a e }) binds
                             ; letbody  = e2a body
                             }
    }
  | Lambda (vars, body) ->
    { empty with lambda = Some { vars; lambdabody = e2a body } }
  | Value v ->
    { empty with value = Some v }
  | Hole id -> { empty with hole = Some { id = id } }

let ast2api_ast = expr2api_expr

let toplevel2api_toplevel (tl: toplevel) : api_toplevel =
  { id = tl.id
  ; pos = tl.pos
  ; ast = ast2api_ast tl.ast }

let toplevel_to_frontend (tl: toplevel) : Yojson.Safe.json =
  tl
  |> toplevel2api_toplevel
  |> api_toplevel_to_yojson


(* -------------------- *)
(* Execution *)
(* -------------------- *)
module Symtable = RT.DvalMap
type symtable = RT.dval_map

let rec exe (st: symtable) (expr: expr) : RT.dval =
  match expr with
  | Hole id ->
    DIncomplete

  | Let (bindings, body) ->
    let bound = List.fold_left ~init:st
        ~f:(fun st (name, expr) ->
        String.Map.add ~key:name ~data:(exe st expr) st) bindings
    in exe bound expr

  | Value s ->
    RT.parse s

  | Variable name ->
      (match Symtable.find st name with
      | None ->
        (* TODO we can put this in a DError and have great error messages *)
        DIncomplete
      | Some result -> result)

  | FnCall (name, exprs) ->
    let fn = Libs.get_fn_exn name in
    let argvals = List.map ~f:(exe st) exprs in
    (* equalize length *)
    let length_diff = List.length fn.parameters - List.length argvals in
    let argvals =
      if length_diff > 0
      then argvals @ (List.init length_diff (fun _ -> RT.DNull))
      else Exception.user ("Too many args in fncall to " ^ name) in
    let args =
      fn.parameters
      |> List.map2_exn ~f:(fun dv (p: RT.param) -> (p.name, dv)) argvals
      |> RT.DvalMap.of_alist_exn in
    RT.exe ~ind:0 fn args

  | If (cond, ifbody, elsebody) ->
    (match (exe st cond) with
    | DBool true -> exe st ifbody
    | DBool false -> exe st elsebody
    | _ -> DIncomplete) (* TODO: better error *)

  | Lambda (vars, body) ->
    (* TODO: this will errror if the number of args and vars arent equal *)
    DBlock (fun args ->
        let bindings = Symtable.of_alist_exn (List.zip_exn vars args) in
        let new_st = Util.merge_left bindings st in
        exe new_st body)


