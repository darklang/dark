open Core

module RT = Runtime

(* --------------------- *)
(* Types *)
(* --------------------- *)
type fnname = string [@@deriving eq, yojson, show]
type varname = string [@@deriving eq, yojson, show]
type id = Types.id [@@deriving eq, yojson, show]

type varbinding = Named of varname
                | BindHole of int
                [@@deriving eq, yojson, show]

let is_unbound (vb: varbinding) : bool =
  match vb with
  | BindHole _ -> true
  | Named _ -> false

type expr = If of id * expr * expr * expr
          | FnCall of id * fnname * expr list
          | Variable of id * varname
          | Let of id * (varbinding * expr) list * expr
          | Lambda of id * varname list * expr
          | Value of id * string
          | Hole of id
          [@@deriving eq, yojson, show]

type ast = expr [@@deriving eq, yojson, show]

type toplevel = { id: Types.id
                ; pos: Types.pos
                ; ast: ast
                } [@@deriving eq, show, yojson]

(* --------------------- *)
(* API Types and Fns *)
(* --------------------- *)

type livevalue = { value: string
                 ; tipe: string [@key "type"]
                 ; json: string
                 ; exc: Exception.exception_data option
                 } [@@deriving yojson, show]

type analysis_record = { livevalue: livevalue
  (* available_symbols: varname list *)
                       } [@@deriving yojson, show]


type api_expr =
  { analysis_record: analysis_record
  ; if_: api_if option [@key "if"] [@default None]
  ; fncall: api_fncall option [@default None]
  ; variable: api_variable option [@default None]
  ; let_: api_let option [@key "let"] [@default None]
  ; lambda: api_lambda option [@default None]
  ; value: api_value option [@default None]
  ; hole: api_hole option [@default None]
  }
and api_if = { ifid : int [@key "id"]
             ; cond: api_expr
             ; then_: api_expr [@key "then"]
             ; else_: api_expr [@key "else"]
             }
and api_fncall = { fnid : int [@key "id"]
                 ; fnname : fnname [@key "name"]
                 ; arguments: api_expr list
                 }
and api_variable = { varid : int [@key "id"]
                   ; varname: varname [@key "name"]}
and api_let = { letid : int [@key "id"]
              ; bindings: api_let_binding list
              ; letbody: api_expr [@key "body"]
              }
and api_let_binding = { bname: varname option [@key "name"] [@default None]
                      ; bhole: api_hole option [@key "hole"] [@default None]
                      ; bexpr: api_expr [@key "expr"]
                      }
and api_lambda = { lambdaid : int [@key "id"]
                 ; varnames: varname list
                 ; lambdabody: api_expr [@key "body"]
                 }
and api_value = { valueid : int [@key "id"]
                ; valuestr: string }
and api_hole = { holeid: int [@key "id"]}
[@@deriving yojson]

type api_ast = api_expr [@@deriving yojson]

type api_toplevel = { tlid: int [@key "id"]
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
    If (a.ifid, a2e a.cond, a2e a.then_, a2e a.else_)
  | { fncall = Some a } ->
    FnCall (a.fnid, a.fnname, List.map ~f:a2e a.arguments)
  | { variable = Some a } ->
    Variable (a.varid, a.varname)
  | { let_ = Some a } ->
    let b2e b =
      (match b.bname with
       | Some n -> (Named n, a2e b.bexpr)
       | None ->
         (match b.bhole with
          | Some hole -> (BindHole hole.holeid, a2e b.bexpr)
          | None -> Exception.internal "Let binding missing name _and_ hole id"))
    in
    Let (a.letid, List.map ~f:b2e a.bindings, a2e a.letbody)
  | { lambda = Some a } ->
    Lambda (a.lambdaid, a.varnames, a2e a.lambdabody)
  | { value = Some a } ->
    Value (a.valueid, a.valuestr)
  | { hole = Some a } ->
    Hole (a.holeid)
  | _ -> Exception.internal "Unexpected api_expr"

let api_ast2ast = api_expr2expr

(* --------------------- *)
(* to API *)
(* --------------------- *)

let to_id = failwith "todo"

let rec expr2api_expr (results: analysis_results) (e: expr) : api_expr =
  let init = { analysis_record = get_analysis_result results (e |> to_id)
             ; if_ = None
             ; fncall = None
             ; variable = None
             ; let_ = None
             ; lambda = None
             ; value = None
             ; hole  = None } in
  let e2a = expr2api_expr in
  match e with
  | If (id, cond, then_, else_) ->
      { init with if_ = Some { ifid = id
                             ; cond = e2a cond
                             ; then_ = e2a then_
                             ; else_ = e2a else_
                       }}
  | FnCall (id, name, args) ->
    { init with fncall = Some { fnid = id; fnname = name; arguments = List.map ~f:e2a args }}
  | Variable (id, name) ->
    { init with variable = Some { varid = id; varname = name }}
  | Let (id, binds, body) ->
    let b2a (vb, e) =
      let binit = { bname = None; bhole = None; bexpr = e2a e } in
      (match vb with
       | BindHole id -> { binit with bhole = Some { holeid = id } }
       | Named s -> { binit with bname = Some s })
    in
    { init with let_ = Some { letid = id
                            ; bindings = List.map ~f:b2a binds
                            ; letbody  = e2a body
                            }
    }
  | Lambda (id, varnames, body) ->
    { init with lambda = Some { lambdaid = id; varnames; lambdabody = e2a body }}
  | Value (id, str) ->
    { init with value = Some { valueid = id; valuestr = str }}
  | Hole id ->
    { init with hole = Some { holeid = id }}

let ast2api_ast = expr2api_expr

let toplevel2api_toplevel (ar: analysis_results) (tl: toplevel) : api_toplevel =
  { tlid = tl.id
  ; pos = tl.pos
  ; ast = ast2api_ast ar tl.ast }

let toplevel_to_frontend (ar: analysis_results) (tl: toplevel) : Yojson.Safe.json =
  tl
  |> toplevel2api_toplevel ar
  |> api_toplevel_to_yojson


(* -------------------- *)
(* Execution *)
(* -------------------- *)
module Symtable = RT.DvalMap
type symtable = RT.dval_map

let rec exe (st: symtable) (expr: expr) : RT.dval =
  try
    (match expr with
    | Hole id ->
      DIncomplete

    | Let (_, bindings, body) ->
      if Util.list_any ~f:(fun (vb, _) -> is_unbound vb) bindings
      then DIncomplete
      else
        let vars = List.filter_map ~f:(fun (vb, expr) ->
            (match vb with
             | Named s -> Some (s, expr)
             | BindHole _ -> None)) bindings
        in
        let bound = List.fold_left ~init:st
            ~f:(fun st (name, expr) ->
            String.Map.add ~key:name ~data:(exe st expr) st) vars
        in exe bound body

    | Value (_, s) ->
      RT.parse s

    | Variable (_, name) ->
        (match Symtable.find st name with
        | None ->
          (* TODO we can put this in a DError and have great error messages *)
          DIncomplete
        | Some result -> result)

    | FnCall (id, name, exprs) ->
      let fn = Libs.get_fn_exn name in
      let argvals = List.map ~f:(exe st) exprs in
      (* equalize length *)
      let length_diff = List.length fn.parameters - List.length argvals in
      let argvals =
        if length_diff > 0
        then argvals @ (List.init length_diff (fun _ -> RT.DNull))
        else if length_diff = 0
        then argvals
        else Exception.user ("Too many args in fncall to " ^ name) in
      let args =
        fn.parameters
        |> List.map2_exn ~f:(fun dv (p: RT.param) -> (p.name, dv)) argvals
        |> RT.DvalMap.of_alist_exn in
      RT.exe ~ind:0 fn args

    | If (id, cond, ifbody, elsebody) ->
      (match (exe st cond) with
      | DBool true -> exe st ifbody
      | DBool false -> exe st elsebody
      | _ -> DIncomplete) (* TODO: better error *)

    | Lambda (id, vars, body) ->
      (* TODO: this will errror if the number of args and vars arent equal *)
      DBlock (fun args ->
          let bindings = Symtable.of_alist_exn (List.zip_exn vars args) in
          let new_st = Util.merge_left bindings st in
          exe new_st body))
  with
  | e ->
    let bt = Exn.backtrace () in
    let msg = Exn.to_string e in
    print_endline bt;
    print_endline msg;
    DIncomplete


