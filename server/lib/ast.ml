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

type expr = If of id * expr * expr * expr
          | Thread of id * expr list
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

type api_expr =
  { if_: api_if option [@key "if"] [@default None]
  ; fncall: api_fncall option [@default None]
  ; variable: api_variable option [@default None]
  ; let_: api_let option [@key "let"] [@default None]
  ; lambda: api_lambda option [@default None]
  ; value: api_value option [@default None]
  ; hole: api_hole option [@default None]
  ; thread: api_thread option [@default None]
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
and api_thread = { threadid : int [@key "id"]
                 ; threadexprs: api_expr list
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
  | { thread = Some a } ->
    Thread (a.threadid, List.map ~f:a2e a.threadexprs)
  | _ -> Exception.internal "Unexpected api_expr"

let api_ast2ast = api_expr2expr

(* --------------------- *)
(* to API *)
(* --------------------- *)

let rec expr2api_expr (e: expr) : api_expr =
  let init = { if_ = None
             ; fncall = None
             ; variable = None
             ; let_ = None
             ; lambda = None
             ; value = None
             ; hole  = None
             ; thread = None
             } in
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
  | Thread (id, exprs) ->
    { init with thread = Some { threadid = id; threadexprs = List.map ~f:e2a exprs }}

let ast2api_ast = expr2api_expr

let toplevel2api_toplevel (tl: toplevel) : api_toplevel =
  { tlid = tl.id
  ; pos = tl.pos
  ; ast = ast2api_ast tl.ast
  }

let toplevel_to_frontend (tl: toplevel) : Yojson.Safe.json =
  tl
  |> toplevel2api_toplevel
  |> api_toplevel_to_yojson


(* -------------------- *)
(* Execution *)
(* -------------------- *)
module Symtable = RT.DvalMap
type symtable = RT.dval_map

let rec exec ~(trace: (expr -> RT.dval -> symtable -> unit)) (st: symtable) (expr: expr) : RT.dval =
  let exe = exec ~trace in
  try
    let value =
      (match expr with
       | Hole id ->
         RT.DIncomplete

       | Let (_, bindings, body) ->
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
            RT.DIncomplete
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
          | _ -> RT.DIncomplete) (* TODO: better error *)

       | Lambda (id, vars, body) ->
         (* TODO: this will errror if the number of args and vars arent equal *)
         DBlock (fun args ->
             let bindings = Symtable.of_alist_exn (List.zip_exn vars args) in
             let new_st = Util.merge_left bindings st in
             exe new_st body)
       | Thread (_, _) -> DIncomplete)
    in
    trace expr value st; value
  with
  | e ->
    let bt = Exn.backtrace () in
    let msg = Exn.to_string e in
    print_endline bt;
    print_endline msg;
    RT.DIncomplete

let to_tuple (expr: expr) : (id * expr) =
  match expr with
  | Hole (id) -> (id, expr)
  | Value (id, s) -> (id, expr)
  | Variable (id, name) -> (id, expr)
  | Let (id, bindings, body) -> (id, expr)
  | FnCall (id, me, exprs) -> (id, expr)
  | If (id, cond, ifbody, elsebody) -> (id, expr)
  | Lambda (id, vars, body) -> (id, expr)
  | Thread (id, exprs) -> (id, expr)

let to_id (expr: expr) : id =
  to_tuple expr |> Tuple.T2.get1

type dval_store = RT.dval Int.Table.t

let execute (ast: expr) : (RT.dval * dval_store) =
  let value_store = Int.Table.create () in
  let trace expr dval st =
    Hashtbl.set value_store ~key:(to_id expr) ~data:dval
  in
  (exec ~trace Symtable.empty ast, value_store)

let ht_to_json_dict ds ~f =
  let alist = Hashtbl.to_alist ds in
  `Assoc (
    List.map ~f:(fun (id, v) ->
        (string_of_int id, f v))
      alist)

type livevalue = { value: string
                 ; tipe: string [@key "type"]
                 ; json: string
                 ; exc: Exception.exception_data option
                 } [@@deriving to_yojson, show]

let dval_to_livevalue (dv: RT.dval) : livevalue =
  { value = RT.to_repr dv
  ; tipe = RT.tipename dv
  ; json = dv |> RT.dval_to_yojson |> Yojson.Safe.pretty_to_string
  ; exc = None
  }

let dval_store_to_yojson (ds : dval_store) : Yojson.Safe.json =
  ht_to_json_dict ds ~f:(fun dv -> dv |> dval_to_livevalue |> livevalue_to_yojson)


module SymSet = Set.Make(String)
type sym_set = SymSet.t

let rec sym_exec ~(trace: (expr -> sym_set -> unit)) (st: sym_set) (expr: expr) : unit =
  let sexe = sym_exec ~trace in
  try
    let _ =
      (match expr with
       | Hole id -> ()
       | Value (_, s) -> ()
       | Variable (_, name) -> ()

       | Let (_, bindings, body) ->
         let vars = List.filter_map ~f:(fun (vb, expr) ->
             (match vb with
              | Named s -> Some (s, expr)
              | BindHole _ -> None)) bindings
         in
         let bound = List.fold_left ~init:st
             ~f:(fun st (name, expr) -> sexe st expr; SymSet.add st name) vars
         in sexe bound body

       | FnCall (id, name, exprs) -> List.iter ~f:(sexe st) exprs

       | If (id, cond, ifbody, elsebody) ->
         sexe st cond;
         sexe st ifbody;
         sexe st elsebody

       | Lambda (id, vars, body) ->
         let new_st = List.fold_left ~init:st ~f:(fun st v -> SymSet.add st v) vars in
         sexe new_st body

       | Thread (id, exprs) ->
         List.iter ~f:(fun expr -> sexe st expr) exprs)
    in
    trace expr st
  with
  | e ->
    let bt = Exn.backtrace () in
    let msg = Exn.to_string e in
    print_endline bt;
    print_endline msg;

type sym_store = sym_set Int.Table.t

let symbolic_execute (ast: expr) : sym_store =
  let sym_store = Int.Table.create () in
  let trace expr st =
    Hashtbl.set sym_store ~key:(to_id expr) ~data:st
  in
  sym_exec ~trace SymSet.empty ast; sym_store

let sym_store_to_yojson (st : sym_store) : Yojson.Safe.json =
  ht_to_json_dict st ~f:(fun syms ->
      `List (syms
             |> SymSet.to_list
             |> List.map ~f:(fun s -> `String s)))

