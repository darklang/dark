open Core

module RT = Runtime

(* --------------------- *)
(* Types *)
(* --------------------- *)
type fnname = string [@@deriving eq, yojson, show]
type fieldname = string [@@deriving eq, yojson, show]
type varname = string [@@deriving eq, yojson, show]
type id = Types.id [@@deriving eq, yojson, show]
type 'a or_hole = 'a Types.or_hole [@@deriving eq, yojson, show]

type varbinding = varname or_hole
                [@@deriving eq, yojson, show]

type field = fieldname or_hole
           [@@deriving eq, yojson, show]


type expr = If of id * expr * expr * expr
          | Thread of id * expr list
          | FnCall of id * fnname * expr list
          | Variable of id * varname
          | Let of id * varbinding * expr * expr
          | Lambda of id * varname list * expr
          | Value of id * string
          | Hole of id
          | FieldAccess of id * expr * field
          [@@deriving eq, yojson, show]

type ast = expr [@@deriving eq, yojson, show]

let to_tuple (expr: expr) : (id * expr) =
  match expr with
  | Hole (id) -> (id, expr)
  | Value (id, s) -> (id, expr)
  | Variable (id, name) -> (id, expr)
  | Let (id, lhs, rhs, body) -> (id, expr)
  | FnCall (id, me, exprs) -> (id, expr)
  | If (id, cond, ifbody, elsebody) -> (id, expr)
  | Lambda (id, vars, body) -> (id, expr)
  | Thread (id, exprs) -> (id, expr)
  | FieldAccess (id, obj, field) -> (id, expr)

let to_id (expr: expr) : id =
  to_tuple expr |> Tuple.T2.get1

let is_hole (expr: expr) =
  match expr with
  | Hole _ -> true
  | _ -> false

(* -------------------- *)
(* Execution *)
(* -------------------- *)
module Symtable = RT.DvalMap
type symtable = RT.dval_map

let empty_trace _ _ _ = ()

let rec exec_ ?(trace: (expr -> RT.dval -> symtable -> unit)=empty_trace) (st: symtable) (expr: expr) : RT.dval =
  let exe = exec_ ~trace in

  (* This is a super hacky way to inject params as the result of pipelining using the `Thread` construct
   * -- it's definitely not a good thing to be doing, for a variety of reasons.
   *     - We dump the passed dval to json to stick it into a Value
   *     - More generally, we're mutating the ASTs exprs to inject dvals into them
   *
   * `Thread` as a separate construct in the AST as opposed to just being a function application
   * is probably the root cause of this. Right now, we don't have function application in the language
   * as FnCall is the AST element that actually handles interacting with the OCaml runtime to do
   * useful work. We're going to need to make this a functional language with functions-as-values
   * and application as a first-class concept sooner rather than later.
   *)
  let call (name: string) (argvals: RT.dval list) : RT.dval =
    let fn = Libs.get_fn_exn name in
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
  in


  let inject_param_and_execute (st: symtable) (param: RT.dval) (exp: expr) : RT.dval =
    match exp with
    | Lambda _ ->
      let result = exe st exp in
      (match result with
       | DBlock blk -> blk [param]
       | _ -> DIncomplete)
    | FnCall (id, name, exprs) ->
      call name (param :: (List.map ~f:(exe st) exprs))
    | _ -> DIncomplete (* partial w/ exception, full with dincomplete, or option dval? *)
  in

  try
    let value =
      (match expr with
       | Hole id ->
         RT.DIncomplete

       | Let (_, lhs, rhs, body) ->
         let bound = match lhs with
              | Full name -> String.Map.add ~key:name ~data:(exe st rhs) st
              | Empty _ -> st
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
         let argvals = List.map ~f:(exe st) exprs in
         call name argvals

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
       | Thread (id, exprs) ->
         (* For each expr, execute it, and then thread the previous result thru *)
         (match exprs with
          | e :: es ->
            let fst = exe st e in
            let results =
              List.fold_left
                ~init:[fst]
                ~f:(fun results nxt ->
                    let previous = List.hd_exn results in
                    let value = inject_param_and_execute st previous nxt in
                    value :: results
                  ) es
            in
            List.hd_exn results
          | _ -> DIncomplete)
       | FieldAccess (id, e, field) ->
         let obj = exe st e in
         (match obj with
          | DObj o ->
            (match field with
             | Empty _ -> DIncomplete
             | Full f ->
               (match Map.find o f with
                | Some v -> v
                | None -> Exception.user ("Object has no field named: " ^ f)))
          | _ -> Exception.user "type mismatch, expected object")
      )
    in
    trace expr value st; value
  with
  | e ->
    let bt = Exn.backtrace () in
    let msg = Exn.to_string e in
    print_endline bt;
    print_endline msg;
    RT.DIncomplete

let execute = exec_

(* -------------------- *)
(* Analysis *)
(* -------------------- *)


type dval_store = RT.dval Int.Table.t

let execute_saving_intermediates (init: symtable) (ast: expr) : (RT.dval * dval_store) =
  let value_store = Int.Table.create () in
  let trace expr dval st =
    Hashtbl.set value_store ~key:(to_id expr) ~data:dval
  in
  (exec_ ~trace init ast, value_store)

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

       | Let (_, lhs, rhs, body) ->
         let bound = match lhs with
           | Full name -> sexe st rhs; SymSet.add st name
           | Empty _ -> st
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
         List.iter ~f:(fun expr -> sexe st expr) exprs
       | FieldAccess (id, obj, field) -> ())
    in
    trace expr st
  with
  | e ->
    let bt = Exn.backtrace () in
    let msg = Exn.to_string e in
    print_endline bt;
    print_endline msg;

type sym_store = sym_set Int.Table.t

let symbolic_execute (init: symtable) (ast: expr) : sym_store =
  let sym_store = Int.Table.create () in
  let trace expr st =
    Hashtbl.set sym_store ~key:(to_id expr) ~data:st
  in
  let init_set =
    List.fold_left
      ~init:SymSet.empty
      ~f:(fun acc s ->
          SymSet.add acc s)
      ("request" :: (Symtable.keys init))
  in
  sym_exec ~trace init_set ast; sym_store

let sym_store_to_yojson (st : sym_store) : Yojson.Safe.json =
  ht_to_json_dict st ~f:(fun syms ->
      `List (syms
             |> SymSet.to_list
             |> List.map ~f:(fun s -> `String s)))

