open Core

open Types
open Types.RuntimeT
module RT = Runtime

let flatten_ff (bo: 'a or_blank) : 'a or_blank =
  match bo with
  | Flagged (msg, setting, l, r) ->
      if setting >= 50
      then r
      else l
  | _ -> bo

let should_be_flat () =
  Exception.internal "This blank_or should have been flattened"

let blank_to_id (blank: 'a or_blank) : id =
  match flatten_ff blank with
  | Filled (id, _) -> id
  | Blank (id) -> id
  | Flagged _ -> should_be_flat ()

let to_id (expr: expr) : id =
  blank_to_id expr

let to_tuple (expr: expr) : (id * expr) =
  let id = to_id expr in
  (id, expr)

(* -------------------- *)
(* Execution *)
(* -------------------- *)
module Symtable = DvalMap
type symtable = dval_map

let empty_trace _ _ _ = ()

let rec exec_ ?(trace: (expr -> dval -> symtable -> unit)=empty_trace)
              ?(trace_blank: (string or_blank -> dval -> symtable -> unit)=empty_trace)
              ~(ctx: context) (st: symtable) (expr: expr) : dval =
  let exe = exec_ ~trace ~trace_blank ~ctx in
  let call (name: string) (argvals: dval list) : dval =
    let fn = Libs.get_fn_exn name in
    (* equalize length *)
    let length_diff = List.length fn.parameters - List.length argvals in
    let argvals =
      if length_diff > 0
      then argvals @ (List.init length_diff (fun _ -> DNull))
      else if length_diff = 0
      then argvals
      else Exception.user ("Too many args in fncall to " ^ name) in
    let args =
      fn.parameters
      |> List.map2_exn ~f:(fun dv (p: param) -> (p.name, dv)) argvals
      |> DvalMap.of_alist_exn in
   call_fn ~ind:0 ~ctx name fn args
  in

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
  let inject_param_and_execute (st: symtable) (param: dval) (exp: expr) : dval =
    let result =
      match exp with
      | Filled (id, Lambda _) ->
        let result = exe st exp in
        (match result with
         | DBlock blk -> blk [param]
         | _ -> DIncomplete)
      | Filled (id, FnCall (name, exprs)) ->
        call name (param :: (List.map ~f:(exe st) exprs))
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | Blank _ ->
        param
      | _ -> DIncomplete (* partial w/ exception, full with dincomplete, or option dval? *)
    in
    trace exp result st;
    result

  in

  let ignoreError e =
    match e with
    | DError _ -> DIncomplete
    | _ -> e in

  let value _ =
    (match flatten_ff expr with
     | Blank id ->
       DIncomplete

     | Flagged _ -> should_be_flat ()

     | Filled (_, Let (lhs, rhs, body)) ->
       let bound = match flatten_ff lhs with
            | Filled (_, name) ->
              let data = exe st rhs in
              trace_blank lhs data st;
              String.Map.set ~key:name ~data:data st
            | Blank _ -> st
            | Flagged _ -> should_be_flat ()
       in exe bound body

     | Filled (_, Value s) ->
       Dval.parse s

     | Filled (_, Variable name) ->
       (match Symtable.find st name with
        | None ->
          Exception.user ("There is no variables named: " ^ name)
        | Some result -> ignoreError result)

     | Filled (id, FnCall (name, exprs)) ->
       let argvals = List.map ~f:(exe st) exprs in
       call name argvals

     | Filled (id, If (cond, ifbody, elsebody)) ->
       (match ctx with
        | Preview ->
          (* In the case of a preview trace execution, we want the 'if' expression as
           * a whole to evaluate to its correct value -- but we also want preview values
           * for _all_ sides of the if *)
          (match (exe st cond) with
           | DBool false | DNull ->
             (* execute the positive side just for the side-effect *)
             let _ = exe st ifbody in
             exe st elsebody
           | DIncomplete ->
             let _ = exe st ifbody in
             let _ = exe st elsebody in
             DIncomplete
           | DError _ ->
             let _ = exe st ifbody in
             let _ = exe st elsebody in
             DIncomplete
           | _ ->
             (* execute the negative side just for the side-effect *)
             let _ = exe st elsebody in
             exe st ifbody)
        | Real ->
          (* In the case of a 'real' evaluation, we shouldn't do unneccessary work and
           * as such should follow the proper evaluation semantics *)
          (match (exe st cond) with
           (* only false and 'null' are falsey *)
           | DBool false | DNull -> exe st elsebody
           | DIncomplete -> DIncomplete
           | DError _ -> DIncomplete
           | _ -> exe st ifbody))

     | Filled (id, Lambda (vars, body)) ->
       (* TODO: this will errror if the number of args and vars arent equal *)
       DBlock (fun args ->
           let bindings = Symtable.of_alist_exn (List.zip_exn vars args) in
           let new_st = Util.merge_left bindings st in
           exe new_st body)
     | Filled (id, Thread exprs) ->
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
        | [] -> DIncomplete)
     | Filled (id, FieldAccess (e, field)) ->
       let obj = exe st e in
       (match obj with
        | DObj o ->
          (match flatten_ff field with
           | Blank _ -> DIncomplete
           | Flagged _ -> should_be_flat ()
           | Filled (_, f) ->
             (match Map.find o f with
              | Some v -> v
              | None -> DNull))
        | DIncomplete -> DIncomplete
        | DError _ -> DIncomplete
        | x -> DError ("Can't access field of non-object: " ^ (Dval.to_repr x)))
    ) in
  (* Only catch if we're tracing *)
  let execed_value =
    if phys_equal trace empty_trace (* only way to compare functions *)
    then value ()
    else
      try
        value ()
      with
      | Exception.DarkException e ->
        let json = e
                   |> Exception.exception_data_to_yojson
                   |> Yojson.Safe.pretty_to_string in
        print_endline json;
        DError json
      | e ->
        let bt = Backtrace.Exn.most_recent () in
        let msg = Exn.to_string e in
        print_endline (Backtrace.to_string bt);
        print_endline msg;
        DIncomplete
  in
  trace expr execed_value st; execed_value
and call_fn ?(ind=0) ~(ctx: context) (fnname: string) (fn: fn) (args: dval_map) : dval =
  let apply f arglist =
    match ctx with
    | Preview ->
      if fn.previewExecutionSafe
      then f arglist
      else DIncomplete
    | Real ->
      f arglist
  in
  match fn.func with
  | InProcess f ->
      let arglist = fn.parameters
                    |> List.map ~f:(fun (p: param) -> p.name)
                    |> List.map ~f:(DvalMap.find_exn args) in
      (try
         if List.exists ~f:(fun x ->
                              match x with
                              | DIncomplete -> true
                              | DError _ -> true
                              | _ -> false)
             arglist
         then DIncomplete
         else apply f arglist
       with
       | TypeError _ ->
           Log.erroR ~name:"execution" ~ind "exception caught" args
             ~f:Dval.dvalmap_to_string;
           let range = List.range 0 (List.length arglist) in
           let all = List.map3_exn range fn.parameters arglist ~f:(fun i p a -> (i,p,a)) in
           let invalid = List.filter_map all
                           ~f:(fun (i,p,a) -> if (Dval.tipe_of a <> p.tipe
                                                  && p.tipe <> TAny)
                               then Some (i,p,a)
                               else None) in
           (* let invalid_count = List.length invalid in *)
           match invalid with
           | [] -> Exception.internal "There was an type error in the arguments, but we had an error and can't find it"

           | (i,p,a) :: _ ->
              RT.raise_error
                ~actual:a
                ~expected:(Dval.tipe_to_string p.tipe)
                (fnname ^ " was called with the wrong type to parameter: " ^ p.name))

  | UserCreated _ -> failwith "TODO(ian)"
  | API f ->
      try
        f args
      with
      | TypeError args ->
        let param_to_string (param: param) : string =
          param.name
          ^ (if param.optional then "?" else "")
          ^ " : "
          ^ (Dval.tipe_to_string param.tipe)
        in
        RT.raise_error (fnname ^ " is missing a parameter")
          ~expected:(fn.parameters
                     |> List.map ~f:param_to_string
                     |> String.concat ~sep:", ")
          ~actual:DIncomplete

(* default to no tracing *)
let execute = exec_ ~trace:empty_trace ~ctx:Real

(* -------------------- *)
(* Analysis *)
(* -------------------- *)


type dval_store = dval Int.Table.t

let execute_saving_intermediates (init: symtable) (ast: expr) : (dval * dval_store) =
  let value_store = Int.Table.create () in
  let trace expr dval st =
    Hashtbl.set value_store ~key:(to_id expr) ~data:dval
  in
  let trace_blank blank dval st =
    Hashtbl.set value_store ~key:(blank_to_id blank) ~data:dval
  in
  (exec_ ~trace ~trace_blank ~ctx:Preview init ast, value_store)

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

let dval_to_livevalue (dv: dval) : livevalue =
  { value = Dval.to_repr dv
  ; tipe = Dval.tipename dv
  ; json = dv |> Dval.dval_to_yojson |> Yojson.Safe.pretty_to_string
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
      (match flatten_ff expr with
       | Blank _ -> ()
       | Flagged _ -> should_be_flat ()
       | Filled (_, Value s) -> ()
       | Filled (_, Variable name) -> ()

       | Filled (_, Let (lhs, rhs, body)) ->
         let bound = match flatten_ff lhs with
           | Flagged _ -> should_be_flat ()
           | Filled (_, name) -> sexe st rhs; SymSet.add st name
           | Blank _ -> st
         in sexe bound body

       | Filled (_, FnCall (name, exprs)) -> List.iter ~f:(sexe st) exprs

       | Filled (_, If (cond, ifbody, elsebody)) ->
         sexe st cond;
         sexe st ifbody;
         sexe st elsebody

       | Filled (_, Lambda (vars, body)) ->
         let new_st = List.fold_left ~init:st ~f:(fun st v -> SymSet.add st v) vars in
         sexe new_st body

       | Filled (_, Thread (exprs)) ->
         List.iter ~f:(fun expr -> sexe st expr) exprs

       | Filled (_, FieldAccess (obj, field)) ->
         sexe st obj)
    in
    trace expr st
  with
  | e ->
    let bt = Backtrace.Exn.most_recent () in
    let msg = Exn.to_string e in
    print_endline (Backtrace.to_string bt);
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
      (Symtable.keys init)
  in
  sym_exec ~trace init_set ast; sym_store

let sym_store_to_yojson (st : sym_store) : Yojson.Safe.json =
  ht_to_json_dict st ~f:(fun syms ->
      `List (syms
             |> SymSet.to_list
             |> List.map ~f:(fun s -> `String s)))

