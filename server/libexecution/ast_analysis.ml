open Core_kernel

open Types
open Types.RuntimeT

module RT = Runtime
module FF = Feature_flag
module PReq = Parsed_request


(* -------------------- *)
(* Types for Analysis *)
(* -------------------- *)

(* Live values *)
type livevalue = { value: string
                 ; tipe: string [@key "type"]
                 ; json: string
                 ; exc: Exception.exception_data option
                 } [@@deriving to_yojson, show]

let dval_to_livevalue (dv: dval) : livevalue =
  { value = Dval.to_livevalue_repr dv
  ; tipe = Dval.tipename dv
  ; json = dv
           |> Dval.dval_to_yojson ~livevalue:true
           |> Yojson.Safe.pretty_to_string
  ; exc = None
  }

let livevalue_dval_to_yojson v = v
                                 |> dval_to_livevalue
                                 |> livevalue_to_yojson


(* Dval store - save per-tl analysis results *)
let ht_to_json_dict ds ~f =
  let alist = Hashtbl.to_alist ds in
  `Assoc (
    List.map ~f:(fun (id, v) ->
        (string_of_int id, f v))
      alist)

type dval_store = dval Int.Table.t

let dval_store_to_yojson (ds : dval_store) : Yojson.Safe.json =
  ht_to_json_dict ds ~f:livevalue_dval_to_yojson


(* Symstore - save available varnames at each point *)
module SymSet = String.Set
type sym_set = SymSet.t
type sym_store = sym_set Int.Table.t

let sym_store_to_yojson (st : sym_store) : Yojson.Safe.json =
  ht_to_json_dict st ~f:(fun syms ->
      `List (syms
             |> SymSet.to_list
             |> List.map ~f:(fun s -> `String s)))




(* Sym lists - list of the input values *)
type sym_list = (string * livevalue) list
                [@@deriving to_yojson]

let sym_list_to_yojson (sl : sym_list) : Yojson.Safe.json =
  `Assoc (sl
          |> List.map ~f:(Tuple.T2.map_snd
                           ~f:livevalue_to_yojson))

let symtable_to_sym_list (st : symtable) : sym_list =
  st
  |> Map.to_alist
  |> List.map ~f:(Tuple.T2.map_snd
                    ~f:dval_to_livevalue)


(* Analysis result *)
type analysis =
  { ast_value: livevalue
  ; live_values : dval_store
  ; available_varnames : sym_store
  ; input_values : sym_list
  } [@@deriving to_yojson]


type analysis_list = analysis list
                     [@@deriving to_yojson]

(* -------------------- *)
(* Symbolically gather varnames *)
(* -------------------- *)

let flatten_ff (bo: 'a or_blank) (ff: feature_flag) : 'a or_blank =
  match bo with
  | Flagged (id, msg, setting, l, r) ->
      FF.select id setting l r ff
  | _ -> bo

let should_be_flat () =
  Exception.internal "This blank_or should have been flattened"

let blank_to_content (bo: 'a or_blank) : 'a option =
  match bo with
  | Filled (_, c) -> Some c
  | _ -> None


let rec sym_exec
    ~(trace: (expr -> sym_set -> unit))
    (state: exec_state)
    (st: sym_set)
    (expr: expr)
  : unit =
  let sexe = sym_exec ~trace state in
  try
    let _ =
      (match expr with
       | Blank _ -> ()
       | Flagged (id, _, _, l, r) ->
         sexe st l;
         sexe st r;
         sexe st (flatten_ff expr state.ff)

       | Filled (_, Value s) -> ()
       | Filled (_, Variable name) -> ()

       | Filled (_, Let (lhs, rhs, body)) ->
         let bound = match lhs with
           | Flagged _ -> should_be_flat ()
           | Filled (_, name) ->
             sexe st rhs;
             SymSet.add st name
           | Blank _ -> st
         in sexe bound body

       | Filled (_, FnCall (name, exprs)) ->
         List.iter ~f:(sexe st) exprs

       | Filled (_, If (cond, ifbody, elsebody)) ->
         sexe st cond;
         sexe st ifbody;
         sexe st elsebody

       | Filled (_, Lambda (vars, body)) ->
         let new_st =
           vars
           |> List.map
             ~f:(fun v -> flatten_ff v state.ff)
           |> List.filter_map ~f:blank_to_content
           |> SymSet.of_list
           |> SymSet.union st
         in
         sexe new_st body

       | Filled (_, Thread (exprs)) ->
         List.iter ~f:(sexe st) exprs

       | Filled (_, FieldAccess (obj, field)) ->
         sexe st obj

       | Filled (_, ListLiteral exprs) ->
         List.iter ~f:(sexe st) exprs

       | Filled (_, ObjectLiteral exprs) ->
         exprs
         |> List.map ~f:Tuple.T2.get2
         |> List.iter ~f:(sexe st))
    in
    trace expr st
  with
  | e ->
    let bt = Exception.get_backtrace () in
    Log.erroR "exception_during_symexec" ~bt
      ~params:["exception", Exception.to_string e
              ;"execution_id", Log.dump state.execution_id
              ]


let symbolic_execute (state: exec_state) (ast: expr) : sym_store =
  let sym_store = Int.Table.create () in
  let trace expr st =
    Hashtbl.set sym_store ~key:(Ast.to_id expr) ~data:st
  in
  let init_set =
    state.env
    |> Symtable.keys
    |> SymSet.of_list
  in
  sym_exec ~trace state init_set ast;
  sym_store


(* -------------------- *)
(* Execution *)
(* -------------------- *)

(* For exec_state *)
let load_nothing _ _ = None
let store_nothing _ _ _ = ()

type engine =
  { trace : expr -> dval -> symtable -> unit
  ; trace_blank : string or_blank -> dval -> symtable -> unit
  ; ctx : context
}

let rec exec ~(engine: engine)
             ~(state: exec_state)
             (st: symtable)
             (expr: expr)
  : dval =
  let exe = exec ~engine ~state in
  let ctx = engine.ctx in
  let trace = engine.trace in
  let trace_blank = engine.trace_blank in

  let call (name: string) (id: id) (argvals: dval list) : dval =
    let fn = Libs.get_fn_exn state.user_fns name in
    (* equalize length *)
    let expected_length = List.length fn.parameters in
    let actual_length = List.length argvals in
    let argvals =
      if expected_length = actual_length
      then argvals
      else
        let actual = Printf.sprintf "%d arguments" actual_length in
        let expected = Printf.sprintf "%d arguments" expected_length in
        Exception.internal ~actual ~expected
            ("Incorrect number of args in fncall to " ^ name)
    in
    let args =
      fn.parameters
      |> List.map2_exn ~f:(fun dv (p: param) -> (p.name, dv)) argvals
      |> DvalMap.of_alist_exn
    in
    call_fn ~engine ~state name id fn args
    (* |> Log.pp ~f:Types.RuntimeT.show_dval "call result" *)
  in
  (* This is a super hacky way to inject params as the result of
   * pipelining using the `Thread` construct
   *
   * It's definitely not a good thing to be doing, as we're mutating
   * the ASTs exprs to inject dvals into them
   *
   * `Thread` as a separate construct in the AST as opposed to just
   * being a function application is probably the root cause of this.
   * Right now, we don't have function application in the language as
   * FnCall is the AST element that actually handles interacting with
   * the OCaml runtime to do useful work. We're going to need to make
   * this a functional language with functions-as-values and application
   * as a first-class concept sooner rather than later.
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
        (try
           call name id (param :: (List.map ~f:(exe st) exprs))
         with e ->
           (* making the error local looks better than making the whole
            * thread fail. *)
           Log.log_exception  "threaded_execution" state.execution_id e;
           Dval.exception_to_dval e)
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | Blank _ ->
        (match param with
         | DError _ -> DIncomplete (* dont repeat errors in threads *)
         | _ -> param)
      | _ ->
        let _ = exe st exp in (* calculate the results inside this
                                 regardless *)
        DIncomplete (* partial w/ exception, full with dincomplete, or option dval? *)
    in
    trace exp result st;
    result

  in

  let ignoreError e =
    match e with
    | DError _ -> DIncomplete
    | _ -> e in

  let value _ =
    (match expr with
     | Blank id -> DIncomplete

     | Flagged (id, msg, setting, l, r) ->
       (* Only compute l & r in Preview to avoid production side-effects *)
       (match ctx with
        | Preview ->
          let _ = exe st l in
          let _ = exe st r in
          ()
        | _ -> ()
       );

       let v = flatten_ff expr state.ff in
       exe st v

     | Filled (_, Let (lhs, rhs, body)) ->
       let bound = match lhs with
            | Filled (_, name) ->
              let data = exe st rhs in
              trace_blank lhs data st;
              String.Map.set ~key:name ~data:data st
            | Blank _ -> st
            | Flagged _ -> should_be_flat ()
       in exe bound body

     | Filled (_, Value s) ->
       Dval.parse s

     | Filled (_, ListLiteral exprs) ->
       exprs
       |> List.filter_map
         ~f:(function
             | Blank _ -> None
             | v -> Some (exe st v)
            )
       |> DList

     | Filled (_, ObjectLiteral pairs) ->
       pairs
       |> List.filter_map
         ~f:(function
             | (Filled (_, k), v) ->
               let expr = exe st v in
               Some (k, expr)
             | (_, v) ->
               let _ = exe st v in
               None
           )
       |> Dval.to_dobj

     | Filled (_, Variable name) ->
       (match Symtable.find st name with
        | None ->
          DError ("There is no variable named: " ^ name)
        | Some result -> ignoreError result)

     | Filled (id, FnCall (name, exprs)) ->
       let argvals = List.map ~f:(exe st) exprs in
       call name id argvals

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
       if ctx = Preview
       then
         (* Since we return a DBlock, it's contents may never be
          * executed. So first we execute with no context to get some
          * live values. *)
         let fake_st = Util.merge_left
                        (Symtable.singleton "var" DIncomplete)
                        st in
         let _ = exe fake_st body in ()
       else ();

       (* TODO: this will errror if the number of args and vars arent equal *)
       DBlock (fun args ->
           let varnames =
             vars
             |> List.map
               ~f:(fun v -> flatten_ff v state.ff)
             |> List.filter_map ~f:blank_to_content
           in
           let bindings = Symtable.of_alist_exn (List.zip_exn varnames args) in
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
          (match flatten_ff field state.ff with
           | Blank _ -> DIncomplete
           | Flagged _ -> should_be_flat ()
           | Filled (_, f) ->
             (match e with
              | Filled (_, Variable "request")
                when ctx = Preview
                  && equal_dval obj (PReq.to_dval PReq.sample_request) ->
                DIncomplete
              | _ ->
                (match Map.find o f with
                  | Some v -> v
                  | None -> DNull)))
        | DIncomplete -> DIncomplete
        | DError _ -> DIncomplete
        | x -> DError ("Can't access field of non-object: " ^ (Dval.to_repr x)))
    ) in
  (* Only catch if we're tracing *)
  let execed_value =
    if ctx = Real
    then value ()
    else
      try
        value ()
      with e ->
        Log.log_exception "exec_execution" state.execution_id e;
        Dval.exception_to_dval e
  in
  trace expr execed_value st;
  execed_value
  (* |> Log.pp "execed" ~f:(fun dv -> sexp_of_dval dv |> *)
  (*                                  Sexp.to_string) *)

and call_fn ~(engine:engine) ~(state: exec_state)
    (fnname: string) (id: id) (fn: fn) (args: dval_map) : dval =

  let paramsIncomplete args =
    List.exists args
      ~f:(fun x ->
          match x with
          | DIncomplete -> true
          | DError _ -> true
          | _ -> false)
  in

  match fn.func with
  | InProcess f ->
    let arglist = fn.parameters
                  |> List.map ~f:(fun (p: param) -> p.name)
                  |> List.map ~f:(DvalMap.find_exn args) in

    if paramsIncomplete arglist
    then DIncomplete
    else
      let executing_unsafe = not fn.preview_execution_safe
                             && (List.mem ~equal:(=) state.exe_fn_ids id
                                 || engine.ctx = Real)
      in
      if executing_unsafe
      then
        Log.infO "executing unsafe result" ~params:[ "fn", fnname
                                                   ; "ctx", Log.dump engine.ctx
                                                   ; "id", Log.dump id
                                                   ; "execution_id", Log.dump state.execution_id
                                                   ];

      let sfr_desc = (state.canvas_id, state.tlid, fnname, id) in
      let maybe_store_result result =
        if executing_unsafe
          (* TODO: add an execution ID here so that multiple requests
           * with the same parameter (from a user) don't pollute old
           * requests. *)
        then state.store_fn_result sfr_desc arglist result
        else ();
      in

      let state =
        { state with fail_fn = Some (Lib.fail_fn fnname fn arglist) }
      in

      let result =
        (try
           match engine.ctx with
           | Real ->
             f (state, arglist)
           | Preview ->
             if fn.preview_execution_safe || executing_unsafe
             then f (state, arglist)
             else
               (match state.load_fn_result sfr_desc arglist with
                | Some (result, _ts) -> result
                | _ -> DIncomplete)
         with
         | e ->
           (* After the rethrow, this gets eventually caught then shown
            * to the user as a Dark Internal Exception. It's an internal
            * exception because we didn't anticipate the problem, give
            * it a nice error message, etc. It'll appear in Rollbar as
            * "Unknown Err".  *)
           Exception.reraise_after e
             (fun bt ->
               maybe_store_result (Dval.exception_to_dval e);
             ))
      in
      maybe_store_result result;
      result


  | UserCreated body ->
    exec ~engine ~state args body
  | API f ->
      f args
      (* | TypeError args -> *)
      (*   let param_to_string (param: param) : string = *)
      (*     param.name *)
      (*     ^ (if param.optional then "?" else "") *)
      (*     ^ " : " *)
      (*     ^ (Dval.tipe_to_string param.tipe) *)
      (*   in *)
      (*   RT.error (fnname ^ " is missing a parameter") *)
      (*     ~expected:(fn.parameters *)
      (*                |> List.map ~f:param_to_string *)
      (*                |> String.concat ~sep:", ") *)
      (*     ~actual:DIncomplete *)


(* -------------------- *)
(* Analysis *)
(* -------------------- *)

(* Trace everything and save it *)
let analysis_engine value_store : engine =
  let trace expr dval st =
    Hashtbl.set value_store ~key:(Ast.to_id expr) ~data:dval
  in
  let trace_blank blank dval st =
    Hashtbl.set value_store ~key:(Ast.blank_to_id blank) ~data:dval
  in
  { trace = trace
  ; trace_blank = trace_blank
  ; ctx = Preview
  }

let execute_saving_intermediates (state : exec_state) (ast: expr)
  : (dval * dval_store) =
  Log.infO "Executing for intermediates"
    ~params:[ "tlid", show_tlid state.tlid
            ; "exe_fn_ids", Log.dump state.exe_fn_ids
            ; "execution_id", Log.dump state.execution_id
            ];
  let value_store = Int.Table.create () in
  let engine = analysis_engine value_store in
  (exec ~engine ~state state.env ast, value_store)

(* -------------------- *)
(* Execution *)
(* -------------------- *)

(* no tracing when running in prod *)
let server_execution_engine : engine =
  let empty_trace _ _ _ = () in
  { trace = empty_trace
  ; trace_blank = empty_trace
  ; ctx = Real
  }

let execute state env expr : dval =
  Log.infO "Executing for real" ~params:[ "tlid", show_tlid state.tlid
                                        ; "execution_id", Log.dump state.execution_id];
  exec env expr
    ~engine:server_execution_engine
    ~state

let handler_default_env (h: Handler.handler) : dval_map =
  match Handler.event_name_for h with
  | Some n ->
    n
    |> Http.route_variables
    |> List.map ~f:(fun k -> (k, DIncomplete))
    |> DvalMap.of_alist_exn
  | None -> DvalMap.empty

let with_defaults (h: Handler.handler) (env: symtable) : symtable =
  Util.merge_left env (handler_default_env h)

let execute_handler (state: exec_state) (h: Handler.handler) : dval =
  let env = with_defaults h state.env in
  execute state env h.ast

let environment_for_user_fn (ufn: user_fn) : dval_map =
  let param_to_dval (p: param) : dval =
    DIncomplete (* TODO(ian): we should trace these correctly *)
  in
  ufn.metadata.parameters
  |> List.filter_map ~f:ufn_param_to_param
  |> List.map ~f:(fun f -> (f.name, param_to_dval f))
  |> DvalMap.of_alist_exn


(* -------------------- *)
(* Run full analyses *)
(* -------------------- *)

let execute_handler_for_analysis (state : exec_state) (h : Handler.handler) :
    analysis =
  Log.infO "Handler for analysis"
    ~params:[ "tlid", show_tlid state.tlid
            ; "input", string_of_int state.input_cursor
            ; "execution_id", Log.dump state.execution_id];
  let default_env = with_defaults h state.env in
  let state = { state with env = default_env } in
  let traced_symbols =
    symbolic_execute state h.ast in
  let (ast_value, traced_values) =
    execute_saving_intermediates state h.ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = symtable_to_sym_list state.env
  }

let execute_function_for_analysis (state : exec_state) (f : user_fn) :
    analysis =
  Log.infO "Function for analysis"
    ~data:(show_tlid state.tlid)
    ~params:["execution_id", Log.dump state.execution_id];
  let traced_symbols =
    symbolic_execute state f.ast in
  let (ast_value, traced_values) =
    execute_saving_intermediates state f.ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = symtable_to_sym_list state.env
  }

