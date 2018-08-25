open Core_kernel
open Libcommon

open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT

module RT = Runtime
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
           |> Yojson.Safe.to_string
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
        (string_of_id id, f v))
      alist)

type dval_store = dval IDTable.t

let dval_store_to_yojson (ds : dval_store) : Yojson.Safe.json =
  ht_to_json_dict ds ~f:livevalue_dval_to_yojson


(* Symstore - save available varnames at each point *)
module SymSet = String.Set
type sym_set = SymSet.t
type sym_store = sym_set IDTable.t

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
    ignore
      ((match expr with
        | Blank _ -> ()
        | Filled (_, Value s) -> ()
        | Filled (_, Variable name) -> ()

        | Filled (_, Let (lhs, rhs, body)) ->
          let bound = match lhs with
            | Filled (_, name) ->
              sexe st rhs;
              SymSet.add st name
            | Blank _ -> st
          in sexe bound body

        | Filled (_, FnCall (name, exprs))
        | Filled (_, FnCallSendToRail (name, exprs)) ->
          List.iter ~f:(sexe st) exprs

        | Filled (_, If (cond, ifbody, elsebody))
        | Filled (_, FeatureFlag(_, cond, elsebody, ifbody)) ->
          sexe st cond;
          sexe st ifbody;
          sexe st elsebody;

        | Filled (_, Lambda (vars, body)) ->
          let new_st =
            vars
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
          |> List.iter ~f:(sexe st)));
    trace expr st
  with
  | e ->
    let bt = Exception.get_backtrace () in
    Log.erroR "exception_during_symexec" ~bt
      ~params:["exception", Exception.to_string e
              ;"execution_id", Log.dump state.execution_id
              ]


let symbolic_execute (state: exec_state) (ast: expr)
    ~(input_vars: symtable)
  : sym_store =
  let sym_store = IDTable.create () in
  let trace expr st =
    Hashtbl.set sym_store ~key:(Ast.to_id expr) ~data:st
  in
  let init_set =
    input_vars
    |> Symtable.keys
    |> SymSet.of_list
  in
  sym_exec ~trace state init_set ast;
  sym_store


(* -------------------- *)
(* Execution *)
(* -------------------- *)

(* For exec_state *)
let load_no_results _ _ = None
let store_no_results _ _ _ = ()
let load_no_arguments _ = []
let store_no_arguments _ _ = ()

type engine =
  { trace : expr -> dval -> symtable -> unit
  ; trace_blank : string or_blank -> dval -> symtable -> unit
  ; ctx : context
}

let find_derrorrail (dvals : dval list) : dval option =
  List.find dvals ~f:Dval.is_errorrail

let should_send_to_rail (expr: nexpr) : bool =
  match expr with
  | FnCallSendToRail _ -> true
  | _ -> false

let rec exec ~(engine: engine)
             ~(state: exec_state)
             (st: symtable)
             (expr: expr)
  : dval =
  let exe = exec ~engine ~state in
  let call = call_fn ~engine ~state in
  let ctx = engine.ctx in
  let trace = engine.trace in
  let trace_blank = engine.trace_blank in

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
         | _ -> Exception.internal "Should have got a block")
      | Filled (id, (FnCall (name, exprs) as fncall))
      | Filled (id, (FnCallSendToRail (name, exprs) as fncall)) ->
        let send_to_rail = should_send_to_rail fncall in
        (try
           call name id (param :: (List.map ~f:(exe st) exprs)) send_to_rail
         with e ->
           (* making the error local looks better than making the whole
            * thread fail. *)
           Log.log_exception
             ~pp:Exception.to_string
             "threaded_execution" (Types.show_id state.execution_id) e;
           Dval.exception_to_dval e)
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | Blank _ -> param
      | _ ->
        ignore (exe st exp); (* calculate the results inside this regardless *)
        DIncomplete (* partial w/ exception, full with dincomplete, or option dval? *)
    in
    trace exp result st;
    result

  in

  let value _ =
    (match expr with
     | Blank id -> DIncomplete

     | Filled (_, Let (lhs, rhs, body)) ->
       let data = exe st rhs in
       (match data with
        | DErrorRail _ -> data
        | _ ->
          trace_blank lhs data st;
          let bound =
            (match lhs with
            | Filled (_, name) ->
              String.Map.set ~key:name ~data:data st
            | Blank _ ->
              st)
          in
          exe bound body)

     | Filled (_, Value s) ->
       Dval.parse s
       |> Option.value ~default:(DError "Unparsable value")

     | Filled (_, ListLiteral exprs) ->
       exprs
       |> List.filter_map
         ~f:(function
             | Blank _ -> None
             | v ->
               match exe st v with
               | DIncomplete -> None (* ignore unfinished subexpr *)
               | dv -> Some dv)
       |> fun l ->
            find_derrorrail l
            |> Option.value ~default:(DList l)

     | Filled (_, ObjectLiteral pairs) ->
       pairs
       |> List.filter_map
         ~f:(function
             | (Filled (_, k), v) ->
               let expr = exe st v in
               (match expr with
                | DIncomplete -> None (* ignore unfinished subexpr *)
                | _ -> Some (k, expr))
             | (_, v) ->
               ignore (exe st v);
               None
           )
       |> fun ps ->
          ps
          |> List.map ~f:Tuple.T2.get2
          |> find_derrorrail
          |> Option.value ~default:(Dval.to_dobj ps)

     | Filled (_, Variable name) ->
       (match Symtable.find st name with
        | None -> DError ("There is no variable named: " ^ name)
        | Some other -> other)

     | Filled (id, FnCallSendToRail (name, exprs)) ->
       let argvals = List.map ~f:(exe st) exprs in
       call name id argvals true

     | Filled (id, FnCall (name, exprs)) ->
       let argvals = List.map ~f:(exe st) exprs in
       call name id argvals false

     | Filled (id, If (cond, ifbody, elsebody))
     | Filled (id, FeatureFlag (_, cond, elsebody, ifbody)) ->
       (match ctx with
        | Preview ->
          (* In the case of a preview trace execution, we want the 'if' expression as
           * a whole to evaluate to its correct value -- but we also want preview values
           * for _all_ sides of the if *)
          (match exe st cond with
           | DBool false | DNull ->
             (* execute the positive side just for the side-effect *)
             ignore (exe st ifbody);
             exe st elsebody
           | DIncomplete ->
             ignore (exe st ifbody);
             ignore (exe st elsebody);
             DIncomplete
           | DError _ ->
             ignore (exe st ifbody);
             ignore (exe st elsebody);
             DError "Expected boolean, got error"
           | DErrorRail _ as er ->
             ignore (exe st ifbody);
             ignore (exe st elsebody);
             er
           | _ ->
             (* execute the negative side just for the side-effect *)
             ignore (exe st elsebody);
             exe st ifbody)
        | Real ->
          (* In the case of a 'real' evaluation, we shouldn't do unneccessary work and
           * as such should follow the proper evaluation semantics *)
          (match exe st cond with
           (* only false and 'null' are falsey *)
           | DBool false | DNull -> exe st elsebody
           | DIncomplete -> DIncomplete
           | DErrorRail _ as er -> er
           | DError _ -> DError "Expected boolean, got error"
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
         ignore (exe fake_st body)
       else ();

       (* TODO: this will error if the number of args and vars arent equal *)
       DBlock (fun args ->
           let varnames = List.filter_map ~f:blank_to_content vars in
           let bindings = Symtable.of_alist_exn (List.zip_exn varnames args) in
           let new_st = Util.merge_left bindings st in
           exe new_st body)

     | Filled (id, Thread exprs) ->
       (* For each expr, execute it, and then thread the previous result thru *)
       (match exprs with
        | e :: es ->
          let fst = exe st e in
          List.fold_left es
            ~init:fst
            ~f:(fun previous nxt ->
                let result = inject_param_and_execute st previous nxt in
                match result with
                | DIncomplete -> previous (* let execution through *)
                (* DErrorRail is handled by inject_param_and_execute *)
                | _ -> result)
        | [] -> DIncomplete)

     | Filled (id, FieldAccess (e, field)) ->
       let obj = exe st e in
       let result =
         (match obj with
          | DObj o ->
            (match field with
             | Blank _ -> DIncomplete
             | Filled (_, f) ->
               (match Map.find o f with
                | Some v -> v
                | None -> DNull))
          | DIncomplete -> DIncomplete
          | DErrorRail _ -> obj
          | x -> DError ("Can't access field of non-object: " ^ (Dval.to_repr x)))
        in
        trace_blank field result st;
        result
    ) in
  (* Only catch if we're tracing *)
  let execed_value =
    if ctx = Real
    then value ()
    else
      try
        value ()
      with e ->
        Log.log_exception
             ~pp:Exception.to_string
             "exec_execution" (Types.show_id state.execution_id) e;
        Dval.exception_to_dval e
  in
  trace expr execed_value st;
  execed_value
  (* |> Log.pp "execed" ~f:(fun dv -> sexp_of_dval dv *)
  (* |> Sexp.to_string) *)

and call_fn ~(engine:engine) ~(state: exec_state) (name: string) (id: id) (argvals: dval list) (send_to_rail: bool) : dval =
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

  match find_derrorrail (DvalMap.data args) with
  | Some er -> er
  | None ->
    let result = exec_fn ~engine ~state name id fn args
                 |> Dval.unwrap_from_errorrail
    in
    if send_to_rail
    then
      (match result with
       | DOption (OptJust v) -> v
       (* There should only be DOptions here, but hypothetically we got
        * something else, they would go on the error rail too.  *)
       | other -> DErrorRail other)
    else
      result

and exec_fn ~(engine:engine) ~(state: exec_state)
    (fnname: string) (id: id) (fn: fn) (args: dval_map) : dval =

  let paramsIncomplete args = List.exists args ~f:((=) DIncomplete) in
  let paramsErroneous args =
    List.exists args
      ~f:(function
          (* HACK: going to hardcode this special case because I don't trust myself
           * to enumerate the consequences of making all DErrors `complete`. In general
           * we have an 'incomplete problem', especially intersecting with production
           * errors. it seems to me that it is something with simply side-step with
           * a proper type-system so I hope this small hack is enough to tide
           * us over relatively safely until then *)
          | DError _ when String.Caseless.equal fnname "Bool::isError" -> false
          | DError _  -> true
          | _ -> false)
  in

  match fn.func with
  | NotClientAvailable -> DIncomplete
  | InProcess f ->
    let arglist = fn.parameters
                  |> List.map ~f:(fun (p: param) -> p.name)
                  |> List.map ~f:(DvalMap.find_exn args) in

    if paramsIncomplete arglist
    then DIncomplete
    else if paramsErroneous arglist
    then DError "Fn called with an error as an argument"
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
           if engine.ctx = Real || fn.preview_execution_safe || executing_unsafe
           then f (state, arglist)
           else
             (match state.load_fn_result sfr_desc arglist with
              | Some (result, _ts) -> result
              | _ -> DIncomplete)
         with
         | Exception.DarkException de as e when de.tipe = UserCode ->
           (* These are exceptions that come from an RT.error, which is all
            * usercode problems. Non user-code problems should use different
            * exception types.
            *)
           let result = Dval.exception_to_dval e in
           maybe_store_result result;
           result
         | e ->
           (* After the rethrow, this gets eventually caught then shown to the
            * user as a Dark Internal Exception. It's an internal exception
            * because we didn't anticipate the problem, give it a nice error
            * message, etc. It'll appear in Rollbar as "Unknown Err". To remedy
            * this, give it a nice exception via RT.error.  *)
           Exception.reraise e)
      in
      maybe_store_result result;
      result


  | UserCreated (tlid, body) ->
    (* TODO: unify with InProcess, esp paramsIncomplete and paramsErroneous *)
    let args_with_dbs =
      let db_dvals =
        state.dbs
        |> List.map ~f:(fun db -> (db.name, DDB db))
        |> DvalMap.of_alist_exn
      in
      Util.merge_left
        (db_dvals)
        (args)
    in

    let fn_clicked = List.mem ~equal:(=) state.exe_fn_ids id in
    if engine.ctx = Real || fn_clicked
    then state.store_fn_arguments (state.canvas_id, tlid) args;

    exec ~engine ~state args_with_dbs body


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
    ~(input_vars: symtable)
  : (dval * dval_store) =
  Log.infO "Executing for intermediates"
    ~params:[ "tlid", show_tlid state.tlid
            ; "exe_fn_ids", Log.dump state.exe_fn_ids
            ; "execution_id", Log.dump state.execution_id
            ];
  let value_store = IDTable.create () in
  let engine = analysis_engine value_store in
  (exec ~engine ~state input_vars ast, value_store)

let input_values st =
  st
  |> Map.to_alist
  |> List.filter ~f:(fun (k,v) -> Dval.tipe_of v <> TDB)
  |> List.map ~f:(Tuple.T2.map_snd ~f:dval_to_livevalue)



(* -------------------- *)
(* Environments *)
(* -------------------- *)
let handler_default_env (h: handler) : dval_map =
  match Handler.event_name_for h with
  | Some n ->
    n
    |> Http.route_variables
    |> List.map ~f:(fun k -> (k, DIncomplete))
    |> DvalMap.of_alist_exn
  | None -> DvalMap.empty

let with_defaults (h: handler) (env: symtable) : symtable =
  Util.merge_left env (handler_default_env h)

let environment_for_user_fn (ufn: user_fn) : dval_map =
  let param_to_dval (p: param) : dval =
    DIncomplete
  in
  ufn.metadata.parameters
  |> List.filter_map ~f:ufn_param_to_param
  |> List.map ~f:(fun f -> (f.name, param_to_dval f))
  |> DvalMap.of_alist_exn


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

let execute_ast (state : exec_state) env expr : dval =
  Log.infO "Executing for real"
    ~params:[ "tlid", show_tlid state.tlid
            ; "execution_id", Log.dump state.execution_id];
  exec env expr
    ~engine:server_execution_engine
    ~state

let execute_userfn (state: exec_state) (name:string) (id:id) (args: dval list) : dval =
  call_fn name id args false
    ~engine:server_execution_engine ~state

(* -------------------- *)
(* Run full analyses *)
(* -------------------- *)


let execute_handler_for_analysis ~(input_vars: symtable)
    (state : exec_state) (h : handler)
  : analysis =
  Log.infO "Handler for analysis"
    ~params:[ "tlid", show_tlid state.tlid
            ; "execution_id", Log.dump state.execution_id];
  let input_vars = with_defaults h input_vars in
  let traced_symbols =
    symbolic_execute state ~input_vars h.ast in
  let (ast_value, traced_values) =
    execute_saving_intermediates state ~input_vars h.ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = input_values input_vars
  }

let execute_user_fn_for_analysis ~(input_vars: symtable)
    (state : exec_state) (f : user_fn)
    : analysis =
  Log.infO "Function for analysis"
    ~data:(show_tlid state.tlid)
    ~params:["execution_id", Log.dump state.execution_id];
  let traced_symbols =
    symbolic_execute state ~input_vars f.ast in
  let (ast_value, traced_values) =
    execute_saving_intermediates state ~input_vars f.ast in
  { ast_value = dval_to_livevalue ast_value
  ; live_values = traced_values
  ; available_varnames = traced_symbols
  ; input_values = input_values input_vars
  }
