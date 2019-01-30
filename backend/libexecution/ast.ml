open Core_kernel
open Libcommon
open Types
open Types.RuntimeT
open Analysis_types
module RT = Runtime

(* -------------------- *)
(* Convert blanks *)
(* -------------------- *)
let blank_to_id (bo : 'a or_blank) : id =
  match bo with Filled (id, _) -> id | Blank id -> id


let rec blank_to_option (bo : 'a or_blank) : 'a option =
  match bo with Blank _ -> None | Filled (_, a) -> Some a


let is_blank (bo : 'a or_blank) : bool =
  match bo with Filled _ -> false | Blank _ -> true


let blank_map ~(f : 'a -> 'b) (bo : 'a or_blank) : 'b or_blank =
  match bo with Filled (id, a) -> Filled (id, f a) | Blank id -> Blank id


let blank_to_string (bo : string or_blank) : string =
  match bo with Filled (_, a) -> a | Blank _ -> "___"


(* -------------------- *)
(* Patterns *)
(* -------------------- *)
let rec pattern2expr p : expr =
  match p with
  | Blank id ->
      Blank id
  | Filled (id, PLiteral p) ->
      Filled (id, Value p)
  | Filled (id, PVariable p) ->
      Filled (id, Variable p)
  | Filled (id, PConstructor (_, _)) ->
      Blank id


(* -------------------- *)
(* AST traversal *)
(* -------------------- *)

(* Co-recursive. See example below. *)
let rec traverse ~(f : expr -> expr) (expr : expr) : expr =
  match expr with
  | Blank _ ->
      expr
  | Filled (id, nexpr) ->
      Filled
        ( id
        , match nexpr with
          | Value _ ->
              nexpr
          | Variable _ ->
              nexpr
          | Let (lhs, rhs, body) ->
              Let (lhs, f rhs, f body)
          | If (cond, ifbody, elsebody) ->
              If (f cond, f ifbody, f elsebody)
          | FnCall (name, exprs) ->
              FnCall (name, List.map ~f exprs)
          | FnCallSendToRail (name, exprs) ->
              FnCallSendToRail (name, List.map ~f exprs)
          | Lambda (vars, lexpr) ->
              Lambda (vars, f lexpr)
          | Thread exprs ->
              Thread (List.map ~f exprs)
          | FieldAccess (obj, field) ->
              FieldAccess (f obj, field)
          | ListLiteral exprs ->
              ListLiteral (List.map ~f exprs)
          | ObjectLiteral pairs ->
              ObjectLiteral (List.map ~f:(fun (k, v) -> (k, f v)) pairs)
          | FeatureFlag (msg, cond, a, b) ->
              FeatureFlag (msg, f cond, f a, f b)
          | Match (matchExpr, cases) ->
              Match (f matchExpr, List.map ~f:(fun (k, v) -> (k, f v)) cases)
          | Constructor (name, args) ->
              Constructor (name, List.map ~f args) )


(* Example usage of traverse. See also AST.ml *)
let rec example_traversal expr =
  match expr with
  | Blank _ ->
      Filled (Util.create_id (), Value "\"example\"")
  | expr ->
      traverse ~f:example_traversal expr


let rec set_expr ~(search : id) ~(replacement : expr) (expr : expr) : expr =
  let replace = set_expr ~search ~replacement in
  if search = blank_to_id expr then replacement else traverse ~f:replace expr


(* -------------------- *)
(* Execution *)
(* -------------------- *)

(* this is _why_ we're executing the AST, to allow us to not
 * emit certain side-effects (eg. DB writes) when showing previews *)
type context =
  | Preview
  | Real
[@@deriving eq, show, yojson]

type engine =
  { trace : expr -> dval -> symtable -> unit
  ; trace_blank : string or_blank -> dval -> symtable -> unit
  ; ctx : context }

let find_derrorrail (dvals : dval list) : dval option =
  List.find dvals ~f:Dval.is_errorrail


let should_send_to_rail (expr : nexpr) : bool =
  match expr with FnCallSendToRail _ -> true | _ -> false


let rec exec
    ~(engine : engine) ~(state : exec_state) (st : symtable) (expr : expr) :
    dval =
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
  let inject_param_and_execute (st : symtable) (param : dval) (exp : expr) :
      dval =
    let result =
      match exp with
      | Filled (id, Lambda _) ->
          let result = exe st exp in
          ( match result with
          | DBlock blk ->
              blk [param]
          | _ ->
              Exception.internal "Should have got a block" )
      | Filled (id, (FnCall (name, exprs) as fncall))
      | Filled (id, (FnCallSendToRail (name, exprs) as fncall)) ->
          let send_to_rail = should_send_to_rail fncall in
          call name id (param :: List.map ~f:(exe st) exprs) send_to_rail
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | Blank _ ->
          param
      | _ ->
          ignore (exe st exp) ;
          (* calculate the results inside this regardless *)
          DIncomplete
      (* partial w/ exception, full with dincomplete, or option dval? *)
    in
    trace exp result st ;
    result
  in
  let value _ =
    match expr with
    | Blank id ->
        DIncomplete
    | Filled (_, Let (lhs, rhs, body)) ->
        let data = exe st rhs in
        ( match data with
        | DErrorRail _ ->
            data
        | _ ->
            trace_blank lhs data st ;
            let bound =
              match lhs with
              | Filled (_, name) ->
                  DvalMap.set ~key:name ~data st
              | Blank _ ->
                  st
            in
            exe bound body )
    | Filled (_, Value s) ->
        Dval.parse_literal s
        |> Option.value ~default:(DError "Unparsable value")
    | Filled (_, ListLiteral exprs) ->
        exprs
        |> List.filter_map ~f:(function
               | Blank _ ->
                   None
               | v ->
                 ( match exe st v with
                 | DIncomplete ->
                     None (* ignore unfinished subexpr *)
                 | dv ->
                     Some dv ) )
        |> fun l -> find_derrorrail l |> Option.value ~default:(DList l)
    | Filled (_, ObjectLiteral pairs) ->
        pairs
        |> List.filter_map ~f:(function
               | Filled (_, k), v ->
                   let expr = exe st v in
                   ( match expr with
                   | DIncomplete ->
                       None (* ignore unfinished subexpr *)
                   | _ ->
                       Some (k, expr) )
               | _, v ->
                   ignore (exe st v) ;
                   None )
        |> fun ps ->
        ps
        |> List.map ~f:Tuple.T2.get2
        |> find_derrorrail
        |> Option.value ~default:(Dval.to_dobj ps)
    | Filled (_, Variable name) ->
      ( match Symtable.find st name with
      | None ->
          DError ("There is no variable named: " ^ name)
      | Some other ->
          other )
    | Filled (id, FnCallSendToRail (name, exprs)) ->
        let argvals = List.map ~f:(exe st) exprs in
        call name id argvals true
    | Filled (id, FnCall (name, exprs)) ->
        let argvals = List.map ~f:(exe st) exprs in
        call name id argvals false
    | Filled (id, If (cond, ifbody, elsebody)) ->
      ( match ctx with
      | Preview ->
          (* In the case of a preview trace execution, we want the 'if'
           * expression as a whole to evaluate to its correct value -- but we
           * also want preview values for _all_ sides of the if *)
          let ifresult = exe st ifbody in
          let elseresult = exe st elsebody in
          ( match exe st cond with
          | DBool false | DNull ->
              elseresult
          | DIncomplete ->
              DIncomplete
          | DError _ ->
              DError "Expected boolean, got error"
          | DErrorRail _ as er ->
              er
          | _ ->
              ifresult )
      | Real ->
        (* In the case of a 'real' evaluation, we shouldn't do unneccessary
           * work and as such should follow the proper evaluation semantics *)
        ( match exe st cond with
        (* only false and 'null' are falsey *)
        | DBool false | DNull ->
            exe st elsebody
        | DIncomplete ->
            DIncomplete
        | DError _ ->
            DError "Expected boolean, got error"
        | DErrorRail _ as er ->
            er
        | _ ->
            exe st ifbody ) )
    | Filled (id, FeatureFlag (_, cond, oldcode, newcode)) ->
      (* True gives newexpr, unlike in If statements *)
      
      (* In If statements, we use a false/null as false, and anything else is
        * true. But this won't work for feature flags. If statements are built
        * as you build you code, with no existing users. But feature flags are
        * created when you have users and don't want to break your code. As a
        * result, anything that isn't an explicitly signalling to use the new
        * code, should use the old code:
        * - errors should be ignored: use old code
        * - incompletes should be ignored: use old code
        * - errorrail should not be propaged: use old code
        * - values which are "truthy" in if statements are not truthy here:
        * imagine you are writing the FF cond and you get a list or object,
        * and you're about to do some other work on it. Should we immediately
        * start serving the new code to all your traffic? No. So only `true`
        * gets new code.
        *)
      ( match ctx with
      | Preview ->
          (* In the case of a preview trace execution, we want the expression
           * as a whole to evaluate to its correct value -- but we also want
           * preview values for both sides *)
          let newresult = exe st newcode in
          let oldresult = exe st oldcode in
          let condresult =
            try
              (* under no circumstances should this cause code to fail *)
              exe st cond
            with e -> Dval.exception_to_dval e
          in
          ( match condresult with
          | DBool true ->
              newresult
          | DIncomplete ->
              oldresult
          | DError _ ->
              oldresult
          | DErrorRail _ ->
              oldresult
          | _ ->
              oldresult )
      | Real ->
          (* In the case of a 'real' evaluation, we shouldn't do unneccessary
           * work and as such should follow the proper evaluation semantics *)
          let condresult =
            try
              (* under no circumstances should this cause code to fail *)
              exe st cond
            with e -> DBool false
          in
          ( match condresult with
          (* only false and 'null' are falsey *)
          | DBool true ->
              exe st newcode
          | DErrorRail _ ->
              exe st oldcode
          | DIncomplete ->
              exe st oldcode
          | DError _ ->
              exe st oldcode
          | _ ->
              exe st oldcode ) )
    | Filled (id, Lambda (vars, body)) ->
        if ctx = Preview
        then
          (* Since we return a DBlock, it's contents may never be
          * executed. So first we execute with no context to get some
          * live values. *)
          let fake_st =
            Util.merge_left (Symtable.singleton "var" DIncomplete) st
          in
          ignore (exe fake_st body)
        else () ;
        (* TODO: this will error if the number of args and vars arent equal *)
        DBlock
          (fun args ->
            let filled = List.filter ~f:(Fn.compose not is_blank) vars in
            List.iter (List.zip_exn filled args) ~f:(fun (var, dv) ->
                trace_blank var dv st ) ;
            let varnames = List.filter_map ~f:blank_to_option vars in
            let bindings =
              Symtable.of_alist_exn (List.zip_exn varnames args)
            in
            let new_st = Util.merge_left bindings st in
            exe new_st body )
    | Filled (id, Thread exprs) ->
      (* For each expr, execute it, and then thread the previous result thru *)
      ( match exprs with
      | e :: es ->
          let fst = exe st e in
          List.fold_left es ~init:fst ~f:(fun previous nxt ->
              let result = inject_param_and_execute st previous nxt in
              match result with
              | DIncomplete ->
                  previous
              (* let execution through *)
              (* DErrorRail is handled by inject_param_and_execute *)
              | _ ->
                  result )
      | [] ->
          DIncomplete )
    | Filled (id, Match (matchExpr, cases)) ->
        let rec matches dv (pat, e) =
          let result =
            match pat with
            | Filled (_, PLiteral l) when Dval.parse_literal l = Some dv ->
                Some (e, [])
            | Filled (_, PVariable v) ->
                Some (e, [(v, dv)])
            | Filled (_, PConstructor ("Just", [p])) ->
              ( match dv with
              | DOption (OptJust v) ->
                  matches v (p, e)
              | _ ->
                  None )
            | Filled (_, PConstructor ("Ok", [p])) ->
              ( match dv with
              | DResult (ResOk v) ->
                  matches v (p, e)
              | _ ->
                  None )
            | Filled (_, PConstructor ("Error", [p])) ->
              ( match dv with
              | DResult (ResError v) ->
                  matches v (p, e)
              | _ ->
                  None )
            | Filled (_, PConstructor ("Nothing", [])) ->
                if dv = DOption OptNothing then Some (e, []) else None
            | _ ->
                None
          in
          if Option.is_some result then trace (pattern2expr pat) dv st ;
          result
        in
        let matchVal = exe st matchExpr in
        let matched = List.filter_map ~f:(matches matchVal) cases in
        ( match matched with
        | [] ->
            DIncomplete
        | (e, vars) :: _ ->
            let newVars = DvalMap.of_alist_exn vars in
            let newSt = Util.merge_left newVars st in
            exe newSt e )
    | Filled (id, FieldAccess (e, field)) ->
        let obj = exe st e in
        let result =
          match obj with
          | DObj o ->
            ( match field with
            | Blank _ ->
                DIncomplete
            | Filled (_, f) ->
              (match Map.find o f with Some v -> v | None -> DNull) )
          | DIncomplete ->
              DIncomplete
          | DErrorRail _ ->
              obj
          | x ->
              DError ("Can't access field of non-object: " ^ Dval.to_repr x)
        in
        trace_blank field result st ;
        result
    | Filled (_, Constructor (name, args)) ->
      ( match (name, args) with
      | Filled (_, "Nothing"), [] ->
          DOption OptNothing
      | Filled (_, "Just"), [arg] ->
          DOption (OptJust (exe st arg))
      | Filled (_, "Ok"), [arg] ->
          DResult (ResOk (exe st arg))
      | Filled (_, "Error"), [arg] ->
          DResult (ResError (exe st arg))
      | _ ->
          DError "Invalid construction option" )
  in
  let execed_value = value () in
  trace expr execed_value st ;
  execed_value


(* |> Log.pp "execed" ~f:(fun dv -> sexp_of_dval dv *)
(* |> Sexp.to_string) *)
and call_fn
    ~(engine : engine)
    ~(state : exec_state)
    (name : string)
    (id : id)
    (argvals : dval list)
    (send_to_rail : bool) : dval =
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
      Exception.internal
        ~actual
        ~expected
        ("Incorrect number of args in fncall to " ^ name)
  in
  let args =
    fn.parameters
    |> List.map2_exn ~f:(fun dv (p : param) -> (p.name, dv)) argvals
    |> DvalMap.of_alist_exn
  in
  match find_derrorrail (DvalMap.data args) with
  | Some er ->
      er
  | None ->
      let result =
        exec_fn ~engine ~state name id fn args |> Dval.unwrap_from_errorrail
      in
      if send_to_rail
      then
        match result with
        | DOption (OptJust v) ->
            v
        | DResult (ResOk v) ->
            v
        (* There should only be DOptions and DResults here, but hypothetically we got
        * something else, they would go on the error rail too.  *)
        | other ->
            DErrorRail other
      else result


and exec_fn
    ~(engine : engine)
    ~(state : exec_state)
    (fnname : string)
    (id : id)
    (fn : fn)
    (args : dval_map) : dval =
  let paramsIncomplete args = List.exists args ~f:(( = ) DIncomplete) in
  let paramsErroneous args =
    List.exists args ~f:(function
        | DError _ when String.Caseless.equal fnname "Bool::isError" ->
            false
        | DError _ ->
            true
        | _ ->
            false )
  in
  let arglist =
    fn.parameters
    |> List.map ~f:(fun (p : param) -> p.name)
    |> List.map ~f:(DvalMap.find_exn args)
  in
  let sfr_desc = (state.tlid, fnname, id) in
  match fn.func with
  | NotClientAvailable ->
    ( match state.load_fn_result sfr_desc arglist with
    | Some (result, _ts) ->
        result
    | _ ->
        DIncomplete )
  | InProcess f ->
      if paramsIncomplete arglist
      then DIncomplete
      else if paramsErroneous arglist
      then DError "Fn called with an error as an argument"
      else if engine.ctx = Preview && not fn.preview_execution_safe
      then
        match state.load_fn_result sfr_desc arglist with
        | Some (result, _ts) ->
            result
        | _ ->
            DIncomplete
      else
        let state =
          {state with fail_fn = Some (Lib.fail_fn fnname fn arglist)}
        in
        let result =
          try f (state, arglist) with
          | Exception.DarkException de as e when de.tipe = UserCode ->
              (* These are exceptions that come from an RT.error, which is all
            * usercode problems. Non user-code problems should use different
            * exception types.
            *)
              Dval.exception_to_dval e
          | e ->
              (* After the rethrow, this gets eventually caught then shown to the
            * user as a Dark Internal Exception. It's an internal exception
            * because we didn't anticipate the problem, give it a nice error
            * message, etc. It'll appear in Rollbar as "Unknown Err". To remedy
            * this, give it a nice exception via RT.error.  *)
              Exception.reraise e
        in
        (* there's no point storing data we'll never ask for *)
        if not fn.preview_execution_safe
        then state.store_fn_result sfr_desc arglist result ;
        result
  | UserCreated (tlid, body) ->
      (* TODO: unify with InProcess, esp paramsIncomplete and paramsErroneous *)
      let args_with_dbs =
        let db_dvals =
          state.dbs
          |> List.filter_map ~f:(fun db ->
                 match db.name with
                 | Filled (_, name) ->
                     Some (name, DDB name)
                 | Blank _ ->
                     None )
          |> DvalMap.of_alist_exn
        in
        Util.merge_left db_dvals args
      in
      state.store_fn_arguments tlid args ;
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
    Hashtbl.set value_store ~key:(blank_to_id expr) ~data:dval
  in
  let trace_blank blank dval st =
    Hashtbl.set value_store ~key:(blank_to_id blank) ~data:dval
  in
  {trace; trace_blank; ctx = Preview}


let execute_saving_intermediates
    ~(input_vars : input_vars) (state : exec_state) (ast : expr) :
    dval * dval_store =
  Log.infO
    "Executing for intermediates"
    ~params:
      [ ("tlid", show_tlid state.tlid)
      ; ("execution_id", Log.dump state.execution_id) ] ;
  let value_store = IDTable.create () in
  let engine = analysis_engine value_store in
  let st = input_vars2symtable input_vars in
  (exec ~engine ~state st ast, value_store)


(* -------------------- *)
(* Execution *)
(* -------------------- *)

(* no tracing when running in prod *)
let server_execution_engine : engine =
  let empty_trace _ _ _ = () in
  {trace = empty_trace; trace_blank = empty_trace; ctx = Real}


let execute_ast ~input_vars (state : exec_state) expr : dval =
  Log.infO
    "Executing for real"
    ~params:
      [ ("tlid", show_tlid state.tlid)
      ; ("execution_id", Log.dump state.execution_id) ] ;
  exec
    (input_vars2symtable input_vars)
    expr
    ~engine:server_execution_engine
    ~state


let execute_userfn
    (state : exec_state) (name : string) (id : id) (args : dval list) : dval =
  call_fn name id args false ~engine:server_execution_engine ~state


let execute_fn
    (state : exec_state) (name : string) (id : id) (args : dval list) : dval =
  call_fn name id args false ~engine:server_execution_engine ~state
