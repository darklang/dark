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
  match bo with Filled (id, _) -> id | Partial (id, _) | Blank id -> id


let rec blank_to_option (bo : 'a or_blank) : 'a option =
  match bo with Partial _ | Blank _ -> None | Filled (_, a) -> Some a


let is_blank (bo : 'a or_blank) : bool =
  match bo with Filled _ -> false | Partial _ | Blank _ -> true


let blank_map ~(f : 'a -> 'b) (bo : 'a or_blank) : 'b or_blank =
  match bo with
  | Filled (id, a) ->
      Filled (id, f a)
  | Partial (id, v) ->
      Partial (id, v)
  | Blank id ->
      Blank id


let blank_to_string (bo : string or_blank) : string =
  match bo with Filled (_, a) -> a | Partial _ | Blank _ -> "___"


(* -------------------- *)
(* Patterns *)
(* -------------------- *)
let rec pattern2expr p : expr =
  match p with
  | Partial (id, v) ->
      Partial (id, v)
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
  | Partial _ | Blank _ ->
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
              Constructor (name, List.map ~f args)
          | FluidPartial (name, old_val) ->
              FluidPartial (name, f old_val)
          | FluidRightPartial (name, old_val) ->
              FluidRightPartial (name, f old_val) )


(* Example usage of traverse. See also AST.ml *)
let rec example_traversal expr =
  match expr with
  | Partial _ | Blank _ ->
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
  ; trace_tlid : tlid -> unit
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
              DError "Internal type error: lambda did not produce a block" )
      | Filled (id, (FnCall (name, exprs) as fncall))
      | Filled (id, (FnCallSendToRail (name, exprs) as fncall)) ->
          let send_to_rail = should_send_to_rail fncall in
          call name id (param :: List.map ~f:(exe st) exprs) send_to_rail
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | Partial _ | Blank _ ->
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
    | Partial _ ->
        DIncomplete
    | Filled (_, FluidPartial (_, expr))
    | Filled (_, FluidRightPartial (_, expr)) ->
        exe st expr
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
                  DvalMap.insert ~key:name ~value:data st
              | Partial _ | Blank _ ->
                  st
            in
            exe bound body )
    | Filled (_, Value s) ->
        Dval.parse_literal s
        |> Option.value ~default:(DError "Unparsable value")
    | Filled (_, ListLiteral exprs) ->
        (* We ignore incompletes but not error rail. Other places that lists
     are created propagate incompletes instead of ignoring *)
        exprs
        |> List.filter_map ~f:(function
               | Partial _ | Blank _ ->
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
        |> List.filter_map ~f:(fun (k, v) ->
               match (k, v) with
               | Filled (_, keyname), v ->
                   let expr = exe st v in
                   trace_blank k expr st ;
                   if expr = DIncomplete
                   then (* ignore unfinished subexpr *)
                     None
                   else Some (keyname, expr)
               | _, v ->
                   ignore (exe st v) ;
                   None )
        |> fun ps ->
        ps
        |> List.map ~f:Tuple.T2.get2
        |> find_derrorrail
        |> Option.value ~default:(Dval.to_dobj_exn ps)
    | Filled (_, Variable name) ->
      ( match (Symtable.get st ~key:name, ctx) with
      | None, Preview ->
          (* The trace is wrong/we have a bug --
           * we guarantee to users that variables they can lookup have been
           * bound. However, we shouldn't crash out here when running analysis
           * because it gives a horrible user experience *)
          DIncomplete
      | None, Real ->
          DError ("There is no variable named: " ^ name)
      | Some other, _ ->
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
            (* If one of the args is fakeCF, return it instead of executing.
             * This is the same behaviour as in fn calls. *)
            let fakecf = List.find args ~f:Dval.is_fake_cf in
            match fakecf with
            | Some dv ->
                dv
            | None ->
                let filled = List.filter ~f:(Fn.compose not is_blank) vars in
                List.iter (List.zip_exn filled args) ~f:(fun (var, dv) ->
                    trace_blank var dv st ) ;
                let new_st =
                  vars
                  |> List.filter_map ~f:blank_to_option
                  |> (fun varnames -> List.zip_exn varnames args)
                  |> Symtable.from_list_exn
                  |> fun bindings -> Util.merge_left bindings st
                in
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
            let newVars = DvalMap.from_list vars in
            let newSt = Util.merge_left newVars st in
            exe newSt e )
    | Filled (id, FieldAccess (e, field)) ->
        let obj = exe st e in
        let result =
          match obj with
          | DObj o ->
            ( match field with
            | Partial _ | Blank _ ->
                DIncomplete
            | Filled (_, f) ->
              (match Map.find o f with Some v -> v | None -> DNull) )
          | DIncomplete ->
              DIncomplete
          | DErrorRail _ ->
              obj
          | x ->
              DError
                ( "Attempting to access of a field of something that isn't an
object but is a "
                ^ (x |> Dval.tipe_of |> Dval.tipe_to_string)
                ^ "" )
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
  let fn = Libs.get_fn state.user_fns name in
  match find_derrorrail argvals with
  | Some er ->
      er
  | None ->
      let result =
        match fn with
        (* Functions which aren't implemented in the client may have results
   * available, otherwise they just return incomplete. *)
        | None ->
            let sfr_desc = (state.tlid, name, id) in
            ( match state.load_fn_result sfr_desc argvals with
            | Some (result, _ts) ->
                result
            | _ ->
                DIncomplete )
        | Some fn ->
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
              |> DvalMap.from_list_exn
            in
            exec_fn ~engine ~state name id fn args
      in
      if send_to_rail
      then
        match Dval.unwrap_from_errorrail result with
        | DOption (OptJust v) ->
            v
        | DResult (ResOk v) ->
            v
        | DIncomplete ->
            DIncomplete
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
    |> List.filter_map ~f:(fun key -> DvalMap.get ~key args)
  in
  let sfr_desc = (state.tlid, fnname, id) in
  if paramsIncomplete arglist
  then DIncomplete
  else if paramsErroneous arglist
  then DError "Fn called with an error as an argument"
  else
    match fn.func with
    | InProcess f ->
        if engine.ctx = Preview && not fn.preview_execution_safe
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
            | Exception.DarkException de as e when de.tipe = Code ->
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
      ( match
          Type_checker.check_function_call ~user_tipes:state.user_tipes fn args
        with
      | Ok () ->
          let args_with_dbs =
            let db_dvals =
              state.dbs
              |> List.filter_map ~f:(fun db ->
                     match db.name with
                     | Filled (_, name) ->
                         Some (name, DDB name)
                     | Partial _ | Blank _ ->
                         None )
              |> DvalMap.from_list
            in
            Util.merge_left db_dvals args
          in
          state.store_fn_arguments tlid args ;
          engine.trace_tlid tlid ;
          let result = exec ~engine ~state args_with_dbs body in
          state.store_fn_result sfr_desc arglist result ;
          Dval.unwrap_from_errorrail result
      | Error errs ->
          let error_msgs =
            errs
            |> List.map ~f:Type_checker.Error.to_string
            |> String.concat ~sep:", "
          in
          DError ("Type error(s) in function parameters: " ^ error_msgs) )
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
let analysis_engine value_store tlid_store : engine =
  let trace expr dval st =
    Hashtbl.set value_store ~key:(blank_to_id expr) ~data:dval
  in
  let trace_blank blank dval st =
    Hashtbl.set value_store ~key:(blank_to_id blank) ~data:dval
  in
  let trace_tlid tlid = Hashtbl.set tlid_store ~key:tlid ~data:true in
  {trace; trace_blank; trace_tlid; ctx = Preview}


let execute_saving_intermediates
    ~(input_vars : input_vars) (state : exec_state) (ast : expr) :
    dval * dval_store * tlid list =
  Log.infO "Executing for intermediates" ;
  let value_store = IDTable.create () in
  let tlid_store = TLIDTable.create () in
  let engine = analysis_engine value_store tlid_store in
  let st = input_vars2symtable input_vars in
  (exec ~engine ~state st ast, value_store, Hashtbl.keys tlid_store)


(* -------------------- *)
(* Execution *)
(* -------------------- *)

(* execute for real, tracing executed toplevels *)
let server_execution_engine tlid_store : engine =
  let empty_trace _ _ _ = () in
  let trace_tlid tlid = Hashtbl.set tlid_store ~key:tlid ~data:true in
  {trace = empty_trace; trace_blank = empty_trace; trace_tlid; ctx = Real}


let execute_ast ~input_vars (state : exec_state) expr : dval * tlid list =
  let tlid_store = TLIDTable.create () in
  let engine = server_execution_engine tlid_store in
  Log.infO "Executing for real" ;
  let result = exec ~engine ~state (input_vars2symtable input_vars) expr in
  (result, Hashtbl.keys tlid_store)


let execute_fn
    (state : exec_state) (name : string) (id : id) (args : dval list) :
    dval * tlid list =
  let tlid_store = TLIDTable.create () in
  let engine = server_execution_engine tlid_store in
  let result = call_fn name id args false ~engine ~state in
  (result, Hashtbl.keys tlid_store)
