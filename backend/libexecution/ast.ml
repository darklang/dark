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


(** [post_travere f ast] walks the entire AST from bottom to top, calling f on
 * each function. It returns a new AST with every subexpression e replaced by
 * [f e]. Unlike traverse, it does not require you to call traverse again (this
 * is not corecursive).  After calling [f], the result is NOT recursed into. *)
let rec post_traverse ~(f : expr -> expr) (expr : expr) : expr =
  let r = post_traverse ~f in
  let result =
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
                Let (lhs, r rhs, r body)
            | If (cond, ifbody, elsebody) ->
                If (r cond, r ifbody, r elsebody)
            | FnCall (name, exprs) ->
                FnCall (name, List.map ~f:r exprs)
            | FnCallSendToRail (name, exprs) ->
                FnCallSendToRail (name, List.map ~f:r exprs)
            | Lambda (vars, lexpr) ->
                Lambda (vars, r lexpr)
            | Thread exprs ->
                Thread (List.map ~f:r exprs)
            | FieldAccess (obj, field) ->
                FieldAccess (r obj, field)
            | ListLiteral exprs ->
                ListLiteral (List.map ~f:r exprs)
            | ObjectLiteral pairs ->
                ObjectLiteral (List.map ~f:(fun (k, v) -> (k, r v)) pairs)
            | FeatureFlag (msg, cond, a, b) ->
                FeatureFlag (msg, r cond, r a, r b)
            | Match (matchExpr, cases) ->
                Match (r matchExpr, List.map ~f:(fun (k, v) -> (k, r v)) cases)
            | Constructor (name, args) ->
                Constructor (name, List.map ~f:r args)
            | FluidPartial (name, old_val) ->
                FluidPartial (name, r old_val)
            | FluidRightPartial (name, old_val) ->
                FluidRightPartial (name, r old_val) )
  in
  f result


let rec set_expr ~(search : id) ~(replacement : expr) (expr : expr) : expr =
  let replace = set_expr ~search ~replacement in
  if search = blank_to_id expr then replacement else traverse ~f:replace expr


let rec iter ~(f : expr -> unit) (expr : expr) : unit =
  let rec recurse e =
    f e ;
    traverse ~f:recurse e
  in
  f expr ;
  traverse ~f:recurse expr |> ignore ;
  ()


(* -------------------- *)
(* Execution *)
(* -------------------- *)
let find_db (dbs : DbT.db list) (name : string) : DbT.db =
  dbs
  |> List.filter ~f:(fun db ->
         match db.name with
         | Partial _ | Blank _ ->
             false
         | Filled (_, dbname) ->
             dbname = name)
  |> List.hd_exn


let find_derrorrail (dvals : dval list) : dval option =
  List.find dvals ~f:Dval.is_errorrail


let should_send_to_rail (expr : nexpr) : bool =
  match expr with FnCallSendToRail _ -> true | _ -> false


let rec execute_dblock
    ~(state : exec_state) {symtable; body; params} (args : dval list) : dval =
  (* If one of the args is fake value used as a marker, return it instead of
   * executing. This is the same behaviour as in fn calls. *)
  let first_marker = List.find args ~f:Dval.is_fake_marker_dval in
  match first_marker with
  | Some dv ->
      dv
  | None ->
      (* One of the reasons to take a separate list of params and args is to
       * provide this error message here. We don't have this information in
       * other places, and the alternative is just to provide incompletes
       * with no context *)
      if List.length params <> List.length args
      then
        DError
          ( SourceNone
          , "Expected "
            ^ string_of_int (List.length params)
            ^ " arguments, got "
            ^ string_of_int (List.length args) )
      else
        let bindings = List.zip_exn params args in
        List.iter bindings ~f:(fun ((id, paramName), dv) ->
            state.trace ~on_execution_path:state.on_execution_path id dv) ;
        let new_st =
          bindings
          |> List.map ~f:(Prelude.Tuple2.mapFirst ~f:Prelude.Tuple2.second)
          |> DvalMap.from_list
          |> Util.merge_right symtable
        in
        exec ~state new_st body


and exec ~(state : exec_state) (st : symtable) (expr : expr) : dval =
  let on_execution_path = state.on_execution_path in
  let ctx = state.context in
  let exe ?(on_execution_path = on_execution_path) st expr =
    let state = {state with on_execution_path} in
    exec ~state st expr
  in
  let preview st expr : unit =
    if ctx = Preview then exe ~on_execution_path:false st expr |> ignore
  in
  let call = call_fn ~state in
  let trace = state.trace in
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
  let inject_param_and_execute (st : symtable) (arg : dval) (exp : expr) : dval
      =
    let result =
      match exp with
      | Filled (id, Lambda _) ->
          let result = exe st exp in
          ( match result with
          | DBlock b ->
              execute_dblock ~state b [arg]
          | _ ->
              (* This should never happen, but the user should be allowed to
               * recover so this shouldn't be an exception *)
              DError
                ( SourceId id
                , "Internal type error: lambda did not produce a block" ) )
      | Filled (id, (FnCall (name, exprs) as fncall))
      | Filled (id, (FnCallSendToRail (name, exprs) as fncall)) ->
          let send_to_rail = should_send_to_rail fncall in
          call name id (arg :: List.map ~f:(exe st) exprs) send_to_rail
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | Partial _ | Blank _ ->
          arg
      | _ ->
          (* calculate the results inside this regardless *)
          DIncomplete SourceNone
      (* partial w/ exception, full with dincomplete, or option dval? *)
    in
    trace ~on_execution_path (blank_to_id exp) result ;
    result
  in
  let value _ =
    match expr with
    | Blank id ->
        DIncomplete (SourceId id)
    | Partial (id, _) ->
        DIncomplete (SourceId id)
    | Filled (_, FluidPartial (_, expr))
    | Filled (_, FluidRightPartial (_, expr)) ->
        exe st expr
    | Filled (_, Let (lhs, rhs, body)) ->
        let data = exe st rhs in
        ( match data with
        | DErrorRail _ ->
            data
        | _ ->
            let bound =
              match lhs with
              | Filled (_, name) ->
                  DvalMap.insert ~key:name ~value:data st
              | Partial _ | Blank _ ->
                  st
            in
            exe bound body )
    | Filled (id, Value s) ->
        Dval.parse_literal s
        |> Option.value ~default:(DError (SourceId id, "Unparsable value"))
    | Filled (_, ListLiteral exprs) ->
        (* We ignore incompletes but not error rail. Other places that lists
     are created propagate incompletes instead of ignoring *)
        exprs
        |> List.filter_map ~f:(fun e ->
               (* exe each list item to store their values, but don't count the incompletes as list items *)
               match exe st e with DIncomplete _ -> None | dv -> Some dv)
        |> fun l -> find_derrorrail l |> Option.value ~default:(DList l)
    | Filled (_, ObjectLiteral pairs) ->
        pairs
        |> List.filter_map ~f:(fun (k, v) ->
               match (k, v) with
               | Filled (_, keyname), v ->
                   let value = exe st v in
                   ( match value with
                   | DIncomplete _ ->
                       None
                   | _ ->
                       Some (keyname, value) )
               | _, v ->
                   ignore (exe st v) ;
                   None)
        |> fun ps ->
        ps
        |> List.map ~f:Tuple.T2.get2
        |> find_derrorrail
        |> Option.value ~default:(Dval.to_dobj_exn ps)
    | Filled (id, Variable name) ->
      ( match (Symtable.get st ~key:name, ctx) with
      | None, Preview ->
          (* The trace is wrong/we have a bug --
           * we guarantee to users that variables they can lookup have been
           * bound. However, we shouldn't crash out here when running analysis
           * because it gives a horrible user experience *)
          DIncomplete (SourceId id)
      | None, Real ->
          DError (SourceId id, "There is no variable named: " ^ name)
      | Some other, _ ->
          other )
    | Filled (id, FnCallSendToRail (name, exprs)) ->
        let argvals = List.map ~f:(exe st) exprs in
        call name id argvals true
    | Filled (id, FnCall (name, exprs)) ->
        let argvals = List.map ~f:(exe st) exprs in
        call name id argvals false
    | Filled (id, If (cond, ifbody, elsebody)) ->
      ( match exe st cond with
      (* only false and 'null' are falsey *)
      | DBool false | DNull ->
          preview st ifbody ;
          exe st elsebody
      | DIncomplete _ as i ->
          preview st ifbody ;
          preview st elsebody ;
          i
      | DError (src, _) ->
          preview st ifbody ;
          preview st elsebody ;
          DError (src, "Expected boolean, got error")
      | DErrorRail _ as er ->
          preview st ifbody ;
          preview st elsebody ;
          er
      | _ ->
          preview st elsebody ;
          exe st ifbody )
    | Filled (id, FeatureFlag (_, cond, oldcode, newcode)) ->
      (* True gives newexpr, unlike in If statements
       *
       * In If statements, we use a false/null as false, and anything else is
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
       * gets new code. *)
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
            with e -> Dval.exception_to_dval e (SourceId id)
          in
          ( match condresult with
          | DBool true ->
              newresult
          | DIncomplete _ ->
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
          | DIncomplete _ ->
              exe st oldcode
          | DError _ ->
              exe st oldcode
          | _ ->
              exe st oldcode ) )
    | Filled (id, Lambda (params, body)) ->
        ( if ctx = Preview
        then
          (* Since we return a DBlock, it's contents may never be
          * executed. So first we execute with no context to get some
          * live values. *)
          let fake_st =
            Util.merge_left
              (Symtable.singleton "var" (DIncomplete SourceNone))
              st
          in
          ignore (exe fake_st body) ) ;
        let params =
          List.filter_map params ~f:(function
              | Blank _ ->
                  None
              | Partial _ ->
                  None
              | Filled (id, name) ->
                  Some (id, name))
        in
        (* It is the responsibility of wherever executes the DBlock to pass in
         * args and execute the body. *)
        DBlock {symtable = st; params; body}
    | Filled (id, Thread exprs) ->
      (* For each expr, execute it, and then thread the previous result thru *)
      ( match exprs with
      | e :: es ->
          let fst = exe st e in
          List.fold_left es ~init:fst ~f:(fun previous nxt ->
              let result = inject_param_and_execute st previous nxt in
              match result with
              | DIncomplete _ ->
                  previous
              (* let execution through *)
              (* DErrorRail is handled by inject_param_and_execute *)
              | _ ->
                  result)
      | [] ->
          DIncomplete (SourceId id) )
    | Filled (id, Match (matchExpr, cases)) ->
        let rec matches dv pat :
            bool (* is a match *)
            * (string * dval) (* symtable vars *) list
            * (id * dval) (* traces *) list
            * bool (* allow previewing the rhs *) =
          match pat with
          | Filled (pid, PLiteral l) ->
            ( match Dval.parse_literal l with
            | Some v ->
                (v = dv, [], [(pid, v)], true)
            | None ->
                (false, [], [(pid, DIncomplete (SourceId pid))], true) )
          | Filled (pid, PVariable v) ->
              (true, [(v, dv)], [(pid, dv)], true)
          | Filled (pid, PConstructor (name, args)) ->
            (* We don't trace constructors, as if they don't match we would add
             * incompletes to things that are not incomplete (just not
             * matched), and users are already confused enough about
             * incompletes. We probably need a way to indicate the live value
             * is "uninteresting" instead.
             *)
            ( match (name, args, dv) with
            | "Just", [p], DOption (OptJust v)
            | "Ok", [p], DResult (ResOk v)
            | "Error", [p], DResult (ResError v) ->
                let is_match, symbols, traces, preview = matches v p in
                let new_traces = if is_match then [(pid, dv)] else [] in
                (is_match, symbols, traces @ new_traces, preview && is_match)
            | "Nothing", [], v ->
                (v = DOption OptNothing, [], [(pid, DOption OptNothing)], true)
            | "Just", _, _ | "Ok", _, _ | "Error", _, _ | "Nothing", _, _ ->
                (false, [], [], false)
            | _, _, _ ->
                ( false
                , []
                , [(pid, DError (SourceId pid, "Invalid constructor"))]
                , false ) )
          | Blank pid | Partial (pid, _) ->
              (false, [], [(pid, DIncomplete (SourceId pid))], true)
        in
        let exe_with_vars e vars =
          let newVars = DvalMap.from_list vars in
          let newSt = Util.merge_left newVars st in
          exe newSt e
        in
        let matchVal = exe st matchExpr in
        (* In Preview, show traces for patterns and execute each expression *)
        if ctx = Preview
        then
          List.iter cases ~f:(fun (pattern, expr) ->
              let _, vars, tracevars, preview_rhs = matches matchVal pattern in
              List.iter tracevars ~f:(fun (id, dv) ->
                  trace ~on_execution_path id dv) ;
              (* Don't preview constructor rhs, as they have misleading incompletes *)
              if preview_rhs then exe_with_vars expr vars |> ignore) ;
        (* Always do the real execution *)
        let continue = ref true in
        let matchResult = ref (DIncomplete (SourceId id)) in
        List.iter cases ~f:(fun (pattern, expr) ->
            if !continue
            then
              let matches, vars, _, _ = matches matchVal pattern in
              if matches
              then (
                continue := false ;
                matchResult := exe_with_vars expr vars )
              else ()) ;
        !matchResult
    | Filled (id, FieldAccess (e, field)) ->
        let obj = exe st e in
        let result =
          match obj with
          | DObj o ->
            ( match field with
            | Partial (id, _) | Blank id ->
                DIncomplete (SourceId id)
            | Filled (_, f) ->
              (match Map.find o f with Some v -> v | None -> DNull) )
          | DIncomplete _ as i ->
              i
          | DErrorRail _ ->
              obj
          | x ->
              DError
                ( SourceId id
                , "Attempting to access of a field of something that isn't an object but is a "
                  ^ (x |> Dval.tipe_of |> Dval.tipe_to_string)
                  ^ "" )
        in
        result
    | Filled (id, Constructor (name, args)) ->
      ( match (name, args) with
      | Filled (_, "Nothing"), [] ->
          DOption OptNothing
      | Filled (_, "Just"), [arg] ->
          Dval.to_opt_just (exe st arg)
      | Filled (_, "Ok"), [arg] ->
          Dval.to_res_ok (exe st arg)
      | Filled (_, "Error"), [arg] ->
          Dval.to_res_err (exe st arg)
      | _ ->
          DError (SourceId id, "Invalid construction option") )
  in
  let execed_value = value () in
  trace ~on_execution_path (blank_to_id expr) execed_value ;
  execed_value


(* |> Log.pp "execed" ~f:(fun dv -> sexp_of_dval dv *)
(* |> Sexp.to_string) *)
and call_fn
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
            let fn_result = state.load_fn_result sfr_desc argvals in
            (* In the case of DB::query (and friends), we want to backfill
             * the lambda's livevalues, as the lambda was never actually
             * executed. We hack this is here as we have no idea what this
             * abstraction might look like in the future. *)
            ( if state.context = Preview
                 (* The prefix might match too much but that's fixed by the
                  * match which is very specific *)
                 && Prelude.String.startsWith ~prefix:"DB::query" name
            then
              match argvals with
              | [DDB dbname; DBlock b] ->
                  let sample =
                    match fn_result with
                    | Some (DList (sample :: _), _) ->
                        sample
                    | _ ->
                        find_db state.dbs dbname
                        |> (fun (db : DbT.db) -> db.cols)
                        |> List.filter_map ~f:(function
                               | Filled (_, field), Filled _ ->
                                   Some (field, DIncomplete SourceNone)
                               | _ ->
                                   None)
                        |> Dval.to_dobj_exn
                  in
                  ignore (execute_dblock ~state b [sample])
              | _ ->
                  () ) ;
            ( match fn_result with
            | Some (result, _ts) ->
                result
            | _ ->
                DIncomplete (SourceId id) )
        | Some fn ->
            (* equalize length *)
            let expected_length = List.length fn.parameters in
            let actual_length = List.length argvals in
            if expected_length = actual_length
            then
              let args =
                fn.parameters
                |> List.map2_exn ~f:(fun dv p -> (p.name, dv)) argvals
                |> DvalMap.from_list_exn
              in
              exec_fn ~state name id fn args
            else
              DError
                ( SourceId id
                , name
                  ^ " has "
                  ^ string_of_int expected_length
                  ^ " parameters, but here was called with "
                  ^ string_of_int actual_length
                  ^ " arguments." )
      in
      if send_to_rail
      then
        match Dval.unwrap_from_errorrail result with
        | DOption (OptJust v) ->
            v
        | DResult (ResOk v) ->
            v
        | DIncomplete _ as i ->
            i
        | DError _ as e ->
            e
        (* There should only be DOptions and DResults here, but hypothetically we got
        * something else, they would go on the error rail too.  *)
        | other ->
            DErrorRail other
      else result


and exec_fn
    ~(state : exec_state)
    (fnname : string)
    (id : id)
    (fn : fn)
    (args : dval_map) : dval =
  let arglist =
    fn.parameters
    |> List.map ~f:(fun (p : param) -> p.name)
    |> List.filter_map ~f:(fun key -> DvalMap.get ~key args)
  in
  let sfr_desc = (state.tlid, fnname, id) in
  let badArg =
    List.find arglist ~f:(function
        | DError _ when String.Caseless.equal fnname "Bool::isError" ->
            false
        | DError _ | DIncomplete _ ->
            true
        | _ ->
            false)
  in
  match badArg with
  | Some (DIncomplete src) ->
      DIncomplete src
  | Some (DError (src, _)) ->
      DError (src, "Fn called with an error as an argument")
  | _ ->
    ( match fn.func with
    | InProcess f ->
        if state.context = Preview && not fn.preview_execution_safe
        then
          match state.load_fn_result sfr_desc arglist with
          | Some (result, _ts) ->
              result
          | inc ->
              DIncomplete (SourceId id)
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
                Dval.exception_to_dval e (SourceId id)
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
                         None)
              |> DvalMap.from_list
            in
            Util.merge_left db_dvals args
          in
          state.trace_tlid tlid ;
          (* Don't execute user functions if it's preview mode and we have a result *)
          ( match (state.context, state.load_fn_result sfr_desc arglist) with
          | Preview, Some (result, _ts) ->
              Dval.unwrap_from_errorrail result
          | _ ->
              (* It's okay to execute user functions in both Preview and Real contexts,
               * But in Preview we might not have all the data we need *)
              state.store_fn_arguments tlid args ;
              let state = {state with tlid} in
              let result = exec ~state args_with_dbs body in
              state.store_fn_result sfr_desc arglist result ;
              Dval.unwrap_from_errorrail result )
      | Error errs ->
          let error_msgs =
            errs
            |> List.map ~f:Type_checker.Error.to_string
            |> String.concat ~sep:", "
          in
          DError
            (SourceId id, "Type error(s) in function parameters: " ^ error_msgs)
      )
    | API f ->
        f args )


(* -------------------- *)
(* Execution *)
(* -------------------- *)

let execute_ast ~(state : exec_state) ~input_vars expr : dval =
  let state = {state with exec} in
  exec ~state (input_vars2symtable input_vars) expr


let execute_fn
    ~(state : exec_state) (name : string) (id : id) (args : dval list) : dval =
  let state = {state with exec} in
  call_fn name id args false ~state
