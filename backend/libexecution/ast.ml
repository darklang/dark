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

(* Dangerous and deprecated, do not use: this function is co-recursive which
 * means you have to use it perfectly which is very hard to do. The example
 * below demonstrates, but you should use post_traverse instead. *)
let rec deprecated_traverse ~(f : fluid_expr -> fluid_expr) (expr : fluid_expr)
    : fluid_expr =
  match expr with
  | EPipeTarget _
  | EInteger _
  | EBool _
  | ENull _
  | EBlank _
  | EFloat _
  | EString _ ->
      expr
  | EVariable _ ->
      expr
  | ELet (id, lhs, rhs, body) ->
      ELet (id, lhs, f rhs, f body)
  | EIf (id, cond, ifbody, elsebody) ->
      EIf (id, f cond, f ifbody, f elsebody)
  | EBinOp (id, name, left, right, ster) ->
      EBinOp (id, name, f left, f right, ster)
  | EFnCall (id, name, exprs, ster) ->
      EFnCall (id, name, List.map ~f exprs, ster)
  | ELambda (id, vars, lexpr) ->
      ELambda (id, vars, f lexpr)
  | EPipe (id, exprs) ->
      EPipe (id, List.map ~f exprs)
  | EFieldAccess (id, obj, field) ->
      EFieldAccess (id, f obj, field)
  | EList (id, exprs) ->
      EList (id, List.map ~f exprs)
  | ERecord (id, pairs) ->
      ERecord (id, List.map ~f:(fun (k, v) -> (k, f v)) pairs)
  | EFeatureFlag (id, msg, cond, a, b) ->
      EFeatureFlag (id, msg, f cond, f a, f b)
  | EMatch (id, matchExpr, cases) ->
      EMatch (id, f matchExpr, List.map ~f:(fun (k, v) -> (k, f v)) cases)
  | EConstructor (id, name, args) ->
      EConstructor (id, name, List.map ~f args)
  | EPartial (id, name, old_val) ->
      EPartial (id, name, f old_val)
  | ERightPartial (id, name, old_val) ->
      ERightPartial (id, name, f old_val)
  | ELeftPartial (id, name, old_val) ->
      ELeftPartial (id, name, f old_val)


(** [post_traverse f ast] walks the entire AST from bottom to top, calling f on
 * each function. It returns a new AST with every subexpression e replaced by
 * [f e]. Unlike traverse, it does not require you to call traverse again (this
 * is not corecursive).  After calling [f], the result is NOT recursed into. *)
let rec post_traverse ~(f : fluid_expr -> fluid_expr) (expr : fluid_expr) :
    fluid_expr =
  let r = post_traverse ~f in
  let result =
    match expr with
    | EPipeTarget _
    | EInteger _
    | EBool _
    | ENull _
    | EBlank _
    | EFloat _
    | EString _ ->
        expr
    | EVariable _ ->
        expr
    | ELet (id, lhs, rhs, body) ->
        ELet (id, lhs, r rhs, r body)
    | EIf (id, cond, ifbody, elsebody) ->
        EIf (id, r cond, r ifbody, r elsebody)
    | EBinOp (id, name, left, right, ster) ->
        EBinOp (id, name, r left, r right, ster)
    | EFnCall (id, name, exprs, ster) ->
        EFnCall (id, name, List.map ~f:r exprs, ster)
    | ELambda (id, vars, lexpr) ->
        ELambda (id, vars, r lexpr)
    | EPipe (id, exprs) ->
        EPipe (id, List.map ~f:r exprs)
    | EFieldAccess (id, obj, field) ->
        EFieldAccess (id, r obj, field)
    | EList (id, exprs) ->
        EList (id, List.map ~f:r exprs)
    | ERecord (id, pairs) ->
        ERecord (id, List.map ~f:(fun (k, v) -> (k, r v)) pairs)
    | EFeatureFlag (id, msg, cond, a, b) ->
        EFeatureFlag (id, msg, r cond, r a, r b)
    | EMatch (id, matchExpr, cases) ->
        EMatch (id, r matchExpr, List.map ~f:(fun (k, v) -> (k, r v)) cases)
    | EConstructor (id, name, args) ->
        EConstructor (id, name, List.map ~f:r args)
    | EPartial (id, name, old_val) ->
        EPartial (id, name, r old_val)
    | ERightPartial (id, name, old_val) ->
        ERightPartial (id, name, r old_val)
    | ELeftPartial (id, name, old_val) ->
        ELeftPartial (id, name, r old_val)
  in
  f result


let rec set_expr ~(search : id) ~(replacement : fluid_expr) (expr : fluid_expr)
    : fluid_expr =
  let replace = set_expr ~search ~replacement in
  if search = Libshared.FluidExpression.toID expr
  then replacement
  else deprecated_traverse ~f:replace expr


let rec iter ~(f : fluid_expr -> unit) (expr : fluid_expr) : unit =
  let rec recurse e =
    f e ;
    deprecated_traverse ~f:recurse e
  in
  f expr ;
  deprecated_traverse ~f:recurse expr |> ignore ;
  ()


(* -------------------- *)
(* Execution *)
(* -------------------- *)
let find_db (dbs : fluid_expr DbT.db list) (name : string) : fluid_expr DbT.db =
  dbs
  |> List.filter ~f:(fun db ->
         match db.name with
         | Partial _ | Blank _ ->
             false
         | Filled (_, dbname) ->
             dbname = name)
  |> List.hd_exn


let find_derrorrail (dvals : 'expr_type dval list) : 'expr_type dval option =
  List.find dvals ~f:Dval.is_errorrail


let should_send_to_rail (expr : nexpr) : bool =
  match expr with FnCallSendToRail _ -> true | _ -> false


let global_input_vars (state : fluid_expr exec_state) :
    (string * 'expr_type dval) list =
  let secrets =
    state.secrets
    |> List.map ~f:(fun s ->
           (s.secret_name, DStr (Unicode_string.of_string_exn s.secret_value)))
  in
  let dbs =
    state.dbs
    |> List.filter_map ~f:(fun db ->
           match db.name with
           | Filled (_, name) ->
               Some (name, DDB name)
           | Partial _ | Blank _ ->
               None)
  in
  secrets @ dbs


let rec execute_dblock
    ~(state : fluid_expr exec_state)
    {symtable; body; params}
    (args : fluid_expr dval list) : fluid_expr dval =
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


and exec
    ~(state : fluid_expr exec_state)
    (st : fluid_expr symtable)
    (expr : fluid_expr) : fluid_expr dval =
  (* Design doc for execution results and previews: https://www.notion.so/darklang/Live-Value-Branching-44ee705af61e416abed90917e34da48e *)
  let on_execution_path = state.on_execution_path in
  let ctx = state.context in
  let exe st expr = exec ~state st expr in
  let sourceId id = SourceId (state.tlid, id) in
  let preview st expr : unit =
    if ctx = Preview
    then
      let state = {state with on_execution_path = false} in
      exec ~state st expr |> ignore
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
  let inject_param_and_execute
      (st : fluid_expr symtable) (arg : fluid_expr dval) (exp : fluid_expr) :
      fluid_expr dval =
    let result =
      match exp with
      | ELambda (id, _, _) ->
          let result = exe st exp in
          ( match result with
          | DBlock b ->
              execute_dblock ~state b [arg]
          | _ ->
              (* This should never happen, but the user should be allowed to
               * recover so this shouldn't be an exception *)
              DError
                ( sourceId id
                , "Internal type error: lambda did not produce a block" ) )
      | EBinOp (id, name, EPipeTarget _, right, ster) ->
          let send_to_rail = ster = Rail in
          call name id [arg; exe st right] send_to_rail
      | EFnCall (id, name, EPipeTarget _ :: exprs, ster) ->
          let send_to_rail = ster = Rail in
          call name id (arg :: List.map ~f:(exe st) exprs) send_to_rail
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | EBlank _ ->
          arg
      | _ ->
          (* calculate the results inside this regardless *)
          DIncomplete SourceNone
      (* partial w/ exception, full with dincomplete, or option dval? *)
    in
    trace ~on_execution_path (Libshared.FluidExpression.toID exp) result ;
    result
  in
  let value _ =
    match expr with
    | EBlank id ->
        DIncomplete (sourceId id)
    | EPartial (_, _, expr)
    | ERightPartial (_, _, expr)
    | ELeftPartial (_, _, expr) ->
        exe st expr
    | EPipeTarget id ->
        DIncomplete (sourceId id)
    | ELet (id, lhs, rhs, body) ->
        let data = exe st rhs in
        ( match data with
        | DErrorRail _ ->
            data
        | _ ->
            let bound =
              match lhs with
              | "" ->
                  st
              | name ->
                  DvalMap.insert ~key:name ~value:data st
            in
            exe bound body )
    | EString (_, value) ->
        Dval.dstr_of_string_exn value
    | EBool (_, value) ->
        DBool value
    | EInteger (_, value) ->
        DInt (Dint.of_string_exn value)
    | EFloat (id, whole, fraction) ->
        let float = whole ^ "." ^ fraction in
        float
        |> float_of_string_opt
        |> Option.map ~f:(fun v -> DFloat v)
        |> Tc.Option.withDefault
             ~default:
               (DError (SourceId (state.tlid, id), "Invalid float: " ^ float))
    | ENull id ->
        DNull
    | EList (id, exprs) ->
        (* We ignore incompletes but not error rail. Other places that lists
         are created propagate incompletes instead of ignoring *)
        exprs
        |> List.filter_map ~f:(fun e ->
               (* exe each list item to store their values, but don't count the incompletes as list items *)
               match exe st e with DIncomplete _ -> None | dv -> Some dv)
        |> fun l -> find_derrorrail l |> Option.value ~default:(DList l)
    | ERecord (id, pairs) ->
        pairs
        |> List.filter_map ~f:(fun (k, v) ->
               match (k, v) with
               | "", v ->
                   ignore (exe st v) ;
                   None
               | keyname, v ->
                   let value = exe st v in
                   ( match value with
                   | DIncomplete _ ->
                       None
                   | _ ->
                       Some (keyname, value) ))
        |> fun ps ->
        ps
        |> List.map ~f:Tuple.T2.get2
        |> find_derrorrail
        |> Option.value ~default:(Dval.to_dobj_exn ps)
    | EVariable (id, name) ->
      ( match (Symtable.get st ~key:name, ctx) with
      | None, Preview ->
          (* The trace is wrong/we have a bug --
           * we guarantee to users that variables they can lookup have been
           * bound. However, we shouldn't crash out here when running analysis
           * because it gives a horrible user experience *)
          DIncomplete (sourceId id)
      | None, Real ->
          DError (sourceId id, "There is no variable named: " ^ name)
      | Some other, _ ->
          other )
    | EBinOp (id, name, left, right, ster) ->
        call name id [exe st left; exe st right] (ster = Rail)
    | EFnCall (id, name, exprs, ster) ->
        let argvals = List.map ~f:(exe st) exprs in
        call name id argvals (ster = Rail)
    | EIf (id, cond, ifbody, elsebody) ->
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
    | EFeatureFlag (id, _, cond, oldcode, newcode) ->
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
        let condresult =
          (* under no circumstances should this cause code to fail *)
          try exe st cond with e -> DBool false
        in
        ( match condresult with
        (* only false and 'null' are falsey *)
        | DBool true ->
            preview st oldcode ;
            exe st newcode
        | DErrorRail _ ->
            preview st newcode ;
            exe st oldcode
        | DIncomplete _ ->
            preview st newcode ;
            exe st oldcode
        | DError _ ->
            preview st newcode ;
            exe st oldcode
        | _ ->
            preview st newcode ;
            exe st oldcode )
    | ELambda (id, params, body) ->
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
          preview fake_st body ) ;
        let params =
          List.filter_map params ~f:(function
              | _, "" ->
                  None
              | id, name ->
                  Some (id, name))
        in
        (* It is the responsibility of wherever executes the DBlock to pass in
         * args and execute the body. *)
        DBlock {symtable = st; params; body}
    | EPipe (id, exprs) ->
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
          DIncomplete (sourceId id) )
    | EMatch (id, matchExpr, cases) ->
        let hasMatched = ref false in
        let matchResult = ref (DIncomplete (sourceId id)) in
        let executeMatch
            (new_defs : (string * fluid_expr dval) list)
            (traces : (id * fluid_expr dval) list)
            (st : fluid_expr dval_map)
            (expr : fluid_expr) : unit =
          (* Once a pattern is matched, this function is called to execute its
           * `expr`. It tracks whether this is the first pattern to execute,
           * and calls preview if it is not. Handles calling trace on the
           * traces that have been collected by pattern matching. *)
          let newVars = DvalMap.from_list new_defs in
          let newSt = Util.merge_left newVars st in
          if !hasMatched
          then (
            (* We matched, but we've already matched a pattern previously *)
            List.iter traces ~f:(fun (id, dval) ->
                trace ~on_execution_path:false id dval) ;
            preview newSt expr )
          else (
            List.iter traces ~f:(fun (id, dval) ->
                trace ~on_execution_path id dval) ;
            hasMatched := true ;
            matchResult := exe newSt expr )
        in
        let incomplete id = DIncomplete (sourceId id) in
        let traceIncompletes traces =
          List.iter traces ~f:(fun (id, _) ->
              trace ~on_execution_path:false id (incomplete id))
        in
        let traceNonMatch
            (st : 'expr_type dval_map)
            (expr : fluid_expr)
            (traces : (id * 'expr_type dval) list)
            (id : id)
            (value : 'expr_type dval) : unit =
          preview st expr ;
          traceIncompletes traces ;
          trace ~on_execution_path:false id value
        in
        let rec matchAndExecute
            dv (builtUpTraces : (id * 'expr_type dval) list) (pattern, expr) =
          (* Compare `dv` to `pattern`, and execute the rhs `expr` of any
           * matches. Tracks whether a branch has already been executed and
           * will exceute later matches in preview mode.  Ensures all patterns
           * and branches are properly traced.  Recurse on partial matches
           * (constructors); builtUpTraces is the set of traces that have been
           * built up by recursing: they can only be matched when the pattern
           * is ready to match. *)
          match pattern with
          | Libshared.FluidPattern.FPInteger (_, pid, int) ->
              let v = DInt (Dint.of_string_exn int) in
              if v = dv
              then executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else traceNonMatch st expr builtUpTraces pid v
          | Libshared.FluidPattern.FPBool (_, pid, bool) ->
              let v = DBool bool in
              if v = dv
              then executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else traceNonMatch st expr builtUpTraces pid v
          | Libshared.FluidPattern.FPString {patternID = pid; str; matchID = _}
            ->
              let v = Dval.dstr_of_string_exn str in
              if v = dv
              then executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else traceNonMatch st expr builtUpTraces pid v
          | Libshared.FluidPattern.FPFloat (_, pid, whole, fraction) ->
              let v =
                let float = whole ^ "." ^ fraction in
                float
                |> float_of_string_opt
                |> Option.map ~f:(fun v -> DFloat v)
                |> Tc.Option.withDefault
                     ~default:
                       (DError
                          (SourceId (state.tlid, id), "Invalid float: " ^ float))
              in
              if v = dv
              then executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else traceNonMatch st expr builtUpTraces pid v
          | Libshared.FluidPattern.FPNull (_, pid) ->
              let v = DNull in
              if v = dv
              then executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else traceNonMatch st expr builtUpTraces pid v
          | Libshared.FluidPattern.FPVariable (_, pid, v) ->
              (* only matches allowed values *)
              if Dval.is_fake_marker_dval dv
              then traceNonMatch st expr builtUpTraces pid dv
              else executeMatch [(v, dv)] ((pid, dv) :: builtUpTraces) st expr
          | Libshared.FluidPattern.FPBlank (_, pid) ->
              (* never matches *)
              traceNonMatch st expr builtUpTraces pid (incomplete pid)
          | Libshared.FluidPattern.FPConstructor (_, pid, name, args) ->
            ( match (name, args, dv) with
            | "Just", [p], DOption (OptJust v)
            | "Ok", [p], DResult (ResOk v)
            | "Error", [p], DResult (ResError v) ->
                matchAndExecute v ((pid, dv) :: builtUpTraces) (p, expr)
            | "Nothing", [], DOption OptNothing ->
                executeMatch [] ((pid, dv) :: builtUpTraces) st expr
            | "Nothing", [], _ ->
                traceNonMatch st expr builtUpTraces pid (DOption OptNothing)
            | _ ->
                let error =
                  if Tc.List.member
                       ~value:name
                       ["Just"; "Ok"; "Error"; "Nothing"]
                  then incomplete pid
                  else DError (sourceId pid, "Invalid constructor")
                in
                traceNonMatch st expr builtUpTraces pid error ;
                (* Trace each argument too. TODO: recurse *)
                List.iter args ~f:(fun pat ->
                    let id = Libshared.FluidPattern.toID pat in
                    trace ~on_execution_path:false id (incomplete id)) ;
                () )
        in
        let matchVal = exe st matchExpr in
        List.iter cases ~f:(fun (pattern, expr) ->
            matchAndExecute matchVal [] (pattern, expr)) ;
        !matchResult
    | EFieldAccess (id, e, field) ->
        let obj = exe st e in
        let result =
          match obj with
          | DObj o ->
            ( match field with
            | "" ->
                DIncomplete (sourceId id)
            | f ->
              (match Map.find o f with Some v -> v | None -> DNull) )
          | DIncomplete _ as i ->
              i
          | DErrorRail _ ->
              obj
          | x ->
              let actualTipe =
                match Dval.tipe_of x with
                | TDB ->
                    "it's a Datastore. Use DB:: standard library functions to interact with Datastores"
                | tipe ->
                    "it's a " ^ Dval.tipe_to_string tipe
              in
              DError
                ( sourceId id
                , "Attempting to access a field of something that isn't a record or dict, ("
                  ^ actualTipe
                  ^ ")." )
        in
        result
    | EConstructor (id, name, args) ->
      ( match (name, args) with
      | "Nothing", [] ->
          DOption OptNothing
      | "Just", [arg] ->
          Dval.to_opt_just (exe st arg)
      | "Ok", [arg] ->
          Dval.to_res_ok (exe st arg)
      | "Error", [arg] ->
          Dval.to_res_err (exe st arg)
      | _ ->
          DError (sourceId id, "Invalid construction option") )
  in
  let execed_value = value () in
  trace ~on_execution_path (Libshared.FluidExpression.toID expr) execed_value ;
  execed_value


(* |> Log.pp "execed" ~f:(fun dv -> sexp_of_dval dv *)
(* |> Sexp.to_string) *)
and call_fn
    ~(state : 'expr_type exec_state)
    (name : string)
    (id : id)
    (argvals : 'expr_type dval list)
    (send_to_rail : bool) : 'expr_type dval =
  let sourceId id = SourceId (state.tlid, id) in
  let fn =
    Libs.get_fn state.user_fns name
    |> function
    | Some fn ->
        Some fn
    | None ->
        (* Clean up - move to Libs or use a map or something *)
        state.package_fns
        |> List.find ~f:(fun fn -> List.mem fn.prefix_names name ~equal:( = ))
  in
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
                        |> (fun (db : 'expr_type DbT.db) -> db.cols)
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
                DIncomplete (sourceId id) )
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
                ( sourceId id
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
         * something else, they would go on the error rail too. *)
        | other ->
            DErrorRail other
      else result


and exec_fn
    ~(state : 'expr_type exec_state)
    (fnname : string)
    (id : id)
    (fn : 'expr_type fn)
    (args : 'expr_type dval_map) : 'expr_type dval =
  let sourceId id = SourceId (state.tlid, id) in
  let type_error_or_value ~user_tipes result =
    (* https://www.notion.so/darklang/What-should-happen-when-the-return-type-is-wrong-533f274f94754549867fefc554f9f4e3 *)
    match Type_checker.check_function_return_type ~user_tipes fn result with
    | Ok () ->
        result
    | Error errs ->
        DError
          ( sourceId id
          , "Type error(s) in return type: "
            ^ Type_checker.Error.list_to_string errs )
  in
  if state.context = Preview
     && (not state.on_execution_path)
     && Tc.StrSet.member state.callstack ~value:fnname
  then
    (* Don't recurse (including transitively!) when previewing unexecuted paths
     * in the editor. If we do, we'll recurse forever and blow the stack. *)
    DIncomplete (SourceId (state.tlid, id))
  else
    let state =
      { state with
        executing_fnname = fnname
      ; callstack = Tc.StrSet.add fnname state.callstack }
    in
    let arglist =
      fn.parameters
      |> List.map ~f:(fun (p : param) -> p.name)
      |> List.filter_map ~f:(fun key -> DvalMap.get ~key args)
    in
    let args_with_globals =
      let globals = state |> global_input_vars |> DvalMap.from_list in
      Util.merge_left globals args
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
          if state.context = Preview && fn.preview_safety <> Safe
          then
            match state.load_fn_result sfr_desc arglist with
            | Some (result, _ts) ->
                result
            | inc ->
                DIncomplete (sourceId id)
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
                  Dval.exception_to_dval e (sourceId id)
              | e ->
                  (* After the rethrow, this gets eventually caught then shown to the
                   * user as a Dark Internal Exception. It's an internal exception
                   * because we didn't anticipate the problem, give it a nice error
                   * message, etc. It'll appear in Rollbar as "Unknown Err". To remedy
                   * this, give it a nice exception via RT.error. *)
                  Exception.reraise e
            in
            (* there's no point storing data we'll never ask for *)
            if fn.preview_safety <> Safe
            then state.store_fn_result sfr_desc arglist result ;
            result
      | PackageFunction body ->
        (* This is similar to InProcess but also has elements of UserCreated. *)
        ( match Type_checker.check_function_call ~user_tipes:[] fn args with
        | Ok () ->
            let result =
              match (state.context, state.load_fn_result sfr_desc arglist) with
              | Preview, Some (result, _ts) ->
                  Dval.unwrap_from_errorrail result
              | Preview, None when fn.preview_safety <> Safe ->
                  DIncomplete (sourceId id)
              | _ ->
                  (* It's okay to execute user functions in both Preview and
                   * Real contexts, But in Preview we might not have all the
                   * data we need *)
                  (* TODO: We don't munge `state.tlid` like we do in
                   * UserCreated, which means there might be `id` collisions between
                   * AST nodes. Munging `state.tlid` would not save us from tlid
                   * collisions either. tl;dr, executing a package function may result
                   * in trace data being associated with the wrong handler/call site.
                   * *)
                  let result = exec ~state args_with_globals body in
                  state.store_fn_result sfr_desc arglist result ;
                  result
                  |> Dval.unwrap_from_errorrail
                  |> type_error_or_value ~user_tipes:[]
            in
            (* there's no point storing data we'll never ask for *)
            if fn.preview_safety <> Safe
            then state.store_fn_result sfr_desc arglist result ;
            result
        | Error errs ->
            DError
              ( sourceId id
              , "Type error(s) in function parameters: "
                ^ Type_checker.Error.list_to_string errs ) )
      | UserCreated (tlid, body) ->
        ( match
            Type_checker.check_function_call
              ~user_tipes:state.user_tipes
              fn
              args
          with
        | Ok () ->
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
                let result = exec ~state args_with_globals body in
                state.store_fn_result sfr_desc arglist result ;
                result
                |> Dval.unwrap_from_errorrail
                |> type_error_or_value ~user_tipes:state.user_tipes )
        | Error errs ->
            DError
              ( sourceId id
              , "Type error(s) in function parameters: "
                ^ Type_checker.Error.list_to_string errs ) )
      | API f ->
          f args )


(* -------------------- *)
(* Execution *)
(* -------------------- *)

let execute_ast ~(state : 'expr_type exec_state) ~input_vars expr :
    'expr_type dval =
  let state = {state with exec} in
  let symtable = input_vars @ global_input_vars state |> input_vars2symtable in
  exec ~state symtable expr


let execute_fn
    ~(state : 'expr_type exec_state)
    (name : string)
    (id : id)
    (args : 'expr_type dval list) : 'expr_type dval =
  let state = {state with exec} in
  call_fn name id args false ~state
