open Core

open Types
open Types.RuntimeT
module RT = Runtime
module FF = Feature_flag

let flatten_ff (bo: 'a or_blank) (ff: feature_flag) : 'a or_blank =
  match bo with
  | Flagged (id, msg, setting, l, r) ->
      FF.select id setting l r ff
  | _ -> bo

let should_be_flat () =
  Exception.internal "This blank_or should have been flattened"

let blank_to_id (bo : 'a or_blank) : id =
  match bo with
  | Filled (id, _) -> id
  | Blank (id) -> id
  | Flagged (id, _, _, _, _) -> id

let to_id (expr: expr) : id =
  blank_to_id expr

let exception_to_dval ~(log: bool) exc =
  match exc with
  | Exception.DarkException e ->
    let json = e
               |> Exception.exception_data_to_yojson
               |> Yojson.Safe.pretty_to_string in
    if log then print_endline json else ();
    DError json

  | Postgresql.Error (Unexpected_status (_actual, msg, _expecteds)) ->
    (* go through a set of known errors and make them user-friendly *)

    (* drop 2nd line which has the sql info on it, then trim different
     * variations of Error from the front. *)
    let msg = msg
              |> String.split ~on:'\n'
              |> List.hd
              |> Option.value ~default:""
              |> Util.maybe_chop_prefix ~prefix:"ERROR:"
              |> String.lstrip
    in
    let m regex : string list option =
      msg
      |> Util.string_match ~regex
      |> Result.ok
    in

    let db_short_name db =
      String.split ~on:'_' db
      |> List.tl
      |> Option.value ~default:[""]
      |> List.hd_exn
    in

    let result =
      match m "column \"(.*)\" of relation \"(.*)\" does not exist" with
      | Some [field; db] ->
          ("Object has field "
          ^ field
          ^ ", but "
          ^ db_short_name db
          ^ " doesn't have that field")
      | Some other -> Exception.internal "wrong arg count"
      | _ -> msg
    in
    DError result

  | Postgresql.Error e ->
    DError (Postgresql.string_of_error e)

  | exc ->
    (* We do this split because it can be fragile grabbing the
     * Backtrace so we need to do that first *)
    if log
    then
      let bt = Backtrace.Exn.most_recent () in
      let msg = Exn.to_string exc in
      print_endline (Backtrace.to_string bt);
      print_endline msg;
      DError msg
    else
      let msg = Exn.to_string exc in
      DError msg


(* -------------------- *)
(* Execution *)
(* -------------------- *)

type exec_trace = (expr -> dval -> symtable -> unit)
type exec_trace_blank = (string or_blank -> dval -> symtable -> unit)
let empty_trace _ _ _ = ()

let rec exec_ ?(trace: exec_trace=empty_trace)
              ?(trace_blank: exec_trace_blank=empty_trace)
              ~(ctx: context)
              ~(state: exec_state)
              (st: symtable) (expr: expr) : dval =
  let exe = exec_ ~trace ~trace_blank ~ctx ~state in
  let call (name: string) (id: id) (argvals: dval list) : dval =
    let fn = Libs.get_fn_exn state.user_fns name in
    (* equalize length *)
    let length_diff = List.length fn.parameters - List.length argvals in
    let argvals =
      if length_diff > 0
      then argvals @ (List.init length_diff (fun _ -> DNull))
      else if length_diff = 0
      then argvals
      else Exception.internal ("Too many args in fncall to " ^ name) in
    let args =
      fn.parameters
      |> List.map2_exn ~f:(fun dv (p: param) -> (p.name, dv)) argvals
      |> DvalMap.of_alist_exn
    in
    call_fn ~state ~ind:0 ~ctx name id fn args
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
        call name id (param :: (List.map ~f:(exe st) exprs))
      (* If there's a hole, just run the computation straight through, as
       * if it wasn't there*)
      | Blank _ ->
        param
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
          (match flatten_ff field state.ff with
           | Blank _ -> DIncomplete
           | Flagged _ -> should_be_flat ()
           | Filled (_, f) ->
             (match e with
              | Filled (_, Variable "request")
                when ctx = Preview
                  && Dval.equal_dval obj
                       (Parsed_request.to_dval Parsed_request.sample) ->
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
    if phys_equal trace empty_trace (* only way to compare functions *)
    then value ()
    else
      try
        value ()
      with e ->
        exception_to_dval ~log:true e
  in
  trace expr execed_value st;
  execed_value
  (* |> Log.pp "execed" ~f:(fun dv -> sexp_of_dval dv |> *)
  (*                                  Sexp.to_string) *)

and call_fn ?(ind=0) ~(ctx: context) ~(state: exec_state)
    (fnname: string) (id: id) (fn: fn) (args: dval_map) : dval =

  let paramsIncomplete args =
    List.exists args
      ~f:(fun x ->
          match x with
          | DIncomplete -> true
          | DError _ -> true
          | _ -> false)
  in

  let raise_arglist_error bt args arglist : unit =
    Log.erroR ~name:"execution" ~ind "exception caught" args
               ~f:Dval.dvalmap_to_string;
    let all = List.zip_exn fn.parameters arglist in
    let invalid =
      List.filter all
        ~f:(fun (p,a) ->
            Dval.tipe_of a <> p.tipe && p.tipe <> TAny)
    in
    match invalid with
    | [] -> ()

    | (p,a) :: _ ->
       RT.error
         ~bt:(Some bt)
         ~actual:a
         ~expected:(Dval.tipe_to_string p.tipe)
         (fnname ^ " was called with the wrong type to parameter: " ^ p.name)

  in

  match fn.func with
  | InProcess f ->
    let arglist = fn.parameters
                  |> List.map ~f:(fun (p: param) -> p.name)
                  |> List.map ~f:(DvalMap.find_exn args) in

    if paramsIncomplete arglist
    then DIncomplete
    else
      let executingUnsafe = ctx = Preview
                            && not fn.preview_execution_safe
                            && List.mem ~equal:(=) state.exe_fn_ids id
      in
      let sfr_state = (state.host, state.tlid, fnname, id) in
      let maybe_store_result result =
        if executingUnsafe
        then Stored_function_result.store sfr_state arglist result
        else ();
      in

      let result =
        (try
           match ctx with
           | Real ->
             f (state, arglist)
           | Preview ->
             if fn.preview_execution_safe || executingUnsafe
             then f (state, arglist)
             else
               (match Stored_function_result.load sfr_state arglist with
                | Some result -> result
               | _ -> DIncomplete)
         with
         | e ->
           Exception.reraise_after e
             (fun bt ->
               maybe_store_result (exception_to_dval ~log:false e);
               raise_arglist_error bt args arglist;
             ))
      in
      maybe_store_result result;
      result


  | UserCreated body ->
    exec_ ~trace:empty_trace ~ctx ~state args body
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
        RT.error (fnname ^ " is missing a parameter")
          ~expected:(fn.parameters
                     |> List.map ~f:param_to_string
                     |> String.concat ~sep:", ")
          ~actual:DIncomplete

(* default to no tracing *)
let execute state env expr : dval =
  exec_ env expr
    ~trace:empty_trace ~trace_blank:empty_trace ~ctx:Real ~state


(* -------------------- *)
(* Analysis *)
(* -------------------- *)


type dval_store = dval Int.Table.t


let execute_saving_intermediates (state : exec_state) (ast: expr)
  : (dval * dval_store) =

  let value_store = Int.Table.create () in
  let trace expr dval st =
    Hashtbl.set value_store ~key:(to_id expr) ~data:dval
  in
  let trace_blank blank dval st =
    Hashtbl.set value_store ~key:(blank_to_id blank) ~data:dval
  in
  (exec_ ~trace ~trace_blank ~ctx:Preview ~state state.env ast, value_store)

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
  { value = Dval.to_livevalue_repr dv
  ; tipe = Dval.tipename dv
  ; json = dv
           |> Dval.dval_to_yojson ~livevalue:true
           |> Yojson.Safe.pretty_to_string
  ; exc = None
  }

let dval_store_to_yojson (ds : dval_store) : Yojson.Safe.json =
  ht_to_json_dict ds ~f:(fun dv -> dv |> dval_to_livevalue |> livevalue_to_yojson)


module SymSet = Set.Make(String)
type sym_set = SymSet.t

let rec sym_exec ~(ff: feature_flag) ~(trace: (expr -> sym_set -> unit)) (st: sym_set) (expr: expr) : unit =
  let sexe = sym_exec ~trace ~ff in
  try
    let _ =
      (match expr with
       | Blank _ -> ()
       | Flagged (id, _, _, l, r) ->
         sexe st l;
         sexe st r;
         sexe st (flatten_ff expr ff)

       | Filled (_, Value s) -> ()
       | Filled (_, Variable name) -> ()

       | Filled (_, Let (lhs, rhs, body)) ->
         let bound = match lhs with
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

let symbolic_execute (ff: feature_flag) (init: symtable) (ast: expr) : sym_store =
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
  sym_exec ~trace ~ff init_set ast; sym_store

let sym_store_to_yojson (st : sym_store) : Yojson.Safe.json =
  ht_to_json_dict st ~f:(fun syms ->
      `List (syms
             |> SymSet.to_list
             |> List.map ~f:(fun s -> `String s)))

let rec traverse ~(f: expr -> expr) (expr:expr) : expr =
  match expr with
  | Blank _ -> expr
  | Flagged (id, msg, setting, l, r) ->
    Flagged (id, msg, setting, f l, f r)
  | Filled (id, nexpr) ->
    Filled (id,
            (match nexpr with
             | Value _ -> nexpr
             | Variable _ -> nexpr

             | Let (lhs, rhs, body) ->
               Let (lhs, f rhs, f body)

             | If (cond, ifbody, elsebody) ->
               If (f cond, f ifbody, f elsebody)

             | FnCall (name, exprs) ->
               FnCall (name, List.map ~f exprs)

             | Lambda (vars, lexpr) ->
               Lambda (vars, f lexpr)

             | Thread exprs ->
               Thread (List.map ~f exprs)

             | FieldAccess (obj, field) ->
               FieldAccess (f obj, field)))




let rec set_expr ~(search: id) ~(replacement: expr) (expr: expr) : expr =
  let replace = set_expr ~search ~replacement in
  if search = to_id expr
  then replacement
  else
    traverse ~f:replace expr



