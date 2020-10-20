module LibExecution.Interpreter

open Thoth.Json.Net
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Runtime

// fsharplint:disable FL0039
let rec eval (state : ExecutionState) (st : Symtable.T) (e : Expr) : DvalTask =
  let tryFindFn desc = state.functions.TryFind(desc)
  let sourceID id = SourceID(state.tlid, id)
  let incomplete id = Plain(DFakeVal(DIncomplete(SourceID(state.tlid, id))))
  match e with
  | EBlank id -> incomplete id
  | EPartial (_, _, expr)
  | ERightPartial (_, _, expr)
  | ELeftPartial (_, _, expr) -> eval state st expr
  | EPipeTarget id -> incomplete id
  | ELet (_id, lhs, rhs, body) ->
      // FSTODO: match with ast.ml
      let rhs = eval state st rhs
      rhs.bind (fun rhs ->
        let st = st.Add(lhs, rhs)
        eval state st body)
  | EString (_id, s) -> Plain(DStr s)
  | EBool (_id, b) -> Plain(DBool b)
  | EInteger (_id, i) -> Plain(DInt(System.Numerics.BigInteger.Parse i))
  | EFloat (_id, whole, fractional) ->
      // FSTODO - add sourceID to errors
      try
        Plain(DFloat(float $"{whole}.{fractional}"))
      with _ -> Plain(err (InvalidFloatExpression(whole, fractional)))
  | ENull _id -> Plain(DNull)
  | ECharacter (_id, s) -> Plain(DChar s)
  | EList (_id, exprs) ->
      // We ignore incompletes but not error rail.
      // TODO: Other places where lists are created propagate incompletes
      // instead of ignoring, this is probably a mistake.
      Task
        (task {
          let! results = Runtime.map_s (eval state st) exprs
          let filtered = List.filter (fun (dv : Dval) -> not dv.isIncomplete) results
          // TODO: why do we only find errorRail, and not errors. Seems like
          // a mistake
          match List.tryFind (fun (dv : Dval) -> dv.isErrorRail) filtered with
          | Some er -> return er
          | None -> return (DList filtered)
         })
  | EVariable (_id, name) ->
      // FSTODO: match ast.ml
      Plain(Symtable.get st name)
  | ERecord (id, _) -> incomplete id // FSTODO
  | EFnCall (_id, desc, exprs, ster) ->
      (match tryFindFn desc with
       | Some fn ->
           Task
             (task {
               let! args = Runtime.map_s (eval state st) exprs
               return! (callFn state fn (Seq.toList args)).toTask()
              })
       | None -> Plain(err (NotAFunction desc)))
  | EBinOp (_id, desc, arg1, arg2, ster) ->
      (match tryFindFn desc with
       | Some fn ->
           let t1 = eval state st arg1
           let t2 = eval state st arg2
           t1.bind2 t2 (fun arg1 arg2 -> callFn state fn [ arg1; arg2 ])
       | None -> Plain(err (NotAFunction desc)))
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
      (* let cond = eval state st cond *)
      (* cond.bind (function *)
      (*   | DBool (true) -> eval state st thenbody *)
      (*   | DBool (false) -> eval state st elsebody *)
      (*   | cond -> Task(task { return (err (CondWithNonBool cond)) })) *)

      let cond =
        (* under no circumstances should this cause code to fail *)
        try
          eval state st cond
        with e -> Plain(DBool false)

      (cond.bind (function
        | DBool true ->
            // FSTODO
            (* preview st oldcode *)
            eval state st newcode
        // FSTODO
        | DFakeVal _ ->
            // FSTODO
            (* preview st newcode *)
            eval state st oldcode
        | _ ->
            // FSTODO
            (* preview st newcode *)
            eval state st oldcode))

  // FSTODO
  | ELambda (_id, parameters, body) ->
      Plain(DLambda({ symtable = st; parameters = parameters; body = body }))
  | EIf (_id, cond, thenbody, elsebody) ->
      let cond = eval state st cond
      cond.bind (function
        | DBool (true) -> eval state st thenbody
        | DBool (false) -> eval state st elsebody
        | cond -> Task(task { return (err (CondWithNonBool cond)) }))
  | EConstructor (id, name, args) ->
      (match (name, args) with
       | "Nothing", [] -> Plain(DOption None)
       | "Just", [ arg ] ->
           Task
             (task {
               let! dv = (eval state st arg).toTask()
               return DOption(Some dv)
              })
       | "Ok", [ arg ] ->
           Task
             (task {
               let! dv = (eval state st arg).toTask()
               return DResult(Ok dv)
              })
       | "Error", [ arg ] ->
           Task
             (task {
               let! dv = (eval state st arg).toTask()
               return DResult(Error dv)
              })
       | _ -> Plain(DFakeVal(DError(UndefinedConstructor name))))



and callFn (state : ExecutionState) (fn : BuiltInFn) (args : List<Dval>) : DvalTask =
  match List.tryFind (fun (dv : Dval) -> dv.isFake) args with
  | Some special -> Plain special
  | None ->
      try
        fn.fn (state, args)
      with
      | RuntimeException rte -> Plain(err rte)
      | FnCallException FnFunctionRemoved -> Plain(err (FunctionRemoved fn.name))
      | FnCallException FnWrongTypes ->
          Plain(err (FnCalledWithWrongTypes(fn.name, args, fn.parameters)))
      | FakeDvalException dval -> Plain(dval)

and eval_lambda (state : ExecutionState)
                (l : Runtime.LambdaBlock)
                (args : List<Dval>)
                : DvalTask =
  (* If one of the args is fake value used as a marker, return it instead of
   * executing. This is the same behaviour as in fn calls. *)
  match List.tryFind (fun (dv : Dval) -> dv.isFake) args with
  | Some dv -> Plain(dv)
  | None ->
      let parameters = List.map snd l.parameters
      (* One of the reasons to take a separate list of params and args is to
       * provide this error message here. We don't have this information in
       * other places, and the alternative is just to provide incompletes
       * with no context *)
      if List.length l.parameters <> List.length args then
        Plain(err (LambdaCalledWithWrongCount(args, parameters)))
      else
        // FSTODO
        // let bindings = List.zip_exn params args in
        // List.iter bindings ~f:(fun ((id, paramName), dv) ->
        //     state.trace ~on_execution_path:state.on_execution_path id dv) ;
        // #FSTODO is this being overwritten correctly? so latest items win?
        let newSymtable = List.zip parameters args |> Map |> Map.union l.symtable

        eval state newSymtable l.body
