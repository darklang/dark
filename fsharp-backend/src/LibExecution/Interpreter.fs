module LibExecution.Interpreter

open Thoth.Json.Net
open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Prelude
open RuntimeTypes
open SharedTypes


// fsharplint:disable FL0039
let rec eval (state : ExecutionState) (st : Symtable.T) (e : Expr) : DvalTask =
  let sourceID id = SourceID(state.tlid, id)
  let incomplete id = Value(DFakeVal(DIncomplete(SourceID(state.tlid, id))))

  taskv {
    match e with
    | EBlank id -> return! (incomplete id)
    | EPartial (_, expr) -> return! eval state st expr
    | ELet (_id, lhs, rhs, body) ->
        // FSTODO: match with ast.ml
        let! rhs = eval state st rhs
        let st = st.Add(lhs, rhs)
        return! (eval state st body)
    | EString (_id, s) -> return (DStr s)
    | EBool (_id, b) -> return DBool b
    | EInteger (_id, i) -> return Dval.int i
    | EFloat (_id, whole, fractional) -> return Dval.float whole fractional
    | ENull _id -> return DNull
    | ECharacter (_id, s) -> return DChar s
    | EList (_id, exprs) ->
        // We ignore incompletes but not error rail.
        // TODO: Other places where lists are created propagate incompletes
        // instead of ignoring, this is probably a mistake.
        let! results = Prelude.map_s (eval state st) exprs

        let filtered = List.filter (fun (dv : Dval) -> not dv.isIncomplete) results
        // TODO: why do we only find errorRail, and not errors. Seems like
        // a mistake
        match List.tryFind (fun (dv : Dval) -> dv.isErrorRail) filtered with
        | Some er -> return er
        | None -> return (DList filtered)

    | EVariable (_id, name) ->
        // FSTODO: match ast.ml
        return Symtable.get st name
    | ERecord (id, pairs) ->
        let skipEmptyKeys =
          pairs
          |> List.choose (function
               | ("", e) -> None
               | k, e -> Some(k, e))
        // FSTODO: we actually want to stop on the first incomplete/error/etc, thing, not do them all.
        let! (resolved : List<string * Dval>) =
          Prelude.map_s (fun (k, v) ->
            taskv {
              let! dv = eval state st v
              return (k, dv)
            }) skipEmptyKeys

        return (resolved
                // allow users to edit code safely
                |> List.filter (fun (k, v : Dval) -> not v.isIncomplete)
                |> Dval.obj)
    | EFnCall (id, fnVal, exprs, ster) ->
        let! fnVal = eval state st fnVal
        let! args = Prelude.map_s (eval state st) exprs
        return! (callFn state fnVal (Seq.toList args) ster)
    | EFQFnValue (id, desc) -> return DFnVal(FQFnName(desc))
    | EFieldAccess (id, _, _) ->
        failwith "todo"
        return! incomplete id
    | EFeatureFlag (id, cond, oldcode, newcode) ->
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

        let! cond =
          (* under no circumstances should this cause code to fail *)
          try
            eval state st cond
          with e -> Value(DBool false)

        match cond with
        | DBool true ->
            // FSTODO
            (* preview st oldcode *)
            return! eval state st newcode
        // FSTODO
        | DFakeVal _ ->
            // FSTODO
            (* preview st newcode *)
            return! eval state st oldcode
        | _ ->
            // FSTODO
            (* preview st newcode *)
            return! eval state st oldcode

    // FSTODO
    | ELambda (_id, parameters, body) ->
        return DFnVal(Lambda { symtable = st; parameters = parameters; body = body })
    | EMatch (id, matchExpr, cases) ->
        let hasMatched = ref false
        let matchResult = ref (incomplete id)

        let executeMatch (new_defs : (string * Dval) list)
                         (traces : (id * Dval) list)
                         (st : DvalMap)
                         (expr : Expr)
                         : unit =
          (* Once a pattern is matched, this function is called to execute its
           * `expr`. It tracks whether this is the first pattern to execute,
           * and calls preview if it is not. Handles calling trace on the
           * traces that have been collected by pattern matching. *)
          let newVars = Map.ofList new_defs
          let newSt = Map.union newVars st
          if !hasMatched then
            ()
          // FSTODO
          (* We matched, but we've already matched a pattern previously *)
          // List.iter (fun (id, dval) -> trace false id dval) traces
          // FSTODO
          // preview newSt expr
          else
            // FSTODO
            // List.iter (fun (id, dval) -> trace on_execution_path id dval) traces
            hasMatched := true
            matchResult := eval state newSt expr

        let traceIncompletes traces = ()
        // FSTODO
        // List.iter traces (fun (id, _) -> trace false id (incomplete id))

        let traceNonMatch (st : DvalMap)
                          (expr : Expr)
                          (traces : (id * Dval) list)
                          (id : id)
                          (value : Dval)
                          : unit =
          // FSTODO
          // preview st expr
          // FSTODO
          // traceIncompletes traces
          // FSTODO
          // trace false id value
          ()

        let rec matchAndExecute dv (builtUpTraces : (id * Dval) list) (pattern, expr) =
          (* Compare `dv` to `pattern`, and execute the rhs `expr` of any
           * matches. Tracks whether a branch has already been executed and
           * will exceute later matches in preview mode.  Ensures all patterns
           * and branches are properly traced.  Recurse on partial matches
           * (constructors); builtUpTraces is the set of traces that have been
           * built up by recursing: they can only be matched when the pattern
           * is ready to match. *)
          match pattern with
          | PInteger (pid, i) ->
              let v = Dval.int i
              if v = dv then
                executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else
                traceNonMatch st expr builtUpTraces pid v
          | PBool (pid, bool) ->
              let v = DBool bool
              if v = dv then
                executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else
                traceNonMatch st expr builtUpTraces pid v
          | PCharacter (pid, c) ->
              let v = DChar(c)
              if v = dv then
                executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else
                traceNonMatch st expr builtUpTraces pid v

          | PString (pid, str) ->
              let v = DStr(str)
              if v = dv then
                executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else
                traceNonMatch st expr builtUpTraces pid v
          | PFloat (pid, whole, fraction) ->
              let v = Dval.float whole fraction
              if v = dv then
                executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else
                traceNonMatch st expr builtUpTraces pid v
          | PNull (pid) ->
              let v = DNull
              if v = dv then
                executeMatch [] ((pid, v) :: builtUpTraces) st expr
              else
                traceNonMatch st expr builtUpTraces pid v
          | PVariable (pid, v) ->
              (* only matches allowed values *)
              if dv.isFake then
                traceNonMatch st expr builtUpTraces pid dv
              else
                executeMatch [ (v, dv) ] ((pid, dv) :: builtUpTraces) st expr
          | PBlank (_pid) ->
              (* never matches *)
              // FSTODO: is this the same in the AST?
              // traceNonMatch st expr builtUpTraces pid (incomplete pid)
              ()
          | PConstructor (pid, name, args) ->
              (match (name, args, dv) with
               | "Just", [ p ], DOption (Some v)
               | "Ok", [ p ], DResult (Ok v)
               | "Error", [ p ], DResult (Error v) ->
                   matchAndExecute v ((pid, dv) :: builtUpTraces) (p, expr)
               | "Nothing", [], DOption None ->
                   executeMatch [] ((pid, dv) :: builtUpTraces) st expr
               | "Nothing", [], _ ->
                   traceNonMatch st expr builtUpTraces pid (DOption None)
               | _ ->
                   // let error =
                   //   if List.contains name [ "Just"; "Ok"; "Error"; "Nothing" ] then
                   //     incomplete pid
                   //   else
                   //     Value(DFakeVal(DError(UndefinedConstructor name)))
                   // FSTODO
                   // traceNonMatch st expr builtUpTraces pid error
                   // FSTODO
                   (* Trace each argument too. TODO: recurse *)
                   // List.iter args (fun pat ->
                   //   let id = Libshared.FluidPattern.toID pat
                   //   trace false id (incomplete id))
                   ())

        let! matchVal = eval state st matchExpr

        List.iter (fun (pattern, expr) -> matchAndExecute matchVal [] (pattern, expr))
          cases

        return! !matchResult

    | EIf (_id, cond, thenbody, elsebody) ->
        let! cond = eval state st cond

        match cond with
        | DBool (true) -> return! eval state st thenbody
        | DBool (false) -> return! eval state st elsebody
        | cond -> return (err (CondWithNonBool cond))
    | EConstructor (id, name, args) ->
        match (name, args) with
        | "Nothing", [] -> return DOption None
        | "Just", [ arg ] ->
            let! dv = (eval state st arg)
            return Dval.optionJust dv
        | "Ok", [ arg ] ->
            let! dv = eval state st arg
            return Dval.resultOk dv
        | "Error", [ arg ] ->
            let! dv = eval state st arg
            return Dval.resultError dv
        | _ -> return DFakeVal(DError(UndefinedConstructor name))
  }

// Unwrap the dval, which we expect to be a function, and error if it's not
and callFn (state : ExecutionState)
           (fn : Dval)
           (args : List<Dval>)
           (ster : SendToRail)
           : DvalTask =
  taskv {
    match fn with
    | DFnVal fnVal -> return! callFnVal state fnVal args ster
    | DFakeVal _ -> return fn
    | other -> return errStr "Expected a function value, got something else"
  }

and callFnVal (state : ExecutionState)
              (fnVal : FnValImpl)
              (args : List<Dval>)
              (ster : SendToRail)
              : DvalTask =
  match fnVal with
  | Lambda l ->
      taskv {
        // If one of the args is fake value used as a marker, return it instead of
        // executing. This is the same behaviour as in fn calls. *)
        match List.tryFind (fun (dv : Dval) -> dv.isFake) args with
        | Some dv -> return dv
        | None ->
            let parameters = List.map snd l.parameters
            // One of the reasons to take a separate list of params and args is to
            // provide this error message here. We don't have this information in
            // other places, and the alternative is just to provide incompletes
            // with no context
            if List.length l.parameters <> List.length args then
              return err (LambdaCalledWithWrongCount(args, parameters))
            else
              // FSTODO
              // let bindings = List.zip_exn params args in
              // List.iter bindings (fun ((id, paramName), dv) ->
              //     state.trace state.on_execution_path id dv) ;
              let paramSyms = List.zip parameters args |> Map
              // paramSyms is higher priority
              let newSymtable = Map.union paramSyms l.symtable
              return! eval state newSymtable l.body
      }
  | (FQFnName desc) ->
      taskv {
        let! result =
          match state.functions.TryFind desc with
          | None -> Value(err (NotAFunction desc))
          | Some fn ->
              match List.tryFind (fun (dv : Dval) -> dv.isFake) args with
              | Some special -> Value special
              | None ->
                  try
                    fn.fn (state, args)
                  with
                  | RuntimeException rte -> Value(err rte)
                  | FnCallException FnFunctionRemoved ->
                      Value(err (FunctionRemoved fn.name))
                  | FnCallException FnWrongTypes ->
                      Value
                        (err (FnCalledWithWrongTypes(fn.name, args, fn.parameters)))
                  | FakeDvalException dval -> Value(dval)

        if ster = Rail then
          match result.unwrapFromErrorRail with
          | DOption (Some v) -> return v
          | DResult (Ok v) -> return v
          | DFakeVal _ as f -> return f
          // There should only be DOptions and DResults here, but hypothetically we got
          // something else, they would go on the error rail too.
          | other -> return DFakeVal(DErrorRail other)
        else
          return result
      }
