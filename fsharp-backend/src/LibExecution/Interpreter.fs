module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharpPlus

open Prelude
open RuntimeTypes

let globalsFor (state : ExecutionState) : Symtable =
  let secrets =
    state.secrets
    |> List.map (fun (s : Secret.T) -> (s.secretName, DStr s.secretValue))
    |> Map.ofList

  let dbs = Map.map (fun _ (db : DB.T) -> DDB db.name) state.dbs
  Map.union secrets dbs


let withGlobals (state : ExecutionState) (symtable : Symtable) : Symtable =
  let globals = globalsFor state
  Map.union globals symtable



// fsharplint:disable FL0039
let rec eval (state : ExecutionState) (st : Symtable) (e : Expr) : DvalTask =
  let sourceID id = SourceID(state.tlid, id)
  let incomplete id = Value(DIncomplete(SourceID(state.tlid, id)))

  taskv {
    match e with
    | EBlank id -> return! (incomplete id)
    | EPartial (_, expr) -> return! eval state st expr
    | ELet (_id, lhs, rhs, body) ->
        // FSTODO: match with ast.ml
        let! rhs = eval state st rhs
        let st = st.Add(lhs, rhs)
        return! (eval state st body)
    | EString (_id, s) -> return (DStr(s.Normalize()))
    | EBool (_id, b) -> return DBool b
    | EInteger (_id, i) -> return DInt i
    | EFloat (_id, value) -> return DFloat value
    | ENull _id -> return DNull
    | ECharacter (_id, s) -> return DChar s
    | EList (_id, exprs) ->
        // We ignore incompletes but not error rail.
        // TODO: Other places where lists are created propagate incompletes
        // instead of ignoring, this is probably a mistake.
        let! results = Prelude.map_s (eval state st) exprs

        let filtered =
          List.filter (fun (dv : Dval) -> not (Dval.isIncomplete dv)) results
        // CLEANUP: why do we only find errorRail, and not errors. Seems like
        // a mistake
        match List.tryFind (fun (dv : Dval) -> Dval.isErrorRail dv) filtered with
        | Some er -> return er
        | None -> return (DList filtered)

    | EVariable (id, name) ->
        // FSTODO: match ast.ml
        match (st.TryFind name, state.context) with
        | None, Preview ->
            // The trace is wrong/we have a bug -- we guarantee to users that
            // variables they can lookup have been bound. However, we
            // shouldn't crash out here when running analysis because it gives
            // a horrible user experience
            return! incomplete id
        | None, Real ->
            return Dval.errSStr (sourceID id) $"There is no variable named: {name}"
        | Some other, _ -> return other
    | ERecord (id, pairs) ->
        let skipEmptyKeys =
          pairs
          |> List.choose
               (function
               | ("", e) -> None
               | k, e -> Some(k, e))
        // FSTODO: we actually want to stop on the first incomplete/error/etc, thing, not do them all.
        let! (resolved : List<string * Dval>) =
          Prelude.map_s
            (fun (k, v) ->
              taskv {
                let! dv = eval state st v
                return (k, dv)
              })
            skipEmptyKeys

        return Dval.interpreterObj resolved
    | EApply (id, fnVal, exprs, inPipe, ster) ->
        let! fnVal = eval state st fnVal
        let! args = Prelude.map_s (eval state st) exprs
        return! (applyFn state id fnVal (Seq.toList args) inPipe ster)
    | EFQFnValue (id, desc) -> return DFnVal(FnName(desc))
    | EFieldAccess (id, e, field) ->
        let! obj = eval state st e

        let result =
          match obj with
          | DObj o ->
              if field = "" then
                DIncomplete(sourceID id)
              else
                Map.tryFind field o |> Option.defaultValue DNull
          | DIncomplete _ -> obj
          | DErrorRail _ -> obj
          | DError _ -> obj // differs from ocaml, but produces an Error either way
          | x ->
              let actualType =
                match Dval.toType x with
                | TDB _ ->
                    "it's a Datastore. Use DB:: standard library functions to interact with Datastores"
                | tipe -> $"it's a {DvalRepr.typeToDeveloperReprV0 tipe}"

              DError(
                sourceID id,
                "Attempting to access a field of something that isn't a record or dict, ("
                + actualType
                + ")."
              )

        return! Value result
    | EFeatureFlag (id, cond, oldcode, newcode) ->
        // True gives newexpr, unlike in If statements
        //
        // In If statements, we use a false/null as false, and anything else is
        // true. But this won't work for feature flags. If statements are built
        // as you build you code, with no existing users. But feature flags are
        // created when you have users and don't want to break your code. As a
        // result, anything that isn't an explicitly signalling to use the new
        // code, should use the old code:
        // - errors should be ignored: use old code
        // - incompletes should be ignored: use old code
        // - errorrail should not be propaged: use old code
        // - values which are "truthy" in if statements are not truthy here:
        // imagine you are writing the FF cond and you get a list or object,
        // and you're about to do some other work on it. Should we immediately
        // start serving the new code to all your traffic? No. So only `true`
        // gets new code.

        let! cond =
          // under no circumstances should this cause code to fail
          try
            eval state st cond
          with e -> Value(DBool false)

        match cond with
        | DBool true ->
            // FSTODO
            (* preview st oldcode *)
            return! eval state st newcode
        // FSTODO
        | DIncomplete _
        | DErrorRail _
        | DError _ ->
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

        let executeMatch
          (new_defs : (string * Dval) list)
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

        let traceNonMatch
          (st : DvalMap)
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

        let rec matchAndExecute
          dv
          (builtUpTraces : (id * Dval) list)
          (pattern, expr)
          =
          (* Compare `dv` to `pattern`, and execute the rhs `expr` of any
           * matches. Tracks whether a branch has already been executed and
           * will exceute later matches in preview mode.  Ensures all patterns
           * and branches are properly traced.  Recurse on partial matches
           * (constructors); builtUpTraces is the set of traces that have been
           * built up by recursing: they can only be matched when the pattern
           * is ready to match. *)
          match pattern with
          | PInteger (pid, i) ->
              let v = DInt i

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
          | PFloat (pid, v) ->
              let v = DFloat v

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
              if Dval.isFake dv then
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
                   //     Value(DError(UndefinedConstructor name))
                   // FSTODO
                   // traceNonMatch st expr builtUpTraces pid error
                   // FSTODO
                   (* Trace each argument too. TODO: recurse *)
                   // List.iter args (fun pat ->
                   //   let id = Libshared.FluidPattern.toID pat
                   //   trace false id (incomplete id))
                   ())

        let! matchVal = eval state st matchExpr

        List.iter
          (fun (pattern, expr) -> matchAndExecute matchVal [] (pattern, expr))
          cases

        return! !matchResult

    | EIf (_id, cond, thenbody, elsebody) ->
        let! cond = eval state st cond

        match cond with
        | DBool (false)
        | DNull -> return! eval state st elsebody
        | _ when Dval.isFake cond -> return cond
        // CLEANUP: I dont know why I made these always true
        | _ -> return! eval state st thenbody
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
        | name, _ ->
            return Dval.errSStr (sourceID id) $"Invalid name for constructor {name}"
  }

// Unwrap the dval, which we expect to be a function, and error if it's not
and applyFn
  (state : ExecutionState)
  (id : id)
  (fn : Dval)
  (args : List<Dval>)
  (isInPipe : IsInPipe)
  (ster : SendToRail)
  : DvalTask =
  taskv {
    let sourceID = SourceID(state.tlid, id)

    match fn with
    | DFnVal fnVal -> return! applyFnVal state id fnVal args isInPipe ster
    // Incompletes are allowed in pipes
    | DIncomplete _ when isInPipe = InPipe ->
        return Option.defaultValue fn (List.tryHead args)
    | other ->
        return
          Dval.errSStr
            sourceID
            $"Expected a function value, got something else: {other}"
  }

and applyFnVal
  (state : ExecutionState)
  (id : id)
  (fnVal : FnValImpl)
  (argList : List<Dval>)
  (isInPipe : IsInPipe)
  (ster : SendToRail)
  : DvalTask =
  taskv {
    let sourceID = SourceID(state.tlid, id)

    let isErrorAllowed =
      match fnVal with
      | FnName name when name = FQFnName.stdlibName "Bool" "isError" 0 -> true
      | _ -> false

    match List.tryFind (fun (dv : Dval) -> Dval.isFake dv) argList with
    // If one of the arglist is a fake value used as a marker, return it instead
    // of executing.
    | Some dv when not isErrorAllowed ->
        match dv with
        // That is, unless it's an incomplete in a pipe. In a pipe, we treat
        // the entire expression as a blank, and skip it, returning the input
        // (first) value to be piped into the next statement instead. *)
        | DIncomplete _ when isInPipe = InPipe ->
            return Option.defaultValue dv (List.tryHead argList)
        | _ -> return dv
    | None
    | Some _ ->
        match fnVal with
        | Lambda l ->
            let parameters = List.map snd l.parameters
            // One of the reasons to take a separate list of params and args is to
            // provide this error message here. We don't have this information in
            // other places, and the alternative is just to provide incompletes
            // with no context
            let paramLength = List.length l.parameters
            let argLength = List.length argList

            if paramLength <> argLength then
              return
                Dval.errSStr
                  sourceID
                  $"Expected {paramLength} arguments, got {argLength}"
            else
              // FSTODO
              // List.iter bindings (fun ((id, paramName), dv) ->
              //     state.trace state.on_execution_path id dv) ;
              let paramSyms = List.zip parameters argList |> Map
              // paramSyms is higher priority
              let newSymtable = Map.union paramSyms l.symtable
              return! eval state newSymtable l.body
        | (FnName desc) ->
            let fn =
              match desc.owner, desc.package, desc.module_ with
              | "dark", "stdlib", _ ->
                  state.functions.TryFind desc |> Option.map builtInFnToFn
              | "", "", "" ->
                  state.userFns.TryFind desc.function_ |> Option.map userFnToFn
              | _ -> state.packageFns.TryFind desc |> Option.map packageFnToFn

            let! (result : Dval) =
              match fn with
              | None -> fstodo $"support builtin function {desc}"
              | Some fn ->
                  taskv {
                    // FSTODO: if an argument is an error rail, return it
                    try
                      // FSTODO: all the behaviour in AST.exec_fn
                      match fn.fn with
                      | StdLib fnval ->
                          // evaluate this here so that the exception gets caught here
                          return! fnval (state, argList)
                      | UserFunction (tlid, body) ->
                          // FSTODO: check arg length
                          let argsWithGlobals =
                            let args =
                              fn.parameters
                              |> List.map (fun p -> p.name)
                              |> fun ps -> List.zip ps argList
                              |> Map.ofList

                            Map.union (globalsFor state) args in

                          // FSTODO
                          // let checkedVals =
                          //   TypeChecker.checkFunctionCall state.userTypes fn args
                          match Ok() with
                          | Ok () ->
                              // FSTODO
                              state.traceTLID tlid
                              // Don't execute user functions if it's preview mode and we have a result
                              match 1 with
                              // FSTODO
                              // (state.context,
                              //         state.loadFnResult sfrDesc arglist) with
                              // FSTODO
                              // | Preview, Some (result, _ts) ->
                              //     return Dval.unwrapFromErrorrail result
                              | _ ->
                                  // It's okay to execute user functions in both Preview and Real contexts,
                                  // But in Preview we might not have all the data we need
                                  // FSTODO
                                  // state.store_fn_arguments tlid args

                                  let state = { state with tlid = tlid }
                                  let! result = eval state argsWithGlobals body
                                  // FSTODO
                                  // state.store_fn_result sfr_desc arglist result

                                  return result |> Dval.unwrapFromErrorRail
                          // |> typeErrorOrValue state.userTypes
                          | Error errs ->
                              return
                                Dval.errSStr
                                  sourceID
                                  "Type error(s) in function parameters: "
                      // FSTODO
                      // ^ Type_checker.Error.list_to_string errs

                      | _ ->
                          fstodo $"support other function type {fn.fn}"
                          return DIncomplete SourceNone
                    with
                    | Errors.FakeValFoundInQuery dv -> return dv
                    | Errors.DBQueryException e ->
                        return Dval.errStr (Errors.queryCompilerErrorTemplate + e)
                    | Errors.StdlibException (Errors.StringError msg) ->
                        return Dval.errSStr sourceID msg
                    | Errors.StdlibException Errors.IncorrectArgs ->
                        let paramLength = List.length fn.parameters
                        let argLength = List.length argList

                        if paramLength <> argLength then
                          return
                            Dval.errSStr
                              sourceID
                              ($"{fn.name} has {paramLength} parameters,"
                               + $" but here was called with {argLength} arguments.")

                        else
                          let invalid =
                            List.zip fn.parameters argList
                            |> List.filter
                                 (fun (p, a) ->
                                   Dval.toType a <> p.typ && p.typ <> TAny)

                          match invalid with
                          | [] ->
                              return
                                Dval.errSStr
                                  sourceID
                                  $"unknown error calling {fn.name}"
                          | (p, actual) :: _ ->
                              let msg = Errors.incorrectArgsMsg (fn.name) p actual
                              return Dval.errSStr sourceID msg
                    | Errors.StdlibException Errors.FunctionRemoved ->
                        return
                          Dval.errSStr sourceID $"{fn.name} was removed from Dark"
                    | Errors.StdlibException (Errors.FakeDvalFound dv) -> return dv

                  }

            if ster = Rail then
              match Dval.unwrapFromErrorRail result with
              | DOption (Some v) -> return v
              | DResult (Ok v) -> return v
              | v when Dval.isFake v -> return v
              // There should only be DOptions and DResults here, but hypothetically we got
              // something else, they would go on the error rail too.
              | other -> return DErrorRail other
            else
              return result

  }
