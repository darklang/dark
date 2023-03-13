/// Interprets Dark expressions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Prelude
open RuntimeTypes
open Prelude

/// Gathers any global data (Secrets, DBs, etc.)
/// that may be needed to evaluate an expression
let globalsFor (state : ExecutionState) : Symtable =
  let secrets =
    state.program.secrets
    |> List.map (fun (s : Secret.T) -> (s.name, DStr s.value))
    |> Map.ofList

  let dbs = Map.map (fun _ (db : DB.T) -> DDB db.name) state.program.dbs

  Map.mergeFavoringLeft secrets dbs


let withGlobals (state : ExecutionState) (symtable : Symtable) : Symtable =
  let globals = globalsFor state
  Map.mergeFavoringRight globals symtable



// fsharplint:disable FL0039

/// Interprets an expression and reduces to a Dark value
/// (or task that should result in such)
let rec eval' (state : ExecutionState) (st : Symtable) (e : Expr) : DvalTask =
  // Design doc for execution results and previews:
  // https://www.notion.so/darklang/Live-Value-Branching-44ee705af61e416abed90917e34da48e
  // TODO remove link from code or avail document - it is either gone or hidden behind login
  let sourceID id = SourceID(state.tlid, id)
  let incomplete id = DIncomplete(SourceID(state.tlid, id))

  /// This function ensures any value not on the execution path is evaluated.
  let preview st expr : Ply<unit> =
    uply {
      if state.tracing.realOrPreview = Preview then
        let state = { state with onExecutionPath = false }
        let! (_result : Dval) = eval state st expr
        return ()
    }


  uply {
    match e with
    | EString (_id, segments) ->
      let! result =
        segments
        |> Ply.List.foldSequentially
             (fun builtUpString seg ->
               uply {
                 match builtUpString, seg with
                 | Ok str, StringText (text) -> return Ok(str + text)
                 | Ok str, StringInterpolation (expr) ->
                   let! result = eval state st expr
                   match result with
                   | DStr s -> return Ok(str + s)
                   | DIncomplete _
                   | DError _ -> return Error(result)
                   | dv ->
                     return
                       Error(
                         DError(
                           sourceID _id,
                           "Expected string, got " + DvalReprDeveloper.toRepr dv
                         )
                       )
                 | Error dv, _ -> return Error(dv)
               })
             (Ok "")
      match result with
      | Ok str -> return DStr(String.normalize str)
      | Error dv -> return dv
    | EBool (_id, b) -> return DBool b
    | EInteger (_id, i) -> return DInt i
    | EFloat (_id, value) -> return DFloat value
    | EUnit _id -> return DUnit
    | ECharacter (_id, s) -> return DChar s

    | ELet (id, pattern, rhs, body) ->
      match pattern with
      | LPVariable (_id, lhs) ->
        if lhs = "" then
          let! _ = preview st rhs
          return DError(sourceID id, "Variable name in `let` is empty")
        else
          let! rhs = eval state st rhs
          match rhs with
          | DError _
          | DIncomplete _ ->
            let st = Map.add lhs rhs st
            do! preview st body
            return rhs
          | _ ->
            let st = Map.add lhs rhs st
            return! eval state st body

    | EList (_id, exprs) ->
      let! results = Ply.List.mapSequentially (eval state st) exprs

      match List.tryFind Dval.isFake results with
      | Some fakeDval -> return fakeDval
      | None -> return DList results


    | ETuple (_id, first, second, theRest) ->

      let! firstResult = eval state st first
      let! secondResult = eval state st second
      let! otherResults = Ply.List.mapSequentially (eval state st) theRest

      let allResults = [ firstResult; secondResult ] @ otherResults

      // If any element in a tuple is fake (blank, error, etc.),
      // we don't want to return a tuple, but rather the fake val.
      match List.tryFind Dval.isFake allResults with
      | Some fakeDval -> return fakeDval
      | None -> return DTuple(firstResult, secondResult, otherResults)


    | EVariable (id, name) ->
      match st.TryFind name with
      | None ->
        return Dval.errSStr (sourceID id) $"There is no variable named: {name}"
      | Some other -> return other


    | ERecord (id, pairs) ->
      return!
        Ply.List.foldSequentially
          (fun r (k, expr) ->
            uply {
              let! v = eval state st expr
              match (r, k, v) with
              | r, _, _ when Dval.isFake r -> return r
              | _, _, v when Dval.isFake v -> return v
              | _, "", _ -> return DError(sourceID id, "Record key is empty")
              | DObj m, k, v -> return (DObj(Map.add k v m))
              // If we haven't got a DObj we're propagating an error so let it go
              | r, _, v -> return r
            })
          (DObj(Map.empty))
          pairs


    | EApply (id, fnVal, exprs, inPipe) ->
      let! fnVal = eval state st fnVal
      let! args = Ply.List.mapSequentially (eval state st) exprs
      let! result = applyFn state id fnVal (Seq.toList args) inPipe

      do
        // Pipes have been removed at this point, but the editor still needs to
        // show a value for the pipe
        // CLEANUP: instead of saving this, fetch it in the right place (the
        // last pipe entry) in the editor
        match inPipe with
        | InPipe pipeID ->
          state.tracing.traceDval state.onExecutionPath pipeID result
        | NotInPipe -> ()
      return result


    | EFQFnValue (_id, desc) -> return DFnVal(FnName(desc))


    | EFieldAccess (id, e, field) ->
      let! obj = eval state st e

      if field = "" then
        return DError(sourceID id, "Field name is empty")
      else
        match obj with
        | DObj o ->
          match Map.tryFind field o with
          | Some v -> return v
          | None -> return DError(sourceID id, $"No field named {field} in record")
        | _ when Dval.isFake obj -> return obj
        | x ->
          let actualType =
            match Dval.toType x with
            | TDB _ ->
              "it's a Datastore. Use DB:: standard library functions to interact with Datastores"
            | tipe -> $"it's a {DvalReprDeveloper.typeName tipe}"

          return
            DError(
              sourceID id,
              $"Attempting to access a field of something that isn't a record or dict, ({actualType})."
            )



    | EFeatureFlag (_id, cond, oldcode, newcode) ->
      // Unlike If statements that check their condition for 'truthiness,'
      // (considering the condition true if it evaluates to anything other
      // than false/null,) Feature Flags require that the condition evaluates
      // to exactly `True`.
      //
      // If statements are built as you build you code, with no existing users.
      // But feature flags are created when you have users and don't want to
      // break your code. As a result, anything that isn't an explicitly
      // signalling to use the new code, should use the old code:
      // - errors should be ignored: use old code
      // - incompletes should be ignored: use old code
      // - values which are "truthy" in if statements are not truthy here
      //
      // Imagine you are writing the FF cond and you get a list or object,
      // and you're about to do some other work on it. Should we immediately
      // start serving the new code to all your traffic? No. So only `true`
      // gets new code.

      let! conditionResult =
        // under no circumstances should this cause code to fail
        uply {
          try
            return! eval state st cond
          with
          | _ -> return DBool false
        }

      if conditionResult = DBool true then
        do! preview st oldcode
        return! eval state st newcode
      else
        do! preview st newcode
        return! eval state st oldcode


    | ELambda (_id, parameters, body) ->
      if state.tracing.realOrPreview = Preview then
        // In case this never gets executed, add default analysis results
        parameters
        |> List.iter (fun (id, _name) ->
          state.tracing.traceDval false id (DIncomplete(sourceID id)))

        // Since we return a DBlock, it's contents may never be
        // executed. So first we execute with no context to get some
        // live values.
        let previewST =
          parameters
          |> List.choose (fun (id, name) ->
            if name = "" then None else Some(name, DIncomplete(sourceID id)))
          |> Map.ofList
        do! preview previewST body

      let parameters =
        parameters
        |> List.choose (fun param ->
          match param with
          | _, "" -> None
          | id, name -> Some(id, name))

      // It is the responsibility of wherever executes the DBlock to pass in
      // args and execute the body.
      return DFnVal(Lambda { symtable = st; parameters = parameters; body = body })


    | EMatch (id, matchExpr, cases) ->
      /// Returns `incomplete` traces for subpatterns of an unmatched pattern
      let traceIncompleteWithArgs id argPatterns =
        let argTraces =
          argPatterns
          |> List.map MatchPattern.toID
          |> List.map (fun pId -> (pId, incomplete pId))

        (id, incomplete id) :: argTraces

      /// Does the dval 'match' the given pattern?
      ///
      /// Returns:
      /// - whether or not the expr 'matches' the pattern
      /// - new vars (name * value)
      /// - traces
      let rec checkPattern
        (dv : Dval)
        (pattern : MatchPattern)
        : bool * List<string * Dval> * List<id * Dval> =
        match pattern with
        | MPInteger (id, i) -> (dv = DInt i), [], [ (id, DInt i) ]
        | MPBool (id, b) -> (dv = DBool b), [], [ (id, DBool b) ]
        | MPCharacter (id, c) -> (dv = DChar c), [], [ (id, DChar c) ]
        | MPString (id, s) -> (dv = DStr s), [], [ (id, DStr s) ]
        | MPFloat (id, f) -> (dv = DFloat f), [], [ (id, DFloat f) ]
        | MPUnit (id) -> (dv = DUnit), [], [ (id, DUnit) ]

        | MPVariable (id, varName) ->
          not (Dval.isFake dv), [ (varName, dv) ], [ (id, dv) ]

        | MPConstructor (id, name, args) ->
          match (name, args, dv) with
          | "Nothing", [], v -> (v = DOption None), [], [ (id, DOption None) ]

          | "Just", [ p ], DOption (Some v)
          | "Ok", [ p ], DResult (Ok v)
          | "Error", [ p ], DResult (Error v) ->
            let (passes, newVars, traces) = checkPattern v p

            if passes then
              true, newVars, (id, dv) :: traces
            else
              false, newVars, traceIncompleteWithArgs id [ p ] @ traces

          // Trace this with incompletes to avoid type errors
          | "Just", [ p ], _
          | "Ok", [ p ], _
          | "Error", [ p ], _ ->
            let pID = MatchPattern.toID p
            let (_, newVars, traces) = checkPattern (incomplete pID) p
            false, newVars, traceIncompleteWithArgs id [] @ traces

          | _name, argPatterns, _dv ->
            let traces = traceIncompleteWithArgs id argPatterns
            false, [], traces


        | MPTuple (id, firstPat, secondPat, theRestPat) ->
          let allPatterns = firstPat :: secondPat :: theRestPat

          match dv with
          | DTuple (first, second, theRest) ->
            let allVals = first :: second :: theRest

            if List.length allVals = List.length allPatterns then
              let (passResults, newVarResults, traceResults) =
                List.zip allVals allPatterns
                |> List.map (fun (dv, pat) -> checkPattern dv pat)
                |> List.unzip3

              let allPass = passResults |> List.forall identity
              let allVars = newVarResults |> List.collect identity
              let allSubTraces = traceResults |> List.collect identity

              if allPass then
                true, allVars, (id, dv) :: allSubTraces
              else
                false, allVars, traceIncompleteWithArgs id allPatterns @ allSubTraces
            else
              false, [], traceIncompleteWithArgs id allPatterns
          | _ -> false, [], traceIncompleteWithArgs id allPatterns

      // This is to avoid checking `state.tracing.realOrPreview = Real` below.
      // If RealOrPreview gets additional branches, reconsider what to do here.
      let isRealExecution =
        match state.tracing.realOrPreview with
        | Real -> true
        | Preview -> false

      // The value we're matching against
      let! matchVal = eval state st matchExpr

      let mutable hasMatched = false
      let mutable matchResult = incomplete id

      for (pattern, rhsExpr) in cases do
        if hasMatched && isRealExecution then
          ()
        else
          let passes, newDefs, traces = checkPattern matchVal pattern
          let newSymtable = Map.mergeFavoringRight st (Map.ofList newDefs)

          if not hasMatched && passes then
            traces
            |> List.iter (fun (id, dv) ->
              state.tracing.traceDval state.onExecutionPath id dv)

            let! r = eval state newSymtable rhsExpr
            matchResult <- r
            hasMatched <- true
          else if isRealExecution then
            // "Real" evaluations don't need to persist non-matched traces
            ()
          else
            // If we're "previewing" (analysis), persist traces for all patterns
            traces |> List.iter (fun (id, dv) -> state.tracing.traceDval false id dv)
            do! preview newSymtable rhsExpr

      return matchResult


    | EIf (id, cond, thenbody, elsebody) ->
      match! eval state st cond with
      | DBool false ->
        do! preview st thenbody
        return! eval state st elsebody
      | DBool true ->
        let! result = eval state st thenbody
        do! preview st elsebody
        return result
      | cond when Dval.isFake cond ->
        do! preview st thenbody
        do! preview st elsebody
        return cond
      | _ ->
        do! preview st thenbody
        do! preview st elsebody
        return DError(sourceID id, "If only supports Booleans")


    | EOr (id, left, right) ->
      match! eval state st left with
      | DBool true ->
        do! preview st right
        return DBool true
      | DBool false ->
        match! eval state st right with
        | DBool true -> return DBool true
        | DBool false -> return DBool false
        | right when Dval.isFake right -> return right
        | _ -> return DError(sourceID id, "|| only supports Booleans")
      | left when Dval.isFake left ->
        do! preview st right
        return left
      | _ ->
        do! preview st right
        return DError(sourceID id, "|| only supports Booleans")


    | EAnd (id, left, right) ->
      match! eval state st left with
      | DBool false ->
        do! preview st right
        return DBool false
      | DBool true ->
        match! eval state st right with
        | DBool true -> return DBool true
        | DBool false -> return DBool false
        | right when Dval.isFake right -> return right
        | _ -> return DError(sourceID id, "&& only supports Booleans")
      | left when Dval.isFake left ->
        do! preview st right
        return left
      | _ ->
        do! preview st right
        return DError(sourceID id, "&& only supports Booleans")


    | EConstructor (id, typeName, caseName, fields) ->
      match typeName with
      | None ->
        match (caseName, fields) with
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
      | Some typeName ->
        // EConstructorTODO: handle analysis/preview
        let! fields = Ply.List.mapSequentially (eval state st) fields

        // EConstructorTODO: reconsider (stole this from DList)
        match List.tryFind Dval.isFake fields with
        | Some fakeDval -> return fakeDval
        | None -> return DUserEnum(typeName, caseName, fields)
  }

/// Interprets an expression and reduces to a Dark value
/// (or a task that results in a dval)
and eval (state : ExecutionState) (st : Symtable) (e : Expr) : DvalTask =
  uply {
    let! (result : Dval) = eval' state st e
    state.tracing.traceDval state.onExecutionPath (Expr.toID e) result
    return result
  }

/// Unwrap the dval, which we expect to be a function, and error if it's not
and applyFn
  (state : ExecutionState)
  (id : id)
  (fn : Dval)
  (args : List<Dval>)
  (isInPipe : IsInPipe)
  : DvalTask =
  let sourceID = SourceID(state.tlid, id)

  // Unwrap
  match fn, isInPipe with
  | DFnVal fnVal, _ -> applyFnVal state id fnVal args isInPipe
  // Incompletes are allowed in pipes
  | DIncomplete _, InPipe _ -> Ply(Option.defaultValue fn (List.tryHead args))
  | _other, InPipe _ ->
    // CLEANUP: this matches the old pipe behaviour, no need to preserve that
    Ply(Option.defaultValue fn (List.tryHead args))
  | other, _ ->
    Ply(
      Dval.errSStr
        sourceID
        $"Expected a function value, got something else: {DvalReprDeveloper.toRepr other}"
    )


and applyFnVal
  (state : ExecutionState)
  (callerID : id)
  (fnVal : FnValImpl)
  (argList : List<Dval>)
  (isInPipe : IsInPipe)
  : DvalTask =
  match fnVal with
  | Lambda l -> executeLambda state l argList
  // CLEANUP: fetch the name when we load it, then we won't need to find it again
  | FnName name -> callFn state callerID name argList isInPipe

and executeLambda
  (state : ExecutionState)
  (l : LambdaImpl)
  (args : List<Dval>)
  : DvalTask =

  // If one of the args is fake value used as a marker, return it instead of
  // executing. This is the same behaviour as in fn calls.
  let firstMarker = List.tryFind Dval.isFake args

  match firstMarker with
  | Some dv -> Ply dv
  | None ->
    let parameters = List.map snd l.parameters
    // One of the reasons to take a separate list of params and args is to
    // provide this error message here. We don't have this information in
    // other places, and the alternative is just to provide incompletes
    // with no context
    if List.length l.parameters <> List.length args then
      Ply(
        DError(
          SourceNone,
          $"Expected {List.length l.parameters} arguments, got {List.length args}"
        )
      )
    else
      List.iter
        (fun ((id, _), dv) -> state.tracing.traceDval state.onExecutionPath id dv)
        (List.zip l.parameters args)

      let paramSyms = List.zip parameters args |> Map
      // paramSyms is higher priority
      let newSymtable = Map.mergeFavoringRight l.symtable paramSyms

      eval state newSymtable l.body

and callFn
  (state : ExecutionState)
  (callerID : id)
  (desc : FQFnName.T)
  (argvals : Dval list)
  (isInPipe : IsInPipe)
  : DvalTask =
  uply {
    let sourceID id = SourceID(state.tlid, id) in

    let fn =
      match desc with
      | FQFnName.Stdlib _std ->
        // CLEANUP: do this when the libraries are loaded
        state.libraries.stdlib.TryFind desc |> Option.map builtInFnToFn
      | FQFnName.User name ->
        state.program.userFns.TryFind name |> Option.map userFnToFn
      | FQFnName.Package _pkg ->
        state.libraries.packageFns.TryFind desc |> Option.map packageFnToFn

    let! result =
      uply {
        match fn with
        // Functions which aren't implemented in the client may have results
        // available, otherwise they just return incomplete.
        | None ->
          let fnRecord = (state.tlid, desc, callerID) in
          let fnResult = state.tracing.loadFnResult fnRecord argvals in
          // In the case of DB::query (and friends), we want to backfill
          // the lambda's livevalues, as the lambda was never actually
          // executed. We hack this is here as we have no idea what this
          // abstraction might look like in the future.
          if state.tracing.realOrPreview = Preview && FQFnName.isDBQueryFn desc then
            match argvals with
            | [ DDB dbname; DFnVal (Lambda b) ] ->
              let sample =
                match fnResult with
                | Some (DList (sample :: _), _) -> sample
                | _ ->
                  Map.find dbname state.program.dbs
                  |> (fun (db : DB.T) -> db.cols)
                  |> List.map (fun (field, _) -> (field, DIncomplete SourceNone))
                  |> Map.ofList
                  |> DObj

              let! (_ : Dval) = executeLambda state b [ sample ]
              ()
            | _ -> ()

          match fnResult with
          | Some (result, _ts) -> return result
          | _ -> return DIncomplete(sourceID callerID)

        | Some fn ->
          // equalize length
          let expectedLength = List.length fn.parameters in
          let actualLength = List.length argvals in

          if expectedLength = actualLength then
            let args =
              fn.parameters
              |> List.map2 (fun dv p -> (p.name, dv)) argvals
              |> Map.ofList

            return! execFn state desc callerID fn args isInPipe
          else
            return
              DError(
                sourceID callerID,
                $"{FQFnName.toString desc} has {expectedLength} parameters, but here was called"
                + $" with {actualLength} arguments."
              )
      }
    return result
  }


and execFn
  (state : ExecutionState)
  (fnDesc : FQFnName.T)
  (id : id)
  (fn : Fn)
  (args : DvalMap)
  (isInPipe : IsInPipe)
  : DvalTask =
  uply {
    let sourceID = SourceID(state.tlid, id) in

    let typeErrorOrValue userTypes result =
      (* https://www.notion.so/darklang/What-should-happen-when-the-return-type-is-wrong-533f274f94754549867fefc554f9f4e3 *)
      match TypeChecker.checkFunctionReturnType userTypes fn result with
      | Ok () -> result
      | Error errs ->
        DError(
          sourceID,
          $"Type error(s) in return type: {TypeChecker.Error.listToString errs}"
        )

    if state.tracing.realOrPreview = Preview
       && not state.onExecutionPath
       && Set.contains fnDesc state.callstack then
      // Don't recurse (including transitively!) when previewing unexecuted paths
      // in the editor. If we do, we'll recurse forever and blow the stack.
      return DIncomplete(SourceID(state.tlid, id))
    else
      // CLEANUP: optimization opportunity
      let state =
        { state with
            executingFnName = Some fnDesc
            callstack = Set.add fnDesc state.callstack }

      // CLEANUP: why do we rebuild the arglist when we already had it before?
      let arglist =
        fn.parameters
        |> List.map (fun (p : Param) -> p.name)
        |> List.choose (fun key -> Map.tryFind key args)

      let argsWithGlobals = withGlobals state args

      let fnRecord = (state.tlid, fnDesc, id) in

      let badArg =
        List.tryFind
          (fun expr ->
            match expr with
            // CLEANUP: does anyone use Bool.isError?
            | DError _ when
              fnDesc = FQFnName.Stdlib
                         { module_ = "Bool"; function_ = "isError"; version = 0 }
              ->
              // TODO: state.notify here, then check a month later, to evaluate and then delete
              false
            | DError _
            | DIncomplete _ -> true
            | _ -> false)
          arglist

      match badArg, isInPipe with
      | Some (DIncomplete _src), InPipe _ ->
        // That is, unless it's an incomplete in a pipe. In a pipe, we treat
        // the entire expression as a blank, and skip it, returning the input
        // (first) value to be piped into the next statement instead.
        return List.head arglist
      | Some (DIncomplete src), _ -> return DIncomplete src
      | Some (DError _ as err), _ -> return err
      | _ ->
        match fn.fn with
        | StdLib f ->
          if state.tracing.realOrPreview = Preview && fn.previewable <> Pure then
            match state.tracing.loadFnResult fnRecord arglist with
            | Some (result, _ts) -> return result
            | None -> return DIncomplete sourceID
          else
            let! result =
              uply {
                try
                  return! f (state, arglist)
                with
                | e ->
                  let context : Metadata =
                    [ "fn", fnDesc; "args", arglist; "id", id; "isInPipe", isInPipe ]
                  return
                    match e with
                    | Errors.IncorrectArgs ->
                      Errors.incorrectArgsToDError sourceID fn arglist
                    | Errors.FakeDvalFound dv ->
                      // We don't expect to see fakeDvals inside functions, so let's
                      // learn where they are so that they can be removed.
                      if fn.deprecated = NotDeprecated then
                        let context = (context @ [ "dval", dv ])
                        state.notify state "fakedval found" context
                      dv
                    | (:? CodeException
                    | :? GrandUserException) as e ->
                      // There errors are created by us, within the libraries, so they are
                      // safe to show to users (but not grandusers)
                      Dval.errSStr sourceID e.Message
                    | e ->
                      // CLEANUP could we show the user the execution id here?
                      state.reportException state context e
                      // These are arbitrary errors, and could include sensitive
                      // information, so best not to show it to the user. If we'd
                      // like to show it to the user, we should catch it and give
                      // them a known safe error.
                      Dval.errSStr sourceID Exception.unknownErrorMessage
              }
            // there's no point storing data we'll never ask for
            if fn.previewable <> Pure then
              state.tracing.storeFnResult fnRecord arglist result

            return result
        | PackageFunction (_tlid, body) ->
          // This is similar to InProcess but also has elements of UserCreated.
          match TypeChecker.checkFunctionCall Map.empty fn args with
          | Ok () ->
            let! result =
              match (state.tracing.realOrPreview,
                     state.tracing.loadFnResult fnRecord arglist)
                with
              | Preview, Some (result, _ts) -> Ply(result)
              | Preview, None when fn.previewable <> Pure ->
                Ply(DIncomplete sourceID)
              | _ ->
                uply {
                  // It's okay to execute user functions in both Preview and
                  // Real contexts, But in Preview we might not have all the
                  // data we need

                  // TODO: We don't munge `state.tlid` like we do in
                  // UserFunction, which means there might be `id`
                  // collisions between AST nodes. Munging `state.tlid`
                  // would not save us from tlid collisions either.
                  // tl;dr, executing a package function may result in
                  // trace data being associated with the wrong
                  // handler/call site.
                  let! result = eval state argsWithGlobals body

                  state.tracing.storeFnResult fnRecord arglist result

                  return result |> typeErrorOrValue Map.empty
                }
            // For now, always store these results
            state.tracing.storeFnResult fnRecord arglist result

            return result |> typeErrorOrValue state.program.userTypes

          | Error errs ->
            return
              DError(
                sourceID,
                ("Type error(s) in function parameters: "
                 + TypeChecker.Error.listToString errs)
              )
        | UserFunction (tlid, body) ->
          match TypeChecker.checkFunctionCall state.program.userTypes fn args with
          | Ok () ->
            state.tracing.traceTLID tlid
            // Don't execute user functions if it's preview mode and we have a result
            match (state.tracing.realOrPreview,
                   state.tracing.loadFnResult fnRecord arglist)
              with
            | Preview, Some (result, _ts) -> return result
            | _ ->
              // It's okay to execute user functions in both Preview and Real contexts,
              // But in Preview we might not have all the data we need
              state.tracing.storeFnArguments tlid args

              let state = { state with tlid = tlid }
              let! result = eval state argsWithGlobals body
              state.tracing.storeFnResult fnRecord arglist result

              return result |> typeErrorOrValue state.program.userTypes
          | Error errs ->
            return
              DError(
                sourceID,
                ("Type error(s) in function parameters: "
                 + TypeChecker.Error.listToString errs)
              )
  }
