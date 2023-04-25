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
    |> List.map (fun (s : Secret.T) -> (s.name, DString s.value))
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
    | EString (id, segments) ->
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
                   | DString s -> return Ok(str + s)
                   | DIncomplete _
                   | DError _ -> return Error(result)
                   | dv ->
                     return
                       Error(
                         DError(
                           sourceID id,
                           "Expected string, got " + DvalReprDeveloper.toRepr dv
                         )
                       )
                 | Error dv, _ -> return Error dv
               })
             (Ok "")
      match result with
      | Ok str -> return DString(String.normalize str)
      | Error dv -> return dv
    | EBool (_id, b) -> return DBool b
    | EInt (_id, i) -> return DInt i
    | EFloat (_id, value) -> return DFloat value
    | EUnit _id -> return DUnit
    | EChar (_id, s) -> return DChar s

    | ELet (id, pattern, rhs, body) ->
      /// Returns `incomplete` traces for subpatterns of an unmatched pattern
      let traceIncompleteWithArgs id argPatterns =
        let argTraces =
          argPatterns
          |> List.map LetPattern.toID
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
        (pattern : LetPattern)
        : bool * List<string * Dval> * List<id * Dval> =
        match pattern with

        | LPVariable (id, varName) ->
          not (Dval.isFake dv), [ (varName, dv) ], [ (id, dv) ]

        | LPTuple (id, firstPat, secondPat, theRestPat) ->
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

      let! rhs = eval state st rhs
      let passes, newDefs, traces = checkPattern rhs pattern
      let newSymtable = Map.mergeFavoringRight st (Map.ofList newDefs)

      let traceDval onExecutionPath (id, dv) =
        state.tracing.traceDval onExecutionPath id dv

      match rhs with
      | DError _
      | DIncomplete _ ->
        List.iter (traceDval false) traces
        return rhs
      | _ ->
        if passes then
          List.iter (traceDval state.onExecutionPath) traces
        else
          List.iter (traceDval false) traces

        let! r = eval state newSymtable body
        return r

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


    | ERecord (id, typeName, fields) ->
      let availableTypes = ExecutionState.availableTypes state
      let typ = Map.tryFind typeName availableTypes
      let typeStr = FQTypeName.toString typeName
      match typ with
      | None ->
        return Dval.errSStr (sourceID id) $"There is no type named: {typeStr}"
      | Some (CustomType.Enum _) ->
        return
          Dval.errSStr (sourceID id) $"Expected a record but {typeStr} is an enum"
      | Some (CustomType.Record (expected1, expectedRest)) ->
        let expectedFields =
          (expected1 :: expectedRest) |> List.map (fun f -> f.name, f.typ) |> Map
        return!
          Ply.List.foldSequentially
            (fun r (k, expr) ->
              uply {
                if Dval.isFake r then
                  return r
                else if not (Map.containsKey k expectedFields) then
                  return
                    Dval.errSStr (sourceID id) $"Unexpected field `{k}` in {typeStr}"
                else
                  let! v = eval state st expr
                  if Dval.isFake v then
                    return v
                  else
                    match
                      TypeChecker.unify availableTypes (Map.find k expectedFields) v
                      with
                    | Ok () ->
                      match r with
                      | DRecord m -> return (DRecord(Map.add k v m))
                      | _ -> return Dval.errSStr (sourceID id) "Expected a record"
                    | Error e ->
                      return
                        Dval.errSStr (sourceID id) (TypeChecker.Error.toString e)
              })
            (DRecord Map.empty)
            fields


    | EDict (id, fields) ->
      return!
        Ply.List.foldSequentially
          (fun r (k, expr) ->
            uply {
              let! v = eval state st expr
              match (r, k, v) with
              | r, _, _ when Dval.isFake r -> return r
              | _, _, v when Dval.isFake v -> return v
              | _, "", _ -> return DError(sourceID id, "Dict key is empty")
              | DDict m, k, v -> return (DDict(Map.add k v m))
              // If we haven't got a DDict we're propagating an error so let it go
              | r, _, v -> return r
            })
          (DDict Map.empty)
          fields



    | EApply (id, fnTarget, typeArgs, exprs) ->
      let! args = Ply.List.mapSequentially (eval state st) exprs

      match fnTarget with
      | FnName name -> return! callFn state id name typeArgs (Seq.toList args)
      | FnTargetExpr e ->
        let! target = eval' state st e
        return! applyFn state target id args


    | EFieldAccess (id, e, field) ->
      let! obj = eval state st e

      if field = "" then
        return DError(sourceID id, "Field name is empty")
      else
        match obj with
        | DRecord o ->
          match Map.tryFind field o with
          | Some v -> return v
          | None -> return DError(sourceID id, $"No field named {field} in record")
        | DDB _ ->
          return
            DError(
              sourceID id,
              $"Attempting to access a field of something that isn't a record or dict, "
              + "(it's a Datastore. Use DB. standard library functions to interact with Datastores)."
            )
        | _ when Dval.isFake obj -> return obj
        | _ ->
          return
            DError(
              sourceID id,
              $"Attempting to access a field of something that isn't a record or dict, "
              + $"(it's a {DvalReprDeveloper.dvalTypeName obj})."
            )


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
        | MPInt (id, i) -> (dv = DInt i), [], [ (id, DInt i) ]
        | MPBool (id, b) -> (dv = DBool b), [], [ (id, DBool b) ]
        | MPChar (id, c) -> (dv = DChar c), [], [ (id, DChar c) ]
        | MPString (id, s) -> (dv = DString s), [], [ (id, DString s) ]
        | MPFloat (id, f) -> (dv = DFloat f), [], [ (id, DFloat f) ]
        | MPUnit (id) -> (dv = DUnit), [], [ (id, DUnit) ]

        | MPVariable (id, varName) ->
          not (Dval.isFake dv), [ (varName, dv) ], [ (id, dv) ]

        | MPEnum (id, caseName, fieldPats) ->
          match (caseName, fieldPats, dv) with
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

          | caseName, fieldPats, DEnum (_dTypeName, dCaseName, dFields) ->
            let fieldPats =
              match fieldPats with
              | [ MPTuple (_, first, second, theRest) ] -> first :: second :: theRest
              | pats -> pats

            if List.length dFields = List.length fieldPats && caseName = dCaseName then
              let (passResults, newVarResults, traceResults) =
                List.zip dFields fieldPats
                |> List.map (fun (dv, pat) -> checkPattern dv pat)
                |> List.unzip3

              let allPass = passResults |> List.forall identity
              let allVars = newVarResults |> List.collect identity
              let allSubTraces = traceResults |> List.collect identity

              if allPass then
                true, allVars, (id, dv) :: allSubTraces
              else
                false, allVars, traceIncompleteWithArgs id fieldPats @ allSubTraces
            else
              false, [], traceIncompleteWithArgs id fieldPats

          | _caseName, fieldPats, _dv ->
            false, [], traceIncompleteWithArgs id fieldPats

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
        | MPList (id, pats) ->
          match dv with
          | DList vals ->
            if List.length vals = List.length pats then
              let (passResults, newVarResults, traceResults) =
                List.zip vals pats
                |> List.map (fun (dv, pat) -> checkPattern dv pat)
                |> List.unzip3

              let allPass = passResults |> List.forall identity
              let allVars = newVarResults |> List.collect identity
              let allSubTraces = traceResults |> List.collect identity

              if allPass then
                true, allVars, (id, dv) :: allSubTraces
              else
                false, allVars, traceIncompleteWithArgs id pats @ allSubTraces
            else
              false, [], traceIncompleteWithArgs id pats
          | _ -> false, [], traceIncompleteWithArgs id pats

      // This is to avoid checking `state.tracing.realOrPreview = Real` below.
      // If RealOrPreview gets additional branches, reconsider what to do here.
      let isRealExecution =
        match state.tracing.realOrPreview with
        | Real -> true
        | Preview -> false

      // The value we're matching against
      let! matchVal = eval state st matchExpr

      let mutable hasMatched = false
      let mutable matchResult =
        // Even though we know it's fakeval, we still run through each pattern for analysis
        if Dval.isFake matchVal then matchVal else DError(sourceID id, "No match")

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


    | EEnum (id, typeName, caseName, fields) ->
      match typeName with
      | FQTypeName.Stdlib ({ modules = []; typ = "Option"; version = 0 }) ->
        match (caseName, fields) with
        | "Nothing", [] -> return DOption None
        | "Nothing", _ ->
          return
            Dval.errSStr
              (sourceID id)
              $"Option.Nothing expects 0 arguments but got {fields.Length}"
        | "Just", [ arg ] ->
          let! dv = (eval state st arg)
          return Dval.optionJust dv
        | "Just", _ ->
          return
            Dval.errSStr
              (sourceID id)
              $"Option.Just expects 1 argument but got {fields.Length}"
        | name, _ ->
          return Dval.errSStr (sourceID id) $"Invalid name for enum {name}"
      | FQTypeName.Stdlib ({ modules = []; typ = "Result"; version = 0 }) ->
        match (caseName, fields) with
        | "Ok", [ arg ] ->
          let! dv = eval state st arg
          return Dval.resultOk dv
        | "Ok", _ ->
          return
            Dval.errSStr
              (sourceID id)
              $"Result.Ok expects 1 argument but got {fields.Length}"
        | "Error", [ arg ] ->
          let! dv = eval state st arg
          return Dval.resultError dv
        | "Error", _ ->
          return
            Dval.errSStr
              (sourceID id)
              $"Result.Error expects 1 argument but got {fields.Length}"
        | name, _ ->
          return Dval.errSStr (sourceID id) $"Invalid name for enum {name}"
      | typeName ->
        // EEnumTODO: handle analysis/preview
        let! fields = Ply.List.mapSequentially (eval state st) fields

        // TYPESCLEANUP typecheck fields against the type
        match List.tryFind Dval.isFake fields with
        | Some fakeDval -> return fakeDval
        | None -> return DEnum(typeName, caseName, fields)
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
  (fn : Dval)
  (id : id)
  (args : List<Dval>)
  : DvalTask =
  uply {
    // Unwrap
    match fn with
    | DFnVal fnVal -> return! applyFnVal state fnVal args
    | other when Dval.isFake other -> return other
    | other ->
      return
        Dval.errSStr
          (SourceID(state.tlid, id))
          $"Expected a function value, got something else: {DvalReprDeveloper.toRepr other}"
  }

and applyFnVal
  (state : ExecutionState)
  (fnVal : FnValImpl)
  (argList : List<Dval>)
  : DvalTask =
  match fnVal with
  | Lambda l -> executeLambda state l argList

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
    let expectedLength = List.length l.parameters
    let actualLength = List.length args
    if expectedLength <> actualLength then
      Ply(
        DError(
          SourceNone,
          $"Expected {expectedLength} arguments, got {actualLength}"
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
  (typeArgs : List<TypeReference>)
  (argvals : List<Dval>)
  : DvalTask =
  uply {
    let sourceID = SourceID(state.tlid, callerID)

    let fn =
      match desc with
      | FQFnName.Stdlib _std ->
        // CLEANUP: do this when the libraries are loaded
        state.libraries.stdlibFns.TryFind desc |> Option.map builtInFnToFn
      | FQFnName.User u -> state.program.userFns.TryFind u |> Option.map userFnToFn
      | FQFnName.Package _pkg ->
        state.libraries.packageFns.TryFind desc |> Option.map packageFnToFn

    let! result =
      uply {
        match fn with
        // Functions which aren't implemented in the client may have results
        // available, otherwise they just return incomplete.
        | None ->
          let fnRecord = (state.tlid, desc, callerID)
          let fnResult = state.tracing.loadFnResult fnRecord argvals

          // TODO: in an old version, we executed the lambda with a fake value to
          // give enough livevalues for the editor to autocomplete. It may be worth
          // doing this again

          match fnResult with
          | Some (result, _ts) -> return result
          | None ->
            return
              DError(sourceID, $"Function {FQFnName.toString desc} is not found")

        | Some fn ->
          // ensure we have the expected # of typeArguments _and_ arguments values
          let expectedTypeParamLength = List.length fn.typeParams
          let expectedArgLength = List.length fn.parameters

          let actualTypeArgLength = List.length typeArgs
          let actualArgLength = List.length argvals

          let err errMsg = DError(sourceID, errMsg)

          if (expectedTypeParamLength = actualTypeArgLength)
             && (expectedArgLength = actualArgLength) then

            let args =
              fn.parameters
              |> List.map2 (fun dv p -> (p.name, dv)) argvals
              |> Map.ofList

            return! execFn state desc callerID fn typeArgs args
          else
            return
              err (
                $"{FQFnName.toString desc} has {expectedTypeParamLength} type parameters and {expectedArgLength} parameters, "
                + $"but here was called with {actualTypeArgLength} type arguments and {actualArgLength} arguments."
              )
      }
    return result
  }


and execFn
  (state : ExecutionState)
  (fnDesc : FQFnName.T)
  (id : id)
  (fn : Fn)
  (typeArgs : List<TypeReference>)
  (args : DvalMap)
  : DvalTask =
  uply {
    let sourceID = SourceID(state.tlid, id) in

    let typeErrorOrValue userTypes result =
      if Dval.isFake result then
        result
      else
        match TypeChecker.checkFunctionReturnType userTypes fn result with
        | Ok () -> result
        | Error err ->
          DError(
            sourceID,
            $"Type error(s) in return type: {TypeChecker.Error.toString err}"
          )

    if state.tracing.realOrPreview = Preview
       && not state.onExecutionPath
       && Set.contains fnDesc state.callstack then
      // Don't recurse (including transitively!) when previewing unexecuted paths
      // in the editor. If we do, we'll recurse forever and blow the stack.
      return DIncomplete(sourceID)
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

      match List.tryFind Dval.isFake arglist with
      | Some fake -> return fake
      | None ->
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
                  return! f (state, typeArgs, arglist)
                with
                | e ->
                  let context : Metadata =
                    [ "fn", fnDesc; "args", arglist; "id", id ]
                  return
                    match e with
                    | Errors.IncorrectArgs ->
                      Errors.incorrectArgsToDError sourceID fn arglist
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
        | PackageFunction body ->
          // This is similar to InProcess but also has elements of UserCreated.
          match TypeChecker.checkFunctionCall Map.empty fn typeArgs args with
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

            return result |> typeErrorOrValue (ExecutionState.availableTypes state)

          | Error err ->
            return
              DError(
                sourceID,
                ("Type error(s) in function parameters: "
                 + TypeChecker.Error.toString err)
              )
        | UserFunction (tlid, body) ->
          match
            TypeChecker.checkFunctionCall
              (ExecutionState.availableTypes state)
              fn
              typeArgs
              args
            with
          | Ok () ->
            state.tracing.traceTLID tlid
            // Don't execute user functions if it's preview mode and we have a result
            match (state.tracing.realOrPreview,
                   state.tracing.loadFnResult fnRecord arglist)
              with
            | Preview, Some (result, _ts) -> return result
            | _ ->
              let state = { state with tlid = tlid }
              let! result = eval state argsWithGlobals body
              state.tracing.storeFnResult fnRecord arglist result

              return result |> typeErrorOrValue (ExecutionState.availableTypes state)
          | Error err ->
            return
              DError(
                sourceID,
                ("Type error(s) in function parameters: "
                 + TypeChecker.Error.toString err)
              )
  }
