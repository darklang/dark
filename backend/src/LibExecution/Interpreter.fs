/// Interprets Dark expressions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Prelude
open RuntimeTypes

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
let rec eval'
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (st : Symtable)
  (e : Expr)
  : DvalTask =
  // Design doc for execution results and previews:
  // https://www.notion.so/darklang/Live-Value-Branching-44ee705af61e416abed90917e34da48e
  // TODO remove link from code or avail document - it is either gone or hidden behind login
  let sourceID id = SourceID(state.tlid, id)
  let incomplete id = DIncomplete(SourceID(state.tlid, id))
  let err id msg = Dval.errSStr (sourceID id) msg

  /// This function ensures any value not on the execution path is evaluated.
  let preview (tst : TypeSymbolTable) (st : Symtable) (expr : Expr) : Ply<unit> =
    uply {
      if state.tracing.realOrPreview = Preview then
        let state = { state with onExecutionPath = false }
        let! (_result : Dval) = eval state tst st expr
        return ()
    }

  let recordMaybe
    (types : Types)
    (typeName : TypeName.T)
    // TypeName, typeParam list, fully-resolved (except for typeParam) field list
    : Ply<Option<TypeName.T * List<string> * List<string * TypeReference>>> =
    let rec inner (typeName : TypeName.T) =
      uply {
        match! Types.find typeName types with
        | Some({ typeParams = outerTypeParams
                 definition = TypeDeclaration.Alias(TCustomType(innerTypeName,
                                                                outerTypeArgs)) }) ->
          // Here we have found an alias, so we need to combine the type's
          // typeArgs with the aliased type's typeParams.
          // Eg in
          //   type Var = Result<Int, String>
          // we need to combine Var's typeArgs (<Int, String>) with Result's
          // typeParams (<`Ok, `Error>)
          //
          // To do this, we use typeArgs from the alias definition
          // (outerTypeArgs) and apply them to the aliased type
          // (innerTypeName)'s params (which are returned from the lookup and
          // used as innerTypeParams below).
          // Example: suppose we have
          //   type Outer<'a> = Inner<'a, Int>
          //   type Inner<'x, 'y> = { x : 'x; y : 'y }
          // The recursive search for Inner will get:
          //   innerTypeName = "Inner"
          //   innerTypeParams = ["x"; "y"]
          //   fields = [("x", TVar "x"); ("y", TVar "y")]
          // The Outer definition provides:
          //   outerTypeArgs = [TVar "a"; TInt]
          // We combine this with innerTypeParams to get:
          //   fields = [("x", TVar "a"); ("b", TInt)]
          //   outerTypeParams = ["a"]
          // So the effective result of this is:
          //   type Outer<'a> = { x : 'a; y : Int }
          let! next = inner innerTypeName
          return
            next
            |> Option.map (fun (innerTypeName, innerTypeParams, fields) ->
              (innerTypeName,
               outerTypeParams,
               fields
               |> List.map (fun (k, v) ->
                 (k, Types.substitute innerTypeParams outerTypeArgs v))))

        | Some({ typeParams = typeParams
                 definition = TypeDeclaration.Record fields }) ->
          return
            Some(
              typeName,
              typeParams,
              fields |> NEList.toList |> List.map (fun f -> f.name, f.typ)
            )
        | _ -> return None
      }
    inner typeName

  let enumMaybe
    (types : Types)
    (typeName : TypeName.T)
    : Ply<Option<TypeName.T * List<string> * NEList<TypeDeclaration.EnumCase>>> =
    let rec inner (typeName : TypeName.T) =
      uply {
        match! Types.find typeName types with
        | Some({ typeParams = outerTypeParams
                 definition = TypeDeclaration.Alias(TCustomType(innerTypeName,
                                                                outerTypeArgs)) }) ->
          let! next = inner innerTypeName
          return
            next
            |> Option.map (fun (innerTypeName, innerTypeParams, cases) ->
              (innerTypeName,
               outerTypeParams,
               cases
               |> NEList.map (fun (c : TypeDeclaration.EnumCase) ->
                 { c with
                     fields =
                       List.map
                         (Types.substitute innerTypeParams outerTypeArgs)
                         c.fields })))
        | Some({ typeParams = typeParams; definition = TypeDeclaration.Enum cases }) ->
          return Some(typeName, typeParams, cases)
        | _ -> return None
      }
    inner typeName


  uply {
    match e with
    | EString(id, segments) ->
      let! result =
        segments
        |> Ply.List.foldSequentially
          (fun builtUpString seg ->
            uply {
              match builtUpString, seg with
              | Ok str, StringText(text) -> return Ok(str + text)
              | Ok str, StringInterpolation(expr) ->
                let! result = eval state tst st expr
                match result with
                | DString s -> return Ok(str + s)
                | DIncomplete _
                | DError _ -> return Error(result)
                | dv ->
                  let msg = "Expected string, got " + DvalReprDeveloper.toRepr dv
                  return Error(err id msg)
              | Error dv, _ -> return Error dv
            })
          (Ok "")
      match result with
      | Ok str -> return DString(String.normalize str)
      | Error dv -> return dv
    | EBool(_id, b) -> return DBool b
    | EInt(_id, i) -> return DInt i
    | EFloat(_id, value) -> return DFloat value
    | EUnit _id -> return DUnit
    | EChar(_id, s) -> return DChar s
    | EConstant(id, name) ->
      match name with
      | FQName.UserProgram c ->
        match state.program.constants.TryFind c with
        | None -> return err id $"There is no user defined constant named: {name}"
        | Some constant -> return constant.body
      | FQName.BuiltIn c ->
        match state.builtIns.constants.TryFind c with
        | None -> return err id $"There is no builtin constant named: {name}"
        | Some constant -> return constant.body
      | FQName.Package c ->
        match! state.packageManager.getConstant c with
        | None -> return err id $"There is no package constant named: {name}"
        | Some constant -> return constant.body
      | FQName.Unknown names ->
        let name = String.concat "." names
        return err id $"There is no constant named: {name}"

    | ELet(id, pattern, rhs, body) ->
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

        | LPVariable(id, varName) ->
          not (Dval.isFake dv), [ (varName, dv) ], [ (id, dv) ]

        | LPTuple(id, firstPat, secondPat, theRestPat) ->
          let allPatterns = firstPat :: secondPat :: theRestPat

          match dv with
          | DTuple(first, second, theRest) ->
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

      let! rhs = eval state tst st rhs
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

        let! r = eval state tst newSymtable body
        return r

    | EList(_id, exprs) ->
      let! results = Ply.List.mapSequentially (eval state tst st) exprs
      return Dval.list Unknown results


    | ETuple(_id, first, second, theRest) ->

      let! firstResult = eval state tst st first
      let! secondResult = eval state tst st second
      let! otherResults = Ply.List.mapSequentially (eval state tst st) theRest

      let allResults = [ firstResult; secondResult ] @ otherResults

      // If any element in a tuple is fake (blank, error, etc.),
      // we don't want to return a tuple, but rather the fake val.
      match List.tryFind Dval.isFake allResults with
      | Some fakeDval -> return fakeDval
      | None -> return DTuple(firstResult, secondResult, otherResults)


    | EVariable(id, name) ->
      match st.TryFind name with
      | None -> return err id $"There is no variable named: {name}"
      | Some other -> return other


    | ERecord(id, typeName, fields) ->
      let typeStr = TypeName.toString typeName
      let types = ExecutionState.availableTypes state
      let! typ = Types.find typeName types

      match! recordMaybe types typeName with
      | None ->
        match typ with
        | None -> return err id $"There is no type named {typeStr}"
        | Some({ definition = TypeDeclaration.Enum _ }) ->
          return err id $"Expected a record but {typeStr} is an enum"
        | _ -> return err id $"Expected a record but {typeStr} is something else"
      | Some(aliasTypeName, typeParams, expected) ->
        let expectedFields = Map expected
        let! result =
          Ply.List.foldSequentially
            (fun r (k, expr) ->
              uply {
                if Dval.isFake r then
                  do! preview tst st expr
                  return r
                else if not (Map.containsKey k expectedFields) then
                  do! preview tst st expr
                  return err id $"Unexpected field `{k}` in {typeStr}"
                else
                  let! v = eval state tst st expr
                  if Dval.isFake v then
                    return v
                  else
                    match r with
                    | DRecord(typeName, original, m) ->
                      if Map.containsKey k m then
                        return err id $"Duplicate field `{k}` in {typeStr}"
                      else
                        let fieldType = Map.find k expectedFields
                        let context =
                          TypeChecker.RecordField(original, k, fieldType, None)
                        let check =
                          TypeChecker.unify context types Map.empty fieldType v
                        match! check with
                        | Ok tst ->
                          // VTTODO stop swallowing the typeSymbolTable
                          return DRecord(typeName, original, Map.add k v m)
                        | Error e ->
                          return err id (Errors.toString (Errors.TypeError(e)))
                    | _ -> return err id "Expected a record in typecheck"
              })
            (DRecord(aliasTypeName, typeName, Map.empty)) // use the alias name here
            fields
        match result with
        | DRecord(_, _, fields) ->
          if Map.count fields = Map.count expectedFields then
            return result
          else
            let expectedKeys = Map.keys expectedFields
            let key = Seq.find (fun k -> not (Map.containsKey k fields)) expectedKeys
            return err id $"Missing field `{key}` in {typeStr}"
        | _ -> return result

    | ERecordUpdate(id, baseRecord, updates) ->
      let! baseRecord = eval state tst st baseRecord
      match baseRecord with
      | DRecord(typeName, _, _) ->
        let typeStr = TypeName.toString typeName
        let types = ExecutionState.availableTypes state
        match! recordMaybe types typeName with
        | None ->
          match! Types.find typeName types with
          | None -> return err id $"There is no type named {typeStr}"
          | Some({ definition = TypeDeclaration.Enum _ }) ->
            return err id $"Expected a record but {typeStr} is an enum"
          | _ -> return err id $"Expected a record but {typeStr} is something else"
        | Some(_, _, expected) ->
          let expectedFields = Map expected
          return!
            Ply.List.foldSequentially
              (fun r (k, expr) ->
                uply {
                  let! v = eval state tst st expr
                  match r, k, v with
                  | r, _, _ when Dval.isFake r -> return r
                  | _, _, v when Dval.isFake v -> return v
                  | _, "", _ -> return err id $"Empty key for value `{v}`"
                  | _, _, _ when not (Map.containsKey k expectedFields) ->
                    return err id $"Unexpected field `{k}` in {typeStr}"
                  | DRecord(typeName, original, m), k, v ->
                    let fieldType = Map.find k expectedFields
                    let context =
                      TypeChecker.RecordField(typeName, k, fieldType, None)
                    match!
                      TypeChecker.unify context types Map.empty fieldType v
                    with
                    | Ok tst ->
                      // VTTODO stop swallowing the typeSymbolTable
                      return DRecord(typeName, original, Map.add k v m)
                    | Error e ->
                      return err id (Errors.toString (Errors.TypeError(e)))
                  | _ ->
                    return
                      err id "Expected a record but {typeStr} is something else"
                })
              baseRecord
              updates
      | _ -> return err id "Expected a record in record update"

    | EDict(_, fields) ->
      return!
        Ply.List.foldSequentially
          (fun r (k, expr) ->
            uply {
              let! v = eval state tst st expr
              match (r, k, v) with
              | r, _, _ when Dval.isFake r -> return r
              | _, _, v when Dval.isFake v -> return v
              | DDict m, k, v -> return (DDict(Map.add k v m))
              // If we haven't got a DDict we're propagating an error so let it go
              | r, _, v -> return r
            })
          (DDict Map.empty)
          fields


    | EFnName(_id, name) -> return DFnVal(NamedFn name)

    | EApply(id, fnTarget, typeArgs, exprs) ->
      match! eval' state tst st fnTarget with
      | DFnVal fnVal ->
        let! args = Ply.List.mapSequentially (eval state tst st) exprs
        return! applyFnVal state id fnVal typeArgs args
      | other when Dval.isFake other -> return other
      | other ->
        return
          Dval.errSStr
            (SourceID(state.tlid, id))
            $"Expected a function value, got something else: {DvalReprDeveloper.toRepr other}"


    | EFieldAccess(id, e, field) ->
      let! obj = eval state tst st e

      if field = "" then
        return err id "Field name is empty"
      else
        match obj with
        | DRecord(_, typeName, o) ->
          match Map.tryFind field o with
          | Some v -> return v
          | None ->
            let typeStr = TypeName.toString typeName
            return err id $"No field named {field} in {typeStr} record"
        | DDB _ ->
          let msg =
            $"Attempting to access field '{field}' of a Datastore "
            + "(use `DB.*` standard library functions to interact with Datastores. "
            + "Field access only work with records)"
          return err id msg
        | _ when Dval.isFake obj -> return obj
        | _ ->
          let typeName = obj |> DvalReprDeveloper.toTypeName

          let msg =
            $"Attempting to access field '{field}' of a "
            + $"{typeName} (field access only works with records)"
          return err id msg


    | ELambda(_id, parameters, body) ->
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
        do! preview tst previewST body

      let parameters =
        parameters
        |> List.choose (fun param ->
          match param with
          | _, "" -> None
          | id, name -> Some(id, name))

      // It is the responsibility of wherever executes the DBlock to pass in
      // args and execute the body.
      return
        DFnVal(
          Lambda
            { typeSymbolTable = tst
              symtable = st
              parameters = parameters
              body = body }
        )

    (*
      let myFn<'a> (x: 'a) (y: 'b): Int =
        List.map (fun (c: 'c) -> Json.serialize<'c> c)
        List.map (fun _ -> Json.serialize<'b> y)
    *)


    | EMatch(id, matchExpr, cases) ->
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
        | MPInt(id, i) -> (dv = DInt i), [], [ (id, DInt i) ]
        | MPBool(id, b) -> (dv = DBool b), [], [ (id, DBool b) ]
        | MPChar(id, c) -> (dv = DChar c), [], [ (id, DChar c) ]
        | MPString(id, s) -> (dv = DString s), [], [ (id, DString s) ]
        | MPFloat(id, f) -> (dv = DFloat f), [], [ (id, DFloat f) ]
        | MPUnit(id) -> (dv = DUnit), [], [ (id, DUnit) ]

        | MPVariable(id, varName) ->
          not (Dval.isFake dv), [ (varName, dv) ], [ (id, dv) ]


        | MPEnum(id, caseName, fieldPats) ->
          match dv with
          | DEnum(_dTypeName, _oTypeName, dCaseName, dFields) when
            List.length dFields = List.length fieldPats && caseName = dCaseName
            ->
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
              false, allVars, (id, incomplete id) :: allSubTraces

          // Trace this with incompletes to avoid type errors
          | _dv ->
            let (newVarResults, traceResults) =
              fieldPats
              |> List.map (fun fp ->
                let pID = MatchPattern.toID fp
                let (_, newVars, traces) = checkPattern (incomplete pID) fp
                newVars, (id, incomplete id) :: traces)
              |> List.unzip
            let allVars = newVarResults |> List.collect identity
            let allSubTraces = traceResults |> List.collect identity
            (false, allVars, (id, incomplete id) :: allSubTraces)


        | MPTuple(id, firstPat, secondPat, theRestPat) ->
          let allPatterns = firstPat :: secondPat :: theRestPat

          match dv with
          | DTuple(first, second, theRest) ->
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


        | MPListCons(id, headPat, tailPat) ->
          match dv with
          // note: if we have the following:
          //   match [1] with
          //   | head :: tail ->
          // , the input list was a List<Int> and so we expect the result to
          // also be a List<Int> even though it's empty
          | DList(typ, headVal :: tailVals) ->
            let (headPass, headVars, headTraces) = checkPattern headVal headPat
            let (tailPass, tailVars, tailTraces) =
              checkPattern (DList(typ, tailVals)) tailPat

            let allSubVars = headVars @ tailVars
            let allSubTraces = headTraces @ tailTraces

            if headPass && tailPass then
              true, allSubVars, (id, dv) :: allSubTraces
            else
              let traces =
                (traceIncompleteWithArgs id [ headPat; tailPat ]) @ allSubTraces
              false, allSubVars, traces
          | _ -> false, [], traceIncompleteWithArgs id [ headPat; tailPat ]

        | MPList(id, pats) ->
          match dv with
          | DList(_, vals) ->
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
      let! matchVal = eval state tst st matchExpr

      let mutable hasMatched = false
      let mutable matchResult =
        // Even though we know it's fakeval, we still run through each pattern for analysis
        if Dval.isFake matchVal then matchVal else err id "No match"

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

            let! r = eval state tst newSymtable rhsExpr
            matchResult <- r
            hasMatched <- true
          else if isRealExecution then
            // "Real" evaluations don't need to persist non-matched traces
            ()
          else
            // If we're "previewing" (analysis), persist traces for all patterns
            traces |> List.iter (fun (id, dv) -> state.tracing.traceDval false id dv)
            do! preview tst newSymtable rhsExpr

      return matchResult


    | EIf(id, cond, thenbody, elsebody) ->
      match! eval state tst st cond with
      | DBool false ->
        do! preview tst st thenbody
        return! eval state tst st elsebody
      | DBool true ->
        let! result = eval state tst st thenbody
        do! preview tst st elsebody
        return result
      | cond when Dval.isFake cond ->
        do! preview tst st thenbody
        do! preview tst st elsebody
        return cond
      | _ ->
        do! preview tst st thenbody
        do! preview tst st elsebody
        return err id "If only supports Booleans"


    | EOr(id, left, right) ->
      match! eval state tst st left with
      | DBool true ->
        do! preview tst st right
        return DBool true
      | DBool false ->
        match! eval state tst st right with
        | DBool true -> return DBool true
        | DBool false -> return DBool false
        | right when Dval.isFake right -> return right
        | _ -> return err id "|| only supports Booleans"
      | left when Dval.isFake left ->
        do! preview tst st right
        return left
      | _ ->
        do! preview tst st right
        return err id "|| only supports Booleans"


    | EAnd(id, left, right) ->
      match! eval state tst st left with
      | DBool false ->
        do! preview tst st right
        return DBool false
      | DBool true ->
        match! eval state tst st right with
        | DBool true -> return DBool true
        | DBool false -> return DBool false
        | right when Dval.isFake right -> return right
        | _ -> return err id "&& only supports Booleans"
      | left when Dval.isFake left ->
        do! preview tst st right
        return left
      | _ ->
        do! preview tst st right
        return err id "&& only supports Booleans"


    | EEnum(id, typeName, caseName, fields) ->
      let typeStr = TypeName.toString typeName
      let types = ExecutionState.availableTypes state
      let! typ = Types.find typeName types

      match! enumMaybe types typeName with
      | None ->
        match typ with
        | None -> return err id $"There is no type named {typeStr}"
        | Some({ definition = TypeDeclaration.Enum _ }) ->
          return err id $"Expected a record but {typeStr} is an enum"
        | _ -> return err id $"Expected a record but {typeStr} is something else"
      | Some(aliasTypeName, _, cases) ->
        let case = cases |> NEList.find (fun c -> c.name = caseName)
        match case with
        | None -> return err id $"There is no case named `{caseName}` in {typeStr}"
        | Some case ->
          if case.fields.Length <> fields.Length then
            let msg =
              $"Case `{caseName}` expected {case.fields.Length} fields but got {fields.Length}"
            return err id msg
          else
            let fields = List.zip case.fields fields
            return!
              Ply.List.foldSequentiallyWithIndex
                (fun i r ((enumFieldType : TypeReference), expr) ->
                  uply {
                    if Dval.isFake r then
                      do! preview tst st expr
                      return r
                    else
                      let! v = eval state tst st expr
                      if Dval.isFake v then
                        return v
                      else
                        let context =
                          TypeChecker.EnumField(
                            typeName,
                            case.name,
                            i,
                            List.length fields,
                            enumFieldType,
                            None
                          )

                        match!
                          TypeChecker.unify context types Map.empty enumFieldType v
                        with
                        | Ok tst ->
                          // VTTODO stop swallowing the typeSymbolTable
                          match r with
                          | DEnum(typeName, original, caseName, existing) ->
                            return
                              DEnum(
                                typeName,
                                original,
                                caseName,
                                List.append existing [ v ]
                              )
                          | _ -> return err id "Expected an enum"
                        | Error e ->
                          return err id (Errors.toString (Errors.TypeError(e)))
                  })
                (DEnum(aliasTypeName, typeName, caseName, []))
                fields
    | EError(id, msg, exprs) ->
      let! args = Ply.List.mapSequentially (eval state tst st) exprs

      return
        args
        |> List.tryFind Dval.isFake
        |> Option.defaultValue (DError(sourceID id, msg))
  }

/// Interprets an expression and reduces to a Dark value
/// (or a task that results in a dval)
and eval
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (st : Symtable)
  (e : Expr)
  : DvalTask =
  uply {
    let! (result : Dval) = eval' state tst st e
    state.tracing.traceDval state.onExecutionPath (Expr.toID e) result
    return result
  }

/// Unwrap the dval, which we expect to be a function, and error if it's not
and applyFnVal
  (state : ExecutionState)
  (id : id)
  (fnVal : FnValImpl)
  (typeArgs : List<TypeReference>)
  (args : List<Dval>)
  : DvalTask =
  match fnVal with
  | Lambda l -> executeLambda state l args
  | NamedFn fn ->
    // I think we'll end up having to pass the
    // `tst` in scope here at some point?
    let tst = Map.empty
    callFn state tst id fn typeArgs args

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
      let paramTypes = Map.empty // TODO support type args from type params in lambdas
      // paramSyms is higher priority

      let newSymtable = Map.mergeFavoringRight l.symtable paramSyms
      let newTypeTable = Map.mergeFavoringRight l.typeSymbolTable paramTypes

      eval state newTypeTable newSymtable l.body

and callFn
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (callerID : id)
  (desc : FnName.T)
  (typeArgs : List<TypeReference>)
  (args : List<Dval>)
  : DvalTask =
  uply {
    let sourceID = SourceID(state.tlid, callerID)
    let handleMissingFunction () : Dval =
      // Functions which aren't implemented in the client may have results
      // available, otherwise they just return incomplete.
      let fnRecord = (state.tlid, desc, callerID)
      let fnResult = state.tracing.loadFnResult fnRecord args

      // TODO: in an old version, we executed the lambda with a fake value to
      // give enough livevalues for the editor to autocomplete. It may be worth
      // doing this again
      match fnResult with
      | Some(result, _ts) -> result
      | None -> DError(sourceID, $"Function {FnName.toString desc} is not found")

    let checkArgsLength fn : Result<unit, string> =
      let expectedTypeParamLength = List.length fn.typeParams
      let expectedArgLength = List.length fn.parameters

      let actualTypeArgLength = List.length typeArgs
      let actualArgLength = List.length args

      if
        expectedTypeParamLength = actualTypeArgLength
        && expectedArgLength = actualArgLength
      then
        Ok()
      else
        Error(
          $"{FnName.toString desc} has {expectedTypeParamLength} type parameters and {expectedArgLength} parameters, "
          + $"but here was called with {actualTypeArgLength} type arguments and {actualArgLength} arguments."
        )


    match List.tryFind Dval.isFake args with
    | Some fakeArg -> return fakeArg
    | None ->
      let! fn =
        match desc with
        | FQName.BuiltIn std ->
          state.builtIns.fns.TryFind std |> Option.map builtInFnToFn |> Ply
        | FQName.UserProgram u ->
          state.program.fns.TryFind u |> Option.map userFnToFn |> Ply
        | FQName.Package pkg ->
          uply {
            let! fn = state.packageManager.getFn pkg
            return Option.map packageFnToFn fn
          }
        | FQName.Unknown fn ->
          Exception.raiseInternal
            "Unknown function should have been converted to EError by PT2RT"
            [ "fn", fn ]

      match fn with
      | None -> return handleMissingFunction ()
      | Some fn ->
        match checkArgsLength fn with
        | Error errMsg -> return (DError(sourceID, errMsg))
        | Ok() ->
          let newlyBoundTypeSymbols =
            List.zip
              fn.typeParams
              (List.map Types.KnownType.fromFullySubstitutedTypeReference typeArgs
               |> List.map Known)
            |> Map
          let updatedTypeSymbolTable =
            Map.mergeFavoringRight tst newlyBoundTypeSymbols
          return! execFn state updatedTypeSymbolTable desc callerID fn typeArgs args
  }



and execFn
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (fnDesc : FnName.T)
  (id : id)
  (fn : Fn)
  (typeArgs : List<TypeReference>)
  (args : List<Dval>)
  : DvalTask =
  uply {
    let sourceID = SourceID(state.tlid, id) in

    if
      state.tracing.realOrPreview = Preview
      && not state.onExecutionPath
      && Set.contains fnDesc state.callstack
    then
      // Don't recurse (including transitively!) when previewing unexecuted paths
      // in the editor. If we do, we'll recurse forever and blow the stack.
      return DIncomplete(sourceID)
    else
      // CLEANUP: optimization opportunity
      let state =
        { state with
            executingFnName = Some fnDesc
            callstack = Set.add fnDesc state.callstack }

      let fnRecord = (state.tlid, fnDesc, id) in

      let types = ExecutionState.availableTypes state

      let typeArgsResolvedInFn =
        List.zip
          fn.typeParams
          (typeArgs
           |> List.map Types.KnownType.fromFullySubstitutedTypeReference
           |> List.map Known)
        |> Map
      let tst = Map.mergeFavoringRight tst typeArgsResolvedInFn

      (*
        let fn<'a> (a : 'a) (b1: 'b) (b2: 'b) (aList: List<'a>) (aResult: Result<'a, Int>) (bList: List<'b>) (bResult: Result<'b, String>)
                    (subFn: 'a -> List<'b> -> 'c)
                    : ('a, 'b, 'c) =

          let something = Result.map (fun a -> a + 1) aResult
          let something2 = Json.serialize<'b> b2
          (a, b2, subFn a bList)
      *)

      // VTTODO
      // need to have the below include any new typeArgs
      // i.e. "a" -> Int
      match! TypeChecker.checkFunctionCall types tst fn args with
      | Error err ->
        let msg = Errors.toString (Errors.TypeError(err))
        return DError(sourceID, msg)
      | Ok tst ->

        let! result =
          match fn.fn with
          | BuiltInFunction f ->
            if state.tracing.realOrPreview = Preview && fn.previewable <> Pure then
              match state.tracing.loadFnResult fnRecord args with
              | Some(result, _ts) -> Ply result
              | None -> Ply(DIncomplete sourceID)
            else
              uply {
                let! result =
                  uply {
                    try
                      return! f (state, typeArgs, args)
                    with e ->
                      let context : Metadata =
                        [ "fn", fnDesc
                          "args", args
                          "typeArgs", typeArgs
                          "id", id ]
                      match e with
                      | Errors.IncorrectArgs ->
                        return Errors.incorrectArgsToDError sourceID fn args
                      | Errors.FakeDvalFound dv -> return dv
                      | (:? CodeException | :? GrandUserException) as e ->
                        // There errors are created by us, within the libraries, so they are
                        // safe to show to users (but not grandusers)
                        return Dval.errSStr sourceID e.Message
                      | e ->
                        // TODO could we show the user the execution id here?
                        state.reportException state context e
                        // These are arbitrary errors, and could include sensitive
                        // information, so best not to show it to the user. If we'd
                        // like to show it to the user, we should catch it and give
                        // them a known safe error.
                        return Dval.errSStr sourceID Exception.unknownErrorMessage
                  }

                // there's no point storing data we'll never ask for
                if fn.previewable <> Pure then
                  state.tracing.storeFnResult fnRecord args result

                return result
              }

          | PackageFunction(tlid, body)
          | UserProgramFunction(tlid, body) ->
            state.tracing.traceTLID tlid
            let state = { state with tlid = tlid }
            let symTable =
              fn.parameters // Lengths are checked in checkFunctionCall
              |> List.map2 (fun dv p -> (p.name, dv)) args
              |> Map.ofList
              |> withGlobals state
            eval state tst symTable body

        if Dval.isFake result then
          return result
        else
          // VTTODO
          // use typeArg table (incl anything returned by checkFnParams or whatever)
          // to make sure this works
          match! TypeChecker.checkFunctionReturnType types tst fn result with
          | Error err ->
            let msg = Errors.toString (Errors.TypeError(err))
            return DError(sourceID, msg)
          | Ok tst -> return result
  }
