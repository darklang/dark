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

  let dbs = Map.map (fun (db : DB.T) -> DDB db.name) state.program.dbs

  Map.mergeFavoringLeft secrets dbs


let withGlobals (state : ExecutionState) (symtable : Symtable) : Symtable =
  let globals = globalsFor state
  Map.mergeFavoringRight globals symtable

module Error =
  module RT2DT = RuntimeTypesToDarkTypes

  type Error =
    | MatchExprEnumPatternWrongCount of string * int * int
    | MatchExprPatternWrongType of string * Dval
    | MatchExprUnmatched of Dval
    | NonStringInStringInterpolation of Dval
    | ConstDoesntExist of ConstantName.ConstantName

  let toDT (e : Error) : RuntimeError =
    let typeName =
      TypeName.fqPackage
        "Darklang"
        [ "LanguageTools"; "RuntimeErrors"; "Execution" ]
        "Error"
        0

    let case (caseName : string) (fields : List<Dval>) : RuntimeError =
      DEnum(typeName, typeName, caseName, fields) |> RuntimeError.executionError

    match e with
    | MatchExprEnumPatternWrongCount(caseName, expected, actual) ->
      case
        "MatchExprEnumPatternWrongCount"
        [ DString caseName; DInt expected; DInt actual ]
    | MatchExprPatternWrongType(expected, actual) ->
      case "MatchExprPatternWrongType" [ DString expected; RT2DT.Dval.toDT actual ]
    | MatchExprUnmatched dv -> case "MatchExprUnmatched" [ RT2DT.Dval.toDT dv ]
    | NonStringInStringInterpolation dv ->
      case "NonStringInStringInterpolation" [ RT2DT.Dval.toDT dv ]
    | ConstDoesntExist name ->
      case "ConstDoesntExist" [ RT2DT.ConstantName.toDT name ]

  let raise (source : DvalSource) (e : Error) : 'a = raiseRTE source (toDT e)


let rec evalConst (source : DvalSource) (c : Const) : Dval =
  let r = evalConst source
  match c with
  | CInt i -> DInt i
  | CBool b -> DBool b
  | CString s -> DString s
  | CChar c -> DChar c
  | CFloat(sign, w, f) -> DFloat(makeFloat sign w f)
  | CUnit -> DUnit
  | CTuple(first, second, rest) -> DTuple(r first, r second, List.map r rest)
  | CEnum(Ok typeName, caseName, fields) ->
    // TYPESTODO: this uses the original type name, so if it's an alias, it won't be equal to the
    DEnum(typeName, typeName, caseName, List.map r fields)
  | CEnum(Error msg, caseName, fields) ->
    raiseRTE source (RuntimeError.oldError $"Invalid const name: {msg}")
  | CList items -> DList(ValueType.Unknown, (List.map r items))
  | CDict items ->
    DDict(ValueType.Unknown, (List.map (Tuple2.mapSecond r) items) |> Map.ofList)




// fsharplint:disable FL0039

/// Interprets an expression and reduces to a Dark value
/// (or task that should result in such)
let rec eval'
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (st : Symtable)
  (e : Expr)
  : DvalTask =
  let sourceID id = SourceID(state.tlid, id)
  let errStr id msg : 'a = raiseRTE (sourceID id) (RuntimeError.oldError msg)
  let err id rte : 'a = raiseRTE (sourceID id) rte
  let raiseExeRTE id (e : Error.Error) : 'a = Error.raise (sourceID id) e

  let typeResolutionError
    (errorType : NameResolutionError.ErrorType)
    (typeName : TypeName.TypeName)
    : 'a =
    let error : NameResolutionError.Error =
      { errorType = errorType
        nameType = NameResolutionError.Type
        names = [ TypeName.toString typeName ] }
    raiseRTE SourceNone (NameResolutionError.RTE.toRuntimeError error)

  let recordMaybe
    (types : Types)
    (typeName : TypeName.TypeName)
    // TypeName, typeParam list, fully-resolved (except for typeParam) field list
    : Ply<TypeName.TypeName * List<string> * List<string * TypeReference>> =
    let rec inner (typeName : TypeName.TypeName) =
      uply {
        match! Types.find typeName types with
        | Some({ typeParams = outerTypeParams
                 definition = TypeDeclaration.Alias(TCustomType(Ok(innerTypeName),
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
          let! (innerTypeName, innerTypeParams, fields) = inner innerTypeName
          return
            (innerTypeName,
             outerTypeParams,
             fields
             |> List.map (fun (k, v) ->
               (k, Types.substitute innerTypeParams outerTypeArgs v)))

        | Some({ definition = TypeDeclaration.Alias(TCustomType(Error e, _)) }) ->
          return raiseRTE SourceNone e

        | Some({ typeParams = typeParams
                 definition = TypeDeclaration.Record fields }) ->
          return
            (typeName,
             typeParams,
             fields |> NEList.toList |> List.map (fun f -> f.name, f.typ))

        | Some({ definition = TypeDeclaration.Alias(_) })
        | Some({ definition = TypeDeclaration.Enum _ }) ->
          return
            typeResolutionError NameResolutionError.ExpectedRecordButNot typeName

        | None -> return typeResolutionError NameResolutionError.NotFound typeName
      }
    inner typeName

  let enumMaybe
    (types : Types)
    (typeName : TypeName.TypeName)
    : Ply<Result<TypeName.TypeName * List<string> * NEList<TypeDeclaration.EnumCase>, RuntimeError>> =
    let rec inner (typeName : TypeName.TypeName) =
      uply {
        match! Types.find typeName types with
        | Some({ typeParams = outerTypeParams
                 definition = TypeDeclaration.Alias(TCustomType(Ok(innerTypeName),
                                                                outerTypeArgs)) }) ->
          let! next = inner innerTypeName
          return
            next
            |> Result.map (fun (innerTypeName, innerTypeParams, cases) ->
              (innerTypeName,
               outerTypeParams,
               cases
               |> NEList.map (fun (c : TypeDeclaration.EnumCase) ->
                 { c with
                     fields =
                       List.map
                         (Types.substitute innerTypeParams outerTypeArgs)
                         c.fields })))

        | Some({ definition = TypeDeclaration.Alias(TCustomType(Error e, _)) }) ->
          return Error e

        | Some({ typeParams = typeParams; definition = TypeDeclaration.Enum cases }) ->
          return Ok(typeName, typeParams, cases)

        | Some({ definition = TypeDeclaration.Alias _ })
        | Some({ definition = TypeDeclaration.Record _ }) ->
          return typeResolutionError NameResolutionError.ExpectedEnumButNot typeName
        | None -> return typeResolutionError NameResolutionError.NotFound typeName
      }
    inner typeName


  uply {
    match e with
    | EString(id, segments) ->
      let! str =
        segments
        |> Ply.List.foldSequentially
          (fun builtUpString seg ->
            uply {
              match builtUpString, seg with
              | str, StringText(text) -> return (str + text)
              | str, StringInterpolation(expr) ->
                let! result = eval state tst st expr
                match result with
                | DString s -> return str + s
                | dv ->
                  // TODO: maybe better with a type error here
                  return raiseExeRTE id (Error.NonStringInStringInterpolation dv)
            })
          ""
      return DString(String.normalize str)
    | EBool(_id, b) -> return DBool b
    | EInt(_id, i) -> return DInt i
    | EFloat(_id, value) -> return DFloat value
    | EUnit _id -> return DUnit
    | EChar(_id, s) -> return DChar s
    | EConstant(id, name) ->
      let source = sourceID id
      match name with
      | FQName.UserProgram c ->
        match state.program.constants.TryFind c with
        | None -> return Error.raise source (Error.ConstDoesntExist name)
        | Some constant -> return evalConst source constant.body
      | FQName.BuiltIn c ->
        match state.builtIns.constants.TryFind c with
        | None -> return Error.raise source (Error.ConstDoesntExist name)
        | Some constant -> return constant.body
      | FQName.Package c ->
        match! state.packageManager.getConstant c with
        | None -> return Error.raise source (Error.ConstDoesntExist name)
        | Some constant -> return evalConst source constant.body


    | ELet(id, pattern, rhs, body) ->
      /// Does the dval 'match' the given pattern?
      ///
      /// Returns:
      /// - whether or not the expr 'matches' the pattern
      /// - new vars (name * value)
      let rec checkPattern (dv : Dval) (pattern : LetPattern) : List<string * Dval> =
        match pattern with

        | LPVariable(id, varName) -> [ (varName, dv) ]

        | LPUnit id ->
          if dv <> DUnit then errStr id "Unit pattern does not match" else []

        | LPTuple(id, firstPat, secondPat, theRestPat) ->
          let allPatterns = firstPat :: secondPat :: theRestPat

          match dv with
          | DTuple(first, second, theRest) ->
            let allVals = first :: second :: theRest

            if List.length allVals = List.length allPatterns then
              List.zip allVals allPatterns
              |> List.map (fun (dv, pat) -> checkPattern dv pat)
              |> List.concat
            else
              errStr id "Tuple pattern has wrong number of elements"
          | _ -> errStr id "Tuple pattern does not match"

      let! rhs = eval state tst st rhs
      let newDefs = checkPattern rhs pattern
      let newSymtable = Map.mergeFavoringRight st (Map.ofList newDefs)

      return! eval state tst newSymtable body

    | EList(_id, exprs) ->
      let! results = Ply.List.mapSequentially (eval state tst st) exprs
      return Dval.list valueTypeTODO results


    | ETuple(_id, first, second, theRest) ->

      let! firstResult = eval state tst st first
      let! secondResult = eval state tst st second
      let! otherResults = Ply.List.mapSequentially (eval state tst st) theRest
      return DTuple(firstResult, secondResult, otherResults)


    | EVariable(id, name) ->
      match st.TryFind name with
      | None -> return errStr id $"There is no variable named: {name}"
      | Some other -> return other


    | ERecord(id, typeName, fields) ->
      let typeStr = TypeName.toString typeName
      let types = ExecutionState.availableTypes state

      let! (aliasTypeName, typeParams, expected) = recordMaybe types typeName
      let expectedFields = Map expected
      let! result =
        fields
        |> NEList.toList
        |> Ply.List.foldSequentially
          (fun r (k, expr) ->
            uply {
              if not (Map.containsKey k expectedFields) then
                return errStr id $"Unexpected field `{k}` in {typeStr}"
              else
                let! v = eval state tst st expr
                match r with
                | DRecord(typeName, original, _valueTypesTODO, m) ->
                  if Map.containsKey k m then
                    return errStr id $"Duplicate field `{k}` in {typeStr}"
                  else
                    let fieldType = Map.find k expectedFields
                    let context =
                      TypeChecker.RecordField(original, k, fieldType, None)
                    let check =
                      TypeChecker.unify context types Map.empty fieldType v
                    match! check with
                    | Ok() ->
                      return
                        DRecord(typeName, original, valueTypesTODO, Map.add k v m)
                    | Error e -> return err id e
                | _ -> return errStr id "Expected a record in typecheck"
            })
          (DRecord(aliasTypeName, typeName, valueTypesTODO, Map.empty)) // use the alias name here
      match result with
      | DRecord(_, _, _valueTypesTODO, fields) ->
        if Map.count fields = Map.count expectedFields then
          return result
        else
          let expectedKeys = Map.keys expectedFields
          let key = Seq.find (fun k -> not (Map.containsKey k fields)) expectedKeys
          return errStr id $"Missing field `{key}` in {typeStr}"
      | _ -> return result

    | ERecordUpdate(id, baseRecord, updates) ->
      let! baseRecord = eval state tst st baseRecord
      match baseRecord with
      | DRecord(typeName, _, _valueTypesTODO, _) ->
        let typeStr = TypeName.toString typeName
        let types = ExecutionState.availableTypes state
        let! (_, _, expected) = recordMaybe types typeName
        let expectedFields = Map expected
        return!
          updates
          |> NEList.toList
          |> Ply.List.foldSequentially
            (fun r (k, expr) ->
              uply {
                let! v = eval state tst st expr
                match r, k, v with
                | _, "", _ -> return errStr id $"Empty key for value `{v}`"
                | _, _, _ when not (Map.containsKey k expectedFields) ->
                  return errStr id $"Unexpected field `{k}` in {typeStr}"
                | DRecord(typeName, original, _valueTypesTODO, m), k, v ->
                  let fieldType = Map.find k expectedFields
                  let context =
                    TypeChecker.RecordField(typeName, k, fieldType, None)
                  match! TypeChecker.unify context types Map.empty fieldType v with
                  | Ok() ->
                    return
                      DRecord(typeName, original, valueTypesTODO, Map.add k v m)
                  | Error rte -> return raiseRTE (SourceID(state.tlid, id)) rte
                | _ ->
                  return
                    errStr id "Expected a record but {typeStr} is something else"
              })
            baseRecord
      | _ -> return errStr id "Expected a record in record update"

    | EDict(_, fields) ->
      return!
        Ply.List.foldSequentially
          (fun r (k, expr) ->
            uply {
              let! v = eval state tst st expr
              match (r, k, v) with
              | DDict(vt, entries), k, v ->
                return entries |> Map.add k v |> Dval.dictFromMap vt

              // If we haven't got a DDict we're propagating an error so let it go
              | r, _, _v -> return r
            })
          (Dval.dict ValueType.Unknown [])
          fields


    | EFnName(_id, name) -> return DFnVal(NamedFn name)

    | EApply(id, fnTarget, typeArgs, exprs) ->
      match! eval' state tst st fnTarget with
      | DFnVal fnVal ->
        let! args = Ply.NEList.mapSequentially (eval state tst st) exprs
        return! applyFnVal state id fnVal typeArgs args
      | other ->
        return
          errStr
            id
            $"Expected a function value, got something else: {DvalReprDeveloper.toRepr other}"


    | EFieldAccess(id, e, field) ->
      let! obj = eval state tst st e

      if field = "" then
        return errStr id "Field name is empty"
      else
        match obj with
        | DRecord(_, typeName, _, o) ->
          match Map.tryFind field o with
          | Some v -> return v
          | None ->
            let typeStr = TypeName.toString typeName
            return errStr id $"No field named {field} in {typeStr} record"
        | DDB _ ->
          let msg =
            $"Attempting to access field '{field}' of a Datastore "
            + "(use `DB.*` standard library functions to interact with Datastores. "
            + "Field access only work with records)"
          return errStr id msg
        | _ ->
          let msg =
            $"Attempting to access field '{field}' of a "
            + $"{DvalReprDeveloper.toTypeName obj} (field access only works with records)"
          return errStr id msg


    | ELambda(_id, parameters, body) ->
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


    | EMatch(id, matchExpr, cases) ->
      /// Does the dval 'match' the given pattern?
      ///
      /// Returns:
      /// - whether or not the expr 'matches' the pattern
      /// - new vars (name * value)
      let rec checkPattern
        (dv : Dval)
        (pattern : MatchPattern)
        : bool * List<string * Dval> =
        match pattern with
        | MPInt(id, pi) ->
          match dv with
          | DInt di -> (di = pi), []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("Int", dv))
        | MPBool(id, pb) ->
          match dv with
          | DBool db -> (db = pb), []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("Bool", dv))
        | MPChar(id, pc) ->
          match dv with
          | DChar dc -> (dc = pc), []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("Char", dv))
        | MPString(id, ps) ->
          match dv with
          | DString ds -> (ds = ps), []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("String", dv))
        | MPFloat(id, pf) ->
          match dv with
          | DFloat df -> (df = pf), []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("Float", dv))
        | MPUnit(id) ->
          match dv with
          | DUnit -> true, []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("Unit", dv))

        | MPVariable(id, varName) -> true, [ (varName, dv) ]


        | MPEnum(id, caseName, fieldPats) ->
          match dv with
          | DEnum(_dTypeName, _oTypeName, dCaseName, dFields) ->
            if caseName <> dCaseName then
              false, []
            else
              let dvFieldLength = List.length dFields
              match fieldPats with
              // wildcard
              | [ MPVariable(_, "_") ] when dvFieldLength > 0 -> true, []
              | _ ->
                let patFieldLength = List.length fieldPats
                if dvFieldLength <> patFieldLength then
                  raiseExeRTE
                    id
                    (Error.MatchExprEnumPatternWrongCount(
                      dCaseName,
                      patFieldLength,
                      dvFieldLength
                    ))
                else
                  let (passResults, newVarResults) =
                    List.zip dFields fieldPats
                    |> List.map (fun (dv, pat) -> checkPattern dv pat)
                    |> List.unzip

                  let allPass = List.forall identity passResults
                  let allVars = newVarResults |> List.collect identity
                  allPass, allVars

          | _dv -> raiseExeRTE id (Error.MatchExprPatternWrongType(caseName, dv))


        | MPTuple(id, firstPat, secondPat, theRestPat) ->
          let allPatterns = firstPat :: secondPat :: theRestPat

          match dv with
          | DTuple(first, second, theRest) ->
            let allVals = first :: second :: theRest

            if List.length allVals = List.length allPatterns then
              let (passResults, newVarResults) =
                List.zip allVals allPatterns
                |> List.map (fun (dv, pat) -> checkPattern dv pat)
                |> List.unzip

              let allPass = List.forall identity passResults
              let allVars = newVarResults |> List.collect identity
              allPass, allVars
            else
              false, []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("Tuple", dv))


        | MPListCons(id, headPat, tailPat) ->
          match dv with
          | DList(_, []) -> false, []
          | DList(vt, headVal :: tailVals) ->
            let (headPass, headVars) = checkPattern headVal headPat
            let (tailPass, tailVars) = checkPattern (Dval.list vt tailVals) tailPat

            let allSubVars = headVars @ tailVars
            let pass = headPass && tailPass
            pass, allSubVars

          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("List", dv))

        | MPList(id, pats) ->
          match dv with
          | DList(_, vals) ->
            if List.length vals = List.length pats then
              let (passResults, newVarResults) =
                List.zip vals pats
                |> List.map (fun (dv, pat) -> checkPattern dv pat)
                |> List.unzip

              let allPass = List.forall identity passResults
              let allVars = newVarResults |> List.collect identity
              allPass, allVars
            else
              false, []
          | _ -> raiseExeRTE id (Error.MatchExprPatternWrongType("List", dv))


      // The value we're matching against
      let! matchVal = eval state tst st matchExpr

      let mutable matchResult = None

      for (pattern, rhsExpr) in NEList.toList cases do
        if Option.isSome matchResult then
          ()
        else
          let passes, newDefs = checkPattern matchVal pattern
          let newSymtable = Map.mergeFavoringRight st (Map.ofList newDefs)
          if matchResult = None && passes then
            let! r = eval state tst newSymtable rhsExpr
            matchResult <- Some r

      match matchResult with
      | Some r -> return r
      | None -> return raiseExeRTE id (Error.MatchExprUnmatched matchVal)


    | EIf(id, cond, thenBody, elseBody) ->
      match! eval state tst st cond with
      | DBool false ->
        match elseBody with
        | None -> return DUnit
        | Some eb -> return! eval state tst st eb
      | DBool true -> return! eval state tst st thenBody
      | _ -> return errStr id "If only supports Booleans"


    | EOr(id, left, right) ->
      match! eval state tst st left with
      | DBool true -> return DBool true
      | DBool false ->
        match! eval state tst st right with
        | DBool _ as b -> return b
        | _ -> return errStr id "|| only supports Booleans"
      | _ -> return errStr id "|| only supports Booleans"


    | EAnd(id, left, right) ->
      match! eval state tst st left with
      | DBool false -> return DBool false
      | DBool true ->
        match! eval state tst st right with
        | DBool _ as b -> return b
        | _ -> return errStr id "&& only supports Booleans"
      | _ -> return errStr id "&& only supports Booleans"


    | EEnum(id, sourceTypeName, caseName, fields) ->
      let typeStr = TypeName.toString sourceTypeName
      let types = ExecutionState.availableTypes state

      match! enumMaybe types sourceTypeName with
      | Error e -> return err id e
      | Ok(resolvedTypeName, _, cases) ->
        let case = cases |> NEList.find (fun c -> c.name = caseName)

        match case with
        | None ->
          return errStr id $"There is no case named `{caseName}` in {typeStr}"
        | Some case ->
          if case.fields.Length <> fields.Length then
            let msg =
              $"Case `{caseName}` expected {case.fields.Length} fields but got {fields.Length}"
            return errStr id msg
          else
            let! (fields : List<Dval>) =
              Ply.List.foldSequentiallyWithIndex
                (fun
                     fieldIndex
                     fieldsSoFar
                     ((enumFieldType : TypeReference), fieldExpr) ->
                  uply {
                    let! v = eval state tst st fieldExpr

                    let context =
                      TypeChecker.EnumField(
                        sourceTypeName,
                        case.name,
                        fieldIndex,
                        List.length fields,
                        enumFieldType,
                        None
                      )

                    // VTTODO: we should be passing in a proper tst, not Map.empty - right?
                    match!
                      TypeChecker.unify context types Map.empty enumFieldType v
                    with
                    | Ok() -> return (List.append fieldsSoFar [ v ])
                    | Error rte -> return raiseRTE (SourceID(state.tlid, id)) rte
                  })
                []
                (List.zip case.fields fields)

            return Dval.enum resolvedTypeName sourceTypeName caseName fields

    | EError(id, rte, exprs) ->
      let! (_ : List<Dval>) = Ply.List.mapSequentially (eval state tst st) exprs
      return raiseRTE (sourceID id) rte
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
    return result
  }

/// Unwrap the dval, which we expect to be a function, and error if it's not
and applyFnVal
  (state : ExecutionState)
  (id : id)
  (fnVal : FnValImpl)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
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
  (args : NEList<Dval>)
  : DvalTask =

  let parameters = NEList.map snd l.parameters
  // One of the reasons to take a separate list of params and args is to
  // provide this error message here. We don't have this information in
  // other places, and the alternative is just to provide incompletes
  // with no context
  let expectedLength = NEList.length l.parameters
  let actualLength = NEList.length args
  if expectedLength <> actualLength then
    raiseRTE
      SourceNone
      (RuntimeError.oldError
        $"Expected {expectedLength} arguments, got {actualLength}")

  else
    let paramSyms = NEList.zip parameters args |> NEList.toList |> Map

    // paramSyms is higher priority
    let newSymtable = Map.mergeFavoringRight l.symtable paramSyms

    eval state l.typeSymbolTable newSymtable l.body

and callFn
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (callerID : id)
  (desc : FnName.FnName)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : DvalTask =
  uply {
    let sourceID = SourceID(state.tlid, callerID)
    let handleMissingFunction () : Dval =
      // Functions which aren't implemented in the client may have results
      // available, otherwise they error.
      let fnRecord = (state.tlid, desc, callerID)
      let fnResult = state.tracing.loadFnResult fnRecord args

      // TODO: in an old version, we executed the lambda with a fake value to
      // give enough livevalues for the editor to autocomplete. It may be worth
      // doing this again
      match fnResult with
      | Some(result, _ts) -> result
      | None ->
        raiseRTE
          sourceID
          (RuntimeError.oldError $"Function {FnName.toString desc} is not found")

    let checkArgsLength fn : unit =
      let expectedTypeParamLength = List.length fn.typeParams
      let expectedArgLength = NEList.length fn.parameters

      let actualTypeArgLength = List.length typeArgs
      let actualArgLength = NEList.length args

      if
        expectedTypeParamLength = actualTypeArgLength
        && expectedArgLength = actualArgLength
      then
        ()
      else
        let msg =
          $"{FnName.toString desc} has {expectedTypeParamLength} type parameters and {expectedArgLength} parameters, "
          + $"but here was called with {actualTypeArgLength} type arguments and {actualArgLength} arguments."
        raiseRTE sourceID (RuntimeError.oldError msg)

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

    match fn with
    | None -> return handleMissingFunction ()
    | Some fn ->
      checkArgsLength fn
      let newlyBoundTypeArgs = List.zip fn.typeParams typeArgs |> Map
      let updatedTypeSymbolTable = Map.mergeFavoringRight tst newlyBoundTypeArgs
      return! execFn state updatedTypeSymbolTable desc callerID fn typeArgs args
  }



and execFn
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (fnDesc : FnName.FnName)
  (id : id)
  (fn : Fn)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : DvalTask =
  uply {
    let sourceID = SourceID(state.tlid, id) in

    // CLEANUP: optimization opportunity
    let state =
      { state with
          executingFnName = Some fnDesc
          callstack = Set.add fnDesc state.callstack }

    let fnRecord = (state.tlid, fnDesc, id) in

    let types = ExecutionState.availableTypes state

    let typeArgsResolvedInFn = List.zip fn.typeParams typeArgs |> Map
    let typeSymbolTable = Map.mergeFavoringRight tst typeArgsResolvedInFn

    match! TypeChecker.checkFunctionCall types typeSymbolTable fn args with
    | Error rte -> return raiseRTE sourceID rte
    | Ok() ->

      let! result =
        match fn.fn with
        | BuiltInFunction f ->
          uply {
            let! result =
              uply {
                try
                  return! f (state, typeArgs, NEList.toList args)
                with e ->
                  match e with
                  | RuntimeErrorException(source, rte) -> return Exception.reraise e
                  | e ->
                    // TODO could we show the user the execution id here?
                    let context : Metadata =
                      [ "fn", fnDesc; "args", args; "typeArgs", typeArgs; "id", id ]
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
            |> NEList.map2 (fun dv p -> (p.name, dv)) args
            |> Map.ofNEList
            |> withGlobals state
          eval state typeSymbolTable symTable body

      match! TypeChecker.checkFunctionReturnType types typeSymbolTable fn result with
      | Error rte -> return raiseRTE sourceID rte
      | Ok() -> return result
  }
