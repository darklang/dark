/// Interprets Dark expressions resulting in (tasks of) Dvals
module LibExecution.Interpreter

open System.Threading.Tasks
open FSharp.Control.Tasks
open FSharp.Control.Tasks.Affine.Unsafe

open Prelude
open RuntimeTypes
module VT = ValueType

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

module ExecutionError =
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
      DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError.executionError

    let (caseName, fields) =
      match e with
      | MatchExprEnumPatternWrongCount(caseName, expected, actual) ->
        "MatchExprEnumPatternWrongCount",
        [ DString caseName; DInt expected; DInt actual ]

      | MatchExprPatternWrongType(expected, actual) ->
        "MatchExprPatternWrongType", [ DString expected; RT2DT.Dval.toDT actual ]

      | MatchExprUnmatched dv -> "MatchExprUnmatched", [ RT2DT.Dval.toDT dv ]

      | NonStringInStringInterpolation dv ->
        "NonStringInStringInterpolation", [ RT2DT.Dval.toDT dv ]

      | ConstDoesntExist name -> "ConstDoesntExist", [ RT2DT.ConstantName.toDT name ]

    case caseName fields

  let raise (source : Source) (e : Error) : 'a = toDT e |> raiseRTE source


let rec evalConst (source : Source) (c : Const) : Dval =
  let r = evalConst source
  match c with
  | CInt i -> DInt i
  | CInt8 i -> DInt8 i
  | CUInt8 i -> DUInt8 i
  | CInt16 i -> DInt16 i
  | CUInt16 i -> DUInt16 i
  | CInt32 i -> DInt32 i
  | CUInt32 i -> DUInt32 i
  | CInt128 i -> DInt128 i
  | CUInt128 i -> DUInt128 i
  | CBool b -> DBool b
  | CString s -> DString s
  | CChar c -> DChar c
  | CFloat(sign, w, f) -> DFloat(makeFloat sign w f)
  | CUnit -> DUnit
  | CTuple(first, second, rest) -> DTuple(r first, r second, List.map r rest)
  | CEnum(Ok typeName, caseName, fields) ->
    // TYPESTODO: this uses the original type name, so if it's an alias, it won't be equal to the
    DEnum(typeName, typeName, VT.typeArgsTODO, caseName, List.map r fields)
  | CEnum(Error msg, _caseName, _fields) ->
    raiseRTE source (RuntimeError.oldError $"Invalid const name: {msg}")
  | CList items -> DList(ValueType.Unknown, (List.map r items))
  | CDict items ->
    DDict(ValueType.Unknown, (List.map (Tuple2.mapSecond r) items) |> Map.ofList)



/// Used in the ELet and ELambda evals
/// Does the dval 'match' the given pattern?
///
/// Returns:
/// - whether or not the expr 'matches' the pattern
/// - new vars (name * value)
let rec checkPattern
  (source : Source)
  (dv : Dval)
  (pattern : LetPattern)
  : List<string * Dval> =
  let errStr msg : 'a = raiseRTE source (RuntimeError.oldError msg)
  let chPat = checkPattern source

  match pattern with

  | LPVariable(_id, varName) -> [ (varName, dv) ]

  | LPUnit _id -> if dv <> DUnit then errStr "Unit pattern does not match" else []

  | LPTuple(_id, firstPat, secondPat, theRestPat) ->
    let allPatterns = firstPat :: secondPat :: theRestPat

    match dv with
    | DTuple(first, second, theRest) ->
      let allVals = first :: second :: theRest

      if List.length allVals = List.length allPatterns then
        List.zip allVals allPatterns
        |> List.map (fun (dv, pat) -> chPat dv pat)
        |> List.concat
      else
        errStr "Tuple pattern has wrong number of elements"
    | _ -> errStr "Tuple pattern does not match"

// fsharplint:disable FL0039

/// Interprets an expression and reduces it to a Dark value
/// (or a task that should result in such)
let rec eval
  (state : ExecutionState)
  (tlid : tlid)
  (tst : TypeSymbolTable)
  (st : Symtable)
  (e : Expr)
  : DvalTask =
  let sourceID id = Some(tlid, id)
  let errStr id msg : 'a = raiseRTE (sourceID id) (RuntimeError.oldError msg)
  let err id rte : 'a = raiseRTE (sourceID id) rte
  let raiseExeRTE id (e : ExecutionError.Error) : Ply<'a> =
    ExecutionError.raise (sourceID id) e

  let typeResolutionError
    (source : Source)
    (errorType : NameResolutionError.ErrorType)
    (typeName : TypeName.TypeName)
    : Ply<'a> =
    let error : NameResolutionError.Error =
      { errorType = errorType
        nameType = NameResolutionError.Type
        names = [ TypeName.toString typeName ] }
    error |> NameResolutionError.RTE.toRuntimeError |> raiseRTE source

  let recordMaybe
    (source : Source)
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
          //   fields = [("x", TVar "a"); ("y", TInt)]
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
          return raiseRTE source e

        | Some({ typeParams = typeParams
                 definition = TypeDeclaration.Record fields }) ->
          return
            (typeName,
             typeParams,
             fields |> NEList.toList |> List.map (fun f -> f.name, f.typ))

        | Some({ definition = TypeDeclaration.Alias(_) })
        | Some({ definition = TypeDeclaration.Enum _ }) ->
          return!
            typeResolutionError
              source
              NameResolutionError.ExpectedRecordButNot
              typeName

        | None ->
          return! typeResolutionError source NameResolutionError.NotFound typeName
      }
    inner typeName

  let enumMaybe
    (source : Source)
    (types : Types)
    (typeName : TypeName.TypeName)
    : Ply<TypeName.TypeName * List<string> * NEList<TypeDeclaration.EnumCase>> =
    let rec inner (typeName : TypeName.TypeName) =
      uply {
        match! Types.find typeName types with
        | Some({ typeParams = outerTypeParams
                 definition = TypeDeclaration.Alias(TCustomType(Ok(innerTypeName),
                                                                outerTypeArgs)) }) ->
          let! (innerTypeName, innerTypeParams, cases) = inner innerTypeName
          return
            (innerTypeName,
             outerTypeParams,
             cases
             |> NEList.map (fun (c : TypeDeclaration.EnumCase) ->
               { c with
                   fields =
                     List.map
                       (Types.substitute innerTypeParams outerTypeArgs)
                       c.fields }))

        | Some({ definition = TypeDeclaration.Alias(TCustomType(Error e, _)) }) ->
          return raiseRTE source e

        | Some({ typeParams = typeParams; definition = TypeDeclaration.Enum cases }) ->
          return (typeName, typeParams, cases)

        | Some({ definition = TypeDeclaration.Alias _ })
        | Some({ definition = TypeDeclaration.Record _ }) ->
          return!
            typeResolutionError
              source
              NameResolutionError.ExpectedEnumButNot
              typeName
        | None ->
          return! typeResolutionError source NameResolutionError.NotFound typeName
      }
    inner typeName


  uply {
    match e with
    | EString(_, [ StringText s ]) ->
      // We expect strings to be normalized during parsing
      return DString(s)
    | EString(id, segments) ->
      let! segments =
        segments
        |> Ply.List.mapSequentially (fun seg ->
          uply {
            match seg with
            | StringText text -> return text
            | StringInterpolation expr ->
              match! eval state tlid tst st expr with
              | DString s -> return s
              | dv ->
                // TODO: maybe better with a type error here
                return!
                  raiseExeRTE id (ExecutionError.NonStringInStringInterpolation dv)
          })
      return segments |> String.concat "" |> String.normalize |> DString
    | EBool(_id, b) -> return DBool b
    | EInt(_id, i) -> return DInt i
    | EInt8(_id, i) -> return DInt8 i
    | EUInt8(_id, i) -> return DUInt8 i
    | EInt16(_id, i) -> return DInt16 i
    | EUInt16(_id, i) -> return DUInt16 i
    | EInt32(_id, i) -> return DInt32 i
    | EUInt32(_id, i) -> return DUInt32 i
    | EInt128(_id, i) -> return DInt128 i
    | EUInt128(_id, i) -> return DUInt128 i
    | EFloat(_id, value) -> return DFloat value
    | EUnit _id -> return DUnit
    | EChar(_id, s) -> return DChar s
    | EConstant(id, name) ->
      let source = sourceID id
      match name with
      | FQName.UserProgram c ->
        match Map.find c state.program.constants with
        | None ->
          return! ExecutionError.raise source (ExecutionError.ConstDoesntExist name)
        | Some constant -> return evalConst source constant.body
      | FQName.BuiltIn c ->
        match Map.find c state.builtIns.constants with
        | None ->
          return! ExecutionError.raise source (ExecutionError.ConstDoesntExist name)
        | Some constant -> return constant.body
      | FQName.Package c ->
        match! state.packageManager.getConstant c with
        | None ->
          return! ExecutionError.raise source (ExecutionError.ConstDoesntExist name)
        | Some constant -> return evalConst source constant.body


    | ELet(id, pattern, rhs, body) ->
      let source = sourceID id
      let! rhs = eval state tlid tst st rhs
      let newDefs = checkPattern source rhs pattern
      let newSymtable = Map.mergeFavoringRight st (Map.ofList newDefs)

      return! eval state tlid tst newSymtable body

    | EList(_id, exprs) ->
      let! results = Ply.List.mapSequentially (eval state tlid tst st) exprs
      return TypeChecker.DvalCreator.list VT.unknown results

    | ETuple(_id, first, second, theRest) ->

      let! firstResult = eval state tlid tst st first
      let! secondResult = eval state tlid tst st second
      let! otherResults = Ply.List.mapSequentially (eval state tlid tst st) theRest
      return DTuple(firstResult, secondResult, otherResults)


    | EVariable(id, name) ->
      match Map.find name st with
      | None -> return errStr id $"There is no variable named: {name}"
      | Some other -> return other


    | ERecord(id, typeName, fields) ->
      let typeStr = TypeName.toString typeName
      let types = ExecutionState.availableTypes state
      let source = sourceID id

      let! (aliasTypeName, _typeParams, expectedFields) =
        recordMaybe source types typeName
      let expectedFields = Map expectedFields

      let! fields =
        fields
        |> NEList.toList
        |> Ply.List.foldSequentially
          (fun fields (k, expr) ->
            uply {
              match Map.find k expectedFields with
              | None -> return errStr id $"Unexpected field `{k}` in {typeStr}"
              | Some fieldType ->
                let! v = eval state tlid tst st expr
                if Map.containsKey k fields then
                  return errStr id $"Duplicate field `{k}` in {typeStr}"
                else
                  let context =
                    TypeChecker.RecordField(typeName, k, fieldType, None)
                  let check = TypeChecker.unify context types Map.empty fieldType v
                  match! check with
                  | Ok() -> return Map.add k v fields
                  | Error e -> return err id e
            })
          Map.empty

      if Map.count fields = Map.count expectedFields then
        return DRecord(aliasTypeName, typeName, VT.typeArgsTODO, fields)
      else
        let expectedKeys = Map.keys expectedFields
        let key = Seq.find (fun k -> not (Map.containsKey k fields)) expectedKeys
        return errStr id $"Missing field `{key}` in {typeStr}"

    | ERecordUpdate(id, baseRecord, updates) ->
      let source = sourceID id
      // CLEANUP refactor this impl
      // namely, focus more on the `fields` and don't pass around DRecord so much

      let! baseRecord = eval state tlid tst st baseRecord
      match baseRecord with
      | DRecord(typeName, _, typ, _) ->
        let typeStr = TypeName.toString typeName
        let types = ExecutionState.availableTypes state

        let! (_, _, expected) = recordMaybe source types typeName
        let expectedFields = Map expected
        return!
          updates
          |> NEList.toList
          |> Ply.List.foldSequentially
            (fun r (k, expr) ->
              uply {
                let! v = eval state tlid tst st expr

                match r, k, v with
                | _, "", _ -> return errStr id $"Empty key for value `{v}`"
                | _, _, _ when not (Map.containsKey k expectedFields) ->
                  return errStr id $"Unexpected field `{k}` in {typeStr}"

                | DRecord(typeName, original, _, m), k, v ->
                  let fieldType = Map.findUnsafe k expectedFields

                  let context =
                    TypeChecker.RecordField(typeName, k, fieldType, None)

                  match! TypeChecker.unify context types Map.empty fieldType v with
                  | Ok() -> return DRecord(typeName, original, typ, Map.add k v m)
                  | Error rte -> return raiseRTE source rte

                | _ ->
                  return
                    errStr id $"Expected a record but {typeStr} is something else"
              })
            baseRecord
      | _ -> return errStr id "Expected a record in record update"

    | EDict(_, fields) ->
      let! fields =
        fields
        |> Ply.List.mapSequentially (fun (k, v) ->
          uply {
            let! v = eval state tlid tst st v
            return (k, v)
          })
      return TypeChecker.DvalCreator.dict ValueType.Unknown fields

    | EFnName(_id, name) -> return DFnVal(NamedFn name)

    | EApply(id, fnTarget, typeArgs, exprs) ->
      match! eval state tlid tst st fnTarget with
      | DFnVal fnVal ->
        let! args = Ply.NEList.mapSequentially (eval state tlid tst st) exprs
        return! applyFnVal state (sourceID id) fnVal typeArgs args
      | other ->
        return
          errStr
            id
            $"Expected a function value, got something else: {DvalReprDeveloper.toRepr other}"


    | EFieldAccess(id, e, fieldName) ->
      let! obj = eval state tlid tst st e

      if fieldName = "" then
        return errStr id "Field name is empty"
      else
        match obj with
        | DRecord(_, typeName, _, fields) ->
          match Map.find fieldName fields with
          | Some v -> return v
          | None ->
            let typeStr = TypeName.toString typeName
            return errStr id $"No field named {fieldName} in {typeStr} record"
        | DDB _ ->
          let msg =
            $"Attempting to access field '{fieldName}' of a Datastore "
            + "(use `DB.*` standard library functions to interact with Datastores. "
            + "Field access only work with records)"
          return errStr id msg
        | _ ->
          let msg =
            $"Attempting to access field '{fieldName}' of a "
            + $"{DvalReprDeveloper.toTypeName obj} (field access only works with records)"
          return errStr id msg


    | ELambda(_id, parameters, body) ->
      // It is the responsibility of wherever executes the DBlock to pass in
      // args and execute the body.
      return
        DFnVal(
          Lambda
            { typeSymbolTable = tst
              tlid = tlid
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
        : Ply<bool * List<string * Dval>> =
        uply {
          match pattern with
          | MPInt(id, pi) ->
            match dv with
            | DInt di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE id (ExecutionError.MatchExprPatternWrongType("Int", dv))

          | MPInt8(id, pi) ->
            match dv with
            | DInt8 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE id (ExecutionError.MatchExprPatternWrongType("Int8", dv))

          | MPUInt8(id, pi) ->
            match dv with
            | DUInt8 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("UInt8", dv))

          | MPInt16(id, pi) ->
            match dv with
            | DInt16 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("Int16", dv))

          | MPUInt16(id, pi) ->
            match dv with
            | DUInt16 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("UInt16", dv))

          | MPInt32(id, pi) ->
            match dv with
            | DInt32 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("Int32", dv))

          | MPUInt32(id, pi) ->
            match dv with
            | DUInt32 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("UInt32", dv))

          | MPInt128(id, pi) ->
            match dv with
            | DInt128 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("Int128", dv))

          | MPUInt128(id, pi) ->
            match dv with
            | DUInt128 di -> return (di = pi), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("UInt128", dv))

          | MPBool(id, pb) ->
            match dv with
            | DBool db -> return (db = pb), []
            | _ ->
              return!
                raiseExeRTE id (ExecutionError.MatchExprPatternWrongType("Bool", dv))
          | MPChar(id, pc) ->
            match dv with
            | DChar dc -> return (dc = pc), []
            | _ ->
              return!
                raiseExeRTE id (ExecutionError.MatchExprPatternWrongType("Char", dv))
          | MPString(id, ps) ->
            match dv with
            | DString ds -> return (ds = ps), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("String", dv))
          | MPFloat(id, pf) ->
            match dv with
            | DFloat df -> return (df = pf), []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("Float", dv))
          | MPUnit(id) ->
            match dv with
            | DUnit -> return true, []
            | _ ->
              return!
                raiseExeRTE id (ExecutionError.MatchExprPatternWrongType("Unit", dv))

          | MPVariable(_id, varName) -> return true, [ (varName, dv) ]


          | MPEnum(id, caseName, fieldPats) ->
            match dv with
            | DEnum(_dTypeName, _oTypeName, _typeArgsDEnumTODO, dCaseName, dFields) ->
              if caseName <> dCaseName then
                return false, []
              else
                let dvFieldLength = List.length dFields
                match fieldPats with
                // wildcard
                | [ MPVariable(_, "_") ] when dvFieldLength > 0 -> return true, []
                | _ ->
                  let patFieldLength = List.length fieldPats
                  if dvFieldLength <> patFieldLength then
                    return!
                      raiseExeRTE
                        id
                        (ExecutionError.MatchExprEnumPatternWrongCount(
                          dCaseName,
                          patFieldLength,
                          dvFieldLength
                        ))
                  else
                    let! (passResults, newVarResults) =
                      List.zip dFields fieldPats
                      |> Ply.List.mapSequentially (fun (dv, pat) ->
                        checkPattern dv pat)
                      |> Ply.map List.unzip

                    let allPass = List.forall identity passResults
                    let allVars = newVarResults |> List.collect identity
                    return allPass, allVars

            | _dv ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType(caseName, dv))


          | MPTuple(id, firstPat, secondPat, theRestPat) ->
            let allPatterns = firstPat :: secondPat :: theRestPat

            match dv with
            | DTuple(first, second, theRest) ->
              let allVals = first :: second :: theRest

              if List.length allVals = List.length allPatterns then
                let! (passResults, newVarResults) =
                  List.zip allVals allPatterns
                  |> Ply.List.mapSequentially (fun (dv, pat) -> checkPattern dv pat)
                  |> Ply.map List.unzip

                let allPass = List.forall identity passResults
                let allVars = newVarResults |> List.collect identity
                return allPass, allVars
              else
                return false, []
            | _ ->
              return!
                raiseExeRTE
                  id
                  (ExecutionError.MatchExprPatternWrongType("Tuple", dv))


          | MPListCons(id, headPat, tailPat) ->
            match dv with
            | DList(_, []) -> return false, []
            | DList(vt, headVal :: tailVals) ->
              let! (headPass, headVars) = checkPattern headVal headPat
              let! (tailPass, tailVars) =
                checkPattern (TypeChecker.DvalCreator.list vt tailVals) tailPat

              let allSubVars = headVars @ tailVars
              let pass = headPass && tailPass
              return pass, allSubVars

            | _ ->
              return!
                raiseExeRTE id (ExecutionError.MatchExprPatternWrongType("List", dv))

          | MPList(id, pats) ->
            match dv with
            | DList(_, vals) ->
              if List.length vals = List.length pats then
                let! (passResults, newVarResults) =
                  List.zip vals pats
                  |> Ply.List.mapSequentially (fun (dv, pat) -> checkPattern dv pat)
                  |> Ply.map List.unzip

                let allPass = List.forall identity passResults
                let allVars = newVarResults |> List.collect identity
                return allPass, allVars
              else
                return false, []
            | _ ->
              return!
                raiseExeRTE id (ExecutionError.MatchExprPatternWrongType("List", dv))
        }


      // The value we're matching against
      let! matchVal = eval state tlid tst st matchExpr

      let mutable matchResult = None

      for case in NEList.toList cases do
        if Option.isSome matchResult then
          ()
        else
          let! passesPattern, newDefs = checkPattern matchVal case.pat
          let newSymtable = Map.mergeFavoringRight st (Map.ofList newDefs)
          let! passesWhenCondition =
            uply {
              match case.whenCondition with
              | Some whenCondition when passesPattern ->
                match! eval state tlid tst newSymtable whenCondition with
                | DBool b -> return b
                | _ -> return errStr id "When condition should be a boolean"
              | _ -> return true
            }
          if passesPattern && passesWhenCondition then
            let! r = eval state tlid tst newSymtable case.rhs
            matchResult <- Some r

      match matchResult with
      | Some r -> return r
      | None -> return! raiseExeRTE id (ExecutionError.MatchExprUnmatched matchVal)


    | EIf(id, cond, thenBody, elseBody) ->
      match! eval state tlid tst st cond with
      | DBool false ->
        match elseBody with
        | None -> return DUnit
        | Some eb -> return! eval state tlid tst st eb
      | DBool true -> return! eval state tlid tst st thenBody
      | _ -> return errStr id "If only supports Booleans"


    | EOr(id, left, right) ->
      match! eval state tlid tst st left with
      | DBool true -> return DBool true
      | DBool false ->
        match! eval state tlid tst st right with
        | DBool _ as b -> return b
        | _ -> return errStr id "|| only supports Booleans"
      | _ -> return errStr id "|| only supports Booleans"


    | EAnd(id, left, right) ->
      match! eval state tlid tst st left with
      | DBool false -> return DBool false
      | DBool true ->
        match! eval state tlid tst st right with
        | DBool _ as b -> return b
        | _ -> return errStr id "&& only supports Booleans"
      | _ -> return errStr id "&& only supports Booleans"


    | EEnum(id, sourceTypeName, caseName, fields) ->
      let source = sourceID id
      let typeStr = TypeName.toString sourceTypeName
      let types = ExecutionState.availableTypes state

      let! (resolvedTypeName, _, cases) = enumMaybe source types sourceTypeName
      let case = cases |> NEList.find (fun c -> c.name = caseName)

      match case with
      | None -> return errStr id $"There is no case named `{caseName}` in {typeStr}"
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
                  let! v = eval state tlid tst st fieldExpr

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
                  | Error rte -> return raiseRTE source rte
                })
              []
              (List.zip case.fields fields)

          return!
            TypeChecker.DvalCreator.enum
              resolvedTypeName
              sourceTypeName
              caseName
              fields

    | EError(id, rte, exprs) ->
      let! (_ : List<Dval>) = Ply.List.mapSequentially (eval state tlid tst st) exprs
      return raiseRTE (sourceID id) rte
  }


and applyFnVal
  (state : ExecutionState)
  (source : Source)
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
    callFn state tst source fn typeArgs args

and executeLambda
  (state : ExecutionState)
  (l : LambdaImpl)
  (args : NEList<Dval>)
  : DvalTask =

  // One of the reasons to take a separate list of params and args is to
  // provide this error message here. We don't have this information in
  // other places, and the alternative is just to provide incompletes
  // with no context
  let expectedLength = NEList.length l.parameters
  let actualLength = NEList.length args
  if expectedLength <> actualLength then
    raiseRTE
      state.caller
      (RuntimeError.oldError
        $"Expected {expectedLength} arguments, got {actualLength}")

  else
    let checkPattern' = checkPattern state.caller
    let paramSyms =
      NEList.map2 checkPattern' args l.parameters
      |> NEList.toList
      |> List.flatten
      |> Map
    let newSymtable = Map.mergeFavoringRight l.symtable paramSyms

    eval state l.tlid l.typeSymbolTable newSymtable l.body

and callFn
  (state : ExecutionState)
  (tst : TypeSymbolTable)
  (caller : Source)
  (desc : FnName.FnName)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : DvalTask =
  uply {
    let! fn =
      match desc with
      | FQName.BuiltIn std ->
        Map.find std state.builtIns.fns |> Option.map builtInFnToFn |> Ply
      | FQName.UserProgram u ->
        Map.find u state.program.fns |> Option.map userFnToFn |> Ply
      | FQName.Package pkg ->
        uply {
          let! fn = state.packageManager.getFn pkg
          return Option.map packageFnToFn fn
        }

    match fn with
    | Some fn ->
      let expectedTypeParams = List.length fn.typeParams
      let expectedArgs = NEList.length fn.parameters

      let actualTypeArgs = List.length typeArgs
      let actualArgs = NEList.length args

      if expectedTypeParams <> actualTypeArgs || expectedArgs <> actualArgs then
        let msg =
          $"{FnName.toString desc} has {expectedTypeParams} type parameters and {expectedArgs} parameters, "
          + $"but here was called with {actualTypeArgs} type arguments and {actualArgs} arguments."
        raiseRTE caller (RuntimeError.oldError msg)

      let state =
        let newlyBoundTypeArgs = List.zip fn.typeParams typeArgs |> Map
        { state with
            typeSymbolTable = Map.mergeFavoringRight tst newlyBoundTypeArgs }
      return! execFn state desc caller fn typeArgs args
    | None ->
      // Functions which aren't implemented in the client may have results
      // available, otherwise they error.
      let fnResult = state.tracing.loadFnResult (caller, desc) args


      // TODO: in an old version, we executed the lambda with a fake value to
      // give enough livevalues for the editor to autocomplete. It may be worth
      // doing this again
      match fnResult with
      | Some(result, _ts) -> return result
      | None ->
        return
          raiseRTE
            caller
            (RuntimeError.oldError $"Function {FnName.toString desc} is not found")
  }


and execFn
  (state : ExecutionState)
  (fnDesc : FnName.FnName)
  (caller : Source)
  (fn : Fn)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : DvalTask =
  uply {
    let types = ExecutionState.availableTypes state

    let typeArgsResolvedInFn = List.zip fn.typeParams typeArgs |> Map
    let typeSymbolTable =
      Map.mergeFavoringRight state.typeSymbolTable typeArgsResolvedInFn

    match! TypeChecker.checkFunctionCall types typeSymbolTable fn args with
    | Error rte -> return raiseRTE caller rte
    | Ok() ->
      let! result =
        match fn.fn with
        | BuiltInFunction f ->
          uply {
            let! result =
              uply {
                try
                  let state = { state with caller = caller }
                  return! f (state, typeArgs, NEList.toList args)
                with e ->
                  match e with
                  | RuntimeErrorException(None _, rte) ->
                    // Add the caller ID to the error if there isn't one already
                    return raiseRTE caller rte
                  | RuntimeErrorException _ -> return Exception.reraise e

                  | e ->
                    let context : Metadata =
                      [ "fn", fnDesc; "args", args; "typeArgs", typeArgs; "id", id ]
                    state.reportException state context e
                    // These are arbitrary errors, and could include sensitive
                    // information, so best not to show it to the user. If we'd
                    // like to show it to the user, we should catch it where it happens
                    // and give them a known safe error via a RuntimeError
                    return raiseRTE caller (RuntimeError.oldError "Unknown error")
              }

            // there's no point storing data we'll never ask for
            if fn.previewable <> Pure then
              state.tracing.storeFnResult (caller, fnDesc) args result

            return result
          }

        | PackageFunction(tlid, body)
        | UserProgramFunction(tlid, body) ->
          state.tracing.traceTLID tlid
          let state = { state with caller = caller }
          let symTable =
            fn.parameters // Lengths are checked in checkFunctionCall
            |> NEList.map2 (fun dv p -> (p.name, dv)) args
            |> Map.ofNEList
            |> withGlobals state
          eval state tlid typeSymbolTable symTable body

      match! TypeChecker.checkFunctionReturnType types typeSymbolTable fn result with
      | Error rte -> return raiseRTE caller rte
      | Ok() -> return result
  }
