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
let globalsFor (_state : ExecutionState) : Symtable =
  let secrets =
    // state.program.secrets
    // |> List.map (fun (s : Secret.T) -> (s.name, DString s.value))
    // |> Map.ofList
    Map.empty

  let dbs =
    //Map.map (fun (db : DB.T) -> DDB db.name) state.program.dbs
    Map.empty

  Map.mergeFavoringLeft secrets dbs


let withGlobals (state : ExecutionState) (symtable : Symtable) : Symtable =
  let globals = globalsFor state
  Map.mergeFavoringRight globals symtable


module ExecutionError =
  //module RT2DT = RuntimeTypesToDarkTypes

  type Error =
    // | MatchExprEnumPatternWrongCount of string * int * int
    // | MatchExprPatternWrongType of string * Dval
    // | MatchExprUnmatched of Dval
    | NonStringInStringInterpolation of Dval
    //| ConstDoesntExist of FQConstantName.FQConstantName
    // | FieldAccessFieldDoesntExist of
    //   typeName : FQTypeName.FQTypeName *
    //   invalidFieldName : string
    // | RecordConstructionFieldDoesntExist of
    //   FQTypeName.FQTypeName *
    //   invalidFieldName : string
    // | RecordConstructionMissingField of
    //   FQTypeName.FQTypeName *
    //   missingFieldName : string
    // | RecordConstructionDuplicateField of
    //   FQTypeName.FQTypeName *
    //   duplicateFieldName : string
    // | FieldAccessNotRecord of ValueType * string
    // | EnumConstructionCaseNotFound of FQTypeName.FQTypeName * string
    | WrongNumberOfFnArgs of
      fn : FQFnName.FQFnName *
      expectedTypeArgs : int *
      expectedArgs : int *
      actualTypeArgs : int *
      actualArgs : int

  let toDT (_e : Error) : RuntimeError =
    // let typeName =
    //   FQTypeName.Package PackageIDs.Type.LanguageTools.RuntimeError.Execution.error

    // let case (caseName : string) (fields : List<Dval>) : RuntimeError =
    //   DEnum(typeName, typeName, [], caseName, fields) |> RuntimeError.executionError

    // let (caseName, fields) =
    //   match e with
    //   | MatchExprEnumPatternWrongCount(caseName, expected, actual) ->
    //     "MatchExprEnumPatternWrongCount",
    //     [ DString caseName; DInt64 expected; DInt64 actual ]

    //   | MatchExprPatternWrongType(expected, actual) ->
    //     "MatchExprPatternWrongType", [ DString expected; RT2DT.Dval.toDT actual ]

    //   | MatchExprUnmatched dv -> "MatchExprUnmatched", [ RT2DT.Dval.toDT dv ]

    //   | NonStringInStringInterpolation dv ->
    //     "NonStringInStringInterpolation", [ RT2DT.Dval.toDT dv ]

    //   | ConstDoesntExist name ->
    //     "ConstDoesntExist", [ RT2DT.FQConstantName.toDT name ]

    //   | FieldAccessFieldDoesntExist(typeName, invalidFieldName) ->
    //     "FieldAccessFieldDoesntExist",
    //     [ RT2DT.FQTypeName.toDT typeName; DString invalidFieldName ]

    //   | FieldAccessNotRecord(vt, fieldName) ->
    //     "FieldAccessNotRecord", [ RT2DT.ValueType.toDT vt; DString fieldName ]

    //   | EnumConstructionCaseNotFound(typeName, caseName) ->
    //     "EnumConstructionCaseNotFound",
    //     [ RT2DT.FQTypeName.toDT typeName; DString caseName ]

    //   | WrongNumberOfFnArgs(fn,
    //                         expectedTypeArgs,
    //                         expectedArgs,
    //                         actualTypeArgs,
    //                         actualArgs) ->
    //     "WrongNumberOfFnArgs",
    //     [ RT2DT.FQFnName.toDT fn
    //       DInt64 expectedTypeArgs
    //       DInt64 expectedArgs
    //       DInt64 actualTypeArgs
    //       DInt64 actualArgs ]

    //   | RecordConstructionFieldDoesntExist(typeName, invalidFieldName) ->
    //     "RecordConstructionFieldDoesntExist",
    //     [ RT2DT.FQTypeName.toDT typeName; DString invalidFieldName ]

    //   | RecordConstructionMissingField(typeName, missingFieldName) ->
    //     "RecordConstructionMissingField",
    //     [ RT2DT.FQTypeName.toDT typeName; DString missingFieldName ]

    //   | RecordConstructionDuplicateField(typeName, duplicateFieldName) ->
    //     "RecordConstructionDuplicateField",
    //     [ RT2DT.FQTypeName.toDT typeName; DString duplicateFieldName ]

    // case caseName fields
    RuntimeError.oldError "TODO"

  let raise (callStack : CallStack) (e : Error) : 'a = toDT e |> raiseRTE callStack


// let rec evalConst (callStack : CallStack) (c : Const) : Dval =
//   let r = evalConst callStack

//   match c with
//   | CUnit -> DUnit
//   | CBool b -> DBool b

//   | CInt8 i -> DInt8 i
//   | CUInt8 i -> DUInt8 i
//   | CInt16 i -> DInt16 i
//   | CUInt16 i -> DUInt16 i
//   | CInt32 i -> DInt32 i
//   | CUInt32 i -> DUInt32 i
//   | CInt64 i -> DInt64 i
//   | CUInt64 i -> DUInt64 i
//   | CInt128 i -> DInt128 i
//   | CUInt128 i -> DUInt128 i

//   | CFloat(sign, w, f) -> DFloat(makeFloat sign w f)

//   | CChar c -> DChar c
//   | CString s -> DString s

//   | CList items -> DList(ValueType.Unknown, (List.map r items))
//   | CTuple(first, second, rest) -> DTuple(r first, r second, List.map r rest)
//   | CDict items ->
//     DDict(ValueType.Unknown, (List.map (Tuple2.mapSecond r) items) |> Map.ofList)

//   | CEnum(Ok typeName, caseName, fields) ->
//     // TYPESTODO: this uses the original type name, so if it's an alias, it won't be equal to the
//     DEnum(typeName, typeName, VT.typeArgsTODO, caseName, List.map r fields)

//   | CEnum(Error msg, _caseName, _fields) ->
//     raiseRTE callStack (RuntimeError.oldError $"Invalid const name: {msg}")



// /// Used in the ELet and ELambda evals
// /// Answers: does the `dval` "match" the given pattern?
// ///
// /// Returns:
// /// - whether or not the expr 'matches' the pattern
// /// - new vars (name * value)
// let rec checkPattern
//   (callStack : CallStack)
//   (dv : Dval)
//   (pattern : LetPattern)
//   : List<string * Dval> =

//   let errStr msg : 'a = raiseRTE callStack (RuntimeError.oldError msg)
//   let chPat = checkPattern callStack

//   match pattern with

//   | LPVariable(_, varName) -> [ (varName, dv) ]

//   | LPUnit _ -> if dv <> DUnit then errStr "Unit pattern does not match" else []

//   | LPTuple(_, firstPat, secondPat, theRestPat) ->
//     let allPatterns = firstPat :: secondPat :: theRestPat

//     match dv with
//     | DTuple(first, second, theRest) ->
//       let allVals = first :: second :: theRest

//       if List.length allVals = List.length allPatterns then
//         List.zip allVals allPatterns
//         |> List.map (fun (dv, pat) -> chPat dv pat)
//         |> List.concat
//       else
//         errStr "Tuple pattern has wrong number of elements"
//     | _ -> errStr "Tuple pattern does not match"

// fsharplint:disable FL0039


let typeResolutionError
  (callStack : CallStack)
  (errorType : NameResolutionError.ErrorType)
  : Ply<'a> =
  let error : NameResolutionError.Error =
    { errorType = errorType; nameType = NameResolutionError.Type }
  error |> NameResolutionError.RTE.toRuntimeError |> raiseRTE callStack


// let recordMaybe
//   (callStack : CallStack)
//   (types : Types)
//   (typeName : FQTypeName.FQTypeName)
//   // TypeName, typeParam list, fully-resolved (except for typeParam) field list
//   : Ply<FQTypeName.FQTypeName * List<string> * List<string * TypeReference>> =
//   let rec inner (typeName : FQTypeName.FQTypeName) =
//     uply {
//       match! Types.find typeName types with
//       | Some({ typeParams = outerTypeParams
//                definition = TypeDeclaration.Alias(TCustomType(Ok(innerTypeName),
//                                                               outerTypeArgs)) }) ->
//         // Here we have found an alias, so we need to combine the type's
//         // typeArgs with the aliased type's typeParams.
//         // e.g. in
//         //   `type Var = Result<Int64, String>`
//         // we need to combine Var's typeArgs (<Int, String>) with Result's
//         // typeParams (<`Ok, `Error>)
//         //
//         // To do this, we use typeArgs from the alias definition
//         // (outerTypeArgs) and apply them to the aliased type
//         // (innerTypeName)'s params (which are returned from the lookup and
//         // used as innerTypeParams below).
//         // Example: suppose we have
//         //   type Outer<'a> = Inner<'a, Int>
//         //   type Inner<'x, 'y> = { x : 'x; y : 'y }
//         // The recursive search for Inner will get:
//         //   innerTypeName = "Inner"
//         //   innerTypeParams = ["x"; "y"]
//         //   fields = [("x", TVar "x"); ("y", TVar "y")]
//         // The Outer definition provides:
//         //   outerTypeArgs = [TVar "a"; TInt64]
//         // We combine this with innerTypeParams to get:
//         //   fields = [("x", TVar "a"); ("y", TInt64)]
//         //   outerTypeParams = ["a"]
//         // So the effective result of this is:
//         //   type Outer<'a> = { x : 'a; y : Int }
//         let! (innerTypeName, innerTypeParams, fields) = inner innerTypeName
//         return
//           (innerTypeName,
//            outerTypeParams,
//            fields
//            |> List.map (fun (k, v) ->
//              (k, Types.substitute innerTypeParams outerTypeArgs v)))

//       | Some({ definition = TypeDeclaration.Alias(TCustomType(Error e, _)) }) ->
//         return raiseRTE callStack e

//       | Some({ typeParams = typeParams; definition = TypeDeclaration.Record fields }) ->
//         return
//           (typeName,
//            typeParams,
//            fields |> NEList.toList |> List.map (fun f -> f.name, f.typ))

//       | Some({ definition = TypeDeclaration.Alias(_) })
//       | Some({ definition = TypeDeclaration.Enum _ }) ->
//         let packageTypeID =
//           match typeName with
//           | FQTypeName.FQTypeName.Package id -> id
//         return!
//           typeResolutionError
//             callStack
//             (NameResolutionError.ExpectedRecordButNot packageTypeID)

//       | None ->
//         return! typeResolutionError callStack (NameResolutionError.NotFound [])
//     }
//   inner typeName


// let enumMaybe
//   (callStack : CallStack)
//   (types : Types)
//   (typeName : FQTypeName.FQTypeName)
//   : Ply<FQTypeName.FQTypeName * List<string> * NEList<TypeDeclaration.EnumCase>> =
//   let rec inner (typeName : FQTypeName.FQTypeName) =
//     uply {
//       match! Types.find typeName types with
//       | Some({ typeParams = outerTypeParams
//                definition = TypeDeclaration.Alias(TCustomType(Ok(innerTypeName),
//                                                               outerTypeArgs)) }) ->
//         let! (innerTypeName, innerTypeParams, cases) = inner innerTypeName
//         return
//           (innerTypeName,
//            outerTypeParams,
//            cases
//            |> NEList.map (fun (c : TypeDeclaration.EnumCase) ->
//              { c with
//                  fields =
//                    List.map
//                      (Types.substitute innerTypeParams outerTypeArgs)
//                      c.fields }))

//       | Some({ definition = TypeDeclaration.Alias(TCustomType(Error e, _)) }) ->
//         return raiseRTE callStack e

//       | Some({ typeParams = typeParams; definition = TypeDeclaration.Enum cases }) ->
//         return (typeName, typeParams, cases)

//       | Some({ definition = TypeDeclaration.Alias _ })
//       | Some({ definition = TypeDeclaration.Record _ }) ->
//         let packageTypeID =
//           match typeName with
//           | FQTypeName.FQTypeName.Package id -> id
//         return!
//           typeResolutionError
//             callStack
//             (NameResolutionError.ExpectedEnumButNot packageTypeID)
//       | None ->
//         return! typeResolutionError callStack (NameResolutionError.NotFound []) // typeName
//     }
//   inner typeName


/// Interprets an expression and reduces it to a Dark value
/// (or a task that should result in such)
let rec eval (state : ExecutionState) (e : Expr) : DvalTask =
  // Some helper fns to make it easier to update the state's callstack
  // for a given expr, match pattern, etc.
  let callStackID (id : id) =
    { state.tracing.callStack with
        lastCalled = (fst state.tracing.callStack.lastCalled, Some id) }
  let stateWithUpdatedCallStack id =
    { state with tracing.callStack = callStackID id }

  // Update the state's callStack with the id of the expr we're evaluating
  let state = stateWithUpdatedCallStack (Expr.toID e)
  let callStack = state.tracing.callStack

  // Some helper fns to make it easier to raise RTEs
  let errStr callStack msg : 'a = raiseRTE callStack (RuntimeError.oldError msg)
  //let err callStack rte : 'a = raiseRTE callStack rte
  let raiseExeRTE callStack (e : ExecutionError.Error) : Ply<'a> =
    ExecutionError.raise callStack e

  uply {
    match e with
    | EUnit _ -> return DUnit

    | EBool(_, b) -> return DBool b

    // | EInt8(_, i) -> return DInt8 i
    // | EUInt8(_, i) -> return DUInt8 i
    // | EInt16(_, i) -> return DInt16 i
    // | EUInt16(_, i) -> return DUInt16 i
    // | EInt32(_, i) -> return DInt32 i
    // | EUInt32(_, i) -> return DUInt32 i
    | EInt64(_, i) -> return DInt64 i
    // | EUInt64(_, i) -> return DUInt64 i
    // | EInt128(_, i) -> return DInt128 i
    // | EUInt128(_, i) -> return DUInt128 i

    // | EFloat(_, value) -> return DFloat value

    // | EChar(_, s) -> return DChar s

    | EString(_, [ StringText s ]) ->
      // We expect strings to be normalized during parsing
      return DString(s)
    | EString(_, segments) ->
      let! segments =
        segments
        |> Ply.List.mapSequentially (fun seg ->
          uply {
            match seg with
            | StringText text -> return text
            | StringInterpolation expr ->
              match! eval state expr with
              | DString s -> return s
              | dv ->
                // TODO: maybe better with a type error here
                return!
                  raiseExeRTE
                    callStack
                    (ExecutionError.NonStringInStringInterpolation dv)
          })
      return segments |> String.concat "" |> String.normalize |> DString


    // | EConstant(_, name) ->
    //   match name with
    //   | FQConstantName.Builtin c ->
    //     match Map.find c state.builtins.constants with
    //     | None ->
    //       return!
    //         ExecutionError.raise callStack (ExecutionError.ConstDoesntExist name)
    //     | Some constant -> return constant.body

    //   | FQConstantName.Package c ->
    //     match! state.packageManager.getConstant c with
    //     | None ->
    //       return!
    //         ExecutionError.raise callStack (ExecutionError.ConstDoesntExist name)
    //     | Some constant -> return evalConst callStack constant.body


    // | ELet(_, pattern, rhs, body) ->
    //   let! rhs = eval state rhs
    //   let newDefs = checkPattern callStack rhs pattern
    //   let newSymtable = Map.mergeFavoringRight state.symbolTable (Map.ofList newDefs)

    //   return! eval { state with symbolTable = newSymtable } body

    // | EList(_, exprs) ->
    //   let! results = Ply.List.mapSequentially (eval state) exprs
    //   return TypeChecker.DvalCreator.list callStack VT.unknown results

    // | ETuple(_, first, second, theRest) ->
    //   let! firstResult = eval state first
    //   let! secondResult = eval state second
    //   let! otherResults = Ply.List.mapSequentially (eval state) theRest
    //   return DTuple(firstResult, secondResult, otherResults)

    // | EVariable(_, name) ->
    //   match Map.find name state.symbolTable with
    //   | None -> return errStr callStack $"There is no variable named: {name}"
    //   | Some other -> return other


    // | ERecord(_, typeName, fields) ->
    //   let types = ExecutionState.availableTypes state

    //   let! (aliasTypeName, _typeParams, expectedFields) =
    //     recordMaybe callStack types typeName
    //   let expectedFields = Map expectedFields

    //   let! fields =
    //     fields
    //     |> NEList.toList
    //     |> Ply.List.foldSequentially
    //       (fun fields (fieldName, expr) ->
    //         uply {
    //           match Map.find fieldName expectedFields with
    //           | None ->
    //             return
    //               ExecutionError.raise
    //                 callStack
    //                 (ExecutionError.RecordConstructionFieldDoesntExist(
    //                   typeName,
    //                   fieldName
    //                 ))
    //           | Some fieldType ->
    //             let! v = eval state expr
    //             if Map.containsKey fieldName fields then
    //               return
    //                 ExecutionError.raise
    //                   callStack
    //                   (ExecutionError.RecordConstructionDuplicateField(
    //                     typeName,
    //                     fieldName
    //                   ))

    //             else
    //               let context =
    //                 TypeChecker.RecordField(typeName, fieldName, fieldType)
    //               let check = TypeChecker.unify context types Map.empty fieldType v
    //               match! check with
    //               | Ok() -> return Map.add fieldName v fields
    //               | Error e -> return err callStack e
    //         })
    //       Map.empty

    //   if Map.count fields = Map.count expectedFields then
    //     return DRecord(aliasTypeName, typeName, VT.typeArgsTODO, fields)
    //   else
    //     let expectedFields = Map.keys expectedFields
    //     let fieldName =
    //       Seq.find (fun k -> not (Map.containsKey k fields)) expectedFields
    //     return
    //       ExecutionError.raise
    //         callStack
    //         (ExecutionError.RecordConstructionMissingField(typeName, fieldName))


    // | ERecordUpdate(_, baseRecord, updates) ->
    //   // CLEANUP refactor this impl
    //   // namely, focus more on the `fields` and don't pass around DRecord so much

    //   let! baseRecord = eval state baseRecord
    //   match baseRecord with
    //   | DRecord(typeName, _, typ, _) ->
    //     let typeStr = FQTypeName.toString typeName
    //     let types = ExecutionState.availableTypes state

    //     let! (_, _, expected) = recordMaybe callStack types typeName
    //     let expectedFields = Map expected
    //     return!
    //       updates
    //       |> NEList.toList
    //       |> Ply.List.foldSequentially
    //         (fun record (fieldName, expr) ->
    //           uply {
    //             let! dv = eval state expr

    //             match record, fieldName, dv with
    //             | _, "", _ -> return errStr callStack $"Empty key for value `{dv}`"
    //             | _, _, _ when not (Map.containsKey fieldName expectedFields) ->
    //               return
    //                 ExecutionError.raise
    //                   callStack
    //                   (ExecutionError.RecordConstructionFieldDoesntExist(
    //                     typeName,
    //                     fieldName
    //                   ))

    //             | DRecord(typeName, original, _, m), fieldName, dv ->
    //               let fieldType = Map.findUnsafe fieldName expectedFields

    //               let context =
    //                 TypeChecker.RecordField(typeName, fieldName, fieldType)

    //               match! TypeChecker.unify context types Map.empty fieldType dv with
    //               | Ok() ->
    //                 return DRecord(typeName, original, typ, Map.add fieldName dv m)
    //               | Error rte -> return raiseRTE callStack rte

    //             | _ ->
    //               return
    //                 errStr
    //                   callStack
    //                   $"Expected a record but {typeStr} is something else"
    //           })
    //         baseRecord
    //   | _ -> return errStr callStack "Expected a record in record update"

    // | EDict(_, fields) ->
    //   let! fields =
    //     fields
    //     |> Ply.List.mapSequentially (fun (k, v) ->
    //       uply {
    //         let! v = eval state v
    //         return (k, v)
    //       })
    //   return TypeChecker.DvalCreator.dict ValueType.Unknown fields

    | EFnName(_, name) -> return DFnVal(NamedFn name)

    | EApply(_, fnTarget, typeArgs, exprs) ->
      match! eval state fnTarget with
      | DFnVal fnVal ->
        let! args = Ply.NEList.mapSequentially (eval state) exprs
        return! applyFnVal state fnVal typeArgs args
      | other ->
        return
          errStr
            callStack
            $"Expected a function value, got something else: {DvalReprDeveloper.toRepr other}"


    // | EFieldAccess(_, e, fieldName) ->
    //   let! obj = eval state e

    //   if fieldName = "" then
    //     return errStr callStack "Field name is empty"
    //   else
    //     match obj with
    //     | DRecord(_, typeName, _, fields) ->
    //       match Map.find fieldName fields with
    //       | Some v -> return v
    //       | None ->
    //         return
    //           ExecutionError.raise
    //             callStack
    //             (ExecutionError.FieldAccessFieldDoesntExist(typeName, fieldName))
    //     | DDB _ ->
    //       let msg =
    //         $"Attempting to access field '{fieldName}' of a Datastore "
    //         + "(use `DB.*` standard library functions to interact with Datastores. "
    //         + "Field access only work with records)"
    //       return errStr callStack msg
    //     | _ ->

    //       return
    //         ExecutionError.raise
    //           callStack
    //           (ExecutionError.FieldAccessNotRecord(Dval.toValueType obj, fieldName))


    // | ELambda(_, parameters, body) ->
    //   // It is the responsibility of wherever executes the DBlock to pass in
    //   // args and execute the body.
    //   return
    //     DFnVal(
    //       Lambda
    //         { typeSymbolTable = state.typeSymbolTable
    //           symtable = state.symbolTable
    //           parameters = parameters
    //           body = body }
    //     )


    // | EMatch(_, matchExpr, cases) ->
    //   /// Does the dval 'match' the given pattern?
    //   ///
    //   /// Returns:
    //   /// - whether or not the expr 'matches' the pattern
    //   /// - new vars (name * value)
    //   let rec checkPattern
    //     (dv : Dval)
    //     (pattern : MatchPattern)
    //     : Ply<bool * List<string * Dval>> =
    //     uply {
    //       // CLEANUP things down the line assume that the `id` in the callStack is an _Expression_ ID.
    //       // It might be nice to also allow for MP IDs. This would require a change in the callStack here.
    //       // let state = stateWithUpdatedCallStack (MatchPattern.toID pattern)
    //       // let callStack = state.tracing.callStack

    //       let errWrongType expected =
    //         raiseExeRTE
    //           callStack
    //           (ExecutionError.MatchExprPatternWrongType(expected, dv))

    //       match pattern with
    //       | MPUnit(_) ->
    //         match dv with
    //         | DUnit -> return true, []
    //         | _ -> return! errWrongType "Unit"

    //       | MPBool(_, pb) ->
    //         match dv with
    //         | DBool db -> return (db = pb), []
    //         | _ -> return! errWrongType "Bool"

    //       | MPInt8(_, pi) ->
    //         match dv with
    //         | DInt8 di -> return (di = pi), []
    //         | _ -> return! errWrongType "Int8"
    //       | MPUInt8(_, pi) ->
    //         match dv with
    //         | DUInt8 di -> return (di = pi), []
    //         | _ -> return! errWrongType "UInt8"
    //       | MPInt16(_, pi) ->
    //         match dv with
    //         | DInt16 di -> return (di = pi), []
    //         | _ -> return! errWrongType "Int16"
    //       | MPUInt16(_, pi) ->
    //         match dv with
    //         | DUInt16 di -> return (di = pi), []
    //         | _ -> return! errWrongType "UInt16"
    //       | MPInt32(_, pi) ->
    //         match dv with
    //         | DInt32 di -> return (di = pi), []
    //         | _ -> return! errWrongType "Int32"
    //       | MPUInt32(_, pi) ->
    //         match dv with
    //         | DUInt32 di -> return (di = pi), []
    //         | _ -> return! errWrongType "UInt32"
    //       | MPInt64(_, pi) ->
    //         match dv with
    //         | DInt64 di -> return (di = pi), []
    //         | _ -> return! errWrongType "Int64"
    //       | MPUInt64(_, pi) ->
    //         match dv with
    //         | DUInt64 di -> return (di = pi), []
    //         | _ -> return! errWrongType "UInt64"
    //       | MPInt128(_, pi) ->
    //         match dv with
    //         | DInt128 di -> return (di = pi), []
    //         | _ -> return! errWrongType "Int128"
    //       | MPUInt128(_, pi) ->
    //         match dv with
    //         | DUInt128 di -> return (di = pi), []
    //         | _ -> return! errWrongType "UInt128"

    //       | MPFloat(_, pf) ->
    //         match dv with
    //         | DFloat df -> return (df = pf), []
    //         | _ -> return! errWrongType "Float"

    //       | MPChar(_, pc) ->
    //         match dv with
    //         | DChar dc -> return (dc = pc), []
    //         | _ -> return! errWrongType "Char"
    //       | MPString(_, ps) ->
    //         match dv with
    //         | DString ds -> return (ds = ps), []
    //         | _ -> return! errWrongType "String"

    //       | MPEnum(_, caseName, fieldPats) ->
    //         match dv with
    //         | DEnum(_dTypeName, _oTypeName, _typeArgsDEnumTODO, dCaseName, dFields) ->
    //           if caseName <> dCaseName then
    //             return false, []
    //           else
    //             let dvFieldLength = List.length dFields
    //             match fieldPats with
    //             // wildcard
    //             | [ MPVariable(_, "_") ] when dvFieldLength > 0 -> return true, []
    //             | _ ->
    //               let patFieldLength = List.length fieldPats
    //               if dvFieldLength <> patFieldLength then
    //                 return!
    //                   raiseExeRTE
    //                     callStack
    //                     (ExecutionError.MatchExprEnumPatternWrongCount(
    //                       dCaseName,
    //                       patFieldLength,
    //                       dvFieldLength
    //                     ))
    //               else
    //                 let! (passResults, newVarResults) =
    //                   List.zip dFields fieldPats
    //                   |> Ply.List.mapSequentially (fun (dv, pat) ->
    //                     checkPattern dv pat)
    //                   |> Ply.map List.unzip

    //                 let allPass = List.forall identity passResults
    //                 let allVars = newVarResults |> List.collect identity
    //                 return allPass, allVars

    //         | _dv -> return! errWrongType caseName


    //       | MPTuple(_, firstPat, secondPat, theRestPat) ->
    //         let allPatterns = firstPat :: secondPat :: theRestPat

    //         match dv with
    //         | DTuple(first, second, theRest) ->
    //           let allVals = first :: second :: theRest

    //           if List.length allVals = List.length allPatterns then
    //             let! (passResults, newVarResults) =
    //               List.zip allVals allPatterns
    //               |> Ply.List.mapSequentially (fun (dv, pat) -> checkPattern dv pat)
    //               |> Ply.map List.unzip

    //             let allPass = List.forall identity passResults
    //             let allVars = newVarResults |> List.collect identity
    //             return allPass, allVars
    //           else
    //             return false, []
    //         | _ ->
    //           // TODO: specify length?
    //           return! errWrongType "Tuple"


    //       | MPListCons(_, headPat, tailPat) ->
    //         match dv with
    //         | DList(_, []) -> return false, []
    //         | DList(vt, headVal :: tailVals) ->
    //           let! (headPass, headVars) = checkPattern headVal headPat
    //           let! (tailPass, tailVars) =
    //             checkPattern
    //               (TypeChecker.DvalCreator.list callStack vt tailVals)
    //               tailPat

    //           let allSubVars = headVars @ tailVars
    //           let pass = headPass && tailPass
    //           return pass, allSubVars
    //         | _ -> return! errWrongType "List"

    //       | MPList(_, pats) ->
    //         match dv with
    //         | DList(_, vals) ->
    //           if List.length vals = List.length pats then
    //             let! (passResults, newVarResults) =
    //               List.zip vals pats
    //               |> Ply.List.mapSequentially (fun (dv, pat) -> checkPattern dv pat)
    //               |> Ply.map List.unzip

    //             let allPass = List.forall identity passResults
    //             let allVars = newVarResults |> List.collect identity
    //             return allPass, allVars
    //           else
    //             return false, []
    //         | _ -> return! errWrongType "List"

    //       | MPVariable(_, varName) -> return true, [ (varName, dv) ]
    //     }


    //   // The value we're matching against
    //   let! matchVal = eval state matchExpr

    //   let mutable matchResult = None

    //   for case in NEList.toList cases do
    //     if Option.isSome matchResult then
    //       ()
    //     else
    //       let! passesPattern, newDefs = checkPattern matchVal case.pat
    //       let newSymtable =
    //         Map.mergeFavoringRight state.symbolTable (Map.ofList newDefs)
    //       let state = { state with symbolTable = newSymtable }
    //       let! passesWhenCondition =
    //         uply {
    //           match case.whenCondition with
    //           | Some whenCondition when passesPattern ->
    //             match! eval state whenCondition with
    //             | DBool b -> return b
    //             | _ -> return errStr callStack "When condition should be a boolean"
    //           | _ -> return true
    //         }
    //       if passesPattern && passesWhenCondition then
    //         let! r = eval state case.rhs
    //         matchResult <- Some r

    //   match matchResult with
    //   | Some r -> return r
    //   | None ->
    //     return! raiseExeRTE callStack (ExecutionError.MatchExprUnmatched matchVal)


    // | EIf(_, cond, thenBody, elseBody) ->
    //   match! eval state cond with
    //   | DBool false ->
    //     match elseBody with
    //     | None -> return DUnit
    //     | Some eb -> return! eval state eb
    //   | DBool true -> return! eval state thenBody
    //   | _ -> return errStr callStack "If only supports Booleans"


    // | EOr(_, left, right) ->
    //   match! eval state left with
    //   | DBool true -> return DBool true
    //   | DBool false ->
    //     match! eval state right with
    //     | DBool _ as b -> return b
    //     | _ -> return errStr callStack "|| only supports Booleans"
    //   | _ -> return errStr callStack "|| only supports Booleans"


    // | EAnd(_, left, right) ->
    //   match! eval state left with
    //   | DBool false -> return DBool false
    //   | DBool true ->
    //     match! eval state right with
    //     | DBool _ as b -> return b
    //     | _ -> return errStr callStack "&& only supports Booleans"
    //   | _ -> return errStr callStack "&& only supports Booleans"


    // | EEnum(_, sourceTypeName, caseName, fields) ->
    //   let types = ExecutionState.availableTypes state

    //   let! (resolvedTypeName, _, cases) = enumMaybe callStack types sourceTypeName
    //   let case = cases |> NEList.find (fun c -> c.name = caseName)

    //   match case with
    //   | None ->
    //     return
    //       ExecutionError.raise
    //         callStack
    //         (ExecutionError.EnumConstructionCaseNotFound(sourceTypeName, caseName))

    //   | Some case ->
    //     if case.fields.Length <> fields.Length then
    //       let msg =
    //         $"Case `{caseName}` expected {case.fields.Length} fields but got {fields.Length}"
    //       return errStr callStack msg
    //     else
    //       let! (fields : List<Dval>) =
    //         Ply.List.foldSequentiallyWithIndex
    //           (fun
    //                fieldIndex
    //                fieldsSoFar
    //                ((enumFieldType : TypeReference), fieldExpr) ->
    //             uply {
    //               let! v = eval state fieldExpr

    //               let context =
    //                 TypeChecker.EnumField(
    //                   sourceTypeName,
    //                   case.name,
    //                   fieldIndex,
    //                   List.length fields,
    //                   enumFieldType
    //                 )

    //               // VTTODO: we should be passing in a proper tst, not Map.empty - right?
    //               match!
    //                 TypeChecker.unify context types Map.empty enumFieldType v
    //               with
    //               | Ok() -> return (List.append fieldsSoFar [ v ])
    //               | Error rte -> return raiseRTE callStack rte
    //             })
    //           []
    //           (List.zip case.fields fields)

    //       return!
    //         TypeChecker.DvalCreator.enum
    //           resolvedTypeName
    //           sourceTypeName
    //           caseName
    //           fields

    | EError(_, rte, exprs) ->
      let! (_ : List<Dval>) = Ply.List.mapSequentially (eval state) exprs
      return raiseRTE callStack rte
  }


and applyFnVal
  (state : ExecutionState)
  (fnVal : FnValImpl)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : DvalTask =
  match fnVal with
  //| Lambda l -> executeLambda state l args
  | NamedFn fn -> callFn state fn typeArgs args

// and executeLambda
//   (state : ExecutionState)
//   (l : LambdaImpl)
//   (args : NEList<Dval>)
//   : DvalTask =

//   // One of the reasons to take a separate list of params and args is to
//   // provide this error message here. We don't have this information in
//   // other places, and the alternative is just to provide incompletes
//   // with no context
//   let expectedLength = NEList.length l.parameters
//   let actualLength = NEList.length args
//   if expectedLength <> actualLength then
//     raiseRTE
//       state.tracing.callStack
//       (RuntimeError.oldError
//         $"Expected {expectedLength} arguments, got {actualLength}")

//   else
//     let checkPattern' = checkPattern state.tracing.callStack

//     let paramSyms =
//       NEList.map2 checkPattern' args l.parameters
//       |> NEList.toList
//       |> List.flatten
//       |> Map

//     let state =
//       { state with symbolTable = Map.mergeFavoringRight l.symtable paramSyms }

//     eval state l.body

and callFn
  (state : ExecutionState)
  (fnToCall : FQFnName.FQFnName)
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : DvalTask =
  uply {
    let! fn =
      match fnToCall with
      | FQFnName.Builtin std ->
        Map.find std state.builtins.fns |> Option.map builtInFnToFn |> Ply

      | FQFnName.Package pkg ->
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
        ExecutionError.raise
          state.tracing.callStack
          (ExecutionError.WrongNumberOfFnArgs(
            fnToCall,
            expectedTypeParams,
            expectedArgs,
            actualTypeArgs,
            actualArgs
          ))

      let state =
        let boundArgs =
          NEList.map2 (fun (p : Param) actual -> (p.name, actual)) fn.parameters args
          |> NEList.toList
          |> Map
        { state with
            symbolTable = Map.mergeFavoringRight state.symbolTable boundArgs }

      let state =
        let newlyBoundTypeArgs = List.zip fn.typeParams typeArgs |> Map
        { state with
            typeSymbolTable =
              Map.mergeFavoringRight state.typeSymbolTable newlyBoundTypeArgs }

      return! execFn state fnToCall fn typeArgs args

    | None ->
      // Functions which aren't available in the runtime (for whatever reason)
      // may have results available in traces. (use case: inspecting a cloud-run trace locally)
      let fnResult =
        state.tracing.loadFnResult
          (state.tracing.callStack.lastCalled, fnToCall)
          args

      match fnResult with
      | Some(result, _ts) -> return result
      | None ->
        return
          raiseRTE
            state.tracing.callStack
            (RuntimeError.oldError
              $"Function {FQFnName.toString fnToCall} is not found")
  }


and execFn
  (state : ExecutionState)
  (fnDesc : FQFnName.FQFnName)
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
    | Error rte -> return raiseRTE state.tracing.callStack rte
    | Ok() ->
      let! result =
        match fn.fn with
        | BuiltInFunction f ->
          let executionPoint = ExecutionPoint.Function fn.name

          state.tracing.traceExecutionPoint executionPoint

          let state =
            { state with tracing.callStack.lastCalled = (executionPoint, None) }

          uply {
            let! result =
              uply {
                try
                  return! f (state, typeArgs, NEList.toList args)
                with e ->
                  match e with
                  | RuntimeErrorException(None, rte) ->
                    // Sometimes it's awkward, in a Builtin fn impl, to pass around the callStack
                    // So we catch the exception here and add the callStack to it so it's handy in error-reporting
                    return raiseRTE state.tracing.callStack rte

                  | RuntimeErrorException _ -> return Exception.reraise e

                  | e ->
                    let context : Metadata =
                      [ "fn", fnDesc; "args", args; "typeArgs", typeArgs; "id", id ]
                    state.reportException state context e
                    // These are arbitrary errors, and could include sensitive
                    // information, so best not to show it to the user. If we'd
                    // like to show it to the user, we should catch it where it happens
                    // and give them a known safe error via a RuntimeError
                    return
                      raiseRTE
                        state.tracing.callStack
                        (RuntimeError.oldError "Unknown error")
              }

            if fn.previewable <> Pure then
              // TODO same thing here -- shouldn't require ourselves to pass in lastCalled - `tracing` should just get access to it underneath
              state.tracing.storeFnResult
                (state.tracing.callStack.lastCalled, fnDesc)
                args
                result

            return result
          }

        | PackageFunction(id, body) ->
          // maybe this should instead be something like `state.tracing.tracePackageFnCall tlid`?
          // and the `caller` would be updated by that function? (maybe `caller` is a read-only thing.)
          let executionPoint = ExecutionPoint.Function(FQFnName.Package id)

          state.tracing.traceExecutionPoint executionPoint

          let state =
            { state with
                tracing.callStack.lastCalled = (executionPoint, Some(Expr.toID body)) }

          eval state body

      match! TypeChecker.checkFunctionReturnType types typeSymbolTable fn result with
      | Error rte -> return raiseRTE state.tracing.callStack rte
      | Ok() -> return result
  }
