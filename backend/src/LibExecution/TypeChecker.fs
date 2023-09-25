module LibExecution.TypeChecker


open Prelude
open RuntimeTypes
module VT = ValueType


/// Returns `Ok ()` if no errors, or `Error first` otherwise
let combineErrorsUnit (l : NEList<Result<unit, 'err>>) : Result<unit, 'err> =
  l |> NEList.find Result.isError |> Option.unwrap (Ok())



type Location = Option<tlid * id>
type Context =
  | FunctionCallParameter of
    fnName : FnName.FnName *
    parameter : Param *
    paramIndex : int *
    // caller : Option<tlid * id> * // TODO add caller
    location : Location
  | FunctionCallResult of
    fnName : FnName.FnName *
    returnType : TypeReference *
    // caller : Option<tlid * id> * // TODO add caller
    location : Location
  | RecordField of
    recordTypeName : TypeName.TypeName *
    fieldName : string *
    fieldType : TypeReference *
    location : Location
  | DictKey of key : string * typ : TypeReference * Location
  | EnumField of
    enumTypeName : TypeName.TypeName *
    caseName : string *
    fieldIndex : int *  // nth argument to the enum constructor
    fieldCount : int *
    fieldType : TypeReference *
    location : Location
  | DBQueryVariable of
    varName : string *
    expected : TypeReference *
    location : Location
  | DBSchemaType of
    name : string *
    expectedType : TypeReference *
    location : Location
  | ListIndex of index : int * listTyp : TypeReference * parent : Context
  | TupleIndex of index : int * elementType : TypeReference * parent : Context
  | FnValResult of returnType : TypeReference * location : Location


module Context =
  let rec toLocation (c : Context) : Location =
    match c with
    | FunctionCallParameter(_, _, _, location) -> location
    | FunctionCallResult(_, _, location) -> location
    | RecordField(_, _, _, location) -> location
    | DictKey(_, _, location) -> location
    | EnumField(_, _, _, _, _, location) -> location
    | DBQueryVariable(_, _, location) -> location
    | DBSchemaType(_, _, location) -> location
    | ListIndex(_, _, parent) -> toLocation parent
    | TupleIndex(_, _, parent) -> toLocation parent
    | FnValResult(_, location) -> location

type Error =
  | ValueNotExpectedType of
    // CLEANUP consider reordering fields to (context * expectedType * actualValue)
    actualValue : Dval *
    expectedType : TypeReference *
    Context
  | TypeDoesntExist of TypeName.TypeName * Context



module Error =

  module RT2DT = RuntimeTypesToDarkTypes

  module Location =
    let toDT (location : Location) : Dval =
      let optType = VT.unknownTODO
      match location with
      | None -> Dval.optionNone optType
      | Some(tlid, id) ->
        let tlid = DInt(int64 tlid)
        let id = DInt(int64 id)
        Dval.optionSome optType (DTuple(tlid, id, []))


  module Context =
    let rec toDT (context : Context) : Ply<Dval> =
      uply {
        let! (caseName, fields) =
          uply {
            match context with
            | FunctionCallParameter(fnName, param, paramIndex, location) ->
              let! fnName = RT2DT.FnName.toDT fnName
              let! param = RT2DT.Param.toDT param
              return
                "FunctionCallParameter",
                [ fnName; param; DInt paramIndex; Location.toDT location ]

            | FunctionCallResult(fnName, returnType, location) ->
              let! fnName = RT2DT.FnName.toDT fnName
              let! returnType = RT2DT.TypeReference.toDT returnType
              return
                "FunctionCallResult", [ fnName; returnType; Location.toDT location ]

            | RecordField(recordTypeName, fieldName, fieldType, location) ->
              let! typeName = RT2DT.TypeName.toDT recordTypeName
              let! fieldType = RT2DT.TypeReference.toDT fieldType
              return
                "RecordField",
                [ typeName; DString fieldName; fieldType; Location.toDT location ]

            | DictKey(key, typ, location) ->
              let! typ = RT2DT.TypeReference.toDT typ
              return "DictKey", [ DString key; typ; Location.toDT location ]

            | EnumField(enumTypeName,
                        caseName,
                        fieldIndex,
                        fieldCount,
                        fieldType,
                        location) ->
              let! typeName = RT2DT.TypeName.toDT enumTypeName
              let! fieldType = RT2DT.TypeReference.toDT fieldType
              return
                "EnumField",
                [ typeName
                  DString caseName
                  DInt fieldIndex
                  DInt fieldCount
                  fieldType
                  Location.toDT location ]

            | DBQueryVariable(varName, expected, location) ->
              let! expected = RT2DT.TypeReference.toDT expected
              return
                "DBQueryVariable",
                [ DString varName; expected; Location.toDT location ]

            | DBSchemaType(name, expectedType, location) ->
              let! expectedType = RT2DT.TypeReference.toDT expectedType
              return
                "DBSchemaType",
                [ DString name; expectedType; Location.toDT location ]

            | ListIndex(index, listTyp, parent) ->
              let! listType = RT2DT.TypeReference.toDT listTyp
              let! parent = toDT parent
              return "ListIndex", [ DInt index; listType; parent ]

            | TupleIndex(index, elementType, parent) ->
              let! elType = RT2DT.TypeReference.toDT elementType
              let! parent = toDT parent
              return "TupleIndex", [ DInt index; elType; parent ]

            | FnValResult(returnType, location) ->
              let! returnType = RT2DT.TypeReference.toDT returnType
              return "FnValResult", [ returnType; Location.toDT location ]
          }

        let typeName = RuntimeError.name [ "TypeChecker" ] "Context" 0
        return! Dval.enum typeName typeName (Some []) caseName fields
      }


  let toRuntimeError (e : Error) : Ply<RuntimeError> =
    uply {
      let! (caseName, fields) =
        uply {
          match e with
          | ValueNotExpectedType(actualValue, expectedType, context) ->
            let! actualValue = actualValue |> RT2DT.Dval.toDT
            let! expectedType = expectedType |> RT2DT.TypeReference.toDT
            let! context = Context.toDT context
            return "ValueNotExpectedType", [ actualValue; expectedType; context ]

          | TypeDoesntExist(typeName, context) ->
            let! typeName = RT2DT.TypeName.toDT typeName
            let! context = Context.toDT context
            return "TypeDoesntExist", [ typeName; context ]
        }

      let typeName = RuntimeError.name [ "TypeChecker" ] "Error" 0
      return!
        Dval.enum typeName typeName (Some []) caseName fields
        |> Ply.map RuntimeError.typeCheckerError
    }

let raiseValueNotExpectedType
  (source : DvalSource)
  (dv : Dval)
  (typ : TypeReference)
  (context : Context)
  : Ply<'a> =
  ValueNotExpectedType(dv, typ, context)
  |> Error.toRuntimeError
  |> Ply.map (raiseRTE source)

let raiseFnValResultNotExpectedType
  (source : DvalSource)
  (dv : Dval)
  (typ : TypeReference)
  : Ply<'a> =
  let location =
    match source with
    | SourceNone -> None
    | SourceID(tlid, id) -> Some(tlid, id)
  let context = FnValResult(typ, location)
  ValueNotExpectedType(dv, typ, context)
  |> Error.toRuntimeError
  |> Ply.map (raiseRTE source)



let rec valueTypeUnifies
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : ValueType)
  : Ply<bool> =
  let r = valueTypeUnifies tst

  let rMult (expected : List<TypeReference>) (actual : List<ValueType>) : Ply<bool> =
    if List.length expected <> List.length actual then
      Ply false
    else
      List.zip expected actual
      |> Ply.List.foldSequentially
        (fun acc (e, a) ->
          match acc with
          | false -> Ply acc
          | true -> r e a)
        true

  uply {
    match expected, actual with
    | _, ValueType.Unknown -> return true

    | TUnit, ValueType.Known KTUnit -> return true
    | TBool, ValueType.Known KTBool -> return true
    | TInt, ValueType.Known KTInt -> return true
    | TFloat, ValueType.Known KTFloat -> return true
    | TChar, ValueType.Known KTChar -> return true
    | TString, ValueType.Known KTString -> return true
    | TUuid, ValueType.Known KTUuid -> return true
    | TDateTime, ValueType.Known KTDateTime -> return true
    | TBytes, ValueType.Known KTBytes -> return true

    | TList innerT, ValueType.Known(KTList innerV) -> return! r innerT innerV

    | TDict innerT, ValueType.Known(KTDict innerV) -> return! r innerT innerV

    | TTuple(tFirst, tSecond, tRest),
      ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
      let expected = tFirst :: tSecond :: tRest
      let actual = vFirst :: vSecond :: vRest
      return! rMult expected actual

    | TCustomType(Error _, _), _ ->
      return
        Exception.raiseInternal
          $"Unexpected - Error typeName in TCustomType in unifyValueType"
          []
    | TCustomType(Ok _typeNameT, _typeArgsT),
      ValueType.Known(KTCustomType(_typeNameV, _typeArgsV)) ->
      // TODO: follow up here when:
      // - type name aliases are and resolved
      // - type args are properly passed around and handled

      // TODO: assert type names are the same,
      // after we've handled all type aliases
      //return! rMult typeArgsT typeArgsV
      return true

    | TFn(argTypes, returnType), ValueType.Known(KTFn(vArgs, vRet)) ->
      // TODO: follow up here when type args are properly passed around and handled

      // let expected = returnType :: (NEList.toList argTypes)
      // let actual = vRet :: (NEList.toList vArgs)
      // return! rMult expected actual
      return true

    | TDB innerT, ValueType.Known(KTDB innerV) -> return! r innerT innerV

    | TVariable name, _ ->
      match Map.get name tst with
      | None -> return true
      | Some t -> return! r t actual

    | _, _ -> return false
  }

let rec unify
  (context : Context)
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (value : Dval)
  : Ply<Result<unit, RuntimeError>> =
  uply {
    match! getTypeReferenceFromAlias types expected with
    | Error rte -> return Error rte
    | Ok expected ->
      match (expected, value) with
      // Any should be removed, but we currently allow it as a param type
      // in user functions, so we should allow it here.
      //
      // Potentially needs to be removed before we use this type checker for DBs?
      //   - Could always have a type checking context that allows/disallows any
      | TVariable name, _ ->
        match Map.get name tst with
        // for now, allow undefined type variables. In the future, we would create a
        // type from the value and return any variables defined this way for usage in
        // further arguments and return values.
        | None -> return Ok()
        | Some t -> return! unify context types tst t value
      | TInt, DInt _ -> return Ok()
      | TFloat, DFloat _ -> return Ok()
      | TBool, DBool _ -> return Ok()
      | TUnit, DUnit -> return Ok()
      | TString, DString _ -> return Ok()
      | TDateTime, DDateTime _ -> return Ok()
      | TUuid, DUuid _ -> return Ok()
      | TChar, DChar _ -> return Ok()
      | TBytes, DBytes _ -> return Ok()
      | TDB _, DDB _ -> return Ok() // TODO: check DB type
      | TList expected, DList(actual, _dvs) ->
        match! valueTypeUnifies tst expected actual with
        | false ->
          return!
            ValueNotExpectedType(value, TList expected, context)
            |> Error.toRuntimeError
            |> Ply.map Error

        | true -> return! Ply()

      | TDict _expected, DDict(_actual, _entries) ->
        // VTTODO uncomment this
        // match! valueTypeUnifies tst expected actual with
        // | false ->
        //   return
        //     ValueNotExpectedType(value, expected, context)
        //     |> Error.toRuntimeError
        //     |> Error

        // | true -> return! Ply()
        return Ok()

      | TFn(argTypes, returnType), DFnVal fnVal -> return Ok() // TYPESTODO check lambdas and fnVals
      | TTuple(t1, t2, tRest), DTuple(v1, v2, vRest) ->
        let ts = t1 :: t2 :: tRest
        let vs = v1 :: v2 :: vRest
        if List.length ts <> List.length vs then
          return!
            ValueNotExpectedType(value, expected, context)
            |> Error.toRuntimeError
            |> Ply.map Error
        else
          // let! results =
          //   List.zip ts vs
          //   |> Ply.List.mapSequentiallyWithIndex (fun i (t, v) ->
          //     let context = TupleIndex(i, t, context)
          //     unify context types tst t v)
          // return combineErrorsUnit results
          // CLEANUP DTuple should include a TypeReference for each part, in which
          // case the type-checking here would just be a comparison of typeRefs.
          // (the construction of that DTuple should have already checked that the
          // types match)
          return Ok()

      // TYPESCLEANUP: handle typeArgs
      | TCustomType(typeName, _typeArgs), value ->

        match typeName with
        | Error rte -> return Error rte
        | Ok typeName ->
          match! Types.find typeName types with
          | None ->
            return!
              TypeDoesntExist(typeName, context)
              |> Error.toRuntimeError
              |> Ply.map Error
          | Some ut ->
            let err =
              ValueNotExpectedType(value, expected, context)
              |> Error.toRuntimeError
              |> Ply.map Error

            match ut, value with
            | { definition = TypeDeclaration.Alias aliasType }, _ ->
              let! resolvedAliasType = getTypeReferenceFromAlias types aliasType

              match resolvedAliasType with
              | Error rte -> return Error rte
              | Ok resolvedAliasType ->
                return! unify context types tst resolvedAliasType value

            | { definition = TypeDeclaration.Record _ },
              DRecord(tn, _, _valueTypesTODO, _fields) ->
              // TYPESCLEANUP: this search should no longer be required
              let! aliasedType =
                getTypeReferenceFromAlias types (TCustomType(Ok tn, []))
              match aliasedType with
              | Ok(TCustomType(Error rte, _)) -> return Error rte
              | Ok(TCustomType(Ok concreteTn, typeArgs)) ->
                if concreteTn <> typeName then
                  return!
                    ValueNotExpectedType(value, expected, context)
                    |> Error.toRuntimeError
                    |> Ply.map Error
                else
                  // CLEANUP DRecord should include a TypeReference, in which case
                  // the type-checking here would just be a `tField = dField` check.
                  // (the construction of that DRecord should have already checked
                  // that the fields match)
                  return Ok()
              | _ -> return! err

            | { definition = TypeDeclaration.Enum cases },
              DEnum(tn, _, _typeArgsDEnumTODO, caseName, valFields) ->
              // TODO: deal with aliased type?
              if tn <> typeName then
                return!
                  ValueNotExpectedType(value, expected, context)
                  |> Error.toRuntimeError
                  |> Ply.map Error
              else
                let matchingCase : Option<TypeDeclaration.EnumCase> =
                  cases |> NEList.find (fun c -> c.name = caseName)

                match matchingCase with
                | None -> return! err
                | Some case ->
                  if List.length case.fields = List.length valFields then
                    // let! unified =
                    //   List.zip case.fields valFields
                    //   |> List.mapi (fun i (expected, actual) ->
                    //     let context =
                    //       EnumField(
                    //         tn,
                    //         expected,
                    //         case.name,
                    //         i,
                    //         Context.toLocation context
                    //       )
                    //     unify context types tst expected.typ actual)
                    //   |> Ply.List.mapSequentially identity

                    // return combineErrorsUnit unified
                    // CLEANUP DEnum should include a TypeReference, in which case
                    // the type-checking here would just be a `tField = dField` check.
                    // (the construction of that DEnum should have already checked
                    // that the fields match)
                    return Ok()
                  else
                    return! err
            | _, _ -> return! err

      // See https://github.com/darklang/dark/issues/4239#issuecomment-1175182695
      // TODO: exhaustiveness check
      | TTuple _, _
      | TCustomType _, _
      | TVariable _, _
      | TInt, _
      | TFloat, _
      | TBool, _
      | TUnit, _
      | TString, _
      | TList _, _
      | TDateTime, _
      | TDict _, _
      | TFn _, _
      | TUuid, _
      | TChar, _
      | TDB _, _
      | TBytes, _ ->
        return!
          ValueNotExpectedType(value, expected, context)
          |> Error.toRuntimeError
          |> Ply.map Error
  }



// TODO: there are missing type checks around type arguments that we should backfill.
//
// given a function
// ```fsharp
// let f<'a>(x: 'a, y: int): List<'a> =
//   ...
// ```
// - we should check that `x` is compatible with `'a`
// - we should check that the return type is compatible with `List<'a>`
//
// also, given the function
// ```fsharp
// let f<'a, 'a, 'b>(x: 'a, y: int): List<'a> =
//   ...
// ```
// - we should raise an error on the same type param name being used more than once
// - we should raise an error/warning that 'b isn't used anywhere
//
// These will involve updates in both `checkFunctionCall` and `checkFunctionReturnType`.

let checkFunctionCall
  (types : Types)
  (tst : TypeSymbolTable)
  (fn : Fn)
  (args : NEList<Dval>)
  : Ply<Result<unit, RuntimeError>> =
  // The interpreter checks these are the same length
  fn.parameters
  |> NEList.map2WithIndex
    (fun i value param ->
      let context = FunctionCallParameter(fn.name, param, i, None)
      unify context types tst param.typ value)
    args
  |> Ply.NEList.mapSequentially identity
  |> Ply.map combineErrorsUnit


let checkFunctionReturnType
  (types : Types)
  (tst : TypeSymbolTable)
  (fn : Fn)
  (result : Dval)
  : Ply<Result<unit, RuntimeError>> =
  let context = FunctionCallResult(fn.name, fn.returnType, None)
  unify context types tst fn.returnType result
