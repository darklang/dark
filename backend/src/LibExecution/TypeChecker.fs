module LibExecution.TypeChecker


open Prelude
open RuntimeTypes


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
      match location with
      | None -> Dval.optionNone
      | Some(tlid, id) ->
        let tlid = DInt(int64 tlid)
        let id = DInt(int64 id)
        Dval.optionSome (DTuple(tlid, id, []))


  module Context =
    let rec toDT (context : Context) : Dval =
      let nameTypeName = RuntimeError.name [ "TypeChecker" ] "Context" 0
      let (caseName, fields) =
        match context with
        | FunctionCallParameter(fnName, param, paramIndex, location) ->
          let fnName = RT2DT.FnName.toDT fnName
          let param = RT2DT.Param.toDT param
          let paramIndex = DInt paramIndex
          let location = Location.toDT location
          "FunctionCallParameter", [ fnName; param; paramIndex; location ]
        | FunctionCallResult(fnName, returnType, location) ->
          let fnName = RT2DT.FnName.toDT fnName
          let returnType = RT2DT.TypeReference.toDT returnType
          let location = Location.toDT location
          "FunctionCallResult", [ fnName; returnType; location ]
        | RecordField(recordTypeName, fieldName, fieldType, location) ->
          let recordTypeName = RT2DT.TypeName.toDT recordTypeName
          let fieldName = DString fieldName
          let fieldType = RT2DT.TypeReference.toDT fieldType
          let location = Location.toDT location
          "RecordField", [ recordTypeName; fieldName; fieldType; location ]
        | DictKey(key, typ, location) ->
          let key = DString key
          let typ = RT2DT.TypeReference.toDT typ
          let location = Location.toDT location
          "DictKey", [ key; typ; location ]
        | EnumField(enumTypeName,
                    caseName,
                    fieldIndex,
                    fieldCount,
                    fieldType,
                    location) ->
          let enumTypeName = RT2DT.TypeName.toDT enumTypeName
          let caseName = DString caseName
          let fieldIndex = DInt fieldIndex
          let fieldCount = DInt fieldCount
          let fieldType = RT2DT.TypeReference.toDT fieldType
          let location = Location.toDT location
          "EnumField",
          [ enumTypeName; caseName; fieldIndex; fieldCount; fieldType; location ]
        | DBQueryVariable(varName, expected, location) ->
          let varName = DString varName
          let expected = RT2DT.TypeReference.toDT expected
          let location = Location.toDT location
          "DBQueryVariable", [ varName; expected; location ]
        | DBSchemaType(name, expectedType, location) ->
          let name = DString name
          let expectedType = RT2DT.TypeReference.toDT expectedType
          let location = Location.toDT location
          "DBSchemaType", [ name; expectedType; location ]
        | ListIndex(index, listTyp, parent) ->
          let index = DInt index
          let listTyp = RT2DT.TypeReference.toDT listTyp
          let parent = toDT parent
          "ListIndex", [ index; listTyp; parent ]
        | TupleIndex(index, elementType, parent) ->
          let index = DInt index
          let elementType = RT2DT.TypeReference.toDT elementType
          let parent = toDT parent
          "TupleIndex", [ index; elementType; parent ]

      Dval.enum nameTypeName caseName fields


  let toRuntimeError (e : Error) : RuntimeError =
    match e with
    | ValueNotExpectedType(actualValue, expectedType, context) ->
      let fields =
        [ actualValue |> RT2DT.Dval.toDT
          expectedType |> RT2DT.TypeReference.toDT
          Context.toDT context ]

      RuntimeError.typeCheckerError (
        Dval.enum
          (RuntimeError.name [ "TypeChecker" ] "Error" 0)
          "ValueNotExpectedType"
          fields
      )

    | TypeDoesntExist(typeName, context) ->
      let fields = [ RT2DT.TypeName.toDT typeName; Context.toDT context ]

      RuntimeError.typeCheckerError (
        Dval.enum
          (RuntimeError.name [ "TypeChecker" ] "Error" 0)
          "TypeDoesntExist"
          fields
      )

let rec unifyValueType
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : ValueType)
  : Ply<Result<TypeSymbolTable, unit>> =
  let r = unifyValueType types

  let rMult
    (tst : TypeSymbolTable)
    (expected : List<TypeReference>)
    (actual : List<ValueType>)
    : Ply<Result<TypeSymbolTable, unit>> =
    if List.length expected <> List.length actual then
      Ply(Error())
    else
      List.zip expected actual
      |> Ply.List.foldSequentially
        (fun acc (e, a) ->
          match acc with
          | Error() -> Ply acc
          | Ok tst -> r tst e a)
        (Ok tst)

  uply {
    match expected, actual with
    | TVariable name, actual ->
      //debuG "TVariable" (name, actual)
      match Map.get name tst with
      | None -> return Ok(Map.add name actual tst)
      | Some t ->
        match Dval.mergeValueTypes t actual with
        | Ok merged -> return Ok(Map.add name merged tst)
        | Error err -> return Error()

    | _, ValueType.Unknown -> return Ok tst

    | TUnit, ValueType.Known KTUnit -> return Ok tst
    | TBool, ValueType.Known KTBool -> return Ok tst
    | TInt, ValueType.Known KTInt -> return Ok tst
    | TFloat, ValueType.Known KTFloat -> return Ok tst
    | TChar, ValueType.Known KTChar -> return Ok tst
    | TString, ValueType.Known KTString -> return Ok tst
    | TUuid, ValueType.Known KTUuid -> return Ok tst
    | TDateTime, ValueType.Known KTDateTime -> return Ok tst
    | TBytes, ValueType.Known KTBytes -> return Ok tst

    | TList innerT, ValueType.Known(KTList innerV) -> return! r tst innerT innerV

    | TDict innerT, ValueType.Known(KTDict innerV) -> return! r tst innerT innerV

    | TTuple(tFirst, tSecond, tRest),
      ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
      let expected = tFirst :: tSecond :: tRest
      let actual = vFirst :: vSecond :: vRest
      return! rMult tst expected actual

    | TCustomType(Error _, _), _ -> return Error()
    | TCustomType(Ok typeNameT, typeArgsT), actual ->
      match! Types.find typeNameT types with
      | None -> return Error()
      | Some expected ->
        match expected, actual with
        | { definition = TypeDeclaration.Alias aliasType }, _ ->
          match! getTypeReferenceFromAlias types aliasType with
          | Error _rte -> return Error()
          | Ok expected -> return! r tst expected actual

        | { definition = TypeDeclaration.Record _ },
          ValueType.Known(KTCustomType(typeNameV, typeArgsV)) ->
          if typeNameV <> typeNameT then
            return Error()
          else
            match! rMult tst typeArgsT typeArgsV with
            | Error() -> return Error()
            | Ok tst -> return Ok tst

        | { definition = TypeDeclaration.Enum _ },
          ValueType.Known(KTCustomType(typeNameV, typeArgsV)) ->
          if typeNameV <> typeNameT then
            return Error()
          else
            match! rMult tst typeArgsT typeArgsV with
            | Error() -> return Error()
            | Ok tst -> return Ok tst

        | _, _ -> return Error()

    | TFn(argTypes, returnType), ValueType.Known(KTFn(vArgs, vRet)) ->
      let expected = returnType :: (NEList.toList argTypes)
      let actual = vRet :: (NEList.toList vArgs)
      return! rMult tst expected actual

    | TPassword, ValueType.Known KTPassword -> return Ok tst
    | TDB innerT, ValueType.Known(KTDB innerV) -> return! r tst innerT innerV


    | _, _ -> return Error()
  }


let unify
  (context : Context)
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (actual : Dval)
  : Ply<Result<TypeSymbolTable, RuntimeError>> =
  uply {
    let actualType = Dval.toValueType actual
    match! unifyValueType types tst expected actualType with
    | Error() ->
      match! getTypeReferenceFromAlias types expected with
      | Error rte -> return Error rte
      | Ok expected ->
        return
          ValueNotExpectedType(actual, expected, context)
          |> Error.toRuntimeError
          |> Error
    | Ok updatedTst -> return Ok updatedTst
  }


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
  (typeArgs : List<TypeReference>)
  (args : NEList<Dval>)
  : Ply<Result<TypeSymbolTable, RuntimeError>> =
  uply {
    // given let myFn<'a>(x: 'a, y: int): List<'a> = ...
    // and a call to myFn<Int>(1, 2),
    // update the tst with "a" -> "Int"
    let typeSymbolsBoundExplicitly =
      List.zip
        fn.typeParams
        (List.map (Types.ValueType.fromTypeReference tst) typeArgs)
      |> Map

    let tst = Map.mergeFavoringRight tst typeSymbolsBoundExplicitly

    return!
      Ply.List.foldSequentiallyWithIndex
        (fun i acc (param, value) ->
          uply {
            match acc with
            | Error _ as err -> return err
            | Ok tst ->
              let context = FunctionCallParameter(fn.name, param, i, None)
              return! unify context types tst param.typ value
          })
        (Ok tst)
        (NEList.zip fn.parameters args |> NEList.toList)
  }


let checkFunctionReturnType
  (types : Types)
  (tst : TypeSymbolTable)
  (fn : Fn)
  (result : Dval)
  : Ply<Result<TypeSymbolTable, RuntimeError>> =
  let context = FunctionCallResult(fn.name, fn.returnType, None)
  unify context types tst fn.returnType result
