module LibExecution.TypeChecker


open Prelude
open VendoredTablecloth
open RuntimeTypes

/// Returns `Ok ()` if no errors, or `Error first` otherwise
let combineErrorsUnit (l : List<Result<unit, 'err>>) : Result<unit, 'err> =
  l |> List.find Result.isError |> Option.unwrap (Ok())

// let combineErrorsTat
//   (l : List<Result<TypeSymbolTable, 'err>>)
//   : Result<TypeSymbolTable, 'err> =
//   List.fold
//     (Ok(Map.empty))
//     (fun acc r ->
//       match acc, r with
//       | Ok acc, Ok r -> Ok(Map.mergeFavoringRight acc r)
//       | Error e, _ -> Error e
//       | _, Error e -> Error e)
//     l



type Location = Option<tlid * id>
type Context =
  | FunctionCallParameter of
    fnName : FnName.T *
    parameter : Param *
    paramIndex : int *
    // caller : Option<tlid * id> * // TODO add caller
    location : Location
  | FunctionCallResult of
    fnName : FnName.T *
    returnType : TypeReference *
    // caller : Option<tlid * id> * // TODO add caller
    location : Location
  | RecordField of
    recordTypeName : TypeName.T *
    fieldName : string *
    fieldType : TypeReference *
    location : Location
  | DictKey of key : string * typ : TypeReference * Location
  | EnumField of
    enumTypeName : TypeName.T *
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
  | MismatchedRecordFields of
    typeName : TypeName.T *
    extraFieldsInActualValue : Set<string> *
    missingFields : Set<string> *
    Context
  | ValueNotExpectedType of
    actualValue : Dval *
    expectedType : TypeReference *
    Context
  | VTTODOMismatchedType
  | TypeDoesntExist of TypeName.T * Context
  | VTTODOMergedValueTypeWrapper of Dval.SomeErrorType

//let something (expected: TypeReference) (actual: ValueType)


(*
  let myUnwrap (r: List<Result<'a, String>>): 'a =
    ..



  myUnwrap [Ok 1; Error "error"] // DList((KTCustomType, "Result", [Some VTnt, None]), [DInt 1, DString "error"])

  then we typeCheck
    KTCustomType("Result", [Some VTnt, None])
  against
    TCustomType Result [TVariable "a", TString]

  we recurse into the typecheck
  and compare Some KTInt against TVariable "a",
  with tst in context

  do we also 'update' the DList to have a concrete KTString instead of the None?
  we're not expecting to update the value in the symtable,
  but maybe the updated dval is useful in later type-checking?

  if we update the dlist we will have a better ...

  suppose `List.take 0 [1, 2, 3]`
  it's useful to learn from the dval (1,2,3) that we're working with a list of _ints_

  actually, suppose `List.take 0 []`

  let myUnwrap' (r: List<Result<'a, String>>): List<Result<'a, String>> =
    r

  if you `myUnwrap [Ok 1]`,
  r should be know to be a List<Result<Int, String>> with one element

  this should be done in tghe unifyReturnType

  VTTODO investigate whether we can update/improve the concrete type  and insert it in the dv
  (by way of merging it with the TypeReference)
*)

let rec unifyType
  (context: Context)
  (tst: TypeSymbolTable)
  (expected: TypeReference)
  (actual: ValueType)
  : Result<TypeSymbolTable, Error> =
  match (expected, actual) with
  | TVariable name, actual ->
    match Map.get name tst with
    | None -> Ok(Map.add name actual tst)
    | Some t ->
      match Dval.mergeValueTypes t actual with
      | Ok merged -> Ok(Map.add name merged tst)
      | Error err -> Error (VTTODOMergedValueTypeWrapper err)

  | TUnit, Known KTUnit -> Ok tst
  | TBool, Known KTBool -> Ok tst
  | TInt, Known KTInt -> Ok tst
  | TFloat, Known KTFloat -> Ok tst
  | TChar, Known KTChar -> Ok tst
  | TString, Known KTString -> Ok tst
  | TUuid, Known KTUuid -> Ok tst
  | TBytes, Known KTBytes -> Ok tst
  | TDateTime, Known KTDateTime -> Ok tst
  | TPassword, Known KTPassword -> Ok tst

  | TList tInner , Known (KTList vtInner) ->
    unifyType context tst tInner vtInner

  | TDict tInner, Known (KTDict vtInner) ->
    unifyType context tst tInner vtInner

  // VTTODO: test case like ('a, List<'a>, ...) // where an early var is used later
  | TTuple(t1, t2, tRest), Known (KTTuple(v1, v2, vRest)) ->
    let expectedTypes = t1 :: t2 :: tRest
    let actualTypes = v1 :: v2 :: vRest
    if List.length expectedTypes <> List.length actualTypes then
      Exception.raiseInternal "Incorrect number of params" []
    else
      List.zip expectedTypes actualTypes
      |> List.fold
        (Ok tst)
        (fun acc (e, a) ->
          match acc with
          | Ok tst -> unifyType context tst e a
          | Error _ as err -> err)

  // VTTODO: test where early arg types are used later
  | TFn (expectedArgs, expectedRet), Known (KTFn (actualArgs, actualRet)) ->
    let expectedTypes = expectedArgs @ [expectedRet]
    let actualTypes = actualArgs @ [actualRet]
    if List.length expectedTypes <> List.length actualTypes then
      Exception.raiseInternal "Incorrect number of params" []
    else
      List.zip expectedTypes actualTypes
      |> List.fold
        (Ok tst)
        (fun acc (e, a) ->
          match acc with
          | Ok tst -> unifyType context tst e a
          | Error _ as err -> err)

  | TDB inner, Known _ ->
    unifyType context tst inner actual

  | TCustomType (expectedName, expectedTypeArgs), Known (KTCustomType(actualName, actualTypeArgs)) ->
    if expectedName <> actualName then
      Error VTTODOMismatchedType
    else if List.length expectedTypeArgs <> List.length actualTypeArgs then
      Exception.raiseInternal "Incorrect name of params" []
    else
      List.zip expectedTypeArgs actualTypeArgs
      |> List.fold
        (Ok tst)
        (fun acc (e, a) ->
          match acc with
          | Ok tst -> unifyType context tst e a
          | Error _ as err -> err)




  // TODO: in the future, we could use this new type `t` to update the type
  // information in the value which had this valueType
  | _t, Unknown -> Ok tst

  | TInt, _
  | TFloat, _
  | TBool, _
  | TUnit, _
  | TString, _
  | TDateTime, _
  | TPassword, _
  | TUuid, _
  | TChar, _
  | TBytes, _
  | TList _, _
  | TDict _, _
  | TFn _, _
  | TTuple _, _
  | TCustomType _, _
  | TDB _, _ ->
    let expected = expected |> Types.ValueType.fromTypeReference
    Dval.mergeValueTypes expected actual // should fail
    |> Result.map (fun _ -> tst)
    |> Result.mapError VTTODOMergedValueTypeWrapper

// let rec unifyType
//   (context: Context)
//   (tst: TypeSymbolTable)
//   (expected: TypeReference)
//   (actual: ValueType)
//   : Result<TypeSymbolTable, Error> =
let unify
  (context: Context)
  (types : Types)
  (tst: TypeSymbolTable)
  (expected: TypeReference)
  (actual: Dval)
   : Ply<Result<TypeSymbolTable, Error>> = uply {
    let! resolvedType = Types.getTypeReferenceFromAlias types expected
    let actualType = Dval.toKnownType actual
    return unifyType context tst resolvedType (Known actualType)
    // VTTODO: use or delete: Error(ValueNotExpectedType(value, resolvedType, context))
   }


// VTTODO: there are missing type checks around type arguments that we should backfill.
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
  (args : List<Dval>)
  : Ply<Result<TypeSymbolTable, Error>> =
  // The interpreter checks these are the same length
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
    (List.zip fn.parameters args)


let checkFunctionReturnType
  (types : Types)
  (typeArgSymbolTable : TypeSymbolTable)
  (fn : Fn)
  (result : Dval)
  : Ply<Result<TypeSymbolTable, Error>> =
  let context = FunctionCallResult(fn.name, fn.returnType, None)
  unify context types typeArgSymbolTable fn.returnType result
