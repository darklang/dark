module LibExecution.TypeChecker


open Prelude
open VendoredTablecloth
open RuntimeTypes

/// Returns `Ok ()` if no errors, or `Error first` otherwise
let combineErrorsUnit (l : List<Result<unit, 'err>>) : Result<unit, 'err> =
  l |> List.find Result.isError |> Option.unwrap (Ok())



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
    fieldDef : TypeDeclaration.RecordField *
    location : Location
  | DictKey of key : string * typ : TypeReference * Location
  | EnumField of
    enumTypeName : TypeName.T *
    definition : TypeDeclaration.EnumField *
    caseName : string *
    paramIndex : int *  // nth argument to the enum constructor
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
    | RecordField(_, _, location) -> location
    | DictKey(_, _, location) -> location
    | EnumField(_, _, _, _, location) -> location
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
  | TypeDoesntExist of TypeName.T * Context



let rec unify
  (context : Context)
  (types : Types)
  (expected : TypeReference)
  (value : Dval)
  : Ply<Result<unit, Error>> =
  uply {
    let! resolvedType = getTypeReferenceFromAlias types expected

    match (resolvedType, value) with
    // Any should be removed, but we currently allow it as a param type
    // in user functions, so we should allow it here.
    //
    // Potentially needs to be removed before we use this type checker for DBs?
    //   - Could always have a type checking context that allows/disallows any
    | TVariable _, _ -> return Ok() // TYPESCLEANUP actually unify this
    | TInt, DInt _ -> return Ok()
    | TFloat, DFloat _ -> return Ok()
    | TBool, DBool _ -> return Ok()
    | TUnit, DUnit -> return Ok()
    | TString, DString _ -> return Ok()
    | TDateTime, DDateTime _ -> return Ok()
    | TPassword, DPassword _ -> return Ok()
    | TUuid, DUuid _ -> return Ok()
    | TChar, DChar _ -> return Ok()
    | TBytes, DBytes _ -> return Ok()
    | TDB _, DDB _ -> return Ok() // TODO: check DB type
    | TList nested, DList dvs ->
      let! results =
        dvs
        |> Ply.List.mapSequentiallyWithIndex (fun i v ->
          let context = ListIndex(i, nested, context)
          unify context types nested v)
      return combineErrorsUnit results
    | TDict valueType, DDict dmap ->
      let! results =
        dmap
        |> Map.toList
        |> Ply.List.mapSequentially (fun (k, v) ->
          let location = Context.toLocation context
          let context = DictKey(k, valueType, location)
          unify context types valueType v)
      return combineErrorsUnit results

    | TFn(argTypes, returnType), DFnVal fnVal -> return Ok() // TYPESTODO check lambdas and fnVals
    | TTuple(t1, t2, tRest), DTuple(v1, v2, vRest) ->
      let ts = t1 :: t2 :: tRest
      let vs = v1 :: v2 :: vRest
      if List.length ts <> List.length vs then
        return Error(ValueNotExpectedType(value, resolvedType, context))
      else
        let! results =
          List.zip ts vs
          |> Ply.List.mapSequentiallyWithIndex (fun i (t, v) ->
            let context = TupleIndex(i, t, context)
            unify context types t v)
        return combineErrorsUnit results

    // TYPESCLEANUP: handle typeArgs
    | TCustomType(typeName, typeArgs), value ->

      match! Types.find typeName types with
      | None -> return Error(TypeDoesntExist(typeName, context))
      | Some ut ->
        let err = Error(ValueNotExpectedType(value, resolvedType, context))

        match ut, value with
        | { definition = TypeDeclaration.Alias aliasType }, _ ->
          let! resolvedAliasType = getTypeReferenceFromAlias types aliasType
          return! unify context types resolvedAliasType value

        | { definition = TypeDeclaration.Record(firstField, additionalFields) },
          DRecord(tn, dmap) ->
          let! aliasedType = getTypeReferenceFromAlias types (TCustomType(tn, []))
          match aliasedType with
          | TCustomType(concreteTn, typeArgs) ->
            if concreteTn <> typeName then
              let! expected =
                getTypeReferenceFromAlias types (TCustomType(typeName, []))
              return Error(ValueNotExpectedType(value, expected, context))
            else
              return!
                unifyRecordFields
                  concreteTn
                  context
                  types
                  (firstField :: additionalFields)
                  dmap
          | _ -> return err

        | { definition = TypeDeclaration.Enum(firstCase, additionalCases) },
          DEnum(tn, caseName, valFields) ->
          // TODO: deal with aliased type?
          if tn <> typeName then
            return Error(ValueNotExpectedType(value, resolvedType, context))
          else
            let matchingCase : Option<TypeDeclaration.EnumCase> =
              firstCase :: additionalCases |> List.find (fun c -> c.name = caseName)

            match matchingCase with
            | None -> return err
            | Some case ->
              if List.length case.fields = List.length valFields then
                let! unified =
                  List.zip case.fields valFields
                  |> List.mapi (fun i (expected, actual) ->
                    let context =
                      EnumField(
                        tn,
                        expected,
                        case.name,
                        i,
                        Context.toLocation context
                      )
                    unify context types expected.typ actual)
                  |> Ply.List.mapSequentially identity

                return combineErrorsUnit unified
              else
                return err
        | _, _ -> return err

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
    | TPassword, _
    | TUuid, _
    | TChar, _
    | TDB _, _
    | TBytes, _ -> return Error(ValueNotExpectedType(value, resolvedType, context))
  }



and unifyRecordFields
  (recordType : TypeName.T)
  (context : Context)
  (types : Types)
  (defs : List<TypeDeclaration.RecordField>)
  (values : DvalMap)
  : Ply<Result<unit, Error>> =
  let completeDefinition =
    defs
    |> List.filterMap (fun (d : TypeDeclaration.RecordField) ->
      if d.name = "" then None else Some(d.name, d))
    |> Map.ofList

  let defNames = completeDefinition |> Map.keys |> Set.ofList
  let valueNames = values |> Map.keys |> Set.ofList

  if defNames = valueNames then
    let location = Context.toLocation context
    values
    |> Map.toList
    |> List.map (fun (fieldName, fieldValue) ->
      let fieldDef =
        Map.get fieldName completeDefinition
        |> Exception.unwrapOptionInternal
          "field name missing from type"
          [ "fieldName", fieldName ]
      let context = RecordField(recordType, fieldDef, location)
      unify context types fieldDef.typ fieldValue)
    |> Ply.List.flatten
    |> Ply.map combineErrorsUnit
  else
    let extraFields = Set.difference valueNames defNames
    let missingFields = Set.difference defNames valueNames
    Error(MismatchedRecordFields(recordType, extraFields, missingFields, context))
    |> Ply


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
  (fn : Fn)
  (_typeArgs : List<TypeReference>)
  (args : List<Dval>)
  : Ply<Result<unit, Error>> =
  fn.parameters
  |> List.mapi2
    (fun i value param ->
      let context = FunctionCallParameter(fn.name, param, i, None)
      unify context types param.typ value)
    args
  |> Ply.List.mapSequentially identity
  |> Ply.map combineErrorsUnit


let checkFunctionReturnType
  (types : Types)
  (fn : Fn)
  (result : Dval)
  : Ply<Result<unit, Error>> =
  let context = FunctionCallResult(fn.name, fn.returnType, None)
  unify context types fn.returnType result
