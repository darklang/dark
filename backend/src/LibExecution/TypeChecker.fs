module LibExecution.TypeChecker


open Prelude
open VendoredTablecloth
open RuntimeTypes

/// Returns `Ok ()` if no errors, or `Error first` otherwise
let combineErrorsUnit (l : List<Result<unit, 'err>>) : Result<unit, 'err> =
  l |> Tablecloth.Result.values |> Result.map ignore<List<unit>>

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
  | EnumField of
    enumTypeName : TypeName.T *
    definition : TypeDeclaration.EnumField *
    caseName : string *
    paramIndex : int *  // nth argument to the enum constructor
    location : Location
  | DBQueryVariable of varName : string * location : Location
  | DBSchemaType of
    name : string *
    expectedType : TypeReference *
    location : Location


module Context =
  let toLocation (c : Context) : Location =
    match c with
    | FunctionCallParameter(_, _, _, location) -> location
    | FunctionCallResult(_, _, location) -> location
    | RecordField(_, _, location) -> location
    | EnumField(_, _, _, _, location) -> location
    | DBQueryVariable(_, location) -> location
    | DBSchemaType(_, _, location) -> location


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
  : Result<unit, Error> =
  let resolvedType = getTypeReferenceFromAlias types expected
  match (resolvedType, value) with
  // Any should be removed, but we currently allow it as a param type
  // in user functions, so we should allow it here.
  //
  // Potentially needs to be removed before we use this type checker for DBs?
  //   - Could always have a type checking context that allows/disallows any
  | TVariable _, _ -> Ok() // TYPESCLEANUP actually unify this
  | TInt, DInt _ -> Ok()
  | TFloat, DFloat _ -> Ok()
  | TBool, DBool _ -> Ok()
  | TUnit, DUnit -> Ok()
  | TString, DString _ -> Ok()
  // TYPESCLEANUP unify nested types too
  | TList _, DList _ -> Ok()
  | TDateTime, DDateTime _ -> Ok()
  | TDict _, DDict _ -> Ok()
  | TFn _, DFnVal _ -> Ok()
  | TPassword, DPassword _ -> Ok()
  | TUuid, DUuid _ -> Ok()
  | TChar, DChar _ -> Ok()
  | TDB _, DDB _ -> Ok()
  | TBytes, DBytes _ -> Ok()
  | TTuple _, DTuple _ -> Ok()

  // TYPESCLEANUP - fold these cases all into TCustomType
  | TOption _, DOption _ -> Ok()

  // TYPESCLEANUP: handle typeArgs
  | TCustomType(typeName, typeArgs), value ->

    match Types.find typeName types with
    | None -> Error(TypeDoesntExist(typeName, context))
    | Some ut ->
      let err = Error(ValueNotExpectedType(value, resolvedType, context))

      match ut, value with
      | TypeDeclaration.Alias aliasType, _ ->
        let resolvedAliasType = getTypeReferenceFromAlias types aliasType
        unify context types resolvedAliasType value

      | TypeDeclaration.Record(firstField, additionalFields), DRecord(tn, dmap) ->
        let aliasedType = getTypeReferenceFromAlias types (TCustomType(tn, []))
        match aliasedType with
        | TCustomType(concreteTn, typeArgs) ->
          if concreteTn <> typeName then
            Error(ValueNotExpectedType(value, aliasedType, context))
          else
            unifyRecordFields
              concreteTn
              context
              types
              (firstField :: additionalFields)
              dmap
        | _ -> err

      | TypeDeclaration.Enum(firstCase, additionalCases),
        DEnum(tn, caseName, valFields) ->
        // TODO: deal with aliased type?
        if tn <> typeName then
          Error(ValueNotExpectedType(value, resolvedType, context))
        else
          let matchingCase : Option<TypeDeclaration.EnumCase> =
            firstCase :: additionalCases |> List.find (fun c -> c.name = caseName)

          match matchingCase with
          | None -> err
          | Some case ->
            if List.length case.fields = List.length valFields then
              List.zip case.fields valFields
              |> List.mapi (fun i (expected, actual) ->
                let context =
                  EnumField(tn, expected, case.name, i, Context.toLocation context)
                unify context types expected.typ actual)
              |> combineErrorsUnit
            else
              err
      | _, _ -> err

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
  | TBytes, _
  | TOption _, _ -> Error(ValueNotExpectedType(value, resolvedType, context))



and unifyRecordFields
  (recordType : TypeName.T)
  (context : Context)
  (types : Types)
  (defs : List<TypeDeclaration.RecordField>)
  (values : DvalMap)
  : Result<unit, Error> =
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
    |> combineErrorsUnit
  else
    let extraFields = Set.difference valueNames defNames
    let missingFields = Set.difference defNames valueNames
    Error(MismatchedRecordFields(recordType, extraFields, missingFields, context))


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
  : Result<unit, Error> =
  fn.parameters
  |> List.mapi2
    (fun i value param ->
      let context = FunctionCallParameter(fn.name, param, i, None)
      unify context types param.typ value)
    args
  |> combineErrorsUnit


let checkFunctionReturnType
  (types : Types)
  (fn : Fn)
  (result : Dval)
  : Result<unit, Error> =
  let context = FunctionCallResult(fn.name, fn.returnType, None)
  unify context types fn.returnType result
