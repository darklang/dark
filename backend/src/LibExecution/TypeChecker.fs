module LibExecution.TypeChecker


open Prelude
open VendoredTablecloth
open RuntimeTypes

/// Returns `Ok ()` if no errors, or `Error first` otherwise
let combineErrorsUnit (l : List<Result<unit, 'err>>) : Result<unit, 'err> =
  List.fold
    (Ok())
    (fun l r ->
      match l, r with
      | _, Ok () -> l
      | Ok (), _ -> r
      | Error _, Error _ -> l)
    l

module Error =
  type Path = List<string>

  type TypeUnificationError = { expectedType : TypeReference; actualValue : Dval }

  type IncorrectNumberOfTypeArgsError = { expected : int; actual : int }

  type MismatchedFields =
    { expectedFields : Set<string>
      actualFields : Set<string> }

  type T =
    /// Failed to find a referenced type
    | TypeLookupFailure of FQTypeName.T * path : Path

    | IncorrectNumberOfTypeArgs of IncorrectNumberOfTypeArgsError * Path

    /// An argument didn't match the expected type
    | TypeUnificationFailure of TypeUnificationError * Path

    | MismatchedRecordFields of MismatchedFields * Path


  let toString (e : T) : string =
    match e with
    | TypeLookupFailure (typeName, path) ->
      let lookupString = FQTypeName.toString typeName
      let path = path |> String.concat "->"
      $"Type {lookupString} could not be found in {path}"

    | TypeUnificationFailure (uf, path) ->
      let expected = DvalReprDeveloper.typeName uf.expectedType
      let actual = DvalReprDeveloper.dvalTypeName uf.actualValue
      let path = path |> String.concat "->"
      $"Expected a value of type `{expected}` but got a `{actual}` in {path}"

    | MismatchedRecordFields (mrf, path) ->
      let expected = mrf.expectedFields
      let actual = mrf.actualFields
      // More or less wholesale from User_db's type checker
      let missingFields = Set.difference expected actual in
      let extraFields = Set.difference actual expected in

      let missingMsg =
        "Expected but did not find: ["
        + (missingFields |> Set.toList |> String.concat ", ")
        + "]"

      let extraMsg =
        "Found but did not expect: ["
        + (extraFields |> Set.toList |> String.concat ", ")
        + "]"

      let path = path |> String.concat "->"
      (match (Set.isEmpty missingFields, Set.isEmpty extraFields) with
       | false, false -> $"{missingMsg} & {extraMsg} in {path}"
       | false, true -> $"{missingMsg} in {path}"
       | true, false -> $"{extraMsg} in {path}"
       | true, true ->
         "Type checker error! Deduced expected fields from type and actual fields in value did not match, but could not find any examples!")

    | IncorrectNumberOfTypeArgs (ita, path) ->
      $"Expected {ita.expected} type arguments but found {ita.actual} in {path}"



open Error

let rec unify
  (path : List<string>)
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (expected : TypeReference)
  (value : Dval)
  : Result<unit, Error.T> =
  let resolvedType = getTypeReferenceFromAlias availableTypes expected
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
  | THttpResponse _, DHttpResponse _ -> Ok()
  | TBytes, DBytes _ -> Ok()
  | TTuple _, DTuple _ -> Ok()

  // TYPESCLEANUP - fold these cases all into TCustomType
  | TOption _, DOption _ -> Ok()
  | TResult _, DResult _ -> Ok()

  // TYPESCLEANUP: handle typeArgs
  | TCustomType (typeName, typeArgs), value ->
    match Map.tryFind typeName availableTypes with
    | None -> Error(TypeLookupFailure(typeName, List.rev path))
    | Some ut ->
      let err =
        Error(
          TypeUnificationFailure(
            { expectedType = expected; actualValue = value },
            List.rev path
          )
        )

      match ut, value with
      | CustomType.Alias aliasType, _ ->
        let resolvedAliasType = getTypeReferenceFromAlias availableTypes aliasType
        unify path availableTypes resolvedAliasType value

      | CustomType.Record (firstField, additionalFields), DRecord (tn, dmap) ->
        let aliasedType =
          getTypeReferenceFromAlias availableTypes (TCustomType(tn, []))
        match aliasedType with
        | TCustomType (concreteTn, typeArgs) ->
          if concreteTn <> typeName then
            Error(
              TypeUnificationFailure(
                { expectedType = expected; actualValue = value },
                List.rev path
              )
            )
          else
            unifyRecordFields
              path
              availableTypes
              (firstField :: additionalFields)
              dmap
        | _ -> err

      | CustomType.Enum (firstCase, additionalCases), DEnum (tn, caseName, valFields) ->
        if tn <> typeName then
          Error(
            TypeUnificationFailure(
              { expectedType = expected; actualValue = value },
              List.rev path
            )
          )
        else
          let matchingCase : Option<CustomType.EnumCase> =
            firstCase :: additionalCases |> List.find (fun c -> c.name = caseName)

          match matchingCase with
          | None -> err
          | Some case ->
            if List.length case.fields = List.length valFields then
              List.zip case.fields valFields
              |> List.map (fun (expected, actual) ->
                unify (case.name :: path) availableTypes expected.typ actual)
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
  | THttpResponse _, _
  | TBytes, _
  | TOption _, _
  | TResult _, _ ->
    Error(
      TypeUnificationFailure(
        { expectedType = expected; actualValue = value },
        List.rev path
      )
    )



and unifyRecordFields
  (path : List<string>)
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (defs : List<CustomType.RecordField>)
  (values : DvalMap)
  : Result<unit, Error.T> =
  let completeDefinition =
    defs
    |> List.filterMap (fun (d : CustomType.RecordField) ->
      if d.name = "" then None else Some(d.name, d.typ))
    |> Map.ofList

  let defNames = completeDefinition |> Map.keys |> Set.ofList
  let valueNames = values |> Map.keys |> Set.ofList

  if defNames = valueNames then
    values
    |> Map.toList
    |> List.map (fun (fieldName, fieldValue) ->
      unify
        (fieldName :: path)
        availableTypes
        (Map.get fieldName completeDefinition
         |> Exception.unwrapOptionInternal
              "field name missing from type"
              [ "fieldName", fieldName ])
        fieldValue)
    |> combineErrorsUnit
  else
    Error(
      MismatchedRecordFields(
        { expectedFields = defNames; actualFields = valueNames },
        List.rev path
      )
    )


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
  (path : List<string>)
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (fn : Fn)
  (typeArgs : List<TypeReference>)
  (args : DvalMap)
  : Result<unit, Error.T> =

  let typeArgErrors =
    let constraints = fn.typeParams

    if List.length constraints <> List.length typeArgs then
      Error(
        IncorrectNumberOfTypeArgs(
          { expected = List.length constraints; actual = List.length typeArgs },
          List.rev path
        )
      )
    else
      Ok()

  let argErrors =
    let args = Map.toList args

    let withParams : List<Param * Dval> =
      List.map
        (fun (argname, argval) ->
          let parameter =
            fn.parameters
            |> List.find (fun (p : Param) -> p.name = argname)
            |> Exception.unwrapOptionInternal
                 "Invalid parameter name"
                 [ "fn", fn.name; "argname", argname ]

          (parameter, argval))
        args

    withParams
    |> List.map (fun (param, value) ->
      unify (param.name :: path) availableTypes param.typ value)

  (typeArgErrors :: argErrors) |> combineErrorsUnit


let checkFunctionReturnType
  (path : List<string>)
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (fn : Fn)
  (result : Dval)
  : Result<unit, Error.T> =

  unify ("result" :: path) availableTypes fn.returnType result
