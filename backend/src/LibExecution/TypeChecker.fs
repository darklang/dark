module LibExecution.TypeChecker


open Prelude
open VendoredTablecloth
open RuntimeTypes

/// Returns `Ok ()` if no errors, or `Error list` otherwise
let combineErrorsUnit (l : List<Result<'ok, 'err>>) : Result<unit, List<'err>> =
  List.fold
    (Ok())
    (fun l r ->
      match l, r with
      | _, Ok _ -> l
      | Ok (), Error e -> Error [ e ]
      | Error l, Error e -> Error(e :: l))
    l

module Error =
  type TypeUnificationError = { expectedType : DType; actualValue : Dval }

  type IncorrectNumberOfTypeArgsError = { expected : int; actual : int }

  type MismatchedFields =
    { expectedFields : Set<string>
      actualFields : Set<string> }

  type T =
    /// Failed to find a referenced type
    | TypeLookupFailure of FQTypeName.T

    | IncorrectNumberOfTypeArgs of IncorrectNumberOfTypeArgsError

    /// An argument didn't match the expected type
    | TypeUnificationFailure of TypeUnificationError

    | MismatchedRecordFields of MismatchedFields


    override this.ToString() : string =
      match this with
      | TypeLookupFailure typeName ->
        let lookupString =
          match typeName with
          | FQTypeName.User t -> $"({t.typ}, v{t.version})"
          | FQTypeName.Stdlib t -> $"({t.typ})"
        $"Type {lookupString} could not be found on the canvas"

      | TypeUnificationFailure uf ->
        let expected = DvalReprDeveloper.typeName uf.expectedType
        let actual = DvalReprDeveloper.dvalTypeName uf.actualValue
        $"Expected to see a value of type {expected} but found a {actual}"

      | MismatchedRecordFields mrf ->
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

        (match (Set.isEmpty missingFields, Set.isEmpty extraFields) with
         | false, false -> $"{missingMsg} & {extraMsg}"
         | false, true -> missingMsg
         | true, false -> extraMsg
         | true, true ->
           "Type checker error! Deduced expected fields from type and actual fields in value did not match, but could not find any examples!")

      | IncorrectNumberOfTypeArgs ita ->
        $"Expected {ita.expected} type arguments but found {ita.actual}"


  let listToString ts = ts |> List.map string |> String.concat ", "

open Error

let rec unify
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (expected : DType)
  (value : Dval)
  : Result<unit, List<Error.T>> =
  match (expected, value) with
  // Any should be removed, but we currently allow it as a param type
  // in user functions, so we should allow it here.
  //
  // Potentially needs to be removed before we use this type checker for DBs?
  //   - Could always have a type checking context that allows/disallows any
  | TVariable _, _ -> Ok()
  | TInt, DInt _ -> Ok()
  | TFloat, DFloat _ -> Ok()
  | TBool, DBool _ -> Ok()
  | TUnit, DUnit -> Ok()
  | TStr, DStr _ -> Ok()
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

  // TODO: fold these cases all into TCustomType,
  // and type-check the generic type args.
  | TOption _, DOption _ -> Ok()
  | TResult _, DResult _ -> Ok()
  | TRecord _, DDict _ -> Ok()

  | TCustomType (typeName, typeArgs), value ->
    match Map.tryFind typeName availableTypes with
    | None -> Error [ TypeLookupFailure typeName ]
    | Some ut ->
      let err =
        Error [ TypeUnificationFailure
                  { expectedType = expected; actualValue = value } ]

      match ut, value with
      | CustomType.Record (firstField, additionalFields), DDict dmap ->
        unifyUserRecordWithDvalMap
          availableTypes
          (firstField :: additionalFields)
          dmap
      | CustomType.Enum (firstCase, additionalCases),
        DConstructor (typeName, caseName, valFields) ->
        let matchingCase : Option<CustomType.EnumCase> =
          firstCase :: additionalCases |> List.find (fun c -> c.name = caseName)

        match matchingCase with
        | None -> err
        | Some case ->
          if List.length case.fields = List.length valFields then
            List.zip case.fields valFields
            |> List.map (fun (expected, actual) ->
              unify availableTypes expected.typ actual)
            |> combineErrorsUnit
            |> Result.mapError List.concat
          else
            err
      | _, _ -> err

  // TODO: support Tuple type-checking.
  // See https://github.com/darklang/dark/issues/4239#issuecomment-1175182695
  | expectedType, actualValue ->
    Error [ TypeUnificationFailure
              { expectedType = expectedType; actualValue = actualValue } ]


and unifyUserRecordWithDvalMap
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (definition : List<CustomType.RecordField>)
  (value : DvalMap)
  : Result<unit, List<Error.T>> =
  let completeDefinition =
    definition
    |> List.filterMap (fun (d : CustomType.RecordField) ->
      if d.name = "" then None else Some(d.name, d.typ))
    |> Map.ofList

  let definitionNames = completeDefinition |> Map.keys |> Set.ofList
  let objNames = value |> Map.keys |> Set.ofList

  if definitionNames = objNames then
    value
    |> Map.toList
    |> List.map (fun (key, data) ->
      unify
        availableTypes
        (Map.get key completeDefinition
         |> Exception.unwrapOptionInternal
              "field name missing from type"
              [ "fieldName", key ])
        data)
    |> combineErrorsUnit
    |> Result.mapError List.concat
  else
    Error [ MismatchedRecordFields
              { expectedFields = definitionNames; actualFields = objNames } ]


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
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (fn : Fn)
  (typeArgs : List<DType>)
  (args : DvalMap)
  : Result<unit, List<Error.T>> =

  let typeArgErrors =
    let constraints = fn.typeParams

    if List.length constraints <> List.length typeArgs then
      Error [ IncorrectNumberOfTypeArgs
                { expected = List.length constraints; actual = List.length typeArgs } ]
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
    |> List.map (fun (param, value) -> unify availableTypes param.typ value)
    |> combineErrorsUnit
    |> Result.mapError List.concat

  [ typeArgErrors; argErrors ] |> combineErrorsUnit |> Result.mapError List.concat


let checkFunctionReturnType
  (availableTypes : Map<FQTypeName.T, CustomType.T>)
  (fn : Fn)
  (result : Dval)
  : Result<unit, Error.T list> =

  unify availableTypes fn.returnType result
