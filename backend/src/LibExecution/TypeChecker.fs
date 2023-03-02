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
  type UnificationError = { expectedType : DType; actualValue : Dval }

  type MismatchedFields =
    { expectedFields : Set<string>
      actualFields : Set<string> }

  type T =
    | TypeLookupFailure of string * int
    | TypeUnificationFailure of UnificationError
    | MismatchedRecordFields of MismatchedFields

    override this.ToString() : string =
      match this with
      | TypeLookupFailure (lookupName, lookupVersion) ->
        let lookupString = $"({lookupName}, v{lookupVersion})"
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


  let listToString ts = ts |> List.map string |> String.concat ", "

open Error

let rec unify
  (userTypes : Map<string * int, UserType.T>)
  (expected : DType)
  (value : Dval)
  : Result<unit, List<Error.T>> =
  match (expected, value) with
  // Any should be removed, but we currently allow it as a param tipe
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
  // TODO: support Tuple type-checking.
  // See https://github.com/darklang/dark/issues/4239#issuecomment-1175182695
  | TDateTime, DDate _ -> Ok()
  | TDict _, DObj _ -> Ok()
  | TRecord _, DObj _ -> Ok()
  | TFn _, DFnVal _ -> Ok()
  | TPassword, DPassword _ -> Ok()
  | TUuid, DUuid _ -> Ok()
  | TOption _, DOption _ -> Ok()
  | TResult _, DResult _ -> Ok()
  | TChar, DChar _ -> Ok()
  | TDB _, DDB _ -> Ok()
  | THttpResponse _, DHttpResponse _ -> Ok()
  | TBytes, DBytes _ -> Ok()
  | TUserType (expectedName, expectedVersion), DObj dmap ->
    (match Map.tryFind (expectedName, expectedVersion) userTypes with
     | None -> Error [ TypeLookupFailure(expectedName, expectedVersion) ]
     | Some ut ->
       (match ut.definition with
        | UserType.UTRecord utd -> unifyUserRecordWithDvalMap userTypes utd dmap))
  | expectedType, actualValue ->
    Error [ TypeUnificationFailure
              { expectedType = expectedType; actualValue = actualValue } ]


and unifyUserRecordWithDvalMap
  (userTypes : Map<string * int, UserType.T>)
  (definition : List<UserType.RecordField>)
  (value : DvalMap)
  : Result<unit, List<Error.T>> =
  let completeDefinition =
    definition
    |> List.filterMap (fun (d : UserType.RecordField) ->
      if d.name = "" then None else Some(d.name, d.typ))
    |> Map.ofList

  let definitionNames = completeDefinition |> Map.keys |> Set.ofList
  let objNames = value |> Map.keys |> Set.ofList
  let sameNames = definitionNames = objNames in

  if sameNames then
    value
    |> Map.toList
    |> List.map (fun (key, data) ->
      unify
        userTypes
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


let checkFunctionCall
  (userTypes : Map<string * int, UserType.T>)
  (fn : Fn)
  (args : DvalMap)
  : Result<unit, List<Error.T>> =
  let args = Map.toList args in

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
  |> List.map (fun (param, value) -> unify userTypes param.typ value)
  |> combineErrorsUnit
  |> Result.mapError List.concat


let checkFunctionReturnType
  (userTypes : Map<string * int, UserType.T>)
  (fn : Fn)
  (result : Dval)
  : Result<unit, Error.T list> =
  unify userTypes fn.returnType result
