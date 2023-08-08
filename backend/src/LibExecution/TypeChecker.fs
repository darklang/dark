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
  | TypeDoesntExist of TypeName.T * Context

//let something (expected: TypeReference) (actual: ConcreteType)


(*


*)

let rec unifyType
  (context: Context)
  (tst: TypeSymbolTable)
  (expected: TypeReference)
  (actual: ConcreteType)
  : Result<TypeSymbolTable, Error> =
  match (expected, actual) with
  | TVariable name, _ ->
    match Map.get name tst with
    | None -> Ok(Map.add name actual tst)
    | Some t ->
      match Dval.mergeConcreteTypes t actual with
      | Ok merged -> Ok(Map.add name merged tst)
      | Error err -> Error err

  | TList tInner , Some (VTList vtInner) ->
    unifyType context tst tInner vtInner

  | TDict tInner, Some (VTDict vtInner) ->
    unifyType context tst tInner vtInner

  // | TTuple(t1, t2, tRest), Some (VTTuple(v1, v2, vRest)) ->
  //   let ts = t1 :: t2 :: tRest
  //   let vs = v1 :: v2 :: vRest
  //   if List.length ts <> List.length vs then
  //     Error(ValueNotExpectedType(None, expected, context))
  //   else
  //     List.zip ts vs
  //     |> List.fold
  //       (fun acc (t, v) ->
  //         match acc with
  //         | Ok tst -> unifyType context tst t v
  //         | Error _ as err -> err)
  //       (Ok tst)

  | _ -> Dval.mergeConcreteTypes expected actual


(*
  let x = MyRecord { field1 = 1; field2 = Json.serialize<Int> 2 }
*)
let rec unify
  (context : Context)
  (types : Types)
  (tst : TypeSymbolTable)
  (expected : TypeReference)
  (value : Dval)
  : Ply<Result<TypeSymbolTable, Error>> =
  uply {
    let! resolvedType = Types.getTypeReferenceFromAlias types expected

    match (resolvedType, value) with
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
      | None -> return Ok tst
      | Some t -> return! unify context types tst t value
    | TInt, DInt _ -> return Ok tst
    | TFloat, DFloat _ -> return Ok tst
    | TBool, DBool _ -> return Ok tst
    | TUnit, DUnit -> return Ok tst
    | TString, DString _ -> return Ok tst
    | TDateTime, DDateTime _ -> return Ok tst
    | TPassword, DPassword _ -> return Ok tst
    | TUuid, DUuid _ -> return Ok tst
    | TChar, DChar _ -> return Ok tst
    | TBytes, DBytes _ -> return Ok tst
    | TDB _, DDB _ -> return Ok tst // TODO: check DB type
    | TList nested, DList(typ, _dvs) ->
      unify nested type // ...
      // Hmm:
      // let's say `nested` above is a TVar "a"
      // and `typ` is a VTInt
      // the next TVar "a" is required to be VTInt
      // how?

      (*
        let i = [5]
        let s = ["str"]
        let f (a: List<'a>): List<'a> =
          [6]

        f i // should return [6]
        f s // should ERROR
      *)


      return Ok tst

    | TDict valueType, DDict dmap ->
      // let! results =
      //   dmap
      //   |> Map.toList
      //   |> Ply.List.mapSequentially (fun (k, v) ->
      //     let location = Context.toLocation context
      //     let context = DictKey(k, valueType, location)
      //     unify context types typeArgSymbolTable valueType v)
      // return combineErrorsUnit results
      // CLEANUP DDict should include a TypeReference, in which case
      // the type-checking here would just be a `tValue = dValue` check.
      // (the construction of that DDict should have already checked that the
      // keys match)
      return Ok tst

    | TFn(argTypes, returnType), DFnVal fnVal -> return Ok tst // TYPESTODO check lambdas and fnVals
    | TTuple(t1, t2, tRest), DTuple(v1, v2, vRest) ->
      let ts = t1 :: t2 :: tRest
      let vs = v1 :: v2 :: vRest
      if List.length ts <> List.length vs then
        return Error(ValueNotExpectedType(value, resolvedType, context))
      else
        // let! results =
        //   List.zip ts vs
        //   |> Ply.List.mapSequentiallyWithIndex (fun i (t, v) ->
        //     let context = TupleIndex(i, t, context)
        //     unify context types typeArgSymbolTable t v)
        // return combineErrorsUnit results
        // CLEANUP DTuple should include a TypeReference for each part, in which
        // case the type-checking here would just be a comparison of typeRefs.
        // (the construction of that DTuple should have already checked that the
        // types match)
        return Ok tst

    // TYPESCLEANUP: handle typeArgs
    | TCustomType(typeName, _typeArgs), value ->

      match! Types.find typeName types with
      | None -> return Error(TypeDoesntExist(typeName, context))
      | Some ut ->
        let err = Error(ValueNotExpectedType(value, resolvedType, context))

        match ut, value with
        | { definition = TypeDeclaration.Alias aliasType }, _ ->
          let! resolvedAliasType = Types.getTypeReferenceFromAlias types aliasType
          return! unify context types tat resolvedAliasType value

        | { definition = TypeDeclaration.Record _ }, DRecord(tn, _, dmap) ->
          // TYPESCLEANUP: this search should no longer be required
          let! aliasedType =
            Types.getTypeReferenceFromAlias types (TCustomType(tn, []))
          match aliasedType with
          | TCustomType(concreteTn, typeArgs) ->
            if concreteTn <> typeName then
              let! expected =
                Types.getTypeReferenceFromAlias types (TCustomType(typeName, []))
              return Error(ValueNotExpectedType(value, expected, context))
            else
              // return!
              //   unifyRecordFields
              //     concreteTn
              //     context
              //     types
              //     typeArgSymbolTable
              //     (firstField :: additionalFields)
              //     dmap
              // CLEANUP DRecord should include a TypeReference, in which case
              // the type-checking here would just be a `tField = dField` check.
              // (the construction of that DRecord should have already checked
              // that the fields match)
              return Ok tst
          | _ -> return err

        | { definition = TypeDeclaration.Enum cases },
          DEnum(tn, _, caseName, valFields) ->
          // TODO: deal with aliased type?
          if tn <> typeName then
            return Error(ValueNotExpectedType(value, resolvedType, context))
          else
            let matchingCase : Option<TypeDeclaration.EnumCase> =
              cases |> NEList.find (fun c -> c.name = caseName)

            match matchingCase with
            | None -> return err
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
                //     unify context types typeArgSymbolTable expected.typ actual)
                //   |> Ply.List.mapSequentially identity

                // return combineErrorsUnit unified
                // CLEANUP DEnum should include a TypeReference, in which case
                // the type-checking here would just be a `tField = dField` check.
                // (the construction of that DEnum should have already checked
                // that the fields match)
                return Ok tst
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



// and unifyRecordFields
//   (recordType : TypeName.T)
//   (context : Context)
//   (types : Types)
//   (typeArgSymbolTable : TypeSymbolTable)
//   (defs : List<TypeDeclaration.RecordField>)
//   (values : DvalMap)
//   : Ply<Result<TypeSymbolTable, Error>> =
//   let completeDefinition =
//     defs
//     |> List.filterMap (fun (d : TypeDeclaration.RecordField) ->
//       if d.name = "" then None else Some(d.name, d))
//     |> Map.ofList

//   let defNames = completeDefinition |> Map.keys |> Set.ofList
//   let valueNames = values |> Map.keys |> Set.ofList

//   if defNames = valueNames then
//     let location = Context.toLocation context
//     values
//     |> Map.toList
//     |> List.map (fun (fieldName, fieldValue) ->
//       let fieldDef =
//         Map.get fieldName completeDefinition
//         |> Exception.unwrapOptionInternal
//           "field name missing from type"
//           [ "fieldName", fieldName ]
//       let context = RecordField(recordType, fieldDef.name, fieldDef.typ, location)
//       unify context types typeArgSymbolTable fieldDef.typ fieldValue)
//     |> Ply.List.flatten
//     |> Ply.map combineErrorsTat
//   else
//     let extraFields = Set.difference valueNames defNames
//     let missingFields = Set.difference defNames valueNames
//     Error(MismatchedRecordFields(recordType, extraFields, missingFields, context))
//     |> Ply


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


// let x (l: List<'a>) : 'a =
//
//
// x [] // List<None>

let checkFunctionCall
  (types : Types)
  (typeArgSymbolTable : TypeSymbolTable)
  (fn : Fn)
  (args : List<Dval>)
  : Ply<Result<TypeSymbolTable, Error>> =
  // The interpreter checks these are the same length
  fn.parameters
  |> List.mapi2
    (fun i value param ->
      let context = FunctionCallParameter(fn.name, param, i, None)
      unify context types typeArgSymbolTable param.typ value)
    args
  |> Ply.List.mapSequentially identity
  |> Ply.map combineErrorsTat


let checkFunctionReturnType
  (types : Types)
  (typeArgSymbolTable : TypeSymbolTable)
  (fn : Fn)
  (result : Dval)
  : Ply<Result<TypeSymbolTable, Error>> =
  let context = FunctionCallResult(fn.name, fn.returnType, None)
  unify context types typeArgSymbolTable fn.returnType result
