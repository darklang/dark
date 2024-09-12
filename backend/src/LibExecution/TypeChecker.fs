/// The run-time type-checker
/// Called by the interpreter, and a few other places
module LibExecution.TypeChecker

open Prelude
open RuntimeTypes
module VT = ValueType
module RTE = RuntimeError


// /// Returns `Ok ()` if no errors, or `Error first` otherwise
// let combineErrorsUnit (l : NEList<Result<unit, 'err>>) : Result<unit, 'err> =
//   l |> NEList.find Result.isError |> Option.unwrap (Ok())




// let rec getTypeReferenceFromAlias
//   (_types : Types)
//   (typ : TypeReference)
//   : Ply<Result<TypeReference, RTE.Error>> =
//   match typ with
//   // | TCustomType(Ok outerTypeName, outerTypeArgs) ->
//   //   uply {
//   //     match! Types.find outerTypeName types with
//   //     | Some { definition = TypeDeclaration.Alias typ; typeParams = typeParams } ->
//   //       let typ = Types.substitute typeParams outerTypeArgs typ
//   //       return! getTypeReferenceFromAlias types typ
//   //     | _ -> return Ok typ
//   //   }

//   // | TCustomType(Error err, _) -> Ply(Error err)

//   | _ -> Ply(Ok typ)



// let rec valueTypeUnifies
//   (tst : TypeSymbolTable)
//   (expected : TypeReference)
//   (actual : ValueType)
//   : Ply<bool> =
//   let r = valueTypeUnifies tst

//   let rMult (expected : List<TypeReference>) (actual : List<ValueType>) : Ply<bool> =
//     if List.length expected <> List.length actual then
//       Ply false
//     else
//       List.zip expected actual
//       |> Ply.List.foldSequentially
//         (fun acc (e, a) ->
//           match acc with
//           | false -> Ply acc
//           | true -> r e a)
//         true

//   uply {
//     match expected, actual with
//     | _, ValueType.Unknown -> return true

//     | TUnit, ValueType.Known KTUnit -> return true
//     | TBool, ValueType.Known KTBool -> return true

//     | TInt8, ValueType.Known KTInt8 -> return true
//     | TUInt8, ValueType.Known KTUInt8 -> return true
//     | TInt16, ValueType.Known KTInt16 -> return true
//     | TUInt16, ValueType.Known KTUInt16 -> return true
//     | TInt32, ValueType.Known KTInt32 -> return true
//     | TUInt32, ValueType.Known KTUInt32 -> return true
//     | TInt64, ValueType.Known KTInt64 -> return true
//     | TUInt64, ValueType.Known KTUInt64 -> return true
//     | TInt128, ValueType.Known KTInt128 -> return true
//     | TUInt128, ValueType.Known KTUInt128 -> return true

//     | TFloat, ValueType.Known KTFloat -> return true

//     | TChar, ValueType.Known KTChar -> return true
//     | TString, ValueType.Known KTString -> return true

//     // | TUuid, ValueType.Known KTUuid -> return true
//     // | TDateTime, ValueType.Known KTDateTime -> return true

//     | TList innerT, ValueType.Known(KTList innerV) -> return! r innerT innerV

//     | TDict innerT, ValueType.Known(KTDict innerV) -> return! r innerT innerV

//     | TTuple(tFirst, tSecond, tRest),
//       ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
//       let expected = tFirst :: tSecond :: tRest
//       let actual = vFirst :: vSecond :: vRest
//       return! rMult expected actual

//     // | TCustomType(Error err, _), _ ->
//     //   return
//     //     Exception.raiseInternal
//     //       $"Unexpected - can't unify valueType against unknown/error type reference"
//     //       [ "err", err ]
//     // | TCustomType(Ok _typeNameT, _typeArgsT),
//     //   ValueType.Known(KTCustomType(_typeNameV, _typeArgsV)) ->
//     //   // TODO: follow up here when:
//     //   // - type name aliases are and resolved
//     //   // - type args are properly passed around and handled

//     //   // TODO: assert type names are the same,
//     //   // after we've handled all type aliases
//     //   //return! rMult typeArgsT typeArgsV
//     //   return true

//     // | TFn(_argTypes, _returnType), ValueType.Known(KTFn(_vArgs, _vRet)) ->
//     //   // TODO: follow up here when type args are properly passed around and handled

//     //   // let expected = returnType :: (NEList.toList argTypes)
//     //   // let actual = vRet :: (NEList.toList vArgs)
//     //   // return! rMult expected actual
//     //   return true

//     //| TDB innerT, ValueType.Known(KTDB innerV) -> return! r innerT innerV

//     | TVariable name, _ ->
//       match Map.get name tst with
//       | None -> return true
//       | Some t -> return! r t actual

//     | _, _ -> return false
//   }

// let rec unify
//   (context : RTE.TypeChecker.Context)
//   (types : Types)
//   (tst : TypeSymbolTable)
//   (expected : TypeReference)
//   (value : Dval)
//   : Ply<Result<unit, RTE.Error>> =
//   uply {
//     match! getTypeReferenceFromAlias types expected with
//     | Error rte -> return Error rte
//     | Ok expected ->
//       match (expected, value) with
//       // // Any should be removed, but we currently allow it as a param type
//       // // in user functions, so we should allow it here.
//       // //
//       // // Potentially needs to be removed before we use this type checker for DBs?
//       // //   - Could always have a type checking context that allows/disallows any
//       | TVariable name, _ ->
//         match Map.get name tst with
//         // for now, allow undefined type variables. In the future, we would create a
//         // type from the value and return any variables defined this way for usage in
//         // further arguments and return values.
//         | None -> return Ok()
//         | Some t -> return! unify context types tst t value

//       | TBool, DBool _ -> return Ok()
//       | TUnit, DUnit -> return Ok()

//       | TInt8, DInt8 _ -> return Ok()
//       | TUInt8, DUInt8 _ -> return Ok()
//       | TInt16, DInt16 _ -> return Ok()
//       | TUInt16, DUInt16 _ -> return Ok()
//       | TInt32, DInt32 _ -> return Ok()
//       | TUInt32, DUInt32 _ -> return Ok()
//       | TInt64, DInt64 _ -> return Ok()
//       | TUInt64, DUInt64 _ -> return Ok()
//       | TInt128, DInt128 _ -> return Ok()
//       | TUInt128, DUInt128 _ -> return Ok()

//       | TFloat, DFloat _ -> return Ok()

//       | TChar, DChar _ -> return Ok()
//       | TString, DString _ -> return Ok()

//       | TDateTime, DDateTime _ -> return Ok()
//       | TUuid, DUuid _ -> return Ok()

//       | TList expected, DList(actual, _dvs) ->
//         match! valueTypeUnifies tst expected actual with
//         | false ->
//           return
//             RTE.ValueNotExpectedType(value, TList expected, context)
//             |> Error

//         | true -> return! Ply()

//       | TDict _expected, DDict(_actual, _entries) ->
//         // VTTODO uncomment this
//         // match! valueTypeUnifies tst expected actual with
//         // | false ->
//         //   return
//         //     ValueNotExpectedType(value, expected, context)
//         //     |> Error.toRuntimeError
//         //     |> Error

//         // | true -> return! Ply()
//         return Ok()

//       | TFn(_argTypes, _returnType), DFnVal _fnVal -> return Ok() // TYPESTODO check lambdas and fnVals
//       // | TTuple(t1, t2, tRest), DTuple(v1, v2, vRest) ->
//       //   let ts = t1 :: t2 :: tRest
//       //   let vs = v1 :: v2 :: vRest
//       //   if List.length ts <> List.length vs then
//       //     return
//       //       { errorType = ValueNotExpectedType(value, expected); context = context }
//       //       |> Error.toRuntimeError
//       //       |> Error
//       //   else
//       //     // let! results =
//       //     //   List.zip ts vs
//       //     //   |> Ply.List.mapSequentiallyWithIndex (fun i (t, v) ->
//       //     //     let context = TupleIndex(i, t, context)
//       //     //     unify context types tst t v)
//       //     // return combineErrorsUnit results
//       //     // CLEANUP DTuple should include a TypeReference for each part, in which
//       //     // case the type-checking here would just be a comparison of typeRefs.
//       //     // (the construction of that DTuple should have already checked that the
//       //     // types match)
//       //     return Ok()

//       // TYPESCLEANUP: handle typeArgs
//       | TCustomType(typeName, _typeArgs), value ->

//         match typeName with
//         | Error rte -> return Error rte
//         | Ok typeName ->
//           match! Types.find typeName types with
//           | None ->
//             return
//               RTE.TypeDoesntExist typeName
//               |> Error
//           | Some ut ->
//             let err =
//               RTE.ValueNotExpectedType(value, expected, context)
//               |> Error

//             match ut, value with
//             | { definition = TypeDeclaration.Alias aliasType }, _ ->
//               let! resolvedAliasType = getTypeReferenceFromAlias types aliasType

//               match resolvedAliasType with
//               | Error rte -> return Error rte
//               | Ok resolvedAliasType ->
//                 return! unify context types tst resolvedAliasType value

//             | { definition = TypeDeclaration.Record _ },
//               DRecord(tn, _, _valueTypesTODO, _fields) ->
//               // TYPESCLEANUP: this search should no longer be required
//               let! aliasedType =
//                 getTypeReferenceFromAlias types (TCustomType(Ok tn, []))
//               match aliasedType with
//               | Ok(TCustomType(Error rte, _)) -> return Error rte
//               | Ok(TCustomType(Ok concreteTn, _typeArgs)) ->
//                 if concreteTn <> typeName then
//                   return
//                     RTE.ValueNotExpectedType(value, expected, context)
//                     |> Error
//                 else
//                   // CLEANUP DRecord should include a TypeReference, in which case
//                   // the type-checking here would just be a `tField = dField` check.
//                   // (the construction of that DRecord should have already checked
//                   // that the fields match)
//                   return Ok()
//               | _ -> return err

//             // | { definition = TypeDeclaration.Enum cases },
//             //   DEnum(tn, _, _typeArgsDEnumTODO, caseName, valFields) ->
//             //   // TODO: deal with aliased type?
//             //   if tn <> typeName then
//             //     return
//             //       { errorType = ValueNotExpectedType(value, expected)
//             //         context = context }
//             //       |> Error.toRuntimeError
//             //       |> Error
//             //   else
//             //     let matchingCase : Option<TypeDeclaration.EnumCase> =
//             //       cases |> NEList.find (fun c -> c.name = caseName)

//             //     match matchingCase with
//             //     | None -> return err
//             //     | Some case ->
//             //       if List.length case.fields = List.length valFields then
//             //         // let! unified =
//             //         //   List.zip case.fields valFields
//             //         //   |> List.mapi (fun i (expected, actual) ->
//             //         //     let context =
//             //         //       EnumField(
//             //         //         tn,
//             //         //         expected,
//             //         //         case.name,
//             //         //         i,
//             //         //         Context.toLocation context
//             //         //       )
//             //         //     unify context types tst expected.typ actual)
//             //         //   |> Ply.List.mapSequentially identity

//             //         // return combineErrorsUnit unified
//             //         // CLEANUP DEnum should include a TypeReference, in which case
//             //         // the type-checking here would just be a `tField = dField` check.
//             //         // (the construction of that DEnum should have already checked
//             //         // that the fields match)
//             //         return Ok()
//             //       else
//             //         return err
//             | _, _ -> return err

//       // | TDB _, DDB _ -> return Ok() // TODO: check DB type

//       // See https://github.com/darklang/dark/issues/4239#issuecomment-1175182695
//       // TODO: exhaustiveness check
//       | TUnit, _
//       | TBool, _

//       | TInt8, _
//       | TUInt8, _
//       | TInt16, _
//       | TUInt16, _
//       | TInt32, _
//       | TUInt32, _
//       | TInt64, _
//       | TUInt64, _
//       | TInt128, _
//       | TUInt128, _

//       | TFloat, _

//       | TChar, _
//       | TString, _

//       | TDateTime, _
//       | TUuid, _

//       | TList _, _
//       | TDict _, _
//       | TTuple _, _

//       // | TCustomType _, _

//       | TVariable _, _

//       | TFn _, _
//       // | TDB _, _
//        ->
//         return
//           RTE.ValueNotExpectedType(value, expected, context)
//           |> Error
//   }



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

// let checkFunctionCall
//   (types : Types)
//   (tst : TypeSymbolTable)
//   (fn : Fn)
//   (args : NEList<Dval>)
//   : Ply<Result<unit, RuntimeError>> =
//   // The interpreter checks these are the same length
//   fn.parameters
//   |> NEList.map2WithIndex
//     (fun i value param ->
//       let context = FunctionCallParameter(fn.name, param, i)
//       unify context types tst param.typ value)
//     args
//   |> Ply.NEList.mapSequentially identity
//   |> Ply.map combineErrorsUnit


// let checkFunctionReturnType
//   (types : Types)
//   (tst : TypeSymbolTable)
//   (fn : Fn)
//   (result : Dval)
//   : Ply<Result<unit, RTE.Error>> =
//   let context = FunctionCallResult(fn.name, fn.returnType)
//   unify context types tst fn.returnType result


/// Helpers for creating type-checked Dvals
/// (lists, records, enums, etc.)
///
/// Dvals should be created carefully:
/// - to have the correct `ValueType`s, where appropriate
///  i.e. we should not have `DList(Known KTInt64, [ DString("hi") ])`
///
/// - similarly, we should fail when trying to merge `Dval`s with conflicting `ValueType`s
///   i.e. `List.append [1] ["hi"]` should fail
///   because we can't merge `Known KTInt64` and `Known KTString`
///
/// These functions are intended to help with both of these, in cases where
/// the functions in Dval.fs are insufficient (i.e. we don't know the Dark sub-types
/// of a Dval in some F# code).
///
/// TODO: ideally we don't require a callStack to be input here -- too much data-passing
/// (Ideally, upon error, we'd "fill in" the callstack in the Interpreter somewhere?)
module DvalCreator =

  let list (threadID : ThreadID) (typ : ValueType) (items : List<Dval>) : Dval =
    let (typ, items) =
      items
      |> List.fold
        (fun (typ, list) dv ->
          let dvalType = Dval.toValueType dv

          match VT.merge typ dvalType with
          | Ok newType -> newType, dv :: list
          | Error() ->
            RTE.Lists.Error.TriedToAddMismatchedData(typ, dvalType, dv)
            |> RTE.Error.List
            |> raiseRTE threadID)
        (typ, [])

    DList(typ, List.rev items)


  let dict
    (threadID : ThreadID)
    (typ : ValueType)
    (entries : List<string * Dval>)
    : Dval =
    let (typ, entries) =
      List.fold
        (fun (typ, entries) (k, v) ->
          if Map.containsKey k entries then
            // should we warn here instead? CLEANUP
            RTE.Dicts.Error.TriedToAddKeyAfterAlreadyPresent k
            |> RTE.Error.Dict
            |> raiseRTE threadID

          let vt = Dval.toValueType v
          match VT.merge typ vt with
          | Ok merged -> (merged, Map.add k v entries)
          | Error() ->
            RTE.Dicts.Error.TriedToAddMismatchedData(typ, vt, v)
            |> RTE.Error.Dict
            |> raiseRTE threadID)

        (typ, Map.empty)
        entries

    DDict(typ, entries)




  let optionNone (innerType : ValueType) : Dval =
    DEnum(Dval.optionType, Dval.optionType, [ innerType ], "None", [])

  let optionSome
    (threadID : ThreadID)
    (expectedType : ValueType)
    (dv : Dval)
    : Dval =
    let typeName = Dval.optionType

    let vt = Dval.toValueType dv

    match VT.merge expectedType vt with
    | Ok typ -> DEnum(typeName, typeName, [ typ ], "Some", [ dv ])
    | Error() ->
      // TODO this should be a more general Enum RTE
      // (and make sure you include the Option wrapper type -- this loses that)
      RuntimeError.CannotMergeValues(expectedType, vt) |> raiseRTE threadID

  let option
    (threadID : ThreadID)
    (expectedType : ValueType)
    (dv : Option<Dval>)
    : Dval =
    match dv with
    | Some dv -> optionSome threadID expectedType dv
    | None -> optionNone expectedType


  // module Result =
  //   let typeName = Dval.resultType

  //   let ok
  //     (threadID: ThreadID)
  //     (okType : ValueType)
  //     (errorType : ValueType)
  //     (dvOk : Dval)
  //     : Dval =
  //     let dvalType = Dval.toValueType dvOk
  //     match VT.merge okType dvalType with
  //     | Ok typ ->
  //       DEnum(typeName, typeName, [ typ; errorType ], "Ok", [ dvOk ])
  //     | Error() ->
  //       // RuntimeError.oldError
  //       //   $"Could not merge types {ValueType.toString (VT.customType typeName [ okType; errorType ])} and {ValueType.toString (VT.customType typeName [ dvalType; errorType ])}"
  //       |> raiseRTE callStack

  //   let error
  //     (threadID: ThreadID)
  //     (okType : ValueType)
  //     (errorType : ValueType)
  //     (dvError : Dval)
  //     : Dval =
  //     let dvalType = Dval.toValueType dvError
  //     match VT.merge errorType dvalType with
  //     | Ok typ -> DEnum(typeName, typeName, [ okType; typ ], "Error", [ dvError ])
  //     | Error() ->
  //       RuntimeError.oldError
  //         $"Could not merge types {ValueType.toString (VT.customType Dval.resultType [ okType; errorType ])} and {ValueType.toString (VT.customType Dval.resultType [ okType; dvalType ])}"
  //       |> raiseRTE callStack

  //   let result
  //     (threadID: ThreadID)
  //     (okType : ValueType)
  //     (errorType : ValueType)
  //     (dv : Result<Dval, Dval>)
  //     : Dval =
  //     match dv with
  //     | Ok dv -> ok callStack okType errorType dv
  //     | Error dv -> error callStack okType errorType dv


  /// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
  ///
  /// note: if provided, the typeArgs must match the # of typeArgs expected by the type
  ///
  /// TODO this probably needs to both _take in_ and _return_ the typeSymbolTable
  /// (just pass it in as a ref -- but if this is happening concurrently with something else, ...)
  let record
    (threadID : ThreadID)
    (_types : Types) // is this Types thing what we want, or should we split tst and types?
    (typeName : FQTypeName.FQTypeName)
    (_typeArgs : List<TypeReference>)
    (fields : List<string * Dval>)
    : Ply<Dval> =

    // hmm we need to know what fields the type expects, so we can raise the right errors
    // should that happen here, or in the interpreter?
    // Besides the interpreter, the only usage (so far) is Json.fs

    let resolvedTypeName = typeName // TODO: alias lookup, etc.

    let fields =
      List.fold
        (fun fields (k, v) ->
          match fields, k, v with
          // skip empty rows
          | _, "", _ ->
            RTE.Records.CreationEmptyKey |> RTE.Record |> raiseRTE threadID

          // error if the key appears twice
          | fields, k, _v when Map.containsKey k fields ->
            RTE.Records.CreationDuplicateField k |> RTE.Record |> raiseRTE threadID

          // otherwise add it
          | fields, k, v ->
            // TODO CreationMissingField
            // TODO CreationFieldOfWrongType
            Map.add k v fields)
        Map.empty
        fields

    // TODO:
    // - pass in a (types: Types) arg
    // - use it to determine type args of resultant Dval
    // - ensure fields match the expected shape (defined by type args and field defs)
    //   - this process should also effect the type args of the resultant Dval
    DRecord(resolvedTypeName, typeName, VT.typeArgsTODO, fields) |> Ply


//   let enum
//     (resolvedTypeName : FQTypeName.FQTypeName) // todo: remove
//     (sourceTypeName : FQTypeName.FQTypeName)
//     (caseName : string)
//     (fields : List<Dval>)
//     : Ply<Dval> =
//     // TODO:
//     // - use passed-in Types to determine type args of resultant Dval
//     // - ensure fields match the expected shape (defined by type args and field defs)
//     //   - this process should also effect the type args of the resultant Dval

//     DEnum(resolvedTypeName, sourceTypeName, VT.typeArgsTODO, caseName, fields)
//     |> Ply
