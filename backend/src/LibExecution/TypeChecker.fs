/// The run-time type-checker
/// Called by the interpreter, and a few other places
module LibExecution.TypeChecker

open Prelude
open RuntimeTypes
module VT = ValueType
module RTE = RuntimeError

let rec unwrapAlias
  (types : Types)
  (typ : TypeReference)
  : Ply<Result<TypeReference, RTE.Error>> =
  match typ with
  | TCustomType(Ok outerTypeName, outerTypeArgs) ->
    uply {
      match! Types.find types outerTypeName with
      | Some { definition = TypeDeclaration.Alias typ; typeParams = typeParams } ->
        let typ = Types.substitute typeParams outerTypeArgs typ
        return! unwrapAlias types typ
      | _ -> return Ok typ
    }

  //| TCustomType(Error err, _) -> Ply(Error err)

  | _ -> Ply(Ok typ)


type TypeCheckPathPart = RuntimeError.TypeChecking.TypeCheckPathPart
type ReverseTypeCheckPath = RuntimeError.TypeChecking.ReverseTypeCheckPath

let rec unifyValueType
  (types : Types)
  (tst : TypeSymbolTable)
  (pathSoFar : ReverseTypeCheckPath)
  (expected : TypeReference) // CLEANUP: maybe ValueType instead? or maybe ValueType + TypeReference, and the TR is only used for... reference?
  (actual : ValueType)
  : Ply<Result<TypeSymbolTable, ReverseTypeCheckPath>> =
  let r = unifyValueType types

  uply {
    match expected, actual with

    | TVariable name, _ ->
      match Map.get name tst with
      | None -> return Ok(Map.add name actual tst)
      | Some t ->
        match ValueType.merge t actual with
        | Ok merged -> return Ok(Map.add name merged tst)
        | Error() -> return Error pathSoFar

    | _, ValueType.Unknown -> return Ok tst


    | TUnit, ValueType.Known KTUnit -> return Ok tst
    | TBool, ValueType.Known KTBool -> return Ok tst

    | TInt8, ValueType.Known KTInt8 -> return Ok tst
    | TUInt8, ValueType.Known KTUInt8 -> return Ok tst
    | TInt16, ValueType.Known KTInt16 -> return Ok tst
    | TUInt16, ValueType.Known KTUInt16 -> return Ok tst
    | TInt32, ValueType.Known KTInt32 -> return Ok tst
    | TUInt32, ValueType.Known KTUInt32 -> return Ok tst
    | TInt64, ValueType.Known KTInt64 -> return Ok tst
    | TUInt64, ValueType.Known KTUInt64 -> return Ok tst
    | TInt128, ValueType.Known KTInt128 -> return Ok tst
    | TUInt128, ValueType.Known KTUInt128 -> return Ok tst

    | TFloat, ValueType.Known KTFloat -> return Ok tst

    | TChar, ValueType.Known KTChar -> return Ok tst
    | TString, ValueType.Known KTString -> return Ok tst

    | TUuid, ValueType.Known KTUuid -> return Ok tst
    | TDateTime, ValueType.Known KTDateTime -> return Ok tst

    | TList innerT, ValueType.Known(KTList innerV) ->
      return! r tst (TypeCheckPathPart.ListType :: pathSoFar) innerT innerV

    | TDict innerT, ValueType.Known(KTDict innerV) ->
      return! r tst (TypeCheckPathPart.DictValueType :: pathSoFar) innerT innerV

    | TTuple(tFirst, tSecond, tRest),
      ValueType.Known(KTTuple(vFirst, vSecond, vRest)) ->
      // first, make sure that the tuple lengths match
      let expectedLen = 2 + List.length tRest
      let actualLen = 2 + List.length vRest
      if expectedLen <> actualLen then
        return
          Error(TypeCheckPathPart.TupleLength(expectedLen, actualLen) :: pathSoFar)
      else
        // then, make sure that the tuple elements match
        let expected = tFirst :: tSecond :: tRest
        let actual = vFirst :: vSecond :: vRest
        let! result =
          Ply.List.foldSequentially
            (fun acc (i, e, a) ->
              match acc with
              | Error _path -> Ply acc
              | Ok tst -> r tst (TypeCheckPathPart.TupleAtIndex i :: pathSoFar) e a)
            (Ok tst)
            (List.zip expected actual |> List.mapi (fun i (e, a) -> (i, e, a)))

        return result

    | TCustomType(Error err, _), _ ->
      return RTE.ParseTimeNameResolution err |> raiseUntargetedRTE

    | TCustomType(Ok typeNameT, typeArgsT), actual ->
      // CLEANUP can't we assume aliases are already unwrapped?
      // if so, we can tidy this case quite a bit
      match! Types.find types typeNameT with
      | None -> return Error pathSoFar
      | Some expected ->
        match expected, actual with
        | { definition = TypeDeclaration.Alias aliasType }, _ ->
          //debuG "unwrapping alias" aliasType
          match! unwrapAlias types aliasType with
          | Error _rte -> return Error pathSoFar
          | Ok expected -> return! r tst pathSoFar expected actual

        | _, ValueType.Known(KTCustomType(typeNameV, typeArgsV)) ->
          if typeNameV <> typeNameT then
            return Error pathSoFar
          else if List.length typeArgsT <> List.length typeArgsV then
            // (this is really unexpected -- something _in our code_ failed hard.)
            return
              Error(
                TypeCheckPathPart.TypeArgLength(
                  typeNameT,
                  List.length typeArgsT,
                  List.length typeArgsV
                )
                :: pathSoFar
              )
          else
            return!
              List.zip typeArgsT typeArgsV
              |> Ply.List.foldSequentiallyWithIndex
                (fun i acc (e, a) ->
                  match acc with
                  | Error _path -> Ply acc
                  | Ok tst ->
                    uply {
                      let path =
                        TypeCheckPathPart.TypeArg(
                          typeNameT,
                          i,
                          List.length typeArgsT
                        )
                        :: pathSoFar
                      match! r tst path e a with
                      | Error path -> return Error path
                      | Ok tst -> return Ok tst
                    })
                (Ok tst)

        | _, _ -> return Error pathSoFar

    | TFn(argTypes, returnType), ValueType.Known(KTFn(vArgs, vRet)) ->
      if NEList.length argTypes <> NEList.length vArgs then
        return Error pathSoFar // TODO include the lengths in the path
      else
        return!
          List.zip
            (returnType :: (NEList.toList argTypes))
            (vRet :: (NEList.toList vArgs))
          |> Ply.List.foldSequentially
            (fun acc (e, a) ->
              match acc with
              | Error _path -> Ply acc
              | Ok tst -> r tst pathSoFar e a)
            (Ok tst)

    | TDB innerT, ValueType.Known(KTDB innerV) ->
      return! r tst pathSoFar innerT innerV

    | _, _ -> return Error pathSoFar
  }


let unify
  (types : Types)
  (tst : TypeSymbolTable)
  // maybe make this take a ValueType instead?
  // the TVariable cases being resolveable allows us to update the tst, though.
  // how else to achieve that?
  // update Unknown to have a var name? enh.
  (expected : TypeReference)
  (actual : Dval)
  : Ply<Result<TypeSymbolTable, ReverseTypeCheckPath>> =
  uply {
    let actualType = Dval.toValueType actual
    match! unifyValueType types tst [] expected actualType with
    | Error path -> return path |> Error
    | Ok updatedTst -> return Ok updatedTst
  }



let checkFnParam
  (types : Types)
  (fnName : FQFnName.FQFnName)
  (tst : TypeSymbolTable)
  (paramIndex : int)
  (paramName : string)
  (expected : TypeReference)
  (actual : Dval)
  : Ply<Result<TypeSymbolTable, RTE.Error>> =
  uply {
    match! unify types tst expected actual with
    | Ok updatedTst -> return Ok updatedTst
    | Error _path ->
      // CLEANUP include the path in the RTE

      return
        RTE.Applications.FnParameterNotExpectedType(
          fnName,
          paramIndex,
          paramName,
          TypeReference.toVT tst expected,
          Dval.toValueType actual,
          actual
        )
        |> RTE.Apply
        |> Error
  }



// let checkFunctionCallParam
//   (types : Types)
//   (tst : TypeSymbolTable)
//   //(fn : Fn) // -- aha, generic thing here, beware
//   (param: Param)
//   (arg : Dval)
//   : Ply<Result<TypeSymbolTable, RuntimeError>> =
//   // The interpreter checks these are the same length
//   fn.parameters
//   |> NEList.map2WithIndex
//     (fun i value param ->
//       // probably, this 'context' thing isn't really what we want any more?
//       //, ah this is maybe a 'path' thing tho?
//       // OK so - we should create and pass in an 'empty' path
//       // and upon failure, here yield the correct 'fancier' error.
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
//   // see notes above - just pass in empty path, and return special RTE
//   unify context types tst fn.returnType result




/// Helpers for creating type-checked Dvals
/// (lists, records, enums, etc.)
///
/// Dvals should be created carefully,:
/// - to have the correct `ValueType`s, where appropriate
///  i.e. we should not have `DList(Known KTInt64, [ DString("hi") ])`
///
/// - similarly, we should fail when trying to merge `Dval`s with conflicting `ValueType`s
///   i.e. `List.append [1] ["hi"]` should fail
///   because we can't merge `Known KTInt64` and `Known KTString`
///
/// These functions are intended to help with both of these,
/// in cases where the functions in `Dval.fs` are insufficient
/// (i.e. we don't know the Dark sub-types of a Dval in some F# code).
///
/// Doing this at-construction is important to ensure efficient run-time type-checking.
///
/// CLEANUP consider: stop type-checking after the first N elements in .list and .dict, for performance
module DvalCreator =
  // CLEANUP consider skipping type-checking after N elements or after the type args are fully resolved, whichever comes last.
  //   In order to support this^, add another param or two so that direct [] interpretation is differentiated from calls to this from Builtins and other places.
  //   (or split this into 2 separate fns with clearer names)
  let list (threadID : ThreadID) (typ : ValueType) (items : List<Dval>) : Dval =
    let (typ, items) =
      items
      |> List.foldWithIndex
        (fun i (typ, list) dv ->
          let dvalType = Dval.toValueType dv

          match VT.merge typ dvalType with
          | Ok newType -> newType, dv :: list
          | Error() ->
            RTE.Lists.Error.TriedToAddMismatchedData(i, typ, dvalType, dv)
            |> RTE.Error.List
            |> raiseRTE threadID)
        (typ, [])

    DList(typ, List.rev items)


  // CLEANUP see notes in `list` above
  let dict
    (threadID : ThreadID)
    (typ : ValueType)
    (entries : List<string * Dval>)
    : Dval =
    let (typ, entries) =
      List.fold
        (fun (typ, entries) (k, v) ->
          if Map.containsKey k entries then
            RTE.Dicts.Error.TriedToAddKeyAfterAlreadyPresent k
            |> RTE.Error.Dict
            |> raiseRTE threadID

          let vt = Dval.toValueType v

          match VT.merge typ vt with
          | Ok merged -> (merged, Map.add k v entries)
          | Error() ->
            RTE.Dicts.Error.TriedToAddMismatchedData(k, typ, vt, v)
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
      RuntimeError.Enums.ConstructionFieldOfWrongType(
        "Some",
        0,
        expectedType,
        vt,
        dv
      )
      |> RuntimeError.Enum
      |> raiseRTE threadID


  let option
    (threadID : ThreadID)
    (expectedType : ValueType)
    (dv : Option<Dval>)
    : Dval =
    match dv with
    | Some dv -> optionSome threadID expectedType dv
    | None -> optionNone expectedType


  module Result =
    let typeName = Dval.resultType

    let ok
      (threadID : ThreadID)
      (okType : ValueType)
      (errorType : ValueType)
      (dvOk : Dval)
      : Dval =
      let dvalType = Dval.toValueType dvOk
      match VT.merge okType dvalType with
      | Ok typ -> DEnum(typeName, typeName, [ typ; errorType ], "Ok", [ dvOk ])
      | Error() ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          "Ok",
          0,
          okType,
          dvalType,
          dvOk
        )
        |> RuntimeError.Enum
        |> raiseRTE threadID

    let error
      // maybe this needs some 'path' or something?
      (threadID : ThreadID)
      (okType : ValueType)
      (errorType : ValueType)
      (dvError : Dval)
      : Dval =
      let dvalType = Dval.toValueType dvError
      match VT.merge errorType dvalType with
      | Ok typ -> DEnum(typeName, typeName, [ okType; typ ], "Error", [ dvError ])
      | Error() ->
        RuntimeError.Enums.ConstructionFieldOfWrongType(
          "Error",
          0,
          errorType,
          dvalType,
          dvError
        )
        |> RuntimeError.Enum
        |> raiseRTE threadID

    let result
      (threadID : ThreadID)
      (okType : ValueType)
      (errorType : ValueType)
      (dv : Result<Dval, Dval>)
      : Dval =
      match dv with
      | Ok dv -> ok threadID okType errorType dv
      | Error dv -> error threadID okType errorType dv


  let rec private resolveEnumType
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (typeName : FQTypeName.FQTypeName)
    (providedTypeArgs : List<TypeReference>)
    : Ply<FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.EnumCase>>
    =
    uply {
      match! Types.find types typeName with
      | None -> return RTE.TypeNotFound typeName |> raiseRTE threadID

      | Some decl ->
        match decl.definition with
        | TypeDeclaration.Enum cases ->
          // If type args weren't provided but are needed, create Unknown ones
          let typeArgsVT =
            if
              List.isEmpty providedTypeArgs && not (List.isEmpty decl.typeParams)
            then
              decl.typeParams |> List.map (fun p -> (p, ValueType.Unknown))
            else
              List.zip decl.typeParams providedTypeArgs
              |> List.map (fun (p, a) -> (p, TypeReference.toVT tst a))

          return (typeName, typeArgsVT, cases)

        | TypeDeclaration.Alias aliasedType ->
          match! unwrapAlias types aliasedType with
          | Error _err -> return RTE.TypeNotFound typeName |> raiseRTE threadID
          | Ok resolvedType ->
            match resolvedType with
            | TCustomType(Ok innerTypeName, innerTypeArgs) ->
              match! Types.find types innerTypeName with
              | None -> return RTE.TypeNotFound innerTypeName |> raiseRTE threadID
              | Some targetDecl ->
                // Create mapping from original type params to provided args/unknowns
                let providedArgsMap =
                  if
                    List.isEmpty providedTypeArgs
                    && not (List.isEmpty decl.typeParams)
                  then
                    decl.typeParams
                    |> List.map (fun p -> p, (p, ValueType.Unknown))
                    |> Map.ofList
                  else
                    List.zip decl.typeParams providedTypeArgs
                    |> List.map (fun (p, a) -> p, (p, TypeReference.toVT tst a))
                    |> Map.ofList

                // Map inner type args, using the target type's param names
                let mappedInnerArgsVT =
                  List.zip targetDecl.typeParams innerTypeArgs
                  |> List.map (fun (targetParam, typeRef) ->
                    let vt = TypeReference.toVT tst typeRef
                    match typeRef with
                    | TVariable name ->
                      match Map.tryFind name providedArgsMap with
                      | Some(_, vt) -> (targetParam, vt)
                      | None -> (targetParam, vt)
                    | _ -> (targetParam, vt))

                return!
                  resolveEnumType types threadID tst innerTypeName []
                  |> Ply.map (fun (resolvedName, _, cases) ->
                    (resolvedName, mappedInnerArgsVT, cases))

            | _ -> return RTE.TypeNotFound typeName |> raiseRTE threadID

        | TypeDeclaration.Record _ ->
          return
            Exception.raiseInternal
              "Expected enum type but found record"
              [ "typeName", typeName ]
    }



  let enum
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<TypeReference>)
    (caseName : string)
    (fields : List<Dval>)
    : Ply<Dval * TypeSymbolTable> =
    uply {
      // do basic resolution of aliases and type args
      let! (resolvedTypeName, typeArgs, caseDefs) =
        resolveEnumType types threadID Map.empty sourceTypeName typeArgs

      // TODO not just add, but _merge_
      let tst = typeArgs |> List.fold (fun acc (name, vt) -> Map.add name vt acc) tst

      // Find the case definition
      let foundCaseDef = caseDefs |> NEList.find (fun c -> c.name = caseName)

      match foundCaseDef with
      | None ->
        return // CLEANUP: do I need this return? and more generally in a uply like this?
          RTE.Enums.ConstructionCaseNotFound(resolvedTypeName, caseName)
          |> RTE.Error.Enum
          |> raiseRTE threadID

      | Some case ->
        // Zip the fields, if we got the right # of them
        let fieldsZipped =
          let expected, actual = (List.length case.fields, List.length fields)

          if expected <> actual then
            RTE.Enums.ConstructionWrongNumberOfFields(
              resolvedTypeName,
              caseName,
              expected,
              actual
            )
            |> RTE.Error.Enum
            |> raiseRTE threadID
          else
            List.zip case.fields fields

        // Process each field, updating type args as we learn more
        let! (typeArgs, fieldsInReverse, updatedTst) =
          Ply.List.foldSequentiallyWithIndex
            (fun fieldIndex (typeArgs, fieldsInReverse, tst) (fieldDef, actualField) ->
              uply {
                match! unify types tst fieldDef actualField with
                | Error _path ->
                  return
                    RTE.Enums.ConstructionFieldOfWrongType(
                      caseName,
                      fieldIndex,
                      TypeReference.toVT tst fieldDef,
                      Dval.toValueType actualField,
                      actualField
                    )
                    |> RTE.Error.Enum
                    |> raiseRTE threadID

                | Ok newTST ->
                  // Update resultant typeArgs based on what we learned from this field
                  // , by checking the TST.
                  let newTypeArgs =
                    typeArgs
                    |> List.map (fun (paramName, vt) ->
                      match vt with
                      | ValueType.Unknown ->
                        match Map.tryFind paramName newTST with
                        | Some known -> (paramName, known)
                        | None -> (paramName, vt)

                      | known ->
                        match ValueType.merge known vt with
                        | Ok merged -> (paramName, merged)
                        | Error() ->
                          RTE.Enums.ConstructionFieldOfWrongType(
                            caseName,
                            fieldIndex,
                            TypeReference.toVT tst fieldDef,
                            Dval.toValueType actualField,
                            actualField
                          )
                          |> RTE.Enum
                          |> raiseRTE threadID)

                  return (newTypeArgs, actualField :: fieldsInReverse, newTST)
              })
            (typeArgs, [], tst)
            fieldsZipped

        let typeArgs = typeArgs |> List.map Tuple2.second
        let fields = List.rev fieldsInReverse
        return
          (DEnum(sourceTypeName, resolvedTypeName, typeArgs, caseName, fields),
           updatedTst)
    }


  // Resolve aliases and collect expected fields for a record type
  let rec resolveRecordType
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (typeName : FQTypeName.FQTypeName)
    (providedTypeArgs : List<TypeReference>)
    : Ply<FQTypeName.FQTypeName *
      List<string * ValueType> *
      NEList<TypeDeclaration.RecordField>>
    =
    uply {
      match! Types.find types typeName with
      | None -> return RTE.TypeNotFound typeName |> raiseRTE threadID

      | Some decl ->
        match decl.definition with
        | TypeDeclaration.Record fields ->
          let (typeArgsVT : List<string * ValueType>) =
            if
              List.isEmpty providedTypeArgs && not (List.isEmpty decl.typeParams)
            then
              decl.typeParams |> List.map (fun p -> (p, ValueType.Unknown))
            else
              List.zip decl.typeParams providedTypeArgs
              |> List.map (fun (p, a) -> (p, TypeReference.toVT tst a))

          return (typeName, typeArgsVT, fields)

        | TypeDeclaration.Alias aliasedType ->
          match! unwrapAlias types aliasedType with
          | Error _err -> return RTE.TypeNotFound typeName |> raiseRTE threadID
          | Ok resolvedType ->
            match resolvedType with
            | TCustomType(Ok innerTypeName, innerTypeArgs) ->
              match! Types.find types innerTypeName with
              | None -> return RTE.TypeNotFound innerTypeName |> raiseRTE threadID
              | Some targetDecl ->
                let providedArgsMap =
                  if
                    List.isEmpty providedTypeArgs
                    && not (List.isEmpty decl.typeParams)
                  then
                    decl.typeParams
                    |> List.map (fun p -> p, (p, ValueType.Unknown))
                    |> Map.ofList
                  else
                    List.zip decl.typeParams providedTypeArgs
                    |> List.map (fun (p, a) -> p, (p, TypeReference.toVT tst a))
                    |> Map.ofList

                let mappedInnerArgsVT =
                  List.zip targetDecl.typeParams innerTypeArgs
                  |> List.map (fun (targetParam, typeRef) ->
                    let vt = TypeReference.toVT tst typeRef
                    match typeRef with
                    | TVariable name ->
                      match Map.tryFind name providedArgsMap with
                      | Some(_, vt) -> (targetParam, vt)
                      | None -> (targetParam, vt)
                    | _ -> (targetParam, vt))

                return!
                  resolveRecordType types threadID tst innerTypeName []
                  |> Ply.map (fun (resolvedName, _, fields) ->
                    (resolvedName, mappedInnerArgsVT, fields))

            | _ ->
              return
                RTE.Records.CreationTypeNotRecord typeName
                |> RTE.Record
                |> raiseRTE threadID

        | TypeDeclaration.Enum _ ->
          return
            RTE.Records.CreationTypeNotRecord typeName
            |> RTE.Record
            |> raiseRTE threadID
    }



  /// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
  ///
  /// note: if provided, the typeArgs must match the # of typeArgs expected by the type
  let record
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (typeArgs : List<TypeReference>)
    (fields : List<string * Dval>)
    : Ply<Dval * TypeSymbolTable> =
    uply {
      let! (resolvedTypeName, resolvedTypeArgs, expectedFields) =
        resolveRecordType types threadID tst sourceTypeName typeArgs

      // TODO not just add, but _merge_
      let tst =
        resolvedTypeArgs |> List.fold (fun acc (name, vt) -> Map.add name vt acc) tst

      // Process each provided field
      let! (processedFields, finalTypeArgs, updatedTST) =
        Ply.List.foldSequentially
          (fun (fieldsSoFar, currentTypeArgs, tst) (fieldName, fieldValue) ->
            uply {
              // Basic validation
              if fieldName = "" then
                return
                  RTE.Records.CreationEmptyKey |> RTE.Record |> raiseRTE threadID

              if Map.containsKey fieldName fieldsSoFar then
                return
                  RTE.Records.CreationDuplicateField fieldName
                  |> RTE.Record
                  |> raiseRTE threadID

              // Find and validate field
              match expectedFields |> NEList.find (fun f -> f.name = fieldName) with
              | None ->
                return
                  RTE.Records.CreationFieldNotExpected fieldName
                  |> RTE.Record
                  |> raiseRTE threadID

              | Some fieldDef ->
                match! unify types tst fieldDef.typ fieldValue with
                | Error _path ->
                  return
                    RTE.Records.CreationFieldOfWrongType(
                      fieldName,
                      TypeReference.toVT tst fieldDef.typ,
                      Dval.toValueType fieldValue,
                      fieldValue
                    )
                    |> RTE.Record
                    |> raiseRTE threadID

                | Ok newTST ->
                  // Update resultant typeArgs based on what we learned from this field
                  // , by checking the TST.
                  let newTypeArgs =
                    currentTypeArgs
                    |> List.map (fun (paramName, vt) ->
                      match vt with
                      | ValueType.Unknown ->
                        match Map.tryFind paramName newTST with
                        | Some known -> (paramName, known)
                        | None -> (paramName, vt)

                      | known ->
                        match ValueType.merge known vt with
                        | Ok merged -> (paramName, merged)
                        | Error() ->
                          RTE.Records.CreationFieldOfWrongType(
                            fieldName,
                            TypeReference.toVT tst fieldDef.typ,
                            Dval.toValueType fieldValue,
                            fieldValue
                          )
                          |> RTE.Record
                          |> raiseRTE threadID)

                  let fields = Map.add fieldName fieldValue fieldsSoFar

                  return (fields, newTypeArgs, newTST)
            })
          (Map.empty, resolvedTypeArgs, tst)
          fields

      // Check for missing fields
      match
        expectedFields
        |> NEList.find (fun f -> not (Map.containsKey f.name processedFields))
      with
      | Some missingField ->
        return
          RTE.Records.CreationMissingField missingField.name
          |> RTE.Record
          |> raiseRTE threadID

      | None ->
        let record =
          DRecord(
            sourceTypeName,
            resolvedTypeName,
            finalTypeArgs |> List.map Tuple2.second,
            processedFields
          )
        return (record, updatedTST)
    }


  /// Constructs a Dval.DRecord, ensuring that the fields match the expected shape
  ///
  /// note: if provided, the typeArgs must match the # of typeArgs expected by the type
  let recordUpdate
    (types : Types)
    (threadID : ThreadID)
    (tst : TypeSymbolTable)
    (sourceTypeName : FQTypeName.FQTypeName)
    (resolvedTypeName : FQTypeName.FQTypeName)
    (typeArgsBeforeUpdate : List<ValueType>)
    (currentFields : Map<string, Dval>)
    (fieldUpdates : List<string * Dval>)
    : Ply<Dval * TypeSymbolTable> =
    uply {
      let! (_resolvedTypeName, resolvedTypeArgs, expectedFields) =
        resolveRecordType types threadID tst sourceTypeName []

      let resolvedTypeArgs =
        List.zip typeArgsBeforeUpdate resolvedTypeArgs
        |> List.map (fun (beforeUpdate, (name, _)) -> (name, beforeUpdate))

      let! (updatedFields, finalTypeArgs, updatedTST) =
        Ply.List.foldSequentially
          (fun (fieldsSoFar, currentTypeArgs, tst) (fieldName, fieldValue) ->
            uply {
              if fieldName = "" then
                return RTE.Records.UpdateEmptyKey |> RTE.Record |> raiseRTE threadID

              // CLEANUP if there are duplicate updates for the the same field, raise a `UpdateDuplicateField` RTE

              else
                match
                  expectedFields |> NEList.find (fun f -> f.name = fieldName)
                with
                | None ->
                  return
                    RTE.Records.UpdateFieldNotExpected fieldName
                    |> RTE.Record
                    |> raiseRTE threadID

                | Some fieldDef ->
                  match! unify types tst fieldDef.typ fieldValue with
                  | Error _path ->
                    // CLEANUP involve path, somehow
                    return
                      RTE.Records.UpdateFieldOfWrongType(
                        fieldName,
                        TypeReference.toVT tst fieldDef.typ,
                        Dval.toValueType fieldValue,
                        fieldValue
                      )
                      |> RTE.Record
                      |> raiseRTE threadID
                  | Ok updatedTst ->
                    // Update resultant typeArgs based on what we learned from this field
                    // , by checking the TST.
                    let newTypeArgs =
                      currentTypeArgs
                      |> List.map (fun (paramName, vt) ->
                        match vt with
                        | ValueType.Unknown ->
                          match Map.tryFind paramName updatedTst with
                          | Some known -> (paramName, known)
                          | None -> (paramName, vt)

                        | known ->
                          match ValueType.merge known vt with
                          | Ok merged -> (paramName, merged)
                          | Error() ->
                            RTE.Records.UpdateFieldOfWrongType(
                              fieldName,
                              TypeReference.toVT tst fieldDef.typ,
                              Dval.toValueType fieldValue,
                              fieldValue
                            )
                            |> RTE.Record
                            |> raiseRTE threadID)

                    let fields = Map.add fieldName fieldValue fieldsSoFar

                    return (fields, newTypeArgs, updatedTst)
            })
          (currentFields, resolvedTypeArgs, tst)
          fieldUpdates

      let finalTypeArgs = finalTypeArgs |> List.map Tuple2.second

      let updatedRecord =
        DRecord(sourceTypeName, resolvedTypeName, finalTypeArgs, updatedFields)

      return (updatedRecord, updatedTST)
    }
