module BuiltinExecution.Libs.NoModule

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
module PackageHashes = LibExecution.PackageHashes
module Dval = LibExecution.Dval
module ValueType = LibExecution.ValueType
module RTE = RuntimeError


/// Note that type errors should be handled by the caller,
/// by using `Dval.toValueType`, attempting to merge the types,
/// and raising an RTE if the merge fails.
/// Any type mis-matches found in this fn will just return `false`.
let rec equals (a : Dval) (b : Dval) : bool =
  let r = equals

  match a, b with
  | DUnit, DUnit -> true

  | DBool a, DBool b -> a = b

  | DInt8 a, DInt8 b -> a = b
  | DUInt8 a, DUInt8 b -> a = b
  | DInt16 a, DInt16 b -> a = b
  | DUInt16 a, DUInt16 b -> a = b
  | DInt32 a, DInt32 b -> a = b
  | DUInt32 a, DUInt32 b -> a = b
  | DInt64 a, DInt64 b -> a = b
  | DUInt64 a, DUInt64 b -> a = b
  | DInt128 a, DInt128 b -> a = b
  | DUInt128 a, DUInt128 b -> a = b

  | DFloat a, DFloat b -> a = b

  | DChar a, DChar b -> a = b
  | DString a, DString b -> a = b

  | DDateTime a, DDateTime b -> a = b

  | DUuid a, DUuid b -> a = b

  | DList(typA, a), DList(typB, b) ->
    Result.isOk (ValueType.merge typA typB)
    && a.Length = b.Length
    && List.forall2 r a b

  | DTuple(a1, a2, a3), DTuple(b1, b2, b3) ->
    if a3.Length <> b3.Length then
      false
    else
      r a1 b1 && r a2 b2 && List.forall2 r a3 b3

  | DDict(typeA, a), DDict(typeB, b) ->
    Result.isOk (ValueType.merge typeA typeB)
    && Map.count a = Map.count b
    && Map.forall
      (fun k v -> Map.find k b |> Option.map (r v) |> Option.defaultValue false)
      a

  | DRecord(_, typeNameA, typeArgsA, fieldsA),
    DRecord(_, typeNameB, typeArgsB, fieldsB) ->
    // same _resolved_ type name
    typeNameA = typeNameB
    // same type args (at least _mergeable_ -- ignores Unknowns)
    && typeArgsA.Length = typeArgsB.Length
    && List.forall2
      (fun typeArgA typeArgB -> Result.isOk (ValueType.merge typeArgA typeArgB))
      typeArgsA
      typeArgsB
    // same fields
    && Map.count fieldsA = Map.count fieldsB
    && Map.forall
      (fun k v ->
        Map.find k fieldsB |> Option.map (r v) |> Option.defaultValue false)
      fieldsA

  | DEnum(_, typeNameA, typeArgsA, caseNameA, fieldsA),
    DEnum(_, typeNameB, typeArgsB, caseNameB, fieldsB) ->
    // same _resolved_ type name
    typeNameA = typeNameB
    // same type args (at least _mergeable_ -- ignores Unknowns)
    && typeArgsA.Length = typeArgsB.Length
    && List.forall2
      (fun typeArgA typeArgB -> Result.isOk (ValueType.merge typeArgA typeArgB))
      typeArgsA
      typeArgsB
    // same case name
    && caseNameA = caseNameB
    // same fields
    && fieldsA.Length = fieldsB.Length
    && List.forall2 r fieldsA fieldsB


  | DApplicable a, DApplicable b ->
    match a, b with
    | AppLambda a, AppLambda b ->
      // CLEANUP this is very incomplete,
      // but/because fully checking for equality of LambdaImpls may require some heavy refactoring.
      a.exprId = b.exprId

    | AppNamedFn a, AppNamedFn b -> a = b

    | AppLambda _, _
    | AppNamedFn _, _ -> false

  | DDB a, DDB b -> a = b

  // exhaustiveness check
  | DUnit, _
  | DBool _, _
  | DInt8 _, _
  | DUInt8 _, _
  | DInt16 _, _
  | DUInt16 _, _
  | DInt32 _, _
  | DUInt32 _, _
  | DInt64 _, _
  | DUInt64 _, _
  | DInt128 _, _
  | DUInt128 _, _
  | DFloat _, _
  | DChar _, _
  | DString _, _
  | DDateTime _, _
  | DUuid _, _
  | DList _, _
  | DTuple _, _
  | DDict _, _
  | DRecord _, _
  | DEnum _, _
  | DApplicable _, _
  | DDB _, _ ->
    // type errors; should be caught above by the caller
    false


let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "equals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are equal"
      fn =
        (function
        | _, vm, _, [ a; b ] ->
          let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
          match ValueType.merge vtA vtB with
          | Error _ ->
            RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB) |> raiseRTE vm.threadID
          | Ok _ -> equals a b |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "="
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "notEquals" 0
      typeParams = []
      parameters = [ Param.make "a" varA ""; Param.make "b" varB "" ]
      returnType = TBool
      description = "Returns true if the two value are not equal"
      fn =
        (function
        | _, vm, _, [ a; b ] ->
          let (vtA, vtB) = (Dval.toValueType a, Dval.toValueType b)
          match ValueType.merge vtA vtB with
          | Error _ ->
            RTE.EqualityCheckOnIncompatibleTypes(vtA, vtB) |> raiseRTE vm.threadID
          | Ok _ -> equals a b |> not |> DBool |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "<>"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TVariable "optOrRes") "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or raising a RuntimeError if None"
      fn =
        (function
        | _, _, _, [] -> incorrectArgs ()
        | _, vm, _, [ dval ] ->
          match dval with

          // Success: extract `Some` out of an Option
          | DEnum(FQTypeName.Package id, _, _, "Some", [ value ]) when
            id = PackageHashes.Type.Stdlib.option
            ->
            Ply value

          // Success: extract `Ok` out of a Result
          | DEnum(FQTypeName.Package id, _, _, "Ok", [ value ]) when
            id = PackageHashes.Type.Stdlib.result
            ->
            Ply value

          // Error: expected Some, got None
          | DEnum(FQTypeName.Package id, _, _, "None", []) when
            id = PackageHashes.Type.Stdlib.option
            ->
            RuntimeError.Unwraps.GotNone
            |> RuntimeError.Unwrap
            |> raiseRTE vm.threadID

          // Error: expected Ok, got Error
          | DEnum(FQTypeName.Package id, _, _, "Error", [ value ]) when
            id = PackageHashes.Type.Stdlib.result
            ->
            RuntimeError.Unwraps.GotError value
            |> RuntimeError.Unwrap
            |> raiseRTE vm.threadID

          // Error: single dval, but not an Option or Result
          | otherDval ->
            RuntimeError.Unwraps.NonOptionOrResult otherDval
            |> RuntimeError.Unwrap
            |> raiseRTE vm.threadID

        // Error: multiple arguments
        | _, vm, _, multipleArgs ->
          RuntimeError.Unwraps.MultipleArgs multipleArgs
          |> RuntimeError.Unwrap
          |> raiseRTE vm.threadID)

      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "debug" 0
      typeParams = []
      parameters =
        [ Param.make "label" TString "The label to be printed."
          Param.make "value" (TVariable "a") "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output"
      fn =
        (function
        | _, _, _, [ DString label; value ] ->
          // TODO: call upon the Dark equivalent fn instead of relying on DvalReprDeveloper
          let value = DvalReprDeveloper.toRepr value
          print $"DEBUG: {label}: {value}"
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
