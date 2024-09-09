module Tests.RuntimeTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes


// // <summary>
// // Checks if a runtime's value matches a given type
// // </summary>
// // <remarks>
// // We have nested types so they need to be checked deeper. CLEANUP:
// // there is also "real" type checking elsewhere - this should be unified.
// // Note, this is primarily used to figure out which argument has ALREADY not
// // matched the actual runtime parameter type of the called function. So more
// // accuracy is better, as the runtime is perfectly accurate.
// // </summary>
// let rec typeMatches (typ : TypeReference) (dv : Dval) : bool =
//   let r = typeMatches

//   match (dv, typ) with
//   //| _, TVariable _ -> true

//   | DUnit, TUnit
//   | DBool _, TBool

//   | DInt8 _, TInt8
//   | DUInt8 _, TUInt8
//   | DInt16 _, TInt16
//   | DUInt16 _, TUInt16
//   | DInt32 _, TInt32
//   | DUInt32 _, TUInt32
//   | DInt64 _, TInt64
//   | DUInt64 _, TUInt64
//   | DInt128 _, TInt128
//   | DUInt128 _, TUInt128

//   | DFloat _, TFloat

//   | DChar _, TChar
//   | DString _, TString

//   | DDateTime _, TDateTime
//   | DUuid _, TUuid

//    -> true

//   | DList(_vtTODO, l), TList t -> List.all (r t) l
//   | DTuple(first, second, theRest), TTuple(firstType, secondType, otherTypes) ->
//     let pairs =
//       [ (first, firstType); (second, secondType) ] @ List.zip theRest otherTypes

//     pairs |> List.all (fun (v, subtype) -> r subtype v)
//   | DDict(_vtTODO, m), TDict t -> Map.all (r t) m

//   | DRecord(typeName, _, _typeArgsTODO, _fields),
//     TCustomType(Ok typeName', _typeArgs) ->
//     // TYPESCLEANUP: should load type by name
//     // TYPESCLEANUP: are we handling type arguments here?
//     // TYPESCLEANUP: do we need to check fields?
//     typeName = typeName'

//   | DEnum(_, typeName, _typeArgsDEnumTODO, _casename, _fields),
//     TCustomType(Ok typeName', _typeArgsExpected) ->
//     // TYPESCLEANUP: should load type by name
//     // TYPESCLEANUP: convert TCustomType's typeArgs to valueTypes, and compare
//     // against the typeArgs in the DEnum - their zipped values should merge OK
//     typeName = typeName'

//   // | DFnVal(Lambda l), TFn(parameters, _) ->
//   //   NEList.length parameters = NEList.length l.parameters

//   // | DDB _, TDB _

//   // exhaustiveness checking
//   | DUnit, _
//   | DBool _, _
//   | DInt8 _, _
//   | DUInt8 _, _
//   | DInt16 _, _
//   | DUInt16 _, _
//   | DInt32 _, _
//   | DUInt32 _, _
//   | DInt64 _, _
//   | DUInt64 _, _
//   | DInt128 _, _
//   | DUInt128 _, _
//   | DFloat _, _
//   | DChar _, _
//   | DString _, _
//   | DDateTime _, _
//   | DUuid _, _
//   | DList _, _
//   | DTuple _, _
//   | DDict _, _
//   | DRecord _, _
//   | DEnum _, _
//   | DFnVal _, _
//   // | DDB _, _
//    -> false

// let dvalTypeMatches =
//   testList
//     "dvalTypeMatches"
//     [ test "matching tuple" {
//         let v =
//           RT.Dval.DTuple(
//             RT.Dval.DInt64 1,
//             RT.Dval.DString "two",
//             [ RT.Dval.DFloat 3.14 ]
//           )
//         let tipe =
//           RT.TypeReference.TTuple(
//             RT.TypeReference.TInt64,
//             RT.TypeReference.TString,
//             [ RT.TypeReference.TFloat ]
//           )

//         Expect.isTrue (RT.Dval.typeMatches tipe v) ""
//       }

//       test "non-matching tuple" {
//         let v =
//           RT.Dval.DTuple(
//             RT.Dval.DInt64 1,
//             RT.Dval.DString "two",
//             [ RT.Dval.DFloat 3.14 ]
//           )
//         let tipe =
//           RT.TypeReference.TTuple(
//             RT.TypeReference.TInt64,
//             RT.TypeReference.TString,
//             [ RT.TypeReference.TChar ]
//           )

//         Expect.isFalse (RT.Dval.typeMatches tipe v) ""
//       } ]

let tests = testList "RuntimeTypes" []
