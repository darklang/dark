module Tests.DvalReprInternalQueryable

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils

module RT = LibExecution.RuntimeTypes
module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module PT = LibExecution.ProgramTypes

module Exe = LibExecution.Execution
module DvalReprInternalQueryable = LibSerialization.DvalReprInternalQueryable
module NR = LibExecution.RuntimeTypes.NameResolution

let toRepr (dval : RT.Dval) : string =
  let builtins = localBuiltIns pmPT
  let state =
    Exe.createState
      builtins
      pmRT
      Exe.noTracing
      (fun _ _ _ _ -> uply { return () })
      (fun _ _ _ _ -> uply { return () })
      PT.mainBranchId
      { dbs = Map.empty }
  (Exe.dvalToRepr state dval).Result
let bogusThreadID = guuid ()

let defaultTypes () = { RT.Types.empty with package = pmRT.getType }

let queryableRoundtripsSuccessfullyInRecord
  (
    dv : RT.Dval,
    fieldTyp : RT.TypeReference
  ) : Task<bool> =

  task {
    let typeHash =
      RT.Hash "82ac8d1c86ef45d4be66052050739a3882ac8d1c86ef45d4be66052050739a38"
    let typeName = RT.FQTypeName.Package typeHash
    let record = RT.DRecord(typeName, typeName, [], Map.ofList [ "field", dv ])
    let typeRef = RT.TCustomType(NR.ok typeName, [])

    let types : RT.Types =
      { package =
          fun id ->
            if id = typeHash then
              let packageType : RT.PackageType.PackageType =
                { hash = typeHash
                  declaration =
                    { typeParams = []
                      definition =
                        RT.TypeDeclaration.Record(
                          NEList.ofList { name = "field"; typ = fieldTyp } []
                        ) } }
              packageType |> Some |> Ply
            else
              pmRT.getType id }

    let! roundtripped =
      record
      |> DvalReprInternalQueryable.toJsonStringV0 types bogusThreadID
      |> Ply.bind (
        DvalReprInternalQueryable.parseJsonV0 types bogusThreadID Map.empty typeRef
      )

    return Expect.RT.dvalEquality record roundtripped
  }

let queryableRoundtripsSuccessfully
  (
    dv : RT.Dval,
    typ : RT.TypeReference
  ) : Task<bool> =
  task {
    let! serialized =
      DvalReprInternalQueryable.toJsonStringV0 (defaultTypes ()) bogusThreadID dv
    let! roundtripped =
      DvalReprInternalQueryable.parseJsonV0
        (defaultTypes ())
        bogusThreadID
        Map.empty
        typ
        serialized
    return Expect.RT.dvalEquality dv roundtripped
  }


let testToDeveloperRepr =
  testList
    "toDeveloperRepr"
    [ testMany
        "toDeveloperRepr string"
        toRepr
        [ RT.DFloat(-0.0), "-0.0"
          RT.DFloat(infinity), "Infinity"
          RT.DTuple(RT.DInt64 1, RT.DInt64 2, [ RT.DInt64 3 ]), "(1, 2, 3)"
          RT.DDict(VT.unit, Map [ "", RT.DUnit ]), "{ : () }"
          RT.DList(VT.unit, [ RT.DUnit ]), "[()]" ] ]

let allRoundtrips =
  let dvs (filter : RT.Dval -> bool) : List<string * (RT.Dval * RT.TypeReference)> =
    List.filter (fun (_, (dv, _)) -> filter dv) (sampleDvals ())

  testList
    "roundtrips"
    [ testListUsingPropertyAsync
        "queryable v0"
        queryableRoundtripsSuccessfully
        (dvs DvalReprInternalQueryable.Test.isQueryableDval)

      testListUsingPropertyAsync
        "queryable record v0"
        queryableRoundtripsSuccessfullyInRecord
        (dvs DvalReprInternalQueryable.Test.isQueryableDval) ]


let tests = testList "dvalRepr" [ testToDeveloperRepr; allRoundtrips ]
