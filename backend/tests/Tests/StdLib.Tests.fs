module Tests.StdLib

// Misc tests of Stdlib (both LibBackend and LibExecution) that could not be
// tested via LibExecution.tests

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PTParser = LibExecution.ProgramTypesParser
module Exe = LibExecution.Execution

open TestUtils.TestUtils

let hardToRepresentTests =
  let execute
    ((fn, args) : PT.FnName.BuiltIn * List<RT.Dval>)
    (expected : RT.Dval)
    : Task<bool> =
    task {
      // evaluate the fn call against both backends
      let! meta = initializeTestCanvas "ExecutePureFunction"
      let args = List.mapi (fun i arg -> ($"v{i}", arg)) args
      let fnArgList = List.map (fun (name, _) -> PT.EVariable(gid (), name)) args

      let ast =
        PT.EApply(gid (), PT.FnTargetName(Ok(PT.FQName.BuiltIn fn)), [], fnArgList)

      let symtable = Map.ofList args

      let! state = executionStateFor meta false false Map.empty Map.empty Map.empty
      let! actual =
        LibExecution.Execution.executeExpr state symtable (PT2RT.Expr.toRT ast)
      return Expect.dvalEquality actual expected
    }

  let fnName mod_ function_ version = PT.FnName.builtIn mod_ function_ version

  // These are hard to represent in .tests files, usually because of FakeDval behaviour
  testMany2Task
    "hardToRepresent"
    execute
    [ (fnName [ "List" ] "fold" 0,
       [ RT.DList [ RT.DBool true; RT.DInt 0L ]

         RT.DList []

         RT.DFnVal(
           RT.Lambda { parameters = []; symtable = Map.empty; body = RT.EUnit 1UL }
         ) ]),
      (RT.DError(RT.SourceNone, "Expected 0 arguments, got 2")),
      true ]

let oldFunctionsAreDeprecated =
  testTask "old functions are deprecated" {
    let counts = ref Map.empty

    let fns = builtIns.fns |> Map.values

    fns
    |> List.iter (fun fn ->
      let key = RT.FnName.builtinToString { fn.name with version = 0 }

      if fn.deprecated = RT.NotDeprecated then
        counts.Value <-
          Map.update
            key
            (fun count -> count |> Option.defaultValue 0 |> (+) 1 |> Some)
            counts.Value

      ())

    Map.iter
      (fun name count ->
        Expect.equal count 1 $"{name} has more than one undeprecated function")
      counts.Value
  }

let oldTypesAreDeprecated =
  testTask "old types are deprecated" {
    let counts = ref Map.empty

    let types = builtIns.types |> Map.values

    types
    |> List.iter (fun typ ->
      let key = RT.TypeName.builtinToString { typ.name with version = 0 }

      if typ.deprecated = RT.NotDeprecated then
        counts.Value <-
          Map.update
            key
            (fun count -> count |> Option.defaultValue 0 |> (+) 1 |> Some)
            counts.Value

      ())

    Map.iter
      (fun name count ->
        Expect.equal count 1 $"{name} has more than one undeprecated type")
      counts.Value
  }

let tests =
  testList
    "stdlib"
    [ hardToRepresentTests; oldFunctionsAreDeprecated; oldTypesAreDeprecated ]
