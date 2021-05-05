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
module PT = LibBackend.ProgramTypes
module Exe = LibExecution.Execution

open TestUtils

let equalsOCaml =
  // These are hard to represent in .tests files, usually because of FakeDval behaviour
  testMany
    "equalsOCaml"
    (FuzzTests.All.ExecutePureFunctions.equalsOCaml)
    [ ((RT.FQFnName.stdlibFnName "List" "fold" 0,
        [ RT.DList [ RT.DBool true; RT.DErrorRail(RT.DInt 0I) ]
          RT.DList []
          RT.DFnVal(
            RT.Lambda { parameters = []; symtable = Map.empty; body = RT.EBlank 1UL }
          ) ]),
       true) ]

let oldFunctionsAreDeprecated =
  test "old functions are deprecated" {

    let counts = ref Map.empty

    let fns =
      LibTest.fns @ LibExecution.StdLib.StdLib.fns @ LibBackend.StdLib.StdLib.fns

    fns
    |> List.iter
         (fun fn ->
           let key = string { fn.name with version = 0 }

           if fn.deprecated = RT.NotDeprecated then
             counts
             := Map.update
                  key
                  (fun count -> count |> Option.defaultValue 0 |> (+) 1 |> Some)
                  !counts

           ())

    Map.iter
      (fun name count ->
        Expect.equal count 1 $"{name} has more than one undeprecated function")
      !counts
  }


// FSTODO
// let t_dark_internal_fns_are_internal () =
//   let ast = fn "DarkInternal::checkAccess" [] in
//   let check_access canvas_name =
//     match exec_ast ~canvas_name ast with DError _ -> None | dval -> Some dval
//   in
//   AT.check
//     (AT.list (AT.option at_dval))
//     "DarkInternal:: functions are internal."
//     [check_access "test"; check_access "test_admin"]
//     [None; Some DNull]

let tests = testList "stdlib" [ equalsOCaml; oldFunctionsAreDeprecated ]
