module Tests.ProgramTypesToRuntimeTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PackageIDs = LibExecution.PackageIDs

// TODO: consider adding an Expect.equalInstructions,
// which better points out the diffs in the lists

module Expressions =
  let one = PT.EInt64(gid (), 1)

  let onePlusTwo : PT.Expr =
    PT.EApply(
      gid (),
      PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "int64Add" 0)),
      [],
      (NEList.ofList (PT.EInt64(gid (), 1)) [ PT.EInt64(gid (), 2) ])
    )

  // TODO: try to use undefined variable
  // TODO: lpunit
  let defineAndUseVar : PT.Expr =
    PT.ELet(
      gid (),
      PT.LPVariable(gid (), "x"),
      PT.EBool(gid (), true),
      PT.EVariable(gid (), "x")
    )

  let boolList : PT.Expr =
    PT.EList(
      gid (),
      [ PT.EBool(gid (), true); PT.EBool(gid (), false); PT.EBool(gid (), true) ]
    )

  let boolListList : PT.Expr =
    PT.EList(
      gid (),
      [ PT.EList(gid (), [ PT.EBool(gid (), true); PT.EBool(gid (), false) ])
        PT.EList(gid (), [ PT.EBool(gid (), false); PT.EBool(gid (), true) ]) ]
    )

module E = Expressions


let one =
  testTask "1" {
    let actual = PT2RT.Expr.toRT 0 E.one
    let expected = (1, [ RT.LoadVal(0, RT.DInt64 1L) ], 0)
    return Expect.equal actual expected ""
  }

let onePlusTwo =
  testTask "1+2" {
    let actual = PT2RT.Expr.toRT 0 E.onePlusTwo

    let expected =
      (4,
       [ RT.LoadVal(
           0,
           RT.DFnVal(
             RT.NamedFn(RT.FQFnName.Builtin { name = "int64Add"; version = 0 })
           )
         )
         RT.LoadVal(1, RT.DInt64 1L)
         RT.LoadVal(2, RT.DInt64 2L)
         RT.Apply(3, 0, [], { head = 1; tail = [ 2 ] })
         RT.Return(3) ],
       3)

    return Expect.equal actual expected ""
  }

let defineAndUseVar =
  testTask "let x = true in x" {
    let actual = PT2RT.Expr.toRT 0 E.defineAndUseVar

    // TODO: re-evaluate if the 3 and 2 here cound be 2 and 1
    // PT2RT uses a register to pass the pass the 'result' of the LP deconstruction
    //, and maybe we could reduce that?
    let expected =
      (3,
       [ RT.LoadVal(0, RT.DBool true)
         RT.SetVar("x", 0) // where the 'true' is stored
         RT.GetVar(2, "x") ],
       2)

    return Expect.equal actual expected ""
  }
let boolList =
  testTask "[true, false, true]" {
    let actual = PT2RT.Expr.toRT 0 E.boolList

    let expected =
      (4,
       [ RT.LoadVal(0, RT.DList(VT.unknown, []))

         RT.LoadVal(1, RT.DBool true)
         RT.AddItemToList(0, 1)

         RT.LoadVal(2, RT.DBool false)
         RT.AddItemToList(0, 2)

         RT.LoadVal(3, RT.DBool true)
         RT.AddItemToList(0, 3) ],
       0)

    return Expect.equal actual expected ""
  }

let boolListList =
  testTask "[[true; false]; [false; true]]" {
    let actual = PT2RT.Expr.toRT 0 E.boolListList

    let expected =
      (7,
       [ // create outer list
         RT.LoadVal(0, RT.DList(VT.unknown, []))

         // first inner list
         RT.LoadVal(1, RT.DList(VT.unknown, []))
         RT.LoadVal(2, RT.DBool true)
         RT.AddItemToList(1, 2)
         RT.LoadVal(3, RT.DBool false)
         RT.AddItemToList(1, 3)
         // add it to outer
         RT.AddItemToList(0, 1)

         // second inner list
         RT.LoadVal(4, RT.DList(VT.unknown, []))
         RT.LoadVal(5, RT.DBool false)
         RT.AddItemToList(4, 5)
         RT.LoadVal(6, RT.DBool true)
         RT.AddItemToList(4, 6)
         // add it to outer
         RT.AddItemToList(0, 4) ],
       0)

    return Expect.equal actual expected ""
  }

let tests =
  testList "PT2RT" [ one; onePlusTwo; defineAndUseVar; boolList; boolListList ]
