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

  let simpleString : PT.Expr = PT.EString(gid (), [ PT.StringText("hello") ])

  let stringWithInterpolation : PT.Expr =
    PT.ELet(
      gid (),
      PT.LPVariable(gid (), "x"),
      PT.EString(gid (), [ PT.StringText ", world" ]),
      PT.EString(
        gid (),
        [ PT.StringText "hello"; PT.StringInterpolation(PT.EVariable(gid (), "x")) ]
      )
    )

  let dictEmpty : PT.Expr = PT.EDict(gid (), [])
  let dictSimple : PT.Expr = PT.EDict(gid (), [ "key", PT.EBool(gid (), true) ])
  let dictMultEntries : PT.Expr =
    PT.EDict(gid (), [ "t", PT.EBool(gid (), true); "f", PT.EBool(gid (), false) ])
  let dictDupeKey : PT.Expr =
    PT.EDict(
      gid (),
      [ "t", PT.EBool(gid (), true)
        "f", PT.EBool(gid (), false)
        "t", PT.EBool(gid (), false) ]
    )

  let ifGotoThenBranch : PT.Expr =
    PT.EIf(
      gid (),
      PT.EBool(gid (), true),
      PT.EInt64(gid (), 1),
      Some(PT.EInt64(gid (), 2))
    )
  let ifGotoElseBranch : PT.Expr =
    PT.EIf(
      gid (),
      PT.EBool(gid (), false),
      PT.EInt64(gid (), 1),
      Some(PT.EInt64(gid (), 2))
    )
  let ifElseMissing : PT.Expr =
    PT.EIf(gid (), PT.EBool(gid (), false), PT.EInt64(gid (), 1), None)

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
  testTask "let x = true\n x" {
    let actual = PT2RT.Expr.toRT 0 E.defineAndUseVar

    let expected =
      (2,
       [ RT.LoadVal(0, RT.DBool true)
         RT.SetVar("x", 0) // where the 'true' is stored
         RT.GetVar(1, "x") ],
       1)

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

let simpleString =
  testTask "[\"hello\"]" {
    let actual = PT2RT.Expr.toRT 0 E.simpleString

    let expected =
      (2,
       [ RT.LoadVal(0, RT.DString "")
         RT.LoadVal(1, RT.DString "hello")
         RT.AppendString(0, 1) ],
       0)

    return Expect.equal actual expected ""
  }

let stringWithInterpolation =
  testTask "[let x = \"world\"\n$\"hello {x}\"]" {
    let actual = PT2RT.Expr.toRT 0 E.stringWithInterpolation

    let expected =
      (5,
       [ RT.LoadVal(0, RT.DString "")
         RT.LoadVal(1, RT.DString ", world")
         RT.AppendString(0, 1)
         RT.SetVar("x", 0)
         RT.LoadVal(2, RT.DString "")
         RT.LoadVal(3, RT.DString "hello")
         RT.AppendString(2, 3)
         RT.GetVar(4, "x")
         RT.AppendString(2, 4) ],
       2)

    return Expect.equal actual expected ""
  }


let dictEmpty =
  testTask "Dict {}" {
    let actual = PT2RT.Expr.toRT 0 E.dictEmpty

    let expected = (1, [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty)) ], 0)

    return Expect.equal actual expected ""
  }
let dictSimple =
  testTask "Dict { t: true}" {
    let actual = PT2RT.Expr.toRT 0 E.dictSimple

    let expected =
      (2,
       [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty))
         RT.LoadVal(1, RT.DBool true)
         RT.AddDictEntry(0, "key", 1) ],
       0)

    return Expect.equal actual expected ""
  }
let dictMultEntries =
  testTask "Dict {t: true; f: false}" {
    let actual = PT2RT.Expr.toRT 0 E.dictMultEntries

    let expected =
      (3,
       [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty))
         RT.LoadVal(1, RT.DBool true)
         RT.AddDictEntry(0, "t", 1)
         RT.LoadVal(2, RT.DBool false)
         RT.AddDictEntry(0, "f", 2) ],
       0)

    return Expect.equal actual expected ""
  }
let dictDupeKey =
  testTask "Dict {t: true; f: false; t: true}" {
    let actual = PT2RT.Expr.toRT 0 E.dictDupeKey

    let expected =
      (4,
       [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty))
         RT.LoadVal(1, RT.DBool true)
         RT.AddDictEntry(0, "t", 1)
         RT.LoadVal(2, RT.DBool false)
         RT.AddDictEntry(0, "f", 2)
         RT.LoadVal(3, RT.DBool false)
         RT.AddDictEntry(0, "t", 3) ],
       0)

    return Expect.equal actual expected ""
  }

let ifGotoThenBranch =
  testTask "if true then 1 else 2" {
    let actual = PT2RT.Expr.toRT 0 E.ifGotoThenBranch

    let expected =
      (4,
       [ // cond
         RT.LoadVal(1, RT.DBool true)
         RT.JumpByIfFalse(3, 1)

         // then
         RT.LoadVal(2, RT.DInt64 1L)
         RT.CopyVal(0, 2)
         RT.JumpBy 2

         // else
         RT.LoadVal(3, RT.DInt64 2L)
         RT.CopyVal(0, 3) ],
       0)

    return Expect.equal actual expected ""
  }

let ifGotoElseBranch =
  testTask "if false then 1 else 2" {
    let actual = PT2RT.Expr.toRT 0 E.ifGotoElseBranch

    let expected =
      (4,
       [ // cond
         RT.LoadVal(1, RT.DBool false)
         RT.JumpByIfFalse(3, 1)

         // then
         RT.LoadVal(2, RT.DInt64 1L)
         RT.CopyVal(0, 2)
         RT.JumpBy 2

         // else
         RT.LoadVal(3, RT.DInt64 2L)
         RT.CopyVal(0, 3) ],
       0)

    return Expect.equal actual expected ""
  }

let ifElseMissing =
  testTask "if false then 1" {
    let actual = PT2RT.Expr.toRT 0 E.ifElseMissing

    let expected =
      (3,
       [ RT.LoadVal(0, RT.DUnit)
         RT.LoadVal(1, RT.DBool false)
         RT.JumpByIfFalse(2, 1)
         RT.LoadVal(2, RT.DInt64 1L)
         RT.CopyVal(0, 2) ],
       0)

    return Expect.equal actual expected ""
  }


let tests =
  testList
    "PT2RT"
    [ one
      onePlusTwo
      defineAndUseVar
      boolList
      boolListList
      simpleString
      stringWithInterpolation
      dictEmpty
      dictSimple
      dictMultEntries
      dictDupeKey
      ifGotoThenBranch
      ifGotoElseBranch
      ifElseMissing ]
