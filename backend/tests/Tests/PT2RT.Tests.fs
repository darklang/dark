module Tests.ProgramTypesToRuntimeTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PackageIDs = LibExecution.PackageIDs

module E = TestValues.Expressions

// TODO: consider adding an Expect.equalInstructions,
// which better points out the diffs in the lists

let t name expr expected =
  testTask name {
    let actual = PT2RT.Expr.toRT 0 expr
    return Expect.equal actual expected ""
  }

module Basic =
  let one = t "1" E.Basic.one (1, [ RT.LoadVal(0, RT.DInt64 1L) ], 0)

  let onePlusTwo =
    t
      "1+2"
      E.Basic.onePlusTwo
      (4,
       [ RT.LoadVal(
           0,
           RT.DFnVal(
             RT.NamedFn(RT.FQFnName.Builtin { name = "int64Add"; version = 0 })
           )
         )
         RT.LoadVal(1, RT.DInt64 1L)
         RT.LoadVal(2, RT.DInt64 2L)
         RT.Apply(3, 0, [], { head = 1; tail = [ 2 ] }) ],
       3)

  let tests = testList "Basic" [ one; onePlusTwo ]


module Let =
  let simple =
    t
      "let x = true\n x"
      E.Let.simple
      (2,
       [ RT.LoadVal(0, RT.DBool true)
         RT.CheckLetPatternAndExtractVars(0, RT.LPVariable "x")
         RT.GetVar(1, "x") ],
       1)

  let tuple =
    t
      "let (x, y) = (1, 2)\nx"
      E.Let.tuple
      (4,
       [ // register 0 isn't exposed, but used to temporarily store the tuple
         RT.LoadVal(1, RT.DInt64 1L)
         RT.LoadVal(2, RT.DInt64 2L)
         RT.CreateTuple(0, 1, 2, [])

         RT.CheckLetPatternAndExtractVars(
           0,
           RT.LPTuple(RT.LPVariable "x", RT.LPVariable "y", [])
         )

         RT.GetVar(3, "x") ],
       3)

  let tupleNested =
    t
      "let (a, (b, c)) = (1, (2, 3)) in b"
      E.Let.tupleNested
      (6,
       [ // reserve 0 for outer tuple
         RT.LoadVal(1, RT.DInt64 1L)
         // reserve 2 for inner tuple
         RT.LoadVal(3, RT.DInt64 2L)
         RT.LoadVal(4, RT.DInt64 3L)
         RT.CreateTuple(2, 3, 4, []) // create inner tuple
         RT.CreateTuple(0, 1, 2, []) // create outer tuple
         RT.CheckLetPatternAndExtractVars(
           0,
           RT.LPTuple(
             RT.LPVariable "a",
             RT.LPTuple(RT.LPVariable "b", RT.LPVariable "c", []),
             []
           )
         )
         RT.GetVar(5, "b") ],
       5)

  let tests = testList "Let" [ simple; tuple; tupleNested ]


module List =
  let simple =
    t
      "[true, false, true]"
      E.List.simple
      (4,
       [ RT.LoadVal(1, RT.DBool true)
         RT.LoadVal(2, RT.DBool false)
         RT.LoadVal(3, RT.DBool true)
         RT.CreateList(0, [ 1; 2; 3 ]) ],
       0)

  let nested =
    t
      "[[true; false]; [false; true]]"
      E.List.nested
      (7,
       [ // first inner list
         RT.LoadVal(2, RT.DBool true)
         RT.LoadVal(3, RT.DBool false)
         RT.CreateList(1, [ 2; 3 ])

         // second inner list
         RT.LoadVal(5, RT.DBool false)
         RT.LoadVal(6, RT.DBool true)
         RT.CreateList(4, [ 5; 6 ])

         // outer list
         RT.CreateList(0, [ 1; 4 ]) ],
       0)

  let mixed =
    t
      "[1, true]"
      E.List.mixed
      (3,
       [ RT.LoadVal(1, RT.DInt64 1L)
         RT.LoadVal(2, RT.DBool true)
         RT.CreateList(0, [ 1; 2 ]) ],
       0)

  let tests = testList "Lists" [ simple; nested; mixed ]


module String =
  let simple =
    t
      "[\"hello\"]"
      E.String.simple
      (2,
       [ RT.LoadVal(0, RT.DString "")
         RT.LoadVal(1, RT.DString "hello")
         RT.AppendString(0, 1) ],
       0)

  let withInterpolation =
    t
      "[let x = \"world\"\n$\"hello {x}\"]"
      E.String.withInterpolation
      (5,
       [ RT.LoadVal(0, RT.DString "")
         RT.LoadVal(1, RT.DString ", world")
         RT.AppendString(0, 1)

         RT.CheckLetPatternAndExtractVars(0, RT.LPVariable "x")

         RT.LoadVal(2, RT.DString "")
         RT.LoadVal(3, RT.DString "hello")
         RT.AppendString(2, 3)

         RT.GetVar(4, "x")
         RT.AppendString(2, 4) ],
       2)

  let tests = testList "String" [ simple; withInterpolation ]


module Dict =
  let empty = t "Dict {}" E.Dict.empty (1, [ RT.CreateDict(0, []) ], 0)

  let simple =
    t
      "Dict { t: true}"
      E.Dict.simple
      (2, [ RT.LoadVal(1, RT.DBool true); RT.CreateDict(0, [ ("key", 1) ]) ], 0)

  let multEntries =
    t
      "Dict {t: true; f: false}"
      E.Dict.multEntries
      (3,
       [ RT.LoadVal(1, RT.DBool true)
         RT.LoadVal(2, RT.DBool false)
         RT.CreateDict(0, [ ("t", 1); ("f", 2) ]) ],
       0)

  let dupeKey =
    t
      "Dict {t: true; f: false; t: true}"
      E.Dict.dupeKey
      (4,
       [ RT.LoadVal(1, RT.DBool true)
         RT.LoadVal(2, RT.DBool false)
         RT.LoadVal(3, RT.DBool false)
         RT.CreateDict(0, [ ("t", 1); ("f", 2); ("t", 3) ]) ],
       0)

  let tests = testList "Dict" [ empty; simple; multEntries; dupeKey ]


module If =
  let gotoThenBranch =
    t
      "if true then 1 else 2"
      E.If.gotoThenBranch
      (4,
       [ // reserve register 0 for the result

         // cond
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


  let gotoElseBranch =
    t
      "if false then 1 else 2"
      E.If.gotoElseBranch
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

  let elseMissing =
    t
      "if false then 1"
      E.If.elseMissing
      (3,
       [ RT.LoadVal(0, RT.DUnit)
         RT.LoadVal(1, RT.DBool false)
         RT.JumpByIfFalse(2, 1)
         RT.LoadVal(2, RT.DInt64 1L)
         RT.CopyVal(0, 2) ],
       0)

  let tests = testList "If" [ gotoThenBranch; gotoElseBranch; elseMissing ]


module Tuples =
  let two =
    t
      "(false, true)"
      E.Tuples.two
      (3,
       [ RT.LoadVal(1, RT.DBool false)
         RT.LoadVal(2, RT.DBool true)
         RT.CreateTuple(0, 1, 2, []) ],
       0)

  let three =
    t
      "(false, true, false)"
      E.Tuples.three
      (4,
       [ RT.LoadVal(1, RT.DBool false)
         RT.LoadVal(2, RT.DBool true)
         RT.LoadVal(3, RT.DBool false)
         RT.CreateTuple(0, 1, 2, [ 3 ]) ],
       0)

  let nested =
    t
      "((false, true), true, (true, false))"
      E.Tuples.nested
      (8,
       [ // 0 "reserved" for outer tuple

         // first inner tuple (1 "reserved")
         RT.LoadVal(2, RT.DBool false)
         RT.LoadVal(3, RT.DBool true)
         RT.CreateTuple(1, 2, 3, [])

         // middle value
         RT.LoadVal(4, RT.DBool true)

         // second inner tuple (5 "reserved")
         RT.LoadVal(6, RT.DBool true)
         RT.LoadVal(7, RT.DBool false)
         RT.CreateTuple(5, 6, 7, [])

         // wrap all in outer tuple
         RT.CreateTuple(0, 1, 4, [ 5 ]) ],
       0)

  let tests = testList "Tuples" [ two; three; nested ]


module Match =
  let simple =
    t
      "match true with\n| false -> \"first branch\"\n| true -> \"second branch\""
      E.Match.simple
      (4,
       [ // handle the value we're matching on
         RT.LoadVal(0, RT.DBool true)

         // FIRST BRANCH
         RT.CheckMatchPatternAndExtractVars(0, RT.MPBool false, 5)
         // rhs
         RT.LoadVal(2, RT.DString "")
         RT.LoadVal(3, RT.DString "first branch")
         RT.AppendString(2, 3)
         RT.CopyVal(1, 2)
         RT.JumpBy 7

         // SECOND BRANCH
         RT.CheckMatchPatternAndExtractVars(0, RT.MPBool true, 5)
         // rhs
         RT.LoadVal(2, RT.DString "")
         RT.LoadVal(3, RT.DString "second branch")
         RT.AppendString(2, 3)
         RT.CopyVal(1, 2)
         RT.JumpBy 1

         // handle the case where no branches match
         RT.MatchUnmatched ],
       1)

  let notMatched =
    t
      "match true with\n| false -> \"first branch\""
      E.Match.notMatched
      (4,
       [ // handle the value we're matching on
         RT.LoadVal(0, RT.DBool true)

         // FIRST BRANCH
         RT.CheckMatchPatternAndExtractVars(0, RT.MPBool false, 5)
         // rhs
         RT.LoadVal(2, RT.DString "")
         RT.LoadVal(3, RT.DString "first branch")
         RT.AppendString(2, 3)
         RT.CopyVal(1, 2)
         RT.JumpBy 1

         // handle the case where no branches match
         RT.MatchUnmatched ],
       1)

  let withVar =
    t
      "match true with\n| x -> x"
      E.Match.withVar
      (3,
       [ RT.LoadVal(0, RT.DBool true)

         RT.CheckMatchPatternAndExtractVars(0, RT.MPVariable "x", 3)
         RT.GetVar(2, "x")
         RT.CopyVal(1, 2)
         RT.JumpBy 1

         RT.MatchUnmatched ],
       1)

  let withVarAndWhenCondition =
    t
      "match 4 with\n| 1 -> \"first branch\"\n| x when x % 2 == 0 -> \"second branch\""
      E.Match.withVarAndWhenCondition
      (10,
       [ RT.LoadVal(0, RT.DInt64 4L)

         // first branch
         RT.CheckMatchPatternAndExtractVars(0, RT.MPInt64 1L, 5)
         RT.LoadVal(2, RT.DString "")
         RT.LoadVal(3, RT.DString "first branch")
         RT.AppendString(2, 3)
         RT.CopyVal(1, 2)
         RT.JumpBy 14

         // second branch
         RT.CheckMatchPatternAndExtractVars(0, RT.MPVariable "x", 12)
         RT.LoadVal(2, (RT.DFnVal(RT.NamedFn(RT.FQFnName.fqBuiltin "equals" 0))))
         RT.LoadVal(3, (RT.DFnVal(RT.NamedFn(RT.FQFnName.fqBuiltin "int64Mod" 0))))
         RT.GetVar(4, "x")
         RT.Apply(5, 3, [], NEList.ofList 4 [])
         RT.LoadVal(6, RT.DInt64 2L)
         RT.Apply(7, 2, [], NEList.ofList 5 [ 6 ])
         RT.JumpByIfFalse(5, 7)
         RT.LoadVal(8, RT.DString "")
         RT.LoadVal(9, RT.DString "second branch")
         RT.AppendString(8, 9)
         RT.CopyVal(1, 8)
         RT.JumpBy 1

         // handle the case where no branches match
         RT.MatchUnmatched ],
       1)

  let list =
    t
      "match [1, 2] with\n| [1, 2] -> \"first branch\""
      E.Match.list
      (6,
       [ // expr, whose result we store in 0
         RT.LoadVal(1, RT.DInt64 1L)
         RT.LoadVal(2, RT.DInt64 2L)
         RT.CreateList(0, [ 1; 2 ])

         // first branch
         RT.CheckMatchPatternAndExtractVars(
           0,
           RT.MPList [ RT.MPInt64 1L; RT.MPInt64 2L ],
           5
         )
         RT.LoadVal(4, RT.DString "")
         RT.LoadVal(5, RT.DString "first branch")
         RT.AppendString(4, 5)
         RT.CopyVal(3, 4)
         RT.JumpBy 1

         // handle the case where no branches match
         RT.MatchUnmatched ],
       3)

  let listCons =
    t
      "match [1, 2] with\n| 1 :: tail -> tail"
      E.Match.listCons
      (5,
       [ // expr, whose result we store in 0
         RT.LoadVal(1, RT.DInt64 1L)
         RT.LoadVal(2, RT.DInt64 2L)
         RT.CreateList(0, [ 1; 2 ])

         // first branch
         RT.CheckMatchPatternAndExtractVars(
           0,
           RT.MPListCons(RT.MPInt64 1L, RT.MPVariable "tail"),
           3
         )
         RT.GetVar(4, "tail")
         RT.CopyVal(3, 4)
         RT.JumpBy 1

         // handle the case where no branches match
         RT.MatchUnmatched ],
       3)

  let tuple =
    t
      "match (1, 2) with\n| (1, 2) -> \"first branch\""
      E.Match.tuple
      (6,
       [ // expr, whose result we store in 0
         RT.LoadVal(1, RT.DInt64 1L)
         RT.LoadVal(2, RT.DInt64 2L)
         RT.CreateTuple(0, 1, 2, [])

         // first branch
         RT.CheckMatchPatternAndExtractVars(
           0,
           RT.MPTuple(RT.MPInt64 1L, RT.MPInt64 2L, []),
           5
         )
         RT.LoadVal(4, RT.DString "")
         RT.LoadVal(5, RT.DString "first branch")
         RT.AppendString(4, 5)
         RT.CopyVal(3, 4)
         RT.JumpBy 1

         // handle the case where no branches match
         RT.MatchUnmatched ],
       3)

  let tests =
    testList
      "Match"
      [ simple
        notMatched
        withVar
        //withVarAndWhenCondition // -- disabled because of fn-calling issues
        list
        listCons
        tuple ]


let tests =
  testList
    "PT2RT"
    [ Basic.tests
      Let.tests
      List.tests
      String.tests
      Dict.tests
      If.tests
      Tuples.tests
      Match.tests ]
