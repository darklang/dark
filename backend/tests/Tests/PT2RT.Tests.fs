module Tests.ProgramTypesToRuntimeTypes

open Expecto
open Prelude
open TestUtils.TestUtils

module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module VT = RT.ValueType
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module PackageIDs = LibExecution.PackageIDs

open TestUtils.PTShortcuts

// TODO: consider adding an Expect.equalInstructions,
// which better points out the diffs in the lists

module Expressions =
  let one = eInt64 1

  let onePlusTwo =
    eApply
      (PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "int64Add" 0)))
      []
      [ eInt64 1; eInt64 2 ]

  // TODO: try to use undefined variable
  // TODO: lpunit
  let letSimple = eLet (lpVar "x") (eBool true) (eVar "x")
  let letTuple =
    eLet
      (lpTuple (lpVar "x") (lpVar "y") [])
      (eTuple (eInt64 1) (eInt64 2) [])
      (eVar "x")
  /// `let (a, (b, c)) = (1, (2, 3)) in b`
  let letTupleNested =
    eLet
      (lpTuple (lpVar "a") (lpTuple (lpVar "b") (lpVar "c") []) [])
      (eTuple (eInt64 1) (eTuple (eInt64 2) (eInt64 3) []) [])
      (eVar "b")

  let boolList = eList [ eBool true; eBool false; eBool true ]

  let boolListList =
    eList [ eList [ eBool true; eBool false ]; eList [ eBool false; eBool true ] ]

  let simpleString = eStr [ strText "hello" ]

  let stringWithInterpolation =
    eLet
      (lpVar "x")
      (eStr [ strText ", world" ])
      (eStr [ strText "hello"; strInterp (eVar "x") ])

  let dictEmpty = eDict []
  let dictSimple = eDict [ "key", eBool true ]
  let dictMultEntries = eDict [ "t", eBool true; "f", eBool false ]
  let dictDupeKey = eDict [ "t", eBool true; "f", eBool false; "t", eBool false ]

  let ifGotoThenBranch = eIf (eBool true) (eInt64 1) (Some(eInt64 2))
  let ifGotoElseBranch = eIf (eBool false) (eInt64 1) (Some(eInt64 2))
  let ifElseMissing = eIf (eBool false) (eInt64 1) None

  /// (false, true)
  let tuple2 = eTuple (eBool false) (eBool true) []

  /// (false, true, false)
  let tuple3 = eTuple (eBool false) (eBool true) [ eBool false ]

  /// ((false, true), true, (true, false))
  let tupleNested =
    eTuple
      (eTuple (eBool false) (eBool true) [])
      (eBool true)
      [ eTuple (eBool true) (eBool false) [] ]

  /// match true with
  /// | false -> "first branch"
  /// | true -> "second branch"
  let matchSimple =
    eMatch
      (eBool true)
      [ { pat = PT.MPBool(gid (), false)
          whenCondition = None
          rhs = eStr [ strText "first branch" ] }
        { pat = PT.MPBool(gid (), true)
          whenCondition = None
          rhs = eStr [ strText "second branch" ] } ]

  /// match true with
  /// | false -> "first branch"
  let matchNotMatched =
    eMatch
      (eBool true)
      [ { pat = PT.MPBool(gid (), false)
          whenCondition = None
          rhs = eStr [ strText "first branch" ] } ]

  /// match true with
  /// | x -> x
  let matchWithVar =
    eMatch
      (eBool true)
      [ { pat = PT.MPVariable(gid (), "x"); whenCondition = None; rhs = eVar "x" } ]

  /// match 4 with
  /// | 1 -> "first branch"
  /// | x when x % 2 == 0 -> "second branch"
  let matchWithVarAndWhenCondition =
    eMatch
      (eInt64 4)
      [ { pat = PT.MPInt64(gid (), 1)
          whenCondition = None
          rhs = eStr [ strText "first branch" ] }
        { pat = PT.MPVariable(gid (), "x")
          // "is even"
          whenCondition =
            Some(
              eApply
                (PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "equals" 0)))
                []
                [ eApply
                    (PT.EFnName(gid (), Ok(PT.FQFnName.fqBuiltIn "int64Mod" 0)))
                    []
                    [ eVar "x" ]
                  eInt64 2 ]
            )
          rhs = eStr [ strText "second branch" ] } ]

  let matchList =
    eMatch
      (eList [ eInt64 1; eInt64 2 ])
      [ { pat = PT.MPList(gid (), [ PT.MPInt64(gid (), 1); PT.MPInt64(gid (), 2) ])
          whenCondition = None
          rhs = eStr [ strText "first branch" ] } ]
  let matchListCons =
    eMatch
      (eList [ eInt64 1; eInt64 2 ])
      [ { pat =
            PT.MPListCons(
              gid (),
              PT.MPInt64(gid (), 1),
              PT.MPVariable(gid (), "tail")
            )
          whenCondition = None
          rhs = eVar "tail" } ]
  let matchTuple =
    eMatch
      (eTuple (eInt64 1) (eInt64 2) [])
      [ { pat = PT.MPTuple(gid (), PT.MPInt64(gid (), 1), PT.MPInt64(gid (), 2), [])
          whenCondition = None
          rhs = eStr [ strText "first branch" ] } ]

module E = Expressions


let t name expr expected =
  testTask name {
    let actual = PT2RT.Expr.toRT 0 expr
    return Expect.equal actual expected ""
  }

let one = t "1" E.one (1, [ RT.LoadVal(0, RT.DInt64 1L) ], 0)


let onePlusTwo =
  t
    "1+2"
    E.onePlusTwo
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

let letSimple =
  t
    "let x = true\n x"
    E.letSimple
    (2,
     [ RT.LoadVal(0, RT.DBool true)
       RT.SetVar("x", 0) // where the 'true' is stored
       RT.GetVar(1, "x") ],
     1)

let letTuple =
  t
    "let (x, y) = (1, 2)\nx"
    E.letTuple
    (6,
     [ // register 0 isn't exposed, but used to temporarily store the tuple
       RT.LoadVal(1, RT.DInt64 1L)
       RT.LoadVal(2, RT.DInt64 2L)
       RT.CreateTuple(0, 1, 2, [])
       RT.ExtractTupleItems(0, 3, 4, [])

       RT.SetVar("x", 3)
       RT.SetVar("y", 4)

       RT.GetVar(5, "x") ],
     5)
let letTupleNested =
  t
    "let (a, (b, c)) = (1, (2, 3)) in b"
    E.letTupleNested
    (10,
     [ // reserve 0 for outer tuple
       RT.LoadVal(1, RT.DInt64 1L)
       // reserve 2 for inner tuple
       RT.LoadVal(3, RT.DInt64 2L)
       RT.LoadVal(4, RT.DInt64 3L)
       RT.CreateTuple(2, 3, 4, []) // create inner tuple
       RT.CreateTuple(0, 1, 2, []) // create outer tuple
       RT.ExtractTupleItems(0, 5, 6, []) // extract outer tuple items
       RT.SetVar("a", 5)
       RT.ExtractTupleItems(6, 7, 8, [])
       RT.SetVar("b", 7)
       RT.SetVar("c", 8)
       RT.GetVar(9, "b") ],
     9)

let boolList =
  t
    "[true, false, true]"
    E.boolList
    (4,
     [ RT.LoadVal(0, RT.DList(VT.unknown, []))

       RT.LoadVal(1, RT.DBool true)
       RT.AddItemToList(0, 1)

       RT.LoadVal(2, RT.DBool false)
       RT.AddItemToList(0, 2)

       RT.LoadVal(3, RT.DBool true)
       RT.AddItemToList(0, 3) ],
     0)

let boolListList =
  t
    "[[true; false]; [false; true]]"
    E.boolListList
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


let simpleString =
  t
    "[\"hello\"]"
    E.simpleString
    (2,
     [ RT.LoadVal(0, RT.DString "")
       RT.LoadVal(1, RT.DString "hello")
       RT.AppendString(0, 1) ],
     0)

let stringWithInterpolation =
  t
    "[let x = \"world\"\n$\"hello {x}\"]"
    E.stringWithInterpolation
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



let dictEmpty =
  t "Dict {}" E.dictEmpty (1, [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty)) ], 0)
let dictSimple =
  t
    "Dict { t: true}"
    E.dictSimple
    (2,
     [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty))
       RT.LoadVal(1, RT.DBool true)
       RT.AddDictEntry(0, "key", 1) ],
     0)
let dictMultEntries =
  t
    "Dict {t: true; f: false}"
    E.dictMultEntries
    (3,
     [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty))
       RT.LoadVal(1, RT.DBool true)
       RT.AddDictEntry(0, "t", 1)
       RT.LoadVal(2, RT.DBool false)
       RT.AddDictEntry(0, "f", 2) ],
     0)
let dictDupeKey =
  t
    "Dict {t: true; f: false; t: true}"
    E.dictDupeKey
    (4,
     [ RT.LoadVal(0, RT.DDict(VT.unknown, Map.empty))
       RT.LoadVal(1, RT.DBool true)
       RT.AddDictEntry(0, "t", 1)
       RT.LoadVal(2, RT.DBool false)
       RT.AddDictEntry(0, "f", 2)
       RT.LoadVal(3, RT.DBool false)
       RT.AddDictEntry(0, "t", 3) ],
     0)

let ifGotoThenBranch =
  t
    "if true then 1 else 2"
    E.ifGotoThenBranch
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


let ifGotoElseBranch =
  t
    "if false then 1 else 2"
    E.ifGotoElseBranch
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


let ifElseMissing =
  t
    "if false then 1"
    E.ifElseMissing
    (3,
     [ RT.LoadVal(0, RT.DUnit)
       RT.LoadVal(1, RT.DBool false)
       RT.JumpByIfFalse(2, 1)
       RT.LoadVal(2, RT.DInt64 1L)
       RT.CopyVal(0, 2) ],
     0)


let tuple2 =
  t
    "(false, true)"
    E.tuple2
    (3,
     [ RT.LoadVal(1, RT.DBool false)
       RT.LoadVal(2, RT.DBool true)
       RT.CreateTuple(0, 1, 2, []) ],
     0)

let tuple3 =
  t
    "(false, true, false)"
    E.tuple3
    (4,
     [ RT.LoadVal(1, RT.DBool false)
       RT.LoadVal(2, RT.DBool true)
       RT.LoadVal(3, RT.DBool false)
       RT.CreateTuple(0, 1, 2, [ 3 ]) ],
     0)

let tupleNested =
  t
    "((false, true), true, (true, false))"
    E.tupleNested
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

let matchSimple =
  t
    "match true with\n| false -> \"first branch\"\n| true -> \"second branch\""
    E.matchSimple
    (4,
     [ // handle the value we're matching on
       RT.LoadVal(0, RT.DBool true)

       // FIRST BRANCH
       RT.MatchValue(0, RT.MPBool false, 5)
       // rhs
       RT.LoadVal(2, RT.DString "")
       RT.LoadVal(3, RT.DString "first branch")
       RT.AppendString(2, 3)
       RT.CopyVal(1, 2)
       RT.JumpBy 7

       // SECOND BRANCH
       RT.MatchValue(0, RT.MPBool true, 5)
       // rhs
       RT.LoadVal(2, RT.DString "")
       RT.LoadVal(3, RT.DString "second branch")
       RT.AppendString(2, 3)
       RT.CopyVal(1, 2)
       RT.JumpBy 1

       // handle the case where no branches match
       RT.MatchUnmatched ],
     1)

let matchNotMatched =
  t
    "match true with\n| false -> \"first branch\""
    E.matchNotMatched
    (4,
     [ // handle the value we're matching on
       RT.LoadVal(0, RT.DBool true)

       // FIRST BRANCH
       RT.MatchValue(0, RT.MPBool false, 5)
       // rhs
       RT.LoadVal(2, RT.DString "")
       RT.LoadVal(3, RT.DString "first branch")
       RT.AppendString(2, 3)
       RT.CopyVal(1, 2)
       RT.JumpBy 1

       // handle the case where no branches match
       RT.MatchUnmatched ],
     1)

let matchWithVar =
  t
    "match true with\n| x -> x"
    E.matchWithVar
    (3,
     [ RT.LoadVal(0, RT.DBool true)

       RT.MatchValue(0, RT.MPVariable "x", 3)
       RT.GetVar(2, "x")
       RT.CopyVal(1, 2)
       RT.JumpBy 1

       RT.MatchUnmatched ],
     1)

let matchWithVarAndWhenCondition =
  t
    "match 4 with\n| 1 -> \"first branch\"\n| x when x % 2 == 0 -> \"second branch\""
    E.matchWithVarAndWhenCondition
    (10,
     [ RT.LoadVal(0, RT.DInt64 4L)

       // first branch
       RT.MatchValue(0, RT.MPInt64 1L, 5)
       RT.LoadVal(2, RT.DString "")
       RT.LoadVal(3, RT.DString "first branch")
       RT.AppendString(2, 3)
       RT.CopyVal(1, 2)
       RT.JumpBy 14

       // second branch
       RT.MatchValue(0, RT.MPVariable "x", 12)
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

let matchList =
  t
    "match [1, 2] with\n| [1, 2] -> \"first branch\""
    E.matchList
    (6,
     [ // expr, whose result we store in 0
       RT.LoadVal(0, RT.DList(VT.unknown, []))
       RT.LoadVal(1, RT.DInt64 1L)
       RT.AddItemToList(0, 1)
       RT.LoadVal(2, RT.DInt64 2L)
       RT.AddItemToList(0, 2)

       // first branch
       RT.MatchValue(0, RT.MPList [ RT.MPInt64 1L; RT.MPInt64 2L ], 5)
       RT.LoadVal(4, RT.DString "")
       RT.LoadVal(5, RT.DString "first branch")
       RT.AppendString(4, 5)
       RT.CopyVal(3, 4)
       RT.JumpBy 1

       // handle the case where no branches match
       RT.MatchUnmatched ],
     3)

let matchListCons =
  t
    "match [1, 2] with\n| 1 :: tail -> tail"
    E.matchListCons
    (5,
     [ // expr, whose result we store in 0
       RT.LoadVal(0, RT.DList(VT.unknown, []))
       RT.LoadVal(1, RT.DInt64 1L)
       RT.AddItemToList(0, 1)
       RT.LoadVal(2, RT.DInt64 2L)
       RT.AddItemToList(0, 2)

       // first branch
       RT.MatchValue(0, RT.MPListCons(RT.MPInt64 1L, RT.MPVariable "tail"), 3)
       RT.GetVar(4, "tail")
       RT.CopyVal(3, 4)
       RT.JumpBy 1

        // handle the case where no branches match
       RT.MatchUnmatched ],
     3)

let matchTuple =
  t
    "match (1, 2) with\n| (1, 2) -> \"first branch\""
    E.matchTuple
    (6,
     [ // expr, whose result we store in 0
       RT.LoadVal(1, RT.DInt64 1L)
       RT.LoadVal(2, RT.DInt64 2L)
       RT.CreateTuple(0, 1, 2, [])

        // first branch
       RT.MatchValue(0, RT.MPTuple(RT.MPInt64 1L, RT.MPInt64 2L, []), 5)
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
    "PT2RT"
    [ one
      onePlusTwo
      letSimple
      letTuple
      letTupleNested
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
      ifElseMissing
      tuple2
      tuple3
      tupleNested
      matchSimple
      matchNotMatched
      matchWithVar
      //matchWithVarAndWhenCondition // -- disabled because of fn-calling issues
      matchList
      matchListCons
      matchTuple ]
