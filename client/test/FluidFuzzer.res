open Prelude
open Fluid
open FluidTestData
open Tester
open FluidShortcuts

type fluidState = AppTypes.fluidState

// See docs/fuzzer.md for documentation on how to use this.

// ------------------
// Settings
// ------------------

// At what size do we start to print more data?
let defaultVerbosityThreshold = 20

let verbosityThreshold = ref(defaultVerbosityThreshold)

// The seed can be changed to get new test data
let initialSeed: ref<int> =
  Js.Date.now()
  |> int_of_float
  |> (n =>
    if n < 0 {
      n * -1
    } else {
      n
    })
  |> mod(1000000000)
  |> ref

// How big should the generated lists be
let defaultItemSize = 4

let itemSize = ref(defaultItemSize)

// Continue after the first seed
let continue: ref<bool> = ref(false)

// Stop after getting our first failure
let stopOnFail: ref<bool> = ref(true)

// Don't generate test cases that are too large
let maxTestSize: ref<int> = ref(10000)

let testSize: ref<int> = ref(0)

// ------------------
// Debugging
// ------------------

let toText = ast => FluidPrinter.eToHumanString(ast)

let pointerToText = (p: blankOrData): string => Pointer.toContent(p)

let debugAST = (_length: int, msg: string, e: E.t): unit => Js.log(msg ++ (":\n" ++ E.show(e)))

// if length < !verbosityThreshold then Js.log (msg ^ ":\n" ^ E.show e)

// ------------------
// Deterministic random number generator
// ------------------

/* Deterministic random number generator, where the random numbers are evenly
 * distributed across the range. */
let state = ref(1)

let setSeed = (seed: int) => state := seed

let random = (): float => {
  let xOrShiftRand = (): int => {
    // Adapted from the C version at https://en.wikipedia.org/wiki/Xorshift
    /* Javascript treats numbers as 32 bit in the presence of bitwise ops and
     * our OCaml compiler defers to Javascript for handling numbers.
     */
    state := lxor(state.contents, lsl(state.contents, 13))
    /* Using lsr instead of asr because these shifts assume unsigned ints,
     * which JS doesn't have */
    state := lxor(state.contents, lsr(state.contents, 17))
    state := lxor(state.contents, lsl(state.contents, 5))
    state.contents
  }

  let twoToNeg32 = 2.0 ** -32.0
  // Conversion from https://www.doornik.com/research/randomdouble.pdf
  float_of_int(xOrShiftRand()) *. twoToNeg32 +. 0.5
}

let range = (max: int): int => truncate(float_of_int(max) *. random())

let oneOf = (l: list<'a>): 'a => List.getAt(~index=range(List.length(l)), l) |> Option.unwrapUnsafe

// ------------------
// AST generators
// ------------------

let generateLength = maxLength => max(0, 1 + range(maxLength - 1))

let generateList = (~minSize: int, ~f: unit => 'a, ()): list<'a> => {
  // Lower the list lengths as we go so eventually the program converges
  itemSize := itemSize.contents - 1
  let s = max(minSize, itemSize.contents)
  let result = List.initialize(generateLength(s), ~f=_ => f())
  itemSize := itemSize.contents + 1
  result
}

let generateName = (): string => {
  let generateChar = (): char => oneOf(list{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'})

  generateList(~minSize=1, ~f=generateChar, ()) |> String.fromList
}

let generateString = (): string => {
  let generateChar = (): char => oneOf(list{'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K'})

  generateList(~minSize=0, ~f=generateChar, ()) |> String.fromList
}

let generateInfixName = (): string => oneOf(list{"+", "++", "-", "*", "/", "||", "&&"})

let generateFnName = (): string =>
  oneOf(list{
    "Int::add",
    "DB::set_v2",
    "HttpClient::post_v4",
    generateName(),
    "DB::getAll_v2",
    "DB::generateKey_v1",
    "Date::now_v1",
    "Date::now",
  })

let checkTestSize = (~default: 'a, f: unit => 'a) =>
  if testSize.contents >= maxTestSize.contents {
    default
  } else {
    testSize := testSize.contents + 1
    f()
  }

// Fields can only have a subset of expressions in the fieldAccess
let rec generateFieldAccessExpr' = (): FluidExpression.t =>
  oneOf(list{
    lazy var(generateName()),
    lazy fieldAccess(generateFieldAccessExpr(), generateName()),
  }) |> Lazy.force

and generateFieldAccessExpr = () => checkTestSize(~default=b, generateFieldAccessExpr')

let rec generatePattern' = (): FluidPattern.t =>
  oneOf(list{
    lazy pInt(range(500)),
    lazy pBool(random() < 0.5),
    lazy pNull(),
    lazy pConstructor(generateName(), generateList(~minSize=0, ~f=generatePattern, ())),
    lazy pVar(generateName()),
    lazy pString(generateString()),
    lazy pFloat(Positive, range(5000000), range(500000)),
    lazy pBlank(),
  }) |> Lazy.force

and generatePattern = () => checkTestSize(~default=pBlank(), generatePattern')

let rec generatePipeArgumentExpr' = (): FluidExpression.t =>
  oneOf(list{
    lazy lambda(generateList(~minSize=1, ~f=generateName, ()), generateExpr()),
    lazy b,
    lazy fn(generateName(), list{pipeTarget, ...generateList(~minSize=0, ~f=generateExpr, ())}),
    lazy binop(generateName(), pipeTarget, generateExpr()),
  }) |> Lazy.force

and generatePipeArgumentExpr = () => checkTestSize(~default=b, generatePipeArgumentExpr')

and generateExpr' = () =>
  oneOf(list{
    lazy b,
    lazy str(generateString()),
    lazy int(range(500)),
    lazy bool(random() < 0.5),
    lazy float'(Positive, range(5000000), range(500000)),
    lazy null,
    lazy var(generateName()),
    lazy partial(generateFnName(), generateExpr()),
    lazy list(generateList((), ~minSize=0, ~f=generateExpr)),
    lazy fn(generateFnName(), generateList(~minSize=0, ~f=generateExpr, ())),
    lazy rightPartial(generateInfixName(), generateExpr()),
    lazy fieldAccess(generateFieldAccessExpr(), generateName()),
    lazy lambda(generateList(~minSize=1, ~f=generateName, ()), generateExpr()),
    lazy let'(generateName(), generateExpr(), generateExpr()),
    lazy binop(generateInfixName(), generateExpr(), generateExpr()),
    lazy if'(generateExpr(), generateExpr(), generateExpr()),
    lazy constructor(generateName(), generateList(~minSize=0, ~f=generateExpr, ())),
    lazy pipe(
      generateExpr(),
      generateExpr(),
      generateList(~minSize=2, ~f=generatePipeArgumentExpr, ()),
    ),
    lazy record(generateList(~minSize=1, (), ~f=() => (generateName(), generateExpr()))),
    lazy match'(
      generateExpr(),
      generateList(~minSize=1, (), ~f=() => (generatePattern(), generateExpr())),
    ),
  }) |> Lazy.force

and generateExpr = () => checkTestSize(~default=b, generateExpr')

// ------------------
// Fuzz Test definition
// ------------------
module FuzzTest = {
  type t = {
    name: string,
    fn: E.t => (E.t, fluidState),
    check: (~testcase: E.t, ~newAST: E.t, fluidState) => bool,
    ignore: // Sometimes you know some things are broken
    E.t => bool,
  }
}

// ------------------
// Test case reduction
// ------------------

let unwrap = (id: id, ast: E.t): E.t => {
  let childOr = (exprs: list<E.t>) => List.find(exprs, ~f=e => E.toID(e) == id)

  E.postTraversal(ast, ~f=e => {
    let newExpr = switch e {
    | ELet(_, _, rhs, next) => childOr(list{rhs, next})
    | EIf(_, cond, ifexpr, elseexpr) => childOr(list{cond, ifexpr, elseexpr})
    | EBinOp(_, _, lexpr, rexpr, _) => childOr(list{lexpr, rexpr})
    | EFieldAccess(_, expr, _) => childOr(list{expr})
    | EFnCall(_, _, exprs, _) => childOr(exprs)
    | ELambda(_, _, body) => childOr(list{body})
    | EList(_, exprs) => childOr(exprs)
    | EMatch(_, mexpr, pairs) => childOr(list{mexpr, ...List.map(~f=Tuple2.second, pairs)})
    | ERecord(_, fields) => childOr(List.map(~f=Tuple2.second, fields))
    | EPipe(_, e1, e2, rest) => childOr(list{e1, e2, ...rest})
    | EConstructor(_, _, exprs) => childOr(exprs)
    | EPartial(_, _, oldExpr) => childOr(list{oldExpr})
    | ERightPartial(_, _, oldExpr) => childOr(list{oldExpr})
    | EFeatureFlag(_, _, cond, casea, caseb) => childOr(list{cond, casea, caseb})
    | _ => None
    }

    Option.unwrap(~default=e, newExpr)
  })
}

let changeStrings = (id: id, ~f: string => string, ast: E.t): E.t => {
  let fStr = (strid, str) =>
    if strid == id {
      f(str)
    } else {
      str
    }
  E.postTraversal(ast, ~f=x =>
    switch x {
    | ELet(id, name, rhs, next) => ELet(id, fStr(id, name), rhs, next)
    | EFieldAccess(id, expr, fieldname) => EFieldAccess(id, expr, fStr(id, fieldname))
    | EPartial(id, name, expr) =>
      let newName = fStr(id, name)
      if newName == "" {
        expr
      } else {
        EPartial(id, newName, expr)
      }
    | ERightPartial(id, name, expr) =>
      let newName = fStr(id, name)
      if newName == "" {
        expr
      } else {
        ERightPartial(id, newName, expr)
      }
    | EFnCall(id, name, exprs, ster) as e =>
      let newName = switch name {
      | Stdlib({module_, function, version}) =>
        FQFnName.Stdlib({
          module_: fStr(id, module_),
          function: fStr(id, function),
          version: version,
        })
      | User(name) => User(fStr(id, name))
      | Package({owner, package, module_, function, version}) =>
        FQFnName.Package({
          owner: fStr(id, owner),
          package: fStr(id, package),
          module_: fStr(id, module_),
          function: fStr(id, function),
          version: version,
        })
      }
      if FQFnName.toString(newName) == "" {
        e
      } else {
        EFnCall(id, newName, exprs, ster)
      }
    | EBinOp(id, name, lhs, rhs, ster) as e =>
      let newName = switch name {
      | {module_: None, function} =>
        ({module_: None, function: fStr(id, function)}: PT.InfixStdlibFnName.t)
      | {module_: Some(mod), function} => {module_: Some(fStr(id, mod)), function: function}
      }

      if newName.module_ == None && newName.function == "" {
        e
      } else {
        EBinOp(id, newName, lhs, rhs, ster)
      }
    | ELambda(id, names, expr) =>
      let names = List.map(names, ~f=((nid, name)) =>
        if nid == id {
          (nid, fStr(id, name))
        } else {
          (nid, name)
        }
      )

      ELambda(id, names, expr)
    | ERecord(rid, fields) => ERecord(rid, List.map(~f=((name, expr)) =>
          if id == E.toID(expr) {
            (fStr(id, name), expr)
          } else {
            (name, expr)
          }
        , fields))
    | EString(id, str) => EString(id, fStr(id, str))
    | expr => expr
    }
  )
}

let blankVarNames = (id: id, expr: E.t): E.t => changeStrings(~f=_ => "", id, expr)

let shortenNames = (id: id, expr: E.t): E.t =>
  changeStrings(~f=String.dropRight(~count=1), id, expr)

let remove = (id: id, ast: E.t): E.t => {
  let removeFromList = exprs => List.filter(exprs, ~f=e => E.toID(e) != id)
  E.postTraversal(ast, ~f=x =>
    switch x {
    | e if E.toID(e) == id => EBlank(id)
    | EFnCall(id, name, exprs, ster) => EFnCall(id, name, removeFromList(exprs), ster)
    | ELambda(id, names, expr) =>
      let names =
        names
        |> List.filter(~f=((nid, _)) => nid != id)
        |> (
          x =>
            if x == list{} {
              List.take(~count=1, names)
            } else {
              x
            }
        )

      ELambda(id, names, expr)
    | EList(id, exprs) => EList(id, removeFromList(exprs))
    | EMatch(mid, mexpr, pairs) =>
      EMatch(
        mid,
        mexpr,
        List.filter(pairs, ~f=((pattern, expr)) =>
          E.toID(expr) != id && FluidPattern.toID(pattern) != id
        ),
      )
    | ERecord(rid, fields) => ERecord(rid, List.filterMap(~f=((name, expr)) =>
          if E.toID(expr) == id {
            None
          } else {
            Some(name, expr)
          }
        , fields))
    | EPipe(id, e1, e2, rest) =>
      switch removeFromList(list{e1, e2, ...rest}) {
      | list{EBlank(_), EBinOp(id, op, EPipeTarget(ptid), rexpr, ster)}
      | list{EBinOp(id, op, EPipeTarget(ptid), rexpr, ster), EBlank(_)} =>
        EBinOp(id, op, EBlank(ptid), rexpr, ster)
      | list{EBlank(_), EFnCall(id, name, list{EPipeTarget(ptid), ...tail}, ster)}
      | list{EFnCall(id, name, list{EPipeTarget(ptid), ...tail}, ster), EBlank(_)} =>
        EFnCall(id, name, list{EBlank(ptid), ...tail}, ster)
      | list{justOne} => justOne
      | list{} => EBlank(id)
      | list{e1, e2, ...rest} => EPipe(id, e1, e2, rest)
      }
    | EConstructor(id, name, exprs) => EConstructor(id, name, removeFromList(exprs))
    | expr => expr
    }
  )
}

let simplify = (id: id, ast: E.t): E.t =>
  E.update(id, ast, ~f=x =>
    switch x {
    | EBlank(e) => EBlank(e)
    | _ => EInteger(id, 5L)
    }
  )

let reduce = (test: FuzzTest.t, ast: E.t) => {
  let runThrough = (msg, reducer, ast) => {
    let tokenIDs =
      ast |> FluidTokenizer.tokenize |> List.map(~f=(ti: T.tokenInfo) => T.tid(ti.token))

    let eIDs = ast |> E.filterMap(~f=e => Some(E.toID(e)))
    let ids =
      Belt.List.concat(tokenIDs, eIDs)
      |> List.uniqueBy(~f=ID.toString)
      |> List.mapWithIndex(~f=(i, v) => (i, v))

    let length = List.length(ids)
    let latestAST = ref(ast)
    List.forEach(ids, ~f=((idx, id)) => {
      try {
        Js.log2(msg, (idx, length, id))
        let reducedAST = reducer(id, latestAST.contents)
        if latestAST.contents == reducedAST || test.ignore(reducedAST) {
          Js.log("no change, trying next id")
        } else {
          let (newAST, newState) = test.fn(reducedAST)
          let passed = test.check(~testcase=reducedAST, ~newAST, newState)
          if passed {
            Js.log("removed the good bit, trying next id")
            debugAST(length, "started with", latestAST.contents)
            debugAST(length, "reduced", reducedAST)
            debugAST(length, "after testing", newAST)
            Js.log2("pos is", newState.newPos)
          } else {
            Js.log("Success! We've reduced and it still fails. Let's keep going!")
            debugAST(length, "started with", latestAST.contents)
            debugAST(length, "reduced", reducedAST)
            debugAST(length, "after testing", newAST)
            if length < verbosityThreshold.contents {
              Js.log2("pos is", newState.newPos)
            }
            latestAST := reducedAST
          }
        }
      } catch {
      | e => Js.log2("Exception, let's skip this one", e)
      }
      Js.log("\n")
    })
    latestAST.contents
  }

  let sentinel = int(56756756)
  let oldAST = ref(sentinel)
  let newAST = ref(ast)
  while oldAST != newAST {
    Js.log2("starting to reduce\n", toText(newAST.contents))
    oldAST := newAST.contents
    let latestAST =
      newAST.contents
      |> runThrough("simplify", simplify)
      |> runThrough("unwrapping", unwrap)
      |> runThrough("removing", remove)
      |> runThrough("blankVarNames", blankVarNames)
      |> runThrough("shortenNames", shortenNames)

    newAST := latestAST
  }
  newAST.contents
}

let generateTestCase = () => {
  testSize := 0
  generateExpr() |> FluidExpression.clone
}

// ------------------
// Driver
// ------------------
let runTest = (test: FuzzTest.t): unit =>
  try {
    let seed = ref(initialSeed.contents)
    let continue_loop = ref(true)
    Tester.describe(test.name, () =>
      while continue_loop.contents {
        let name = test.name ++ (" #" ++ string_of_int(seed.contents))
        Tester.test(name, () => {
          setSeed(seed.contents)
          let testcase = generateTestCase()
          if test.ignore(testcase) {
            Js.log2("ignoring: ", name)
            skip()
          } else {
            Js.log2("testing: ", name)
            debugAST(0, "starting with", testcase)
            let passed = switch try Some(test.fn(testcase)) catch {
            | _ => None
            } {
            | Some(newAST, newState) =>
              Js.log2("checking: ", name)
              test.check(~testcase, ~newAST, newState)
            | None => false
            }

            if passed == false {
              Js.log2("failed: ", name)
              let reduced = reduce(test, testcase)
              Js.log2("finished program:\n  ", toText(reduced))
              Js.log2("as expr:\n  ", E.show(reduced))
              Js.log2("as testcase:\n  ", FluidPrinter.eToTestcase(reduced))
              fail()
            } else {
              pass()
            }
          }
        })
        seed := seed.contents + 1
        continue_loop := continue.contents
        if stopOnFail.contents && Tester.fails() != list{} {
          exit(-1)
        }
        ()
      }
    )
  } catch {
  | _ => ()
  }
