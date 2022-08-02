// See docs/unittests.md for documentation on how to use this.
open Tc

module Private = {
  type success =
    | Passed
    | Failed
    | Skipped

  type t = {
    categories: list<string>,
    name: string,
    success: success,
    actual: option<string>,
    expected: option<string>,
  }

  let results: ref<list<t>> = ref(list{})

  let categories: ref<list<string>> = ref(list{})

  let runningTest: ref<string> = ref("")

  type expectation<'a> = {
    actual: 'a,
    equalityFn: ('a, 'a) => bool,
    printer: 'a => string,
  }
}

// ------------------
// Command line options
// ------------------

let pattern: ref<option<Js.Re.t>> = ref(None)

let verbose: ref<bool> = ref(false)

// ------------------
// Output / results
// ------------------

let categoryIndent = () => String.repeat(~count=List.length(Private.categories.contents), " ")

let reset = j`\\x1b[0m`

let grey = j`\\x1b[37m`

let cyan = j`\\x1b[36m`

let green = j`\\x1b[32m`

let yellow = j`\\x1b[33m`

let brightWhite = j`\\x1b[1m\\x1b[37m`

let categoryColor = (): string =>
  switch List.length(Private.categories.contents) {
  | 1 => brightWhite
  | 2 => cyan
  | 3 => yellow
  | 4 => green
  | _ => grey
  }

let testIndent = () => categoryIndent() ++ "  "

let print_category_start = (name): unit => {
  Js.log(categoryIndent() ++ (j`↣` ++ (categoryColor() ++ (" " ++ (name ++ reset)))))
  ()
}

let print_test_skip = (name): unit => Js.log(testIndent() ++ (j`🙅` ++ (" " ++ name)))

let print_test_end = (name, t: Private.t): unit => {
  open Private
  let shortName = String.slice(~from=0, ~to_=68, name)
  if t.success == Passed {
    \"@@"(Js.log, testIndent() ++ (j`✅` ++ (" " ++ shortName)))
  } else if t.success == Failed {
    let actual = Option.unwrap(~default="None", t.actual)
    let expected = Option.unwrap(~default="None", t.expected)
    Js.log(testIndent() ++ j`❌ ` ++ name)
    Js.log(testIndent() ++ `Expected: ${expected}`)
    Js.log(testIndent() ++ `  Actual: ${actual}`)
    let cont = ref(true)
    let count = min(String.length(actual), String.length(expected))
    for i in 0 to count {
      if cont.contents && String.getAt(actual, ~index=i) != String.getAt(expected, ~index=i) {
        Js.log(`Strings differ at char ${string_of_int(i)}`)
        if count > 100 {
          // Don't print for short strings
          Js.log("  Expected (before): " ++ String.slice(~from=i - 50, ~to_=i, expected))
          Js.log("  Actual   (before): " ++ String.slice(~from=i - 50, ~to_=i, actual))
          Js.log("  Expected (after): " ++ String.slice(~from=i, ~to_=i + 50, expected))
          Js.log("  Actual   (after): " ++ String.slice(~from=i, ~to_=i + 50, actual))
        }
        cont := false
      }
    }
  } else {
    print_test_skip(name)
  }
}

// ------------------
// Framework - test creation functions
// ------------------

let shouldRun = name =>
  switch pattern.contents {
  | None => true
  | Some(pattern) =>
    let fullname = String.join(~sep=" ", list{name, ...Private.categories.contents})
    Regex.contains(~re=pattern, fullname)
  }

let describe = (name: string, testFn: unit => unit): unit => {
  open Private
  categories := list{name, ...categories.contents}
  if List.length(categories.contents) <= 1 || verbose.contents {
    print_category_start(name)
  }
  testFn()
  switch categories.contents {
  | list{} => ()
  | list{_, ...rest} => categories := rest
  }
}

let test = (name: string, testFn: unit => Private.t): unit => {
  open Private
  let run = shouldRun(name)
  if run {
    runningTest := name
  } else if verbose.contents {
    print_test_skip(name)
  }
  let result = if run {
    try testFn() catch {
    | e =>
      let error = switch e {
      | Failure(msg) => "Failure: " ++ msg
      | _ => Printexc.to_string(e)
      }

      {
        categories: categories.contents,
        name: runningTest.contents,
        success: Failed,
        actual: Some(error),
        expected: None,
      }
    }
  } else {
    {
      categories: categories.contents,
      name: runningTest.contents,
      success: Skipped,
      actual: None,
      expected: None,
    }
  }

  if run && (result.success == Failed || verbose.contents) {
    print_test_end(name, result)
  }
  results := list{result, ...results.contents}
}

let testAll = (name: string, items: list<'a>, testFn: 'a => Private.t): unit =>
  items |> List.forEach(~f=item => {
    let name' = j`$name  - $item`
    test(name', () => testFn(item))
  })

// ------------------
// Framework - test evaluation functions
// ------------------
let expect = (actual: 'a) => {
  Private.actual: actual,
  equalityFn: \"=",
  printer: \">>"(Js.Json.stringifyAny, Option.unwrapUnsafe),
}

let toEqual = (expected: 'a, e: Private.expectation<'a>) => {
  open Private
  if e.equalityFn(e.actual, expected) {
    {
      categories: categories.contents,
      name: runningTest.contents,
      success: Passed,
      actual: None,
      expected: None,
    }
  } else {
    {
      categories: categories.contents,
      name: runningTest.contents,
      success: Failed,
      actual: Some(e.printer(e.actual)),
      expected: Some(e.printer(expected)),
    }
  }
}

@ocaml.doc(" withEquality replaces the equality function used for comparing the expected
 * and actual values with the given equalityFn (it defaults to ( = )).
 *
 * Eg:
      expect actualExpr
      |> withEquality FluidExpression.testEqualIgnoringIds
      |> toEqual expectedExpr
 ")
let withEquality = (equalityFn: ('a, 'a) => bool, e: Private.expectation<'a>): Private.expectation<
  'a,
> => {...e, equalityFn: equalityFn}

let withPrinter = (printer: 'a => string, e: Private.expectation<'a>): Private.expectation<'a> => {
  ...e,
  printer: printer,
}

let pass = (): Private.t => {
  open Private
  {
    categories: categories.contents,
    name: runningTest.contents,
    success: Passed,
    actual: None,
    expected: None,
  }
}

let fail = (): Private.t => {
  open Private
  {
    categories: categories.contents,
    name: runningTest.contents,
    success: Failed,
    actual: Some("fail was called"),
    expected: None,
  }
}

let skip = (): Private.t => {
  open Private
  {
    categories: categories.contents,
    name: runningTest.contents,
    success: Skipped,
    actual: None,
    expected: None,
  }
}

// ------------------
// Announce completion
// ------------------

let successes = () => {
  open Private
  List.filter(results.contents, ~f=r => r.success == Passed)
}

let fails = () => {
  open Private
  List.filter(results.contents, ~f=r => r.success == Failed)
}

let skips = () => {
  open Private
  List.filter(results.contents, ~f=r => r.success == Skipped)
}

let finish = () => {
  open Private
  let successes = successes()
  let fails = fails()
  let skips = skips()
  let successCount = List.length(successes) |> string_of_int
  let failCount = List.length(fails) |> string_of_int
  let skipCount = List.length(skips) |> string_of_int
  Js.log("\n\n")
  if fails == list{} {
    Js.log("Passed " ++ (successCount ++ (" tests (" ++ (skipCount ++ " skipped)"))))
    exit(0)
  } else {
    Js.log("Failures:")
    fails |> List.forEach(~f=({name, _}) =>
      \"@@"(Js.log, testIndent() ++ (j`❌` ++ (" " ++ name)))
    )
    Js.log("")
    Js.log(
      "Failed " ++
      (failCount ++
      (" tests (" ++ (successCount ++ (" passed, " ++ (skipCount ++ " skipped)"))))),
    )
    exit(1)
  }
}
