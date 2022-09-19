open Tester
open Prelude
open Fluid
open FluidTestData
module B = BlankOr
module K = FluidKeyboard
module E = FluidExpression
open ProgramTypes.Expr
open FluidShortcuts

type fluidState = AppTypes.fluidState

@@ocaml.text("
 ************
 * Overview *
 ************
 *
 * These tests are all written in a common style:
 *
 * t \"del end of whole\" aFloat ~pos:2 del \"12~.456\" ;
 *
 * This is a test that takes the fluidExpr called aFloat, starts the caret at
 * position 2, and inputs a single delete.
 *
 * The stringified result is \"12.456\" and the caret should be in position 2
 * (indicated by the tilde in the expectation string).
 *
 * There are a handful of functions you can call to simulate input, including
 * key, keys, insert, bs, del, tab, and shiftTab. render works similarly, but
 * doesn't simulate input; it only verifies that the given expression renders
 * correctly.

 ***********
 * Options *
 ***********

 * There are a few options that can be passed to [t]. All are optional.
 *
 * pos:
 *   Sets the initial position of the caret (fluidState.newPos), which
 *   otherwise defaults to 0.
 *
 * sel:
 *   Takes a tuple and sets an initial selection (fluidState.selectionStart and
 *   newPos).  The first element in the tuple always sets selectionStart, so a
 *   RTL selection can be set by giving a \"backwards\" tuple. Eg, ~sel:(0,2)
 *   selects the first two characters left-to-right while ~sel:(2,0) selects
 *   them right-to-left.
 *
 * expectsPartial
 *   By default, tests expect zero partials in the result. When passed
 *   ~expectsPartial:true, the test will assert that the result /does/ include
 *   a partial.
 *
 * debug
 *   When you need more information about a single test, set the ~debug:true
 *   flag to see the entire AST/fluidState before and after the test.
 *
 **********************
 * Expectation String *
 **********************

 * There are also certain conventions in the display of text in the output:
 *
 *  TBlanks are displayed as `___`
 *
 *  TPlaceHolders are displayed as underscores of the original length.
 *  For example if the original is \" a : Int \" then we show  `_________`
 *
 *  TPartials are displayed as text - to detect their presence, see
 *  \"~expectsPartial:true\" above.
 *
 *  TGhostPartials are displayed as normal except their first and last
 *  characters are replaced with @ signs.
 *
 *  Other blanks (see FluidToken.isBlank) are displayed as `***` This is
 *  controlled by FluidTokenizer.toTestText.
 *
 *  Caret placement is marked with a single `~`. Selections are marked with `»`
 *  as the start of the selection and `«` as the end. This allow detecting both
 *  LTR and RTL selection.

 ***********
 * Wrapper *
 ***********

 * All test case expressions are automatically wrapped in an if statement. This
 * allows us to detect abnormalities that would otherwise be hidden by the
 * expression being the root of the AST. For example, before this we frequently
 * had tests that passed despite there being a bug in the caret placement that
 * would otherwise move the caret out-of-bounds.
 *
 * if true
 * then
 *   test-expression-tokenized-here
 * else
 *   5
 *
 * We go to great efforts to fix the indentation afterwards. However, you
 * may find that your test works without the wrapping and doesn't work
 * with it. The solution is to try it wrapped in the editor and see where
 * it goes wrong.
 *
 * If all else fails, you can disable wrapping your testcase with ~wrap:false.

 *****************
 * Feature Flags *
 *****************

 * All test cases created with `t` are automatically run inside a feature flag
 * panel as well as in the main editor. That is, they are run in the \"new code\"
 * section of the feature flag, with the flag being the root of the AST.
 *
 * The token list in the flag panel always contains more than just the tokens
 * from the test expression, since a flag panel contains the condition and
 * keywords as well:
 *
 * when true
 * enabled
 *   test-expression-tokenized-here
 *
 * As with the if/then/else wrapper, similar steps are taken to remove the
 * extra tokens and spacing in the flag panel, adjusting all positions as
 * necessary.
 *
 * If for some reason your test doesn't run successfully in both the main
 * editor and the flag panel (which is definitely a bug!), you can disable
 * running the test in the flag panel with `brokenInFF:true` until you fix the
 * behavior.
 ")

let magicIfElseWrapperPrefix = 15 // "if true\nthen\n  "

let magicFeatureFlagPrefix = 20 // "when true\nenabled\n  "

let deOption = (msg, v) =>
  switch v {
  | Some(v) => v
  | None => failwith(msg)
  }

module TestCase = {
  type t = {
    ast: FluidAST.t,
    originalExpr: FluidExpression.t,
    state: fluidState,
    ff: bool,
    wrap: bool,
    clone: bool,
    debug: bool,
  }

  let init = (
    ~wrap=true,
    ~clone=true,
    ~debug=false,
    ~pos=0,
    ~sel=None,
    ~ff=false,
    originalExpr: FluidExpression.t,
  ): t => {
    let (selectionStart, pos) = switch sel {
    | None => (None, pos)
    | Some(a, b) => (Some(a), b)
    }

    let ast = {
      let fullExpr = if ff {
        flagNew(originalExpr)
      } else if wrap {
        if'(bool(true), originalExpr, int(5))
      } else {
        originalExpr
      }

      let fullExpr = if clone {
        FluidExpression.clone(fullExpr)
      } else {
        fullExpr
      }

      FluidAST.ofExpr(fullExpr)
    }

    let state = {
      let tlid = defaultTLID
      let extraEditors = Fluid.buildFeatureFlagEditors(tlid, ast)
      let activeEditor = if ff {
        List.head(extraEditors) |> Option.unwrapUnsafe
      } else {
        MainEditor(tlid)
      }

      /* re-calculate selectionStart, pos taking into account either
       * None -> the if/else wrapper because we are testing the main editor
       * Some -> the feature flag tokenization because we're in an extra (FF) editor
       *
       * See the block comment at the top of the file for further discussion of this */
      let (origSel, origPos) = (selectionStart, pos)
      let (selectionStart, pos) = {
        let newlinesBefore = (pos: int) =>
          /* Both the main editor if/then/else wrapper and the feature flag
           * panel tokenization cause the actual tokenization of our result to
           * contain an extra indent (2 spaces) for each line in the result.
           *
           * Consider:
           *
           * if true
           * then
           *   [1, 2, 3] << test case expectation is only this line
           * else
           * 5
           *
           * or in the feature flag case:
           *
           * when true
           * enabled
           *   [1, 2, 3] << test case expectation is only this line
           *
           * In order to correctly "fix" the expected caret pos, we need to
           * count the number of newlines before that position so that we can
           * add (2 * count) to the pos.
           *
           * Importantly, we do this on the original expr
           */
          originalExpr
          |> Tokenizer.tokenize
          |> List.filter(~f=(ti: T.tokenInfo) =>
            FluidToken.isNewline(ti.token) && ti.startPos < pos
          )
          |> List.length

        if ff {
          /* if we're testing a flag panel, it ALWAYS has the extra
           * tokenization to deal with, regardless of if ~wrap was set */
          let fixup = (p: int) => p + magicFeatureFlagPrefix + newlinesBefore(p) * 2

          let selectionStart = Option.map(selectionStart, ~f=fixup)
          (selectionStart, fixup(pos))
        } else if wrap {
          // if the main editor was wrapped, remove it in the same way
          let fixup = (p: int) => p + magicIfElseWrapperPrefix + newlinesBefore(p) * 2

          let selectionStart = Option.map(selectionStart, ~f=fixup)
          (selectionStart, fixup(pos))
        } else {
          // if not in FF and not wrapped, then just leave the pos/selection alone
          (selectionStart, pos)
        }
      }

      if debug {
        Js.log(
          Printf.sprintf(
            "(sel, pos) fixed from (%s, %d) to (%s, %d)",
            origSel |> Option.map(~f=string_of_int) |> Option.unwrap(~default="None"),
            origPos,
            selectionStart |> Option.map(~f=string_of_int) |> Option.unwrap(~default="None"),
            pos,
          ),
        )
      }
      {
        ...defaultTestState,
        activeEditor: activeEditor,
        selectionStart: selectionStart,
        oldPos: pos,
        newPos: pos,
      }
    }

    {
      originalExpr: originalExpr,
      ff: ff,
      ast: ast,
      state: state,
      wrap: wrap,
      clone: clone,
      debug: debug,
    }
  }
}

module TestResult = {
  type t = {
    testcase: TestCase.t,
    resultAST: FluidAST.t,
    resultState: fluidState,
  }

  let tokenizeResult = (res: t): list<FluidToken.tokenInfo> =>
    FluidAST.toExpr(res.resultAST) |> FluidTokenizer.tokenizeForEditor(res.resultState.activeEditor)

  let containsPartials = (res: t): bool =>
    List.any(tokenizeResult(res), ~f=ti =>
      switch ti.token {
      | TRightPartial(_) | TPartial(_) | TFieldPartial(_) => true
      | _ => false
      }
    )

  let containsFnsOnRail = (res: t): bool =>
    FluidAST.filter(res.resultAST, ~f=x =>
      switch x {
      | EBinOp(_, _, _, _, Rail) | EFnCall(_, _, _, Rail) => true
      | _ => false
      }
    ) |> \"<>"(list{})

  let removeWrapperFromCaretPos = (res: t, whichMagic: int, p: int): int => {
    let endPos = ref(p - whichMagic)
    /* Account for the newlines as we find them, or else we won't know our
     * position to find the newlines correctly. There'll be extra indentation,
     * so we need to subtract those to get the pos we expect. */
    let tokens = tokenizeResult(res)
    List.forEach(tokens, ~f=ti =>
      switch ti.token {
      | TNewline(_) if endPos.contents > ti.endPos => endPos := endPos.contents - 2
      | _ => ()
      }
    )
    let last = tokens |> List.last |> deOption("last") |> (x => x.endPos)
    /* even though the wrapper allows tests to go past the start and end, it's
     * weird to test for */
    max(0, min(last, endPos.contents))
  }

  @ocaml.doc(" [pos r] returns the equivalent of fluidState.newPos from test result [r],
    * adjusted to take into account either the test wrapper (if enabled) or the
    * extra tokens in the feature flag panel (if running the test in a FF)
    *
    * See the block comment at top of file for more info about wrapping magic. ")
  let pos = (res: t): int =>
    if res.testcase.ff {
      removeWrapperFromCaretPos(res, magicFeatureFlagPrefix, res.resultState.newPos)
    } else if res.testcase.wrap {
      removeWrapperFromCaretPos(res, magicIfElseWrapperPrefix, res.resultState.newPos)
    } else {
      res.resultState.newPos
    }

  @ocaml.doc(" [selection r] returns the equivalent of fluidState.selectionStart from
    * test result [r], adjusted to take into account either the test wrapper (if
    * enabled) or the extra tokens in the feature flag panel (if running the
    * test in a FF)
    *
    * See the block comment at top of file for more info about wrapping magic. ")
  let selection = (res: t): option<int> =>
    if res.testcase.ff {
      Option.map(
        res.resultState.selectionStart,
        ~f=removeWrapperFromCaretPos(res, magicFeatureFlagPrefix),
      )
    } else if res.testcase.wrap {
      Option.map(
        res.resultState.selectionStart,
        ~f=removeWrapperFromCaretPos(res, magicIfElseWrapperPrefix),
      )
    } else {
      res.resultState.selectionStart
    }

  let toString = (res: t): string => FluidAST.toExpr(res.resultAST) |> Printer.eToTestString

  let toStringWithCaret = (res: t): string => {
    /* "pos" = the caret position if no selection
     * or the end of the selection if there is one */
    let s = toString(res)
    switch selection(res) {
    | None => String.insertAt(~value="~", ~index=pos(res), s)
    | Some(startPos) =>
      let s = String.insertAt(~value="»", ~index=startPos, s)
      let endPos = pos(res)
      /* if a LTR selection, then we'll have already inserted an extra
       * character, so need to bump the position more to account for that
       * since we use a 2-byte character, this is 2 not 1 */
      if endPos > startPos {
        String.insertAt(~value="«", ~index=endPos + 2, s)
      } else {
        String.insertAt(~value="«", ~index=endPos, s)
      }
    }
  }
}

type modifierKeys = {
  shiftKey: bool,
  altKey: bool,
  metaKey: bool,
  ctrlKey: bool,
}

let processMsg = (inputs: list<FluidTypes.Msg.inputEvent>, astInfo: ASTInfo.t): ASTInfo.t => {
  let h = FluidUtils.h(FluidAST.toExpr(astInfo.ast))
  let m = {...defaultTestModel, handlers: Handlers.fromList(list{h})}
  let astInfo = Fluid.updateAutocomplete(m, TLID.fromInt(7), astInfo)
  List.fold(inputs, ~initial=astInfo, ~f=(astInfo: ASTInfo.t, input) =>
    Fluid.updateMsg'(m, h.tlid, astInfo, FluidInputEvent(input))
  )
}

let process = (inputs: list<FluidTypes.Msg.inputEvent>, tc: TestCase.t): TestResult.t => {
  let before = Fluid.ASTInfo.make(tc.ast, tc.state)
  if tc.debug {
    Js.log("\n\n\n=============")
    Js.log("state before\n" ++ FluidUtils.debugState(tc.state))
    Js.log("expr before\n" ++ FluidAST.show(tc.ast))
    Js.log(
      "\ntoken structure before\n" ++
      FluidAST.toExpr(tc.ast)->FluidPrinter.eToStructure(~includeIDs=true),
    )
    Js.log(
      "\ntokens before\n" ++ FluidPrinter.tokensToString(Fluid.ASTInfo.activeTokenInfos(before)),
    )
  }
  let result = processMsg(inputs, before)

  if tc.debug {
    Js.log2("\n\nstate after\n", FluidUtils.debugState(result.state))
    Js.log("expr after\n" ++ FluidAST.show(result.ast))
    Js.log(
      "\ntoken structure after\n" ++
      FluidAST.toExpr(result.ast)->FluidPrinter.eToStructure(~includeIDs=true),
    )
    Js.log(
      "\ntokens after\n" ++ FluidPrinter.tokensToString(Fluid.ASTInfo.activeTokenInfos(result)),
    )
    Js.log("=============\n\n\n")
  }

  let resultAST = FluidAST.map(result.ast, ~f=x =>
    switch x {
    | EFeatureFlag(_, _, _, _, expr) if tc.ff => expr
    | EIf(_, _, expr, _) if tc.wrap => expr
    | expr if !tc.wrap => expr
    | expr => failwith("the wrapper is broken: " ++ Printer.eToTestString(expr))
    }
  )
  {TestResult.testcase: tc, resultAST: resultAST, resultState: result.state}
}

let render = (case: TestCase.t): TestResult.t => process(list{}, case)

let keypress = (~shiftHeld=false, key: K.key): FluidTypes.Msg.inputEvent => Keypress({
  key: key,
  shiftKey: shiftHeld,
  altKey: false,
  metaKey: false,
  ctrlKey: false,
})

let del = (case: TestCase.t): TestResult.t => process(list{DeleteContentForward}, case)

let bs = (case: TestCase.t): TestResult.t => process(list{DeleteContentBackward}, case)

let ctrlLeft = process(list{keypress(K.GoToStartOfWord(K.DropSelection))})

let ctrlRight = process(list{keypress(K.GoToEndOfWord(K.DropSelection))})

let shiftTab = process(list{keypress(~shiftHeld=true, K.ShiftTab)})

let space = process(list{keypress(K.Space)})

let enter = process(list{keypress(K.Enter)})

let key = (~shiftHeld=false, key: K.key, case: TestCase.t): TestResult.t =>
  process(list{keypress(~shiftHeld, key)}, case)

let keys = (~shiftHeld=false, keys: list<K.key>, case: TestCase.t): TestResult.t =>
  process(List.map(~f=keypress(~shiftHeld), keys), case)

let modkeys = (keys: list<(K.key, modifierKeys)>, case: TestCase.t): TestResult.t =>
  process(List.map(~f=((key, mods)) => FluidTypes.Msg.Keypress({
      key: key,
      shiftKey: mods.shiftKey,
      altKey: mods.altKey,
      metaKey: mods.metaKey,
      ctrlKey: mods.ctrlKey,
    }), keys), case)

let ins = (s: string, case: TestCase.t): TestResult.t => process(list{InsertText(s)}, case)

let insMany = (strings: list<string>, case: TestCase.t): TestResult.t =>
  process(List.map(strings, ~f=s => FluidTypes.Msg.InsertText(s)), case)

let inputs = (inputs: list<FluidTypes.Msg.inputEvent>, case: TestCase.t): TestResult.t =>
  process(inputs, case)

// Test expecting no partials found and an expected caret position but no selection
let t = (
  ~expectsPartial=false,
  ~expectsFnOnRail=false,
  ~brokenInFF=false,
  ~wrap=true,
  ~clone=true,
  ~debug=false,
  ~pos=0,
  ~sel=?,
  name: string,
  expr: fluidExpr,
  fn: TestCase.t => TestResult.t,
  expectedStr: string,
) => {
  let testName = (~ff=false, ()) =>
    name ++
    (if ff {
      " in FF "
    } else {
      " no FF "
    } ++
    (" - `" ++
    ((Printer.eToTestString(expr) |> Regex.replace(~re=Regex.regex("\n"), ~repl=" ")) ++
    "`")))

  let case = TestCase.init(~wrap, ~clone, ~debug, ~pos, ~sel, expr)
  test(testName(), () => {
    let res = fn(case)
    open TestResult
    expect((toStringWithCaret(res), containsPartials(res), containsFnsOnRail(res))) |> toEqual((
      expectedStr,
      expectsPartial,
      expectsFnOnRail,
    ))
  })
  // also run the same test in a feature flag editor panel, unless it's marked as not working
  if !brokenInFF {
    let case = TestCase.init(~wrap=false, ~clone, ~debug, ~pos, ~sel, ~ff=true, expr)

    test(testName(~ff=true, ()), () => {
      let res = fn(case)
      open TestResult
      expect((toStringWithCaret(res), containsPartials(res), containsFnsOnRail(res))) |> toEqual((
        expectedStr,
        expectsPartial,
        expectsFnOnRail,
      ))
    })
  }
}

@ocaml.doc(" [tStruct name ast pos inputs expected] tests if applying [inputs] to the
 * [ast] with a non-selecting state derived from [pos] produces a structure
 * that matches the [expected] structure. ")
let tStruct = (
  name: string,
  ast: FluidExpression.t,
  ~pos: int,
  inputs: list<FluidTypes.Msg.inputEvent>,
  expected: FluidExpression.t,
) =>
  test(name, () => {
    let state = {...defaultTestState, oldPos: pos, newPos: pos, selectionStart: None}

    let astInfo = Fluid.ASTInfo.make(FluidAST.ofExpr(ast), state)

    let astInfo = processMsg(inputs, astInfo)
    expect(FluidAST.toExpr(astInfo.ast))
    |> withEquality(FluidExpression.testEqualIgnoringIds)
    |> withPrinter(FluidExpression.toHumanReadable)
    |> toEqual(expected)
  })

let run = () => {
  describe("Strings", () => {
    t("insert mid string", aStr, ~pos=3, ins("c"), "\"soc~me string\"")
    t("del mid string", aStr, ~pos=3, del, "\"so~e string\"")
    t("bs mid string", aStr, ~pos=4, bs, "\"so~e string\"")
    t("insert empty string", emptyStr, ~pos=1, ins("c"), "\"c~\"")
    t("del empty string", emptyStr, ~pos=1, del, "\"~\"")
    t("del empty string from outside", emptyStr, del, "~___")
    t("bs empty string", emptyStr, ~pos=1, bs, "~___")
    t("bs outside empty string", emptyStr, ~pos=2, bs, "\"~\"")
    t("bs near-empty string", oneCharStr, ~pos=2, bs, "\"~\"")
    t("del near-empty string", oneCharStr, ~pos=1, del, "\"~\"")
    t("insert outside string is no-op", aStr, ins("c"), "~\"some string\"")
    // tStruct(
    //   "insert outside string at top-level creates left partial",
    //   aStr,
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", aStr),
    // )
    t("del outside string", aStr, del, "~\"some string\"")
    t("bs outside string", aStr, bs, "~\"some string\"")
    t("insert start of string", aStr, ~pos=1, ins("c"), "\"c~some string\"")
    t("del start of string", aStr, ~pos=1, del, "\"~ome string\"")
    t("bs start of string", aStr, ~pos=1, bs, "~\"some string\"")
    t("insert end of string", aStr, ~pos=12, ins("c"), "\"some stringc~\"")
    t("del end of string", aStr, ~pos=12, del, "\"some string~\"")
    t("bs end of string", aStr, ~pos=12, bs, "\"some strin~\"")
    t("insert after end", aStr, ~pos=13, ins("c"), "\"some string\"~")
    t("del after end of string", aStr, ~pos=13, del, "\"some string\"~")
    t("bs after end", aStr, ~pos=13, bs, "\"some string~\"")
    t("insert space in string", aStr, ~pos=3, key(K.Space), "\"so ~me string\"")
    t("del space in string", aStr, ~pos=5, del, "\"some~string\"")
    t("bs space in string", aStr, ~pos=6, bs, "\"some~string\"")
    t("final quote is swallowed", aStr, ~pos=12, ins("\""), "\"some string\"~")
    t("insert backtick in string", aStr, ~pos=1, ins("`"), "\"`~some string\"")
    t(
      "ctrl+left from mid string goes front of word in string",
      aStr,
      ~pos=12,
      ctrlLeft,
      "\"some ~string\"",
    )
    t(
      "ctrl+right from mid string goes end of word in string",
      aStr,
      ~pos=2,
      ctrlRight,
      "\"some~ string\"",
    )
    t(
      "ctrl+left from beg of string goes front of next word in string",
      aStr,
      ~pos=6,
      ctrlLeft,
      "\"~some string\"",
    )
    t("ctrl+right goes end of word in string", aStr, ~pos=5, ctrlRight, "\"some string~\"")
    t(
      "DeleteWordBackward at end of last word in string should only delete last word",
      aStr,
      ~pos=12,
      inputs(list{DeleteWordBackward}),
      "\"some ~\"",
    )
    t(
      "DeleteWordBackward at beg of last word in string should delete first word",
      aStr,
      ~pos=6,
      inputs(list{DeleteWordBackward}),
      "\"~string\"",
    )
    t(
      "DeleteWordBackward at beg of first word in string does nothing",
      aStr,
      ~pos=1,
      inputs(list{DeleteWordBackward}),
      "~\"some string\"",
    )
    t(
      "DeleteWordBackward in the middle of first word in string only deletes in front of the cursor",
      aStr,
      ~pos=3,
      inputs(list{DeleteWordBackward}),
      "\"~me string\"",
    )
    t(
      "DeleteWordForward at beg of last word in string should delete last word",
      aStr,
      ~pos=6,
      inputs(list{DeleteWordForward}),
      "\"some ~\"",
    )
    t(
      "DeleteWordForward at beg of first word deletes first word",
      aStr,
      ~pos=1,
      inputs(list{DeleteWordForward}),
      "\"~ string\"",
    )
    t(
      "DeleteWordForward in the middle of first word in string deletes to next whitespace",
      aStr,
      ~pos=3,
      inputs(list{DeleteWordForward}),
      "\"so~ string\"",
    )
    t(
      "When the entire string is selected, backspace will delete entire string, returning a blank",
      aStr,
      ~sel=(0, 13),
      inputs(list{DeleteContentBackward}),
      "~___",
    )
    t(
      "Replace text in string if text is inserted with selection",
      aStr,
      ~sel=(1, 5),
      ins("a"),
      "\"a~ string\"",
    )
    t("insert square bracket in string goes in string", aStr, ~pos=3, ins("["), "\"so[~me string\"")
    t("insert square bracket outside string wraps", aStr, ~pos=0, ins("["), "[~\"some string\"]")
  })
  describe("Multi-line Strings", () => {
    t(
      "insert into start string",
      mlStr,
      ~pos=3,
      ins("c"),
      "\"12c~3456789_abcdefghi,123456789_abcdefghi\n ," ++
      "123456789_abcdefghi,123456789_abcdefghi\n ," ++ "123456789_\"",
    )
    t(
      "insert into middle string",
      mlStr,
      ~pos=/* quote + 2 + newline + 1 */ 45,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 12c~3456789_abcdefghi,123456789_abcdefghi\n ," ++ "123456789_\"",
    )
    t(
      "insert into end string",
      mlStr,
      ~pos=/* quote + 2 + (newline+1)*2 */ 87,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 12c~3456789_\"",
    )
    t(
      "del mid start string",
      mlStr,
      ~pos=3,
      del,
      "\"12~456789_abcdefghi,123456789_abcdefghi," ++
      "1\n 23456789_abcdefghi,123456789_abcdefghi," ++ "1\n 23456789_\"",
    )
    t(
      "del mid middle string",
      mlStr,
      ~pos=45,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 12~456789_abcdefghi,123456789_abcdefghi," ++ "1\n 23456789_\"",
    )
    t(
      "del mid end string",
      mlStr,
      ~pos=87,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 12~456789_\"",
    )
    t(
      "bs mid start string",
      mlStr,
      ~pos=4,
      bs,
      "\"12~456789_abcdefghi,123456789_abcdefghi," ++
      "1\n 23456789_abcdefghi,123456789_abcdefghi," ++ "1\n 23456789_\"",
    )
    t(
      "bs mid middle string",
      mlStr,
      ~pos=46,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 12~456789_abcdefghi,123456789_abcdefghi," ++ "1\n 23456789_\"",
    )
    t(
      "bs mid end string",
      mlStr,
      ~pos=88,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 12~456789_\"",
    )
    t(
      "insert outside string is no-op",
      mlStr,
      ~pos=0,
      ins("c"),
      "~\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"",
    )
    // tStruct(
    //   "insert outside mlstring at top-level creates left partial",
    //   mlStr,
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", mlStr),
    // )
    t(
      "del outside string",
      mlStr,
      ~pos=0,
      del,
      "~\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"",
    )
    t(
      "bs outside string",
      mlStr,
      ~pos=0,
      bs,
      "~\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"",
    )
    t(
      "insert start of start string",
      mlStr,
      ~pos=1,
      ins("c"),
      "\"c~123456789_abcdefghi,123456789_abcdefghi\n ," ++
      "123456789_abcdefghi,123456789_abcdefghi\n ," ++ "123456789_\"",
    )
    t(
      "insert start of middle string",
      mlStr,
      ~pos=43,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " c~123456789_abcdefghi,123456789_abcdefghi\n ," ++ "123456789_\"",
    )
    t(
      "insert start of end string",
      mlStr,
      ~pos=83,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " c~123456789_\"",
    )
    t(
      "del start of start string",
      mlStr,
      ~pos=1,
      del,
      "\"~23456789_abcdefghi,123456789_abcdefghi," ++
      "1\n 23456789_abcdefghi,123456789_abcdefghi," ++ "1\n 23456789_\"",
    )
    t(
      "del start of middle string",
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      mlStr,
      ~pos=43,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,~\n" ++
      " 23456789_abcdefghi,123456789_abcdefghi," ++ "1\n 23456789_\"",
    )
    t(
      "del start of end string",
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      mlStr,
      ~pos=85,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,~\n" ++ " 23456789_\"",
    )
    t(
      "bs start of start string",
      mlStr,
      ~pos=1,
      bs,
      "~\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"",
    )
    t(
      "bs start of middle string",
      mlStr,
      ~pos=43,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi,~\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"",
    )
    t(
      "bs start of end string",
      mlStr,
      ~pos=85,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,~\n" ++ " 123456789_\"",
    )
    t(
      "insert end of start string",
      mlStr,
      ~pos=43,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n c~" ++
      "123456789_abcdefghi,123456789_abcdefghi\n ," ++ "123456789_\"",
    )
    t(
      "insert end of middle string",
      mlStr,
      ~pos=85,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n c~" ++ "123456789_\"",
    )
    t(
      "insert end of end string",
      mlStr,
      ~pos=95,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_c~\"",
    )
    t(
      "string converts to ml string",
      str(mlSegment),
      ~pos=41,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n c~\"",
    )
    t(
      "indented string converts to ml string",
      if'(str(mlSegment), b, b),
      ~pos=44,
      ins("c"),
      "if \"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "    c~\"\n" ++ "then\n  ___\nelse\n  ___",
    )
    t(
      "insert end of indented start string",
      if'(str(mlSegment ++ mlSegment), b, b),
      ~pos=44,
      ins("c"),
      "if \"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "    c~123456789_abcdefghi,123456789_abcdefghi\n" ++
      "    ,\"\n" ++ "then\n  ___\nelse\n  ___",
    )
    t(
      "insert end of indented end string",
      if'(str(mlSegment ++ mlSegment), b, b),
      ~pos=89,
      ins("c"),
      "if \"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "    123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "    c~\"\n" ++ "then\n  ___\nelse\n  ___",
    )
    t(
      "del end of start string",
      mlStr,
      ~pos=41,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,~\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"",
    )
    t(
      "del end of middle string",
      mlStr,
      ~pos=83,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,~\n" ++ " 123456789_\"",
    )
    t(
      "del end of end string",
      mlStr,
      ~pos=95,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_~\"",
    )
    t(
      "bs end of start string",
      mlStr,
      ~pos=41,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi" ++
      "~1\n 23456789_abcdefghi,123456789_abcdefghi," ++ "1\n 23456789_\"",
    )
    t(
      "bs end of middle string",
      mlStr,
      ~pos=83,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi" ++ "~1\n 23456789_\"",
    )
    t(
      "bs end of end string",
      mlStr,
      ~pos=95,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789~\"",
    )
    t(
      "insert after end of end string",
      mlStr,
      ~pos=96,
      ins("c"),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"~",
    )
    t(
      "del after end of end string",
      mlStr,
      ~pos=96,
      del,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"~",
    )
    t(
      "bs after end of end string",
      mlStr,
      ~pos=96,
      bs,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_~\"",
    )
    // Skipped insert, del, bs of space, as it doesn't seem interesting
    t(
      "final quote is swallowed",
      mlStr,
      ~pos=95,
      ins("\""),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      " 123456789_abcdefghi,123456789_abcdefghi,\n" ++ " 123456789_\"~",
    )
    t(
      "bs, 3 lines to 2, end",
      if'(str(mlSegment ++ mlSegment ++ "c"), b, b),
      ~pos=95,
      bs,
      "if \"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "    123456789_abcdefghi,123456789_abcdefghi,~\"\n" ++ "then\n  ___\nelse\n  ___",
    )
    t(
      "bs, 2 lines to 1, end",
      if'(str(mlSegment ++ "c"), b, b),
      ~pos=50,
      bs,
      "if \"123456789_abcdefghi,123456789_abcdefghi,~\"\n" ++ "then\n  ___\nelse\n  ___",
    )
    t(
      "del, 3 lines to 2, end",
      if'(str(mlSegment ++ (mlSegment ++ "c")), b, b),
      ~pos=94,
      del,
      "if \"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "    123456789_abcdefghi,123456789_abcdefghi,~\"\n" ++ "then\n  ___\nelse\n  ___",
    )
    t(
      "del, 2 lines to 1, end",
      if'(str(mlSegment ++ "c"), b, b),
      ~pos=49,
      del,
      "if \"123456789_abcdefghi,123456789_abcdefghi,~\"\n" ++ "then\n  ___\nelse\n  ___",
    )
    t(
      "ctrl+left at beg of start string moves to beg",
      mlStrWSpace,
      ~pos=6,
      ctrlLeft,
      "\"~123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "  123456789_ abcdefghi, 123456789_ abcdef\n" ++ " ghi,\"",
    )
    t(
      "ctrl+left at beg of middle string moves to beg",
      mlStrWSpace,
      ~pos=54,
      ctrlLeft,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "  ~123456789_ abcdefghi, 123456789_ abcdef\n" ++ " ghi,\"",
    )
    t(
      "ctrl+left at beg of end string moves to beg",
      mlStrWSpace,
      ~pos=76,
      ctrlLeft,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "  123456789_ abcdefghi, ~123456789_ abcdef\n" ++ " ghi,\"",
    )
    t(
      "ctrl+right at beg of start string moves to end",
      mlStrWSpace,
      ctrlRight,
      "\"123456789_abcdefghi,123456789_abcdefghi,~\n" ++
      "  123456789_ abcdefghi, 123456789_ abcdef\n" ++ " ghi,\"",
    )
    t(
      "ctrl+right at beg of middle string moves to end",
      mlStrWSpace,
      ~pos=46,
      ctrlRight,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "  123456789_~ abcdefghi, 123456789_ abcdef\n" ++ " ghi,\"",
    )
    t(
      "ctrl+right at beg of end string moves to end",
      mlStrWSpace,
      ~pos=76,
      ctrlRight,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "  123456789_ abcdefghi, 123456789_ abcdef~\n" ++ " ghi,\"",
    )
    t(
      "DeleteWordBackward at the end of line deletes word in front",
      mlStrWSpace,
      ~pos=83,
      inputs(list{DeleteWordBackward}),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++ "  123456789_ abcdefghi, 123456789_ ~ghi,\"",
    )
    t(
      "DeleteWordBackward at the beg of line goes to end of line above ",
      mlStrWSpace,
      ~pos=42,
      inputs(list{DeleteWordBackward}),
      "\"123456789_abcdefghi,123456789_abcdefghi,~\n" ++
      "  123456789_ abcdefghi, 123456789_ abcdef\n" ++ " ghi,\"",
    )
    t(
      "DeleteWordForward at the end of line deletes up to the next whitespace",
      mlStrWSpace,
      ~pos=83,
      inputs(list{DeleteWordForward}),
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++ "  123456789_ abcdefghi, 123456789_ abcdef~\"",
    )
    t(
      "DeleteWordForward at the beg of line deletes until the next whitespace",
      mlStrWSpace,
      ~pos=43,
      inputs(list{DeleteWordForward}),
      /* ( "\"123456789_abcdefghi,123456789_abcdefghi,\n"
       ^ "~ abcdefghi, 123456789_ abcdefghi,\"" ) ; */
      /* The non-commented version is a bit weird for caret placement,
       but matches what happens in XCode */
      "\"123456789_abcdefghi,123456789_abcdefghi,~\n" ++ "  abcdefghi, 123456789_ abcdefghi,\"",
    )
    t(
      "adding a quote at the front turns a partial into a string",
      partial("abcdefgh\"", b),
      ins("\""),
      "\"~abcdefgh\"",
    )
    t(
      "adding a quote at the back turns a partial into a string",
      partial("\"abcdefgh", b),
      ~pos=9,
      ins("\""),
      "\"abcdefgh\"~",
    )
    t(
      ~expectsPartial=true,
      "just one quote doesn't turn a partial into a string",
      partial("abcdefgh", b),
      ins("\""),
      "\"~abcdefgh",
    )
    t(
      "Replace text in multiline string if text is inserted with selection",
      mlStrWSpace,
      ~sel=(1, 73),
      inputs(list{InsertText("a")}),
      "\"a~89_ abcdefghi,\"",
    )
  })
  describe("Integers", () => {
    t("insert 0 at front ", anInt, ins("0"), "~12345")
    t("insert at end of short", aShortInt, ~pos=1, ins("2"), "12~")
    t("insert start of number", anInt, ins("5"), "5~12345")
    t("del start of number", anInt, del, "~2345")
    t("bs start of number", anInt, bs, "~12345")
    t("insert end of number", anInt, ~pos=5, ins("0"), "123450~")
    t("del end of number", anInt, ~pos=5, del, "12345~")
    t("bs end of number", anInt, ~pos=5, bs, "1234~")
    t("insert non-number at start is no-op", anInt, ins("c"), "~12345")
    // tStruct(
    //   "insert non-number without wrapper creates left partial",
    //   anInt,
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", anInt),
    // )
    t("insert number at scale", aHugeInt, ~pos=5, ins("9"), "300009~0000000000000")
    t("insert number at scale", aHugeInt, ins("9"), "9~30000000000000000")
    t("insert number at scale", aHugeInt, ins("8"), "8~300000000000000000")
    t("insert number at scale", aHugeInt, ~pos=19, ins("9"), "3000000000000000000~")
    t(
      "insert number at scale",
      oneShorterThanMax63BitInt,
      ~pos=18,
      ins("3"),
      "9223372036854775803~",
    )
    t(
      "insert number at scale",
      oneShorterThanMax63BitInt,
      ~pos=18,
      ins("4"),
      "9223372036854775804~",
    )
    t(
      "ctrl+left go to beg of int moves to beg",
      oneShorterThanMax63BitInt,
      ~pos=11,
      ctrlLeft,
      "~922337203685477580",
    )
    t(
      "ctrl+right go to end of int moves to end",
      oneShorterThanMax63BitInt,
      ~pos=11,
      ctrlRight,
      "922337203685477580~",
    )
    t(
      "DeleteWordBackward in the middle of an int deletes all the nums in front of cursor",
      oneShorterThanMax63BitInt,
      ~pos=11,
      inputs(list{DeleteWordBackward}),
      "~5477580",
    )
    t(
      "DeleteWordBackward at the end of an int deletes it all",
      oneShorterThanMax63BitInt,
      ~pos=18,
      inputs(list{DeleteWordBackward}),
      "~___",
    )
    t(
      "Replace int if inserted with selection",
      anInt,
      ~sel=(0, 4),
      inputs(list{InsertText("4")}),
      "4~5",
    )
  })
  describe("Floats", () => {
    t("insert . converts to float - end", anInt, ~pos=5, ins("."), "12345.~")
    t("insert . converts to float - middle", anInt, ~pos=3, ins("."), "123.~45")
    t("insert . converts to float - start", anInt, ~pos=0, ins("."), ".~12345")
    t("insert . converts to float - short", aShortInt, ~pos=1, ins("."), "1.~")
    t("continue after adding dot", aPartialFloat, ~pos=2, ins("2"), "1.2~")
    t("insert zero in whole - start", aFloat, ~pos=0, ins("0"), "~123.456")
    t("insert zero in whole - no whole", aFloatWithoutWhole, ~pos=0, ins("0"), "0~.1")
    t("insert int in whole - start", aFloat, ~pos=0, ins("9"), "9~123.456")
    t("insert int in whole - middle", aFloat, ~pos=1, ins("0"), "10~23.456")
    t("insert int in whole - end", aFloat, ~pos=3, ins("0"), "1230~.456")
    t("insert int in fraction - start", aFloat, ~pos=4, ins("0"), "123.0~456")
    t("insert int in fraction - middle", aFloat, ~pos=6, ins("0"), "123.450~6")
    t("insert int in fraction - end", aFloat, ~pos=7, ins("0"), "123.4560~")
    t("insert non-int in whole", aFloat, ~pos=2, ins("c"), "12~3.456")
    t("insert non-int in fraction", aFloat, ~pos=6, ins("c"), "123.45~6")
    t("del dot", aFloat, ~pos=3, del, "123~456")
    t("del dot at scale", aHugeFloat, ~pos=9, del, "123456789~123456789")
    t("del dot at limit1", maxPosIntWithDot, ~pos=16, del, "9223372036854775~807")
    t("del dot at limit2", maxPosIntPlus1WithDot, ~pos=16, del, "9223372036854775~80")
    t("del start of whole", aFloat, ~pos=0, del, "~23.456")
    t("del middle of whole", aFloat, ~pos=1, del, "1~3.456")
    t("del end of whole", aFloat, ~pos=2, del, "12~.456")
    t("del start of fraction", aFloat, ~pos=4, del, "123.~56")
    t("del middle of fraction", aFloat, ~pos=5, del, "123.4~6")
    t("del end of fraction", aFloat, ~pos=6, del, "123.45~")
    t("del dot converts to int", aFloat, ~pos=3, del, "123~456")
    t("del dot converts to int, no fraction", aPartialFloat, ~pos=1, del, "1~")
    t("bs dot", aFloat, ~pos=4, bs, "123~456")
    t("bs dot at scale", aHugeFloat, ~pos=10, bs, "123456789~123456789")
    t("bs dot at limit1", maxPosIntWithDot, ~pos=17, bs, "9223372036854775~807")
    t("bs dot at limit2", maxPosIntPlus1WithDot, ~pos=17, bs, "9223372036854775~80")
    t("bs start of whole", aFloat, ~pos=1, bs, "~23.456")
    t("bs middle of whole", aFloat, ~pos=2, bs, "1~3.456")
    t("bs end of whole", aFloat, ~pos=3, bs, "12~.456")
    t("bs start of fraction", aFloat, ~pos=5, bs, "123.~56")
    t("bs middle of fraction", aFloat, ~pos=6, bs, "123.4~6")
    t("bs end of fraction", aFloat, ~pos=7, bs, "123.45~")
    t("bs dot converts to int", aFloat, ~pos=4, bs, "123~456")
    t("bs dot converts to int, no fraction", aPartialFloat, ~pos=2, bs, "1~")
    t("continue after adding dot", aPartialFloat, ~pos=2, ins("2"), "1.2~")
    // tStruct(
    //   "insert letter at beginning of float at top-level creates left partial",
    //   aFloat,
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", aFloat),
    // )
    t("ctrl+left start of whole moves to beg", aFloat, ~pos=0, ctrlLeft, "~123.456")
    t("ctrl+left middle of whole moves to beg", aFloat, ~pos=1, ctrlLeft, "~123.456")
    t("ctrl+left end of whole moves to beg", aFloat, ~pos=2, ctrlLeft, "~123.456")
    t("ctrl+left start of fraction moves to beg", aFloat, ~pos=4, ctrlLeft, "123~.456")
    t("ctrl+left middle of fraction moves to beg", aFloat, ~pos=5, ctrlLeft, "123.~456")
    t("ctrl+left end of fraction moves to beg", aFloat, ~pos=6, ctrlLeft, "123.~456")
    t("ctrl+right start of whole moves to end", aFloat, ~pos=0, ctrlRight, "123~.456")
    t("ctrl+right middle of whole moves to end", aFloat, ~pos=1, ctrlRight, "123~.456")
    t("ctrl+right end of whole moves to end", aFloat, ~pos=2, ctrlRight, "123~.456")
    t("ctrl+right end of whole moves to end", aFloat, ~pos=3, ctrlRight, "123.~456")
    t("ctrl+right start of fraction moves to end", aFloat, ~pos=4, ctrlRight, "123.456~")
    t("ctrl+right middle of fraction moves to end", aFloat, ~pos=5, ctrlRight, "123.456~")
    t("ctrl+right end of fraction moves to end", aFloat, ~pos=6, ctrlRight, "123.456~")
    t(
      "DeleteWordBackward in the middle of an fraction deletes all the nums in front of cursor up to the .",
      aFloat,
      ~pos=6,
      inputs(list{DeleteWordBackward}),
      "123.~6",
    )
    t(
      "DeleteWordBackward in the middle of a whole deletes all the nums in front of cursor",
      aFloat,
      ~pos=2,
      inputs(list{DeleteWordBackward}),
      "~3.456",
    )
    t(
      "DeleteWordBackward in the end of a fraction deletes all the nums in up to the .",
      aFloat,
      ~pos=7,
      inputs(list{DeleteWordBackward}),
      "123.~",
    )
    t(
      "DeleteWordBackward in the end of a whole deletes all the nums in front of cursor",
      aFloat,
      ~pos=3,
      inputs(list{DeleteWordBackward}),
      "~.456",
    )
    t(
      "DeleteWordForward in the middle of an fraction deletes all the nums after the cursor",
      aFloat,
      ~pos=6,
      inputs(list{DeleteWordForward}),
      "123.45~",
    )
    t(
      "DeleteWordForward in the middle of a whole deletes all the nums in front of the .",
      aFloat,
      ~pos=2,
      inputs(list{DeleteWordForward}),
      "12~.456",
    )
    t(
      "DeleteWordForward in the end of a fraction does nothing",
      aFloat,
      ~pos=7,
      inputs(list{DeleteWordForward}),
      "123.456~",
    )
    t(
      "DeleteWordForward in the end of a whole deletes dot",
      aFloat,
      ~pos=3,
      inputs(list{DeleteWordForward}),
      "123~456",
    )
    t(
      "Replace text in float if int is inserted with selection",
      aFloat,
      ~sel=(1, 6),
      inputs(list{InsertText("4")}),
      "14~6",
    )
  })
  describe("Bools", () => {
    t("insert start of true is no-op", ~pos=0, bool(true), ins("c"), "~true")
    // tStruct(
    //   "insert start of true at top-level creates left partial",
    //   bool(true),
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", bool(true)),
    // )
    t(~expectsPartial=true, "del start of true", trueBool, del, "~rue")
    t("bs start of true", trueBool, bs, "~true")
    t(~expectsPartial=true, "insert end of true", trueBool, ~pos=4, ins("0"), "true0~")
    t("del end of true", trueBool, ~pos=4, del, "true~")
    t(~expectsPartial=true, "bs end of true", trueBool, ~pos=4, bs, "tru~")
    t(~expectsPartial=true, "insert middle of true", trueBool, ~pos=2, ins("0"), "tr0~ue")
    t(~expectsPartial=true, "del middle of true", trueBool, ~pos=2, del, "tr~e")
    t(~expectsPartial=true, "bs middle of true", trueBool, ~pos=2, bs, "t~ue")
    t("insert start of false is no-op", ~pos=0, bool(false), ins("c"), "~false")
    // tStruct(
    //   "insert start of false at top-level creates left partial",
    //   bool(false),
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", bool(false)),
    // )
    t(~expectsPartial=true, "del start of false", falseBool, del, "~alse")
    t("bs start of false", falseBool, bs, "~false")
    t(~expectsPartial=true, "insert end of false", falseBool, ~pos=5, ins("0"), "false0~")
    t("del end of false", falseBool, ~pos=5, del, "false~")
    t(~expectsPartial=true, "bs end of false", falseBool, ~pos=5, bs, "fals~")
    t(~expectsPartial=true, "insert middle of false", falseBool, ~pos=2, ins("0"), "fa0~lse")
    t(~expectsPartial=true, "del middle of false", falseBool, ~pos=2, del, "fa~se")
    t(~expectsPartial=true, "bs middle of false", falseBool, ~pos=2, bs, "f~lse")
    t("ctrl+left start of true doesnt move", trueBool, ~pos=0, ctrlLeft, "~true")
    t("ctrl+right start of true moves to beg", trueBool, ~pos=0, ctrlRight, "true~")
    t("ctrl+left middle of true moves to beg", trueBool, ~pos=2, ctrlLeft, "~true")
    t("ctrl+left end of true moves to bed", trueBool, ~pos=4, ctrlLeft, "~true")
    t("ctrl+right end of true doesnt move", trueBool, ~pos=4, ctrlRight, "true~")
    t("ctrl+right middle of true moves to end", trueBool, ~pos=2, ctrlRight, "true~")
    t("ctrl+left start of false doesnt move", falseBool, ~pos=0, ctrlLeft, "~false")
    t("ctrl+right start of false moves to end", falseBool, ctrlRight, "false~")
    t("ctrl+left end of false moves to beg", falseBool, ~pos=5, ctrlLeft, "~false")
    t("ctrl+right end of false moves to end", falseBool, ~pos=5, ctrlRight, "false~")
    t("ctrl+left middle of false moves to beg", falseBool, ~pos=2, ctrlLeft, "~false")
    t("ctrl+right middle of false moves to end", falseBool, ~pos=2, ctrlRight, "false~")
    t(
      "DeleteWordBackward at the end of a true deletes entire true",
      trueBool,
      ~pos=4,
      inputs(list{DeleteWordBackward}),
      "~___",
    )
    t(
      "DeleteWordBackward at the end of a false deletes entire false",
      falseBool,
      ~pos=5,
      inputs(list{DeleteWordBackward}),
      "~___",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward at the mid of a true deletes to beg",
      trueBool,
      ~pos=2,
      inputs(list{DeleteWordBackward}),
      "~ue",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward at the mid of a false deletes to beg",
      falseBool,
      ~pos=3,
      inputs(list{DeleteWordBackward}),
      "~se",
    )
    t(
      "DeleteWordForward at the end of a true does nothing",
      trueBool,
      ~pos=4,
      inputs(list{DeleteWordForward}),
      "true~",
    )
    t(
      "DeleteWordForward at the end of a false does nothing",
      falseBool,
      ~pos=5,
      inputs(list{DeleteWordForward}),
      "false~",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordForward at the mid of a true deletes to end",
      trueBool,
      ~pos=2,
      inputs(list{DeleteWordForward}),
      "tr~",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordForward at the mid of a false deletes to end",
      falseBool,
      ~pos=3,
      inputs(list{DeleteWordForward}),
      "fal~",
    )
  })
  describe("Nulls", () => {
    // tStruct(
    //   "insert start of null at top-level creates left partial",
    //   aNull,
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", aNull),
    // )
    t("insert start of null is no-op", aNull, ~pos=0, ins("c"), "~null")
    t(~expectsPartial=true, "del start of null", aNull, ~pos=0, del, "~ull")
    t("bs start of null", aNull, ~pos=0, bs, "~null")
    t("ctrl+left start of null doesnt move", aNull, ~pos=0, ctrlLeft, "~null")
    t("ctrl+right start of null moves to end", aNull, ~pos=0, ctrlRight, "null~")
    t(~expectsPartial=true, "insert end of null", aNull, ~pos=4, ins("0"), "null0~")
    t("del end of null", aNull, ~pos=4, del, "null~")
    t(~expectsPartial=true, "bs end of null", aNull, ~pos=4, bs, "nul~")
    t("ctrl+left end of null doesnt move", aNull, ~pos=4, ctrlLeft, "~null")
    t("ctrl+right end of null moves to beg", aNull, ~pos=4, ctrlRight, "null~")
    t(~expectsPartial=true, "insert middle of null", aNull, ~pos=2, ins("0"), "nu0~ll")
    t(~expectsPartial=true, "del middle of null", aNull, ~pos=2, del, "nu~l")
    t(~expectsPartial=true, "bs middle of null", aNull, ~pos=2, bs, "n~ll")
    t("ctrl+left middle of null moves to beg", aNull, ~pos=2, ctrlLeft, "~null")
    t("ctrl+right middle of null moves to end", aNull, ~pos=2, ctrlRight, "null~")
    t(
      "DeleteWordBackward at the end of a null deletes entire null",
      aNull,
      ~pos=4,
      inputs(list{DeleteWordBackward}),
      "~___",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward at the mid of a null deletes to beg",
      aNull,
      ~pos=2,
      inputs(list{DeleteWordBackward}),
      "~ll",
    )
    t(
      "DeleteWordForward at the end of a null does nothing",
      aNull,
      ~pos=4,
      inputs(list{DeleteWordForward}),
      "null~",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordForward at the mid of a null deletes to end",
      aNull,
      ~pos=2,
      inputs(list{DeleteWordForward}),
      "nu~",
    )
  })
  describe("Blanks", () => {
    t("insert middle of blank->string", b, ~pos=3, ins("\""), "\"~\"")
    t("del middle of blank->blank", b, ~pos=3, del, "___~")
    t("bs middle of blank->blank", b, ~pos=3, bs, "~___")
    t("ctrl+left middle of null moves to beg", b, ~pos=2, ctrlLeft, "~___")
    t("ctrl+right middle of null moves to end", b, ~pos=2, ctrlRight, "___~")
    t("insert blank->string", b, ~pos=0, ins("\""), "\"~\"")
    t("del blank->string", emptyStr, ~pos=0, del, "~___")
    t("bs blank->string", emptyStr, ~pos=1, bs, "~___")
    t("insert blank->int", b, ~pos=0, ins("5"), "5~")
    t("insert blank->int", b, ~pos=0, ins("0"), "0~")
    t("del int->blank ", five, ~pos=0, del, "~___")
    t("bs int->blank ", five, ~pos=1, bs, "~___")
    t("insert end of blank->int", b, ~pos=1, ins("5"), "5~")
    t(~expectsPartial=true, "insert partial", b, ~pos=0, ins("t"), "t~")
    t(
      "backspacing your way through a partial finishes",
      trueBool,
      ~pos=4,
      inputs(list{
        DeleteContentBackward,
        DeleteContentBackward,
        DeleteContentBackward,
        DeleteContentBackward,
        keypress(K.Left),
      }),
      "~___",
    )
    t("insert blank->space", b, ~pos=0, space, "~___")
    t(
      "DeleteWordBackward at the end of a blank does nothing",
      b,
      ~pos=3,
      inputs(list{DeleteWordBackward}),
      "~___",
    )
    t(
      "DeleteWordForward at the end of a blank does nothing",
      b,
      ~pos=3,
      inputs(list{DeleteWordForward}),
      "___~",
    )
  })
  describe("Fields", () => {
    t(~expectsPartial=true, "insert middle of fieldname", aField, ~pos=5, ins("c"), "obj.fc~ield")
    t("cant insert invalid chars fieldname", aField, ~pos=5, ins("$"), "obj.f~ield")
    t("cant insert invalid chars fieldname - hyphen", aField, ~pos=5, ins("-"), "obj.f~ield")
    t(~expectsPartial=true, "del middle of fieldname", aField, ~pos=5, del, "obj.f~eld@")
    t(~expectsPartial=true, "del fieldname", aShortField, ~pos=4, del, "obj.~***")
    t(~expectsPartial=true, "bs fieldname", aShortField, ~pos=5, bs, "obj.~***")
    t(~expectsPartial=true, "insert end of fieldname", aField, ~pos=9, ins("c"), "obj.fieldc~")
    t(~expectsPartial=true, "insert end of varname", aField, ~pos=3, ins("c"), "objc~.field")
    t(~expectsPartial=true, "insert start of fieldname", aField, ~pos=4, ins("c"), "obj.c~field")
    t(~expectsPartial=true, "insert blank fieldname", aBlankField, ~pos=4, ins("c"), "obj.c~")
    t("del fieldop with name", aShortField, ~pos=3, del, "obj~")
    t("bs fieldop with name", aShortField, ~pos=4, bs, "obj~")
    t("del fieldop with blank", aBlankField, ~pos=3, del, "obj~")
    t("bs fieldop with blank", aBlankField, ~pos=4, bs, "obj~")
    t("del fieldop in nested", aNestedField, ~pos=3, del, "obj~.field2")
    t("bs fieldop in nested", aNestedField, ~pos=4, bs, "obj~.field2")
    t(~expectsPartial=true, "add dot after variable", aVar, ~pos=8, ins("."), "variable.~***")
    t(~expectsPartial=true, "add dot after partial ", aPartialVar, ~pos=3, ins("."), "request.~***")
    t(
      ~expectsPartial=true,
      "add dot after field in object commits and adds faccess",
      let'("obj", record(list{("f1", int(1))}), partial("f1", fieldAccess(var("obj"), ""))),
      ~pos=49,
      ins("."),
      "let obj = {\n            f1 : 1\n          }\nobj.f1.~***",
    )
    t(
      ~expectsPartial=true,
      "add dot after field not in object commits and adds faccess",
      let'("obj", record(list{("f1", int(1))}), partial("f2", fieldAccess(var("obj"), ""))),
      ~pos=49,
      ins("."),
      "let obj = {\n            f1 : 1\n          }\nobj.f2.~***",
    )
    tStruct(
      "add dot after field not in object commits and adds faccess",
      let'("obj", record(list{("f1", int(1))}), partial("f2", fieldAccess(var("obj"), ""))),
      ~pos=49,
      list{InsertText(".")},
      let'(
        "obj",
        record(list{("f1", int(1))}),
        partial("", fieldAccess(fieldAccess(var("obj"), "f2"), "")),
      ),
    )
    t("insert space in blank ", aBlankField, ~pos=4, space, "obj.~***")
    t("ctrl+left in name moves to beg of name", aShortField, ~pos=2, ctrlLeft, "~obj.f")
    t("ctrl+right in name moves to end of name", aShortField, ~pos=2, ctrlRight, "obj~.f")
    t(
      "ctrl+left in beg of fieldname moves to beg of fieldname",
      aNestedField,
      ~pos=4,
      ctrlLeft,
      "~obj.field.field2",
    )
    t(
      "ctrl+right in beg of fieldname moves to the end of fieldname",
      aNestedField,
      ~pos=4,
      ctrlRight,
      "obj.field~.field2",
    )
    t(
      "ctrl+left in middle of fieldname moves to end of fieldname",
      aNestedField,
      ~pos=5,
      ctrlLeft,
      "obj.~field.field2",
    )
    t(
      "ctrl+right in middle of fieldname moves to beg of fieldname",
      aNestedField,
      ~pos=5,
      ctrlRight,
      "obj.field~.field2",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward in middle of fieldname deletes to beg of fieldname",
      aNestedField,
      ~pos=6,
      inputs(list{DeleteWordBackward}),
      "obj.~eld@@.field2",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward at end of fieldname deletes entire fieldname",
      aNestedField,
      ~pos=9,
      inputs(list{DeleteWordBackward}),
      "obj.~***@@.field2",
    )
    t(
      "DeleteWordBackward at end of dot deletes fieldname",
      aNestedField,
      ~pos=4,
      inputs(list{DeleteWordBackward}),
      "obj~.field2",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordForward in middle of fieldname deletes to end of fieldname",
      aNestedField,
      ~pos=6,
      inputs(list{DeleteWordForward}),
      "obj.fi~@l@.field2",
    )
    t(
      "DeleteWordForward at end of fieldname deletes next fieldname",
      aNestedField,
      ~pos=9,
      inputs(list{DeleteWordForward}),
      "obj.field~",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordForward at end of dot deletes fieldname",
      aNestedField,
      ~pos=4,
      inputs(list{DeleteWordForward}),
      "obj.~***@@.field2",
    )
    t(
      ~expectsPartial=true,
      ~clone=false,
      "insert dot to complete partial field",
      EPartial(gid(), "body", EFieldAccess(gid(), EVariable(fakeID1, "request"), "")),
      ~pos=11,
      ins("."),
      "request.body.~***",
    )
    t(
      ~expectsPartial=true,
      ~clone=false,
      "insert dot even when no content in the field",
      EVariable(fakeID1, "request"),
      ~pos=7,
      insMany(list{".", "."}),
      "request.body.~***",
    )
    t(
      ~expectsPartial=true,
      "bs fieldpartial character",
      partial("a", fieldAccess(b, "")),
      ~pos=5,
      bs,
      "___.~***",
    )
    t(
      ~expectsPartial=true,
      "del fieldpartial character",
      partial("a", fieldAccess(b, "")),
      ~pos=4,
      del,
      "___.~***",
    )
    t(
      "commit fieldpartial on enter",
      partial("u", fieldAccess(aShortVar, "")),
      ~pos=3,
      enter,
      "v.u~",
    )
    t(
      "commit fieldpartial when cursor moves elsewhere",
      partial("u", fieldAccess(aShortVar, "")),
      ~pos=3,
      keys(list{K.Left, K.Left}),
      "v~.u",
    )
  })
  describe("Functions", () => {
    t(
      ~expectsPartial=true,
      "pipe key starts partial after zero arg function",
      aFnCallWithZeroArgs,
      ~pos=11,
      ins("|"),
      "List::empty |~",
    )
    t(
      ~expectsPartial=true,
      "pipe key starts partial after zero arg function with version",
      aFnCallWithZeroArgsAndVersion,
      ~pos=13,
      ins("|"),
      "List::emptyv1 |~",
    )
    t(
      ~expectsPartial=true,
      "pipe key starts partial after piped function with one arg",
      aPipeWithFilledFunction,
      ~pos=26,
      ins("|"),
      "\"hello\"\n|>String::lengthv1 |~\n",
    )
    t("space on a sep goes to next arg", aFnCall, ~pos=10, space, "Int::add 5 ~_________")
    t(~expectsPartial=true, "bs function renames", aFnCall, ~pos=8, bs, "Int::ad~@ 5 _________")
    t(
      ~expectsPartial=true,
      "deleting a function renames",
      aFnCall,
      ~pos=7,
      del,
      "Int::ad~@ 5 _________",
    )
    t(
      ~expectsPartial=true,
      "inserting in middle of function creates a partial",
      aFnCall,
      ~pos=1,
      ins("x"),
      "Ix~nt::add 5 _________",
    )
    t("insert start of function is no-op", ~pos=0, aFnCall, ins("c"), "~Int::add 5 _________")
    // tStruct(
    //   "inserting at beginning of function at top-level creates left partial",
    //   aFnCall,
    //   ~pos=0,
    //   list{InsertText("i")},
    //   leftPartial("i", aFnCall),
    // )
    t(
      ~expectsFnOnRail=true,
      "change a function keeps it on the error rail",
      partial("HttpClient::post_v4", fn(~ster=Rail, "HttpClient::get_v3", list{b, b, b, b})),
      ~pos=18,
      enter,
      "HttpClient::postv4 ~______________ ____________ ______________ ________________",
    )
    t(
      ~expectsFnOnRail=false,
      "change a function keeps it off the error rail",
      partial("HttpClient::post_v4", fn(~ster=NoRail, "HttpClient::get_v3", list{b, b, b, b})),
      ~pos=18,
      enter,
      "HttpClient::postv4 ~______________ ____________ ______________ ________________",
    )
    t(
      ~expectsFnOnRail=false,
      "change a function to one not allowed does not stay on error rail",
      partial("Int::add", fn(~ster=Rail, "HttpClient::get_v3", list{b, b, b, b})),
      ~pos=8,
      enter,
      "Int::add ~_________ _________",
    )
    t(
      ~expectsFnOnRail=true,
      "changing a default-off to a default-on goes onto the rail",
      partial("HttpClient::post_v4", fn(~ster=NoRail, "Int::add", list{b, b})),
      ~pos=18,
      enter,
      "HttpClient::postv4 ~______________ ____________ ______________ ________________",
    )
    t(
      ~expectsFnOnRail=false,
      "changing a default-on to a default-off does not stay onto the rail",
      partial("Int::add", fn(~ster=Rail, "HttpClient::get_v3", list{b, b, b, b})),
      ~pos=8,
      enter,
      "Int::add ~_________ _________",
    )
    t(
      "renaming a function maintains unaligned params in let scope",
      partial("Int::", fn(~mod="Int", "add", list{five, six})),
      ~pos=5,
      inputs(list{InsertText("s"), InsertText("q"), keypress(K.Enter)}),
      "let b = 6\nInt::sqrt ~5",
    )
    t(
      "renaming a function doesn't maintain unaligned params if they're already set to variables",
      partial("Int::", fn(~mod="Int", "add", list{var("a"), var("b")})),
      ~pos=5,
      inputs(list{InsertText("s"), InsertText("q"), keypress(K.Enter)}),
      "Int::sqrt ~a",
    )
    t(
      "renaming a function doesn't maintain unaligned params if they're not set (blanks)",
      partial("Int::", fn(~mod="Int", "add", list{b, b})),
      ~pos=5,
      inputs(list{InsertText("s"), InsertText("q"), keypress(K.Enter)}),
      "Int::sqrt ~_________",
    )
    t(
      "renaming a function maintains aligned params to correct positions",
      partial(
        "HttpClient::",
        fn(
          ~mod="HttpClient",
          "get",
          ~version=3,
          list{EString(gid(), "someurl"), emptyRecord, emptyRecord},
        ),
      ),
      ~pos=12,
      inputs(list{
        InsertText("p"),
        InsertText("o"),
        InsertText("s"),
        InsertText("t"),
        keypress(K.Enter),
      }),
      "HttpClient::postv4 ~\"someurl\" ____________ {} {}",
    )
    /* TODO: functions are not implemented fully. I deld bs and
     * del because we were switching to partials, but this isn't
     * implemented. Some tests we need:
     * myFunc arg1 arg2, 6 => Backspace => myFun arg1 arg2, with a ghost and a partial.
     * same with del */
    t(
      ~expectsPartial=true,
      "del on function with version",
      aFnCallWithVersion,
      ~pos=11,
      del,
      "DB::getAll~@@ ___________________",
    )
    t(
      ~expectsPartial=true,
      "bs on function with version",
      aFnCallWithVersion,
      ~pos=12,
      bs,
      "DB::getAll~@@ ___________________",
    )
    t(
      ~expectsPartial=true,
      "del on function with version in between the version and function name",
      aFnCallWithVersion,
      ~pos=10,
      del,
      "DB::getAll~@@ ___________________",
    )
    t(
      ~expectsPartial=true,
      "bs on function with version in between the version and function name",
      aFnCallWithVersion,
      ~pos=10,
      bs,
      "DB::getAl~@v@ ___________________",
    )
    t(
      ~expectsPartial=true,
      "del on function with version in function name",
      aFnCallWithVersion,
      ~pos=7,
      del,
      "DB::get~ll@v@ ___________________",
    )
    t(
      ~expectsPartial=true,
      "bs on function with version in function name",
      aFnCallWithVersion,
      ~pos=8,
      bs,
      "DB::get~ll@v@ ___________________",
    )
    t(
      "adding function with version goes to the right place",
      b,
      ~pos=0,
      inputs(list{InsertText("d"), InsertText("b"), keypress(K.Enter)}),
      "DB::getAllv1 ~___________________",
    )
    t(
      "backspacing a fn arg's separator goes to the right place",
      fn(~mod="Int", "add", list{five, six}),
      ~pos=11,
      bs,
      "Int::add 5~ 6",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward in middle of function deletes to beg of function",
      aFnCallWithVersion,
      ~pos=6,
      inputs(list{DeleteWordBackward}),
      "~tAll@etAllv@ ___________________",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward in end of function version deletes to function",
      aFnCallWithVersion,
      ~pos=12,
      inputs(list{DeleteWordBackward}),
      "DB::getAll~@@ ___________________",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordForward in middle of function deletes to beg of function",
      aFnCallWithVersion,
      ~pos=6,
      inputs(list{DeleteWordForward}),
      "DB::ge~@Allv@ ___________________",
    )
    t(
      "DeleteWordForward in end of function version moves cursor to end of blank",
      aFnCallWithVersion,
      ~pos=12,
      inputs(list{DeleteWordForward}),
      "DB::getAllv1 ___________________~",
    )
    let string40 = "0123456789abcdefghij0123456789abcdefghij"
    let string80 = string40 ++ string40
    let string160 = string80 ++ string80
    t(
      "reflows work for functions",
      fn(
        "HttpClient::post_v4",
        list{str(string40), record(list{(string80, b)}), emptyRecord, emptyRecord},
      ),
      render,
      "~HttpClient::postv4\n  \"0123456789abcdefghij0123456789abcdefghij\"\n  {\n    0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij : ___\n  }\n  {}\n  {}",
    )
    t(
      "reflows work for functions with long strings",
      fn(~mod="HttpClient", "post", ~version=4, list{str(string160), b, b, b}),
      render,
      "~HttpClient::postv4\n  \"0123456789abcdefghij0123456789abcdefghij\n   0123456789abcdefghij0123456789abcdefghij\n   0123456789abcdefghij0123456789abcdefghij\n   0123456789abcdefghij0123456789abcdefghij\"\n  ____________\n  ______________\n  ________________",
    )
    t(
      ~expectsPartial=true,
      "reflows work for partials too",
      partial("TEST", fn(~mod="HttpClient", "post", ~version=4, list{str(string160), b, b, b})),
      render,
      "~TEST@lient::postv@\n  \"0123456789abcdefghij0123456789abcdefghij\n   0123456789abcdefghij0123456789abcdefghij\n   0123456789abcdefghij0123456789abcdefghij\n   0123456789abcdefghij0123456789abcdefghij\"\n  ____________\n  ______________\n  ________________",
    )
    t(
      "reflows happen for functions whose arguments have newlines",
      fn(~mod="HttpClient", "post", ~version=4, list{emptyStr, emptyRowRecord, b, b}),
      render,
      "~HttpClient::postv4\n  \"\"\n  {\n    *** : ___\n  }\n  ______________\n  ________________",
    )
    t(
      "reflows don't happen for functions whose only newline is in the last argument",
      fn(~mod="HttpClient", "post", ~version=4, list{emptyStr, b, b, emptyRowRecord}),
      render,
      "~HttpClient::postv4 \"\" ____________ ______________ {\n                                                    *** : ___\n                                                  }",
    )
    t(
      "reflows put the caret in the right place on insert",
      ~brokenInFF=true,
      ~expectsPartial=true,
      ~wrap=false,
      {
        let justShortEnoughNotToReflow = "abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01"

        fn(
          "HttpClient::post_v4",
          list{emptyStr, emptyRecord, emptyRecord, var(justShortEnoughNotToReflow)},
        )
      },
      ~pos=120,
      ins("x"),
      "HttpClient::postv4\n  \"\"\n  {}\n  {}\n  abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01x~",
    )
    t(
      "reflows put the caret in the right place on bs",
      ~expectsPartial=true,
      ~wrap=false,
      {
        let justLongEnoughToReflow = "abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij012"

        fn(
          "HttpClient::post_v4",
          list{emptyStr, emptyRecord, emptyRecord, var(justLongEnoughToReflow)},
        )
      },
      ~pos=129,
      bs,
      "HttpClient::postv4 \"\" {} {} abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij01~",
    )
    t(
      "ctrl+left on function in middle of version moves to beg of version",
      aFnCallWithVersion,
      ~pos=11,
      ctrlLeft,
      "DB::getAll~v1 ___________________",
    )
    t(
      "ctrl+right on function in middle of version moves to end of version",
      aFnCallWithVersion,
      ~pos=11,
      ctrlRight,
      "DB::getAllv1~ ___________________",
    )
    t(
      "ctrl+left on function in middle of function name moves to beg of fn name",
      aFnCallWithVersion,
      ~pos=7,
      ctrlLeft,
      "~DB::getAllv1 ___________________",
    )
    t(
      "ctrl+right on function in middle of function name moves to end of fn name",
      aFnCallWithVersion,
      ~pos=7,
      ctrlRight,
      "DB::getAll~v1 ___________________",
    )
    t(
      "backspace after selecting a versioned 0-arg fnCall deletes all",
      ~wrap=/* wrap false because else we delete the wrapper */ false,
      fn(~mod="HttpClient", "post", ~version=4, list{}),
      ~pos=0,
      inputs(list{keypress(K.SelectAll), DeleteContentBackward}),
      "~___",
    )
  })
  describe("Binops", () => {
    t(~expectsPartial=true, "pipe key starts partial", trueBool, ~pos=4, ins("|"), "true |~")
    t(
      "pressing enter completes partial",
      trueBool,
      ~pos=4,
      inputs(list{InsertText("|"), keypress(K.Down), keypress(K.Enter)}),
      "true || ~__________",
    )
    t(
      "pressing space completes partial",
      trueBool,
      ~pos=4,
      inputs(list{InsertText("|"), keypress(K.Down), keypress(K.Space)}),
      "true || ~__________",
    )
    t(
      ~expectsPartial=true,
      "pressing plus key starts partial",
      trueBool,
      ~pos=4,
      ins("+"),
      "true +~",
    )
    t(
      ~expectsPartial=true,
      "pressing caret key starts partial",
      anInt,
      ~pos=5,
      ins("^"),
      "12345 ^~",
    )
    t(
      ~expectsPartial=true,
      "pressing plus key starts partial after string",
      aStr,
      ~pos=13,
      ins("+"),
      "\"some string\" +~",
    )
    t(
      ~expectsPartial=true,
      "pressing plus key starts partial after float",
      aFloat,
      ~pos=7,
      ins("+"),
      "123.456 +~",
    )
    t(~expectsPartial=true, "ins | starts partial after null", aNull, ~pos=4, ins("|"), "null |~")
    t(
      ~expectsPartial=true,
      "ins | starts partial after variable",
      aVar,
      ~pos=8,
      ins("|"),
      "variable |~",
    )
    t(~expectsPartial=true, "ins | starts partial after list", aList5, ~pos=3, ins("|"), "[5] |~")
    t(
      ~expectsPartial=true,
      "ins + starts partial after fieldname",
      aField,
      ~pos=9,
      ins("+"),
      "obj.field +~",
    )
    t(
      ~expectsPartial=true,
      "ins | starts partial after multiRowRecord",
      multiRowRecord,
      ~pos=23,
      ins("|"),
      "{\n  f1 : 56\n  f2 : 78\n} |~",
    )
    t(
      "pressing pipe twice then space completes partial",
      trueBool,
      ~pos=4,
      inputs(list{InsertText("|"), InsertText("|"), keypress(K.Space)}),
      "true || ~__________",
    )
    t(
      "piping into newline creates pipe",
      trueBool,
      ~pos=4,
      inputs(list{InsertText("|"), InsertText(">"), keypress(K.Space)}),
      "true\n|>~___\n",
    )
    t(
      "pressing bs to clear partial reverts for blank rhs",
      partial("|", binop("||", anInt, b)),
      ~pos=7,
      bs,
      "12345~",
    )
    t(
      "pressing bs to clear partial reverts for blank rhs, check lhs pos goes to start",
      partial("|", binop("||", b, b)),
      ~pos=12,
      bs,
      "~___",
    )
    t(
      "pressing del to clear partial reverts for blank rhs",
      partial("|", binop("||", anInt, b)),
      ~pos=6,
      del,
      "12345~",
    )
    t(
      "pressing del to clear partial reverts for blank rhs, check lhs pos goes to start",
      partial("|", binop("||", b, b)),
      ~pos=11,
      del,
      "~___",
    )
    t(
      "using bs to remove an infix with a placeholder goes to right place",
      partial("|", binop("||", b, b)),
      ~pos=12,
      bs,
      "~___",
    )
    t(
      "using bs to remove an infix with a placeholder goes to right place 2",
      partial("|", binop("||", five, b)),
      ~pos=3,
      bs,
      "5~",
    )
    t(
      "deleting binop between bools does not combine them",
      partial("|", binop("||", trueBool, falseBool)),
      ~pos=6,
      bs,
      "true~",
    )
    t(
      "pressing bs to clear rightpartial reverts for blank rhs",
      rightPartial("|", b),
      ~pos=5,
      bs,
      "~___",
    )
    t(
      "pressing bs on single digit binop deletes binop and combines rhs and lhs",
      binop("+", anInt, anInt),
      ~pos=7,
      bs,
      "12345~12345",
    )
    t(
      "using del to remove an infix with a placeholder goes to right place",
      partial("|", binop("||", b, b)),
      ~pos=11,
      del,
      "~___",
    )
    t(
      "pressing del to clear rightpartial reverts for blank rhs",
      rightPartial("|", b),
      ~pos=4,
      del,
      "~___",
    )
    t(
      "pressing del on single digit binop deletes binop and combines rhs and lhs",
      binop("+", anInt, anInt),
      ~pos=6,
      del,
      "12345~12345",
    )
    t(
      "pressing del to remove a string binop combines lhs and rhs",
      binop("++", str("five"), str("six")),
      ~pos=7,
      inputs(list{DeleteContentForward, DeleteContentForward}),
      "\"five~six\"",
    )
    t(
      "pressing backspace to remove a string binop combines lhs and rhs",
      binop("++", str("five"), str("six")),
      ~pos=9,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "\"five~six\"",
    )
    t(
      "pressing bs to remove a binop after a blank doesnt delete rhs",
      binop("++", b, str("six")),
      ~pos=15,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "~\"six\"",
    )
    t(
      "pressing bs to remove a string binop combines lhs and rhs",
      binop("++", str("one"), binop("++", str("two"), binop("++", str("three"), str("four")))),
      ~pos=17,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "\"one\" ++ \"two~three\" ++ \"four\"",
    )
    t(
      "pressing bs to remove binop before a blank doesnt entire delete rhs",
      binop("++", str("one"), binop("++", str("two"), binop("++", b, str("four")))),
      ~pos=17,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "\"one\" ++ \"two\"~ ++ \"four\"",
    )
    t(
      "pressing bs to remove binop after a blank doesnt entire delete rhs",
      binop("++", str("one"), binop("++", str("two"), binop("++", b, str("four")))),
      ~pos=33,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "\"one\" ++ \"two\" ++ ~\"four\"",
    )
    t(
      "pressing letters and numbers on a partial completes it",
      b,
      ~pos=0,
      insMany(list{"5", "+", "5"}),
      "5 + 5~",
    )
    t(
      ~expectsPartial=true,
      "pressing pipe while editing a partial works properly",
      partial("|", binop("||", anInt, anInt)),
      ~pos=7,
      ins("|"),
      "12345 ||~ 12345",
    )
    t(
      ~expectsPartial=true,
      "pressing = after < should go to partial",
      binop("<", anInt, anInt),
      ~pos=7,
      ins("="),
      "12345 <=~ 12345",
    )
    t(
      "changing binop to fn should work",
      partial("Int::add", binop("+", anInt, anInt)),
      ~pos=14,
      keys(list{K.Enter}),
      "Int::add ~12345 12345",
    )
    t(
      "changing fn to binops should work",
      partial("+", fn(~mod="Int", "add", list{anInt, anInt})),
      ~pos=1,
      keys(list{K.Enter}),
      "~12345 + 12345",
    )
    t(
      "changing binop should work",
      binop("<", anInt, anInt),
      ~pos=7,
      inputs(list{InsertText("="), keypress(K.Enter)}),
      "12345 <= ~12345",
    )
    tStruct(
      "wrapping a binop in a let with enter creates correct ast",
      binop("+", int(1), int(2)),
      ~pos=0,
      list{keypress(~shiftHeld=false, K.Enter)},
      let'("", blank(), binop("+", int(1), int(2))),
    )
    t(
      ~expectsPartial=true,
      "adding binop in `if` works",
      if'(b, b, b),
      ~pos=3,
      ins("%"),
      "if %~\nthen\n  ___\nelse\n  ___",
    )
    t(~expectsPartial=true, "show ghost partial", aFullBinOp, ~pos=8, bs, "myvar |~@ 5")
    t(
      "ctrl+left from end of < moves to front of <",
      binop("<", anInt, anInt),
      ~pos=7,
      ctrlLeft,
      "12345 ~< 12345",
    )
    t(
      "ctrl+right from end of < moves to end of second int",
      binop("<", anInt, anInt),
      ~pos=7,
      ctrlRight,
      "12345 < 12345~",
    )
    t(
      "ctrl+left from beg of < moves to front of first int",
      binop("<", anInt, anInt),
      ~pos=6,
      ctrlLeft,
      "~12345 < 12345",
    )
    t(
      "ctrl+right from beg of < moves to end of <",
      binop("<", anInt, anInt),
      ~pos=6,
      ctrlRight,
      "12345 <~ 12345",
    )
    t(
      "DeleteWordBackward in end of binop deletes binop and combines rhs and lhs",
      binop("<", anInt, anInt),
      ~pos=7,
      inputs(list{DeleteWordBackward}),
      "12345~12345",
    )
    t(
      "DeleteWordBackward in front of binop deletes first int",
      binop("<", anInt, anInt),
      ~pos=5,
      inputs(list{DeleteWordBackward}),
      "~_________ < 12345",
    )
    t(
      "DeleteWordForward in end of binop deletes second int",
      binop("<", anInt, anInt),
      ~pos=8,
      inputs(list{DeleteWordForward}),
      "12345 < ~_________",
    )
    t(
      "DeleteWordForward in front of binop deletes binop and combines rhs and lhs",
      binop("<", anInt, anInt),
      ~pos=6,
      inputs(list{DeleteWordForward}),
      "12345~12345",
    )
    // TODO bs on empty partial does something
    // TODO support del on all the bs commands
    // TODO pressing enter at the end of the partialGhost
    t(
      "pressing bs on || in binop deletes right side",
      binop("||", trueBool, falseBool),
      ~pos=7,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "true~",
    )
    t(
      "pressing bs on || in binop deletes blank on rhs",
      binop("||", falseBool, b),
      ~pos=8,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "false~",
    )
    t(
      "pressing bs on || in binop deletes blank on lhs",
      binop("||", b, falseBool),
      ~pos=13,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "~false",
    )
    t(
      "pressing bs on || in binop after blank deletes blank but rest of the lhs",
      binop("||", falseBool, binop("||", b, trueBool)),
      ~pos=22,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "false || ~true",
    )
    t(
      "pressing bs on || in binop before blank deletes blank but rest of the lhs",
      binop("||", falseBool, binop("||", b, trueBool)),
      ~pos=8,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "false~ || true",
    )
    t(
      "pressing bs on ++ binop before blank deletes blank but rest of the lhs",
      binop("+", int(10), binop("*", int(5), binop("+", b, int(10)))),
      ~pos=8,
      bs,
      "10 + 5~ + 10",
    )
    t(
      "pressing bs on ++ binop after blank deletes blank but rest of the lhs",
      binop("+", int(20), binop("*", int(1), binop("+", b, int(5)))),
      ~pos=20,
      bs,
      "20 + 1 * ~5",
    )
    t(
      "pressing bs on < binop before blank deletes blank but rest of the lhs",
      binop("<", int(20), binop("<", b, int(50))),
      ~pos=4,
      bs,
      "20~ < 50",
    )
    t(
      "pressing bs on < binop after blank deletes blank but rest of the lhs",
      binop("<", int(25), binop("<", b, int(50))),
      ~pos=16,
      bs,
      "25 < ~50",
    )
    t(
      "pressing bs on - binop before blank deletes blank but rest of the lhs",
      binop("-", int(200), binop("-", int(5), binop("*", b, int(24)))),
      ~pos=9,
      bs,
      "200 - 5~ * 24",
    )
    t(
      "pressing bs on - binop after blank deletes blank but rest of the lhs",
      binop("-", int(200), binop("-", int(5), binop("*", b, int(24)))),
      ~pos=15,
      bs,
      "200 - 5 - ~24",
    )
    t(
      "pressing bs on != binop before blank deletes blank but rest of the lhs",
      binop("!=", int(54321), binop("!=", int(21), binop("!=", b, int(5)))),
      ~pos=14,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "54321 != 21~ != 5",
    )
    t(
      "pressing bs on != binop after blank deletes blank but rest of the lhs",
      binop("!=", int(54321), binop("!=", int(21), binop("!=", b, int(5)))),
      ~pos=21,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "54321 != 21 != ~5",
    )
    t(
      "pressing bs on != binop combines lhs and rhs string",
      binop("!=", str("One"), binop("!=", str("Two"), str("Three"))),
      ~pos=8,
      inputs(list{DeleteContentBackward, DeleteContentBackward}),
      "\"One~Two\" != \"Three\"",
    )
    t("pressing bs on / binop deletes rhs", binop("/", aFloat, aFloat), ~pos=9, bs, "123.456~")
    t(
      "pressing bs on / binop before blank deletes blank",
      binop("/", b, aFloat),
      ~pos=5,
      bs,
      "~123.456",
    )
    t(
      "backspace after selecting all with a versioned 0-arg fnCall in a binop deletes all",
      ~wrap=/* wrap false because else we delete the wrapper */ false,
      ~pos=0,
      binop("/", fn(~mod="HttpClient", "post", ~version=4, list{}), int(5)),
      inputs(list{keypress(K.SelectAll), DeleteContentBackward}),
      "~___",
    )
    t(
      "backspace after selecting all with a binop partial in a binop deletes all",
      ~wrap=/* wrap false because else we delete the wrapper */ false,
      binop("+", partial("D", binop("-", int(5), int(5))), int(5)),
      inputs(list{keypress(K.SelectAll), DeleteWordBackward}),
      "~___",
    )
    t(
      ~expectsPartial=true,
      "inserting a binop in a placeholder works",
      if'(binop("++", b, b), b, b),
      ~pos=3,
      ins("&"),
      "if &~ ++ ____________\nthen\n  ___\nelse\n  ___",
    )
    t(
      "Replacing text when selecting over binop works",
      binop("++", str("five"), str("six")),
      ~sel=(3, 13),
      inputs(list{InsertText("a")}),
      "\"fia~x\"",
    )
  })
  describe("Constructors", () => {
    t(
      ~expectsPartial=true,
      "arguments work in constructors",
      aConstructor,
      ~pos=5,
      ins("t"),
      "Just t~",
    )
    t("int arguments work in constructors", aConstructor, ~pos=5, ins("5"), "Just 5~")
    t(
      ~expectsPartial=true,
      "bs on a constructor converts it to a partial with ghost",
      aConstructor,
      ~pos=4,
      bs,
      "Jus~@ ___",
    )
    t(
      ~expectsPartial=true,
      "del on a constructor converts it to a partial with ghost",
      aConstructor,
      ~pos=0,
      del,
      "~ust@ ___",
    )
    t("space on a constructor blank does nothing", aConstructor, ~pos=5, space, "Just ~___")
    t("ctrl+left mid constructor moves to beg", aConstructor, ~pos=2, ctrlLeft, "~Just ___")
    t("ctrl+left mid constructor moves to end", aConstructor, ~pos=2, ctrlRight, "Just~ ___")
    t(
      "DeleteWordBackward at end of constructor deletes to beg",
      aConstructor,
      ~pos=4,
      inputs(list{DeleteWordBackward}),
      "~___",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordBackward mid constructor deletes to beg",
      aConstructor,
      ~pos=2,
      inputs(list{DeleteWordBackward}),
      "~st@@ ___",
    )
    t(
      "DeleteWordForward at end of constructor moves to blank",
      aConstructor,
      ~pos=4,
      inputs(list{DeleteWordForward}),
      "Just ___~",
    )
    t(
      ~expectsPartial=true,
      "DeleteWordForward mid constructor deletes to end ",
      aConstructor,
      ~pos=2,
      inputs(list{DeleteWordForward}),
      "Ju~@@ ___",
    )
    t(
      "backspace after selecting all with a `Just |___` in a match deletes all",
      ~wrap=/* wrap false because else we delete the wrapper */ false,
      match'(b, list{(pConstructor("Just", list{pBlank()}), b)}),
      ~pos=0,
      inputs(list{keypress(K.SelectAll), DeleteContentBackward}),
      "~___",
    )
    /* TODO: test renaming constructors.
     * It's not too useful yet because there's only 4 constructors and,
     * hence, unlikely that anyone will rename them this way.
     * Also, the names of the temporary variables used to store the old arguments of a changed
     * constructor are randomly generated and would be hard to test */
  })
  describe("Lambdas", () => {
    // type -> to move through a lambda
    t(
      "type - after a lambda var to move into a lambda arrow",
      aLambda,
      ~pos=4,
      ins("-"),
      "\\*** -~> ___",
    )
    t(
      "type - before a lambda arrow to move into a lambda arrow",
      aLambda,
      ~pos=5,
      ins("-"),
      "\\*** -~> ___",
    )
    t("type > inside a lambda arrow to move past it", aLambda, ~pos=6, ins(">"), "\\*** -> ~___")
    // end type -> to move through a lambda
    t("bs over lambda symbol", aLambda, ~pos=1, bs, "~___")
    t("insert space in lambda", aLambda, ~pos=1, key(K.Space), "\\~*** -> ___")
    t("bs non-empty lambda symbol", nonEmptyLambda, ~pos=1, bs, "~5")
    t("del lambda symbol", aLambda, ~pos=0, del, "~___")
    t("del non-empty lambda symbol", nonEmptyLambda, ~pos=0, del, "~5")
    t(
      "insert changes occurence of binding var",
      lambdaWithUsedBinding("binding"),
      ~pos=8,
      ins("c"),
      "\\bindingc~ -> bindingc",
    )
    t(
      "insert changes occurence of binding 2nd var",
      lambdaWithUsed2ndBinding("binding"),
      ~pos=17,
      ins("c"),
      "\\somevar, bindingc~ -> bindingc",
    )
    t("dont jump in lambdavars with infix chars", aLambda, ~pos=1, ins("+"), "\\~*** -> ___")
    t("dont allow name to start with a number", aLambda, ~pos=1, ins("5"), "\\~*** -> ___")
    t(
      "dont allow name to start with a number, pt 2",
      lambdaWithBinding("test", five),
      ~pos=1,
      ins("2"),
      "\\~test -> 5",
    )
    t(
      "dont allow name to start with a number, pt 3",
      aLambda,
      ~pos=3,
      ins("5"),
      // TODO: this looks wrong
      "\\**~* -> ___",
    )
    t(
      "creating lambda in block placeholder should set arguments",
      aFnCallWithBlockArg,
      ~pos=24,
      ins("\\"),
      "Dict::map _____________ \\~key, value -> ___",
    )
    t(
      "creating lambda in block placeholder should set arguments when wrapping expression is inside pipe",
      pipe(b, b, list{}),
      ~pos=6,
      inputs(/* we have to insert the function with completion here
       * so the arguments are adjusted based on the pipe */
      list{InsertText("m"), InsertText("a"), InsertText("p"), keypress(K.Enter), InsertText("\\")}),
      "___\n|>Dict::map \\~key, value -> ___\n",
    )
    t("deleting a lambda argument should work", lambdaWithTwoBindings, ~pos=2, del, "\\x~ -> ___")
    t("backspacing a lambda argument should work", lambdaWithTwoBindings, ~pos=3, bs, "\\x~ -> ___")
    t(
      "deleting a lambda argument should update used variable",
      lambdaWithUsed2ndBinding("x"),
      ~pos=8,
      del,
      "\\somevar~ -> ___",
    )
    t("can add lambda arguments when blank", aLambda, ~pos=4, ins(","), "\\***, ~*** -> ___")
    t(
      "can add lambda arguments to used binding",
      lambdaWithTwoBindings,
      ~pos=5,
      ins(","),
      "\\x, y, ~*** -> ___",
    )
    t(
      "can add lambda arguments in middle used binding",
      lambdaWithTwoBindings,
      ~pos=2,
      ins(","),
      "\\x, ~***, y -> ___",
    )
    t(
      "can add lambda arguments in the front",
      lambdaWithTwoBindings,
      ~pos=1,
      ins(","),
      "\\~***, x, y -> ___",
    )
    t(
      "can add lambda arguments in front of middle",
      lambdaWithTwoBindings,
      ~pos=4,
      ins(","),
      "\\x, ~***, y -> ___",
    )
    t(
      "cant insert a blank from outside the lambda",
      lambdaWithTwoBindings,
      ~pos=0,
      ins(","),
      "~\\x, y -> ___",
    )
    t(
      "cant bs a blank from the space in a lambda",
      lambdaWithTwoBindings,
      ~pos=4,
      bs,
      "\\x,~ y -> ___",
    )
    t(
      "ctrl+left twice over lambda from beg moves to beg of first param",
      lambdaWithTwoBindings,
      ~pos=1,
      keys(list{K.GoToStartOfWord(DropSelection), K.GoToStartOfWord(DropSelection)}),
      "~\\x, y -> ___",
    )
    t(
      "ctrl+right twice over lambda from beg moves to last blank",
      lambdaWithTwoBindings,
      ~pos=1,
      keys(list{K.GoToEndOfWord(DropSelection), K.GoToEndOfWord(DropSelection)}),
      "\\x, y~ -> ___",
    )
    t(
      "ctrl+left twice over lambda from end moves to end of second param",
      lambdaWithTwoBindings,
      ~pos=12,
      keys(list{K.GoToStartOfWord(DropSelection), K.GoToStartOfWord(DropSelection)}),
      "\\x, ~y -> ___",
    )
    t(
      "ctrl+right twice over lambda from end doesnt move",
      lambdaWithTwoBindings,
      ~pos=12,
      keys(list{K.GoToEndOfWord(DropSelection), K.GoToEndOfWord(DropSelection)}),
      "\\x, y -> ___~",
    )
    t(
      "bs second sep in 3-var lambda",
      lambdaWith3UsedBindings,
      ~pos=12,
      bs,
      "\\aVar, bVar~ -> aVar + ___ * bVar",
    )
    t("enter in lambda arrow does nothing", lambdaWithTwoBindings, ~pos=7, enter, "\\x, y -~> ___")
    t(
      "enter after lambda arrow inserts let",
      lambdaWithTwoBindings,
      ~pos=9,
      enter,
      "\\x, y -> let *** = ___\n         ~___",
    )
  })
  describe("Variables", () => {
    t(~expectsPartial=true, "insert middle of variable", aVar, ~pos=5, ins("c"), "variac~ble")
    t(~expectsPartial=true, "del middle of variable", aVar, ~pos=5, del, "varia~le")
    t(~expectsPartial=true, "insert capital works", aVar, ~pos=5, ins("A"), "variaA~ble")
    t(~expectsPartial=true, "insert non-identifier symbol", aVar, ~pos=5, ins("$"), "varia$~ble")
    t("del variable", aShortVar, ~pos=0, del, "~___")
    t(~expectsPartial=true, "del long variable", aVar, ~pos=0, del, "~ariable")
    t(~expectsPartial=true, "del mid variable", aVar, ~pos=6, del, "variab~e")
    t("bs variable", aShortVar, ~pos=1, bs, "~___")
    t(~expectsPartial=true, "bs mid variable", aVar, ~pos=8, bs, "variabl~")
    t(~expectsPartial=true, "bs mid variable", aVar, ~pos=6, bs, "varia~le")
    t(
      "variable doesn't override if",
      let'("i", b, partial("i", b)),
      ~pos=13,
      inputs(list{InsertText("f"), keypress(K.Enter)}),
      "let i = ___\nif ~___\nthen\n  ___\nelse\n  ___",
    )
    t("ctrl+left from beg of variable doesnt move", aVar, ~pos=0, ctrlLeft, "~variable")
    t("ctrl+right from beg of variable moves to end", aVar, ~pos=0, ctrlRight, "variable~")
    t("ctrl+left from end of variable moves to beg", aVar, ~pos=8, ctrlLeft, "~variable")
    t("ctrl+right from end of variable doesnt move", aVar, ~pos=8, ctrlRight, "variable~")
  })
  describe("Match", () => {
    t(
      "move to the front of match",
      emptyMatch,
      ~pos=6,
      key(K.GoToStartOfLine(DropSelection)),
      "~match ___\n  *** -> ___\n",
    )
    t(
      "move to the end of match",
      emptyMatch,
      ~pos=0,
      key(K.GoToEndOfLine(DropSelection)),
      "match ___~\n  *** -> ___\n",
    )
    t(
      "move to the front of match on line 2",
      emptyMatch,
      ~pos=15,
      key(K.GoToStartOfLine(DropSelection)),
      "match ___\n  ~*** -> ___\n",
    )
    t(
      "move to the end of match on line 2",
      emptyMatch,
      ~pos=12,
      key(K.GoToEndOfLine(DropSelection)),
      "match ___\n  *** -> ___~\n",
    )
    t("move back over match", emptyMatch, ~pos=6, key(K.Left), "~match ___\n  *** -> ___\n")
    t("move forward over match", emptyMatch, ~pos=0, key(K.Right), "match ~___\n  *** -> ___\n")
    t("bs over empty match", emptyMatch, ~pos=6, bs, "~___")
    t("bs over empty match with cond", matchWithCond(five), ~pos=6, bs, "~5")
    t("bs over empty match with 2 patterns", emptyMatchWithTwoPatterns, ~pos=6, bs, "~___")
    t("bs over match with 1 pattern", matchWithPattern, ~pos=6, bs, "match ~___\n  3 -> ___\n")
    t(
      "bs over match with 2 patterns",
      matchWithTwoPatterns,
      ~pos=6,
      bs,
      "match ~___\n  3 -> ___\n  4 -> ___\n",
    )
    t("bs over match with 1 blank pattern", matchWithOneExpr(five), ~pos=6, bs, "~5")
    t("del over empty match", emptyMatch, ~pos=0, del, "~___")
    t("del over empty match with cond", matchWithCond(five), ~pos=0, del, "~5")
    t("del over empty match with 2 patterns", emptyMatchWithTwoPatterns, ~pos=0, del, "~___")
    t("del over match with 1 pattern", matchWithPattern, ~pos=0, del, "~match ___\n  3 -> ___\n")
    t(
      "del over match with 2 patterns",
      matchWithTwoPatterns,
      ~pos=0,
      del,
      "~match ___\n  3 -> ___\n  4 -> ___\n",
    )
    t("del over match with 1 blank pattern", matchWithOneExpr(five), ~pos=0, del, "~5")
    t(
      "del constructor in match pattern",
      matchWithConstructorPattern,
      ~pos=12,
      del,
      "match ___\n  ~ust -> ___\n",
    )
    t(
      "bs constructor in match pattern",
      matchWithConstructorPattern,
      ~pos=16,
      bs,
      "match ___\n  Jus~ -> ___\n",
    )
    t(
      "insert changes occurence of non-shadowed var in case",
      matchWithBinding("binding", var("binding")),
      ~pos=19,
      ins("c"),
      "match ___\n  bindingc~ -> bindingc\n",
    )
    t(
      "insert only changes var in same branch",
      matchWithTwoBindings("binding", var("binding"), "binding", var("binding")),
      ~pos=19,
      ins("c"),
      "match ___\n  bindingc~ -> bindingc\n  binding -> binding\n",
    )
    t(
      "bs only changes var in same branch",
      matchWithTwoBindings("binding", var("binding"), "binding", var("binding")),
      ~pos=19,
      bs,
      "match ___\n  bindin~ -> bindin\n  binding -> binding\n",
    )
    /* TODO: uncomment this once the behavior is fixed
      t
        "del only changes var in same branch"
        (matchWithTwoBindings
           "binding"
           (var "binding")
           "binding"
           (var "binding"))
        (del 12)
        "match ___\n  binding -> inding\n  binding -> binding\n" ; */
    t(
      "insert changes occurence of non-shadowed var in case constructor",
      matchWithConstructorBinding("binding", var("binding")),
      ~pos=22,
      ins("c"),
      "match ___\n  Ok bindingc~ -> bindingc\n",
    )
    t("insert space in blank match", emptyMatch, ~pos=6, key(K.Space), "match ~___\n  *** -> ___\n")
    t(
      "insert space in blank match on line 2",
      emptyMatch,
      ~pos=12,
      key(K.Space),
      "match ___\n  ~*** -> ___\n",
    )
    t(
      "enter at the end of the cond creates a new row",
      matchWithPattern,
      ~pos=9,
      enter,
      "match ___\n  ~*** -> ___\n  3 -> ___\n",
    )
    t(
      "enter at the end of a row creates a new row",
      emptyMatchWithTwoPatterns,
      ~pos=22,
      enter,
      "match ___\n  *** -> ___\n  ~*** -> ___\n  *** -> ___\n",
    )
    t(
      "enter at the end of the last row creates a new row",
      emptyMatchWithTwoPatterns,
      ~pos=35,
      enter,
      "match ___\n  *** -> ___\n  *** -> ___\n  ~*** -> ___\n",
    )
    t(
      "enter at the end of the last row in nested match creates a new row",
      nestedMatch,
      ~pos=50,
      enter,
      "match ___\n  *** -> match ___\n           *** -> ___\n           ~*** -> ___\n",
    )
    t(
      "enter at beginning of line after match adds let, not match row",
      let'("a", emptyMatch, five),
      ~pos=39,
      enter,
      "let a = match ___\n          *** -> ___\nlet *** = ___\n~5",
    )
    t(
      "enter at beginning of match expr body adds let, not match row",
      matchWithBinding("a", five),
      ~pos=17,
      enter,
      "match ___\n  a -> let *** = ___\n       ~5\n",
    )
    t(
      "enter in a match arrow does nothing",
      matchWithBinding("a", five),
      ~pos=15,
      enter,
      "match ___\n  a -~> 5\n",
    )
    t(
      "enter at the start of a row creates a new row",
      matchWithPattern,
      ~pos=12,
      enter,
      "match ___\n  *** -> ___\n  ~3 -> ___\n",
    )
    t(
      "enter at the start of row, with match in row above, creates a new row",
      match'(int(1), list{(pInt(5), match'(int(2), list{(pInt(6), b)})), (pInt(7), b)}),
      ~pos=43,
      enter,
      "match 1\n  5 -> match 2\n         6 -> ___\n  *** -> ___\n  ~7 -> ___\n",
    )
    t(
      "backspace first row deletes it",
      emptyMatchWithTwoPatterns,
      ~pos=12,
      bs,
      "match ~___\n  *** -> ___\n",
    )
    t(
      "backspace second row deletes it",
      emptyMatchWithTwoPatterns,
      ~pos=25,
      bs,
      "match ___\n  *** -> ~___\n",
    )
    t("backspacing only row doesn't delete", emptyMatch, ~pos=12, bs, "match ~___\n  *** -> ___\n")
    t(
      "backspacing second matchSep ( |-> ) moves to end of pattern",
      emptyMatchWithTwoPatterns,
      ~pos=29,
      bs,
      "match ___\n  *** -> ___\n  ~*** -> ___\n",
    )
    t(
      "backspacing second matchSep ( -> |) -> moves to end of pattern",
      emptyMatchWithTwoPatterns,
      ~pos=32,
      bs,
      "match ___\n  *** -> ___\n  ~*** -> ___\n",
    )
    t(
      "ctrl+left 2 times from end moves to first blank",
      emptyMatch,
      ~pos=22,
      keys(list{K.GoToStartOfWord(DropSelection), K.GoToStartOfWord(DropSelection)}),
      "match ___\n  ~*** -> ___\n",
    )
    t(
      "ctrl+right 2 times from end doesnt move",
      ~brokenInFF=true,
      emptyMatch,
      ~pos=22,
      keys(list{K.GoToEndOfWord(DropSelection), K.GoToEndOfWord(DropSelection)}),
      "match ___\n  *** -> ___\n~",
    )
    t(
      "ctrl+left 2 times from beg doesnt move",
      emptyMatch,
      ~pos=0,
      keys(list{K.GoToStartOfWord(DropSelection), K.GoToStartOfWord(DropSelection)}),
      "~match ___\n  *** -> ___\n",
    )
    t(
      "ctrl+right 2 times from beg moves to last blank",
      emptyMatch,
      ~pos=0,
      keys(list{K.GoToEndOfWord(DropSelection), K.GoToEndOfWord(DropSelection)}),
      "match ___\n  ***~ -> ___\n",
    )
    t(
      "ctrl+left from mid moves to previous blank ",
      emptyMatch,
      ~pos=15,
      ctrlLeft,
      "match ___\n  ~*** -> ___\n",
    )
    t(
      "ctrl+right from mid moves to next blank",
      emptyMatch,
      ~pos=15,
      ctrlRight,
      "match ___\n  *** -> ___~\n",
    )
    // delete row with delete
  })
  describe("Lets", () => {
    t(
      "move to the front of let",
      emptyLet,
      ~pos=4,
      key(K.GoToStartOfLine(DropSelection)),
      "~let *** = ___\n5",
    )
    t(
      "move to the end of let",
      emptyLet,
      ~pos=4,
      key(K.GoToEndOfLine(DropSelection)),
      "let *** = ___~\n5",
    )
    t("move back over let", emptyLet, ~pos=4, key(K.Left), "~let *** = ___\n5")
    t("move forward over let", emptyLet, ~pos=0, key(K.Right), "let ~*** = ___\n5")
    t("bs over empty let", emptyLet, ~pos=3, bs, "~5")
    t("bs over empty let, after space ", emptyLet, ~pos=4, bs, "~5")
    t("del empty let", emptyLet, ~pos=0, del, "~5")
    t("bs over empty let - underscore", let'("_", b, b), ~pos=3, bs, "~___")
    t("bs over empty let - underscore, after space", let'("_", b, b), ~pos=4, bs, "~___")
    t("del empty let - underscore", let'("_", b, b), ~pos=0, del, "~___")
    t("bs over non-empty let", nonEmptyLet, ~pos=3, bs, "let~ *** = 6\n5")
    t("bs over non-empty let, after space", nonEmptyLet, ~pos=4, bs, "let ~*** = 6\n5")
    t("del non-empty let", nonEmptyLet, ~pos=0, del, "~let *** = 6\n5")
    t("bs with let, empty body", let'("", int(5), b), ~pos=3, bs, "~5")
    t("bs with let, empty body, after space", let'("", int(5), b), ~pos=4, bs, "~5")
    t(
      "bs with let, complex rhs",
      let'("", let'("", int(5), int(6)), b),
      ~pos=3,
      bs,
      "~let *** = 5\n6",
    )
    t(
      "bs with nested let to remove",
      let'("x", int(5), let'("", let'("y", int(6), b), int(7))),
      ~pos=13,
      bs,
      "let x = 5\n~let y = 6\n7",
    )
    t("del with let empty body", let'("", int(5), b), ~pos=0, del, "~5")
    t("bs with let empty body", let'("_", int(5), b), ~pos=3, bs, "~5")
    t("bs with let empty body, after space", let'("_", int(5), b), ~pos=4, bs, "~5")
    t("del with let empty body", let'("_", int(5), b), ~pos=0, del, "~5")
    t("insert space on blank let", emptyLet, ~pos=4, key(K.Space), "let ~*** = ___\n5")
    t("lhs on empty", emptyLet, ~pos=4, ins("c"), "let c~ = ___\n5")
    t("middle of blank", emptyLet, ~pos=5, ins("c"), "let c~ = ___\n5")
    t("bs letlhs", letWithLhs, ~pos=5, bs, "let ~*** = 6\n5")
    t("del letlhs", letWithLhs, ~pos=4, del, "let ~*** = 6\n5")
    t("equals skips over assignment", emptyLet, ~pos=4, insMany(list{"c", "="}), "let c = ~___\n5")
    t("equals skips over assignment 1", emptyLet, ~pos=7, ins("="), "let *** = ~___\n5")
    t("equals skips over assignment 2", emptyLet, ~pos=8, ins("="), "let *** = ~___\n5")
    t("equals skips over assignment 3", emptyLet, ~pos=9, ins("="), "let *** = ~___\n5")
    t(
      "bs changes occurence of binding var",
      letWithUsedBinding("binding"),
      ~pos=11,
      bs,
      "let bindin~ = 6\nbindin",
    )
    t(
      "insert changes occurence of binding var",
      letWithUsedBinding("binding"),
      ~pos=11,
      ins("c"),
      "let bindingc~ = 6\nbindingc",
    )
    t(
      "insert changes occurence of binding in match nested expr",
      letWithBinding(
        "binding",
        match'(b, list{(pVar("binding"), var("binding")), (pInt(5), var("binding"))}),
      ),
      ~pos=11,
      ins("c"),
      "let bindingc~ = 6\nmatch ___\n  binding -> binding\n  5 -> bindingc\n",
    )
    t(
      "insert doesn't change occurence of binding in shadowed lambda expr",
      letWithBinding("binding", lambda(list{"binding"}, var("binding"))),
      ~pos=11,
      ins("c"),
      "let bindingc~ = 6\n\\binding -> binding",
    )
    t(
      "insert changes occurence of binding in lambda expr",
      letWithBinding("binding", lambda(list{"somevar"}, var("binding"))),
      ~pos=11,
      ins("c"),
      "let bindingc~ = 6\n\\somevar -> bindingc",
    )
    t("dont jump in letlhs with infix chars", emptyLet, ~pos=4, ins("+"), "let ~*** = ___\n5")
    t("dont allow letlhs to start with a number", emptyLet, ~pos=4, ins("5"), "let ~*** = ___\n5")
    t(
      "dont allow letlhs to start with a number, pt 2",
      letWithLhs,
      ~pos=4,
      ins("2"),
      "let ~n = 6\n5",
    )
    t(
      "dont allow letlhs to start with a number, pt 3",
      emptyLet,
      ~pos=6,
      ins("5"),
      "let **~* = ___\n5",
    )
    t(
      "enter on the end of let goes to blank",
      nonEmptyLetWithBlankEnd,
      ~pos=11,
      enter,
      "let *** = 6\n~___",
    )
    t(
      "enter at the end of a line inserts let if no blank is next",
      nonEmptyLet,
      ~pos=11,
      enter,
      "let *** = 6\nlet ~*** = ___\n5",
    )
    t(
      "enter at the start of a let creates let above",
      twoLets,
      ~pos=10,
      enter,
      "let x = 5\nlet *** = ___\n~let y = 6\n7",
    )
    t(
      "enter at the start of first let creates let above",
      nonEmptyLet,
      ~pos=0,
      enter,
      "let *** = ___\n~let *** = 6\n5",
    )
    t(
      "enter at the end of a let with a let below inserts new let",
      twoLets,
      ~pos=9,
      enter,
      "let x = 5\nlet ~*** = ___\nlet y = 6\n7",
    )
    t(
      "enter on the end of first let inserts new let",
      matchWithTwoLets,
      ~pos=28,
      enter,
      "match ___\n  *** -> let x = 5\n         let ~*** = ___\n         let y = 6\n         ___\n",
    )
    t(
      "enter on the end of second let goes to blank",
      matchWithTwoLets,
      ~pos=47,
      enter,
      "match ___\n  *** -> let x = 5\n         let y = 6\n         ~___\n",
    )
    t(
      "enter at the start of a non-let also creates let above",
      anInt,
      ~pos=0,
      enter,
      "let *** = ___\n~12345",
    )
    tStruct(
      "wrapping a pipe in a let with enter creates correct ast",
      aPipe,
      ~pos=0,
      list{keypress(~shiftHeld=false, K.Enter)},
      let'(
        "",
        blank(),
        pipe(
          list(list{}),
          fn(~mod="List", "append", list{pipeTarget, list(list{int(5)})}),
          list{fn(~mod="List", "append", list{pipeTarget, list(list{int(5)})})},
        ),
      ),
    )
    t(
      "wrapping a pipe in a let with enter places caret correctly",
      aPipe,
      enter,
      "let *** = ___\n~[]\n|>List::append [5]\n|>List::append [5]\n",
    )
    t(
      "Ctrl+left in front of a varname moves to previous editable text",
      matchWithTwoLets,
      ~pos=23,
      ctrlLeft,
      "match ___\n  ~*** -> let x = 5\n         let y = 6\n         ___\n",
    )
    t(
      "Ctrl+right in front of a varname moves to next editable text",
      matchWithTwoLets,
      ~pos=15,
      ctrlRight,
      "match ___\n  *** -> let x~ = 5\n         let y = 6\n         ___\n",
    )
    t(
      "enter at the end of a non-let wraps literal expr in let",
      aShortInt,
      ~pos=1,
      enter,
      "let _ = 1\n~___",
    )
    t(
      "enter at the end of a non-let wraps fncall in let",
      aFullFnCall,
      ~pos=12,
      enter,
      "let _ = Int::add 5 5\n~___",
    )
    t(
      "enter at the end of non-final arg, should just go to next line: #1",
      let'("x", fn(~mod="Int", "add", list{record(list{("", int(5))}), int(6)}), b),
      ~pos=60,
      enter,
      "let x = Int::add\n          {\n            *** : 5\n          }\n          ~6\n___",
    )
    t(
      "enter at the end of a non-final arg should just go to next line: #2",
      fn(~mod="Int", "add", list{record(list{("", int(5))}), int(6)}),
      ~pos=28,
      enter,
      "Int::add\n  {\n    *** : 5\n  }\n  ~6",
    )
    t("enter at the start of ast also creates let", anInt, ~pos=0, enter, "let *** = ___\n~12345")
  })
  describe("Pipes", () => {
    // TODO: add tests for clicking in the middle of a pipe (or blank)
    t(
      "move to the front of pipe on line 1",
      aPipe,
      ~pos=2,
      key(K.GoToStartOfLine(DropSelection)),
      "~[]\n|>List::append [5]\n|>List::append [5]\n",
    )
    t(
      "move to the end of pipe on line 1",
      aPipe,
      ~pos=0,
      key(K.GoToEndOfLine(DropSelection)),
      "[]~\n|>List::append [5]\n|>List::append [5]\n",
    )
    t(
      "move to the front of pipe on line 2",
      aPipe,
      ~pos=8,
      key(K.GoToStartOfLine(DropSelection)),
      "[]\n|>~List::append [5]\n|>List::append [5]\n",
    )
    t(
      "move to the end of pipe on line 2",
      aPipe,
      ~pos=5,
      key(K.GoToEndOfLine(DropSelection)),
      "[]\n|>List::append [5]~\n|>List::append [5]\n",
    )
    t(
      "move to the front of pipe on line 3",
      aPipe,
      ~pos=40,
      key(K.GoToStartOfLine(DropSelection)),
      "[]\n|>List::append [5]\n|>~List::append [5]\n",
    )
    t(
      "move to the end of pipe on line 3",
      aPipe,
      ~pos=24,
      key(K.GoToEndOfLine(DropSelection)),
      "[]\n|>List::append [5]\n|>List::append [5]~\n",
    )
    t("pipes appear on new lines", aPipe, render, "~[]\n|>List::append [5]\n|>List::append [5]\n")
    t(
      "nested pipes will indent",
      aNestedPipe,
      render,
      "~[]\n|>List::append [5]\n               |>List::append [6]\n",
    )
    t(
      "backspacing a pipe's first pipe works",
      aLongPipe,
      ~pos=5,
      bs,
      "[]~\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n",
    )
    t(
      "deleting a pipe's first pipe works",
      aLongPipe,
      ~pos=3,
      del,
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      "[]~\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n",
    )
    t(
      "backspacing a pipe's second pipe works",
      aLongPipe,
      ~pos=24,
      bs,
      "[]\n|>List::append [2]~\n|>List::append [4]\n|>List::append [5]\n",
    )
    t(
      "deleting a pipe's second pipe works",
      aLongPipe,
      ~pos=22,
      del,
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      "[]\n|>List::append [2]~\n|>List::append [4]\n|>List::append [5]\n",
    )
    t(
      "backspacing a pipe's third pipe works",
      aLongPipe,
      ~pos=43,
      bs,
      "[]\n|>List::append [2]\n|>List::append [3]~\n|>List::append [5]\n",
    )
    t(
      "deleting a pipe's third pipe works",
      aLongPipe,
      ~pos=41,
      del,
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      "[]\n|>List::append [2]\n|>List::append [3]~\n|>List::append [5]\n",
    )
    t(
      "backspacing a pipe's last pipe works",
      aLongPipe,
      ~pos=62,
      bs,
      "[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]~\n",
    )
    t(
      "deleting a pipe's last pipe works",
      aLongPipe,
      ~pos=60,
      del,
      "[]\n|>List::append [2]\n|>List::append [3]\n|>List::append [4]~\n",
    )
    t(
      "backspacing a pipe's first pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=21,
      bs,
      "if ___\nthen\n  []~\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___",
    )
    t(
      "deleting a pipe's first pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=19,
      del,
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      "if ___\nthen\n  []~\n  |>List::append [3]\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___",
    )
    t(
      "backspacing a pipe's second pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=42,
      bs,
      "if ___\nthen\n  []\n  |>List::append [2]~\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___",
    )
    t(
      "deleting a pipe's second pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=40,
      del,
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      "if ___\nthen\n  []\n  |>List::append [2]~\n  |>List::append [4]\n  |>List::append [5]\nelse\n  ___",
    )
    t(
      "backspacing a pipe's third pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=63,
      bs,
      "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]~\n  |>List::append [5]\nelse\n  ___",
    )
    t(
      "deleting a pipe's third pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=61,
      del,
      // TODO: fix caret affinity https://www.notion.so/darklang/Keyboard-and-Input-Handling-44eeedc4953846159e96af1e979004ad
      "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]~\n  |>List::append [5]\nelse\n  ___",
    )
    t(
      "backspacing a pipe's fourth pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=84,
      bs,
      "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]~\nelse\n  ___",
    )
    t(
      "deleting a pipe's fourth pipe that isn't in the first column works",
      aPipeInsideIf,
      ~pos=82,
      del,
      "if ___\nthen\n  []\n  |>List::append [2]\n  |>List::append [3]\n  |>List::append [4]~\nelse\n  ___",
    )
    t(
      ~expectsPartial=true,
      "backspacing a pipe's first fn works",
      aLongPipe,
      ~pos=17,
      bs,
      "[]\n|>List::appen~@ [2]\n|>List::append [3]\n|>List::append [4]\n|>List::append [5]\n",
    )
    t(
      ~expectsPartial=true,
      "backspacing a pipe's first binop works",
      aBinopPipe,
      ~pos=8,
      bs,
      "___\n|>+~@ \"asd\"\n",
    )
    t(
      "bs to remove binop in pipe places caret correctly",
      aBinopPlusPipe,
      ~pos=7,
      bs,
      "___\n|>~10\n",
    )
    t(
      "adding binops adds the right number of blanks",
      emptyPipe,
      ~pos=6,
      inputs(list{InsertText("+"), keypress(K.Enter)}),
      "___\n|>+ ~_________\n",
    )
    t(
      "creating a pipe from an fn via a partial works",
      partial("|>", aFnCall),
      ~pos=2,
      enter,
      "Int::add 5 _________\n|>~___\n",
    )
    t(
      "enter at the end of a pipe expr creates a new entry",
      aPipe,
      ~pos=21,
      enter,
      "[]\n|>List::append [5]\n|>~___\n|>List::append [5]\n",
    )
    t(
      "enter at the end of the opening expr creates a new entry",
      aPipe,
      ~pos=2,
      enter,
      "[]\n|>~___\n|>List::append [5]\n|>List::append [5]\n",
    )
    t(
      "enter at the start of a line creates a new entry",
      aPipe,
      ~pos=3,
      enter,
      "[]\n|>___\n~|>List::append [5]\n|>List::append [5]\n",
    )
    t(
      "enter at start of blank (within pipe) creates a new entry",
      aPipe,
      ~pos=5,
      enter,
      "[]\n|>___\n|>~List::append [5]\n|>List::append [5]\n",
    )
    t(
      "enter at the end of the last expr creates a new entry",
      aPipe,
      ~pos=40,
      enter,
      "[]\n|>List::append [5]\n|>List::append [5]\n|>~___\n",
    )
    t(
      "enter at the end of the last nested expr creates a new entry",
      aNestedPipe,
      ~pos=55,
      enter,
      "[]\n|>List::append [5]\n               |>List::append [6]\n               |>~___\n",
    )
    t(
      "enter at the end of pipe expression with line below creates a new entry",
      ~wrap=/* indent counting is all weird with wrapper */ false,
      let'("a", pipe(list(list{}), listFn(list{aList5}), list{}), five),
      ~pos=37,
      enter,
      "let a = []\n        |>List::append [5]\n        |>~___\n5",
    )
    t(
      "enter at the beginning of expression after pipe creates let, not pipe",
      ~wrap=/* indent counting is all weird with wrapper */ false,
      let'("a", pipe(list(list{}), listFn(list{aList5}), list{}), five),
      ~pos=38,
      enter,
      "let a = []\n        |>List::append [5]\nlet *** = ___\n~5",
    )
    t(
      "inserting a pipe into a list at a pipeble gives a new pipe from the list element",
      listFn(list{aList5}),
      ~pos=15,
      key(K.ShiftEnter),
      "List::append [5\n              |>~___\n             ]",
    )
    t(
      "inserting a pipe into a list within a list gives a new pipe from the interior list element",
      listFn(list{list(list{five, list(list{six})})}),
      ~pos=18,
      key(K.ShiftEnter),
      "List::append [5,[6\n                 |>~___\n                ]]",
    )
    t(
      "inserting a pipe at a blank element within a list gives a new pipe from the blank element",
      listFn(list{list(list{b, six})}),
      ~pos=15,
      key(K.ShiftEnter),
      "List::append [___\n              |>~___\n             ,6]",
    )
    t(
      "inserting a pipe into another pipe gives a single pipe1",
      pipe(five, listFn(list{rightPartial("|>", aList5)}), list{}),
      ~pos=23,
      enter,
      "5\n|>List::append [5]\n|>~___\n",
    )
    t(
      "inserting a pipe into another pipe at a pipeable gives a new pipe",
      pipe(five, listFn(list{aList5}), list{}),
      ~pos=19,
      key(K.ShiftEnter),
      "5\n|>List::append [5\n                |>~___\n               ]\n",
    )
    t(
      "inserting a pipe into another pipe gives a single pipe3",
      five,
      ~pos=1,
      key(K.ShiftEnter),
      "5\n|>~___\n",
    )
    t(
      "shift enter at a let's newline creates the pipe on the rhs",
      nonEmptyLet,
      ~pos=11,
      key(K.ShiftEnter),
      "let *** = 6\n          |>~___\n5",
    )
    t(
      "shift enter in a record's newline creates the pipe in the expr, not the entire record",
      record(list{("f1", fiftySix), ("f2", seventyEight)}),
      ~pos=11,
      key(K.ShiftEnter),
      "{\n  f1 : 56\n       |>~___\n  f2 : 78\n}",
    )
    t(
      "ctrl+left moves to front of thread ",
      aPipe,
      ~pos=19,
      ctrlLeft,
      "[]\n|>~List::append [5]\n|>List::append [5]\n",
    )
    t(
      "ctrl+right moves to end of next thread ",
      aPipe,
      ~pos=20,
      ctrlRight,
      "[]\n|>List::append [5]\n|>List::append~ [5]\n",
    )
    t(
      "bsing a blank pipe after a piped 1-arg function deletes all",
      ~wrap=/* wrap false because else we delete the wrapper */ false,
      pipe(aList5, fn(~mod="List", "length", list{pipeTarget}), list{b}),
      ~pos=0,
      inputs(list{keypress(K.SelectAll), DeleteContentBackward}),
      "~___",
    )
    t(
      "replacing piped blank with fn does not insert pipe target",
      aPipe,
      ~pos=2,
      inputs(list{
        DeleteContentBackward,
        DeleteContentBackward,
        InsertText("List::append"),
        keypress(K.Enter),
      }),
      "List::append ~___________ ___________\n|>List::append [5]\n|>List::append [5]\n",
    )
    // TODO: test for prefix fns
    // TODO: test for deleting pipeed infix fns
    // TODO: test for deleting pipeed prefix fns
  })
  describe("Ifs", () => {
    t("move over indent 1", plainIf, ~pos=12, key(K.Left), "if 5\nthen~\n  6\nelse\n  7")
    t("move over indent 2", plainIf, ~pos=21, key(K.Left), "if 5\nthen\n  6\nelse~\n  7")
    t("bs over indent 1", plainIf, ~pos=12, bs, "if 5\nthen~\n  6\nelse\n  7")
    t("bs over indent 2", plainIf, ~pos=21, bs, "if 5\nthen\n  6\nelse~\n  7")
    t("bs over empty if", emptyIf, ~pos=2, bs, "~___")
    t("bs over if with cond", ifOnlyCond, ~pos=2, bs, "~5")
    t("bs over if with then", ifOnlyThen, ~pos=2, bs, "~5")
    t("bs over if with else", ifOnlyElse, ~pos=2, bs, "~5")
    t("del over empty if", emptyIf, ~pos=0, del, "~___")
    t("del over if with cond", ifOnlyCond, ~pos=0, del, "~5")
    t("del over if with then", ifOnlyThen, ~pos=0, del, "~5")
    t("del over if with else", ifOnlyElse, ~pos=0, del, "~5")
    t(
      "move to front of line 1",
      plainIf,
      ~pos=4,
      key(K.GoToStartOfLine(DropSelection)),
      "~if 5\nthen\n  6\nelse\n  7",
    )
    t(
      "move to end of line 1",
      plainIf,
      ~pos=0,
      key(K.GoToEndOfLine(DropSelection)),
      "if 5~\nthen\n  6\nelse\n  7",
    )
    t(
      "move to front of line 3",
      plainIf,
      ~pos=13,
      key(K.GoToStartOfLine(DropSelection)),
      "if 5\nthen\n  ~6\nelse\n  7",
    )
    t(
      "move to end of line 3",
      plainIf,
      ~pos=12,
      key(K.GoToEndOfLine(DropSelection)),
      "if 5\nthen\n  6~\nelse\n  7",
    )
    t(
      "move to front of line 5 in nested if",
      nestedIf,
      ~pos=16,
      key(K.GoToStartOfLine(DropSelection)),
      "if 5\nthen\n  ~if 5\n  then\n    6\n  else\n    7\nelse\n  7",
    )
    t(
      "move to end of line 5 in nested if",
      nestedIf,
      ~pos=12,
      key(K.GoToEndOfLine(DropSelection)),
      "if 5\nthen\n  if 5~\n  then\n    6\n  else\n    7\nelse\n  7",
    )
    t(
      "try to insert space on blank",
      emptyIf,
      ~pos=3,
      key(K.Space),
      "if ~___\nthen\n  ___\nelse\n  ___",
    )
    t(
      "try to insert space on blank indent 2",
      emptyIf,
      ~pos=14,
      key(K.Space),
      "if ___\nthen\n  ~___\nelse\n  ___",
    )
    t(
      "enter in front of an if wraps in a let",
      plainIf,
      ~pos=0,
      enter,
      "let *** = ___\n~if 5\nthen\n  6\nelse\n  7",
    )
    t("enter at end of if line does nothing", plainIf, ~pos=4, enter, "if 5\n~then\n  6\nelse\n  7")
    t(
      "enter at the end of then line wraps expr in let",
      plainIf,
      ~pos=13,
      enter,
      "if 5\nthen\n  let _ = 6\n  ~___\nelse\n  7",
    )
    t(
      "enter at the end of else line wraps expr in let",
      plainIf,
      ~pos=22,
      enter,
      "if 5\nthen\n  6\nelse\n  let _ = 7\n  ~___",
    )
    t(
      "enter at end of then line inserts let if no blank next ",
      plainIf,
      ~pos=9,
      enter,
      "if 5\nthen\n  let ~*** = ___\n  6\nelse\n  7",
    )
    t(
      "enter at end of else line does inserts let if no blank next",
      /* TODO: This should probably do nothing, but right now it acts like
       * it's at the front of the line below. */
      plainIf,
      ~pos=18,
      enter,
      "if 5\nthen\n  6\nelse\n  let ~*** = ___\n  7",
    )
    t(
      "ctrl+left from value moves to condition ",
      plainIf,
      ~pos=12,
      ctrlLeft,
      "if ~5\nthen\n  6\nelse\n  7",
    )
    t(
      "ctrl+right from condition moves to value ",
      plainIf,
      ~pos=4,
      ctrlRight,
      "if 5\nthen\n  6~\nelse\n  7",
    )
    t(
      "space in AC at end of then line should not advance",
      let'("x", int(5), if'(partial("x", b), b, b)),
      ~pos=14,
      space,
      "let x = 5\nif x~\nthen\n  ___\nelse\n  ___",
    )
  })
  describe("Lists", () => {
    t("create list", b, ~pos=0, ins("["), "[~]")
    t("insert into empty list inserts", emptyList, ~pos=1, ins("5"), "[5~]")
    t("inserting before a list is no-op", emptyList, ~pos=0, ins("5"), "~[]")
    // tStruct(
    //   "inserting before a list at top-level creates left partial",
    //   emptyList,
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", emptyList),
    // )
    t("insert space into multi list", multiList, ~pos=6, key(K.Space), "[56,78~]")
    t("insert space into single list", singleElementList, ~pos=3, key(K.Space), "[56~]")
    t("insert into existing list item", singleElementList, ~pos=1, ins("4"), "[4~56]")
    t(
      "insert separator before item creates blank",
      singleElementList,
      ~pos=1,
      ins(","),
      "[~___,56]",
    )
    t("insert separator after item creates blank", singleElementList, ~pos=3, ins(","), "[56,~___]")
    t(
      "insert separator after separator creates blank",
      list(list{int(1), int(2), int(3)}),
      ~pos=5,
      ins(","),
      "[1,2,~___,3]",
    )
    t(
      "insert , in string in list types ,",
      list(list{str("01234567890123456789012345678901234567890")}),
      ~pos=45,
      ins(","),
      "[\"0123456789012345678901234567890123456789\n  ,~0\"]",
    )
    t(
      "insert separator after item creates blank when list is in match",
      match'(singleElementList, list{(pBlank(), b)}),
      ~pos=9,
      ins(","),
      "match [56,~___]\n  *** -> ___\n",
    )
    t("insert separator between items creates blank", multiList, ~pos=3, ins(","), "[56,~___,78]")
    // t "insert separator mid integer makes two items" single (ins ',' 2)
    // ("[5,6]", 3) ;
    // TODO: when on a separator in a nested list, pressing comma makes an entry outside the list.
    t(
      "insert separator mid string does nothing special ",
      listWithStr,
      ~pos=3,
      ins(","),
      "[\"a,~b\"]",
    )
    t("backspacing open bracket of empty list dels list", emptyList, ~pos=1, bs, "~___")
    t("backspacing close bracket of empty list moves inside list", emptyList, ~pos=2, bs, "[~]")
    t("deleting open bracket of empty list dels list", emptyList, ~pos=0, del, "~___")
    t("close bracket at end of list is swallowed", emptyList, ~pos=1, ins("]"), "[]~")
    t(
      "bs on first separator between ints merges ints on either side of sep",
      multiList,
      ~pos=4,
      bs,
      "[56~78]",
    )
    t(
      "del before first separator between ints merges ints on either side of sep",
      multiList,
      ~pos=3,
      del,
      "[56~78]",
    )
    t(
      "bs on middle separator between ints merges ints on either side of sep",
      longList,
      ~pos=10,
      bs,
      "[56,78,56~78,56,78]",
    )
    t(
      "del before middle separator between ints merges ints on either side of sep",
      longList,
      ~pos=9,
      del,
      "[56,78,56~78,56,78]",
    )
    t("bs on separator between item and blank dels blank", listWithBlank, ~pos=7, bs, "[56,78~,56]")
    t(
      "del on separator between item and blank dels blank",
      listWithBlank,
      ~pos=6,
      del,
      "[56,78~,56]",
    )
    t(
      "bs on separator between blank and item dels blank",
      listWithBlank,
      ~pos=11,
      bs,
      "[56,78,~56]",
    )
    t(
      "del on separator between blank and item dels blank",
      listWithBlank,
      ~pos=10,
      del,
      "[56,78,~56]",
    )
    t(
      "bs on separator between string items merges strings",
      listWithMultiStrs,
      ~pos=6,
      bs,
      "[\"ab~cd\",\"ef\"]",
    )
    t(
      "del before separator between string items merges strings",
      listWithMultiStrs,
      ~pos=5,
      del,
      "[\"ab~cd\",\"ef\"]",
    )
    t(
      "ctrl+left at the beg of list item moves to beg of next list item",
      longList,
      ~pos=10,
      ctrlLeft,
      "[56,78,~56,78,56,78]",
    )
    t(
      "ctrl+right at the end of list item moves to end of next list item",
      longList,
      ~pos=12,
      ctrlRight,
      "[56,78,56,78,56~,78]",
    )
    t(
      "a very long list wraps",
      veryLongList,
      render,
      "~[56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n 78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,\n 56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n 78,56,78,56,78,56,78,56,78,56,78]",
    )
    t(
      "a list of long floats does not break upon wrap",
      list(list{
        EFloat(
          gid(),
          Positive,
          "4611686018427387",
          "12345678901234567989048290381902830912830912830912830912309128901234567890123456789",
        ),
        EFloat(
          gid(),
          Positive,
          "4611686018427387",
          "1234567890183918309183091809183091283019832345678901234567890123456789",
        ),
        EFloat(gid(), Positive, "4611686018427387", "123456"),
      }),
      render,
      "~[4611686018427387.12345678901234567989048290381902830912830912830912830912309128901234567890123456789,\n 4611686018427387.1234567890183918309183091809183091283019832345678901234567890123456789,\n 4611686018427387.123456]",
    )
    t(
      "a nested very list wraps with proper indents",
      let'("a", veryLongList, b),
      render,
      "~let a = [56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n         78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,\n         56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n         78,56,78,56,78,56,78,56,78,56,78]\n___",
    )
    t("inserting [ before int creates a singleton int list", aShortInt, ~pos=0, ins("["), "[~1]")
    t(
      "inserting [ before multiline string creates a singleton string list",
      mlStrWSpace,
      ~pos=0,
      ins("["),
      "[~\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "   123456789_ abcdefghi, 123456789_ abcdef\n" ++ "  ghi,\"]",
    )
    t(
      "inserting [ before true creates a singleton bool list",
      trueBool,
      ~pos=0,
      ins("["),
      "[~true]",
    )
    t(
      "inserting [ before true creates a singleton bool list",
      falseBool,
      ~pos=0,
      ins("["),
      "[~false]",
    )
    t("inserting [ before true creates a singleton bool list", aNull, ~pos=0, ins("["), "[~null]")
    t(
      "inserting [ before float creates a singleton float list",
      aFloat,
      ~pos=0,
      ins("["),
      "[~123.456]",
    )
    t(
      "inserting [ before function call creates a singleton function list",
      aFullFnCall,
      ~pos=0,
      ins("["),
      "[~Int::add 5 5]",
    )
    t(
      "inserting [ before variable creates a singleton with variable",
      aVar,
      ~pos=0,
      ins("["),
      "[~variable]",
    )
    t(
      "inserting [ before empty list open wraps it in another list",
      emptyList,
      ~pos=0,
      ins("["),
      "[~[]]",
    )
    t(
      "inserting [ before list open wraps it in another list",
      multiList,
      ~pos=0,
      ins("["),
      "[~[56,78]]",
    )
    t(
      "inserting [ before record open wraps it in a list",
      singleRowRecord,
      ~pos=0,
      ins("["),
      "[~{\n   f1 : 56\n }]",
    )
    t(
      "inserting [ before constructor creates a singleton of that type",
      aConstructor,
      ~pos=0,
      ins("["),
      "[~Just ___]",
    )
    t("deleting [ from a singleton remove a list wrapping", singleElementList, ~pos=0, del, "~56")
    t("backspacing [ from a singleton remove a list wrapping", singleElementList, ~pos=1, bs, "~56")
    t(
      "a delete with caret before a blank in front of a list will delete the blank",
      listWithBlankAtStart,
      ~pos=1,
      del,
      "[~56,78,56]",
    )
    t(
      "a delete with caret before a list with just a blank removes the blank",
      listWithJustABlank,
      ~pos=1,
      del,
      "[~]",
    )
  })

  describe("Tuples", () => {
    describe("render", () => {
      t("blank tuple", tuple2WithBothBlank, render, "~(___,___)")
      t("simple tuple", tuple2WithNoBlank, render, "~(56,78)")
      t(
        "a very long tuple wraps",
        tupleHuge,
        render,
        "~(56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n 78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,\n 56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n 78,56,78,56,78,56,78,56,78,56,78)",
      )
      t(
        "a tuple of long floats does not break upon wrap",
        tuple(
          EFloat(
            gid(),
            Positive,
            "4611686018427387",
            "12345678901234567989048290381902830912830912830912830912309128901234567890123456789",
          ),
          EFloat(
            gid(),
            Positive,
            "4611686018427387",
            "1234567890183918309183091809183091283019832345678901234567890123456789",
          ),
          list{EFloat(gid(), Positive, "4611686018427387", "123456")},
        ),
        render,
        "~(4611686018427387.12345678901234567989048290381902830912830912830912830912309128901234567890123456789,\n 4611686018427387.1234567890183918309183091809183091283019832345678901234567890123456789,\n 4611686018427387.123456)",
      )
      t(
        "a nested very tuple wraps with proper indents",
        let'("a", tupleHuge, b),
        render,
        "~let a = (56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n         78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,\n         56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,78,56,\n         78,56,78,56,78,56,78,56,78,56,78)\n___",
      )
    })
    describe("navigate", () => {
      t(
        "ctrl+left at the beginning of tuple item moves to beginning of next tuple item",
        tuple(fiftySix, seventyEight, list{fiftySix, seventyEight, fiftySix, seventyEight}),
        ~pos=10, // after the third comma, before the 2nd 78
        ctrlLeft,
        "(56,78,~56,78,56,78)",
      )
      t(
        "ctrl+right at the end of tuple item moves to end of next tuple item",
        tuple6,
        ~pos=12,
        ctrlRight,
        "(56,78,56,78,56~,78)",
      )
    })

    if defaultTestProps.settings.allowTuples {
      describe("create", () => {
        t("create tuple", b, ~pos=0, ins("("), "(~___,___)")
        t("create and fill in tuple", b, ~pos=0, insMany(list{"(", "1", ",", "2", ")"}), "(1,2)~")
      })
    }
    describe("insert", () => {
      t("insert into empty tuple inserts", tuple2WithBothBlank, ~pos=1, ins("5"), "(5~,___)")
      t("inserting before a tuple is no-op", tuple2WithBothBlank, ~pos=0, ins("5"), "~(___,___)")
      tStruct(
        "inserting before a tuple at top-level is a no-op",
        tuple2WithBothBlank,
        ~pos=0,
        list{InsertText("c")},
        tuple2WithBothBlank,
      )
      t(
        "insert space into multi tuple does nothing",
        tuple2WithNoBlank,
        ~pos=6, // right before closing )
        key(K.Space),
        "(56,78~)",
      )
      t(
        "insert separator after opening parens creates blank",
        tuple(int(1), int(2), list{int(3)}),
        ~pos=1, // after the (
        ins(","),
        "(~___,1,2,3)",
      )
      t(
        "insert separator before closing parens creates blank",
        tuple(int(1), int(2), list{int(3)}), // (1,2,3)
        ~pos=6, // before the )
        ins(","),
        "(1,2,3,~___)",
      )
      t(
        "insert separator after separator creates blank",
        tuple(int(1), int(2), list{int(3)}),
        ~pos=5, // after the second comma
        ins(","),
        "(1,2,~___,3)",
      )
      t(
        "inserting space into simple tuple does nothing",
        tuple2WithNoBlank,
        ~pos=3,
        key(K.Space),
        "(56~,78)",
      )
      t(
        "insert separator before item creates blank",
        tuple2WithNoBlank,
        ~pos=1,
        ins(","),
        "(~___,56,78)",
      )
      t(
        "insert separator after item creates blank",
        tuple2WithNoBlank,
        ~pos=6, // after 78
        ins(","),
        "(56,78,~___)",
      )
      t(
        "insert , in string in tuple types ,",
        tuple(str("01234567890123456789012345678901234567890"), fiftySix, list{}),
        ~pos=45, // right before the last 0
        ins(","),
        "(\"0123456789012345678901234567890123456789\n" ++ "  ,~0\",56)",
      )
      t(
        "insert separator after item creates blank when tuple is in match",
        match'(tuple2WithNoBlank, list{(pBlank(), b)}),
        ~pos=12, // before the closing )
        ins(","),
        "match (56,78,~___)\n  *** -> ___\n",
      )

      t(
        "insert separator just before another skips over it",
        tuple2WithNoBlank,
        ~pos=3, // before the first comma
        ins(","),
        "(56,~78)",
      )
      t(
        "insert separator just after another creates blank",
        tuple2WithNoBlank,
        ~pos=4, // just after the comma
        ins(","),
        "(56,~___,78)",
      )

      t(
        "insert separator mid int does nothing special ",
        tuple2WithNoBlank,
        ~pos=2, // halfway through `56`
        ins(","),
        "(5~6,78)",
      )
      t(
        "insert separator mid string does nothing special ",
        tuple3WithStrs,
        ~pos=3, // in between the a and b of the first str
        ins(","),
        "(\"a,~b\",\"cd\",\"ef\")",
      )
      t(
        "close bracket at end of tuple is swallowed",
        tuple2WithNoBlank,
        ~pos=6, // right before closing )
        ins(")"),
        "(56,78)~",
      )
    })

    describe("delete", () => {
      // 2-tuple, no blanks
      t("deleting ( from a filled 2-tuple does nothing", tuple2WithNoBlank, ~pos=0, del, "~(56,78)")
      t(
        "deleting ) from a filled 2-tuple just moves cursor left",
        tuple2WithNoBlank,
        ~pos=7,
        bs,
        "(56,78~)",
      )
      t(
        "deleting , from a filled 2-tuple leaves only the first item",
        tuple2WithNoBlank, // (56,78)
        ~pos=4, // at the ,
        bs,
        "56~",
      )

      // 2-tuple, first blank
      t(
        "deleting ( from a 2-tuple with first value blank converts to the non-blank value",
        tuple2WithFirstBlank,
        ~pos=0,
        del,
        "~78",
      )
      t(
        "deleting ) from a 2-tuple with first value blank just moves the cursor to left of )",
        tuple2WithFirstBlank,
        ~pos=8, // just after )
        bs,
        "(___,78~)",
      )
      t(
        "deleting , from a 2-tuple with first value blank converts to a blank",
        tuple2WithFirstBlank,
        ~pos=4, // just after ,
        del,
        "~___",
      )

      // 2-tuple, second blank
      t(
        "deleting ( from a 2-tuple with second value blank converts to the non-blank value",
        tuple2WithSecondBlank,
        ~pos=0,
        del,
        "~56",
      )
      t(
        "deleting ) from a 2-tuple with second value blank just moves the cursor left",
        tuple2WithSecondBlank,
        ~pos=7, // just before )
        del,
        "(56,___~)",
      )
      t(
        "deleting , from a 2-tuple with second value blank converts to the non-blank value",
        tuple2WithSecondBlank,
        ~pos=3, // just before ,
        del,
        "56~",
      )

      // 2-tuple, both blank
      t(
        "deleting ( from a blank 2-tuple converts to blank",
        tuple2WithBothBlank,
        ~pos=0,
        del,
        "~___",
      )
      t(
        "deleting ) from a blank 2-tuple just moves the cursor left",
        tuple2WithBothBlank,
        ~pos=9,
        bs,
        "(___,___~)",
      )
      t(
        "deleting , from a blank 2-tuple replaces the tuple with a blank",
        tuple2WithBothBlank,
        ~pos=5,
        bs,
        "~___",
      )

      // 3-tuple, no blanks
      t(
        "deleting ( from a filled 3-tuple does nothing",
        tuple3WithNoBlanks,
        ~pos=0,
        del,
        "~(56,78,56)",
      )
      t(
        "deleting ) from a filled 3-tuple just moves cursor left",
        tuple3WithNoBlanks,
        ~pos=10, // just after )
        bs,
        "(56,78,56~)",
      )
      t(
        "deleting first , from a filled 3-tuple removes 2nd item",
        tuple3WithNoBlanks,
        ~pos=3, // just before ,
        del,
        "(56~,56)",
      )
      t(
        "deleting second , from a filled 3-tuple removes 3rd item",
        tuple3WithNoBlanks,
        ~pos=6, // just before ,
        del,
        "(56,78~)",
      )

      // 3-tuple, first blank
      t(
        "deleting ( from a 3-tuple with first item blank does nothing",
        tuple3WithFirstBlank,
        ~pos=0,
        del,
        "~(___,78,56)",
      )
      t(
        "deleting first , from a 3-tuple with first item blank removes 2nd item",
        tuple3WithFirstBlank,
        ~pos=4, // just before ,
        del,
        "(~___,56)",
      )
      t(
        "deleting second , from a 3-tuple with first item blank removes 3rd item",
        tuple3WithFirstBlank,
        ~pos=8, // just after ,
        bs,
        "(___,78~)",
      )
      t(
        "deleting ) from a 3-tuple with first item blank just moves cursor left",
        tuple3WithFirstBlank,
        ~pos=11, // just after )
        bs,
        "(___,78,56~)",
      )

      // 3-tuple, second blank
      t(
        "deleting ( from a 3-tuple with the second item blank does nothing",
        tuple3WithSecondBlank,
        ~pos=0,
        del,
        "~(56,___,78)",
      )
      t(
        "deleting first , from a 3-tuple with the second item blank removes the blank",
        tuple3WithSecondBlank,
        ~pos=3, // just before ,
        del,
        "(56~,78)",
      )
      t(
        "deleting second , from a 3-tuple with the second item blank removes 3rd item",
        tuple3WithSecondBlank, // (56,___,78)
        ~pos=7, // just before ,
        del,
        "(56,~___)",
      )
      t(
        "deleting ) from a 3-tuple with the second item blank just moves cursor left",
        tuple3WithSecondBlank,
        ~pos=11, // just after )
        bs,
        "(56,___,78~)",
      )

      // 3-tuple, third blank `(56,78,___)`
      t(
        "deleting ( from a 3-tuple with the third item blank does nothing",
        tuple3WithThirdBlank,
        ~pos=0,
        del,
        "~(56,78,___)",
      )
      t(
        "deleting first , from a 3-tuple with the third item blank removes the second item",
        tuple3WithThirdBlank,
        ~pos=3, // just before ,
        del,
        "(56~,___)", // or maybe 1char to the right of this
      )
      t(
        "deleting second , from a 3-tuple with the third item blank removes the blank",
        tuple3WithThirdBlank,
        ~pos=7, // just after ,
        bs,
        "(56,78~)",
      )
      t(
        "deleting ) from a 3-tuple with the third item blank just moves the cursor left",
        tuple3WithThirdBlank,
        ~pos=11, // just after )
        bs,
        "(56,78,___~)",
      )

      // 3-tuple, first non-blank `(56,___,___)`
      t(
        "deleting ( from a 3-tuple with only first item replaces it with that item",
        tuple3WithFirstFilled,
        ~pos=0,
        del,
        "~56",
      )
      t(
        "deleting first , from a 3-tuple with only first item filled ___",
        tuple3WithFirstFilled,
        ~pos=3, // just before ,
        del,
        "(56~,___)",
      )
      t(
        "deleting second , from a 3-tuple with only first item filled ___",
        tuple3WithFirstFilled,
        ~pos=8, // just after ,
        bs,
        "(56,~___)",
      )
      t(
        "deleting ) from a 3-tuple with only first item filled just moves cursor left",
        tuple3WithFirstFilled,
        ~pos=12, // just after )
        bs,
        "(56,___,___~)",
      )

      // 3-tuple, second non-blank `(___,56,___)`
      t(
        "deleting ( from a 3-tuple with only second item filled replaces the tuple with the item",
        tuple3WithSecondFilled,
        ~pos=0,
        del,
        "~56",
      )
      t(
        "deleting first , from a 3-tuple with only second item filled removes the second item",
        tuple3WithSecondFilled,
        ~pos=4, // just before ,
        del,
        "(~___,___)",
      )
      t(
        "deleting second , from a 3-tuple with only second item filled removes the ending blank",
        tuple3WithSecondFilled,
        ~pos=8, // just after ,
        bs,
        "(___,56~)",
      )
      t(
        "deleting ) from a 3-tuple with only second item filled just moves the cursor left",
        tuple3WithSecondFilled,
        ~pos=12, // just after )
        bs,
        "(___,56,___~)",
      )

      // 3-tuple, third non-blank `(___,___,56)`
      t(
        "deleting ( from a 3-tuple with only third item filled replaces the tuple with the item",
        tuple3WithThirdFilled,
        ~pos=0,
        del,
        "~56",
      )
      t(
        "deleting first , from a 3-tuple with only third item filled removes the second blank",
        tuple3WithThirdFilled,
        ~pos=4, // just before ,
        del,
        "(~___,56)",
      )
      t(
        "deleting second , from a 3-tuple with only third item filled removes the filled item",
        tuple3WithThirdFilled,
        ~pos=9, // just after ,
        bs,
        "(___,~___)",
      )
      t(
        "deleting ) from a 3-tuple with only third item filled just moves the cursor left",
        tuple3WithThirdFilled,
        ~pos=12, // just after )
        bs,
        "(___,___,56~)",
      )

      // 3-tuple, all blank `(___,___,___)`
      t(
        "deleting ( from a 3-tuple of all blanks replaces the tuple with a blank",
        tuple3WithAllBlank,
        ~pos=0,
        del,
        "~___",
      )
      t(
        "deleting first , from a 3-tuple of all blanks removes the second blank",
        tuple3WithAllBlank,
        ~pos=4, // just before ,
        del,
        "(~___,___)",
      )
      t(
        "deleting second , from a 3-tuple of all blanks removes the third blank",
        tuple3WithAllBlank,
        ~pos=9, // just after ,
        bs,
        "(___,~___)",
      )
      t(
        "deleting ) from a 3-tuple of all blanks just moves left",
        tuple3WithAllBlank,
        ~pos=13, // just after )
        bs,
        "(___,___,___~)", // I could see this instead turning into `~___`
      )
    })
  })

  describe("Records", () => {
    t("create record", b, ~pos=0, ins("{"), "{~}")
    t("inserting before a record is no-op", emptyRecord, ~pos=0, ins("5"), "~{}")
    // tStruct(
    //   "inserting before a record at top-level inserts left partial",
    //   emptyRecord,
    //   ~pos=0,
    //   list{InsertText("c")},
    //   leftPartial("c", emptyRecord),
    // )
    t("inserting space between empty record does nothing", emptyRecord, ~pos=1, space, "{~}")
    t(
      "inserting valid text in an empty record works",
      ~wrap=false,
      emptyRecord,
      ~pos=1,
      ins("f"),
      "{\n  f~ : ___\n}",
    )
    t(
      "inserting text in nested record gets correct position",
      ~wrap=false,
      listWithRecord,
      ~pos=2,
      ins("f"),
      "[{\n   f~ : ___\n }]",
    )
    t(
      "inserting space in empty record field does nothing",
      emptyRowRecord,
      ~pos=4,
      space,
      "{\n  ~*** : ___\n}",
    )
    t(
      "inserting space in empty record value does nothing",
      emptyRowRecord,
      ~pos=10,
      space,
      "{\n  *** : ~___\n}",
    )
    t(
      "pressing enter in an the start of empty record adds a new line",
      emptyRecord,
      ~pos=1,
      enter,
      "{\n  ~*** : ___\n}",
    )
    t("enter fieldname", emptyRowRecord, ~pos=4, ins("c"), "{\n  c~ : ___\n}")
    t(
      "move to the front of an empty record",
      emptyRowRecord,
      ~pos=13,
      key(K.GoToStartOfLine(DropSelection)),
      "{\n  ~*** : ___\n}",
    )
    t(
      "move to the end of an empty record",
      emptyRowRecord,
      ~pos=4,
      key(K.GoToEndOfLine(DropSelection)),
      "{\n  *** : ___~\n}",
    )
    t("cant enter invalid fieldname", emptyRowRecord, ~pos=4, ins("^"), "{\n  ~*** : ___\n}")
    t("backspacing open brace of empty record dels record", emptyRecord, ~pos=1, bs, "~___")
    t("backspacing close brace of empty record moves inside record", emptyRecord, ~pos=2, bs, "{~}")
    t("deleting open brace of empty record dels record", emptyRecord, ~pos=0, del, "~___")
    t("close brace at end of record is swallowed", emptyRecord, ~pos=1, ins("}"), "{}~")
    t("backspacing empty record field clears entry", emptyRowRecord, ~pos=4, bs, "{~}")
    t(
      "pressing enter at start to insert field places the caret correctly",
      functionWrappedEmptyRecord,
      ~pos=22,
      enter,
      "HttpClient::getv4\n  \"\"\n  {\n    ~*** : ___\n  }\n  {}",
    )
    t("appending to int in expr works", singleRowRecord, ~pos=11, ins("1"), "{\n  f1 : 561~\n}")
    t(
      "appending to int in expr works",
      multiRowRecord,
      ~pos=21,
      ins("1"),
      "{\n  f1 : 56\n  f2 : 781~\n}",
    )
    t(
      "move to the front of a record with multiRowRecordple values",
      multiRowRecord,
      ~pos=21,
      key(K.GoToStartOfLine(DropSelection)),
      "{\n  f1 : 56\n  ~f2 : 78\n}",
    )
    t(
      "move to the end of a record with multiRowRecordple values",
      multiRowRecord,
      ~pos=14,
      key(K.GoToEndOfLine(DropSelection)),
      "{\n  f1 : 56\n  f2 : 78~\n}",
    )
    t("inserting at the end of the key works", emptyRowRecord, ~pos=6, ins("f"), "{\n  f~ : ___\n}")
    t(
      "pressing enter at start adds a row",
      multiRowRecord,
      ~pos=1,
      enter,
      "{\n  ~*** : ___\n  f1 : 56\n  f2 : 78\n}",
    )
    t(
      "pressing enter at the back adds a row",
      multiRowRecord,
      ~pos=22,
      enter,
      "{\n  f1 : 56\n  f2 : 78\n  *** : ___\n~}",
    )
    t(
      "pressing enter at the start of a field adds a row",
      multiRowRecord,
      ~pos=14,
      enter,
      "{\n  f1 : 56\n  *** : ___\n  ~f2 : 78\n}",
    )
    t(
      "pressing enter at the start of a field adds a row to the correct expression",
      record(list{("", match'(b, list{(pInt(5), int(6))})), ("asd", b)}),
      ~pos=39,
      enter,
      "{\n  *** : match ___\n          5 -> 6\n  *** : ___\n  ~asd : ___\n}",
    )
    t(
      "pressing enter at the end of row adds a row",
      multiRowRecord,
      ~pos=11,
      enter,
      "{\n  f1 : 56\n  ~*** : ___\n  f2 : 78\n}",
    )
    t(
      "dont allow weird chars in recordFieldnames",
      emptyRowRecord,
      ~pos=4,
      ins(")"),
      "{\n  ~*** : ___\n}",
    )
    t(
      "dont jump in recordFieldnames with infix chars",
      emptyRowRecord,
      ~pos=4,
      ins("+"),
      "{\n  ~*** : ___\n}",
    )
    t(
      "dont jump in recordFieldnames with infix chars, pt 2",
      singleRowRecord,
      ~pos=6,
      ins("+"),
      "{\n  f1~ : 56\n}",
    )
    t(
      "colon should skip over the record colon",
      emptyRowRecord,
      ~pos=7,
      ins(":"),
      "{\n  *** : ~___\n}",
    )
    t(
      "dont allow key to start with a number",
      emptyRowRecord,
      ~pos=4,
      ins("5"),
      "{\n  ~*** : ___\n}",
    )
    t(
      "dont allow key to start with a number, pt 2",
      singleRowRecord,
      ~pos=4,
      ins("5"),
      "{\n  ~f1 : 56\n}",
    )
    t(
      "dont allow key to start with a number, pt 3",
      emptyRowRecord,
      ~pos=4,
      ins("5"),
      "{\n  ~*** : ___\n}",
    )
    t(
      "hyphens are allowed in records",
      emptyRowRecord,
      ~pos=4,
      insMany(list{"x", "-"}),
      "{\n  x-~ : ___\n}",
    )
    t(
      "ctrl+left at beg of value movese to beg of key",
      multiRowRecord,
      ~pos=9,
      ctrlLeft,
      "{\n  ~f1 : 56\n  f2 : 78\n}",
    )
    t(
      "ctrl+right at end of key moves to end of value",
      multiRowRecord,
      ~pos=6,
      ctrlRight,
      "{\n  f1 : 56~\n  f2 : 78\n}",
    )
    t(
      "Replace text when selecting over record",
      record(list{("f1", fiftySix), ("f2", seventyEight)}),
      ~sel=(10, 21),
      inputs(list{InsertText("5")}),
      "{\n  f1 : 55~\n}",
    )
    t(
      "Replace text remove selected text when inserting wrong type",
      record(list{("f1", fiftySix), ("f2", seventyEight)}),
      ~sel=(10, 21),
      inputs(list{InsertText("a")}),
      "{\n  f1 : 5~\n}",
    )
  })
  describe("Autocomplete", () => {
    // Note that many of these autocomplete tests use ~clone:false
    // because they rely on specific IDs (update: not sure why this is?)
    t(
      "space autocompletes correctly",
      partial("if", b),
      ~pos=2,
      space,
      "if ~___\nthen\n  ___\nelse\n  ___",
    )
    t("let moves to right place", partial("let", b), ~pos=3, enter, "let ~*** = ___\n___")
    t(
      "autocomplete space moves forward by 1",
      aBinOp,
      ~pos=0,
      inputs(list{InsertText("r"), keypress(K.Space)}),
      "request ~== _________",
    )
    t(
      "autocomplete enter moves to end of value",
      aBinOp,
      ~pos=0,
      inputs(list{InsertText("r"), keypress(K.Enter)}),
      "request~ == _________",
    )
    t("can tab to lambda blank", aLambda, ~pos=0, key(K.Tab), "\\~*** -> ___")
    t(
      "autocomplete tab moves to next blank",
      aBinOp,
      ~pos=0,
      inputs(list{InsertText("r"), keypress(K.Tab)}),
      "request == ~_________",
    )
    t(
      "autocomplete enter on bin-op moves to start of first blank",
      b,
      ~pos=0,
      inputs(list{InsertText("="), keypress(K.Enter)}),
      "~_________ == _________",
    )
    t(
      "autocomplete enter on function with parameters moves to start of first blank",
      partial("sqrt", b),
      ~pos=4,
      enter,
      "Int::sqrt ~_________",
    )
    t(
      "autocomplete enter on niladic function moves to end of function",
      partial("empty", b),
      ~pos=5,
      enter,
      "List::empty~",
    )
    t(
      "autocomplete tab on bin-op moves to start of second blank",
      b,
      ~pos=0,
      inputs(list{InsertText("="), keypress(K.Tab)}),
      "_________ == ~_________",
    )
    t(
      "autocomplete space on bin-op moves to start of first blank",
      b,
      ~pos=0,
      inputs(list{InsertText("="), keypress(K.Space)}),
      "~_________ == _________",
    )
    t("variable moves to right place", partial("req", b), ~pos=3, enter, "request~")
    t(
      "pipe moves to right place on blank",
      b,
      ~pos=2,
      inputs(list{InsertText("|"), InsertText(">"), keypress(K.Enter)}),
      "___\n|>~___\n",
    )
    t(
      "pipe moves to right place on placeholder",
      aFnCall,
      ~pos=11,
      inputs(list{InsertText("|"), InsertText(">"), keypress(K.Enter)}),
      "Int::add 5 _________\n|>~___\n",
    )
    t(
      "pipe moves to right place in if then",
      emptyIf,
      ~pos=14,
      inputs(list{InsertText("|"), InsertText(">"), keypress(K.Enter)}),
      "if ___\nthen\n  ___\n  |>~___\nelse\n  ___",
    )
    t(
      "pipe moves to right place in lambda body",
      aLambda,
      ~pos=8,
      inputs(list{InsertText("|"), InsertText(">"), keypress(K.Enter)}),
      "\\*** -> ___\n        |>~___\n",
    )
    t(
      "pipe moves to right place in match body",
      emptyMatch,
      ~pos=19,
      inputs(list{InsertText("|"), InsertText(">"), keypress(K.Enter)}),
      "match ___\n  *** -> ___\n         |>~___\n",
    )
    t(
      "shift enter autocompletes and creates pipe",
      partial("empty", b),
      ~pos=5,
      key(K.ShiftEnter),
      "List::empty\n|>~___\n",
    )
    t(
      "shift enter in a field works correctly",
      ~clone=false,
      EPartial(gid(), "bo", EFieldAccess(gid(), EVariable(fakeID1, "request"), "")),
      ~pos=10,
      key(K.ShiftEnter),
      "request.body\n|>~___\n",
    )
    t(
      "shift enter in pipe autocompletes and creates pipe",
      ~wrap=false,
      pipe(list(list{}), partial("appe", b), list{}),
      ~pos=9,
      key(K.ShiftEnter),
      "[]\n|>List::append ___________\n|>~___\n",
    )
    t("autocomplete for Just", partial("Just", b), ~pos=4, enter, "Just ~___")
    t("autocomplete for Ok", partial("Ok", b), ~pos=2, enter, "Ok ~___")
    t("autocomplete for Nothing", partial("Nothing", b), ~pos=7, enter, "Nothing~")
    t(
      "autocomplete for Nothing at end of a line",
      if'(b, partial("Nothing", b), b),
      ~pos=21,
      space,
      "if ___\nthen\n  Nothing~\nelse\n  ___",
    )
    t("autocomplete for Error", partial("Error", b), ~pos=5, enter, "Error ~___")
    t(
      "autocomplete for field",
      ~clone=false,
      EPartial(gid(), "bo", EFieldAccess(gid(), EVariable(fakeID1, "request"), "")),
      ~pos=10,
      enter,
      "request.body~",
    )
    t(
      "autocomplete shows first alphabetical item for fields",
      ~clone=false,
      let'("request", int(5), EVariable(fakeID2, "request")),
      ~pos=23,
      inputs(list{InsertText("."), keypress(K.Enter)}),
      "let request = 5\nrequest.author~",
    )
    t(
      "autocomplete doesn't stick on the first alphabetical item for fields, when it refines further",
      ~clone=false,
      let'("request", int(5), EVariable(fakeID2, "request")),
      ~pos=23,
      inputs(list{InsertText("."), InsertText("t"), keypress(K.Enter)}),
      "let request = 5\nrequest.title~",
    )
    t(
      "autocomplete for field autocommits",
      ~clone=false,
      let'("x", partial("body", fieldAccess(EVariable(fakeID1, "request"), "longfield")), b),
      // Right should make it commit
      ~pos=20,
      key(K.Right),
      "let x = request.body\n~___",
    )
    t(
      "down works on autocomplete for fields",
      ~clone=false,
      let'("x", partial("body", fieldAccess(EVariable(fakeID1, "request"), "longfield")), b),
      ~pos=16,
      keys(list{K.Down, K.Enter}),
      "let x = request.formBody~\n___",
    )
    t(
      "autocomplete for field is committed by dot",
      ~clone=false,
      ~expectsPartial=true,
      EPartial(gid(), "bod", EFieldAccess(gid(), EVariable(fakeID1, "request"), "longfield")),
      // Dot should select the autocomplete
      ~pos=11,
      ins("."),
      "request.body.~***",
    )
    t(
      ~expectsPartial=true,
      "pressing + doesn't introduce two pluses (check committing bug)",
      partial("asdasd", b) /* Right should make it commit */,
      ~pos=3,
      ins("+"),
      "asd+~asd",
    )
    t(
      "autocomplete with space moves to next non-whitespace rather than blank",
      ~clone=false,
      ~pos=105,
      ELet(
        gid(),
        "request",
        ERecord(gid(), list{("body", EInteger(gid(), 5L)), ("blank", EBlank(gid()))}),
        ELet(
          gid(),
          "foo",
          EPartial(gid(), "bo", EFieldAccess(gid(), EVariable(fakeID3, "request"), "")),
          EVariable(gid(), "foo"),
        ),
      ),
      space,
      "let request = {\n                body : 5\n                blank : ___\n              }\nlet foo = request.body~\nfoo",
    )
    t(
      "autocomplete with tab in presence of no blanks places caret at end of autocompleted thing",
      ~clone=false,
      ELet(
        gid(),
        "request",
        ERecord(gid(), list{("body", EInteger(gid(), 5L))}),
        ELet(
          gid(),
          "foo",
          EPartial(gid(), "bo", EFieldAccess(gid(), EVariable(fakeID3, "request"), "")),
          EVariable(gid(), "foo"),
        ),
      ),
      ~pos=77,
      key(K.Tab),
      "let request = {\n                body : 5\n              }\nlet foo = request.body~\nfoo",
    )
    t(
      "Deleting then re-entering an infix op still alows you to select from ac",
      binop("+", int(4), b),
      ~pos=3,
      inputs(list{DeleteContentBackward, InsertText("+"), keypress(K.Enter)}),
      "4 + ~_________",
    )
    t(
      "putting a comma does autocomplete, creates a new element on the list and jumps to that one",
      list(list{aPartialVar}),
      ~pos=4,
      inputs(list{InsertText(",")}),
      "[request,~___]",
    )
    t(
      "putting a comma does autocomplete, creates a new element on the list and jumps to that one in the middle of the list",
      list(list{int(1), aPartialVar, int(3)}),
      ~pos=6,
      inputs(list{InsertText(",")}),
      "[1,request,~___,3]",
    )
    test("click into partial opens autocomplete", () => {
      let ast = let'("request", aShortInt, aPartialVar)
      let h = FluidUtils.h(ast)
      let m = {...defaultTestModel, handlers: Handlers.fromList(list{h})}
      let tlid = h.tlid
      expect({
        let (_, newState, _) = updateMsg(
          m,
          tlid,
          h.ast,
          m.fluidState,
          FluidMouseUp({
            tlid: tlid,
            editor: MainEditor(TLID.fromInt(7)),
            selection: ClickAt(18),
          }),
        )

        newState.ac.index
      }) |> toEqual(Some(0))
    })
    test("Backspace over binop resets upDownCol but not autocomplete", () => {
      let ast = binop("+", aShortInt, b)
      let h = FluidUtils.h(ast)
      let m = {...defaultTestModel, handlers: Handlers.fromList(list{h})}
      let tlid = h.tlid
      expect({
        let (_, newState, _) = updateMsg(
          m,
          tlid,
          h.ast,
          {...defaultTestState, newPos: 3},
          FluidInputEvent(DeleteContentBackward),
        )

        let (_, newState, _) = updateMsg(m, tlid, h.ast, newState, FluidInputEvent(InsertText("+")))

        (newState.ac.index, newState.upDownCol)
      }) |> toEqual((Some(0), None))
    })
    test("backspace on partial will open AC if query matches", () => {
      let ast = FluidAST.ofExpr(let'("request", aShortInt, aPartialVar))
      let astInfo =
        ASTInfo.make(ast, defaultTestState)
        |> moveTo(19)
        |> updateKey(defaultTestProps, keypress(K.Down))
        |> processMsg(list{DeleteContentBackward})

      let result = ASTInfo.activeTokenInfos(astInfo) |> Printer.tokensToString

      expect((result, astInfo.state.ac.index)) |> toEqual(("let request = 1\nre", Some(0)))
    })
    // TODO: this doesn't work but should
    // t
    // "autocomplete for field in body"
    // (EMatch
    // ( gid ()
    // , EFieldAccess (gid (), EVariable (fakeID1, "request"), gid (), "bo")
    // , [] ))
    // (enter 18)
    // ("match request.body", 18) ;
    // test "backspacing on variable reopens autocomplete" (fun () ->
    // expect (bs (EVariable (5, "request"))).
  })
  describe("Movement", () => {
    let s = defaultTestState
    let tokens = FluidTokenizer.tokenize(compoundExpr)
    let len = tokens |> List.map(~f=(ti: T.tokenInfo) => ti.token) |> length
    let ast = compoundExpr |> FluidAST.ofExpr
    let astInfo = ASTInfo.make(ast, defaultTestState)
    test("gridFor - 1", () => expect(gridFor(~pos=116, tokens)) |> toEqual({row: 2, col: 2}))
    test("gridFor - 2", () => expect(gridFor(~pos=70, tokens)) |> toEqual({row: 0, col: 70}))
    test("gridFor - 3", () => expect(gridFor(~pos=129, tokens)) |> toEqual({row: 2, col: 15}))
    test("gridFor - start of line", () =>
      expect(gridFor(~pos=130, tokens)) |> toEqual({row: 3, col: 0})
    )
    test("gridFor - in an indent", () =>
      expect(gridFor(~pos=158, tokens)) |> toEqual({row: 5, col: 1})
    )
    test("gridFor - (reverse) in an indent", () =>
      expect(posFor(~row=5, ~col=1, tokens)) |> toEqual(158)
    )
    test("gridFor roundtrips", () => {
      let poses = List.range(0, len)
      let newPoses = List.map(poses, ~f=pos => {
        let {row, col}: Fluid.gridPos = gridFor(~pos, tokens)
        posFor(~row, ~col, tokens)
      })

      expect(poses) |> toEqual(newPoses)
    })
    t(
      "right skips over indent when in indent",
      emptyIf,
      ~pos=12,
      key(K.Right),
      "if ___\nthen\n  ___~\nelse\n  ___",
    )
    t(
      "left skips over indent when in indent",
      emptyIf,
      ~pos=13,
      key(K.Left),
      "if ___\nthen~\n  ___\nelse\n  ___",
    )
    describe("length", () => {
      test("up from first row is zero", () =>
        expect(astInfo |> doUp(~pos=5) |> (({state, _}) => state.newPos)) |> toEqual(0)
      )
      test("down from first row is end of last row", () =>
        expect(astInfo |> doDown(~pos=168) |> (({state, _}) => state.newPos)) |> toEqual(174)
      )
      // end of short row
      test("up into shorter row goes to end of row", () =>
        expect(astInfo |> doUp(~pos=172) |> (({state, _}) => state.newPos)) |> toEqual(156)
      )
      test("down into shorter row goes to end of row", () =>
        expect(astInfo |> doDown(~pos=143) |> (({state, _}) => state.newPos)) |> toEqual(156)
      )
      // start of indented row
      test("up into indented row goes to first token", () =>
        expect(astInfo |> doUp(~pos=152) |> (({state, _}) => state.newPos)) |> toEqual(130)
      )
      test("down into indented row goes to first token", () =>
        expect(astInfo |> doDown(~pos=109) |> (({state, _}) => state.newPos)) |> toEqual(114)
      )
    })
    t(
      "enter at the end of a line goes to first non-whitespace token",
      indentedIfElse,
      ~pos=16,
      enter,
      "let var = if ___\n" ++
      ("          ~then\n" ++
      ("            6\n" ++ ("          else\n" ++ ("            7\n" ++ "var")))),
    )
    t(
      "end of if-then blank goes up properly",
      emptyIf,
      ~pos=17,
      keys(list{K.Escape, K.Up}),
      "if ___\nthen~\n  ___\nelse\n  ___",
    )
    t(
      "end of if-then blank goes up properly, twice",
      emptyIf,
      ~pos=17,
      keys(list{K.Escape, K.Up, K.Up}),
      "if __~_\nthen\n  ___\nelse\n  ___",
    )
    t(
      "end of if-then blank goes down properly",
      emptyIf,
      ~pos=5,
      keys(list{K.Escape, K.Down}),
      "if ___\nthen~\n  ___\nelse\n  ___",
    )
    t(
      "end of if-then blank goes down properly, twice",
      emptyIf,
      ~pos=5,
      keys(list{K.Escape, K.Down, K.Down}),
      "if ___\nthen\n  ___~\nelse\n  ___",
    )
    // moving through the autocomplete
    test("up goes through the autocomplete", () =>
      expect(
        astInfo
        |> moveTo(143)
        |> updateKey(defaultTestProps, keypress(K.Up))
        |> updateKey(defaultTestProps, keypress(K.Up))
        |> updateKey(defaultTestProps, keypress(K.Up))
        |> (astInfo => astInfo.state.newPos),
      ) |> toEqual(13)
    )
    test("down goes through the autocomplete", () =>
      expect(
        moveTo(14, astInfo)
        |> updateKey(defaultTestProps, keypress(K.Down))
        |> updateKey(defaultTestProps, keypress(K.Down))
        |> updateKey(defaultTestProps, keypress(K.Down))
        |> (astInfo => astInfo.state.newPos),
      ) |> toEqual(144)
    )
    test("clicking away from autocomplete commits", () =>
      expect({
        let expr = let'("var", partial("false", b), b)
        ASTInfo.setAST(FluidAST.ofExpr(expr), astInfo)
        |> moveTo(14)
        |> (astInfo => {
          let h = FluidUtils.h(FluidAST.toExpr(astInfo.ast))
          let m = {...defaultTestModel, handlers: Handlers.fromList(list{h})}

          updateAutocomplete(m, h.tlid, astInfo)
        })
        |> updateMouseClick(defaultTestProps, 0)
        |> (
          astInfo =>
            switch FluidAST.toExpr(astInfo.ast) {
            | ELet(_, _, EBool(_, false), _) => "success"
            | e => Printer.eToStructure(e)
            }
        )
      }) |> toEqual("success")
    )
    t(
      "moving right off a function autocompletes it anyway",
      let'("x", partial("Int::add", b), b),
      ~pos=16,
      key(K.Right),
      "let x = Int::add ~_________ _________\n___",
    )
    t(
      ~expectsPartial=true,
      "pressing an infix which could be valid doesn't commit",
      b,
      ~pos=0,
      insMany(list{"|", "|"}),
      "||~",
    )
    t(
      ~expectsPartial=true,
      "pressing an infix after true commits it ",
      partial("true", b),
      ~pos=4,
      ins("+"),
      "true +~",
    )
    t(
      "moving left off a function autocompletes it anyway",
      let'("x", partial("Int::add", b), b),
      ~pos=8,
      key(K.Left),
      "let x =~ Int::add _________ _________\n___",
    )
    test("escape hides autocomplete", () =>
      expect(
        ASTInfo.make(FluidAST.ofExpr(b), s)
        |> moveTo(0)
        |> updateKey(defaultTestProps, InsertText("r"))
        |> updateKey(defaultTestProps, keypress(K.Escape))
        |> (astInfo => astInfo.state.ac.index),
      ) |> toEqual(None)
    )
    test("right/left brings back autocomplete", () => {
      let astInfo = ASTInfo.make(FluidAST.ofExpr(b), s)
      expect(
        moveTo(0, astInfo)
        |> updateKey(defaultTestProps, InsertText("r"))
        |> updateKey(defaultTestProps, keypress(K.Escape))
        |> (astInfo => astInfo.state.ac.index),
      ) |> toEqual(None)
    })
  })
  describe("Line-based Deletion", () => {
    t(
      "DeleteSoftLineBackward with selection deletes just the selection",
      ~sel=(66, 115),
      {
        let veryLongString = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"

        fn(
          "HttpClient::post_v4",
          list{emptyStr, record(list{("data", str(veryLongString))}), emptyRecord, emptyRecord},
        )
      },
      inputs(list{DeleteSoftLineBackward}),
      "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz~1234567890abcd\n            efghijklmnopqrstuvwxyz\"\n  }\n  {}\n  {}",
    )
    t(
      "DeleteSoftLineForward with selection deletes just the selection",
      ~sel=(66, 115),
      {
        let veryLongString = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"

        fn(
          "HttpClient::post_v4",
          list{emptyStr, record(list{("data", str(veryLongString))}), emptyRecord, emptyRecord},
        )
      },
      inputs(list{DeleteSoftLineForward}),
      "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz~1234567890abcd\n            efghijklmnopqrstuvwxyz\"\n  }\n  {}\n  {}",
    )
    t(
      "DeleteSoftLineBackward with no selection deletes to visual start of line",
      {
        let veryLongString = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"

        fn(
          "HttpClient::post_v4",
          list{emptyStr, record(list{("data", str(veryLongString))}), emptyRecord, emptyRecord},
        )
      },
      ~pos=66,
      inputs(list{DeleteSoftLineBackward}),
      "HttpClient::postv4\n  \"\"\n  {\n    ~*** : \"1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234\n           567890abcdefghijklmnopqrstuvwxyz\"\n  }\n  {}\n  {}",
    )
    t(
      "DeleteSoftLineForward with no selection deletes to visual end of line",
      {
        let veryLongString = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"

        fn(
          "HttpClient::post_v4",
          list{emptyStr, record(list{("data", str(veryLongString))}), emptyRecord, emptyRecord},
        )
      },
      ~pos=66,
      inputs(list{DeleteSoftLineForward}),
      "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz~EFGHIJKLMNOPQR\n            STUVWXYZ1234567890abcdefghijklmnopqrstuv\n            wxyz\"\n  }\n  {}\n  {}",
    )
    t(
      "DeleteSoftLineBackward deletes up to line start at the end of a wrapping string",
      {
        let veryLongString = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefghijklmnopqrstuvwxyz"

        fn(
          "HttpClient::post_v4",
          list{emptyStr, record(list{("data", str(veryLongString))}), emptyRecord, emptyRecord},
        )
      },
      ~pos=165,
      inputs(list{DeleteSoftLineBackward}),
      "HttpClient::postv4\n  \"\"\n  {\n    data : \"abcdefghijklmnopqrstuvwxyz1234567890ABCD\n            EFGHIJKLMNOPQRSTUVWXYZ1234567890abcdefgh~\"\n  }\n  {}\n  {}",
    )
  })
  describe("Selection Movement", () => {
    t(
      "shift right selects",
      longLets,
      modkeys(list{
        (
          K.Right,
          {
            shiftKey: true,
            altKey: false,
            metaKey: false,
            ctrlKey: false,
          },
        ),
      }),
      "»let «firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "shift down selects",
      longLets,
      ~pos=4,
      modkeys(list{
        (
          K.Down,
          {
            shiftKey: true,
            altKey: false,
            metaKey: false,
            ctrlKey: false,
          },
        ),
      }),
      "let »firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet «secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "shift left selects",
      longLets,
      ~pos=52,
      modkeys(list{
        (
          K.Left,
          {
            shiftKey: true,
            altKey: false,
            metaKey: false,
            ctrlKey: false,
          },
        ),
      }),
      "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\n«let »secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "keypress on selection drops selection",
      longLets,
      ~sel=(0, 13),
      key(K.Left),
      "~let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "shiftless left aborts left-to-right selection on left",
      longLets,
      ~sel=(4, 52),
      key(K.Left),
      "let ~firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "shiftless left aborts right-to-left selection on left",
      longLets,
      ~sel=(52, 4),
      key(K.Left),
      "let ~firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "shiftless right aborts left-to-right selection on right",
      longLets,
      ~sel=(4, 52),
      key(K.Right),
      "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet ~secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "shiftless right aborts right-to-left selection on right",
      longLets,
      ~sel=(52, 4),
      key(K.Right),
      "let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet ~secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "selecting an expression LTR pipes from it",
      binop("+", int(4), int(5)),
      ~sel=(4, 5),
      key(~shiftHeld=true, K.ShiftEnter),
      "4 + 5\n    |>~___\n",
    )
    t(
      "selecting an expression RTL pipes from it",
      binop("+", int(4), int(5)),
      ~sel=(5, 4),
      key(~shiftHeld=true, K.ShiftEnter),
      "4 + 5\n    |>~___\n",
    )
    t(
      "K.ShiftEnter doesn't persist selection",
      anInt,
      ~sel=(0, 5),
      key(~shiftHeld=true, K.ShiftEnter),
      "12345\n|>~___\n",
    )
    t(
      "K.SelectAll selects all",
      longLets,
      ~pos=4,
      key(K.SelectAll),
      "»let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"«",
    )
    t(
      "K.GoToStartOfWord + shift selects to start of word",
      longLets,
      ~pos=16,
      key(K.GoToStartOfWord(KeepSelection)),
      "let «firstLetName» = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "K.GoToEndOfWord selects to end of word",
      longLets,
      ~pos=4,
      key(K.GoToEndOfWord(KeepSelection)),
      "let »firstLetName« = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "K.GoToStartOfLine selects from mid to start of line",
      longLets,
      ~pos=29,
      key(K.GoToStartOfLine(KeepSelection)),
      "«let firstLetName = \"ABCDEFGHI»JKLMNOPQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "K.GoToEndOfLine selects from mid to end of line",
      longLets,
      ~pos=29,
      key(K.GoToEndOfLine(KeepSelection)),
      "let firstLetName = \"ABCDEFGHI»JKLMNOPQRSTUVWXYZ\"«\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "K.GoToStartOfLine selects from end to start of line",
      longLets,
      ~pos=47,
      key(K.GoToStartOfLine(KeepSelection)),
      "«let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"»\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "K.GoToEndOfLine selects to end of line",
      longLets,
      ~pos=0,
      key(K.GoToEndOfLine(KeepSelection)),
      "»let firstLetName = \"ABCDEFGHIJKLMNOPQRSTUVWXYZ\"«\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
    t(
      "Replace text in let if text is inserted with selection",
      longLets,
      ~sel=(9, 35),
      inputs(list{InsertText("a")}),
      "let firsta~ = \"PQRSTUVWXYZ\"\nlet secondLetName = \"0123456789\"\n\"RESULT\"",
    )
  })
  describe("Neighbours", () => {
    test("with empty AST, have left neighbour", () => {
      open FluidTokenizer
      let id = ID.fromInt(543)
      let token = FluidTypes.Token.TString(id, "test", None)
      let ti: T.tokenInfo = {
        token: token,
        startRow: 0,
        startCol: 1,
        startPos: 1,
        endPos: 5,
        length: 4,
      }
      let nextToken = FluidTypes.Token.TStringCloseQuote(id, "test")
      let nextTI: T.tokenInfo = {
        token: nextToken,
        startRow: 0,
        startCol: 5,
        startPos: 5,
        endPos: 6,
        length: 1,
      }

      let ast = EString(id, "test")
      let tokens = tokenize(ast)
      expect(getNeighbours(~pos=3, tokens)) |> toEqual((L(token, ti), R(token, ti), Some(nextTI)))
    })
  })
  describe("Tabs", () => {
    t("tab goes to first block in a let", emptyLet, key(K.Tab), "let ~*** = ___\n5")
    t("tab goes when on blank", completelyEmptyLet, ~pos=10, key(K.Tab), "let *** = ___\n~___")
    t("tab goes to second block in a let", emptyLet, ~pos=4, key(K.Tab), "let *** = ~___\n5")
    t(
      "tab wraps second block in a let",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      emptyLet,
      ~pos=15,
      key(K.Tab),
      "let ~*** = ___\n5",
    )
    t("shift tab goes to last block in a let", emptyLet, ~pos=14, shiftTab, "let *** = ~___\n5")
    t("shift tab goes to previous block in a let", emptyLet, ~pos=10, shiftTab, "let ~*** = ___\n5")
    t(
      "shift tab completes autocomplete",
      completelyEmptyLet,
      ~pos=14,
      inputs(list{InsertText("i"), InsertText("f"), keypress(K.ShiftTab)}),
      "let *** = ~___\nif ___\nthen\n  ___\nelse\n  ___",
    )
    t("shift-tab goes when on blank", completelyEmptyLet, ~pos=14, shiftTab, "let *** = ~___\n___")
    t(
      "shift tab wraps from start of let",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      emptyLet,
      ~pos=4,
      shiftTab,
      "let *** = ___\n5~",
    )
    t(
      "shift tab goes to last blank in editor",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      nonEmptyLetWithBlankEnd,
      ~pos=4,
      shiftTab,
      "let *** = 6\n~___",
    )
    t("cant tab to filled letLHS", letWithLhs, key(K.Tab), "let ~n = 6\n5")
    t("can tab to lambda blank", aLambda, key(K.Tab), "\\~*** -> ___")
    t(
      "can shift tab to field blank",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      aBlankField,
      shiftTab,
      "obj.~***",
    )
    t(
      "shift tab at beg of line, wraps to end",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      longList,
      ~pos=1,
      shiftTab,
      "[56,78,56,78,56,78~]",
    )
    t(
      "tab at end of line, wraps to beginning",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      aFnCall,
      ~pos=11,
      key(K.Tab),
      "~Int::add 5 _________",
    )
    t(
      "tab at end of line, wraps to beginning",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      multiList,
      ~pos=6,
      key(K.Tab),
      "[~56,78]",
    )
    t(
      "tab does not go to middle of multiline string",
      mlStrWSpace,
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~pos=0,
      key(K.Tab),
      "~\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "  123456789_ abcdefghi, 123456789_ abcdef\n" ++ " ghi,\"",
    )
    t(
      "tab does not stop on function version",
      aFnCallWithVersion,
      ~pos=0,
      key(K.Tab),
      "DB::getAllv1 ~___________________",
    )
    t(
      "shift tab does not go to middle of multiline string",
      mlStrWSpace,
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      shiftTab,
      "\"123456789_abcdefghi,123456789_abcdefghi,\n" ++
      "  123456789_ abcdefghi, 123456789_ abcdef\n" ++ " ghi,\"~",
    )
    t(
      "shift tab does not stop on function version",
      ~wrap=false,
      // wrap false because else we move the cursor into the wrapper
      ~brokenInFF=true,
      // brokenInFF false because else we move the cursor into the ff condition
      aFnCallWithVersion,
      shiftTab,
      "DB::getAllv1 ~___________________",
    )
  })
  // Disable string escaping for now
  // describe "String escaping" (fun () -> ()) ;
  // t ~expectsPartial:true
  // "typing \\ in a string makes it a partial"
  // aStr
  // (key K.Backslash 3)
  /* TODO this works in a handler with _only_ a string, but if you wrap it in
   * anything else (let, if, etc) you get "some string\\~" */
  // "so\\~me string" ;
  // t
  // "typing n after an escape in a partial creates a newline"
  // aStrEscape
  // (ins 'n' 3)
  // "\"so\n~me string\"" ;
  // t
  // "typing \\ after an escape in a partial creates a visible backslash"
  // aStrEscape
  // (key K.Backslash 3)
  // "\"so\\~me string\"" ;
  /* TODO this doesn't work yet, filed as
   * https://trello.com/c/kBsS9Qb2/2156-string-escaping-should-work-for-repeated-backslashes
  t ~expectsPartial:true
    "typing \\ after an escaped backslash in a partial creates a visible backslash and back to a partial"
    aStrEscape
    (keys [K.Backslash; K.Backslash] 3)
    "so\\\\\\~me string" ;
 */
  /* TODO this doesn't work yet, filed as
   * https://trello.com/c/kBsS9Qb2/2156-string-escaping-should-work-for-repeated-backslashes
  t ~expectsPartial:true
    "typing \\\\ results two visible backslashes"
    aStr
    (keys [K.Backslash; K.Backslash; K.Backslash; K.Backslash] 3)
    "\"so\\\\~me string\"" ;
 */
  // Disable string escaping for now
  // t
  // "deleting the \\ in a partial brings back the string"
  // aStrEscape
  // (del 2)
  // "\"so~me string\"" ;
  t(
    ~expectsPartial=true,
    "typing an unsupported char after an escape leaves us with a partial",
    aStrEscape,
    ~pos=3,
    ins("f"),
    "so\\f~me string",
  )
  /* Not quite a regression, in that I noticed it pre-review, but still a thing
   * to check */
  t(
    ~expectsPartial=true,
    "typing and then deleting an unsupported char after an escape leaves us with a partial with the caret in the right place",
    aStrEscape,
    ~pos=3,
    inputs(list{InsertText("f"), DeleteContentBackward}),
    "so\\~me string",
  )
  describe("Feature Flags", () =>
    t(
      ~brokenInFF=true,
      "feature flag renders old code with no change",
      letWithflagBody,
      render,
      "~let a = 1\n\"c\"",
    )
  )
}
