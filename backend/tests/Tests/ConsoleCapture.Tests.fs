/// Tests for `NonBlockingConsole`'s capture window.
///
/// The window is flow-scoped (an `AsyncLocal`), and the case worth pinning is stopping
/// one from a NESTED flow. The CLI test runner evaluates each test in a nested flow, so
/// when a `Stop` reached from there cleared only the child's slot, the flow that opened
/// the window still saw it open and every later write vanished into a buffer nobody read:
/// `dark test` printed nothing after the first test that tried a denied `printLine`,
/// summary included, while the remaining tests still ran.
module Tests.ConsoleCapture

open Expecto
open Prelude

/// Run `f` on a genuinely separate async flow, the way a nested evaluation does, and
/// wait for it. `Task.Run` gives `f` a child ExecutionContext, which is what makes an
/// `AsyncLocal` write inside it invisible to this flow.
let private onNestedFlow (f : unit -> 'a) : 'a =
  System.Threading.Tasks.Task.Run(f).GetAwaiter().GetResult()


let captureTests =
  testList
    "capture window"
    [ test "a window captures what is written while it is open" {
        Expect.isTrue (NonBlockingConsole.startCapture ()) "opened"
        NonBlockingConsole.writeLine "inside"
        let captured = NonBlockingConsole.stopCapture ()
        Expect.equal captured "inside\n" "captured the write"
      }

      test "nesting is refused, and the outer window keeps its output" {
        Expect.isTrue (NonBlockingConsole.startCapture ()) "opened"
        NonBlockingConsole.writeLine "outer"
        Expect.isFalse (NonBlockingConsole.startCapture ()) "second start refused"
        let captured = NonBlockingConsole.stopCapture ()
        Expect.equal captured "outer\n" "the outer window still owns its output"
      }

      test "a window can be reopened once it is stopped" {
        Expect.isTrue (NonBlockingConsole.startCapture ()) "opened"
        NonBlockingConsole.stopCapture () |> ignore<string>
        Expect.isTrue (NonBlockingConsole.startCapture ()) "reopened"
        NonBlockingConsole.stopCapture () |> ignore<string>
      }

      test "stopping when nothing is open returns empty rather than raising" {
        Expect.equal (NonBlockingConsole.stopCapture ()) "" "no window, no output"
      }

      // The regression. `Stop` has to be visible to the flow that opened the window,
      // not only to the flow that called it.
      test "a stop from a nested flow closes the window for the opener too" {
        Expect.isTrue (NonBlockingConsole.startCapture ()) "opened"
        NonBlockingConsole.writeLine "before"

        let captured = onNestedFlow NonBlockingConsole.stopCapture
        Expect.equal captured "before\n" "the nested stop returned the output"

        // The window must be closed HERE, so a later write reaches the console and a
        // later start owns a window of its own.
        Expect.isTrue
          (NonBlockingConsole.startCapture ())
          "the opener sees the window closed, so it can open a fresh one"
        NonBlockingConsole.writeLine "after"
        Expect.equal
          (NonBlockingConsole.stopCapture ())
          "after\n"
          "the fresh window captured only what followed it"
      }

      // A window opened by a nested flow stays that flow's own: closing it there must
      // not hand this flow a window it never opened.
      test "a window opened by a nested flow does not escape into the opener" {
        let captured =
          onNestedFlow (fun () ->
            Expect.isTrue (NonBlockingConsole.startCapture ()) "opened in the child"
            NonBlockingConsole.writeLine "child only"
            NonBlockingConsole.stopCapture ())
        Expect.equal captured "child only\n" "the child captured its own write"
        Expect.isTrue
          (NonBlockingConsole.startCapture ())
          "this flow was never captured by the child's window"
        NonBlockingConsole.stopCapture () |> ignore<string>
      } ]


// Sequenced for the same reason the CLI suites are: these assert on the EXACT contents of
// a window, so anything else printing into this flow while one is open would break them.
// The window itself is flow-scoped, so this is belt-and-braces rather than load-bearing.
let tests = testSequenced (testList "ConsoleCapture" [ captureTests ])
