/// Scoped stdout capture, including output written by child execution flows.
module Tests.ConsoleCapture

open Expecto
open Prelude

/// Run `f` on a genuinely separate async flow, the way a nested evaluation does, and
/// wait for it. `Task.Run` gives `f` a child ExecutionContext, which is what makes an
/// `AsyncLocal` write inside it invisible to this flow.
let private onNestedFlow (f : unit -> 'a) : 'a =
  System.Threading.Tasks.Task.Run(f).GetAwaiter().GetResult()


let tests =
  testList
    "ConsoleCapture"
    [ test "a child capture restores its inherited outer capture" {
        use outer = NonBlockingConsole.captureOutput ()
        let innerOutput =
          onNestedFlow (fun () ->
            let captured =
              use inner = NonBlockingConsole.captureOutput ()
              NonBlockingConsole.writeLine "child only"
              inner.Output
            NonBlockingConsole.writeLine "back in outer"
            captured)
        NonBlockingConsole.writeLine "parent"
        Expect.equal innerOutput "child only\n" "child output stays local"
        Expect.equal
          outer.Output
          "back in outer\nparent\n"
          "both flows use the outer capture again"
      }

      test "a continuation retaining a closed capture writes to its parent" {
        use outer = NonBlockingConsole.captureOutput ()
        let continuationContext =
          use inner = NonBlockingConsole.captureOutput ()
          NonBlockingConsole.writeLine "inner only"
          System.Threading.ExecutionContext.Capture()

        // Resuming restores the inner AsyncLocal slot, even though that scope
        // has closed. The verdict and error details must still reach the outer capture.
        System.Threading.ExecutionContext.Run(
          continuationContext,
          (fun _ -> NonBlockingConsole.writeLine "ERROR"),
          null
        )
        NonBlockingConsole.writeLine "summary"
        Expect.equal
          outer.Output
          "ERROR\nsummary\n"
          "the resumed continuation and its caller share the enclosing capture"
      } ]
