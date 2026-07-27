/// Tests the native terminal mechanisms that Dark cannot provide itself.
module Tests.Terminal

open Expecto

module TerminalRestoreGuard = Builtins.Cli.Libs.Terminal.TerminalRestoreGuard
module DisplayWidth = Builtins.Cli.Libs.Terminal.DisplayWidth
module PosixLibc = Builtins.Cli.Libs.Posix.Libc


let displayWidthTests =
  [ "", 0
    "hello", 5
    "é", 1
    "e\u0301", 1
    "界", 2
    "🙂", 2
    "👨‍👩‍👧‍👦", 2
    "🇺🇸", 2
    "♥️", 2
    "·", 1
    "A界🙂e\u0301", 6 ]
  |> List.map (fun (text, expected) ->
    let display = sprintf "%A" text
    test $"width of {display}" {
      Expect.equal
        (DisplayWidth.ofString text)
        expected
        "text should occupy the expected number of terminal columns"
    })
  |> testList "Terminal display width"


let tests =
  testList
    "Terminal"
    [ test "terminal size sample is always positive" {
        let (width, height) = Builtins.Cli.Libs.Terminal.terminalSize ()
        Expect.isGreaterThan width 0L "terminal width should be positive"
        Expect.isGreaterThan height 0L "terminal height should be positive"
      }
      test "fallback restoration can be armed, replaced, and disarmed" {
        TerminalRestoreGuard.disarm ()
        let writes = ResizeArray<string>()

        try
          TerminalRestoreGuard.restoreWith writes.Add

          TerminalRestoreGuard.arm "restore-fullscreen"
          TerminalRestoreGuard.restoreWith writes.Add

          TerminalRestoreGuard.arm "restore-inline"
          TerminalRestoreGuard.restoreWith writes.Add

          TerminalRestoreGuard.disarm ()
          TerminalRestoreGuard.restoreWith writes.Add

          Expect.sequenceEqual
            writes
            [ "restore-fullscreen"; "restore-inline" ]
            "only the currently armed restoration sequence should be invoked"
        finally
          TerminalRestoreGuard.disarm ()
      }
      test "terminal text metrics identify dynamic controls" {
        Expect.isFalse
          (DisplayWidth.containsControl "plain界")
          "plain Unicode text should be safe"
        Expect.isTrue
          (DisplayWidth.containsControl "before\u001b[2Jafter")
          "terminal escape content should require sanitization"
      }
      test "file descriptors can seek before a bounded read" {
        let path = System.IO.Path.GetTempFileName()

        try
          System.IO.File.WriteAllText(path, "one\ntwo\nthree\n")

          match PosixLibc.openFile path PosixLibc.O_RDONLY 0 with
          | Error(errno, message) ->
            failtestf "open failed with errno %d: %s" errno message
          | Ok fd ->
            try
              Expect.equal
                (PosixLibc.fdSeek fd 8L PosixLibc.SEEK_SET)
                (Ok 8L)
                "seek should return the new absolute byte offset"

              match PosixLibc.fdRead fd 6 with
              | Error(errno, message) ->
                failtestf "read failed with errno %d: %s" errno message
              | Ok bytes ->
                Expect.equal
                  (System.Text.Encoding.UTF8.GetString bytes)
                  "three\n"
                  "the bounded read should start at the seeked offset"
            finally
              PosixLibc.fdClose fd |> ignore
        finally
          System.IO.File.Delete path
      }
      displayWidthTests ]
