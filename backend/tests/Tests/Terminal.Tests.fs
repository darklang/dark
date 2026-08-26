/// Tests the native terminal mechanisms that Dark cannot provide itself.
module Tests.Terminal

open Expecto
open TestUtils.TestUtils

module TerminalRestoreGuard = Builtins.Cli.Libs.Terminal.TerminalRestoreGuard
module TerminalText = Builtins.Cli.Libs.TerminalText
module PosixLibc = Builtins.Cli.Libs.Posix.Libc


let private red = "\u001b[31m"
let private reset = "\u001b[0m"

let terminalTextTests =
  testList
    "TerminalText"
    [ testMany
        "styledWidth"
        TerminalText.styledWidth
        [ "", 0
          "abc", 3
          $"{red}abc{reset}", 3
          // a non-SGR escape is dropped whole, and so is a bare control character
          "before\u001b[2Jafter", 11
          "a\tb", 2
          "界", 2 ]
      testMany
        "stripSgr"
        TerminalText.stripSgr
        [ "", ""
          "abc", "abc"
          $"{red}abc{reset}", "abc"
          "before\u001b[2Jafter", "beforeafter"
          // an incomplete escape takes the rest of the row with it
          "abc\u001b[3", "abc" ]
      test "styledWidth agrees with measuring the stripped text" {
        // `fitToWidth` pads by the difference between these two, and they walk the row separately.
        for row in
          [ ""
            "plain"
            $"{red}styled{reset}"
            $"{red}界{reset}🙂"
            "cursor\u001b[2Jmoves"
            "\u001b[1;31mbold red\u001b[0m tail" ] do
          Expect.equal
            (TerminalText.styledWidth row)
            (TextWidth.ofString (TerminalText.stripSgr row))
            $"styledWidth should equal the width of the stripped row: {row}"
      }
      test "clipToWidth keeps styling, drops other escapes, never splits a cluster" {
        Expect.equal
          (TerminalText.clipToWidth $"{red}abcdef" 3)
          $"{red}abc"
          "styling is retained and costs no columns"
        Expect.equal
          (TerminalText.clipToWidth "界界" 3)
          "界"
          "a double-width cluster that doesn't fit is dropped whole"
        Expect.equal
          (TerminalText.clipToWidth "abc" 0)
          ""
          "a zero width clips everything"
      }
      test "hardWrap reapplies active styling on each row" {
        Expect.equal
          (TerminalText.hardWrap $"{red}abcd" 2)
          [ $"{red}ab"; $"{red}cd" ]
          "styling active at the wrap opens the next row"
        Expect.equal
          (TerminalText.hardWrap "" 4)
          [ "" ]
          "empty input is one empty row"
        Expect.equal
          (TerminalText.hardWrap "界界" 2)
          [ "界"; "界" ]
          "a row holds whole clusters only"
      }
      testMany2
        "positionAfter"
        TerminalText.positionAfter
        [ "", 4, (0, 0)
          "abc", 4, (0, 3)
          // a cursor exactly past the last column belongs at the start of the next row
          "abcd", 4, (1, 0)
          "abcde", 4, (1, 1)
          "界界", 4, (1, 0) ] ]


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
      terminalTextTests ]
