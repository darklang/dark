/// Tests the native terminal mechanisms that Dark cannot provide itself.
module Tests.Terminal

open Expecto
open TestUtils.TestUtils

module TerminalRestoreGuard = Builtins.Cli.Libs.Terminal.TerminalRestoreGuard
module TerminalText = Builtins.Cli.Libs.TerminalText
module Stdin = Builtins.Cli.Libs.Stdin
module HostPosix = LibExecution.HostPosix


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
          $"{red}abc{reset}"
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
      test "clipToWidth closes styling the cut left open" {
        Expect.equal
          (TerminalText.clipToWidth $"\u001b[1mabc{reset}def" 3)
          $"\u001b[1mabc{reset}"
          "a reset past the cut is still there"
        Expect.equal
          (TerminalText.clipToWidth $"{red}ab{reset}cdef" 3)
          $"{red}ab{reset}c"
          "styling already closed before the cut needs no second reset"
        Expect.equal
          (TerminalText.clipToWidth $"{red}ab\u001b[mcdef" 3)
          $"{red}ab\u001b[mc"
          "the short reset counts too"
        Expect.equal
          (TerminalText.clipToWidth $"{red}abc" 5)
          $"{red}abc"
          "a row that fits is left as it is"
        Expect.equal
          (TerminalText.clipToWidth $"{red}a界" 2)
          $"{red}a{reset}"
          "a wide cluster that doesn't fit is a cut too"
      }
      test "wrapAtColumn reapplies active styling on each row" {
        Expect.equal
          (TerminalText.wrapAtColumn $"{red}abcd" 2)
          [ $"{red}ab"; $"{red}cd" ]
          "styling active at the wrap opens the next row"
        Expect.equal
          (TerminalText.wrapAtColumn "" 4)
          [ "" ]
          "empty input is one empty row"
        Expect.equal
          (TerminalText.wrapAtColumn "界界" 2)
          [ "界"; "界" ]
          "a row holds whole clusters only"
      }
      test "wrapWithCursor: the end of input is the end of the last drawn row" {
        for width in [ 1; 2; 3; 4; 5; 7; 10; 13; 20; 80 ] do
          for text in
            [ ""
              "> echo hello world"
              "> a bb ccc dddd eeeee ffffff"
              "> two  spaces   here "
              "> 界界 漢字 かな 界"
              "> 🙂 👩‍💻 ok 🙂🙂 go"
              $"{red}> styled{reset} words and 界 🙂 mixed" ] do
            let (rows, row, column) = TerminalText.wrapWithCursor text "" width
            let last =
              TerminalText.styledWidth (List.item (List.length rows - 1) rows)
            let expected =
              if last >= width then
                (List.length rows, 0)
              else
                (List.length rows - 1, last)
            Expect.equal (row, column) expected $"{text} at {width}: rows {rows}"
            Expect.equal
              rows
              (TerminalText.wrapAtColumn text width)
              "the same rows as wrapAtColumn"
      }
      test "wrapWithCursor follows a word that moves down" {
        Expect.equal
          (TerminalText.wrapWithCursor "> echo hello world" "" 10)
          ([ "> echo"; "hello"; "world" ], 2, 5)
          "the cursor sits after the last drawn word"
        Expect.equal
          (TerminalText.wrapWithCursor "> echo hello wo" "rld" 10)
          ([ "> echo"; "hello"; "world" ], 2, 2)
          "a cursor inside a word that moved goes with it"
        Expect.equal
          (TerminalText.wrapWithCursor "> echo" " hello world" 10)
          ([ "> echo"; "hello"; "world" ], 0, 6)
          "a cursor on the break space stays at the end of its row"
        Expect.equal
          (TerminalText.wrapWithCursor "abcd" "" 4)
          ([ "abcd" ], 1, 0)
          "exactly past a full row is the start of the next one"
        Expect.equal
          (TerminalText.wrapWithCursor $"{red}ab" "cd" 2)
          ([ $"{red}ab"; $"{red}cd" ], 1, 0)
          "styling costs no columns"
      } ]


/// The LSP's `Content-Length` counts bytes. Every case reads a framed body and
/// then checks what is LEFT on the reader, because over-reading never breaks the
/// body you asked for: it eats the start of the next message.
let readExactlyBytesTests =
  let read (input : string) (length : int) =
    let reader = new System.IO.StringReader(input)
    let result = Stdin.readExactlyBytes reader System.Text.Encoding.UTF8 length
    (result, reader.ReadToEnd())

  testList
    "readExactlyBytes"
    [ test "ascii reads its length and no further" {
        Expect.equal (read "abcdeNEXT" 5) (Ok "abcde", "NEXT") ""
      }
      test "a multi-byte char counts as its bytes, so the next header survives" {
        // the shape that killed the server: a body with "café", then a header
        Expect.equal
          (read "// café\nContent-Length: 2" 9)
          (Ok "// café\n", "Content-Length: 2")
          ""
      }
      test "a surrogate pair is four bytes and is never split" {
        Expect.equal (read "a\U0001F600bNEXT" 6) (Ok "a\U0001F600b", "NEXT") ""
      }
      test "a long body of two-byte chars" {
        let body = System.String('é', 10000)
        Expect.equal (read (body + "NEXT") 20000) (Ok body, "NEXT") ""
      }
      test "zero bytes reads nothing" {
        Expect.equal (read "NEXT" 0) (Ok "", "NEXT") ""
      }
      test "input ending early is an error, not a short body" {
        Expect.isError (fst (read "ab" 5)) ""
      }
      test "a length ending inside a character is an error, not mangled text" {
        Expect.isError (fst (read "éNEXT" 1)) ""
      } ]


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

          match HostPosix.openFile path HostPosix.O_RDONLY 0 with
          | Error(errno, message) ->
            failtestf "open failed with errno %d: %s" errno message
          | Ok fd ->
            try
              Expect.equal
                (HostPosix.fdSeek fd 8L HostPosix.SEEK_SET)
                (Ok 8L)
                "seek should return the new absolute byte offset"

              match HostPosix.fdRead fd 6 with
              | Error(errno, message) ->
                failtestf "read failed with errno %d: %s" errno message
              | Ok bytes ->
                Expect.equal
                  (System.Text.Encoding.UTF8.GetString bytes)
                  "three\n"
                  "the bounded read should start at the seeked offset"
            finally
              HostPosix.fdClose fd |> ignore
        finally
          System.IO.File.Delete path
      }
      terminalTextTests
      readExactlyBytesTests ]
