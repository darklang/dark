module Tests.Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils


let asyncTests =

  // slow it down so later items might be run first
  let delay (f : unit -> 'a) (i : int) : Ply<'a> =
    uply {
      do! Task.Delay(100 - (i * 10))
      return (f ())
    }

  testList
    "sequential"
    [ testTask "mapSequentially" {
        let fn (i : int) = delay (fun () -> i + 1) i
        let! result = Ply.List.mapSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result [ 2; 3; 4; 5 ] ""
      }
      testTask "filterSequentially" {
        let fn (i : int) = uply { return (i % 2) = 0 }
        let! result = Ply.List.filterSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result [ 2; 4 ] ""
      }
      testTask "findSequentially" {
        let fn (i : int) = delay (fun () -> i = 3) i
        let! result = Ply.List.findSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result (Some 3) ""
      }
      testTask "iterSequentially" {
        let mutable state = []
        let fn (i : int) = delay (fun () -> state <- i + 1 :: state) i
        do! Ply.List.iterSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal state [ 5; 4; 3; 2 ] ""
      } ]

let mapTests =
  testList
    "map"
    [ testMany2
        "Map.mergeFavoringRight"
        Map.mergeFavoringRight
        [ Map.empty, Map.empty, Map.empty
          (Map.ofList [ (1, 1); (2, 2); (3, 3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ]) ]
      testMany2
        "Map.mergeFavoringLeft"
        Map.mergeFavoringLeft
        [ Map.empty, Map.empty, Map.empty
          (Map.ofList [ (1, 1); (2, 2); (3, 3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ],
           Map.ofList [ (1, 1); (2, 2); (3, 3) ]) ] ]

let floatTests =
  testList
    "Float"
    [ testMany
        "readFloat"
        readFloat
        [ -0.0, (Negative, "0", "0")
          0.0, (Positive, "0", "0")
          82.10, (Positive, "82", "099999999999994315658113919198513031005859375")
          -180.0, (Negative, "180", "0") ] ]

let dateTests =
  testList
    "DateTime"
    [ testMany
        "toIsoString"
        (fun (i : NodaTime.Instant) -> i.toIsoString ())
        [ NodaTime.Instant.ofUtcInstant (2000, 10, 1, 16, 1, 1),
          "2000-10-01T16:01:01Z" ]
      testMany
        "ofIsoString"
        NodaTime.Instant.ofIsoString
        [ "2000-10-01T16:01:01Z",
          NodaTime.Instant.ofUtcInstant (2000, 10, 1, 16, 1, 1) ] ]

/// The cluster walks in `String` skip `StringInfo` where they can: a whole string below U+0300
/// with no CR, and any two such chars in a row. These pin them to the general path, which is
/// `StringInfo` alone, on text built from the pieces that join clusters.
let graphemeTests =
  let reference (s : string) : List<string> =
    let e = System.Globalization.StringInfo.GetTextElementEnumerator(s)
    [ while e.MoveNext() do
        yield e.GetTextElement() ]

  // `String.slice` without its fast path: clamp against the cluster count,
  // then join the clusters in range.
  let referenceSlice (s : string) (first : int64) (last : int64) : string =
    let clusters = reference s |> Array.ofList
    let norm (i : int64) =
      let i = if i < 0L then int64 clusters.Length + i else i
      int (max 0L (min i (int64 clusters.Length)))
    let first = norm first
    let last = max first (norm last)
    String.concat "" clusters[first .. last - 1]

  let pieces =
    [| "a"
       "Z"
       " "
       "\t"
       "\n"
       "\r"
       "\r\n"
       "\u00E9"
       "\u00FF"
       "\u02FF"
       "\u00AD"
       "\u00A9"
       "\u0300"
       "e\u0301"
       "\u036F"
       "\u200D"
       "\uFE0F"
       "\U0001F469"
       "\U0001F44D"
       "\U0001F3FD"
       "\U0001F1F5"
       "\U0001F1F1"
       "\u1100"
       "\u1161"
       "\u11A8"
       "\uAC00"
       "\u0915"
       "\u094D"
       "\u0903"
       "\u0600"
       "\uD800"
       "\uDC00"
       "\u4E16" |]

  let random = System.Random(6147)

  let samples =
    [ for _ in 1..3000 do
        let n = random.Next(0, 12)
        yield
          String.concat "" [ for _ in 1..n -> pieces[random.Next(pieces.Length)] ] ]
    @ [ ""
        "plain ascii"
        "caf\u00E9 na\u00EFve"
        "\U0001F469\u200D\U0001F469\u200D\U0001F467" ]

  testList
    "graphemes"
    [ test "two chars below U+0300, neither CR, are two clusters" {
        let simple =
          [ for c in 0..0x2FF do
              if c <> 13 then yield char c ]
        for a in simple do
          for b in simple do
            let pair = [| a; b |]
            if
              System.Globalization.StringInfo.GetNextTextElementLength(
                System.ReadOnlySpan<char>(pair)
              )
              <> 1
            then
              failtest $"U+{int a:X4} U+{int b:X4} is one cluster"
      }

      test "egcs, lengthInEgcs and isCharwise agree with StringInfo" {
        for s in samples do
          let expected = reference s
          Expect.equal (String.egcs s) expected $"egcs {s}"
          Expect.equal (String.egcsReversed s) (List.rev expected) $"reversed {s}"
          Expect.equal (String.lengthInEgcs s) expected.Length $"length {s}"
          if String.isCharwise s then
            Expect.equal expected.Length s.Length $"isCharwise {s}"
      }

      test "sliceEgcs and dropFirstEgcs agree with StringInfo" {
        for s in samples do
          let len = int64 (reference s).Length
          let indices =
            [ -len - 2L .. len + 2L ]
            @ [ System.Int64.MaxValue; System.Int64.MinValue ]
          for first in indices do
            for last in indices do
              Expect.equal
                (String.sliceEgcs s first last)
                (referenceSlice s first last)
                $"slice {s} {first} {last}"
            let dropped = if first < 0L then s else referenceSlice s first len
            Expect.equal
              (String.dropFirstEgcs s first)
              dropped
              $"dropFirst {s} {first}"
      }

      test "startsWithEgcs agrees with slicing" {
        let others = samples |> List.take 200
        for s in samples do
          // Every ordinal prefix, cut at a cluster boundary or not, and some unrelated strings.
          let prefixes = [ for i in 0 .. s.Length -> s.Substring(0, i) ] @ others
          for prefix in prefixes do
            let expected =
              referenceSlice s 0L (int64 (reference prefix).Length) = prefix
            Expect.equal
              (String.startsWithEgcs s prefix)
              expected
              $"startsWith {s} {prefix}"
      }

      test "endsWithEgcs agrees with slicing" {
        let others = samples |> List.take 200
        for s in samples do
          let len = int64 (reference s).Length
          let suffixes = [ for i in 0 .. s.Length -> s.Substring i ] @ others
          for suffix in suffixes do
            let expected =
              referenceSlice s (len - int64 (reference suffix).Length) len = suffix
            Expect.equal
              (String.endsWithEgcs s suffix)
              expected
              $"endsWith {s} {suffix}"
      }

      testMany
        "egcs on the joining pieces"
        String.egcs
        [ "a\r\nb", [ "a"; "\r\n"; "b" ]
          "e\u0301x", [ "e\u0301"; "x" ]
          "\u00A9\u200D\u00A9", [ "\u00A9\u200D\u00A9" ]
          "\U0001F1F5\U0001F1F1\U0001F1FA", [ "\U0001F1F5\U0001F1F1"; "\U0001F1FA" ]
          "\U0001F469\u200D\U0001F469\u200D\U0001F467",
          [ "\U0001F469\u200D\U0001F469\u200D\U0001F467" ] ] ]

let stringTests =
  let samples =
    [ ""
      "hello"
      "\r\n"
      "a\r\r\n\n\rb"
      "e\u0301\u0323"
      "\U0001F1F5\U0001F1F1\U0001F1FA\U0001F1F8"
      "\U0001F1F5\U0001F1F1\U0001F1FA"
      "\U0001F469\u200D\U0001F469\u200D\U0001F467"
      "\U0001F44D\U0001F3FD"
      "\uD800x" ]
  testList
    "String"
    [ test "lengthInEgcs agrees with StringInfo" {
        for a in samples do
          for b in samples do
            let s = a + "ab" + b + String.replicate 100 a
            Expect.equal
              (String.lengthInEgcs s)
              (System.Globalization.StringInfo(s).LengthInTextElements)
              s
      }
      testMany
        "toEgcChar"
        String.toEgcChar
        [ "", None
          "a", Some "a"
          "ab", None
          "\r\n", Some "\r\n"
          "e\u0301", Some "e\u0301"
          "\U0001F1F5\U0001F1F1", Some "\U0001F1F5\U0001F1F1"
          "\U0001F1F5\U0001F1F1\U0001F1FA", None
          "\U0001F469\u200D\U0001F469\u200D\U0001F467",
          Some "\U0001F469\u200D\U0001F469\u200D\U0001F467" ]
      graphemeTests ]

let textWidthTests =
  testList
    "TextWidth"
    [ testMany
        "ofString"
        TextWidth.ofString
        [ "", 0
          "hello", 5
          // a cluster is measured whole, however it's spelled
          "é", 1
          "e\u0301", 1
          "界", 2
          "🙂", 2
          "👨‍👩‍👧‍👦", 2
          "🇺🇸", 2
          // VS16 asks for emoji presentation, so two columns
          "♥️", 2
          "·", 1
          "A界🙂e\u0301", 6 ] ]


let ansiTests =
  let esc = "\u001b"
  testList
    "ansi"
    [ test "a pipe gets no escape sequences at all" {
        let coloured = $"{esc}[31mError:{esc}[0m {esc}[1mbold{esc}[22m plain"
        Expect.equal
          (NonBlockingConsole.Ansi.withoutSgr coloured)
          "Error: bold plain"
          "every SGR sequence goes"
      }
      test "NO_COLOR keeps the style and drops the colour" {
        Expect.equal
          (NonBlockingConsole.Ansi.withoutColour $"{esc}[1;31mx{esc}[0m")
          $"{esc}[1mx{esc}[0m"
          "bold stays, red goes, the reset stays"
        Expect.equal
          (NonBlockingConsole.Ansi.withoutColour $"{esc}[38;5;208mx{esc}[39m")
          "x"
          "a 256-colour foreground goes whole, and a sequence left empty goes too"
        Expect.equal
          (NonBlockingConsole.Ansi.withoutColour $"{esc}[48;2;1;2;3;7mx")
          $"{esc}[7mx"
          "a truecolour background goes whole and reverse video stays"
      } ]

let assertions =
  testList
    "Assertions"
    [ test "assertFn" { assertFn "msg" System.Double.IsFinite 6.0 }
      test "assertFn2" { assertFn2 "msg" String.contains "x" "xxx" }
      test "assertEq" { assertEq "msg" "x" "x" }
      test "assertIn" { assertIn "msg" [ "x" ] "x" }
      test "assert_" { assert_ "_" [] true } ]


/// Every `Ply.List` sequential helper, over a list long enough to overflow a fold-based version.
///
/// In `flatten` and `foldSequentially` a fold builds one nested continuation per element and unwinds only
/// at the end, and a .NET stack overflow cannot be caught, so the process disappears.
///
/// Worth being precise about what this pins and what it does not. It asserts these helpers are CORRECT and
/// order-preserving at 100k. It does NOT reproduce an overflow: reverting `iterSequentially` to its fold form
/// still passes here, because awaiting an already-completed Ply does not deepen the stack the way building
/// a result chain does. The uniform iterative shape is the invariant; an overflow is only reachable by some
/// of them.
let deepRecursion =
  let big = List.init 100_000 (fun i -> i)

  testList
    "Ply.List handles lists too deep for a fold"
    [ testTask "mapSequentially" {
        let! r = Ply.List.mapSequentially (fun i -> Ply(i + 1)) big |> Ply.toTask
        Expect.equal (List.length r) 100_000 "mapped every element"
      }
      testTask "filterSequentially" {
        let! r =
          Ply.List.filterSequentially (fun i -> Ply(i % 2 = 0)) big |> Ply.toTask
        Expect.equal (List.length r) 50_000 "kept the evens, in order"
        Expect.equal (Seq.head r) 0 "order preserved"
      }
      testTask "filterMapSequentially" {
        let! r =
          Ply.List.filterMapSequentially
            (fun i -> Ply(if i % 10 = 0 then Some i else None))
            big
          |> Ply.toTask
        Expect.equal (List.length r) 10_000 "kept every tenth, in order"
        Expect.equal (Seq.head r) 0 "order preserved"
      }
      testTask "iterSequentially" {
        let mutable n = 0
        do!
          Ply.List.iterSequentially (fun _ -> uply { n <- n + 1 }) big |> Ply.toTask
        Expect.equal n 100_000 "visited every element"
      }
      testTask "findSequentially" {
        let! r =
          Ply.List.findSequentially (fun i -> Ply((i = 99_999))) big |> Ply.toTask
        Expect.equal r (Some 99_999) "found the last element"
      }
      testTask "flatten" {
        let! r = Ply.List.flatten (big |> List.map Ply) |> Ply.toTask
        Expect.equal (List.length r) 100_000 "flattened every element"
      }
      testTask "foldSequentially" {
        let! r =
          Ply.List.foldSequentially (fun acc i -> Ply(acc + int64 i)) 0L big
          |> Ply.toTask
        Expect.equal r 4_999_950_000L "summed every element"
      } ]


let outputCaptureTests =
  testList
    "output capture"
    [ testTask "a child can capture after its parent's window closes" {
        use parent = new OutputCapture()
        let released =
          TaskCompletionSource<unit>(
            TaskCreationOptions.RunContinuationsAsynchronously
          )
        let child =
          task {
            do! released.Task
            use output = new OutputCapture()
            NonBlockingConsole.writeLine "child output"
            output.Check(fun stdout stderr ->
              Expect.equal stdout "child output\n" "the child owns a new capture"
              Expect.equal stderr "" "no warnings")
          }
        try
          parent.Check(fun stdout stderr ->
            Expect.equal (stdout, stderr) ("", "") "the parent has not printed")
        finally
          released.TrySetResult() |> ignore<bool>
        do! child
      } ]


let tests =
  testList
    "prelude"
    [ outputCaptureTests
      asyncTests
      mapTests
      floatTests
      dateTests
      stringTests
      textWidthTests
      ansiTests
      assertions
      deepRecursion ]
