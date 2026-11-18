module Tests.Prelude

open System.Threading.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils


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
    "Float64"
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

  // `String.slice`: the clusters in range, or None unless 0 <= first <= last <= length.
  let referenceStrictSlice
    (s : string)
    (first : int64)
    (last : int64)
    : Option<string> =
    let len = int64 (reference s).Length
    if first < 0L || last < first || last > len then
      None
    else
      Some(referenceSlice s first last)

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
                (referenceStrictSlice s first last)
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

      test "splitEgcs agrees with splitting the clusters" {
        let referenceSplit (s : string) (sep : string) : List<string> =
          let sepClusters = reference sep
          let n = List.length sepClusters
          let part (current : List<string>) = String.concat "" (List.rev current)
          let rec go (rest : List<string>) (current : List<string>) acc =
            match rest with
            | [] -> List.rev (part current :: acc)
            | _ when List.truncate n rest = sepClusters ->
              go (List.skip n rest) [] (part current :: acc)
            | c :: tail -> go tail (c :: current) acc
          go (reference s) [] []
        // Long runs of simple chars around the joining pieces, which the walks step over at once.
        let long =
          [ for s in List.take 300 samples ->
              String.replicate 40 "ab" + s + String.replicate 40 "c" + s ]
        let separators = List.ofArray pieces @ [ "ab"; "a\r"; "\u0301x"; "c\r\n" ]
        for s in samples @ long do
          for sep in separators do
            Expect.equal
              (String.splitEgcs s sep)
              (referenceSplit s sep)
              $"split {s} {sep}"
      }

      test "the walks agree with StringInfo over long runs of simple chars" {
        for s in List.take 300 samples do
          let s = String.replicate 40 "ab" + s + String.replicate 40 "c" + s
          let clusters = reference s
          let len = int64 clusters.Length
          Expect.equal (String.lengthInEgcs s) clusters.Length $"length {s}"
          for first in [ 0L; 1L; 39L; 40L; 41L; 79L; 80L; 81L; len - 2L; len ] do
            for last in [ 40L; 80L; 81L; 120L; len - 1L; len ] do
              Expect.equal
                (String.sliceEgcs s first last)
                (referenceStrictSlice s first last)
                $"slice {s} {first} {last}"
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
      }
      test "NO_COLOR drops a reset that only ended a colour" {
        Expect.equal
          (NonBlockingConsole.Ansi.withoutColour $"{esc}[31mx{esc}[0m y")
          "x y"
          "nothing is left for the reset to undo"
        Expect.equal
          (NonBlockingConsole.Ansi.withoutColour
            $"{esc}[1m{esc}[31mx{esc}[0m{esc}[32my{esc}[m")
          $"{esc}[1mx{esc}[0my"
          "a reset after bold stays; the next one only ends green"
        Expect.equal
          (NonBlockingConsole.Ansi.withoutColour $"x{esc}[0m")
          $"x{esc}[0m"
          "a reset with nothing before it may end a style an earlier write set"
      } ]

let assertions =
  testList
    "Assertions"
    [ test "assertFn" { assertFn "msg" System.Double.IsFinite 6.0 }
      test "assertFn2" { assertFn2 "msg" String.contains "x" "xxx" }
      test "assertEq" { assertEq "msg" "x" "x" }
      test "assertIn" { assertIn "msg" [ "x" ] "x" }
      test "assert_" { assert_ "_" [] true } ]


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
      mapTests
      floatTests
      dateTests
      stringTests
      textWidthTests
      ansiTests
      assertions ]
