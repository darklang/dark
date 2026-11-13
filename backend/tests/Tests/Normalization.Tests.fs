/// `Normalization` against .NET's `String.Normalize`, which uses ICU in the test build, and
/// against Unicode's own conformance tests.
module Tests.Normalization

open Expecto
open Prelude

open System.Text

let private forms =
  [ NormalizationForm.FormC
    NormalizationForm.FormD
    NormalizationForm.FormKC
    NormalizationForm.FormKD ]

let private codepointsOf (s : string) : string =
  s.EnumerateRunes() |> Seq.map (fun r -> $"%04X{r.Value}") |> String.concat " "

/// Each form of each string where `Normalization` and ICU disagree, the first 20 of them.
let private disagreements (strings : seq<string>) : List<string> =
  strings
  |> Seq.collect (fun s -> forms |> Seq.map (fun form -> s, form))
  |> Seq.filter (fun (s, form) ->
    Normalization.normalizeTo form s <> s.Normalize form)
  |> Seq.truncate 20
  |> Seq.map (fun (s, form) ->
    $"{form} of [{codepointsOf s}]: ours [{codepointsOf (Normalization.normalizeTo form s)}], ICU [{codepointsOf (s.Normalize form)}]")
  |> List.ofSeq

/// Every codepoint but the ones ICU refuses, which have a test of their own.
let private everyCodepoint : seq<string> =
  seq { 0..0x10FFFF }
  |> Seq.filter (fun cp -> (cp < 0xD800 || cp > 0xDFFF) && cp <> 0xFFFE)
  |> Seq.map System.Char.ConvertFromUtf32

let private parseCodepoints (field : string) : string =
  field.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
  |> Array.map (fun hex ->
    System.Char.ConvertFromUtf32(System.Convert.ToInt32(hex, 16)))
  |> String.concat ""

/// Parts 0, 2 and 3 of Unicode 15.1.0's NormalizationTest.txt, each line's five columns. Part 1
/// tests characters one at a time, which the every-codepoint test already does.
let private conformanceLines : Lazy<List<string[]>> =
  lazy
    let bytes =
      LibConfig.File.readfileBytes
        LibConfig.Dirs.Testdata
        "normalization-test.txt.gz"
    use gzip =
      new System.IO.Compression.GZipStream(
        new System.IO.MemoryStream(bytes),
        System.IO.Compression.CompressionMode.Decompress
      )
    use reader = new System.IO.StreamReader(gzip, Encoding.UTF8)
    reader.ReadToEnd().Split('\n')
    |> Seq.filter (fun line ->
      line <> "" && not (line.StartsWith "#" || line.StartsWith "@"))
    |> Seq.map (fun line -> line.Split(';')[0..4] |> Array.map parseCodepoints)
    |> List.ofSeq

/// Random strings over the characters normalization treats specially, so the quick check, the
/// point it restarts from, reordering and composition all meet each other.
let private randomStrings : seq<string> =
  let alphabet =
    [| 0x41
       0x61
       0x65
       0x20
       0xE9
       0xC5
       0xA0
       0xFB01
       0x212B
       0x2126
       0x0340
       0x0344
       0x0300
       0x0301
       0x0302
       0x0308
       0x0323
       0x0327
       0x031B
       0x0345
       0x05B0
       0x0F71
       0x0F72
       0x0F73
       0x1100
       0x1161
       0x11A8
       0xAC00
       0xAC01
       0x3099
       0x304B
       0x30AB
       0x1D15E
       0x1D165
       0x1D16E
       0x110B9
       0x110BA
       0x0B47
       0x0B3E
       0x0FB5
       0x1E0A
       0x1E9B
       0x00B4
       0x0385
       0x1FEE
       0xFF76
       0xFF9E
       0x2460
       0x0CC6
       0x0CD5 |]
  let random = System.Random(6275)
  seq {
    for _ in 1..50000 ->
      String.concat
        ""
        [ for _ in 1 .. random.Next(0, 10) ->
            System.Char.ConvertFromUtf32(alphabet[random.Next alphabet.Length]) ]
  }


let tests =
  testList
    "Normalization"
    [ test "every codepoint on its own, as ICU normalizes it" {
        Expect.equal (disagreements everyCodepoint) [] "disagreements"
      }

      test "every codepoint's decompositions, composed again, as ICU does" {
        let decomposed =
          everyCodepoint
          |> Seq.collect (fun s ->
            [ s.Normalize NormalizationForm.FormD
              s.Normalize NormalizationForm.FormKD ])
          |> Seq.filter (fun d -> d.Length > 1)
          |> Seq.distinct
        Expect.equal (disagreements decomposed) [] "disagreements"
      }

      test "random strings of combining characters, as ICU normalizes them" {
        Expect.equal (disagreements randomStrings) [] "disagreements"
      }

      test "NormalizationTest.txt, as ICU normalizes it" {
        let strings = conformanceLines.Force() |> Seq.concat
        Expect.equal (disagreements strings) [] "disagreements"
      }

      test "NormalizationTest.txt's invariants" {
        let failures =
          conformanceLines.Force()
          |> List.filter (fun columns ->
            let c = Array.append [| "" |] columns // 1-based, as the file names them
            let nfc = Normalization.normalizeTo NormalizationForm.FormC
            let nfd = Normalization.normalizeTo NormalizationForm.FormD
            let nfkc = Normalization.normalizeTo NormalizationForm.FormKC
            let nfkd = Normalization.normalizeTo NormalizationForm.FormKD
            not (
              [ 1; 2; 3 ]
              |> List.forall (fun i -> nfc c[i] = c[2] && nfd c[i] = c[3])
              && [ 4; 5 ]
                 |> List.forall (fun i -> nfc c[i] = c[4] && nfd c[i] = c[5])
              && [ 1..5 ]
                 |> List.forall (fun i -> nfkc c[i] = c[4] && nfkd c[i] = c[5])
            ))
          |> List.truncate 20
          |> List.map (Array.map codepointsOf >> String.concat "; ")
        Expect.equal failures [] "lines that fail"
      }

      test "only what can change is rebuilt" {
        let ascii = "plain ascii"
        Expect.isTrue
          (obj.ReferenceEquals(Normalization.normalize ascii, ascii))
          "ASCII comes back as it is"
        let composed = "déjà vu, 한국어"
        Expect.isTrue
          (obj.ReferenceEquals(Normalization.normalize composed, composed))
          "so does text that passes the quick check"
        Expect.equal
          (Normalization.normalize ("prefix " + "e\u0301"))
          "prefix \u00E9"
          "a change at the end keeps the start"
      }

      test "joining NFC pieces, as normalizing the whole" {
        // Each line split at every position, and each line followed by the next.
        let lines = conformanceLines.Force() |> List.map (fun c -> c[0])
        let splits =
          lines
          |> List.collect (fun s ->
            [ for i in 0 .. s.Length do
                if i = s.Length || not (System.Char.IsLowSurrogate s[i]) then
                  [| s.Substring(0, i); s.Substring i |] ])
        let adjacent = List.pairwise lines |> List.map (fun (a, b) -> [| a; b |])
        let random = System.Random(6324)
        let pieces = randomStrings |> Array.ofSeq
        let randomJoins =
          [ for _ in 1..50000 ->
              Array.init (random.Next(0, 6)) (fun _ ->
                pieces[random.Next pieces.Length]) ]
        let failures =
          splits @ adjacent @ randomJoins
          |> List.map (Array.map Normalization.normalize)
          |> List.filter (fun parts ->
            Normalization.concat parts
            <> Normalization.normalize (String.concat "" parts))
          |> List.truncate 20
          |> List.map (Array.map codepointsOf >> String.concat " | ")
        Expect.equal failures [] "joins that differ"
      }

      test "a join next to a lone surrogate keeps the text as it was" {
        let hi = string (char 0xD83D)
        for a, b in [ "e", hi + "a"; "e" + hi, "\u0301"; "e", "\u0301" + hi ] do
          Expect.equal
            (Normalization.concat [| a; b |])
            (a + b)
            $"[{codepointsOf (a + b)}]"
      }

      test "a lone surrogate or U+FFFE is an error, as it is for ICU" {
        // Built at runtime: the compiler turns a lone surrogate in a literal into U+FFFD.
        let refused =
          [ "abc" + string (char 0xD800)
            string (char 0xDC00) + "a\u0301"
            "\u00E9" + string (char 0xD800) + "e\u0301"
            "a\uFFFE" ]
        for s in refused do
          for form in forms do
            Expect.throwsT<System.ArgumentException>
              (fun () -> Normalization.normalizeTo form s |> ignore<string>)
              $"ours, {form} of [{codepointsOf s}]"
            Expect.throwsT<System.ArgumentException>
              (fun () -> s.Normalize form |> ignore<string>)
              $"ICU's, {form} of [{codepointsOf s}]"
      } ]
