module Builtins.Pure.Libs.String

open System.Globalization
open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module Interpreter = LibExecution.Interpreter
module Blob = LibExecution.Blob

/// The substring of `s` between cluster indices `first` and `last`, both already clamped to
/// [0, length] with `first <= last`.
///
/// Shared by `stringSlice` and `stringStartsWith` so the two cannot drift.
/// Prefix and suffix tests are cluster tests in Dark, not byte tests: `String.startsWith` was
/// `slice subject 0 (length prefix) == prefix`, and ordinal `StartsWith` is NOT equivalent -- "\U0001F471"
/// is a byte-prefix of "\U0001F471\U0001F3FB" but not a cluster-prefix of it.
let private egcSubstring (s : string) (first : int) (last : int) : string =
  if first >= last then
    ""
  else
    let e = System.Globalization.StringInfo.GetTextElementEnumerator(s)
    let mutable startIndex = 0
    let mutable endIndex = 0
    let mutable index = 0

    while e.MoveNext() do
      if index = first then startIndex <- e.ElementIndex
      if index = last then endIndex <- e.ElementIndex
      index <- index + 1

    if endIndex = 0 then endIndex <- s.Length
    s.Substring(startIndex, endIndex - startIndex)


/// Clamp a Dark `Int` index against a cluster length, the way `String.slice` always has: a negative
/// index counts from the end, then the result is clamped to [0, len].
///
/// Clamps in the `Int` domain and only then narrows -- narrowing first raises `OutOfRange` on an
/// index past int32, where this clamps. `String.slice "abc" 0 4503599627370498` is "abc", and there
/// is a testfile line for it.
let private normalizeEgcIndex (len : int) (d : DarkInt) : int =
  match d with
  | DarkInt.Finite i ->
    let i = if i < 0L then int64 len + i else i
    if i < 0L then 0
    elif i > int64 len then len
    else int i
  // Past int64 either way: still just "before the start" or "past the end".
  | DarkInt.Infinite b -> if b.Sign > 0 then len else 0


/// Return a byte from `s`'s UTF-8 encoding without encoding the whole string.
/// UTF-8 offsets use int64 because the encoding can exceed Int32.MaxValue bytes.
let private utf8ByteAt (s : string) (index : int64) : byte option =
  if index < 0L then
    None
  else
    let mutable offset = 0L
    let mutable found = None
    let mutable runes = s.EnumerateRunes()

    while found.IsNone && runes.MoveNext() do
      let rune = runes.Current
      let length = int64 rune.Utf8SequenceLength

      if index < offset + length then
        let buffer = Array.zeroCreate<byte> 4
        rune.EncodeToUtf8(System.Span<byte> buffer) |> ignore<int>
        // The offset within one rune is at most three.
        found <- Some buffer[int (index - offset)]
      else
        offset <- offset + length

    found


let fns () : List<BuiltInFn> =
  [ { name = fn "stringDisplayWidth" 0
      typeParams = []
      parameters = [ Param.make "text" TString "Plain, single-line text" ]
      returnType = TInt
      description =
        "Cells the text occupies in a fixed-width grid, over extended grapheme clusters"
      fn =
        (function
        | _, _, _, [| DString text |] ->
          text |> TextWidth.ofString |> bigint |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "stringToList" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TList TChar
      description = "Returns the list of Characters (EGC, not byte) in the string"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          s
          |> String.toEgcSeq
          |> Seq.map DChar
          |> Seq.toList
          |> Dval.list KTChar
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringReplaceAll" 0
      typeParams = []
      parameters =
        [ Param.make "s" TString "The string to operate on"
          Param.make "searchFor" TString "The string to search for within <param s>"
          Param.make "replaceWith" TString "" ]
      returnType = TString
      description =
        "Replace all instances on <param searchFor> in <param s> with <param "
        + "replaceWith>"
      fn =
        (function
        | _, _, _, [| DString s; DString search; DString replace |] ->
          if search = "" then
            if s = "" then
              Ply(DString replace)
            else
              // .Net Replace doesn't allow empty string, but we do.
              // intersperse `replace` between each grapheme cluster, then
              // wrap with `replace` on both ends.
              let interspersed =
                String.toEgcSeq s
                |> Seq.toList
                |> function
                  | [] -> []
                  | head :: tail ->
                    head :: (tail |> List.collect (fun y -> [ replace; y ]))
              interspersed
              |> (fun l -> replace :: l @ [ replace ])
              |> String.concat ""
              |> DString
              |> Ply
          else
            Ply(DString(s.Replace(search, replace)))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "replace"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringToUppercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description = "Returns the string, uppercased"
      fn =
        (function
        | _, _, _, [| DString s |] -> Ply(DString(String.toUppercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "upper"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringToLowercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description = "Returns the string, lowercased"
      fn =
        (function
        | _, _, _, [| DString s |] -> Ply(DString(String.toLowercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "lower"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringLength" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          s |> String.lengthInEgcs |> bigint |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented // CLEANUP: Sqlite has "LENGTH" but that counts characters; if we can get it to count EGCs, great
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringRepeat" 0
      typeParams = []
      parameters =
        [ Param.make "s" TString "The string to repeat"
          Param.make "count" TInt "How many times to repeat it" ]
      returnType = TString
      description =
        "Returns <param s> repeated <param count> times. An empty string when <param count> is not positive."
      fn =
        (function
        | _, vm, _, [| DString s; DInt count |] ->
          DString(String.repeat s (intToInt32 vm count)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringAppend" 0
      typeParams = []
      parameters = [ Param.make "s1" TString ""; Param.make "s2" TString "" ]
      returnType = TString
      description =
        "Concatenates the two strings by appending <param s2> to <param s1> and "
        + "returns the joined string."
      fn =
        (function
        // TODO add fuzzer to ensure all strings are normalized no matter what we do to them.
        | _, _, _, [| DString s1; DString s2 |] ->
          (s1 + s2) |> String.normalize |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // CLEANUP move implementation to Darklang, in package space, in darklang stdlib
    // (RegEx support now available via Stdlib.Regex.*)
    { name = fn "stringSlugify" 0
      typeParams = []
      parameters = [ Param.make "string" TString "" ]
      returnType = TString
      description =
        "Turns a string into a prettified slug, including only lowercased "
        + "alphanumeric characters, joined by hyphens"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          // Should work the same as https://blog.tersmitten.nl/slugify/
          // explicitly limit to (roman) alphanumeric for pretty urls
          let toRemove = "([^a-z0-9\\s_-]|\x0b)+"
          let toBeHyphenated = @"[-_\s]+"

          let replace (pattern : string) (replacement : string) (input : string) =
            Regex.Replace(input, pattern, replacement)

          s
          |> String.toLowercase
          |> replace toRemove ""
          |> String.trim
          |> replace toBeHyphenated "-"
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringReverse" 0
      typeParams = []
      parameters = [ Param.make "string" TString "" ]
      returnType = TString
      description = "Reverses <param string>"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          String.toEgcSeq s |> Seq.rev |> String.concat "" |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "reverse"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringSplit" 0
      typeParams = []
      parameters = [ Param.make "s" TString ""; Param.make "separator" TString "" ]
      returnType = TList TString
      description =
        "Splits a string at the separator, returning a list of strings without "
        + "the separator. If the separator is not present, returns a list "
        + "containing only the initial string."
      fn =
        (function
        | _, _, _, [| DString s; DString sep |] ->
          let ecgStringSplit str sep =
            let startsWithSeparator str = sep = (str |> List.truncate sep.Length)

            let result = ResizeArray<string>()

            let rec r (strRemaining : List<string>, inProgress) : unit =
              if strRemaining = [] then
                result |> ResizeArray.append (inProgress.ToString())
              elif startsWithSeparator strRemaining then
                result |> ResizeArray.append (inProgress.ToString())

                r (List.skip sep.Length strRemaining, StringBuilder())
              else
                r (strRemaining.Tail, inProgress.Append(strRemaining.Head))

            r (str, StringBuilder())

            result |> ResizeArray.toList

          let parts =
            if sep = "" then
              s |> String.toEgcSeq |> Seq.toList
            // When both sides are charwise, splitting by cluster and splitting by char give the
            // same answer, and the framework's split does it without building a string per
            // character of the input. Paths, keys and identifiers all take this route.
            elif String.isCharwise s && String.isCharwise sep then
              s.Split([| sep |], System.StringSplitOptions.None) |> Array.toList
            else
              ecgStringSplit
                (s |> String.toEgcSeq |> Seq.toList)
                (sep |> String.toEgcSeq |> Seq.toList)

          parts |> List.map DString |> Dval.list KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringJoin" 0
      typeParams = []
      parameters =
        [ Param.make "l" (TList TString) ""; Param.make "separator" TString "" ]
      returnType = TString
      description = "Combines a list of strings with the provided separator"
      fn =
        (function
        | _, _, _, [| DList(_, l); DString sep |] ->
          l
          |> List.map (fun s ->
            match s with
            | DString st -> st
            | dv ->
              // CLEANUP should be a proper "bad param" RTE
              Exception.raiseInternal "expected string in join" [ "dval", dv ])
          |> String.concat sep
          |> String.normalize
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringDropFirst" 0
      typeParams = []
      parameters = [ Param.make "string" TString ""; Param.make "count" TInt "" ]
      returnType = TString
      description =
        "Returns <param string> with the first <param count> characters removed. The whole string "
        + "if <param count> is negative, the empty string if it is longer than <param string>."
      fn =
        (function
        | _, _, _, [| DString s; DInt countD |] ->
          // The Dark version was two `String.length` calls, two comparisons and a `slice`. The
          // "count is longer than the string" case needs no branch here: `normalizeEgcIndex` clamps
          // it to the length, and an empty range gives "". A NEGATIVE count does need one, because
          // a negative index counts from the end rather than clamping to zero.
          match countD with
          | DarkInt.Finite i when i < 0L -> Ply(DString s)
          | DarkInt.Infinite b when b.Sign < 0 -> Ply(DString s)
          | _ ->
            let len = String.lengthInEgcs s
            let first = normalizeEgcIndex len countD
            egcSubstring s first len |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringIsEmpty" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TBool
      description = "Returns true if <param s> is the empty string"
      fn =
        (function
        | _, _, _, [| DString s |] -> Ply(DBool(s.Length = 0))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringStartsWith" 0
      typeParams = []
      parameters =
        [ Param.make "subject" TString ""; Param.make "prefix" TString "" ]
      returnType = TBool
      description = "Checks if <param subject> starts with <param prefix>"
      fn =
        (function
        | _, _, _, [| DString subject; DString prefix |] ->
          // Exactly what the Dark version computed -- `slice subject 0 (length prefix) == prefix` --
          // but without the three package calls. Cluster-aware, so NOT `String.StartsWith`: see the
          // note on `egcSubstring`.
          //
          // When both sides are charwise every cluster is one character, so a cluster-prefix and an
          // ordinal prefix coincide and the walk can be skipped. That is nearly every call.
          if prefix = "" then
            Ply(DBool true)
          elif String.isCharwise subject && String.isCharwise prefix then
            Ply(DBool(subject.StartsWith(prefix, System.StringComparison.Ordinal)))
          else
            let subjectLen = String.lengthInEgcs subject
            let last = min (String.lengthInEgcs prefix) subjectLen
            Ply(DBool(egcSubstring subject 0 last = prefix))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringSlice" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""
          Param.make "from" TInt ""
          Param.make "to" TInt "" ]
      returnType = TString
      description =
        "Returns the substring of <param string> between the <param from> and "
        + "<param to> indices. Negative indices start counting from the end of "
        + "<param string>."
      fn =
        (function
        | _, _, _, [| DString s; DInt firstD; DInt lastD |] ->
          // `String.lengthInEgcs`, not `StringInfo(_).LengthInTextElements`: the former checks
          // `isCharwise` first and answers with `s.Length` for text that has no multi-char clusters,
          // which is nearly all of it. Going straight to `StringInfo` measurably regressed the CLI.
          let len = String.lengthInEgcs s

          // Normalizing here rather than in the Dark wrapper, which built a lambda and applied it
          // twice to do exactly this. That wrapper is a bare forwarder now, so it elides.
          let first = normalizeEgcIndex len firstD
          let last = normalizeEgcIndex len lastD
          let last = if first > last then first else last

          egcSubstring s first last |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringTrim" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all leading and trailing whitespace "
        + "removed. 'whitespace' here means all Unicode characters with the "
        + "{{White_Space}} property, which includes {{\" \"}}, {{\"\\t\"}} and "
        + "{{\"\\n\"}}"
      fn =
        (function
        | _, _, _, [| DString toTrim |] -> toTrim.Trim() |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "trim"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringTrimStart" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all leading whitespace removed. "
        + "'whitespace' here means all Unicode characters with the "
        + "{{White_Space}} property, which includes {{\" \"}}, {{\"\\t\"}} and "
        + "{{\"\\n\"}}"
      fn =
        (function
        | _, _, _, [| DString toTrim |] -> Ply(DString(toTrim.TrimStart()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "ltrim"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringTrimEnd" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all trailing whitespace removed. "
        + "'whitespace' here means all Unicode characters with the "
        + "{{White_Space}} property, which includes {{\" \"}}, {{\"\\t\"}} and "
        + "{{\"\\n\"}}."
      fn =
        (function
        | _, _, _, [| DString toTrim |] -> Ply(DString(toTrim.TrimEnd()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "rtrim"
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringToBlob" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TBlob
      description = "Converts the given unicode string to a UTF8-encoded Blob."
      fn =
        (function
        | _, _, _, [| DString str |] ->
          let theBytes = System.Text.Encoding.UTF8.GetBytes str
          Blob.newEphemeral theBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringFromBlobWithReplacement" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TString
      description =
        "Converts the UTF8-encoded <param blob> into a string. Invalid sequences are replaced."
      fn =
        (function
        | state, _, _, [| DBlob ref |] ->
          uply {
            let! bytes = Blob.readBytes state ref
            return DString(System.Text.Encoding.UTF8.GetString bytes)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringFromBytes" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TypeReference.option TString
      description =
        "Converts the UTF8-encoded byte sequence into a string. Returns None if the bytes aren't valid UTF-8."
      fn =
        (function
        | _, _, _, [| DList(_vt, bytes) |] ->
          try
            let bytes = Dval.dlistToByteArray bytes
            let str = UTF8Encoding(false, true).GetString bytes
            Dval.optionSome KTString (DString str) |> Ply
          with _e ->
            Dval.optionNone KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringFromBlob" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TypeReference.option TString
      description =
        "Converts the UTF8-encoded <param blob> into a string. Returns None if the bytes aren't valid UTF-8."
      fn =
        (function
        | state, _, _, [| DBlob ref |] ->
          uply {
            let! bytes = Blob.readBytes state ref
            try
              let str = UTF8Encoding(false, true).GetString bytes
              return Dval.optionSome KTString (DString str)
            with _e ->
              return Dval.optionNone KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringContains" 0
      typeParams = []
      parameters =
        [ Param.make "lookingIn" TString "The string to search within"
          Param.make "searchingFor" TString "The substring to look for" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param lookingIn> contains <param searchingFor>. "
        + "SQL-queryable: it only checks found/not-found, which SQLite's INSTR "
        + "and .NET's Contains agree on (unlike index position, which they count "
        + "differently for some Unicode text)."
      fn =
        (function
        | _, _, _, [| DString lookingIn; DString searchingFor |] ->
          Ply(DBool(lookingIn.Contains(searchingFor)))
        | _ -> incorrectArgs ())
      // Emits a complete boolean fragment, so no Int value reaches the SqlCompiler.
      sqlSpec =
        SqlCallback2(fun lookingIn searchingFor ->
          $"(INSTR({lookingIn}, {searchingFor}) > 0)")
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringIndexOf" 0
      typeParams = []
      parameters =
        [ Param.make "str" TString "The string to search in"
          Param.make
            "searchFor"
            TString
            "The string to search for within <param str>" ]
      returnType = TInt
      description =
        "Returns the index of the first occurrence of <param searchFor> in "
        + "<param str>, measured in UTF-16 code units (the .NET string "
        + "representation). Returns -1 if <param searchFor> does not occur. For "
        + "an index that pairs with {{String.length}}, {{String.slice}}, "
        + "{{String.first}}, and {{String.dropFirst}} (which are all "
        + "EGC-indexed), use {{stringIndexOfEgc}}.\n\nNote on SQL: SQLite's INSTR "
        + "and .NET's IndexOf both tell us whether a substring was found, but "
        + "they count positions differently for some Unicode text. SQLite counts "
        + "Unicode characters; .NET IndexOf counts UTF-16 code units. For "
        + "example, SQLite reports the \"a\" in \"😄a\" at index 1, while .NET "
        + "reports it at index 2 because the emoji takes two UTF-16 code units. "
        + "So {{String.contains}} is portable in SQL because it only checks "
        + "found/not-found, but {{String.indexOf}} may return different numeric "
        + "indexes outside simple ASCII text."
      fn =
        (function
        | _, _, _, [| DString str; DString search |] ->
          let index = str.IndexOf(search)
          Ply(Dval.int (bigint index))
        | _ -> incorrectArgs ())
      // `Int` isn't queryable in DB.query yet, and the compiler treats any spec as
      // active — so mark NotQueryable rather than advertise a spec that fails on the
      // comparison literal. (String.contains has its own queryable Bool builtin, so
      // it's unaffected.) Restore once Int is queryable:
      //   SqlCallback2(fun str search -> $"(INSTR({str}, {search}) - 1)")
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringIndexOfEgc" 0
      typeParams = []
      parameters =
        [ Param.make "str" TString "The string to search in"
          Param.make
            "searchFor"
            TString
            "The string to search for within <param str>" ]
      returnType = TInt
      description =
        "Returns the index of the first occurrence of <param searchFor> in "
        + "<param str>, measured in extended grapheme clusters (consistent with "
        + "{{String.length}}, {{String.slice}}, {{String.first}}, and "
        + "{{String.dropFirst}}). Returns -1 if <param searchFor> does not occur. "
        + "The match must begin and end on EGC boundaries — partial-grapheme "
        + "matches (e.g. finding a skin-tone modifier alone inside a full "
        + "skin-toned emoji) are not reported. Not SQL-queryable because SQLite "
        + "has no EGC-aware INSTR; use {{stringIndexOf}} inside DB query lambdas."
      fn =
        (function
        | _, _, _, [| DString str; DString search |] ->
          if search = "" then
            Ply(Dval.int (bigint 0))
          else
            // EGC start offsets (UTF-16) of str. A valid match must start at one
            // of these AND end at one of these (or at str.Length).
            let starts = StringInfo.ParseCombiningCharacters(str)
            let mutable foundAt = -1
            let mutable boundaryPtr = 0
            let mutable egcIndex = 0
            while foundAt = -1 && egcIndex < starts.Length do
              let elementIndex = starts[egcIndex]
              let targetEnd = elementIndex + search.Length
              while boundaryPtr < starts.Length && starts[boundaryPtr] < targetEnd do
                boundaryPtr <- boundaryPtr + 1
              let endIsBoundary =
                if boundaryPtr < starts.Length then
                  starts[boundaryPtr] = targetEnd
                else
                  targetEnd = str.Length
              let matches =
                System.String.Compare(
                  str,
                  elementIndex,
                  search,
                  0,
                  search.Length,
                  System.StringComparison.Ordinal
                ) = 0
              if endIsBoundary && matches then foundAt <- egcIndex
              egcIndex <- egcIndex + 1
            Ply(Dval.int (bigint foundAt))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringLastIndexOf" 0
      typeParams = []
      parameters =
        [ Param.make "str" TString "The string to search in"
          Param.make
            "searchFor"
            TString
            "The string to search for within <param str>" ]
      returnType = TInt
      description =
        "Returns the index of the last occurrence of <param searchFor> in "
        + "<param str>, measured in UTF-16 code units. Returns -1 if <param "
        + "searchFor> does not occur. For an EGC-indexed result that pairs with "
        + "{{String.length}} / {{String.slice}}, use {{stringLastIndexOfEgc}}."
      fn =
        (function
        | _, _, _, [| DString str; DString search |] ->
          let index = str.LastIndexOf(search)
          Ply(Dval.int (bigint index))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringLastIndexOfEgc" 0
      typeParams = []
      parameters =
        [ Param.make "str" TString "The string to search in"
          Param.make
            "searchFor"
            TString
            "The string to search for within <param str>" ]
      returnType = TInt
      description =
        "Returns the index of the last occurrence of <param searchFor> in "
        + "<param str>, measured in extended grapheme clusters (consistent with "
        + "{{String.length}}, {{String.slice}}, {{String.first}}, and "
        + "{{String.dropFirst}}). Returns -1 if <param searchFor> does not occur. "
        + "The match must begin and end on EGC boundaries — partial-grapheme "
        + "matches are not reported."
      fn =
        (function
        | _, _, _, [| DString str; DString search |] ->
          if search = "" then
            Ply(Dval.int (bigint (StringInfo(str).LengthInTextElements)))
          else
            let starts = StringInfo.ParseCombiningCharacters(str)
            let mutable lastFound = -1
            let mutable boundaryPtr = 0
            let mutable egcIndex = 0
            while egcIndex < starts.Length do
              let elementIndex = starts[egcIndex]
              let targetEnd = elementIndex + search.Length
              while boundaryPtr < starts.Length && starts[boundaryPtr] < targetEnd do
                boundaryPtr <- boundaryPtr + 1
              let endIsBoundary =
                if boundaryPtr < starts.Length then
                  starts[boundaryPtr] = targetEnd
                else
                  targetEnd = str.Length
              let matches =
                System.String.Compare(
                  str,
                  elementIndex,
                  search,
                  0,
                  search.Length,
                  System.StringComparison.Ordinal
                ) = 0
              if endIsBoundary && matches then lastFound <- egcIndex
              egcIndex <- egcIndex + 1
            Ply(Dval.int (bigint lastFound))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringToCodepoints" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TList TInt
      description =
        "Return the Unicode codepoints of <param s>, in order. A different question from "
        + "<fn String.toList>, which cuts <param s> into grapheme clusters: a flag, or an "
        + "emoji built with a zero-width joiner, is one Char and several codepoints."
      fn =
        (function
        | _, _, _, [| DString s |] ->
          s.EnumerateRunes()
          |> Seq.map (fun rune -> Dval.int (bigint rune.Value))
          |> Seq.toList
          |> Dval.list KTInt
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringCodepointLength" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TInt
      description =
        "Return how many Unicode codepoints <param s> has. At least <fn String.length>, "
        + "which counts grapheme clusters, and unrelated to <fn String.displayWidth>, "
        + "which counts the cells the text occupies."
      fn =
        (function
        | _, _, _, [| DString s |] ->
          // Count directly to avoid boxing the rune enumerator.
          let mutable count = 0
          let mutable i = 0

          while i < s.Length do
            let isPair =
              System.Char.IsHighSurrogate s[i]
              && i + 1 < s.Length
              && System.Char.IsLowSurrogate s[i + 1]

            count <- count + 1
            i <- i + (if isPair then 2 else 1)

          count |> bigint |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringGetByteAt" 0
      typeParams = []
      parameters = [ Param.make "s" TString ""; Param.make "index" TInt "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Return {{Some <var b>}} for the byte at <param index> of <param s>'s UTF-8 "
        + "encoding, or {{None}} when <param index> is negative or past the end. This "
        + "indexes bytes, not characters: every character outside ASCII spans more than "
        + "one, so an index here does not line up with one into <fn String.toList>."
      fn =
        (function
        | _, _, _, [| DString s; DInt index |] ->
          // Values outside int64 are necessarily past the end.
          match DarkInt.toInt64 index |> Option.bind (utf8ByteAt s) with
          | Some b -> Dval.optionSome KTUInt8 (DUInt8 b) |> Ply
          | None -> Dval.optionNone KTUInt8 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
