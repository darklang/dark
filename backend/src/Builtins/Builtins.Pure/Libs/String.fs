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
module Bytes = LibExecution.Bytes

/// The first offset at or after `offset` whose char is U+0300 or above or is CR, or `s.Length`.
/// Every char before it, except perhaps the last, is a cluster of its own: that one may combine
/// with the char after. See `String.clusterEnd`.
let private simpleRunEnd (s : string) (offset : int) : int =
  let rest = System.MemoryExtensions.AsSpan(s, offset)
  let other = System.MemoryExtensions.IndexOfAnyExceptInRange(rest, char 0, '\u02FF')
  let runEnd = if other < 0 then s.Length else offset + other
  let cr = s.IndexOf('\r', offset, runEnd - offset)
  if cr < 0 then runEnd else cr

/// The UTF-16 offset of the first `needle` in `s` at or after `from` that starts and ends on
/// cluster boundaries, or -1. `needle` is not empty.
///
/// `offset` is a cluster boundary at or before `from`, and `index` the number of clusters before
/// it. Both move forward as the search walks, so a search that carries on from a match does not
/// walk the text again. Searches ordinally and walks only as far as a hit, so a miss costs what
/// ordinal `IndexOf` does, and a run of simple chars is jumped rather than walked.
let private nextEgcMatch
  (s : string)
  (needle : string)
  (from : int)
  (offset : byref<int>)
  (index : byref<int>)
  : int =
  let mutable at = s.IndexOf(needle, from, System.StringComparison.Ordinal)
  let mutable found = -1
  let mutable runEnd = offset
  while found < 0 && at >= 0 do
    while offset < at do
      if offset >= runEnd then runEnd <- simpleRunEnd s offset
      // Jump over simple chars, one cluster per char, rather than asking for each cluster.
      let jump = min at (runEnd - 1)
      if jump > offset then
        index <- index + (jump - offset)
        offset <- jump
      else
        offset <- String.clusterEnd s offset
        index <- index + 1
    if offset = at && String.landsOn s (at + needle.Length) at then
      found <- at
    else
      at <- s.IndexOf(needle, at + 1, System.StringComparison.Ordinal)
  found

/// The cluster index of the first whole-cluster `needle` in `s`, or -1. `needle` is not empty.
let private egcIndexOf (s : string) (needle : string) : int =
  if String.isCharwise s then
    s.IndexOf(needle, System.StringComparison.Ordinal)
  else
    let mutable offset = 0
    let mutable index = 0
    if nextEgcMatch s needle 0 &offset &index < 0 then -1 else index

/// The cluster index of the last whole-cluster `needle` in `s`, or -1. `needle` is not empty.
let private egcLastIndexOf (s : string) (needle : string) : int =
  if String.isCharwise s then
    s.LastIndexOf(needle, System.StringComparison.Ordinal)
  else
    let mutable offset = 0
    let mutable index = 0
    let mutable last = -1
    let mutable at = nextEgcMatch s needle 0 &offset &index
    while at >= 0 do
      last <- index
      at <- nextEgcMatch s needle (at + 1) &offset &index
    last


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
      returnType = TInt64
      description =
        "Cells the text occupies in a fixed-width grid, over extended grapheme clusters"
      fn =
        (function
        | _, _, _, [| DString text |] ->
          text |> TextWidth.ofString |> int64 |> Dval.dint64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "stringCompare" 0
      typeParams = []
      parameters = [ Param.make "a" TString ""; Param.make "b" TString "" ]
      returnType = TInt64
      description =
        "Orders two strings by code point: a negative number when <param a> sorts first, 0 when they are equal, a positive number when <param b> sorts first. Code point order is also UTF-8 byte order, and not language-aware, so every machine orders the same pair the same way. Only the sign is meaningful."
      fn =
        (function
        | _, _, _, [| DString a; DString b |] ->
          Ply(DInt64(int64 (String.compareByCodepoint a b)))
        | _ -> incorrectArgs ())
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
          s |> String.mapEgcs DChar |> Dval.list KTChar |> Ply
        | _ -> incorrectArgs ())
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
        "Replace every <param searchFor> in <param s> with <param replaceWith>. Matches whole "
        + "characters (extended grapheme clusters) only, so a skin-tone emoji does not match the "
        + "plain emoji inside it. An empty <param searchFor> matches between every two characters "
        + "and at both ends."
      fn =
        (function
        | _, _, _, [| DString s; DString search; DString replace |] ->
          let replaced =
            if search = "" then
              // `replace` around and between the clusters.
              let sb = StringBuilder(replace)
              for egc in String.egcs s do
                sb.Append(egc).Append(replace) |> ignore<StringBuilder>
              sb.ToString()
            elif String.isCharwise s then
              s.Replace(search, replace)
            else
              let sb = StringBuilder()
              let mutable offset = 0
              let mutable index = 0
              let mutable copied = 0
              let mutable at = nextEgcMatch s search 0 &offset &index
              while at >= 0 do
                sb.Append(s, copied, at - copied).Append(replace)
                |> ignore<StringBuilder>
                copied <- at + search.Length
                at <- nextEgcMatch s search copied &offset &index
              sb.Append(s, copied, s.Length - copied).ToString()
          // A replacement can start with a mark that composes with what precedes it.
          replaced |> String.normalize |> DString |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringToUppercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description =
        "Returns <param s> uppercased with Unicode's full case mapping, which can "
        + "make it longer"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          Ply(DString(Builtins.Pure.CaseMapping.toUppercase s))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringToLowercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description =
        "Returns <param s> lowercased with Unicode's full case mapping, which can "
        + "make it longer"
      fn =
        (function
        | _, _, _, [| DString s |] ->
          Ply(DString(Builtins.Pure.CaseMapping.toLowercase s))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringLength" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TInt64
      description = "Returns the length of the string"
      fn =
        (function
        | _, _, _, [| DStr s |] -> s.EgcCount |> int64 |> Dval.dint64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringRepeat" 0
      typeParams = []
      parameters =
        [ Param.make "s" TString "The string to repeat"
          Param.make "count" TInt64 "How many times to repeat it" ]
      returnType = TString
      description =
        "Returns <param s> repeated <param count> times. An empty string when <param count> is not positive."
      fn =
        (function
        | _, vm, _, [| DString s; DInt64 count |] ->
          DString(String.repeat s (int64ToInt32 vm count)) |> Ply
        | _ -> incorrectArgs ())
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
          if String.isCharwise s then
            let chars = s.ToCharArray()
            System.Array.Reverse chars
            Ply(DString(System.String chars))
          else
            String.egcsReversed s |> String.concat "" |> DString |> Ply
        | _ -> incorrectArgs ())
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
                result.Add(inProgress.ToString())
              elif startsWithSeparator strRemaining then
                result.Add(inProgress.ToString())

                r (List.skip sep.Length strRemaining, StringBuilder())
              else
                r (strRemaining.Tail, inProgress.Append(strRemaining.Head))

            r (str, StringBuilder())

            List.ofSeq result

          let parts =
            if sep = "" then
              String.egcs s
            // When both sides are charwise, splitting by cluster and splitting by char give the
            // same answer, and the framework's split does it without building a string per
            // character of the input. Paths, keys and identifiers all take this route.
            elif String.isCharwise s && String.isCharwise sep then
              s.Split([| sep |], System.StringSplitOptions.None) |> Array.toList
            else
              ecgStringSplit (String.egcs s) (String.egcs sep)

          parts |> List.map DString |> Dval.list KTString |> Ply
        | _ -> incorrectArgs ())
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
        | _, vm, _, [| DList(_, Items(l)) as list; DString sep |] ->
          l
          |> List.map (fun s ->
            match s with
            | DString st -> st
            | _ ->
              RuntimeError.Applications.FnParameterNotExpectedType(
                FQFnName.fqBuiltin "stringJoin" 0,
                0,
                "l",
                None,
                VT.list VT.string,
                Dval.toValueType list,
                list
              )
              |> RuntimeError.Apply
              |> raiseRTE vm.threadID)
          |> String.concat sep
          |> String.normalize
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringDropFirst" 0
      typeParams = []
      parameters = [ Param.make "string" TString ""; Param.make "count" TInt64 "" ]
      returnType = TString
      description =
        "Returns <param string> with the first <param count> characters removed. The whole string "
        + "if <param count> is negative, the empty string if it is longer than <param string>."
      fn =
        (function
        | _, _, _, [| DString s; DInt64 countD |] ->
          // The Dark version was two `String.length` calls, two comparisons and a `slice`.
          Ply(DString(String.dropFirstEgcs s countD))
        | _ -> incorrectArgs ())
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
          // but without the three package calls, and without walking either string to its end.
          Ply(DBool(String.startsWithEgcs subject prefix))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringSlice" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""
          Param.make "from" TInt64 ""
          Param.make "to" TInt64 "" ]
      returnType = TString
      description =
        "Returns the substring of <param string> between the <param from> and "
        + "<param to> indices. Negative indices start counting from the end of "
        + "<param string>."
      fn =
        (function
        | _, _, _, [| DString s; DInt64 firstD; DInt64 lastD |] ->
          // Normalizing the indices here rather than in the Dark wrapper, which built a lambda and
          // applied it twice to do exactly this. That wrapper is a bare forwarder now, so it elides.
          String.sliceEgcs s firstD lastD |> DString |> Ply
        | _ -> incorrectArgs ())
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
        "Returns {{true}} if <param lookingIn> contains <param searchingFor> as whole characters "
        + "(extended grapheme clusters): half a flag, or the plain emoji inside a skin-tone one, "
        + "does not count."
      fn =
        (function
        | _, _, _, [| DString lookingIn; DString searchingFor |] ->
          Ply(DBool(searchingFor = "" || egcIndexOf lookingIn searchingFor >= 0))
        | _ -> incorrectArgs ())
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
      returnType = TInt64
      description =
        "Returns the index of the first <param searchFor> in <param str>, counted in characters "
        + "(extended grapheme clusters) like {{String.length}} and {{String.slice}}. Returns -1 if "
        + "it does not occur. Matches whole characters only."
      fn =
        (function
        | _, _, _, [| DString str; DString search |] ->
          let index = if search = "" then 0 else egcIndexOf str search
          Ply(Dval.dint64 (int64 index))
        | _ -> incorrectArgs ())
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
      returnType = TInt64
      description =
        "Returns the index of the last <param searchFor> in <param str>, counted in characters "
        + "(extended grapheme clusters) like {{String.length}} and {{String.slice}}. Returns -1 if "
        + "it does not occur. Matches whole characters only."
      fn =
        (function
        | _, _, _, [| DStr str; DString search |] ->
          let index =
            if search = "" then str.EgcCount else egcLastIndexOf str.Value search
          Ply(Dval.dint64 (int64 index))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringToCodepoints" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TList TUInt32
      description =
        "Return the Unicode codepoints of <param s>, in order. A different question from "
        + "<fn String.toList>, which cuts <param s> into grapheme clusters: a flag, or an "
        + "emoji built with a zero-width joiner, is one Char and several codepoints."
      fn =
        (function
        | _, _, _, [| DString s |] ->
          s.EnumerateRunes()
          |> Seq.map (fun rune -> DUInt32(uint32 rune.Value))
          |> Seq.toList
          |> Dval.list KTUInt32
          |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringCodepointLength" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TInt64
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

          count |> int64 |> Dval.dint64 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "stringGetByteAt" 0
      typeParams = []
      parameters = [ Param.make "s" TString ""; Param.make "index" TInt64 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Return {{Some <var b>}} for the byte at <param index> of <param s>'s UTF-8 "
        + "encoding, or {{None}} when <param index> is negative or past the end. This "
        + "indexes bytes, not characters: every character outside ASCII spans more than "
        + "one, so an index here does not line up with one into <fn String.toList>."
      fn =
        (function
        | _, _, _, [| DString s; DInt64 index |] ->
          match utf8ByteAt s index with
          | Some b -> Dval.optionSome KTUInt8 (DUInt8 b) |> Ply
          | None -> Dval.optionNone KTUInt8 |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
