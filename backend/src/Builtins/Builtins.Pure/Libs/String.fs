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

let fns () : List<BuiltInFn> =
  [ { name = fn "stringToList" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TList TChar
      description = "Returns the list of Characters (EGC, not byte) in the string"
      fn =
        (function
        | _, _, _, [ DString s ] ->
          s
          |> String.toEgcSeq
          |> Seq.map DChar
          |> Seq.toList
          |> Dval.list KTChar
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringReplaceAll" 0
      typeParams = []
      parameters =
        [ Param.make "s" TString "The string to operate on"
          Param.make "searchFor" TString "The string to search for within <param s>"
          Param.make "replaceWith" TString "" ]
      returnType = TString
      description =
        "Replace all instances on <param searchFor> in <param s> with <param
         replaceWith>"
      fn =
        (function
        | _, _, _, [ DString s; DString search; DString replace ] ->
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringToUppercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description = "Returns the string, uppercased"
      fn =
        (function
        | _, _, _, [ DString s ] -> Ply(DString(String.toUppercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "upper"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringToLowercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description = "Returns the string, lowercased"
      fn =
        (function
        | _, _, _, [ DString s ] -> Ply(DString(String.toLowercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "lower"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringLength" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, _, _, [ DString s ] ->
          s |> String.lengthInEgcs |> bigint |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented // CLEANUP: Sqlite has "LENGTH" but that counts characters; if we can get it to count EGCs, great
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringAppend" 0
      typeParams = []
      parameters = [ Param.make "s1" TString ""; Param.make "s2" TString "" ]
      returnType = TString
      description =
        "Concatenates the two strings by appending <param s2> to <param s1> and
         returns the joined string."
      fn =
        (function
        // TODO add fuzzer to ensure all strings are normalized no matter what we do to them.
        | _, _, _, [ DString s1; DString s2 ] ->
          (s1 + s2) |> String.normalize |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // CLEANUP move implementation to Darklang, in package space, in darklang stdlib
    // (RegEx support now available via Stdlib.Regex.*)
    { name = fn "stringSlugify" 0
      typeParams = []
      parameters = [ Param.make "string" TString "" ]
      returnType = TString
      description =
        "Turns a string into a prettified slug, including only lowercased
         alphanumeric characters, joined by hyphens"
      fn =
        (function
        | _, _, _, [ DString s ] ->
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringReverse" 0
      typeParams = []
      parameters = [ Param.make "string" TString "" ]
      returnType = TString
      description = "Reverses <param string>"
      fn =
        (function
        | _, _, _, [ DString s ] ->
          String.toEgcSeq s |> Seq.rev |> String.concat "" |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "reverse"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringSplit" 0
      typeParams = []
      parameters = [ Param.make "s" TString ""; Param.make "separator" TString "" ]
      returnType = TList TString
      description =
        "Splits a string at the separator, returning a list of strings without the separator.
        If the separator is not present, returns a list containing only the initial string."
      fn =
        (function
        | _, _, _, [ DString s; DString sep ] ->
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
            else
              ecgStringSplit
                (s |> String.toEgcSeq |> Seq.toList)
                (sep |> String.toEgcSeq |> Seq.toList)

          parts |> List.map DString |> Dval.list KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringJoin" 0
      typeParams = []
      parameters =
        [ Param.make "l" (TList TString) ""; Param.make "separator" TString "" ]
      returnType = TString
      description = "Combines a list of strings with the provided separator"
      fn =
        (function
        | _, _, _, [ DList(_, l); DString sep ] ->
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringSlice" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""
          Param.make "from" TInt ""
          Param.make "to" TInt "" ]
      returnType = TString
      description =
        "Returns the substring of <param string> between the <param from> and <param to> indices.
         Negative indices start counting from the end of <param string>."
      fn =
        (function
        | _, _, _, [ DString s; DInt firstD; DInt lastD ] ->
          let getLengthInTextElements s = StringInfo(s).LengthInTextElements

          // slice positions are bounded by string length; narrow Int -> native int
          let first = int (DarkInt.toBigInt firstD)
          let last = int (DarkInt.toBigInt lastD)

          // Handle negative indexes (which allow counting from the end)
          let first =
            if first < 0 then getLengthInTextElements (s) + first else first
          let last = if last < 0 then getLengthInTextElements (s) + last else last

          if first >= last then
            Ply(DString "")
          else
            // Create a TextElementEnumerator to handle EGCs
            let textElemEnumerator =
              System.Globalization.StringInfo.GetTextElementEnumerator(s)
            let mutable startIndex = 0
            let mutable endIndex = 0
            let mutable index = 0

            // Iterate through EGCs and record the byte indexes of the start and end
            while textElemEnumerator.MoveNext() do
              if index = first then startIndex <- textElemEnumerator.ElementIndex
              if index = last then endIndex <- textElemEnumerator.ElementIndex
              index <- index + 1

            // Check out of bounds
            if endIndex = 0 then endIndex <- s.Length

            let substringLength = endIndex - startIndex
            s.Substring(startIndex, substringLength) |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringTrim" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all leading and trailing whitespace
         removed. 'whitespace' here means all Unicode characters with the
         {{White_Space}} property, which includes {{\" \"}}, {{\"\\t\"}} and
         {{\"\\n\"}}"
      fn =
        (function
        | _, _, _, [ DString toTrim ] -> toTrim.Trim() |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "trim"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringTrimStart" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all leading whitespace removed. 'whitespace'
         here means all Unicode characters with the {{White_Space}} property, which
         includes {{\" \"}}, {{\"\\t\"}} and {{\"\\n\"}}"
      fn =
        (function
        | _, _, _, [ DString toTrim ] -> Ply(DString(toTrim.TrimStart()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "ltrim"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringTrimEnd" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all trailing whitespace removed.
         'whitespace' here means all Unicode characters with the {{White_Space}}
         property, which includes {{\" \"}}, {{\"\\t\"}} and {{\"\\n\"}}."
      fn =
        (function
        | _, _, _, [ DString toTrim ] -> Ply(DString(toTrim.TrimEnd()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "rtrim"
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringToBlob" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TBlob
      description = "Converts the given unicode string to a UTF8-encoded Blob."
      fn =
        (function
        | _, _, _, [ DString str ] ->
          let theBytes = System.Text.Encoding.UTF8.GetBytes str
          Blob.newEphemeral theBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringFromBlobWithReplacement" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TString
      description =
        "Converts the UTF8-encoded <param blob> into a string. Invalid sequences are replaced."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bytes = Blob.readBytes state ref
            return DString(System.Text.Encoding.UTF8.GetString bytes)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringFromBytes" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TypeReference.option TString
      description =
        "Converts the UTF8-encoded byte sequence into a string. Returns None if the bytes aren't valid UTF-8."
      fn =
        (function
        | _, _, _, [ DList(_vt, bytes) ] ->
          try
            let bytes = Dval.dlistToByteArray bytes
            let str = UTF8Encoding(false, true).GetString bytes
            Dval.optionSome KTString (DString str) |> Ply
          with _e ->
            Dval.optionNone KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringFromBlob" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TypeReference.option TString
      description =
        "Converts the UTF8-encoded <param blob> into a string. Returns None if the bytes aren't valid UTF-8."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "stringContains" 0
      typeParams = []
      parameters =
        [ Param.make "lookingIn" TString "The string to search within"
          Param.make "searchingFor" TString "The substring to look for" ]
      returnType = TBool
      description =
        "Returns {{true}} if <param lookingIn> contains <param searchingFor>.
         SQL-queryable: it only checks found/not-found, which SQLite's INSTR and
         .NET's Contains agree on (unlike index position, which they count
         differently for some Unicode text)."
      fn =
        (function
        | _, _, _, [ DString lookingIn; DString searchingFor ] ->
          Ply(DBool(lookingIn.Contains(searchingFor)))
        | _ -> incorrectArgs ())
      // Emits a complete boolean fragment, so no Int value reaches the SqlCompiler.
      sqlSpec =
        SqlCallback2(fun lookingIn searchingFor ->
          $"(INSTR({lookingIn}, {searchingFor}) > 0)")
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
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
        "Returns the index of the first occurrence of <param searchFor> in <param str>,
         measured in UTF-16 code units (the .NET string representation). Returns -1 if
         <param searchFor> does not occur. For an index that pairs with {{String.length}},
         {{String.slice}}, {{String.first}}, and {{String.dropFirst}} (which are all
         EGC-indexed), use {{stringIndexOfEgc}}.

         Note on SQL: SQLite's INSTR and .NET's IndexOf both tell us whether
         a substring was found, but they count positions differently for some
         Unicode text. SQLite counts Unicode characters; .NET IndexOf counts
         UTF-16 code units. For example, SQLite reports the \"a\" in \"😄a\" at
         index 1, while .NET reports it at index 2 because the emoji takes two
         UTF-16 code units. So {{String.contains}} is portable in SQL because it
         only checks found/not-found, but {{String.indexOf}} may return different
         numeric indexes outside simple ASCII text."
      fn =
        (function
        | _, _, _, [ DString str; DString search ] ->
          let index = str.IndexOf(search)
          Ply(Dval.int (bigint index))
        | _ -> incorrectArgs ())
      // CLEANUP: now returns Int, which the SqlCompiler can't compile, so this
      // sqlSpec is dormant — String.indexOf isn't usable in DB.query until Int is
      // queryable. (String.contains has its own queryable Bool builtin, so it's
      // unaffected.) The INSTR spec is kept so it works again automatically once
      // Int becomes queryable.
      sqlSpec = SqlCallback2(fun str search -> $"(INSTR({str}, {search}) - 1)")
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
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
        "Returns the index of the first occurrence of <param searchFor> in <param str>,
         measured in extended grapheme clusters (consistent with {{String.length}},
         {{String.slice}}, {{String.first}}, and {{String.dropFirst}}). Returns -1 if
         <param searchFor> does not occur. The match must begin and end on EGC
         boundaries — partial-grapheme matches (e.g. finding a skin-tone modifier
         alone inside a full skin-toned emoji) are not reported. Not SQL-queryable
         because SQLite has no EGC-aware INSTR; use {{stringIndexOf}} inside DB
         query lambdas."
      fn =
        (function
        | _, _, _, [ DString str; DString search ] ->
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
      capabilities = LibExecution.Capabilities.noCaps
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
        "Returns the index of the last occurrence of <param searchFor> in <param str>,
         measured in UTF-16 code units. Returns -1 if <param searchFor> does not occur.
         For an EGC-indexed result that pairs with {{String.length}} / {{String.slice}},
         use {{stringLastIndexOfEgc}}."
      fn =
        (function
        | _, _, _, [ DString str; DString search ] ->
          let index = str.LastIndexOf(search)
          Ply(Dval.int (bigint index))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
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
        "Returns the index of the last occurrence of <param searchFor> in <param str>,
         measured in extended grapheme clusters (consistent with {{String.length}},
         {{String.slice}}, {{String.first}}, and {{String.dropFirst}}). Returns -1 if
         <param searchFor> does not occur. The match must begin and end on EGC
         boundaries — partial-grapheme matches are not reported."
      fn =
        (function
        | _, _, _, [ DString str; DString search ] ->
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
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
