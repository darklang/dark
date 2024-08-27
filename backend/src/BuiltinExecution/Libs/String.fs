module BuiltinExecution.Libs.String

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Globalization
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module Interpreter = LibExecution.Interpreter


let fns : List<BuiltInFn> =
  [
    // { name = fn "stringMap" 0
    //   typeParams = []
    //   parameters =
    //     [ Param.make "s" TString ""
    //       Param.makeWithArgs
    //         "fn"
    //         (TFn(NEList.singleton TChar, TChar))
    //         ""
    //         [ "character" ] ]
    //   returnType = TString
    //   description =
    //     "Iterate over each Char (EGC, not byte) in the string, performing the
    //      operation in <param fn> on each one."
    //   fn =
    //     (function
    //     | state, _, _, [ DString s; DFnVal b ] ->
    //       (String.toEgcSeq s
    //        |> Seq.toList
    //        |> Ply.List.mapSequentially (fun te ->
    //          let args = NEList.singleton (DChar te)
    //          Interpreter.applyFnVal state b [] args)
    //        |> Ply.bind (fun dvals ->
    //          dvals
    //          |> Ply.List.mapSequentially (function
    //            | DChar c -> Ply c
    //            | dv ->
    //              TypeChecker.raiseFnValResultNotExpectedType
    //                state.tracing.callStack
    //                dv
    //                TChar)
    //          |> Ply.map (fun parts ->
    //            parts |> String.concat "" |> String.normalize |> DString)))
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotQueryable
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    { name = fn "stringToList" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TList TChar
      description = "Returns the list of Characters (EGC, not byte) in the string"
      fn =
        (function
        | _, _, _, [ DString s ] ->
          s
          |> String.toEgcSeq
          |> Seq.map (fun c -> DChar c)
          |> Seq.toList
          |> Dval.list KTChar
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
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
              String.toEgcSeq s
              |> Seq.toList
              |> FSharpPlus.List.intersperse replace
              |> (fun l -> replace :: l @ [ replace ])
              |> String.concat ""
              |> DString
              |> Ply
          else
            Ply(DString(s.Replace(search, replace)))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "replace"
      previewable = Pure
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
      deprecated = NotDeprecated }


    { name = fn "stringLength" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TInt64
      description = "Returns the length of the string"
      fn =
        (function
        | _, _, _, [ DString s ] ->
          s |> String.lengthInEgcs |> int64 |> Dval.int64 |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented // there isn't a unicode version of length
      previewable = Pure
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
      deprecated = NotDeprecated }


    // CLEANUP move implementation to Darklang, in package space, in darklang stdlib
    // (blocked by lack of RegEx support in Dark)
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
            | dv -> Exception.raiseInternal "expected string in join" [ "dval", dv ])
          |> String.concat sep
          |> String.normalize
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "stringSlice" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""
          Param.make "from" TInt64 ""
          Param.make "to" TInt64 "" ]
      returnType = TString
      description =
        "Returns the substring of <param string> between the <param from> and <param to> indices.
         Negative indices start counting from the end of <param string>."
      fn =
        (function
        | _, _, _, [ DString s; DInt64 first; DInt64 last ] ->
          let getLengthInTextElements s =
            System.Globalization.StringInfo(s).LengthInTextElements

          // Handle negative indexes (which allow counting from the end)
          let first =
            if first < 0 then getLengthInTextElements (s) + int first else int first
          let last =
            if last < 0 then getLengthInTextElements (s) + int last else int last

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
      deprecated = NotDeprecated }


    { name = fn "stringToBytes" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TList TUInt8
      description =
        "Converts the given unicode string to a UTF8-encoded byte sequence."
      fn =
        (function
        | _, _, _, [ DString str ] ->
          let theBytes = System.Text.Encoding.UTF8.GetBytes str
          Ply(Dval.byteArrayToDvalList theBytes)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "stringFromBytesWithReplacement" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TString
      description =
        "Converts the UTF8-encoded byte sequence into a string. Errors will be ignored by replacing invalid characters"
      fn =
        (function
        | _, _, _, [ DList(_vt, bytes) ] ->
          let bytes = Dval.dlistToByteArray bytes
          let str = System.Text.Encoding.UTF8.GetString bytes
          Ply(DString str)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "stringFromBytes" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TypeReference.option TString
      description =
        "Converts the UTF8-encoded byte sequence into a string. Errors will be ignored by replacing invalid characters"
      fn =
        (function
        | _, _, _, [ DList(_vt, bytes) ] ->
          try
            let bytes = Dval.dlistToByteArray bytes
            let str = System.Text.UTF8Encoding(false, true).GetString bytes
            Dval.optionSome KTString (DString str) |> Ply
          with e ->
            Dval.optionNone KTString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
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
        "Returns the index of the first occurrence of <param searchFor> in <param str>, or returns -1 if <param searchFor> does not occur."
      fn =
        (function
        | _, _, _, [ DString str; DString search ] ->
          let index = str.IndexOf(search)
          Ply(DInt64 index)
        | _ -> incorrectArgs ())
      sqlSpec = SqlCallback2(fun str search -> $"strpos({str}, {search}) - 1")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "stringHead" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TypeReference.option TChar
      description =
        "Returns {{Some char}} of the first character of <param str>, or returns {{None}} if <param str> is empty."
      fn =
        (function
        | _, _, _, [ DString str ] ->
          if str = "" then
            Dval.optionNone KTChar |> Ply
          else
            let head = String.toEgcSeq str |> Seq.head
            Dval.optionSome KTChar (DChar head) |> Ply
        | _ -> incorrectArgs ())

      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
