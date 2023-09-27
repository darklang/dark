module BuiltinExecution.Libs.String

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Globalization
open System.Security.Cryptography
open System
open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module TypeChecker = LibExecution.TypeChecker
module Interpreter = LibExecution.Interpreter


let modules = [ "String" ]
let fn = fn modules
let constant = constant modules

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []


let fns : List<BuiltInFn> =
  [ { name = fn "map" 0
      typeParams = []
      parameters =
        [ Param.make "s" TString ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton TChar, TChar))
            ""
            [ "character" ] ]
      returnType = TString
      description =
        "Iterate over each Character (EGC, not byte) in the string, performing the
         operation in <param fn> on each one."
      fn =
        (function
        | state, _, [ DString s; DFnVal b ] ->
          (String.toEgcSeq s
           |> Seq.toList
           |> Ply.List.mapSequentially (fun te ->
             let args = NEList.singleton (DChar te)
             Interpreter.applyFnVal state 0UL b [] args)
           |> Ply.bind (fun dvals ->
             dvals
             |> Ply.List.mapSequentially (function
               | DChar c -> Ply c
               | dv ->
                 TypeChecker.raiseFnValResultNotExpectedType SourceNone dv TChar)
             |> Ply.map (fun parts ->
               parts |> String.concat "" |> String.normalize |> DString)))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "toList" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TList TChar
      description = "Returns the list of Characters (EGC, not byte) in the string"
      fn =
        (function
        | _, _, [ DString s ] ->
          (s
           |> String.toEgcSeq
           |> Seq.map (fun c -> DChar c)
           |> Seq.toList
           |> Dval.list (ValueType.Known KTChar)
           |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "replaceAll" 0
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
        | _, _, [ DString s; DString search; DString replace ] ->
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


    { name = fn "toUppercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description = "Returns the string, uppercased"
      fn =
        (function
        | _, _, [ DString s ] -> Ply(DString(String.toUppercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "upper"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toLowercase" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description = "Returns the string, lowercased"
      fn =
        (function
        | _, _, [ DString s ] -> Ply(DString(String.toLowercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "lower"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "length" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, _, [ DString s ] -> s |> String.lengthInEgcs |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented // there isn't a unicode version of length
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "append" 0
      typeParams = []
      parameters = [ Param.make "s1" TString ""; Param.make "s2" TString "" ]
      returnType = TString
      description =
        "Concatenates the two strings by appending <param s2> to <param s1> and
         returns the joined string."
      fn =
        (function
        // TODO add fuzzer to ensure all strings are normalized no matter what we do to them.
        | _, _, [ DString s1; DString s2 ] ->
          (s1 + s2) |> String.normalize |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // CLEANUP move implementation to Darklang, in package space, in darklang stdlib
    // (blocked by lack of RegEx support in Dark)
    { name = fn "slugify" 0
      typeParams = []
      parameters = [ Param.make "string" TString "" ]
      returnType = TString
      description =
        "Turns a string into a prettified slug, including only lowercased
         alphanumeric characters, joined by hyphens"
      fn =
        (function
        | _, _, [ DString s ] ->
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


    { name = fn "reverse" 0
      typeParams = []
      parameters = [ Param.make "string" TString "" ]
      returnType = TString
      description = "Reverses <param string>"
      fn =
        (function
        | _, _, [ DString s ] ->
          String.toEgcSeq s |> Seq.rev |> String.concat "" |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "reverse"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "split" 0
      typeParams = []
      parameters = [ Param.make "s" TString ""; Param.make "separator" TString "" ]
      returnType = TList TString
      description =
        "Splits a string at the separator, returning a list of strings without the separator.
        If the separator is not present, returns a list containing only the initial string."
      fn =
        (function
        | _, _, [ DString s; DString sep ] ->
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

          parts |> List.map DString |> Dval.list (ValueType.Known KTString) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "join" 0
      typeParams = []
      parameters =
        [ Param.make "l" (TList TString) ""; Param.make "separator" TString "" ]
      returnType = TString
      description = "Combines a list of strings with the provided separator"
      fn =
        (function
        | _, _, [ DList(_, l); DString sep ] ->
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


    { name = fn "slice" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""
          Param.make "from" TInt ""
          Param.make "to" TInt "" ]
      returnType = TString
      description =
        "Returns the substring of <param string> between the <param from> and <param
         to> indices.

         Negative indices start counting from the end of <param string>."
      fn =
        (function
        | _, _, [ DString s; DInt first; DInt last ] ->
          String.toEgcSeq s
          |> Seq.toList
          |> FSharpPlus.List.drop (int first)
          |> List.truncate (int (last - first))
          |> String.concat ""
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "trim" 0
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
        | _, _, [ DString toTrim ] -> toTrim.Trim() |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "trim"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "trimStart" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all leading whitespace removed. 'whitespace'
         here means all Unicode characters with the {{White_Space}} property, which
         includes {{\" \"}}, {{\"\\t\"}} and {{\"\\n\"}}"
      fn =
        (function
        | _, _, [ DString toTrim ] -> Ply(DString(toTrim.TrimStart()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "ltrim"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "trimEnd" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TString
      description =
        "Returns a copy of <param str> with all trailing whitespace removed.
         'whitespace' here means all Unicode characters with the {{White_Space}}
         property, which includes {{\" \"}}, {{\"\\t\"}} and {{\"\\n\"}}."
      fn =
        (function
        | _, _, [ DString toTrim ] -> Ply(DString(toTrim.TrimEnd()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "rtrim"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "toBytes" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TBytes
      description =
        "Converts the given unicode string to a UTF8-encoded byte sequence."
      fn =
        (function
        | _, _, [ DString str ] ->
          let theBytes = System.Text.Encoding.UTF8.GetBytes str
          Ply(DBytes theBytes)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromBytesWithReplacement" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Converts the UTF8-encoded byte sequence into a string. Errors will be ignored by replacing invalid characters"
      fn =
        (function
        | _, _, [ DBytes bytes ] ->
          let str = System.Text.Encoding.UTF8.GetString bytes
          Ply(DString str)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fromBytes" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TypeReference.option TString
      description =
        "Converts the UTF8-encoded byte sequence into a string. Errors will be ignored by replacing invalid characters"
      fn =
        (function
        | _, _, [ DBytes bytes ] ->
          try
            let str = System.Text.UTF8Encoding(false, true).GetString bytes
            Dval.optionSome VT.string (DString str) |> Ply
          with e ->
            Dval.optionNone VT.string |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "indexOf" 0
      typeParams = []
      parameters =
        [ Param.make "str" TString "The string to search in"
          Param.make
            "searchFor"
            TString
            "The string to search for within <param str>" ]
      returnType = TInt
      description =
        "Returns the index of the first occurrence of <param searchFor> in <param str>, or returns -1 if <param searchFor> does not occur."
      fn =
        (function
        | _, _, [ DString str; DString search ] ->
          let index = str.IndexOf(search)
          Ply(DInt index)
        | _ -> incorrectArgs ())
      sqlSpec = SqlCallback2(fun str search -> $"strpos({str}, {search}) - 1")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "head" 0
      typeParams = []
      parameters = [ Param.make "str" TString "" ]
      returnType = TypeReference.option TChar
      description =
        "Returns {{Some char}} of the first character of <param str>, or returns {{None}} if <param str> is empty."
      fn =
        (function
        | _, _, [ DString str ] ->
          if str = "" then
            Dval.optionNone VT.char |> Ply
          else
            let head = String.toEgcSeq str |> Seq.head
            Dval.optionSome VT.char (DChar head) |> Ply
        | _ -> incorrectArgs ())

      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
