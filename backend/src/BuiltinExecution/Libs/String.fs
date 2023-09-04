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

module Errors = LibExecution.Errors


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
             LibExecution.Interpreter.applyFnVal state 0UL b [] args)
           |> (fun dvals ->
             (uply {
               let! (dvals : List<Dval>) = dvals

               match List.tryFind (fun dv -> Dval.isFake dv) dvals with
               | Some i -> return i
               | None ->
                 let chars =
                   List.map
                     (function
                     | DChar c -> c
                     | dv ->
                       Exception.raiseInternal
                         (Errors.expectedLambdaType "fn" TChar dv)
                         [])
                     dvals

                 let str = String.concat "" chars
                 return DString str
             })))

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
           |> DList
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

          if sep = "" then
            s |> String.toEgcSeq |> Seq.toList |> List.map DString |> DList |> Ply
          else
            ecgStringSplit
              (s |> String.toEgcSeq |> Seq.toList)
              (sep |> String.toEgcSeq |> Seq.toList)
            |> List.map DString
            |> DList
            |> Ply
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
        | _, _, [ DList l; DString sep ] ->
          let strs =
            List.map
              (fun s ->
                match s with
                | DString st -> st
                | dv ->
                  Exception.raiseInternal
                    (Errors.argumentWasntType (TList TString) "l" dv)
                    [])
              l

          Ply(DString((String.concat sep strs).Normalize()))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "base64Encode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description =
        "URLBase64 encodes a string without padding. Uses URL-safe encoding with
        {{-}} and {{_}} instead of {{+}} and {{/}}, as defined in
        [RFC 4648 section 5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)"
      fn =
        (function
        | _, _, [ DString s ] ->
          let defaultEncoded = s |> UTF8.toBytes |> Convert.ToBase64String
          // Inlined version of Base64.urlEncodeToString, except
          defaultEncoded.Replace('+', '-').Replace('/', '_').Replace("=", "")
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "base64Decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TString TString
      description =
        "Base64 decodes a string. The returned value is wrapped in {{Result}}.
         Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648
         sections](https://www.rfc-editor.org/rfc/rfc4648.html)
         [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, _, [ DString s ] ->
          let errPipe e = e |> DString |> Dval.resultError |> Ply
          let okPipe r = r |> DString |> Dval.resultOk |> Ply
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            // This seems like we should allow it
            "" |> okPipe
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            "Not a valid base64 string" |> errPipe
          else
            try
              s
              |> base64FromUrlEncoded
              |> Convert.FromBase64String
              |> System.Text.Encoding.UTF8.GetString
              |> String.normalize
              |> okPipe
            with e ->
              "Not a valid base64 string" |> errPipe
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      // CLEANUP: this shouldnt return a string and should be deprecated
      deprecated = NotDeprecated }


    { name = fn "digest" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TString
      description =
        "Take a string and hash it to a cryptographically-secure digest.
         Don't rely on either the size or the algorithm."
      fn =
        (function
        | _, _, [ DString s ] ->
          let sha384Hash = SHA384.Create()
          let data = System.Text.Encoding.UTF8.GetBytes(s)

          let bytes = sha384Hash.ComputeHash(data)

          // Deliberately keep padding
          System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "random" 0
      typeParams = []
      parameters = [ Param.make "length" TInt "" ]
      returnType = TypeReference.result TString TString
      description =
        "Generate a <type String> of length <param length> from random characters"
      fn =
        (function
        | _, _, [ DInt l as dv ] ->
          if l < 0L then
            dv
            |> Errors.argumentWasnt "positive" "length"
            |> DString
            |> Dval.resultError
            |> Ply
          else
            let randomString length =
              let gen () =
                match RNG.GetInt32(26 + 26 + 10) with
                | n when n < 26 -> ('a' |> int) + n
                | n when n < 26 + 26 -> ('A' |> int) + n - 26
                | n -> ('0' |> int) + n - 26 - 26

              let gen _ = char (gen ()) in

              (Array.toList (Array.init length gen))
              |> List.map string
              |> String.concat ""

            randomString (int l) |> DString |> Dval.resultOk |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "htmlEscape" 0
      typeParams = []
      parameters = [ Param.make "html" TString "" ]
      returnType = TString
      description =
        "Escape an untrusted string in order to include it safely in HTML output"
      fn =
        (function
        | _, _, [ DString s ] ->
          let htmlEscape (html : string) : string =
            List.map
              (fun c ->
                match c with
                | '<' -> "&lt;"
                | '>' -> "&gt;"
                | '&' -> "&amp;"
                // include these for html-attribute-escaping
                // even though they're not strictly necessary
                // for html-escaping proper.
                | '"' -> "&quot;"
                // &apos; doesn't work in IE....
                | ''' -> "&#x27;"
                | _ -> string c)
              (Seq.toList html)
            |> String.concat ""

          Ply(DString(htmlEscape s))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "contains" 0
      typeParams = []
      parameters =
        [ Param.make "lookingIn" TString ""; Param.make "searchingFor" TString "" ]
      returnType = TBool
      description = "Checks if <param lookingIn> contains <param searchingFor>"
      fn =
        (function
        | _, _, [ DString haystack; DString needle ] ->
          Ply(DBool(haystack.Contains needle))
        | _ -> incorrectArgs ())
      sqlSpec =
        SqlCallback2(fun lookingIn searchingFor ->
          // strpos returns indexed from 1; 0 means missing
          $"strpos({lookingIn}, {searchingFor}) > 0")
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

          let chars = String.toEgcSeq s
          let length = Seq.length chars |> int64

          let normalize (i : int64) =
            i
            |> fun i -> if i < 0L then length + i else i // account for - values
            |> min length
            |> max 0L


          let f = normalize first |> int
          let l = normalize last |> int
          let l = if f > l then f else l // return empty string when start is less than end

          chars
          |> Seq.toList
          |> FSharpPlus.List.drop f
          |> List.truncate (l - f)
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


    { name = fn "fromBytes" 0
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


    { name = fn "indexOf" 0
      typeParams = []
      parameters =
        [ Param.make "str" TString "The string to search in"
          Param.make
            "searchFor"
            TString
            "The string to search for within <param str>" ]
      returnType = TypeReference.option TInt
      description =
        "Returns {{Some index}} of the first occurrence of <param searchFor> in <param str>, or returns {{None}} if <param searchFor> does not occur."
      fn =
        (function
        | _, _, [ DString str; DString search ] ->
          let index = str.IndexOf(search)
          if index = -1 then
            Ply(Dval.optionNone)
          else
            Ply(Dval.optionSome (DInt index))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
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
            Ply(Dval.optionNone)
          else
            let head = String.toEgcSeq str |> Seq.head
            Ply(Dval.optionSome (DChar head))
        | _ -> incorrectArgs ())

      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
