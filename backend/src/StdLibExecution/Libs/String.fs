module StdLibExecution.Libs.String

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Globalization
open System.Security.Cryptography
open System
open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth
open LibExecution.StdLib.Shortcuts

module Errors = LibExecution.Errors

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "String" "isEmpty" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TBool
      description = "Returns {{true}} if <param s> is the empty string {{\"\"}}"
      fn =
        (function
        | _, _, [ DString s ] -> Ply(DBool(s = ""))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "foreach" 1
      typeParams = []
      parameters =
        [ Param.make "s" TString ""
          Param.makeWithArgs "fn" (TFn([ TChar ], TChar)) "" [ "character" ] ]
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
             LibExecution.Interpreter.applyFnVal state b [ DChar te ])
           |> (fun dvals ->
             (uply {
               let! (dvals : List<Dval>) = dvals

               match List.tryFind (fun dv -> Dval.isIncomplete dv) dvals with
               | Some i -> return i
               | None ->
                 let chars =
                   List.map
                     (function
                     | DChar c -> c
                     | dv ->
                       Exception.raiseCode (Errors.expectedLambdaType "fn" TChar dv))
                     dvals

                 let str = String.concat "" chars
                 return DString str
             })))

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "newline" 0
      typeParams = []
      parameters = []
      returnType = TString
      description = "Returns a string containing a single '\n'"
      fn =
        (function
        | _, _, [] -> Ply(DString "\n")
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "toList" 1
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


    { name = fn "String" "replaceAll" 0
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


    { name = fn "String" "toUppercase" 1
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


    { name = fn "String" "toLowercase" 1
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


    { name = fn "String" "length" 1
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


    { name = fn "String" "append" 1
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


    { name = fn "String" "prepend" 0
      typeParams = []
      parameters = [ Param.make "s1" TString ""; Param.make "s2" TString "" ]
      returnType = TString
      description =
        "Concatenates the two strings by prepending <param s2> to <param s1> and
         returns the joined string."
      fn =
        (function
        | _, _, [ DString s1; DString s2 ] -> Ply(DString(s2 + s1))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // CLEANUP move to stdlib
    { name = fn "String" "slugify" 2
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


    { name = fn "String" "reverse" 0
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


    { name = fn "String" "split" 1
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


    { name = fn "String" "join" 0
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
                  Exception.raiseCode (
                    Errors.argumentWasntType (TList TString) "l" dv
                  ))
              l

          Ply(DString((String.concat sep strs).Normalize()))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "fromList" 1
      typeParams = []
      parameters = [ Param.make "l" (TList TChar) "" ]
      returnType = TString
      description = "Returns the list of characters as a string"
      fn =
        (function
        | _, _, [ DList l ] ->
          DString(
            l
            |> List.map (fun dval ->
              match dval with
              | DChar c -> c
              | dv ->
                Exception.raiseCode (Errors.argumentWasntType (TList TChar) "l" dv))
            |> String.concat ""
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "fromChar" 1
      typeParams = []
      parameters = [ Param.make "c" TChar "" ]
      returnType = TString
      description = "Converts a <type Char> to a <type String>"
      fn =
        (function
        | _, _, [ DChar c ] -> Ply(DString(c))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "base64Encode" 0
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


    { name = fn "String" "base64Decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TResult(TString, TString)
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
          let errPipe e = e |> DString |> Error |> DResult |> Ply
          let okPipe r = r |> DString |> Ok |> DResult |> Ply
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
            with
            | e -> "Not a valid base64 string" |> errPipe
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      // CLEANUP: this shouldnt return a string and should be deprecated
      deprecated = NotDeprecated }


    { name = fn "String" "digest" 0
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


    { name = fn "String" "random" 2
      typeParams = []
      parameters = [ Param.make "length" TInt "" ]
      returnType = TResult(TString, TString)
      description =
        "Generate a <type String> of length <param length> from random characters"
      fn =
        (function
        | _, _, [ DInt l as dv ] ->
          if l < 0L then
            dv
            |> Errors.argumentWasnt "positive" "length"
            |> DString
            |> Error
            |> DResult
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

            randomString (int l) |> DString |> Ok |> DResult |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "String" "htmlEscape" 0
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


    { name = fn "String" "contains" 0
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
        SqlCallback2 (fun lookingIn searchingFor ->
          // strpos returns indexed from 1; 0 means missing
          $"strpos({lookingIn}, {searchingFor}) > 0")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "slice" 0
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


    { name = fn "String" "first" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""; Param.make "characterCount" TInt "" ]
      returnType = TString
      description =
        "Returns the first <param characterCount> characters of <param string>, as a
         String.

         If <param characterCount> is longer than <param string>, returns <param
         string>.

         If <param characterCount> is negative, returns the empty string."
      fn =
        (function
        | _, _, [ DString s; DInt n ] ->
          let n = String.lengthInEgcs s |> int64 |> min n |> max 0L |> int

          String.toEgcSeq s
          |> Seq.toList
          |> List.truncate n
          |> String.concat ""
          |> String.normalize
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "last" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""; Param.make "characterCount" TInt "" ]
      returnType = TString
      description =
        "Returns the last <param characterCount> characters of <param string>, as a
         String.

         If <param characterCount> is longer than <param string>, returns <param
         string>.

         If <param characterCount> is negative, returns the empty string."
      fn =
        (function
        | _, _, [ DString s; DInt n ] ->
          let egcSeq = String.toEgcSeq s
          let stringEgcCount = Seq.length egcSeq

          let lastN s (numEgcs : int64) =
            let stringBuilder = new StringBuilder(String.length s) in
            (* We iterate through every EGC, adding it to the buffer
                * if its [idx] >= ([stringEgcCount] - [numEgcs]).
                * Consider if the string is "abcde" and [numEgcs] = 2,
                * [stringEgcCount] = 5; 5-2 = 3. The index of "d" is 3 and
                * we want to keep it and everything after it so we end up with "de". *)

            let startIdx = int64 stringEgcCount - numEgcs in

            let lastFunc (idx : int64) (seg : string) =
              if idx >= startIdx then
                stringBuilder.Append seg |> ignore<StringBuilder>
              else
                ()

              1L + idx

            ignore<List<int64>> (
              egcSeq
              |> Seq.toList
              |> List.mapi (fun index value -> (lastFunc (int64 index) value))
            )
            (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
            stringBuilder.ToString()

          Ply(DString(lastN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "dropLast" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""; Param.make "characterCount" TInt "" ]
      returnType = TString
      description =
        "Returns all but the last <param characterCount> characters of <param
         string>, as a String.

         If <param characterCount> is longer than <param string>, returns the empty string.

         If <param characterCount> is negative, returns <param string>."
      fn =
        (function
        | _, _, [ DString s; DInt n ] ->
          let egcSeq = String.toEgcSeq s
          let stringEgcCount = Seq.length egcSeq

          let dropLastN s (numEgcs : int64) =
            let stringBuilder = new StringBuilder(String.length s) in
            // We iterate through every EGC, adding it to the buffer
            // if its [idx] < ([stringEgcCount] - [numEgcs]).
            // This works by the inverse of the logic for [lastN].

            let startIdx = int64 stringEgcCount - numEgcs in

            let lastFunc (idx : int64) (seg : string) =
              if idx < startIdx then
                stringBuilder.Append seg |> ignore<StringBuilder>
              else
                ()

              1L + idx

            ignore<List<int64>> (
              egcSeq
              |> Seq.toList
              |> List.mapi (fun index value -> (lastFunc (int64 index) value))
            )
            (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
            stringBuilder.ToString()

          Ply(DString(dropLastN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "dropFirst" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""; Param.make "characterCount" TInt "" ]
      returnType = TString
      description =
        "Returns all but the first <param characterCount> characters of <param
         string>, as a <type String>.

         If <param characterCount> is longer than <param string>, returns the empty
         string.

         If <param characterCount> is negative, returns <param string>."
      fn =
        (function
        | _, _, [ DString s; DInt n ] ->
          let dropFirstN s (numEgcs : int64) =
            let stringBuilder = new StringBuilder(String.length s) in
            // We iterate through every EGC, adding it to the buffer
            // if its index >= numEgcs. This works by the inverse of the logic for [first_n]:
            let firstFunc (idx : int64) (seg : string) =
              if idx >= numEgcs then
                stringBuilder.Append seg |> ignore<StringBuilder>
              else
                ()

              1L + idx

            ignore<List<int64>> (
              String.toEgcSeq s
              |> Seq.toList
              |> List.mapi (fun index value -> (firstFunc (int64 index) value))
            )
            // We don't need to renormalize because all normalization forms are closed
            // under substringing (see https://unicode.org/reports/tr15/#Concatenation).
            stringBuilder.ToString()

          Ply(DString(dropFirstN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "padStart" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""
          Param.make "padWith" TString ""
          Param.make "goalLength" TInt "" ]
      returnType = TResult(TString, TString)
      description =
        "If <param string> is shorter than <param goalLength> characters, returns a
         copy of <param string> starting with enough copies of <param padWith> for the
         result have <param goalLength>. A returning value is wrapped in a {{Result}}.

         If the <param string> is longer than <param goalLength>, returns an unchanged copy of <param string>"
      fn =
        (function
        | _, _, [ DString s; DString padWith as dv; DInt l ] ->
          let errPipe e = e |> DString |> Error |> DResult |> Ply
          let okPipe r = r |> DString |> Ok |> DResult |> Ply
          if String.lengthInEgcs padWith <> 1 then
            Errors.argumentWasnt "1 character long" "padWith" dv |> errPipe
          else
            let targetLength = int l
            let currentLength = String.lengthInEgcs s
            let requiredPads = max 0 (targetLength - currentLength)

            let stringBuilder = new StringBuilder()

            for _ = 1 to requiredPads do
              stringBuilder.Append(padWith) |> ignore<StringBuilder>

            stringBuilder.Append(s) |> ignore<StringBuilder>

            stringBuilder |> string |> String.normalize |> okPipe
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "padEnd" 0
      typeParams = []
      parameters =
        [ Param.make "string" TString ""
          Param.make "padWith" TString ""
          Param.make "goalLength" TInt "" ]
      returnType = TResult(TString, TString)
      description =
        "If <param string> is shorter than <param goalLength> characters, returns a
         copy of <param string> ending with enough copies of <param padWith> for the
         result have <param goalLength>. A returning value is wrapped in a {{Result}}.

         If the <param string> is longer than <param goalLength>, returns an unchanged copy of
         <param string>."
      fn =
        (function
        | _, _, [ DString s; DString padWith as dv; DInt l ] ->
          let errPipe e = e |> DString |> Error |> DResult |> Ply
          let okPipe r = r |> DString |> Ok |> DResult |> Ply
          if String.lengthInEgcs padWith <> 1 then
            Errors.argumentWasnt "1 character long" "padWith" dv |> errPipe
          else
            let targetLength = int l
            let currentLength = String.lengthInEgcs s
            let requiredPads = max 0 (targetLength - currentLength)

            let stringBuilder = new StringBuilder()
            stringBuilder.Append(s) |> ignore<StringBuilder>

            for _ = 1 to requiredPads do
              stringBuilder.Append(padWith) |> ignore<StringBuilder>

            stringBuilder |> string |> String.normalize |> okPipe

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "trim" 0
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
        | _, _, [ DString toTrim ] -> toTrim |> String.trim |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "trim"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "trimStart" 0
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


    { name = fn "String" "trimEnd" 0
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


    { name = fn "String" "toBytes" 0
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


    { name = fn "String" "fromBytes" 0
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


    { name = fn "String" "startsWith" 0
      typeParams = []
      parameters =
        [ Param.make "subject" TString ""; Param.make "prefix" TString "" ]
      returnType = TBool
      description = "Checks if <param subject> starts with <param prefix>"
      fn =
        (function
        | _, _, [ DString subject; DString prefix ] ->
          Ply(DBool(subject.StartsWith(prefix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "endsWith" 0
      typeParams = []
      parameters =
        [ Param.make "subject" TString "String to test"
          Param.make "suffix" TString "" ]
      returnType = TBool
      description = "Checks if <param subject> ends with <param suffix>"
      fn =
        (function
        | _, _, [ DString subject; DString suffix ] ->
          Ply(DBool(subject.EndsWith(suffix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
