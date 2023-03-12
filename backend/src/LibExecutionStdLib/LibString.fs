module LibExecutionStdLib.LibString

open System.Globalization
open System.Security.Cryptography
open System
open System.Text

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.VendoredTablecloth

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "String" "isEmpty" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TBool
      description = "Returns {{true}} if <param s> is the empty string {{\"\"}}"
      fn =
        (function
        | _, [ DStr s ] -> Ply(DBool(s = ""))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "foreach" 1
      parameters =
        [ Param.make "s" TStr ""
          Param.makeWithArgs "fn" (TFn([ TChar ], TChar)) "" [ "character" ] ]
      returnType = TStr
      description =
        "Iterate over each Character (EGC, not byte) in the string, performing the
         operation in <param fn> on each one."
      fn =
        (function
        | state, [ DStr s; DFnVal b ] ->
          (String.toEgcSeq s
           |> Seq.toList
           |> Ply.List.mapSequentially (fun te ->
             (LibExecution.Interpreter.applyFnVal
               state
               (id 0)
               b
               [ DChar te ]
               NotInPipe))
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
                 return DStr str
             })))

        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "newline" 0
      parameters = []
      returnType = TStr
      description = "Returns a string containing a single '\n'"
      fn =
        (function
        | _, [] -> Ply(DStr "\n")
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "toList" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TList TChar
      description = "Returns the list of Characters (EGC, not byte) in the string"
      fn =
        (function
        | _, [ DStr s ] ->
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
      parameters =
        [ Param.make "s" TStr "The string to operate on"
          Param.make "searchFor" TStr "The string to search for within <param s>"
          Param.make "replaceWith" TStr "" ]
      returnType = TStr
      description =
        "Replace all instances on <param searchFor> in <param s> with <param
         replaceWith>"
      fn =
        (function
        | _, [ DStr s; DStr search; DStr replace ] ->
          if search = "" then
            if s = "" then
              Ply(DStr replace)
            else
              // .Net Replace doesn't allow empty string, but we do.
              String.toEgcSeq s
              |> Seq.toList
              |> FSharpPlus.List.intersperse replace
              |> (fun l -> replace :: l @ [ replace ])
              |> String.concat ""
              |> DStr
              |> Ply
          else
            Ply(DStr(s.Replace(search, replace)))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "replace"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "toUppercase" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Returns the string, uppercased"
      fn =
        (function
        | _, [ DStr s ] -> Ply(DStr(String.toUppercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "upper"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "toLowercase" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Returns the string, lowercased"
      fn =
        (function
        | _, [ DStr s ] -> Ply(DStr(String.toLowercase s))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "lower"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "length" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, [ DStr s ] -> s |> String.lengthInEgcs |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented // there isn't a unicode version of length
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "append" 1
      parameters = [ Param.make "s1" TStr ""; Param.make "s2" TStr "" ]
      returnType = TStr
      description =
        "Concatenates the two strings by appending <param s2> to <param s1> and
         returns the joined string."
      fn =
        (function
        // TODO add fuzzer to ensure all strings are normalized no matter what we do to them.
        | _, [ DStr s1; DStr s2 ] -> (s1 + s2) |> String.normalize |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "prepend" 0
      parameters = [ Param.make "s1" TStr ""; Param.make "s2" TStr "" ]
      returnType = TStr
      description =
        "Concatenates the two strings by prepending <param s2> to <param s1> and
         returns the joined string."
      fn =
        (function
        | _, [ DStr s1; DStr s2 ] -> Ply(DStr(s2 + s1))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "slugify" 2
      parameters = [ Param.make "string" TStr "" ]
      returnType = TStr
      description =
        "Turns a string into a prettified slug, including only lowercased
         alphanumeric characters, joined by hyphens"
      fn =
        (function
        | _, [ DStr s ] ->
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
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "reverse" 0
      parameters = [ Param.make "string" TStr "" ]
      returnType = TStr
      description = "Reverses <param string>"
      fn =
        (function
        | _, [ DStr s ] ->
          String.toEgcSeq s |> Seq.rev |> String.concat "" |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "reverse"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "split" 1
      parameters = [ Param.make "s" TStr ""; Param.make "separator" TStr "" ]
      returnType = TList TStr
      description =
        "Splits a string at the separator, returning a list of strings without the separator.
        If the separator is not present, returns a list containing only the initial string."
      fn =
        (function
        | _, [ DStr s; DStr sep ] ->
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
            s |> String.toEgcSeq |> Seq.toList |> List.map DStr |> DList |> Ply
          else
            ecgStringSplit
              (s |> String.toEgcSeq |> Seq.toList)
              (sep |> String.toEgcSeq |> Seq.toList)
            |> List.map DStr
            |> DList
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "join" 0
      parameters = [ Param.make "l" (TList TStr) ""; Param.make "separator" TStr "" ]
      returnType = TStr
      description = "Combines a list of strings with the provided separator"
      fn =
        (function
        | _, [ DList l; DStr sep ] ->
          let strs =
            List.map
              (fun s ->
                match s with
                | DStr st -> st
                | dv -> Exception.raiseCode (Errors.argumentMemberWasnt TStr "l" dv))
              l

          // CLEANUP: The OCaml doesn't normalize after concat, so we don't either
          Ply(DStr((String.concat sep strs)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "fromList" 1
      parameters = [ Param.make "l" (TList TChar) "" ]
      returnType = TStr
      description = "Returns the list of characters as a string"
      fn =
        (function
        | _, [ DList l ] ->
          DStr(
            l
            |> List.map (fun dval ->
              match dval with
              | DChar c -> c
              | dv -> Exception.raiseCode (Errors.argumentMemberWasnt TChar "l" dv))
            |> String.concat ""
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "fromChar" 1
      parameters = [ Param.make "c" TChar "" ]
      returnType = TStr
      description = "Converts a <type char> to a <type string>"
      fn =
        (function
        | _, [ DChar c ] -> Ply(DStr(c))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "base64Encode" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        "URLBase64 encodes a string without padding. Uses URL-safe encoding with
        {{-}} and {{_}} instead of {{+}} and {{/}}, as defined in
        [RFC 4648 section 5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)"
      fn =
        (function
        | _, [ DStr s ] ->
          let defaultEncoded = s |> UTF8.toBytes |> Convert.ToBase64String
          // Inlined version of Base64.urlEncodeToString, except
          defaultEncoded.Replace('+', '-').Replace('/', '_').Replace("=", "")
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "base64Decode" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Base64 decodes a string. The returned value is wrapped in {{Result}}.
         Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648
         sections](https://www.rfc-editor.org/rfc/rfc4648.html)
         [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, [ DStr s ] ->
          let errPipe e = e |> DStr |> Error |> DResult |> Ply
          let okPipe r = r |> DStr |> Ok |> DResult |> Ply
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
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        "Take a string and hash it to a cryptographically-secure digest.
         Don't rely on either the size or the algorithm."
      fn =
        (function
        | _, [ DStr s ] ->
          let sha384Hash = SHA384.Create()
          let data = System.Text.Encoding.UTF8.GetBytes(s)

          let bytes = sha384Hash.ComputeHash(data)

          // Deliberately keep padding
          System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "random" 2
      parameters = [ Param.make "length" TInt "" ]
      returnType = TResult(TStr, TStr)
      description =
        "Generate a <type string> of length <param length> from random characters"
      fn =
        (function
        | _, [ DInt l as dv ] ->
          if l < 0L then
            dv
            |> Errors.argumentWasnt "positive" "length"
            |> DStr
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

            randomString (int l) |> DStr |> Ok |> DResult |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "String" "htmlEscape" 0
      parameters = [ Param.make "html" TStr "" ]
      returnType = TStr
      description =
        "Escape an untrusted string in order to include it safely in HTML output"
      fn =
        (function
        | _, [ DStr s ] ->
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

          Ply(DStr(htmlEscape s))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      // CLEANUP mark as Pure
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "String" "contains" 0
      parameters =
        [ Param.make "lookingIn" TStr ""; Param.make "searchingFor" TStr "" ]
      returnType = TBool
      description = "Checks if <param lookingIn> contains <param searchingFor>"
      fn =
        (function
        | _, [ DStr haystack; DStr needle ] -> Ply(DBool(haystack.Contains needle))
        | _ -> incorrectArgs ())
      sqlSpec =
        SqlCallback2 (fun lookingIn searchingFor ->
          // strpos returns indexed from 1; 0 means missing
          $"strpos({lookingIn}, {searchingFor}) > 0")
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "slice" 0
      parameters =
        [ Param.make "string" TStr ""
          Param.make "from" TInt ""
          Param.make "to" TInt "" ]
      returnType = TStr
      description =
        "Returns the substring of <param string> between the <param from> and <param
         to> indices.

         Negative indices start counting from the end of <param string>."
      fn =
        (function
        | _, [ DStr s; DInt first; DInt last ] ->

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
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "first" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns the first <param characterCount> characters of <param string>, as a
         String.

         If <param characterCount> is longer than <param string>, returns <param
         string>.

         If <param characterCount> is negative, returns the empty string."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
          let n = String.lengthInEgcs s |> int64 |> min n |> max 0L |> int

          String.toEgcSeq s
          |> Seq.toList
          |> List.truncate n
          |> String.concat ""
          |> String.normalize
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "last" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns the last <param characterCount> characters of <param string>, as a
         String.

         If <param characterCount> is longer than <param string>, returns <param
         string>.

         If <param characterCount> is negative, returns the empty string."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
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

          Ply(DStr(lastN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "dropLast" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns all but the last <param characterCount> characters of <param
         string>, as a String.

         If <param characterCount> is longer than <param string>, returns the empty string.

         If <param characterCount> is negative, returns <param string>."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
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

          Ply(DStr(dropLastN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "dropFirst" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns all but the first <param characterCount> characters of <param
         string>, as a <type String>.

         If <param characterCount> is longer than <param string>, returns the empty
         string.

         If <param characterCount> is negative, returns <param string>."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
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

          Ply(DStr(dropFirstN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "padStart" 0
      parameters =
        [ Param.make "string" TStr ""
          Param.make "padWith" TStr ""
          Param.make "goalLength" TInt "" ]
      returnType = TResult(TStr, TStr)
      description =
        "If <param string> is shorter than <param goalLength> characters, returns a
         copy of <param string> starting with enough copies of <param padWith> for the
         result have <param goalLength>. A returning value is wrapped in a {{Result}}.

         If the <param string> is longer than <param goalLength>, returns an unchanged copy of <param string>"
      fn =
        (function
        | _, [ DStr s; DStr padWith as dv; DInt l ] ->
          let errPipe e = e |> DStr |> Error |> DResult |> Ply
          let okPipe r = r |> DStr |> Ok |> DResult |> Ply
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
      parameters =
        [ Param.make "string" TStr ""
          Param.make "padWith" TStr ""
          Param.make "goalLength" TInt "" ]
      returnType = TResult(TStr, TStr)
      description =
        "If <param string> is shorter than <param goalLength> characters, returns a
         copy of <param string> ending with enough copies of <param padWith> for the
         result have <param goalLength>. A returning value is wrapped in a {{Result}}.

         If the <param string> is longer than <param goalLength>, returns an unchanged copy of
         <param string>."
      fn =
        (function
        | _, [ DStr s; DStr padWith as dv; DInt l ] ->
          let errPipe e = e |> DStr |> Error |> DResult |> Ply
          let okPipe r = r |> DStr |> Ok |> DResult |> Ply
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
      parameters = [ Param.make "str" TStr "" ]
      returnType = TStr
      description =
        "Returns a copy of <param str> with all leading and trailing whitespace
         removed. 'whitespace' here means all Unicode characters with the
         {{White_Space}} property, which includes {{\" \"}}, {{\"\\t\"}} and
         {{\"\\n\"}}"
      fn =
        (function
        | _, [ DStr toTrim ] -> toTrim |> String.trim |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "trim"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "trimStart" 0
      parameters = [ Param.make "str" TStr "" ]
      returnType = TStr
      description =
        "Returns a copy of <param str> with all leading whitespace removed. 'whitespace'
         here means all Unicode characters with the {{White_Space}} property, which
         includes {{\" \"}}, {{\"\\t\"}} and {{\"\\n\"}}"
      fn =
        (function
        | _, [ DStr toTrim ] -> Ply(DStr(toTrim.TrimStart()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "ltrim"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "trimEnd" 0
      parameters = [ Param.make "str" TStr "" ]
      returnType = TStr
      description =
        "Returns a copy of <param str> with all trailing whitespace removed.
         'whitespace' here means all Unicode characters with the {{White_Space}}
         property, which includes {{\" \"}}, {{\"\\t\"}} and {{\"\\n\"}}."
      fn =
        (function
        | _, [ DStr toTrim ] -> Ply(DStr(toTrim.TrimEnd()))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "rtrim"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "toBytes" 0
      parameters = [ Param.make "str" TStr "" ]
      returnType = TBytes
      description =
        "Converts the given unicode string to a UTF8-encoded byte sequence."
      fn =
        (function
        | _, [ DStr str ] ->
          let theBytes = System.Text.Encoding.UTF8.GetBytes str
          Ply(DBytes theBytes)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "startsWith" 0
      parameters = [ Param.make "subject" TStr ""; Param.make "prefix" TStr "" ]
      returnType = TBool
      description = "Checks if <param subject> starts with <param prefix>"
      fn =
        (function
        | _, [ DStr subject; DStr prefix ] ->
          Ply(DBool(subject.StartsWith(prefix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "endsWith" 0
      parameters =
        [ Param.make "subject" TStr "String to test"; Param.make "suffix" TStr "" ]
      returnType = TBool
      description = "Checks if <param subject> ends with <param suffix>"
      fn =
        (function
        | _, [ DStr subject; DStr suffix ] ->
          Ply(DBool(subject.EndsWith(suffix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
