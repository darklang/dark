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

module DvalReprExternal = LibExecution.DvalReprExternal
module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs


let fns : List<BuiltInFn> =
  [ { name = fn "String" "isEmpty" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TBool
      description = "Returns `true` if `s` is the empty string \"\"."
      fn =
        (function
        | _, [ DStr s ] -> Ply(DBool(s = ""))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "foreach" 0
      parameters =
        [ Param.make "s" TStr "string to iterate over"
          Param.makeWithArgs
            "fn"
            (TFn([ TChar ], TChar))
            "function used to convert one character to another"
            [ "char" ] ]
      returnType = TStr
      description =
        "Iterate over each character (byte, not EGC) in the string, performing the operation in the block on each one"
      fn =
        function
        | state, [ s; f ] -> Errors.removedFunction state "String::foreach"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "foreach" 1) }


    { name = fn "String" "foreach" 1
      parameters =
        [ Param.make "s" TStr ""
          Param.makeWithArgs "fn" (TFn([ TChar ], TChar)) "" [ "character" ] ]
      returnType = TStr
      description =
        "Iterate over each Character (EGC, not byte) in the string, performing the operation in the block on each one."
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
               NotInPipe
               NoRail))
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "toList" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TList TChar
      description = "Returns the list of characters (byte, not EGC) in the string"
      fn =
        function
        | state, [ s ] -> Errors.removedFunction state "String::toList"
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toList" 1) }


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
      description = "Replace all instances on `searchFor` in `s` with `replaceWith`"
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


    { name = fn "String" "toInt" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TInt
      description = "Returns the int value of the string"
      fn =
        (function
        | _, [ DStr s ] ->
          (try
            let int = s |> System.Convert.ToInt64

            // These constants represent how high the old OCaml parsers would go
            if int < -4611686018427387904L then
              Exception.raiseInternal "goto exception case" []
            else if int >= 4611686018427387904L then
              Exception.raiseInternal "goto exception case" []
            else
              int |> DInt |> Ply
           with
           | e -> err (Errors.argumentWasnt "numeric" "s" (DStr s)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toInt" 1) }


    { name = fn "String" "toInt" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TInt, TStr)
      description =
        "Returns the int value of the string, wrapped in a `Ok`, or `Error <msg>` if the string contains characters other than numeric digits"
      fn =
        (function
        | _, [ DStr s ] ->
          try
            let int = s |> System.Convert.ToInt64

            // These constants represent how high the old OCaml parsers would go
            if int < -4611686018427387904L then
              Exception.raiseInternal "goto exception case" []
            else if int >= 4611686018427387904L then
              Exception.raiseInternal "goto exception case" []
            else
              int |> DInt |> Ok |> DResult |> Ply
          with
          | e ->
            $"Expected to parse string with only numbers, instead got \"{s}\""
            |> DStr
            |> Error
            |> DResult
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Int" "parse" 0) }


    { name = fn "String" "toFloat" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TFloat
      description = "Returns the float value of the string"
      fn =
        (function
        | _, [ DStr s as dv ] ->
          (try
            float (s) |> DFloat |> Ply
           with
           | e ->
             err (
               Errors.argumentWasnt "a string representation of an IEEE float" "s" dv
             ))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toFloat" 1) }


    { name = fn "String" "toFloat" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TFloat, TStr)
      description = "Returns the float value of the string"
      fn =
        (function
        | _, [ DStr s ] ->
          (try
            float (s) |> DFloat |> Ok |> DResult |> Ply
           with
           | e ->
             "Expected a string representation of an IEEE float"
             |> DStr
             |> Error
             |> DResult
             |> Ply)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "toUppercase" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        // CLEANUP "Returns the string, uppercased (only ASCII characters are uppercased)"
        "Returns the string, uppercased"
      fn =
        (function
        | _, [ DStr s ] ->
          s
          |> String.toArray
          |> Array.map (fun c -> if int c < 128 then Char.ToUpper c else c)
          |> System.String
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "upper"
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toUppercase" 1) }


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


    { name = fn "String" "toLowercase" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        // CLEANUP "Returns the string, lowercased (only ASCII characters are lowercased)"
        "Returns the string, lowercased"
      fn =
        (function
        | _, [ DStr s ] ->
          s
          |> String.toArray
          |> Array.map (fun c -> if int c < 128 then Char.ToLower c else c)
          |> System.String
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "lower"
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toLowercase" 1) }


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


    { name = fn "String" "length" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, [ DStr s ] ->
          s |> System.Text.ASCIIEncoding.UTF8.GetByteCount |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "length"
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "length" 1) }


    { name = fn "String" "length" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, [ DStr s ] -> s |> String.lengthInEgcs |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO // there isn't a unicode version of length
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "append" 0
      (* This used to provide "++" as an infix op.
       * It was moved to [String::append_v1] instead,
       * because we do not yet support versioning infix operators.
       * We decided this was safe under the assumption that no one should be
       * (and very likely no one is) relying on broken normalization. *)
      parameters = [ Param.make "s1" TStr ""; Param.make "s2" TStr "" ]
      returnType = TStr
      description = "Concatenates the two strings and returns the joined string"
      fn =
        (function
        | _, [ DStr s1; DStr s2 ] ->
          // This implementation does not normalize post-concatenation.
          // This is a problem because it breaks our guarantees about strings always being normalized;
          // concatenating two normalized strings does not always result in a normalized string.
          // replicating known broken behaviour feels wrong, but maybe necessary
          Ply(
            DStr(
              System.Text.Encoding.UTF8.GetString(
                Array.append
                  (System.Text.Encoding.UTF8.GetBytes s1)
                  (System.Text.Encoding.UTF8.GetBytes s2)
              )
            )
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "append" 1) }


    { name = fn "String" "append" 1
      parameters = [ Param.make "s1" TStr ""; Param.make "s2" TStr "" ]
      returnType = TStr
      description =
        "Concatenates the two strings by appending `s2` to `s1` and returns the joined string."
      fn =
        (function
        // TODO add fuzzer to ensure all strings are normalized no matter what we do to them.
        | _, [ DStr s1; DStr s2 ] -> (s1 + s2) |> String.normalize |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "prepend" 0
      parameters = [ Param.make "s1" TStr ""; Param.make "s2" TStr "" ]
      returnType = TStr
      description =
        "Concatenates the two strings by prepending `s2` to `s1` and returns the joined string."
      fn =
        (function
        | _, [ DStr s1; DStr s2 ] -> Ply(DStr(s2 + s1))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "slugify" 0
      parameters = [ Param.make "string" TStr "" ]
      returnType = TStr
      description = "Turns a string into a slug"
      fn =
        (function
        | _, [ DStr s ] ->
          let toRemove = "[^-a-zA-Z0-9\\s\\$\\*_+~\\.\\(\\)'\"!:@]|\x0b"
          let trim = @"^\s+|\s+$"
          let spaces = @"[-\s]+"

          let replace (pattern : string) (replacement : string) (input : string) =
            Regex.Replace(input, pattern, replacement)

          s
          |> replace toRemove ""
          |> replace trim ""
          |> replace spaces "-"
          |> String.toLowercase
          |> DStr
          |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "slugify" 1) }


    { name = fn "String" "slugify" 1
      parameters = [ Param.make "string" TStr "" ]
      returnType = TStr
      description = "Turns a string into a slug"
      fn =
        (function
        | _, [ DStr s ] ->
          let toRemove = "[^a-zA-Z0-9\\s_-]|\x0b"
          let trim = @"^\s+|\s+$"
          let newSpaces = @"[-_\s]+"

          let replace (pattern : string) (replacement : string) (input : string) =
            Regex.Replace(input, pattern, replacement)

          s
          |> replace toRemove ""
          |> replace trim ""
          |> replace newSpaces "-"
          |> String.toLowercase
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "slugify" 2) }


    { name = fn "String" "slugify" 2
      parameters = [ Param.make "string" TStr "" ]
      returnType = TStr
      description =
        "Turns a string into a prettified slug, including only lowercased alphanumeric characters, joined by hyphens"
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "reverse" 0
      parameters = [ Param.make "string" TStr "" ]
      returnType = TStr
      description = "Reverses `string`"
      fn =
        (function
        | _, [ DStr s ] ->
          String.toEgcSeq s |> Seq.rev |> String.concat "" |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "reverse"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "split" 0
      parameters = [ Param.make "s" TStr ""; Param.make "separator" TStr "" ]
      returnType = TList TStr
      description =
        "Splits a string at the separator, returning a list of strings without the separator. If the separator is not present, returns a list containing only the initial string."
      fn =
        (function
        | _, [ DStr s; DStr sep ] ->
          if sep = "" then
            s |> String.toEgcSeq |> Seq.toList |> List.map DStr |> DList |> Ply
          else
            // CLEANUP: we need a new version of this fn.
            // This behaviour is the worst. This mimics what OCaml did: There
            // should be (n-1) empty strings returned for each sequence of n
            // strings matching the separator (eg: split "aaaa" "a" = ["",
            // "", ""]). However, the .NET string split puts in n-1 empty
            // strings correctly everywhere except at the start and end,
            // where there are n empty strings instead.
            let stripStartingEmptyString =
              (function
              | "" :: rest -> rest
              | all -> all)

            s.Split sep
            |> Array.toList
            |> stripStartingEmptyString
            |> List.rev
            |> stripStartingEmptyString
            |> List.rev
            |> List.map DStr
            |> DList
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "fromList" 0
      parameters = [ Param.make "l" (TList TChar) "" ]
      returnType = TStr
      description = "Returns the list of characters as a string"
      fn =
        function
        | state, [ l ] -> Errors.removedFunction state "String::fromList"
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "fromList" 1) }


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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "fromChar" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description = "Converts a char to a string"
      fn =
        function
        | state, [ c ] -> Errors.removedFunction state "String::fromChar"
        | _ -> incorrectArgs ()
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "fromChar" 1) }


    { name = fn "String" "fromChar" 1
      parameters = [ Param.make "c" TChar "" ]
      returnType = TStr
      description = "Converts a char to a string"
      fn =
        (function
        | _, [ DChar c ] -> Ply(DStr(c))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "base64Encode" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        "URLBase64 encodes a string without padding. Uses URL-safe encoding with `-` and `_` instead of `+` and `/`, as defined in RFC 4648 section 5."
      fn =
        (function
        | _, [ DStr s ] ->
          let defaultEncoded = s |> UTF8.toBytes |> Convert.ToBase64String
          // Inlined version of Base64.urlEncodeToString, except
          defaultEncoded.Replace('+', '-').Replace('/', '_').Replace("=", "")
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "base64Decode" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64 alphabets defined in RFC 4648 sections 4 and 5."
      fn =
        (function
        | _, [ DStr s ] ->
          // CLEANUP this should be a result
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            // This seems like we should allow it
            Ply(DStr "")
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            err "Not a valid base64 string"
          else
            try
              s
              |> base64FromUrlEncoded
              |> Convert.FromBase64String
              |> System.Text.Encoding.UTF8.GetString
              |> String.normalize
              |> DStr
              |> Ply
            with
            | e -> err "Not a valid base64 string"
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "sha384" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        "Take a string and hash it using SHA384. Please use Crypto::sha384 instead."
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Crypto" "sha384" 0) }


    { name = fn "String" "sha256" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        "Take a string and hash it using SHA256. Please use Crypto::sha256 instead."
      fn =
        (function
        | _, [ DStr s ] ->
          let sha256Hash = SHA256.Create()
          let data = System.Text.Encoding.UTF8.GetBytes(s)

          let bytes = sha256Hash.ComputeHash(data)

          // Deliberately keep padding
          System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Crypto" "sha256" 0) }


    { name = fn "String" "random" 0
      parameters = [ Param.make "length" TInt "" ]
      returnType = TStr
      description = "Generate a string of length `length` from random characters."
      fn =
        (function
        | _, [ DInt l as dv ] ->
          if l < 0L then
            err "l should be a positive integer"
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

            randomString (int l) |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "String" "random" 1) }


    { name = fn "String" "random" 1
      parameters = [ Param.make "length" TInt "" ]
      returnType = TResult(TStr, TStr)
      description = "Generate a string of length `length` from random characters."
      fn =
        (function
        | _, [ DInt l ] ->
          if l < 0L then
            "l should be a positive integer" |> DStr |> Error |> DResult |> Ply
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "String" "random" 1) }


    { name = fn "String" "random" 2
      parameters = [ Param.make "length" TInt "" ]
      returnType = TResult(TStr, TStr)
      description = "Generate a string of length `length` from random characters."
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
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "String" "htmlEscape" 0
      parameters = [ Param.make "html" TStr "" ]
      returnType = TStr
      description =
        "Escape an untrusted string in order to include it safely in HTML output."
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
      sqlSpec = NotYetImplementedTODO
      // CLEANUP mark as Pure
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "String" "toUUID" 0
      parameters = [ Param.make "uuid" TStr "" ]
      returnType = TUuid
      description =
        "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
      fn =
        (function
        | _, [ DStr s ] ->
          match Guid.TryParse s with
          | true, x -> x |> DUuid |> Ply
          | _ ->
            err (
              "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
            )

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toUUID" 1) }


    { name = fn "String" "toUUID" 1
      parameters = [ Param.make "uuid" TStr "" ]
      returnType = TResult(TUuid, TStr)
      description =
        "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
      fn =
        (function
        | _, [ DStr s ] ->
          match Guid.TryParse s with
          | true, x -> x |> DUuid |> Ok |> DResult |> Ply
          | _ ->
            "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
            |> DStr
            |> Error
            |> DResult
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "isSubstring" 0
      parameters =
        [ Param.make "searchingFor" TStr ""; Param.make "lookingIn" TStr "" ]
      returnType = TBool
      description = "Checks if `lookingIn` contains `searchingFor`"
      fn =
        (function
        | _, [ DStr needle; DStr haystack ] -> DBool(haystack.Contains needle) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "isSubstring" 1) }


    { name = fn "String" "isSubstring" 1
      parameters =
        [ Param.make "lookingIn" TStr ""; Param.make "searchingFor" TStr "" ]
      returnType = TBool
      description = "Checks if `lookingIn` contains `searchingFor`"
      fn =
        (function
        | _, [ DStr haystack; DStr needle ] -> DBool(haystack.Contains needle) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec =
        SqlCallback2 (fun lookingIn searchingFor ->
          // strpos returns indexed from 1; 0 means missing
          $"(strpos({lookingIn}, {searchingFor}) > 0)")
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "contains" 0) }


    { name = fn "String" "contains" 0
      parameters =
        [ Param.make "lookingIn" TStr ""; Param.make "searchingFor" TStr "" ]
      returnType = TBool
      description = "Checks if `lookingIn` contains `searchingFor`"
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
        "Returns the substring of `string` between the `from` and `to` indices.
       Negative indices start counting from the end of `string`.
       Indices represent characters."
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "first" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns the first `characterCount` characters of `string`, as a String.
      If `characterCount` is longer than `string`, returns `string`.
      If `characterCount` is negative, returns the empty string."
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "last" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns the last `characterCount` characters of `string`, as a String.
      If `characterCount` is longer than `string`, returns `string`.
      If `characterCount` is negative, returns the empty string."
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "dropLast" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns all but the last `characterCount` characters of `string`, as a String.
      If `characterCount` is longer than `string`, returns the empty string.
      If `characterCount` is negative, returns `string`."
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "dropFirst" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description =
        "Returns all but the first `characterCount` characters of `string`, as a String.
        If `characterCount` is longer than `string`, returns the empty string.
        If `characterCount` is negative, returns `string`."
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
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "padStart" 0
      parameters =
        [ Param.make "string" TStr ""
          Param.make "padWith" TStr ""
          Param.make "goalLength" TInt "" ]
      returnType = TStr
      description =
        "If `string` is shorter than `goalLength` characters, returns a copy of `string` starting with enough copies of `padWith` for the result have `goalLength`.
      If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
      fn =
        (function
        | _, [ DStr s; DStr padWith as dv; DInt l ] ->
          if String.lengthInEgcs padWith <> 1 then
            err (Errors.argumentWasnt "1 character long" "padWith" dv)
          else
            let targetLength = int l
            let currentLength = String.lengthInEgcs s
            let requiredPads = max 0 (targetLength - currentLength)

            let stringBuilder = new StringBuilder()

            for _ = 1 to requiredPads do
              stringBuilder.Append(padWith) |> ignore<StringBuilder>

            stringBuilder.Append(s) |> ignore<StringBuilder>

            stringBuilder |> string |> String.normalize |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "padEnd" 0
      parameters =
        [ Param.make "string" TStr ""
          Param.make "padWith" TStr ""
          Param.make "goalLength" TInt "" ]
      returnType = TStr
      description =
        "If `string` is shorter than `goalLength` characters, returns a copy of `string` ending with enough copies of `padWith` for the result have `goalLength`.
      If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
      fn =
        (function
        | _, [ DStr s; DStr padWith as dv; DInt l ] ->
          if String.lengthInEgcs padWith <> 1 then
            err (Errors.argumentWasnt "1 character long" "padWith" dv)
          else
            let targetLength = int l
            let currentLength = String.lengthInEgcs s
            let requiredPads = max 0 (targetLength - currentLength)

            let stringBuilder = new StringBuilder()
            stringBuilder.Append(s) |> ignore<StringBuilder>

            for _ = 1 to requiredPads do
              stringBuilder.Append(padWith) |> ignore<StringBuilder>

            stringBuilder |> string |> String.normalize |> DStr |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "trim" 0
      parameters = [ Param.make "str" TStr "" ]
      returnType = TStr
      description =
        "Returns a copy of `str` with all leading and trailing whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
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
        "Returns a copy of `str` with all leading whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
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
        "Returns a copy of `str` with all trailing whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
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
        "Converts the given unicode string to a utf8-encoded byte sequence."
      fn =
        (function
        | _, [ DStr str ] ->
          let theBytes = System.Text.Encoding.UTF8.GetBytes str
          Ply(DBytes theBytes)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "startsWith" 0
      parameters = [ Param.make "subject" TStr ""; Param.make "prefix" TStr "" ]
      returnType = TBool
      description = "Checks if `subject` starts with `prefix`"
      fn =
        (function
        | _, [ DStr subject; DStr prefix ] ->
          Ply(DBool(subject.StartsWith(prefix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "String" "endsWith" 0
      parameters =
        [ Param.make "subject" TStr "String to test"; Param.make "suffix" TStr "" ]
      returnType = TBool
      description = "Checks if `subject` ends with `suffix`"
      fn =
        (function
        | _, [ DStr subject; DStr suffix ] ->
          Ply(DBool(subject.EndsWith(suffix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
