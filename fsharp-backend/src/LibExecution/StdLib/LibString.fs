module LibExecution.StdLib.LibString

open System.Globalization
open System.Security.Cryptography
open System
open System.Text

open System.Threading.Tasks
open FSharp.Control.Tasks

open FSharpPlus
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr
module Errors = LibExecution.Errors

(* type coerces one list to another using a function *)

// let list_coerce (f: Dval -> Option<'a>) (l: List<Dval>): Result<List<'a>, List<Dval> * Dval> =
//   l
//   |> List.map (fun dv ->
//        match f dv with
//        | Some v -> Result.Ok v
//        | None -> Result.Error(l, dv))
//   |> Result.all

// let error_result msg =
//   DResult(ResError(DStr msg))
let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "String" "isEmpty" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TBool
      description = "Returns `true` if `s` is the empty string \"\"."
      fn =
        (function
        | _, [ DStr s ] -> Value(DBool(s = ""))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "foreach" 0
      parameters =
        [ Param.make "s" TStr "" // CLEANUP "string to iterate over"
          Param.makeWithArgs "f" (TFn([ TChar ], TChar)) "" [ "char" ]
          // CLEANUP "function used to convert one character to another"
          ]
      returnType = TStr
      description =
        "Iterate over each character (byte, not EGC) in the string, performing the operation in the block on each one"
      fn =
        function
        | state, [ s; f ] -> Errors.removedFunction ()
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "foreach" 1) }
    { name = fn "String" "foreach" 1
      parameters =
        [ Param.make "s" TStr ""
          Param.makeWithArgs "f" (TFn([ TChar ], TChar)) "" [ "character" ] ]
      returnType = TStr
      description =
        "Iterate over each Character (EGC, not byte) in the string, performing the operation in the block on each one."
      fn =
        (function
        | state, [ DStr s; DFnVal b ] ->
            (String.toEgcSeq s
             |> Seq.toList
             |> map_s
                  (fun te ->
                    (LibExecution.Interpreter.applyFnVal
                      state
                      (id 0)
                      b
                      [ DChar te ]
                      NotInPipe
                      NoRail))
             |> (fun dvals ->
               (taskv {
                 let! (dvals : List<Dval>) = dvals

                 match List.tryFind (fun dv -> Dval.isIncomplete dv) dvals with
                 | Some i -> return i
                 | None ->
                     let chars =
                       List.map
                         (function
                         | DChar c -> c
                         | dv -> Errors.throw (Errors.expectedLambdaType TChar dv))
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
        | _, [] -> Value(DStr "\n")
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
        | state, [ s ] -> Errors.removedFunction ()
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
             |> Value)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "replaceAll" 0
      parameters =
        [ Param.make "s" TStr "" // CLEANUP "The string to operate on"
          Param.make "searchFor" TStr "" // CLEANUP "The string to search for within <param s>"
          Param.make "replaceWith" TStr "" ]
      returnType = TStr
      description = "Replace all instances on `searchFor` in `s` with `replaceWith`"
      fn =
        (function
        | _, [ DStr s; DStr search; DStr replace ] ->
            if search = "" then
              if s = "" then
                Value(DStr replace)
              else
                // .Net Replace doesn't allow empty string, but we do.
                String.toEgcSeq s
                |> Seq.toList
                |> List.intersperse replace
                |> (fun l -> replace :: l @ [ replace ])
                |> String.concat ""
                |> DStr
                |> Value
            else
              Value(DStr(s.Replace(search, replace)))
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
              let int = s |> System.Numerics.BigInteger.Parse

              if int < -4611686018427387904I then failwith "goto exception case"
              else if int >= 4611686018427387904I then failwith "goto exception case"
              else int |> DInt |> Value
             with e -> err (Errors.argumentWasnt "numeric" "s" (DStr s)))
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
              // CLEANUP: These constants represent how high the OCaml parsers would go
              let int = s |> System.Numerics.BigInteger.Parse

              if int < -4611686018427387904I then failwith "goto exception case"
              else if int >= 4611686018427387904I then failwith "goto exception case"
              else int |> DInt |> Ok |> DResult |> Value
            with e ->
              $"Expected to parse string with only numbers, instead got \"{s}\""
              |> DStr
              |> Error
              |> DResult
              |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "toFloat" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TFloat
      description = "Returns the float value of the string"
      fn =
        (function
        | _, [ DStr s as dv ] ->
            (try
              float (s) |> DFloat |> Value
             with e ->
               err (
                 Errors.argumentWasnt
                   "a string representation of an IEEE float"
                   "s"
                   dv
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
              float (s) |> DFloat |> Ok |> DResult |> Value
             with e ->
               "Expected a string representation of an IEEE float"
               |> DStr
               |> Error
               |> DResult
               |> Value)
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
            |> Value
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
        | _, [ DStr s ] -> Value(DStr(String.toUpper s))
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
            |> Value
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
        | _, [ DStr s ] -> Value(DStr(String.toLower s))
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
            s |> System.Text.ASCIIEncoding.UTF8.GetByteCount |> Dval.int |> Value
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
        | _, [ DStr s ] -> s |> String.lengthInEgcs |> Dval.int |> Value
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
            Value(
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
        | _, [ DStr s1; DStr s2 ] -> Value(DStr((s1 + s2).Normalize()))
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
        | _, [ DStr s1; DStr s2 ] -> Value(DStr(s2 + s1))
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
            |> String.toLower
            |> DStr
            |> Value

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
            |> String.toLower
            |> DStr
            |> Value
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
            |> String.toLower
            |> replace toRemove ""
            |> fun s -> s.Trim()
            |> replace toBeHyphenated "-"
            |> DStr
            |> Value
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
            String.toEgcSeq s |> Seq.rev |> String.concat "" |> DStr |> Value
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
              s |> String.toEgcSeq |> Seq.toList |> List.map DStr |> DList |> Value
            else
              // CLEANUP
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
              |> Value
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
                  | _ -> Errors.throw (Errors.expectedLambdaType TStr s))
                l

            // CLEANUP: The OCaml doesn't normalize after concat, so we don't either
            Value(DStr((String.concat sep strs)))
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
        | state, [ l ] -> Errors.removedFunction ()
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
              |> List.map
                   (function
                   | DChar c -> c
                   | dv -> Errors.throw (Errors.expectedLambdaType TChar dv))
              |> String.concat ""
            )
            |> Value
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
        | state, [ c ] -> Errors.removedFunction ()
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
        | _, [ DChar c ] -> Value(DStr(c))
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
        | _, [ DStr s ] -> s |> toBytes |> base64UrlEncode |> DStr |> Value
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
              // This seems valid
              Value(DStr "")
            // dotnet ignores whitespace but we don't allow it
            else if Regex.IsMatch(s, @"\s") then
              err "Not a valid base64 string"
            else
              try
                let decodedBytes =
                  try
                    // Try regular
                    System.Convert.FromBase64String s
                  with e ->
                    // try URL safe
                    s |> base64FromUrlEncoded |> System.Convert.FromBase64String

                decodedBytes |> ofBytes |> fun s -> s.Normalize() |> DStr |> Value
              with _ -> err "Not a valid base64 string"
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      // CLEANUP: this shouldnt return a string and should be deprecated
      deprecated = NotDeprecated }
    { name = fn "String" "digest" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Take a string and hash it to a cryptographically-secure digest.
         Don't rely on either the size or the algorithm."
      fn =
        (function
        | _, [ DStr s ] ->
            let sha384Hash = SHA384.Create()
            let data = System.Text.Encoding.UTF8.GetBytes(s)

            let bytes = sha384Hash.ComputeHash(data)

            System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
            |> DStr
            |> Value
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

            System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
            |> DStr
            |> Value
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

            System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
            |> DStr
            |> Value
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
            if l < 0I then
              err "l should be a positive integer"
            else
              let randomString length =
                let gen () =
                  match random.Next(26 + 26 + 10) with
                  | n when n < 26 -> ('a' |> int) + n
                  | n when n < 26 + 26 -> ('A' |> int) + n - 26
                  | n -> ('0' |> int) + n - 26 - 26

                let gen _ = char (gen ()) in

                (Array.toList (Array.init length gen))
                |> List.map (fun i -> i.ToString())
                |> String.concat ""

              randomString (int l) |> DStr |> Value
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
            if l < 0I then
              "l should be a positive integer" |> DStr |> Error |> DResult |> Value
            else
              let randomString length =
                let gen () =
                  match random.Next(26 + 26 + 10) with
                  | n when n < 26 -> ('a' |> int) + n
                  | n when n < 26 + 26 -> ('A' |> int) + n - 26
                  | n -> ('0' |> int) + n - 26 - 26

                let gen _ = char (gen ()) in

                (Array.toList (Array.init length gen))
                |> List.map (fun i -> i.ToString())
                |> String.concat ""

              randomString (int l) |> DStr |> Ok |> DResult |> Value
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
            if l < 0I then
              dv
              |> Errors.argumentWasnt "positive" "length"
              |> DStr
              |> Error
              |> DResult
              |> Value
            else
              let randomString length =
                let gen () =
                  match random.Next(26 + 26 + 10) with
                  | n when n < 26 -> ('a' |> int) + n
                  | n when n < 26 + 26 -> ('A' |> int) + n - 26
                  | n -> ('0' |> int) + n - 26 - 26

                let gen _ = char (gen ()) in

                (Array.toList (Array.init length gen))
                |> List.map (fun i -> i.ToString())
                |> String.concat ""

              randomString (int l) |> DStr |> Ok |> DResult |> Value
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

            Value(DStr(htmlEscape s))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
            | true, x -> x |> DUuid |> Value
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
            | true, x -> x |> DUuid |> Ok |> DResult |> Value
            | _ ->
                "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
                |> DStr
                |> Error
                |> DResult
                |> Value
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
        | _, [ DStr needle; DStr haystack ] ->
            DBool(haystack.Contains needle) |> Value
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
        | _, [ DStr haystack; DStr needle ] ->
            DBool(haystack.Contains needle) |> Value
        | _ -> incorrectArgs ())
      sqlSpec =
        SqlCallback2
          (fun lookingIn searchingFor ->
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
        | _, [ DStr haystack; DStr needle ] -> Value(DBool(haystack.Contains needle))
        | _ -> incorrectArgs ())
      sqlSpec =
        SqlCallback2
          (fun lookingIn searchingFor ->
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
      description = "Returns the substring of `string` between the `from` and `to` indices.
       Negative indices start counting from the end of `string`.
       Indices represent characters."
      fn =
        (function
        | _, [ DStr s; DInt first; DInt last ] ->

            let chars = String.toEgcSeq s
            let length = length chars |> bigint

            let normalize (i : bigint) =
              i
              |> fun i -> if i < 0I then length + i else i // account for - values
              |> min length
              |> max 0I


            let f = normalize first |> int
            let l = normalize last |> int
            let l = if f > l then f else l // return empty string when start is less than end

            chars
            |> Seq.toList
            |> List.drop f
            |> List.take (l - f)
            |> String.concat ""
            |> DStr
            |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "first" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description = "Returns the first `characterCount` characters of `string`, as a String.
      If `characterCount` is longer than `string`, returns `string`.
      If `characterCount` is negative, returns the empty string."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
            let n = String.lengthInEgcs s |> bigint |> min n |> max 0I |> int

            String.toEgcSeq s
            |> List.take n
            |> String.concat ""
            |> fun s -> s.Normalize()
            |> DStr
            |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "last" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description = "Returns the last `characterCount` characters of `string`, as a String.
      If `characterCount` is longer than `string`, returns `string`.
      If `characterCount` is negative, returns the empty string."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
            let egcSeq = String.toEgcSeq s
            let stringEgcCount = length egcSeq

            let lastN s (numEgcs : bigint) =
              let stringBuilder = new StringBuilder(String.length s) in
              (* We iterate through every EGC, adding it to the buffer
                * if its [idx] >= ([stringEgcCount] - [numEgcs]).
                * Consider if the string is "abcde" and [numEgcs] = 2,
                * [stringEgcCount] = 5; 5-2 = 3. The index of "d" is 3 and
                * we want to keep it and everything after it so we end up with "de". *)

              let startIdx = bigint stringEgcCount - numEgcs in

              let lastFunc (idx : bigint) (seg : string) =
                if idx >= startIdx then
                  stringBuilder.Append seg |> ignore
                else
                  () |> ignore

                1I + idx

              ignore (
                egcSeq
                |> Seq.toList
                |> List.mapi (fun index value -> (lastFunc (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              stringBuilder.ToString()

            Value(DStr(lastN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "dropLast" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description = "Returns all but the last `characterCount` characters of `string`, as a String.
      If `characterCount` is longer than `string`, returns the empty string.
      If `characterCount` is negative, returns `string`."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
            let egcSeq = String.toEgcSeq s
            let stringEgcCount = length egcSeq

            let dropLastN s (numEgcs : bigint) =
              let stringBuilder = new StringBuilder(String.length s) in
              (* We iterate through every EGC, adding it to the buffer
               * if its [idx] < ([stringEgcCount] - [numEgcs]).
               * This works by the inverse of the logic for [lastN]. *)

              let startIdx = bigint stringEgcCount - numEgcs in

              let lastFunc (idx : bigint) (seg : string) =
                if idx < startIdx then
                  stringBuilder.Append seg |> ignore
                else
                  () |> ignore

                1I + idx

              ignore (
                egcSeq
                |> Seq.toList
                |> List.mapi (fun index value -> (lastFunc (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              stringBuilder.ToString()

            Value(DStr(dropLastN s n))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "dropFirst" 0
      parameters =
        [ Param.make "string" TStr ""; Param.make "characterCount" TInt "" ]
      returnType = TStr
      description = "Returns all but the first `characterCount` characters of `string`, as a String.
        If `characterCount` is longer than `string`, returns the empty string.
        If `characterCount` is negative, returns `string`."
      fn =
        (function
        | _, [ DStr s; DInt n ] ->
            let dropFirstN s (numEgcs : bigint) =
              let stringBuilder = new StringBuilder(String.length s) in
              (* We iterate through every EGC, adding it to the buffer
               * if its index >= numEgcs. This works by the inverse of the logic for [first_n]: *)
              let firstFunc (idx : bigint) (seg : string) =
                if idx >= numEgcs then
                  stringBuilder.Append seg |> ignore
                else
                  () |> ignore

                1I + idx

              ignore (
                String.toEgcSeq s
                |> Seq.toList
                |> List.mapi (fun index value -> (firstFunc (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              stringBuilder.ToString()

            Value(DStr(dropFirstN s n))
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
      description = "If `string` is shorter than `goalLength` characters, returns a copy of `string` starting with enough copies of `padWith` for the result have `goalLength`.
      If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
      fn =
        (function
        | state, [ DStr s; DStr padWith as dv; DInt l ] ->

            let egcSeq = String.toEgcSeq s

            let padStart s padWith targetEgcs =
              let max a b = if a > b then a else b in
              (* Compute the size in bytes and # of required EGCs for s and padWith: *)
              let padSize = String.length padWith in

              let padEgcs = length padWith in
              let stringSize = String.length s in
              let stringEgcs = length egcSeq in
              (* Compute how many copies of padWith we require,
               * accounting for the string longer than [targetEgcs]: *)
              let requiredEgcs = targetEgcs - stringEgcs in

              let reqPads = max 0 (if padEgcs = 0 then 0 else requiredEgcs / padEgcs) in
              (* Create a buffer large enough to hold the padded result: *)
              let requiredSize = stringSize + (reqPads * padSize) in

              let stringBuilder = new StringBuilder(requiredSize) in
              (* Fill with the required number of pads: *)
              for i = 1 to reqPads do
                stringBuilder.Append padWith |> ignore
              (* Finish by filling with the string: *)
              stringBuilder.Append s |> ignore
              (* Renormalize because concatenation may break normalization
               * (see https://unicode.org/reports/tr15/#Concatenation): *)

              stringBuilder.ToString().Normalize()

            let padLen = length padWith in

            if padLen = 1 then
              let l = int l in Value(DStr(padStart s padWith (int l)))
            else
              err (Errors.argumentWasnt "1 character long" "padWith" dv)
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
      description = "If `string` is shorter than `goalLength` characters, returns a copy of `string` ending with enough copies of `padWith` for the result have `goalLength`.
      If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
      fn =
        (function
        | state, [ DStr s; DStr padWith as dv; DInt l ] ->

            let egcSeq = String.toEgcSeq s

            let padEnd s padWith targetEgcs =
              let max a b = if a > b then a else b in
              // Compute the size in bytes and # of required EGCs for s and padWith:
              let padSize = String.length padWith in

              let padEgcs = length padWith in
              let stringSize = String.length s in
              let stringEgcs = length egcSeq in
              // Compute how many copies of padWith we require,
              // accounting for the string longer than [targetEgcs]:
              let requiredEgcs = targetEgcs - stringEgcs in

              let requiredPads =
                max 0 (if padEgcs = 0 then 0 else requiredEgcs / padEgcs) in
              // Create a buffer large enough to hold the padded result:
              let requiredSize = stringSize + (requiredPads * padSize) in

              let stringBuilder = new StringBuilder(requiredSize) in
              // Start the buffer with the string: *)
              stringBuilder.Append s |> ignore
              // Finish by filling with the required number of pads:
              for i = 1 to requiredPads do
                stringBuilder.Append padWith |> ignore
              // Renormalize because concatenation may break normalization
              // (see https://unicode.org/reports/tr15/#Concatenation):

              stringBuilder.ToString().Normalize()

            let padLen = length padWith in

            if padLen = 1 then
              Value(DStr(padEnd s padWith (int l)))
            else
              err (Errors.argumentWasnt "1 character long" "padWith" dv)
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
        | _, [ DStr toTrim ] -> Value(DStr(toTrim.Trim()))
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
        | _, [ DStr toTrim ] -> Value(DStr(toTrim.TrimStart()))
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
        | _, [ DStr toTrim ] -> Value(DStr(toTrim.TrimEnd()))
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
            Value(DBytes theBytes)
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
            Value(DBool(subject.StartsWith(prefix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "endsWith" 0
      parameters =
        [ Param.make "subject" TStr "" // CLEANUP "String to test"
          Param.make "suffix" TStr "" ]
      returnType = TBool
      description = "Checks if `subject` ends with `suffix`"
      fn =
        (function
        | _, [ DStr subject; DStr suffix ] ->
            Value(DBool(subject.EndsWith(suffix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
