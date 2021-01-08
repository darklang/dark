module LibExecution.StdLib.LibString

open System.Globalization
open System.Security.Cryptography
open System
open System.Text

(* type coerces one list to another using a function *)

// let list_coerce (f: Dval -> Option<'a>) (l: List<Dval>): Result<List<'a>, List<Dval> * Dval> =
//   l
//   |> List.map (fun dv ->
//        match f dv with
//        | Some v -> Result.Ok v
//        | None -> Result.Error(l, dv))
//   |> Result.all

// let error_result msg =
//   DResult(ResError(Dval.dstr_of_string_exn msg))
open System.Threading.Tasks
open FSharp.Control.Tasks
open LibExecution.RuntimeTypes
open FSharpPlus
open Prelude
open System.Text.RegularExpressions

let fn = FQFnName.stdlibName

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "String" "isEmpty" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TBool
      description = "Returns <val true> if <param s> is the empty string <val \"\">."
      fn =
        (function
        | _, [ DStr s ] -> Value(DBool(s = ""))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "foreach" 0
      parameters =
        [ Param.make "s" TStr "string to iterate over"
          Param.make
            "f"
            (TFn([ TChar ], TChar))
            "function used to convert one character to another" ]
      returnType = TStr
      description =
        "Iterate over each character (byte, not EGC) in the string, performing the operation in the block on each one"
      fn = removedFunction
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "foreach" 1) }
    { name = fn "String" "foreach" 1
      parameters =
        [ Param.make "s" TStr ""; Param.make "f" (TFn([ TChar ], TChar)) "" ]
      returnType = TStr
      description =
        "Iterate over each Character (EGC, not byte) in the string, performing the operation in the block on each one."
      fn =
        (function
        | state, [ DStr s; DFnVal b ] ->
            (String.toEgcSeq s
             |> Seq.toList
             |> Prelude.map_s
                  (fun te ->
                    (LibExecution.Interpreter.applyFnVal
                      state
                      b
                      [ DChar te ]
                      NotInPipe
                      NoRail))
             |> (fun dvals ->
               (taskv {
                 let! dvals = dvals

                 let chars =
                   List.map
                     (function
                     | DChar c -> c
                     | dv ->
                         raise (
                           RuntimeException(LambdaResultHasWrongType(dv, TChar))
                         ))
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
      fn = removedFunction
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
        | args -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "replaceAll" 0
      parameters =
        [ Param.make "s" TStr "The string to operate on"
          Param.make "searchFor" TStr "The string to search for within <param s>"
          Param.make
            "replaceWith"
            TStr
            "The string to replace all instances of <param searchFor> with" ]
      returnType = TStr
      description = "Replace all instances on `searchFor` in `s` with `replaceWith`"
      fn =
        (function
        | _, [ DStr s; DStr search; DStr replace ] ->
            Value(DStr(s.Replace(search, replace)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
              s |> System.Numerics.BigInteger.Parse |> DInt |> Value
             with e -> Value(errStr ("Expected a string with only numbers")))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toInt" 1) }
    { name = fn "String" "toInt" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TInt
      description =
        "Returns the int value of the string, wrapped in a `Ok`, or `Error <msg>` if the string contains characters other than numeric digits"
      fn =
        (function
        | _, [ DStr s ] ->
            try
              s |> System.Numerics.BigInteger.Parse |> DInt |> Ok |> DResult |> Value
            with e ->
              $"Expected to parse string with only numbers, instead got \"{s}\""
              |> DStr
              |> Error
              |> DResult
              |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "toFloat" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TFloat
      description = "Returns the float value of the string"
      fn =
        (function
        | _, [ DStr s ] ->
            (try
              float (s) |> DFloat |> Value
             with e ->
               Value(errStr ("Expected a string representation of an IEEE float")))
        | args -> incorrectArgs ())
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
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "toUppercase" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Returns the string, uppercased"
      fn =
        (function
        | _, [ DStr s ] -> Value(DStr(String.toUpper s))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "toUppercase" 1) }
    { name = fn "String" "toUppercase" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Returns the string, uppercased"
      fn =
        (function
        | _, [ DStr s ] -> Value(DStr(String.toUpper s))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "toLowercase" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Returns the string, lowercased"
      fn =
        (function
        | _, [ DStr s ] -> Value(DStr(String.toLower s))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "String" "toLowercase" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description = "Returns the string, lowercased"
      fn =
        (function
        | _, [ DStr s ] -> Value(DStr(String.toLower s))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "length" 1) }
    { name = fn "String" "length" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, [ DStr s ] -> s |> String.lengthInEgcs |> Dval.int |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
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
        | args -> incorrectArgs ())
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
        | args -> incorrectArgs ())
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
        | args -> incorrectArgs ())
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

            let to_remove = @"[^\-\w\s$*_+~.()'\""!:@]"
            let trim = @"^\s+|\s+$"
            let spaces = @"[-\s]+"

            let objRegex (pattern : string) (input : string) (replacement : string) =
              Regex.Replace(input, pattern, replacement)

            s
            |> fun s -> objRegex to_remove s ""
            |> fun s -> objRegex trim s ""
            |> fun s -> objRegex spaces s "-"

            |> String.toLower
            |> DStr
            |> Value

        | args -> incorrectArgs ())
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

            let to_remove = @"[^\w\s_-]"
            let trim = @"^\s+|\s+$"
            let newspaces = @"[-_\s]+"

            let objRegex (pattern : string) (input : string) (replacement : string) =
              Regex.Replace(input, pattern, replacement)

            s
            |> fun s -> objRegex to_remove s ""
            |> fun s -> objRegex trim s ""
            |> fun s -> objRegex newspaces s "-"

            |> String.toLower
            |> DStr
            |> Value
        | args -> incorrectArgs ())
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
            let to_remove = @"[^a-z0-9\s_-]+"
            let to_be_hyphenated = @"[-_\s]+"

            let objRegex (pattern : string) (input : string) (replacement : string) =
              Regex.Replace(input, pattern, replacement)

            s
            |> String.toLower
            |> fun s -> objRegex to_remove s ""
            |> fun s -> s.Trim()
            |> fun s -> objRegex to_be_hyphenated s "-"

            |> String.toLower
            |> DStr
            |> Value
        | args -> incorrectArgs ())
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
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "split" 0
      parameters = [ Param.make "s" TStr ""; Param.make "separator" TStr "" ]
      returnType = TList varA
      description =
        "Splits a string at the separator, returning a list of strings without the separator. If the separator is not present, returns a list containing only the initial string."
      fn =
        (function
        | _, [ DStr s; DStr sep ] ->
            s.Split sep
            |> Array.toList
            |> List.map (fun str -> DStr str)
            |> DList
            |> Value
        | args -> incorrectArgs ())
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
                  | _ ->
                      raise (
                        RuntimeException(JustAString(SourceNone, "Expected String"))
                      ))
                l

            Value(DStr((String.concat sep strs).Normalize()))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "fromList" 0
      parameters = [ Param.make "l" (TList TChar) "" ]
      returnType = TStr
      description = "Returns the list of characters as a string"
      fn =
        (fun _ ->
          raise (
            RuntimeException(
              JustAString(SourceNone, "This function no longer exists.")
            )
          ))
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
                   | dv ->
                       raise (RuntimeException(LambdaResultHasWrongType(dv, TChar))))
              |> String.concat ""
            )
            |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "fromChar" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description = "Converts a char to a string"
      fn = (fun _ -> failwith "This function no longer exists.")
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
        | args -> incorrectArgs ())
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
        | _, [ DStr s ] -> String.base64UrlEncode s |> DStr |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    //      { name = fn "String" "base64Decode" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description =
//       "Base64 decodes a string. Works with both the URL-safe and standard Base64 alphabets defined in RFC 4648 sections 4 and 5."
//      ; fn =
//         (function
//         | _, [DStr s] ->
//           ( try
//               Dval.dstr_of_string_exn
//                 (B64.decode
//                    B64.uri_safe_alphabet
//                    (Unicode_string.to_string s))
//             with Not_found_s _ | Caml.Not_found ->
//               ( try
//                   Dval.dstr_of_string_exn
//                     (B64.decode
//                        B64.default_alphabet
//                        (Unicode_string.to_string s))
//                 with Not_found_s _ | Caml.Not_found ->
//                   RT.error
//                       (Dval.dstr_of_string_exn (Unicode_string.to_string s))
//                     "Not a valid base64 string" ) )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "digest" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description =
//       "Take a string and hash it to a cryptographically-secure digest.
// Don't rely on either the size or the algorithm."
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Libtarget.digest384 (Unicode_string.to_string s))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
    { name = fn "String" "sha384" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TStr
      description =
        "Take a string and hash it using SHA. Please use Crypto::sha384 instead."
      fn =
        (function
        | _, [ DStr s ] ->
            let sha384Hash = SHA384.Create()
            let data = System.Text.Encoding.UTF8.GetBytes(s)

            let bytes = sha384Hash.ComputeHash(data)

            System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
            |> DStr
            |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "" "" 0) }
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
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "String" "random" 0
      parameters = [ Param.make "length" TInt "" ]
      returnType = TStr
      description = "Generate a string of length `length` from random characters."
      fn =
        (function
        | _, [ DInt l ] ->
            if l < 0I then
              raise (
                RuntimeException(
                  JustAString(SourceNone, "l should be a positive integer")
                )
              )
            else
              let random_string length =
                let gen () =
                  match random.Next(26 + 26 + 10) with
                  | n when n < 26 -> ('a' |> int) + n
                  | n when n < 26 + 26 -> ('A' |> int) + n - 26
                  | n -> ('0' |> int) + n - 26 - 26

                let gen _ = char (gen ()) in

                (Array.toList (Array.init length gen))
                |> List.map (fun i -> i.ToString())
                |> String.concat ""

              random_string (int l) |> DStr |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = ReplacedBy(fn "" "" 0) }
    //      { name = fn "String" "random" 1
//      ; parameters = [Param.make "length" TInt]
//      ; returnType = TResult
//      ; description =
//       "Generate a string of length `length` from random characters."
//      ; fn =
//         (function
//         | _, [DInt l] ->
//             if l < Dint.zero
//             then error_result "l should be a positive integer"
//             else
//               DResult
//                 (ResOk
//                    (Dval.dstr_of_string_exn
//                       (Stdlib_util.random_string (Dint.to_int_exn l))))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Impure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "random" 2
//      ; parameters = [Param.make "length" TInt]
//      ; returnType = TResult
//      ; description =
//       "Generate a string of length `length` from random characters."
//      ; fn =
//         (function
//         | _, [DInt l] ->
//             if l < Dint.zero
//             then error_result "l should be a positive integer"
//             else
//               Dval.to_res_ok
//                 (Dval.dstr_of_string_exn
//                    (Stdlib_util.random_string (Dint.to_int_exn l)))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Impure
//      ; deprecated = NotDeprecated }
    { name = fn "String" "htmlEscape" 0
      parameters = [ Param.make "html" TStr "" ]
      returnType = TStr
      description =
        "Escape an untrusted string in order to include it safely in HTML output."
      fn =
        (function
        | _, [ DStr s ] ->
            let html_escape (html : string) : string =
              List.map
                (fun c ->
                  match c with
                  | '<' -> "&lt;"
                  | '>' -> "&gt;"
                  | '&' -> "&amp;"
                  (* include these for html-attribute-escaping
                            even though they're not strictly necessary
                            for html-escaping proper. *)
                  | '"' -> "&quot;"
                  (* &apos; doesn't work in IE.... *)
                  | ''' -> "&#x27;"
                  | _ -> string c)
                (Seq.toList html)
              |> String.concat ""

            Value(DStr(html_escape s))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated }
    //      { name = fn "String" "toUUID" 0
//      ; parameters = [Param.make "uuid" TStr]
//      ; returnType = TUuid
//      ; description =
//       "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//           ( match Uuidm.of_string (Unicode_string.to_string s) with
//           | Some id ->
//               DUuid id
//           | None ->
//               Exception.code
//                 "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
//           )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "toUUID" 1
//      ; parameters = [Param.make "uuid" TStr]
//      ; returnType = TResult
//      ; description =
//       "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//           ( match Uuidm.of_string (Unicode_string.to_string s) with
//           | Some id ->
//               DResult (ResOk (DUuid id))
//           | None ->
//               error_result
//                 "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
//           )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "isSubstring" 0
//      ; parameters = [Param.make "searchingFor" TStr; Param.make "lookingIn" TStr]
//      ; returnType = TBool
//      ; description = "Checks if `lookingIn` contains `searchingFor`"
//      ; fn =
//         (function
//         | _, [DStr needle; DStr haystack] ->
//             DBool (Unicode_string.is_substring needle haystack)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "isSubstring" 1
//      ; parameters = [Param.make "lookingIn" TStr; Param.make "searchingFor" TStr]
//      ; returnType = TBool
//      ; description = "Checks if `lookingIn` contains `searchingFor`"
//      ; fn =
//         (function
//         | _, [DStr haystack; DStr needle] ->
//             DBool (Unicode_string.is_substring needle haystack)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "contains" 0
//      ; parameters = [Param.make "lookingIn" TStr; Param.make "searchingFor" TStr]
//      ; returnType = TBool
//      ; description = "Checks if `lookingIn` contains `searchingFor`"
//      ; fn =
//         (function
//         | _, [DStr haystack; DStr needle] ->
//             DBool (Unicode_string.is_substring needle haystack)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
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
        | _, [ DStr s; DInt f; DInt l ] ->
            let egc_seq = String.toEgcSeq s
            let s_egc_count = bigint (length egc_seq)

            let slice s (first : bigint) (last : bigint) =
              let clamp_unchecked t min max =
                if t < min then min
                else if t <= max then t
                else max

              let len = s_egc_count in
              let min = 0I in
              let max = len + 1I in
              (* If we get negative indices, we need to treat them as indices from the end
               * which means that we need to add [len] to them. We clamp the result to
               * a value within range of the actual string: *)

              let first =
                if first >= 0I then
                  first
                else
                  len + first |> clamp_unchecked min max

              let last =
                if last >= 0I then
                  last
                else
                  len + last |> clamp_unchecked min max

              let b = new StringBuilder(String.length s) in
              (* To slice, we iterate through every EGC, adding it to the buffer
               * if it is within the specified index range: *)
              let slicer_func (acc : bigint) (seg : string) =
                if acc >= first && acc < last then
                  b.Append seg |> ignore
                else
                  () |> ignore

                1I + acc

              ignore (
                egc_seq
                |> Seq.toList
                |> List.mapi (fun index value -> (slicer_func (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              b.ToString()

            let first, last = (f, l) in
            Value(DStr(slice s first last))
        | args -> incorrectArgs ())
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
            let first_n s (num_egcs : bigint) =
              let b = new StringBuilder(String.length s) in
              (* We iterate through every EGC, adding it to the buffer
               * if its index < num_egcs: *)

              let first_func (idx : bigint) (seg : string) =
                if idx < num_egcs then b.Append seg |> ignore else () |> ignore
                1I + idx

              ignore (
                String.toEgcSeq s
                |> Seq.toList
                |> List.mapi (fun index value -> (first_func (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              b.ToString()

            Value(DStr(first_n s n))
        | args -> incorrectArgs ())
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
            let egc_seq = String.toEgcSeq s
            let s_egc_count = length egc_seq

            let last_n s (num_egcs : bigint) =
              let b = new StringBuilder(String.length s) in
              (* We iterate through every EGC, adding it to the buffer
                * if its [idx] >= ([s_egc_count] - [num_egcs]).
                * Consider if the string is "abcde" and [num_egcs] = 2,
                * [s_egc_count] = 5; 5-2 = 3. The index of "d" is 3 and
                * we want to keep it and everything after it so we end up with "de". *)

              let start_idx = bigint s_egc_count - num_egcs in

              let last_func (idx : bigint) (seg : string) =
                if idx >= start_idx then b.Append seg |> ignore else () |> ignore
                1I + idx

              ignore (
                egc_seq
                |> Seq.toList
                |> List.mapi (fun index value -> (last_func (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              b.ToString()

            Value(DStr(last_n s n))
        | args -> incorrectArgs ())
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
            let egc_seq = String.toEgcSeq s
            let s_egc_count = length egc_seq

            let drop_last_n s (num_egcs : bigint) =
              let b = new StringBuilder(String.length s) in
              (* We iterate through every EGC, adding it to the buffer
               * if its [idx] < ([s_egc_count] - [num_egcs]).
               * This works by the inverse of the logic for [last_n]. *)

              let start_idx = bigint s_egc_count - num_egcs in

              let last_func (idx : bigint) (seg : string) =
                if idx < start_idx then b.Append seg |> ignore else () |> ignore
                1I + idx

              ignore (
                egc_seq
                |> Seq.toList
                |> List.mapi (fun index value -> (last_func (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              b.ToString()

            Value(DStr(drop_last_n s n))
        | args -> incorrectArgs ())
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
            let drop_first_n s (num_egcs : bigint) =
              let b = new StringBuilder(String.length s) in
              (* We iterate through every EGC, adding it to the buffer
               * if its index >= num_egcs. This works by the inverse of the logic for [first_n]: *)
              let first_func (idx : bigint) (seg : string) =
                if idx >= num_egcs then b.Append seg |> ignore else () |> ignore
                1I + idx

              ignore (
                String.toEgcSeq s
                |> Seq.toList
                |> List.mapi (fun index value -> (first_func (bigint index) value))
              )
              (* We don't need to renormalize because all normalization forms are closed
               * under substringing (see https://unicode.org/reports/tr15/#Concatenation). *)
              b.ToString()

            Value(DStr(drop_first_n s n))
        | args -> incorrectArgs ())
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
        | state, [ DStr s; DStr pad_with; DInt l ] ->

            let egc_seq = String.toEgcSeq s

            let pad_start s pad_with target_egcs =
              let max a b = if a > b then a else b in
              (* Compute the size in bytes and # of required EGCs for s and pad_with: *)
              let pad_size = String.length pad_with in

              let pad_egcs = length pad_with in
              let s_size = String.length s in
              let s_egcs = length egc_seq in
              (* Compute how many copies of pad_with we require,
               * accounting for the string longer than [target_egcs]: *)
              let req_egcs = target_egcs - s_egcs in

              let req_pads = max 0 (if pad_egcs = 0 then 0 else req_egcs / pad_egcs) in
              (* Create a buffer large enough to hold the padded result: *)
              let req_size = s_size + (req_pads * pad_size) in

              let b = new StringBuilder(req_size) in
              (* Fill with the required number of pads: *)
              for i = 1 to req_pads do
                b.Append pad_with |> ignore
              (* Finish by filling with the string: *)
              b.Append s |> ignore
              (* Renormalize because concatenation may break normalization
               * (see https://unicode.org/reports/tr15/#Concatenation): *)

              b.ToString().Normalize()

            let padLen = length pad_with in

            if padLen = 1 then
              let l = int l in
              Value(DStr(pad_start s pad_with l))
            else
              Value(
                errStr (
                  $"Expected the argument `padWith` passed to ` String:padStart ` to be one character long. However, `({
                                                                                                                          pad_with
                  }).` is characters long."
                )
              )
        | args -> incorrectArgs ())
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
        | state, [ DStr s; DStr pad_with; DInt l ] ->

            let egc_seq = String.toEgcSeq s

            let pad_end s pad_with target_egcs =
              let max a b = if a > b then a else b in
              (* Compute the size in bytes and # of required EGCs for s and pad_with: *)
              let pad_size = String.length pad_with in

              let pad_egcs = length pad_with in
              let s_size = String.length s in
              let s_egcs = length egc_seq in
              (* Compute how many copies of pad_with we require,
               * accounting for the string longer than [target_egcs]: *)
              let req_egcs = target_egcs - s_egcs in

              let req_pads = max 0 (if pad_egcs = 0 then 0 else req_egcs / pad_egcs) in
              (* Create a buffer large enough to hold the padded result: *)
              let req_size = s_size + (req_pads * pad_size) in

              let b = new StringBuilder(req_size) in
              (* Start the buffer with the string: *)
              b.Append s |> ignore
              (* Finish by filling with the required number of pads: *)
              for i = 1 to req_pads do
                b.Append pad_with |> ignore
              (* Renormalize because concatenation may break normalization
               * (see https://unicode.org/reports/tr15/#Concatenation): *)

              b.ToString().Normalize()

            let padLen = length pad_with in

            if padLen = 1 then
              let l = int l in
              Value(DStr(pad_end s pad_with l))
            else
              Value(
                errStr (
                  $"Expected the argument `padWith` passed to ` String:padEnd ` to be one character long. However, `({
                                                                                                                        pad_with
                  }).` is characters long."
                )
              )
        | args -> incorrectArgs ())
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
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    //      { name = fn "String" "trimStart" 0
//      ; parameters = [Param.make "str" TStr]
//      ; returnType = TStr
//      ; description =
//       "Returns a copy of `str` with all leading whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
//      ; fn =
//         (function
//         | _, [DStr to_trim] ->
//             DStr (Unicode_string.trim_start to_trim)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "trimEnd" 0
//      ; parameters = [Param.make "str" TStr]
//      ; returnType = TStr
//      ; description =
//       "Returns a copy of `str` with all trailing whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
//      ; fn =
//         (function
//         | _, [DStr to_trim] ->
//             DStr (Unicode_string.trim_end to_trim)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
    { name = fn "String" "toBytes" 0
      parameters = [ Param.make "str" TStr "" ]
      returnType = TBytes
      description =
        "Converts the given unicode string to a utf8-encoded byte sequence."
      fn =
        (function
        | _, [ DStr str ] ->
            let theBytes = System.Text.Encoding.UTF8.GetBytes str in
            Value(DBytes theBytes)
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "startsWith" 0
      parameters = [ Param.make "subject" TStr ""; Param.make "prefix" TStr "" ]
      returnType = TBool
      description = "Checks if `subject` starts with `prefix`"
      fn =
        (function
        | _, [ DStr subject; DStr prefix ] -> Value(DBool(subject.StartsWith prefix))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "String" "endsWith" 0
      parameters =
        [ Param.make "subject" TStr "String to test"
          Param.make "suffix" TStr "Suffix we're testing for" ]
      returnType = TBool
      description = "Checks if `subject` ends with `suffix`"
      fn =
        (function
        | _, [ DStr subject; DStr suffix ] -> Value(DBool(subject.EndsWith suffix))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
