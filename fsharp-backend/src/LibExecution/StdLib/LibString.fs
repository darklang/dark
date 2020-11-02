module LibExecution.LibString

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
open LibExecution.Runtime
open FSharpPlus
open Prelude

let fn = FnDesc.stdFnDesc

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
        | state, [ DStr s; DLambda b ] ->
            (String.toEgcSeq s
             |> Seq.toList
             |> Runtime.map_s (fun te ->
                  (Interpreter.eval_lambda state b [ DChar te ]))
             |> (fun dvals ->
             Task
               (task {
                 let! dvals = dvals

                 let chars =
                   List.map (function
                     | DChar c -> c
                     | dv ->
                         raise
                           (RuntimeException(LambdaResultHasWrongType(dv, TChar))))
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
    //      { name = fn "String" "toInt" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TInt
//      ; description = "Returns the int value of the string"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DInt (Dint.of_string_exn utf8)
//               with e ->
//                 Exception.code
//                   utf8
//                   "\\d+"
//                   "Expected a string with only numbers" )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "toInt" 1
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TResult
//      ; description =
//       "Returns the int value of the string, wrapped in a `Ok`, or `Error <msg>` if the string contains characters other than numeric digits"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DResult (ResOk (DInt (Dint.of_string_exn utf8)))
//               with e ->
//                 error_result
//                   ("Expected a string with only numbers, got " ^ utf8) )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "toFloat" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TFloat
//      ; description = "Returns the float value of the string"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DFloat (float_of_string utf8)
//               with e ->
//                 Exception.code
//                   utf8
//                   "Expected a string representation of an IEEE float" )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "toFloat" 1
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TResult
//      ; description = "Returns the float value of the string"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DResult (ResOk (DFloat (float_of_string utf8)))
//               with e ->
//                 error_result
//                   "Expected a string representation of an IEEE float" )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
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
    //      { name = fn "String" "toUppercase" 1
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description = "Returns the string, uppercased"
//      ; fn =
//         (function
//         | _, [DStr s] -> DStr (Unicode_string.uppercase s) | args -> incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "toLowercase" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description = "Returns the string, lowercased"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (String.lowercase (Unicode_string.to_string s))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "toLowercase" 1
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description = "Returns the string, lowercased"
//      ; fn =
//         (function
//         | _, [DStr s] -> DStr (Unicode_string.lowercase s) | args -> incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
    { name = fn "String" "length" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TInt
      description = "Returns the length of the string"
      fn =
        (function
        | _, [ DStr s ] ->
            s |> System.Text.ASCIIEncoding.Unicode.GetByteCount |> Dval.int |> Value
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
            Value
              (DStr
                (System.Text.Encoding.UTF8.GetString
                  (Array.append
                    (System.Text.Encoding.UTF8.GetBytes s1)
                     (System.Text.Encoding.UTF8.GetBytes s2))))
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
        | _, [ DStr s1; DStr s2 ] -> Value(DStr(s1 + s2))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    //      { name = fn "String" "prepend" 0
//      ; parameters = [Param.make "s1" TStr; Param.make "s2" TStr]
//      ; returnType = TStr
//      ; description =
//       "Concatenates the two strings by prepending `s2` to `s1` and returns the joined string."
//      ; fn =
//         (function
//         | _, [DStr s1; DStr s2] ->
//             DStr (Unicode_string.append s2 s1)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "slugify" 0
//      ; parameters = [Param.make "string" TStr]
//      ; returnType = TStr
//      ; description = "Turns a string into a slug"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             let replace = Unicode_string.regexp_replace in
//             let to_remove = "[^\\w\\s$*_+~.()'\"!\\-:@]" in
//             let trim = "^\\s+|\\s+$" in
//             let spaces = "[-\\s]+" in
//             s
//             |> replace
//                  to_remove
//                  (Unicode_string.of_string_exn "")
//             |> replace
//                  trim
//                  (Unicode_string.of_string_exn "")
//             |> replace
//                  spaces
//                  (Unicode_string.of_string_exn "-")
//             |> Unicode_string.lowercase
//             |> fun s -> DStr s
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "slugify" 1
//      ; parameters = [Param.make "string" TStr]
//      ; returnType = TStr
//      ; description = "Turns a string into a slug"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             let replace = Unicode_string.regexp_replace in
//             let to_remove = "[^\\w\\s_-]" in
//             let trim = "^\\s+|\\s+$" in
//             let newspaces = "[-_\\s]+" in
//             s
//             |> replace
//                  to_remove
//                  (Unicode_string.of_string_exn "")
//             |> replace
//                  trim
//                  (Unicode_string.of_string_exn "")
//             |> replace
//                  newspaces
//                  (Unicode_string.of_string_exn "-")
//             |> Unicode_string.lowercase
//             |> fun s -> DStr s
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "slugify" 2
//      ; parameters = [Param.make "string" TStr]
//      ; returnType = TStr
//      ; description =
//       "Turns a string into a prettified slug, including only lowercased alphanumeric characters, joined by hyphens"
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             (* Should work the same as https://blog.tersmitten.nl/slugify/ *)
//             let replace = Unicode_string.regexp_replace in
//             (* explicitly limit to (roman) alphanumeric for pretty urls *)
//             let to_remove = "[^a-z0-9\\s_-]+" in
//             let to_be_hyphenated = "[-_\\s]+" in
//             s
//             |> Unicode_string.lowercase
//             |> replace
//                  to_remove
//                  (Unicode_string.of_string_exn "")
//             |> Unicode_string.trim
//             |> replace
//                  to_be_hyphenated
//                  (Unicode_string.of_string_exn "-")
//             |> Unicode_string.lowercase
//             |> fun s -> DStr s
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "reverse" 0
//      ; parameters = [Param.make "string" TStr]
//      ; returnType = TStr
//      ; description = "Reverses `string`"
//      ; fn =
//         (function
//         | _, [DStr s] -> DStr (Unicode_string.rev s) | args -> incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "split" 0
//      ; parameters = [Param.make "s" TStr; Param.make "separator" TStr]
//      ; returnType = TList
//      ; description =
//       "Splits a string at the separator, returning a list of strings without the separator. If the separator is not present, returns a list containing only the initial string."
//      ; fn =
//         (function
//         | _, [DStr s; DStr sep] ->
//             s
//             |> Unicode_string.split ~sep
//             |> List.map (fun str -> DStr str)
//             |> DList
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
    { name = fn "String" "join" 0
      parameters = [ Param.make "l" (TList TStr) ""; Param.make "separator" TStr "" ]
      returnType = TStr
      description = "Combines a list of strings with the provided separator"
      fn =
        (function
        | _, [ DList l; DStr sep ] ->
            let strs =
              List.map (fun s ->
                match s with
                | DStr st -> st
                | _ ->
                    raise
                      (RuntimeException(JustAString(SourceNone, "Expected String"))))
                l

            Value(DStr(String.concat sep strs))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    //      { name = fn "String" "fromList" 0
//      ; parameters = [Param.make "l" TList]
//      ; returnType = TStr
//      ; description = "Returns the list of characters as a string"
//      ; fn =
//        (fun _ -> Exception.code "This function no longer exists.")
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "fromList" 1
//      ; parameters = [Param.make "l" TList]
//      ; returnType = TStr
//      ; description = "Returns the list of characters as a string"
//      ; fn =
//         (function
//         | _, [DList l] ->
//             DStr
//               ( l
//               |> List.map (function
//                      | DCharacter c ->
//                          c
//                      | dv ->
//                          RT.error dv "expected a char")
//               |> Unicode_string.of_characters )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
    { name = fn "String" "fromChar" 0
      parameters = [ Param.make "c" TChar "" ]
      returnType = TChar
      description = "Converts a char to a string"
      fn = (fun _ -> failwith "This function no longer exists.")
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "fromChar" 0) }
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
    //      { name = fn "String" "base64Encode" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description =
//       "URLBase64 encodes a string without padding. Uses URL-safe encoding with `-` and `_` instead of `+` and `/`, as defined in RFC 4648 section 5."
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (B64.encode
//                  B64.uri_safe_alphabet
//                  false
//                  (Unicode_string.to_string s))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
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
//      { name = fn "String" "sha384" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description =
//       "Take a string and hash it using SHA384. Please use Crypto::sha384 instead."
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Libtarget.digest384 (Unicode_string.to_string s))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "sha256" 0
//      ; parameters = [Param.make "s" TStr]
//      ; returnType = TStr
//      ; description =
//       "Take a string and hash it using SHA256. Please use Crypto::sha256 instead."
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Libtarget.digest256 (Unicode_string.to_string s))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
//      { name = fn "String" "random" 0
//      ; parameters = [Param.make "length" TInt]
//      ; returnType = TStr
//      ; description =
//       "Generate a string of length `length` from random characters."
//      ; fn =
//         (function
//         | _, [DInt l] ->
//             if l < Dint.zero
//             then Exception.code "l should be a positive integer"
//             else
//               Dval.dstr_of_string_exn
//                 (Stdlib_util.random_string (Dint.to_int_exn l))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Impure
//      ; deprecated = ReplacedBy(fn "" "" 0) }
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
//      { name = fn "String" "htmlEscape" 0
//      ; parameters = [Param.make "html" TStr]
//      ; returnType = TStr
//      ; description =
//       "Escape an untrusted string in order to include it safely in HTML output."
//      ; fn =
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Stdlib_util.html_escape (Unicode_string.to_string s))
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Impure
//      ; deprecated = NotDeprecated }
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
//      { name = fn "String" "slice" 0
//      ; parameters = [Param.make "string" TStr; Param.make "from" TInt; Param.make "to" TInt]
//      ; returnType = TStr
//      ; description =
//       "Returns the substring of `string` between the `from` and `to` indices.
//        Negative indices start counting from the end of `string`.
//        Indices represent characters."
//      ; fn =
//         (function
//         | _, [DStr s; DInt f; DInt l] ->
//             let first, last = (Dint.to_int_exn f, Dint.to_int_exn l) in
//             DStr (Unicode_string.slice s ~first ~last)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "first" 0
//      ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//      ; returnType = TStr
//      ; description =
//       "Returns the first `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns `string`.
//       If `characterCount` is negative, returns the empty string."
//      ; fn =
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.first_n s n)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "last" 0
//      ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//      ; returnType = TStr
//      ; description =
//       "Returns the last `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns `string`.
//       If `characterCount` is negative, returns the empty string."
//      ; fn =
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.last_n s n)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "dropLast" 0
//      ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//      ; returnType = TStr
//      ; description =
//       "Returns all but the last `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns the empty string.
//       If `characterCount` is negative, returns `string`."
//      ; fn =
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.drop_last_n s n)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "dropFirst" 0
//      ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//      ; returnType = TStr
//      ; description =
//       "Returns all but the first `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns the empty string.
//       If `characterCount` is negative, returns `string`."
//      ; fn =
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.drop_first_n s n)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "padStart" 0
//      ; parameters = [Param.make "string" TStr; Param.make "padWith" TStr; Param.make "goalLength" TInt]
//      ; returnType = TStr
//      ; description =
//       "If `string` is shorter than `goalLength` characters, returns a copy of `string` starting with enough copies of `padWith` for the result have `goalLength`.
//       If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
//      ; fn =
//         (function
//         | state, [DStr s; DStr pad_with; DInt l] ->
//             let padLen = Unicode_string.length pad_with in
//             if padLen = 1
//             then
//               let l = Dint.to_int_exn l in
//               DStr (Unicode_string.pad_start s ~pad_with l)
//             else
//               DError
//                 ( SourceNone
//                 , "Expected the argument `padWith` passed to `"
//                   ^ state.executing_fnname
//                   ^ "` to be one character long. However, `"
//                   ^ Dval.to_developer_repr_v0 (DStr pad_with)
//                   ^ "` is "
//                   ^ Int.to_string padLen
//                   ^ " characters long." )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "padEnd" 0
//      ; parameters = [Param.make "string" TStr; Param.make "padWith" TStr; Param.make "goalLength" TInt]
//      ; returnType = TStr
//      ; description =
//       "If `string` is shorter than `goalLength` characters, returns a copy of `string` ending with enough copies of `padWith` for the result have `goalLength`.
//       If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
//      ; fn =
//         (function
//         | state, [DStr s; DStr pad_with; DInt l] ->
//             let padLen = Unicode_string.length pad_with in
//             if padLen = 1
//             then
//               let l = Dint.to_int_exn l in
//               DStr (Unicode_string.pad_end s ~pad_with l)
//             else
//               DError
//                 ( SourceNone
//                 , "Expected the argument `padWith` passed to `"
//                   ^ state.executing_fnname
//                   ^ "` to be one character long. However, `"
//                   ^ Dval.to_developer_repr_v0 (DStr pad_with)
//                   ^ "` is "
//                   ^ Int.to_string padLen
//                   ^ " characters long." )
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
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
//      { name = fn "String" "toBytes" 0
//      ; parameters = [Param.make "str" TStr]
//      ; returnType = TBytes
//      ; description =
//       "Converts the given unicode string to a utf8-encoded byte sequence."
//      ; fn =
//         (function
//         | _, [DStr str] ->
//             let theBytes = Unicode_string.to_utf8_bytes str in
//             DBytes theBytes
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
//      { name = fn "String" "startsWith" 0
//      ; parameters = [Param.make "subject" TStr; Param.make "prefix" TStr]
//      ; returnType = TBool
//      ; description = "Checks if `subject` starts with `prefix`"
//      ; fn =
//         (function
//         | _, [DStr subject; DStr prefix] ->
//             DBool (Unicode_string.starts_with ~prefix subject)
//         | args ->
//             incorrectArgs ())
//      ; sqlSpec = NotYetImplementedTODO
//      ; previewable = Pure
//      ; deprecated = NotDeprecated }
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
