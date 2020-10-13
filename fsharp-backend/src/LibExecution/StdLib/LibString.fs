module LibExecution.LibString

(* type coerces one list to another using a function *)

// let list_coerce (f: Dval -> Option<'a>) (l: List<Dval>): Result<List<'a>, List<Dval> * Dval> =
//   l
//   |> List.map (fun dv ->
//        match f dv with
//        | Some v -> Result.Ok v
//        | None -> Result.Error(l, dv))
//   |> Result.all
//

// let error_result msg =
//   DResult(ResError(Dval.dstr_of_string_exn msg))
open LibExecution.Runtime
open LibExecution.Runtime.Environment

let fn = FnDesc.stdFnDesc

let fns: List<Environment.BuiltInFn> =
  [ { name = fn "String" "isEmpty" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TBool
      description = "Returns <val true> if <param s> is the empty string <val \"\">."
      fn =
        (function
        | _, [ DStr s ] -> Ok(Plain(DBool(s.Length = 0)))
        | args -> Error FnWrongTypes)
      previewable = Pure
      sqlSpec = NotYetImplementedTODO
      deprecated = NotDeprecated }
    { name = fn "String" "foreach" 0
      parameters =
        [ Param.make "s" TStr "string to iterate over"
          Param.make "f" (TFn([ TChar ], TChar)) "function used to convert one character to another" ]
      returnType = TStr
      description =
        "Iterate over each character (byte, not EGC) in the string, performing the operation in the block on each one"
      fn = (fun _ -> Error FnFunctionRemoved)
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "String" "foreach" 1) } ]
// ; { name = fn "String" "foreach" 1
//
//   ; parameters = [Param.make "s" TStr; func ["character"]]
//   ; returnType = TStr
//   ; description =
//       "Iterate over each Character (EGC, not byte) in the string, performing the operation in the block on each one."
//   ; func =
//       InProcess
//         (function
//         | state, [DStr s; DBlock b] ->
//             let result =
//               Unicode_string.map_characters
//                 ~f:(fun c -> Ast.execute_dblock state b [DCharacter c])
//                 s
//             in
//             ( match
//                 List.find
//                   ~f:(function DIncomplete _ -> true | _ -> false)
//                   result
//               with
//             | Some i ->
//                 i
//             | None ->
//                 result
//                 |> list_coerce ~f:Dval.to_char
//                 >>| String.concat
//                 >>| (fun x -> Dval.dstr_of_string_exn x)
//                 |> Result.map_error ~f:(fun (result, example_value) ->
//                        RT.error
//                          ~actual:(DList result)
//                          ~result:(DList result)
//                          ~long:
//                            ( "String::foreach needs to get chars back in order to reassemble them into a string. The values returned by your code are not chars, for example "
//                            ^ Dval.to_developer_repr_v0 example_value
//                            ^ " is a "
//                            ^ Dval.pretty_tipename example_value )
//                          ~expected:"every value to be a char"
//                          "foreach expects you to return chars")
//                 |> Result.ok_exn )
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "newline" 0
//
//   ; parameters = []
//   ; returnType = TStr
//   ; description = "Returns a string containing a single '\n'"
//   ; func =
//       InProcess (function _ -> DStr (Unicode_string.of_string_exn "\n"))
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "toList" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TList
//   ; description =
//       "Returns the list of characters (byte, not EGC) in the string"
//   ; func =
//       InProcess (fun _ -> Exception.code "This function no longer exists.")
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "toList" 1
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TList
//   ; description =
//       "Returns the list of Characters (EGC, not byte) in the string"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             DList (Unicode_string.map_characters ~f:(fun c -> DCharacter c) s)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "replaceAll" 0
//
//   ; parameters = [Param.make "s" TStr; Param.make "searchFor" TStr; Param.make "replaceWith" TStr]
//   ; returnType = TStr
//   ; description =
//       "Replace all instances on `searchFor` in `s` with `replaceWith`"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s; DStr search; DStr replace] ->
//             DStr (Unicode_string.replace ~search ~replace s)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "toInt" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TInt
//   ; description = "Returns the int value of the string"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DInt (Dint.of_string_exn utf8)
//               with e ->
//                 Exception.code
//                   ~actual:utf8
//                   ~expected:"\\d+"
//                   "Expected a string with only numbers" )
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "toInt" 1
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TResult
//   ; description =
//       "Returns the int value of the string, wrapped in a `Ok`, or `Error <msg>` if the string contains characters other than numeric digits"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DResult (ResOk (DInt (Dint.of_string_exn utf8)))
//               with e ->
//                 error_result
//                   ("Expected a string with only numbers, got " ^ utf8) )
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "toFloat" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TFloat
//   ; description = "Returns the float value of the string"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DFloat (float_of_string utf8)
//               with e ->
//                 Exception.code
//                   ~actual:utf8
//                   "Expected a string representation of an IEEE float" )
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "toFloat" 1
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TResult
//   ; description = "Returns the float value of the string"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             let utf8 = Unicode_string.to_string s in
//             ( try DResult (ResOk (DFloat (float_of_string utf8)))
//               with e ->
//                 error_result
//                   "Expected a string representation of an IEEE float" )
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "toUppercase" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description = "Returns the string, uppercased"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (String.uppercase (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "toUppercase" 1
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description = "Returns the string, uppercased"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] -> DStr (Unicode_string.uppercase s) | args -> fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "toLowercase" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description = "Returns the string, lowercased"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (String.lowercase (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "toLowercase" 1
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description = "Returns the string, lowercased"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] -> DStr (Unicode_string.lowercase s) | args -> fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "length" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TInt
//   ; description = "Returns the length of the string"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dint (String.length (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "length" 1
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TInt
//   ; description = "Returns the length of the string"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dint (Unicode_string.length s)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "append" 0
//       (* This used to provide "++" as an infix op.
//        * It was moved to [String::append_v1] instead,
//        * because we do not yet support versioning infix operators.
//        * We decided this was safe under the assumption that no one should be
//        * (and very likely no one is) relying on broken normalization. *)
//
//   ; parameters = [Param.make "s1" TStr; Param.make "s2" TStr]
//   ; returnType = TStr
//   ; description = "Concatenates the two strings and returns the joined string"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s1; DStr s2] ->
//             (* This implementation does not normalize post-concatenation.
//             * This is a problem because it breaks our guarantees about strings always being normalized;
//             * concatenating two normalized strings does not always result in a normalized string. *)
//             DStr (Unicode_string.append_broken s1 s2)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "append" 1
//   ; infix_names = ["++"]
//   ; parameters = [Param.make "s1" TStr; Param.make "s2" TStr]
//   ; returnType = TStr
//   ; description =
//       "Concatenates the two strings by appending `s2` to `s1` and returns the joined string."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s1; DStr s2] ->
//             DStr (Unicode_string.append s1 s2)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "prepend" 0
//
//   ; parameters = [Param.make "s1" TStr; Param.make "s2" TStr]
//   ; returnType = TStr
//   ; description =
//       "Concatenates the two strings by prepending `s2` to `s1` and returns the joined string."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s1; DStr s2] ->
//             DStr (Unicode_string.append s2 s1)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "slugify" 0
//
//   ; parameters = [Param.make "string" TStr]
//   ; returnType = TStr
//   ; description = "Turns a string into a slug"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             let replace = Unicode_string.regexp_replace in
//             let to_remove = "[^\\w\\s$*_+~.()'\"!\\-:@]" in
//             let trim = "^\\s+|\\s+$" in
//             let spaces = "[-\\s]+" in
//             s
//             |> replace
//                  ~pattern:to_remove
//                  ~replacement:(Unicode_string.of_string_exn "")
//             |> replace
//                  ~pattern:trim
//                  ~replacement:(Unicode_string.of_string_exn "")
//             |> replace
//                  ~pattern:spaces
//                  ~replacement:(Unicode_string.of_string_exn "-")
//             |> Unicode_string.lowercase
//             |> fun s -> DStr s
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "slugify" 1
//
//   ; parameters = [Param.make "string" TStr]
//   ; returnType = TStr
//   ; description = "Turns a string into a slug"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             let replace = Unicode_string.regexp_replace in
//             let to_remove = "[^\\w\\s_-]" in
//             let trim = "^\\s+|\\s+$" in
//             let newspaces = "[-_\\s]+" in
//             s
//             |> replace
//                  ~pattern:to_remove
//                  ~replacement:(Unicode_string.of_string_exn "")
//             |> replace
//                  ~pattern:trim
//                  ~replacement:(Unicode_string.of_string_exn "")
//             |> replace
//                  ~pattern:newspaces
//                  ~replacement:(Unicode_string.of_string_exn "-")
//             |> Unicode_string.lowercase
//             |> fun s -> DStr s
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "slugify" 2
//
//   ; parameters = [Param.make "string" TStr]
//   ; returnType = TStr
//   ; description =
//       "Turns a string into a prettified slug, including only lowercased alphanumeric characters, joined by hyphens"
//   ; func =
//       InProcess
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
//                  ~pattern:to_remove
//                  ~replacement:(Unicode_string.of_string_exn "")
//             |> Unicode_string.trim
//             |> replace
//                  ~pattern:to_be_hyphenated
//                  ~replacement:(Unicode_string.of_string_exn "-")
//             |> Unicode_string.lowercase
//             |> fun s -> DStr s
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "reverse" 0
//
//   ; parameters = [Param.make "string" TStr]
//   ; returnType = TStr
//   ; description = "Reverses `string`"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] -> DStr (Unicode_string.rev s) | args -> fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "split" 0
//
//   ; parameters = [Param.make "s" TStr; Param.make "separator" TStr]
//   ; returnType = TList
//   ; description =
//       "Splits a string at the separator, returning a list of strings without the separator. If the separator is not present, returns a list containing only the initial string."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s; DStr sep] ->
//             s
//             |> Unicode_string.split ~sep
//             |> List.map ~f:(fun str -> DStr str)
//             |> DList
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "join" 0
//
//   ; parameters = [Param.make "l" TList; Param.make "separator" TStr]
//   ; returnType = TStr
//   ; description = "Combines a list of strings with the provided separator"
//   ; func =
//       InProcess
//         (function
//         | _, [DList l; DStr sep] ->
//             let s =
//               List.map
//                 ~f:(fun s ->
//                   match s with
//                   | DStr st ->
//                       st
//                   | _ ->
//                       Exception.code "Expected string")
//                 l
//             in
//             DStr (Unicode_string.concat ~sep s)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "fromList" 0
//
//   ; parameters = [Param.make "l" TList]
//   ; returnType = TStr
//   ; description = "Returns the list of characters as a string"
//   ; func =
//       InProcess (fun _ -> Exception.code "This function no longer exists.")
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "fromList" 1
//
//   ; parameters = [Param.make "l" TList]
//   ; returnType = TStr
//   ; description = "Returns the list of characters as a string"
//   ; func =
//       InProcess
//         (function
//         | _, [DList l] ->
//             DStr
//               ( l
//               |> List.map ~f:(function
//                      | DCharacter c ->
//                          c
//                      | dv ->
//                          RT.error ~actual:dv "expected a char")
//               |> Unicode_string.of_characters )
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "fromChar" 0
//
//   ; parameters = [Param.make "c" TCharacter]
//   ; returnType = TCharacter
//   ; description = "Converts a char to a string"
//   ; func =
//       InProcess (fun _ -> Exception.code "This function no longer exists.")
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "fromChar" 1
//
//   ; parameters = [Param.make "c" TCharacter]
//   ; returnType = TStr
//   ; description = "Converts a char to a string"
//   ; func =
//       InProcess
//         (function
//         | _, [DCharacter c] ->
//             DStr (Unicode_string.of_character c)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "base64Encode" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description =
//       "URLBase64 encodes a string without padding. Uses URL-safe encoding with `-` and `_` instead of `+` and `/`, as defined in RFC 4648 section 5."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (B64.encode
//                  ~alphabet:B64.uri_safe_alphabet
//                  ~pad:false
//                  (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "base64Decode" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description =
//       "Base64 decodes a string. Works with both the URL-safe and standard Base64 alphabets defined in RFC 4648 sections 4 and 5."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//           ( try
//               Dval.dstr_of_string_exn
//                 (B64.decode
//                    ~alphabet:B64.uri_safe_alphabet
//                    (Unicode_string.to_string s))
//             with Not_found_s _ | Caml.Not_found ->
//               ( try
//                   Dval.dstr_of_string_exn
//                     (B64.decode
//                        ~alphabet:B64.default_alphabet
//                        (Unicode_string.to_string s))
//                 with Not_found_s _ | Caml.Not_found ->
//                   RT.error
//                     ~actual:
//                       (Dval.dstr_of_string_exn (Unicode_string.to_string s))
//                     "Not a valid base64 string" ) )
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "digest" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description =
//       "Take a string and hash it to a cryptographically-secure digest.
// Don't rely on either the size or the algorithm."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Libtarget.digest384 (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "sha384" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description =
//       "Take a string and hash it using SHA384. Please use Crypto::sha384 instead."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Libtarget.digest384 (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "sha256" 0
//
//   ; parameters = [Param.make "s" TStr]
//   ; returnType = TStr
//   ; description =
//       "Take a string and hash it using SHA256. Please use Crypto::sha256 instead."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Libtarget.digest256 (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "random" 0
//
//   ; parameters = [Param.make "length" TInt]
//   ; returnType = TStr
//   ; description =
//       "Generate a string of length `length` from random characters."
//   ; func =
//       InProcess
//         (function
//         | _, [DInt l] ->
//             if l < Dint.zero
//             then Exception.code "l should be a positive integer"
//             else
//               Dval.dstr_of_string_exn
//                 (Stdlib_util.random_string (Dint.to_int_exn l))
//         | args ->
//             fail args)
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "random" 1
//
//   ; parameters = [Param.make "length" TInt]
//   ; returnType = TResult
//   ; description =
//       "Generate a string of length `length` from random characters."
//   ; func =
//       InProcess
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
//             fail args)
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "random" 2
//
//   ; parameters = [Param.make "length" TInt]
//   ; returnType = TResult
//   ; description =
//       "Generate a string of length `length` from random characters."
//   ; func =
//       InProcess
//         (function
//         | _, [DInt l] ->
//             if l < Dint.zero
//             then error_result "l should be a positive integer"
//             else
//               Dval.to_res_ok
//                 (Dval.dstr_of_string_exn
//                    (Stdlib_util.random_string (Dint.to_int_exn l)))
//         | args ->
//             fail args)
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "htmlEscape" 0
//
//   ; parameters = [Param.make "html" TStr]
//   ; returnType = TStr
//   ; description =
//       "Escape an untrusted string in order to include it safely in HTML output."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s] ->
//             Dval.dstr_of_string_exn
//               (Stdlib_util.html_escape (Unicode_string.to_string s))
//         | args ->
//             fail args)
//   ; previewable = Impure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "toUUID" 0
//
//   ; parameters = [Param.make "uuid" TStr]
//   ; returnType = TUuid
//   ; description =
//       "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
//   ; func =
//       InProcess
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
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "toUUID" 1
//
//   ; parameters = [Param.make "uuid" TStr]
//   ; returnType = TResult
//   ; description =
//       "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
//   ; func =
//       InProcess
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
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "isSubstring" 0
//
//   ; parameters = [Param.make "searchingFor" TStr; Param.make "lookingIn" TStr]
//   ; returnType = TBool
//   ; description = "Checks if `lookingIn` contains `searchingFor`"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr needle; DStr haystack] ->
//             DBool (Unicode_string.is_substring ~substring:needle haystack)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "isSubstring" 1
//
//   ; parameters = [Param.make "lookingIn" TStr; Param.make "searchingFor" TStr]
//   ; returnType = TBool
//   ; description = "Checks if `lookingIn` contains `searchingFor`"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr haystack; DStr needle] ->
//             DBool (Unicode_string.is_substring ~substring:needle haystack)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "String" "contains" 0
//
//   ; parameters = [Param.make "lookingIn" TStr; Param.make "searchingFor" TStr]
//   ; returnType = TBool
//   ; description = "Checks if `lookingIn` contains `searchingFor`"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr haystack; DStr needle] ->
//             DBool (Unicode_string.is_substring ~substring:needle haystack)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "slice" 0
//
//   ; parameters = [Param.make "string" TStr; Param.make "from" TInt; Param.make "to" TInt]
//   ; returnType = TStr
//   ; description =
//       "Returns the substring of `string` between the `from` and `to` indices.
//        Negative indices start counting from the end of `string`.
//        Indices represent characters."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s; DInt f; DInt l] ->
//             let first, last = (Dint.to_int_exn f, Dint.to_int_exn l) in
//             DStr (Unicode_string.slice s ~first ~last)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "first" 0
//
//   ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//   ; returnType = TStr
//   ; description =
//       "Returns the first `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns `string`.
//       If `characterCount` is negative, returns the empty string."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.first_n s n)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "last" 0
//
//   ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//   ; returnType = TStr
//   ; description =
//       "Returns the last `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns `string`.
//       If `characterCount` is negative, returns the empty string."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.last_n s n)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "dropLast" 0
//
//   ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//   ; returnType = TStr
//   ; description =
//       "Returns all but the last `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns the empty string.
//       If `characterCount` is negative, returns `string`."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.drop_last_n s n)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "dropFirst" 0
//
//   ; parameters = [Param.make "string" TStr; Param.make "characterCount" TInt]
//   ; returnType = TStr
//   ; description =
//       "Returns all but the first `characterCount` characters of `string`, as a String.
//       If `characterCount` is longer than `string`, returns the empty string.
//       If `characterCount` is negative, returns `string`."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr s; DInt n] ->
//             let n = Dint.to_int_exn n in
//             DStr (Unicode_string.drop_first_n s n)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "padStart" 0
//
//   ; parameters = [Param.make "string" TStr; Param.make "padWith" TStr; Param.make "goalLength" TInt]
//   ; returnType = TStr
//   ; description =
//       "If `string` is shorter than `goalLength` characters, returns a copy of `string` starting with enough copies of `padWith` for the result have `goalLength`.
//       If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
//   ; func =
//       InProcess
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
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "padEnd" 0
//
//   ; parameters = [Param.make "string" TStr; Param.make "padWith" TStr; Param.make "goalLength" TInt]
//   ; returnType = TStr
//   ; description =
//       "If `string` is shorter than `goalLength` characters, returns a copy of `string` ending with enough copies of `padWith` for the result have `goalLength`.
//       If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
//   ; func =
//       InProcess
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
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "trim" 0
//
//   ; parameters = [Param.make "str" TStr]
//   ; returnType = TStr
//   ; description =
//       "Returns a copy of `str` with all leading and trailing whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr to_trim] ->
//             DStr (Unicode_string.trim to_trim)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "trimStart" 0
//
//   ; parameters = [Param.make "str" TStr]
//   ; returnType = TStr
//   ; description =
//       "Returns a copy of `str` with all leading whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr to_trim] ->
//             DStr (Unicode_string.trim_start to_trim)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "trimEnd" 0
//
//   ; parameters = [Param.make "str" TStr]
//   ; returnType = TStr
//   ; description =
//       "Returns a copy of `str` with all trailing whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr to_trim] ->
//             DStr (Unicode_string.trim_end to_trim)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "toBytes" 0
//
//   ; parameters = [Param.make "str" TStr]
//   ; returnType = TBytes
//   ; description =
//       "Converts the given unicode string to a utf8-encoded byte sequence."
//   ; func =
//       InProcess
//         (function
//         | _, [DStr str] ->
//             let theBytes = Unicode_string.to_utf8_bytes str in
//             DBytes theBytes
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "startsWith" 0
//
//   ; parameters = [Param.make "subject" TStr; Param.make "prefix" TStr]
//   ; returnType = TBool
//   ; description = "Checks if `subject` starts with `prefix`"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr subject; DStr prefix] ->
//             DBool (Unicode_string.starts_with ~prefix subject)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
// ; { name = fn "String" "endsWith" 0
//
//   ; parameters = [Param.make "subject" TStr; Param.make "suffix" TStr]
//   ; returnType = TBool
//   ; description = "Checks if `subject` ends with `suffix`"
//   ; func =
//       InProcess
//         (function
//         | _, [DStr subject; DStr suffix] ->
//             DBool (Unicode_string.ends_with ~suffix subject)
//         | args ->
//             fail args)
//   ; previewable = Pure
//   ; deprecated = NotDeprecated }
