open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

(* type coerces one list to another using a function *)
let list_coerce ~(f : 'expr_type dval -> 'a option) (l : 'expr_type dval list) :
    ('a list, 'expr_type dval list * 'expr_type dval) Result.t =
  l
  |> List.map ~f:(fun dv ->
         match f dv with Some v -> Result.Ok v | None -> Result.Error (l, dv))
  |> Result.all


let error_result msg = DResult (ResError (Dval.dstr_of_string_exn msg))

let ( >>| ) = Result.( >>| )

let fns : Types.fluid_expr fn list =
  [ { prefix_names = ["String::isEmpty"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TBool
    ; description = "Returns `true` if `s` is the empty string \"\"."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              DBool (Unicode_string.length s = 0)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::foreach"]
    ; infix_names = []
    ; parameters = [par "s" TStr; func ["char"]]
    ; return_type = TStr
    ; description =
        "Iterate over each character (byte, not EGC) in the string, performing the operation in the block on each one"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::foreach_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr; func ["character"]]
    ; return_type = TStr
    ; description =
        "Iterate over each Character (EGC, not byte) in the string, performing the operation in the block on each one."
    ; func =
        InProcess
          (function
          | state, [DStr s; DBlock b] ->
              let result =
                Unicode_string.map_characters
                  ~f:(fun c -> Ast.execute_dblock state b [DCharacter c])
                  s
              in
              ( match
                  List.find
                    ~f:(function DIncomplete _ -> true | _ -> false)
                    result
                with
              | Some i ->
                  i
              | None ->
                  result
                  |> list_coerce ~f:Dval.to_char
                  >>| String.concat
                  >>| (fun x -> Dval.dstr_of_string_exn x)
                  |> Result.map_error ~f:(fun (result, example_value) ->
                         RT.error
                           ~actual:(DList result)
                           ~result:(DList result)
                           ~long:
                             ( "String::foreach needs to get chars back in order to reassemble them into a string. The values returned by your code are not chars, for example "
                             ^ Dval.to_developer_repr_v0 example_value
                             ^ " is a "
                             ^ Dval.pretty_tipename example_value )
                           ~expected:"every value to be a char"
                           "foreach expects you to return chars")
                  |> Result.ok_exn )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::newline"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TStr
    ; description = "Returns a string containing a single '\n'"
    ; func =
        InProcess (function _ -> DStr (Unicode_string.of_string_exn "\n"))
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::toList"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TList
    ; description =
        "Returns the list of characters (byte, not EGC) in the string"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::toList_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TList
    ; description =
        "Returns the list of Characters (EGC, not byte) in the string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              DList (Unicode_string.map_characters ~f:(fun c -> DCharacter c) s)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::replaceAll"]
    ; infix_names = []
    ; parameters = [par "s" TStr; par "searchFor" TStr; par "replaceWith" TStr]
    ; return_type = TStr
    ; description =
        "Replace all instances on `searchFor` in `s` with `replaceWith`"
    ; func =
        InProcess
          (function
          | _, [DStr s; DStr search; DStr replace] ->
              DStr (Unicode_string.replace ~search ~replace s)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::toInt"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TInt
    ; description = "Returns the int value of the string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              let utf8 = Unicode_string.to_string s in
              ( try DInt (Dint.of_string_exn utf8)
                with e ->
                  Exception.code
                    ~actual:utf8
                    ~expected:"\\d+"
                    "Expected a string with only numbers" )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::toInt_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TResult
    ; description =
        "Returns the int value of the string, wrapped in a `Ok`, or `Error <msg>` if the string contains characters other than numeric digits"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              let utf8 = Unicode_string.to_string s in
              ( try DResult (ResOk (DInt (Dint.of_string_exn utf8)))
                with e ->
                  error_result
                    ("Expected a string with only numbers, got " ^ utf8) )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::toFloat"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TFloat
    ; description = "Returns the float value of the string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              let utf8 = Unicode_string.to_string s in
              ( try DFloat (float_of_string utf8)
                with e ->
                  Exception.code
                    ~actual:utf8
                    "Expected a string representation of an IEEE float" )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::toFloat_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TResult
    ; description = "Returns the float value of the string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              let utf8 = Unicode_string.to_string s in
              ( try DResult (ResOk (DFloat (float_of_string utf8)))
                with e ->
                  error_result
                    "Expected a string representation of an IEEE float" )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::toUppercase"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description = "Returns the string, uppercased"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (String.uppercase (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::toUppercase_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description = "Returns the string, uppercased"
    ; func =
        InProcess
          (function
          | _, [DStr s] -> DStr (Unicode_string.uppercase s) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::toLowercase"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description = "Returns the string, lowercased"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (String.lowercase (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::toLowercase_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description = "Returns the string, lowercased"
    ; func =
        InProcess
          (function
          | _, [DStr s] -> DStr (Unicode_string.lowercase s) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::length"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TInt
    ; description = "Returns the length of the string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dint (String.length (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::length_v1"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TInt
    ; description = "Returns the length of the string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dint (Unicode_string.length s)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names =
        ["String::append"]
        (* This used to provide "++" as an infix op.
         * It was moved to [String::append_v1] instead,
         * because we do not yet support versioning infix operators.
         * We decided this was safe under the assumption that no one should be
         * (and very likely no one is) relying on broken normalization. *)
    ; infix_names = []
    ; parameters = [par "s1" TStr; par "s2" TStr]
    ; return_type = TStr
    ; description = "Concatenates the two strings and returns the joined string"
    ; func =
        InProcess
          (function
          | _, [DStr s1; DStr s2] ->
              (* This implementation does not normalize post-concatenation.
              * This is a problem because it breaks our guarantees about strings always being normalized;
              * concatenating two normalized strings does not always result in a normalized string. *)
              DStr (Unicode_string.append_broken s1 s2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::append_v1"]
    ; infix_names = ["++"]
    ; parameters = [par "s1" TStr; par "s2" TStr]
    ; return_type = TStr
    ; description =
        "Concatenates the two strings by appending `s2` to `s1` and returns the joined string."
    ; func =
        InProcess
          (function
          | _, [DStr s1; DStr s2] ->
              DStr (Unicode_string.append s1 s2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::prepend"]
    ; infix_names = []
    ; parameters = [par "s1" TStr; par "s2" TStr]
    ; return_type = TStr
    ; description =
        "Concatenates the two strings by prepending `s2` to `s1` and returns the joined string."
    ; func =
        InProcess
          (function
          | _, [DStr s1; DStr s2] ->
              DStr (Unicode_string.append s2 s1)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::slugify"]
    ; infix_names = []
    ; parameters = [par "string" TStr]
    ; return_type = TStr
    ; description = "Turns a string into a slug"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              let replace = Unicode_string.regexp_replace in
              let to_remove = "[^\\w\\s$*_+~.()'\"!\\-:@]" in
              let trim = "^\\s+|\\s+$" in
              let spaces = "[-\\s]+" in
              s
              |> replace
                   ~pattern:to_remove
                   ~replacement:(Unicode_string.of_string_exn "")
              |> replace
                   ~pattern:trim
                   ~replacement:(Unicode_string.of_string_exn "")
              |> replace
                   ~pattern:spaces
                   ~replacement:(Unicode_string.of_string_exn "-")
              |> Unicode_string.lowercase
              |> fun s -> DStr s
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::slugify_v1"]
    ; infix_names = []
    ; parameters = [par "string" TStr]
    ; return_type = TStr
    ; description = "Turns a string into a slug"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              let replace = Unicode_string.regexp_replace in
              let to_remove = "[^\\w\\s_-]" in
              let trim = "^\\s+|\\s+$" in
              let newspaces = "[-_\\s]+" in
              s
              |> replace
                   ~pattern:to_remove
                   ~replacement:(Unicode_string.of_string_exn "")
              |> replace
                   ~pattern:trim
                   ~replacement:(Unicode_string.of_string_exn "")
              |> replace
                   ~pattern:newspaces
                   ~replacement:(Unicode_string.of_string_exn "-")
              |> Unicode_string.lowercase
              |> fun s -> DStr s
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::slugify_v2"]
    ; infix_names = []
    ; parameters = [par "string" TStr]
    ; return_type = TStr
    ; description =
        "Turns a string into a prettified slug, including only lowercased alphanumeric characters, joined by hyphens"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              (* Should work the same as https://blog.tersmitten.nl/slugify/ *)
              let replace = Unicode_string.regexp_replace in
              (* explicitly limit to (roman) alphanumeric for pretty urls *)
              let to_remove = "[^a-z0-9\\s_-]+" in
              let to_be_hyphenated = "[-_\\s]+" in
              s
              |> Unicode_string.lowercase
              |> replace
                   ~pattern:to_remove
                   ~replacement:(Unicode_string.of_string_exn "")
              |> Unicode_string.trim
              |> replace
                   ~pattern:to_be_hyphenated
                   ~replacement:(Unicode_string.of_string_exn "-")
              |> Unicode_string.lowercase
              |> fun s -> DStr s
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::reverse"]
    ; infix_names = []
    ; parameters = [par "string" TStr]
    ; return_type = TStr
    ; description = "Reverses `string`"
    ; func =
        InProcess
          (function
          | _, [DStr s] -> DStr (Unicode_string.rev s) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::split"]
    ; infix_names = []
    ; parameters = [par "s" TStr; par "separator" TStr]
    ; return_type = TList
    ; description =
        "Splits a string at the separator, returning a list of strings without the separator. If the separator is not present, returns a list containing only the initial string."
    ; func =
        InProcess
          (function
          | _, [DStr s; DStr sep] ->
              s
              |> Unicode_string.split ~sep
              |> List.map ~f:(fun str -> DStr str)
              |> DList
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::join"]
    ; infix_names = []
    ; parameters = [par "l" TList; par "separator" TStr]
    ; return_type = TStr
    ; description = "Combines a list of strings with the provided separator"
    ; func =
        InProcess
          (function
          | _, [DList l; DStr sep] ->
              let s =
                List.map
                  ~f:(fun s ->
                    match s with
                    | DStr st ->
                        st
                    | _ ->
                        Exception.code "Expected string")
                  l
              in
              DStr (Unicode_string.concat ~sep s)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::fromList"]
    ; infix_names = []
    ; parameters = [par "l" TList]
    ; return_type = TStr
    ; description = "Returns the list of characters as a string"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::fromList_v1"]
    ; infix_names = []
    ; parameters = [par "l" TList]
    ; return_type = TStr
    ; description = "Returns the list of characters as a string"
    ; func =
        InProcess
          (function
          | _, [DList l] ->
              DStr
                ( l
                |> List.map ~f:(function
                       | DCharacter c ->
                           c
                       | dv ->
                           RT.error ~actual:dv "expected a char")
                |> Unicode_string.of_characters )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::fromChar"]
    ; infix_names = []
    ; parameters = [par "c" TCharacter]
    ; return_type = TCharacter
    ; description = "Converts a char to a string"
    ; func =
        InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::fromChar_v1"]
    ; infix_names = []
    ; parameters = [par "c" TCharacter]
    ; return_type = TStr
    ; description = "Converts a char to a string"
    ; func =
        InProcess
          (function
          | _, [DCharacter c] ->
              DStr (Unicode_string.of_character c)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::base64Encode"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description =
        "URLBase64 encodes a string without padding. Uses URL-safe encoding with `-` and `_` instead of `+` and `/`, as defined in RFC 4648 section 5."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (B64.encode
                   ~alphabet:B64.uri_safe_alphabet
                   ~pad:false
                   (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::base64Decode"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64 alphabets defined in RFC 4648 sections 4 and 5."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
            ( try
                Dval.dstr_of_string_exn
                  (B64.decode
                     ~alphabet:B64.uri_safe_alphabet
                     (Unicode_string.to_string s))
              with Not_found_s _ | Caml.Not_found ->
                ( try
                    Dval.dstr_of_string_exn
                      (B64.decode
                         ~alphabet:B64.default_alphabet
                         (Unicode_string.to_string s))
                  with Not_found_s _ | Caml.Not_found ->
                    RT.error
                      ~actual:
                        (Dval.dstr_of_string_exn (Unicode_string.to_string s))
                      "Not a valid base64 string" ) )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::digest"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description =
        "Take a string and hash it to a cryptographically-secure digest.
  Don't rely on either the size or the algorithm."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Libtarget.digest384 (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::sha384"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description =
        "Take a string and hash it using SHA384. Please use Crypto::sha384 instead."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Libtarget.digest384 (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::sha256"]
    ; infix_names = []
    ; parameters = [par "s" TStr]
    ; return_type = TStr
    ; description =
        "Take a string and hash it using SHA256. Please use Crypto::sha256 instead."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Libtarget.digest256 (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::random"]
    ; infix_names = []
    ; parameters = [par "length" TInt]
    ; return_type = TStr
    ; description =
        "Generate a string of length `length` from random characters."
    ; func =
        InProcess
          (function
          | _, [DInt l] ->
              if l < Dint.zero
              then Exception.code "l should be a positive integer"
              else
                Dval.dstr_of_string_exn
                  (Stdlib_util.random_string (Dint.to_int_exn l))
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["String::random_v1"]
    ; infix_names = []
    ; parameters = [par "length" TInt]
    ; return_type = TResult
    ; description =
        "Generate a string of length `length` from random characters."
    ; func =
        InProcess
          (function
          | _, [DInt l] ->
              if l < Dint.zero
              then error_result "l should be a positive integer"
              else
                DResult
                  (ResOk
                     (Dval.dstr_of_string_exn
                        (Stdlib_util.random_string (Dint.to_int_exn l))))
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["String::random_v2"]
    ; infix_names = []
    ; parameters = [par "length" TInt]
    ; return_type = TResult
    ; description =
        "Generate a string of length `length` from random characters."
    ; func =
        InProcess
          (function
          | _, [DInt l] ->
              if l < Dint.zero
              then error_result "l should be a positive integer"
              else
                Dval.to_res_ok
                  (Dval.dstr_of_string_exn
                     (Stdlib_util.random_string (Dint.to_int_exn l)))
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["String::htmlEscape"]
    ; infix_names = []
    ; parameters = [par "html" TStr]
    ; return_type = TStr
    ; description =
        "Escape an untrusted string in order to include it safely in HTML output."
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Stdlib_util.html_escape (Unicode_string.to_string s))
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["String::toUUID"]
    ; infix_names = []
    ; parameters = [par "uuid" TStr]
    ; return_type = TUuid
    ; description =
        "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
            ( match Uuidm.of_string (Unicode_string.to_string s) with
            | Some id ->
                DUuid id
            | None ->
                Exception.code
                  "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
            )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::toUUID_v1"]
    ; infix_names = []
    ; parameters = [par "uuid" TStr]
    ; return_type = TResult
    ; description =
        "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
    ; func =
        InProcess
          (function
          | _, [DStr s] ->
            ( match Uuidm.of_string (Unicode_string.to_string s) with
            | Some id ->
                DResult (ResOk (DUuid id))
            | None ->
                error_result
                  "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX"
            )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::isSubstring"]
    ; infix_names = []
    ; parameters = [par "searchingFor" TStr; par "lookingIn" TStr]
    ; return_type = TBool
    ; description = "Checks if `lookingIn` contains `searchingFor`"
    ; func =
        InProcess
          (function
          | _, [DStr needle; DStr haystack] ->
              DBool (Unicode_string.is_substring ~substring:needle haystack)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::isSubstring_v1"]
    ; infix_names = []
    ; parameters = [par "lookingIn" TStr; par "searchingFor" TStr]
    ; return_type = TBool
    ; description = "Checks if `lookingIn` contains `searchingFor`"
    ; func =
        InProcess
          (function
          | _, [DStr haystack; DStr needle] ->
              DBool (Unicode_string.is_substring ~substring:needle haystack)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["String::contains"]
    ; infix_names = []
    ; parameters = [par "lookingIn" TStr; par "searchingFor" TStr]
    ; return_type = TBool
    ; description = "Checks if `lookingIn` contains `searchingFor`"
    ; func =
        InProcess
          (function
          | _, [DStr haystack; DStr needle] ->
              DBool (Unicode_string.is_substring ~substring:needle haystack)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::slice"]
    ; infix_names = []
    ; parameters = [par "string" TStr; par "from" TInt; par "to" TInt]
    ; return_type = TStr
    ; description =
        "Returns the substring of `string` between the `from` and `to` indices.
         Negative indices start counting from the end of `string`.
         Indices represent characters."
    ; func =
        InProcess
          (function
          | _, [DStr s; DInt f; DInt l] ->
              let first, last = (Dint.to_int_exn f, Dint.to_int_exn l) in
              DStr (Unicode_string.slice s ~first ~last)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::first"]
    ; infix_names = []
    ; parameters = [par "string" TStr; par "characterCount" TInt]
    ; return_type = TStr
    ; description =
        "Returns the first `characterCount` characters of `string`, as a String.
        If `characterCount` is longer than `string`, returns `string`.
        If `characterCount` is negative, returns the empty string."
    ; func =
        InProcess
          (function
          | _, [DStr s; DInt n] ->
              let n = Dint.to_int_exn n in
              DStr (Unicode_string.first_n s n)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::last"]
    ; infix_names = []
    ; parameters = [par "string" TStr; par "characterCount" TInt]
    ; return_type = TStr
    ; description =
        "Returns the last `characterCount` characters of `string`, as a String.
        If `characterCount` is longer than `string`, returns `string`.
        If `characterCount` is negative, returns the empty string."
    ; func =
        InProcess
          (function
          | _, [DStr s; DInt n] ->
              let n = Dint.to_int_exn n in
              DStr (Unicode_string.last_n s n)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::dropLast"]
    ; infix_names = []
    ; parameters = [par "string" TStr; par "characterCount" TInt]
    ; return_type = TStr
    ; description =
        "Returns all but the last `characterCount` characters of `string`, as a String.
        If `characterCount` is longer than `string`, returns the empty string.
        If `characterCount` is negative, returns `string`."
    ; func =
        InProcess
          (function
          | _, [DStr s; DInt n] ->
              let n = Dint.to_int_exn n in
              DStr (Unicode_string.drop_last_n s n)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::dropFirst"]
    ; infix_names = []
    ; parameters = [par "string" TStr; par "characterCount" TInt]
    ; return_type = TStr
    ; description =
        "Returns all but the first `characterCount` characters of `string`, as a String.
        If `characterCount` is longer than `string`, returns the empty string.
        If `characterCount` is negative, returns `string`."
    ; func =
        InProcess
          (function
          | _, [DStr s; DInt n] ->
              let n = Dint.to_int_exn n in
              DStr (Unicode_string.drop_first_n s n)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::padStart"]
    ; infix_names = []
    ; parameters = [par "string" TStr; par "padWith" TStr; par "goalLength" TInt]
    ; return_type = TStr
    ; description =
        "If `string` is shorter than `goalLength` characters, returns a copy of `string` starting with enough copies of `padWith` for the result have `goalLength`.
        If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
    ; func =
        InProcess
          (function
          | state, [DStr s; DStr pad_with; DInt l] ->
              let padLen = Unicode_string.length pad_with in
              if padLen = 1
              then
                let l = Dint.to_int_exn l in
                DStr (Unicode_string.pad_start s ~pad_with l)
              else
                DError
                  ( SourceNone
                  , "Expected the argument `padWith` passed to `"
                    ^ state.executing_fnname
                    ^ "` to be one character long. However, `"
                    ^ Dval.to_developer_repr_v0 (DStr pad_with)
                    ^ "` is "
                    ^ Int.to_string padLen
                    ^ " characters long." )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::padEnd"]
    ; infix_names = []
    ; parameters = [par "string" TStr; par "padWith" TStr; par "goalLength" TInt]
    ; return_type = TStr
    ; description =
        "If `string` is shorter than `goalLength` characters, returns a copy of `string` ending with enough copies of `padWith` for the result have `goalLength`.
        If the `string` is longer than `goalLength`, returns an unchanged copy of `string`."
    ; func =
        InProcess
          (function
          | state, [DStr s; DStr pad_with; DInt l] ->
              let padLen = Unicode_string.length pad_with in
              if padLen = 1
              then
                let l = Dint.to_int_exn l in
                DStr (Unicode_string.pad_end s ~pad_with l)
              else
                DError
                  ( SourceNone
                  , "Expected the argument `padWith` passed to `"
                    ^ state.executing_fnname
                    ^ "` to be one character long. However, `"
                    ^ Dval.to_developer_repr_v0 (DStr pad_with)
                    ^ "` is "
                    ^ Int.to_string padLen
                    ^ " characters long." )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::trim"]
    ; infix_names = []
    ; parameters = [par "str" TStr]
    ; return_type = TStr
    ; description =
        "Returns a copy of `str` with all leading and trailing whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
    ; func =
        InProcess
          (function
          | _, [DStr to_trim] ->
              DStr (Unicode_string.trim to_trim)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::trimStart"]
    ; infix_names = []
    ; parameters = [par "str" TStr]
    ; return_type = TStr
    ; description =
        "Returns a copy of `str` with all leading whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
    ; func =
        InProcess
          (function
          | _, [DStr to_trim] ->
              DStr (Unicode_string.trim_start to_trim)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::trimEnd"]
    ; infix_names = []
    ; parameters = [par "str" TStr]
    ; return_type = TStr
    ; description =
        "Returns a copy of `str` with all trailing whitespace removed. 'whitespace' here means all Unicode characters with the `White_Space` property, which includes \" \", \"\\t\" and \"\\n\"."
    ; func =
        InProcess
          (function
          | _, [DStr to_trim] ->
              DStr (Unicode_string.trim_end to_trim)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::toBytes"]
    ; infix_names = []
    ; parameters = [par "str" TStr]
    ; return_type = TBytes
    ; description =
        "Converts the given unicode string to a utf8-encoded byte sequence."
    ; func =
        InProcess
          (function
          | _, [DStr str] ->
              let theBytes = Unicode_string.to_utf8_bytes str in
              DBytes theBytes
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::startsWith"]
    ; infix_names = []
    ; parameters = [par "subject" TStr; par "prefix" TStr]
    ; return_type = TBool
    ; description = "Checks if `subject` starts with `prefix`"
    ; func =
        InProcess
          (function
          | _, [DStr subject; DStr prefix] ->
              DBool (Unicode_string.starts_with ~prefix subject)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["String::endsWith"]
    ; infix_names = []
    ; parameters = [par "subject" TStr; par "suffix" TStr]
    ; return_type = TBool
    ; description = "Checks if `subject` ends with `suffix`"
    ; func =
        InProcess
          (function
          | _, [DStr subject; DStr suffix] ->
              DBool (Unicode_string.ends_with ~suffix subject)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false } ]
