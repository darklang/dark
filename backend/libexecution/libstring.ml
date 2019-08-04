open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

(* type coerces one list to another using a function *)
let list_coerce ~(f : dval -> 'a option) (l : dval list) :
    ('a list, dval list * dval) Result.t =
  l
  |> List.map ~f:(fun dv ->
         match f dv with Some v -> Result.Ok v | None -> Result.Error (l, dv)
     )
  |> Result.all


let error_result msg = DResult (ResError (Dval.dstr_of_string_exn msg))

let ( >>| ) = Result.( >>| )

let fns : Lib.shortfn list =
  [ { pns = ["String::foreach"]
    ; ins = []
    ; p = [par "s" TStr; func ["char"]]
    ; r = TStr
    ; d =
        "Iterate over each character (byte, not EGC) in the string, performing the operation in the block on each one"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["String::foreach_v1"]
    ; ins = []
    ; p = [par "s" TStr; func ["character"]]
    ; r = TStr
    ; d =
        "Iterate over each character (EGC, not byte) in the string, performing the operation in the block on each one"
    ; f =
        InProcess
          (function
          | _, [DStr s; DBlock fn] ->
              let result =
                Unicode_string.map_characters ~f:(fun c -> fn [DCharacter c]) s
              in
              if List.exists ~f:(( = ) DIncomplete) result
              then DIncomplete
              else
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
                           ^ Dval.tipename example_value )
                         ~expected:"every value to be a char"
                         "foreach expects you to return chars" )
                |> Result.ok_exn
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::newline"]
    ; ins = []
    ; p = []
    ; r = TStr
    ; d = "Returns a string containing a single '\n'"
    ; f = InProcess (function _ -> DStr (Unicode_string.of_string_exn "\n"))
    ; ps = true
    ; dep = false }
  ; { pns = ["String::toList"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TList
    ; d = "Returns the list of characters (byte, not EGC) in the string"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["String::toList_v1"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TList
    ; d = "Returns the list of characters (EGC, not byte) in the string"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              DList
                (Unicode_string.map_characters ~f:(fun c -> DCharacter c) s)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::replaceAll"]
    ; ins = []
    ; p = [par "s" TStr; par "searchFor" TStr; par "replaceWith" TStr]
    ; r = TList
    ; d = "Replace all instances on `searchFor` in `s` with `replaceWith`"
    ; f =
        InProcess
          (function
          | _, [DStr s; DStr search; DStr replace] ->
              DStr (Unicode_string.replace ~search ~replace s)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::toInt"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TInt
    ; d = "Returns the int value of the string"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              let utf8 = Unicode_string.to_string s in
              ( try DInt (Dint.of_string_exn utf8) with e ->
                  Exception.code
                    ~actual:utf8
                    ~expected:"\\d+"
                    "Expected a string with only numbers" )
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["String::toInt_v1"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TResult
    ; d =
        "Returns the int value of the string, wrapped in a `Ok`, or `Error <msg>` if the string contains characters other than numeric digits"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["String::toFloat"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TFloat
    ; d = "Returns the float value of the string"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              let utf8 = Unicode_string.to_string s in
              ( try DFloat (float_of_string utf8) with e ->
                  Exception.code
                    ~actual:utf8
                    "Expected a string representation of an IEEE float" )
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["String::toFloat_v1"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TResult
    ; d = "Returns the float value of the string"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              let utf8 = Unicode_string.to_string s in
              ( try DResult (ResOk (DFloat (float_of_string utf8))) with e ->
                  error_result
                    "Expected a string representation of an IEEE float" )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::toUppercase"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Returns the string, uppercased"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (String.uppercase (Unicode_string.to_string s))
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["String::toUppercase_v1"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Returns the string, uppercased"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              DStr (Unicode_string.uppercase s)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::toLowercase"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Returns the string, lowercased"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (String.lowercase (Unicode_string.to_string s))
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["String::toLowercase_v1"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Returns the string, lowercased"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              DStr (Unicode_string.lowercase s)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::length"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TInt
    ; d = "Returns the length of the string"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dint (String.length (Unicode_string.to_string s))
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["String::length_v1"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TInt
    ; d = "Returns the length of the string"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dint (Unicode_string.length s)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::append"]
    ; ins = ["++"]
    ; p = [par "s1" TStr; par "s2" TStr]
    ; r = TStr
    ; d = "Concatenates the two strings and returns the joined string"
    ; f =
        InProcess
          (function
          | _, [DStr s1; DStr s2] ->
              DStr (Unicode_string.append s1 s2)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::slugify"]
    ; ins = []
    ; p = [par "string" TStr]
    ; r = TStr
    ; d = "Turns a string into a slug"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["String::reverse"]
    ; ins = []
    ; p = [par "string" TStr]
    ; r = TStr
    ; d = "Reverses `string`"
    ; f =
        InProcess
          (function
          | _, [DStr s] -> DStr (Unicode_string.rev s) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::split"]
    ; ins = []
    ; p = [par "s" TStr; par "separator" TStr]
    ; r = TList
    ; d =
        "Splits a string at the separator, returning a list of strings without the separator. If the separator is not present, returns a list containing only the initial string."
    ; f =
        InProcess
          (function
          | _, [DStr s; DStr sep] ->
              s
              |> Unicode_string.split ~sep
              |> List.map ~f:(fun str -> DStr str)
              |> DList
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::join"]
    ; ins = []
    ; p = [par "l" TList; par "separator" TStr]
    ; r = TStr
    ; d = "Combines a list of strings with the provided separator"
    ; f =
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
                        Exception.code "Expected string" )
                  l
              in
              DStr (Unicode_string.concat ~sep s)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::fromList"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TStr
    ; d = "Returns the list of characters as a string"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["String::fromList_v1"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TStr
    ; d = "Returns the list of characters as a string"
    ; f =
        InProcess
          (function
          | _, [DList l] ->
              DStr
                ( l
                |> List.map ~f:(function
                       | DCharacter c ->
                           c
                       | dv ->
                           RT.error ~actual:dv "expected a char" )
                |> Unicode_string.of_characters )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::fromChar"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TCharacter
    ; d = "Converts a char to a string"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["String::fromChar_v1"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TStr
    ; d = "Converts a char to a string"
    ; f =
        InProcess
          (function
          | _, [DCharacter c] ->
              DStr (Unicode_string.of_character c)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::base64Encode"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Base64 encodes a string. Uses URL-safe encoding."
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["String::base64Decode"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Base64 decodes a string."
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
            ( try
                Dval.dstr_of_string_exn
                  (B64.decode
                     ~alphabet:B64.uri_safe_alphabet
                     (Unicode_string.to_string s))
              with
            | Not_found_s _ | Caml.Not_found ->
              ( try
                  Dval.dstr_of_string_exn
                    (B64.decode
                       ~alphabet:B64.default_alphabet
                       (Unicode_string.to_string s))
                with
              | Not_found_s _ | Caml.Not_found ->
                  RT.error
                    ~actual:
                      (Dval.dstr_of_string_exn (Unicode_string.to_string s))
                    "Not a valid base64 string" ) )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::digest"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d =
        "Take a string and hash it to a cryptographically-secure digest.
  Don't rely on either the size or the algorithm."
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Libtarget.digest384 (Unicode_string.to_string s))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::sha384"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Take a string and hash it using SHA384."
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Libtarget.digest384 (Unicode_string.to_string s))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::sha256"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TStr
    ; d = "Take a string and hash it using SHA256."
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Libtarget.digest256 (Unicode_string.to_string s))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["String::random"]
    ; ins = []
    ; p = [par "length" TInt]
    ; r = TStr
    ; d = "Generate a string of length `length` from random characters."
    ; f =
        InProcess
          (function
          | _, [DInt l] ->
              if l < Dint.zero
              then Exception.code "l should be a positive integer"
              else
                Dval.dstr_of_string_exn
                  (Util.random_string (Dint.to_int_exn l))
          | args ->
              fail args)
    ; ps = false
    ; dep = true }
  ; { pns = ["String::random_v1"]
    ; ins = []
    ; p = [par "length" TInt]
    ; r = TResult
    ; d = "Generate a string of length `length` from random characters."
    ; f =
        InProcess
          (function
          | _, [DInt l] ->
              if l < Dint.zero
              then error_result "l should be a positive integer"
              else
                DResult
                  (ResOk
                     (Dval.dstr_of_string_exn
                        (Util.random_string (Dint.to_int_exn l))))
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["String::htmlEscape"]
    ; ins = []
    ; p = [par "html" TStr]
    ; r = TStr
    ; d =
        "Escape an untrusted string in order to include it safely in HTML output."
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
              Dval.dstr_of_string_exn
                (Util.html_escape (Unicode_string.to_string s))
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["String::toUUID"]
    ; ins = []
    ; p = [par "uuid" TStr]
    ; r = TUuid
    ; d =
        "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
    ; f =
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
    ; ps = true
    ; dep = true }
  ; { pns = ["String::toUUID_v1"]
    ; ins = []
    ; p = [par "uuid" TStr]
    ; r = TResult
    ; d =
        "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from the input `uuid` string"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["String::isSubstring"]
    ; ins = []
    ; p = [par "searchingFor" TStr; par "lookingIn" TStr]
    ; r = TBool
    ; d = "Checks if `lookingIn` contains `searchingFor`"
    ; f =
        InProcess
          (function
          | _, [DStr needle; DStr haystack] ->
              DBool (Unicode_string.is_substring ~substring:needle haystack)
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
