open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let list_repeat = Util.list_repeat

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
  [ (* ====================================== *)
    (* Dict  *)
    (* ====================================== *)
    { pns = ["Dict::keys"]
    ; ins = []
    ; p = [par "dict" TObj]
    ; r = TList
    ; d = "Return the dictionary's keys"
    ; f =
        InProcess
          (function
          | _, [DObj o] ->
              o
              |> DvalMap.keys
              |> List.map ~f:(fun k -> Dval.dstr_of_string_exn k)
              |> fun l -> DList l
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::values"]
    ; ins = []
    ; p = [par "dict" TObj]
    ; r = TList
    ; d = "Return the dictionary's values"
    ; f =
        InProcess
          (function
          | _, [DObj o] -> DList (DvalMap.values o) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::get"]
    ; ins = []
    ; p = [par "dict" TObj; par "key" TStr]
    ; r = TAny
    ; d =
        "Looks up `key` in object `dict` and returns the value if found, and Error otherwise"
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
            ( match DvalMap.get o (Unicode_string.to_string s) with
            | Some d ->
                d
            | None ->
                DNull )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::get_v1"]
    ; ins = []
    ; p = [par "dict" TObj; par "key" TStr]
    ; r = TOption
    ; d = "Looks up `key` in object `dict` and returns an option"
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
            ( match DvalMap.get o (Unicode_string.to_string s) with
            | Some d ->
                DOption (OptJust d)
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::foreach"]
    ; ins = []
    ; p = [par "dict" TObj; func ["value"]]
    ; r = TObj
    ; d =
        "Iterates each `value` in object `dict` and mutates it according to the provided lambda"
    ; f =
        InProcess
          (function
          | _, [DObj o; DBlock fn] ->
              let f (dv : dval) : dval = fn [dv] in
              DObj (Map.map ~f o)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Objects *)
    (* ====================================== *)
    { pns = ["Object::empty"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Return an empty object"
    ; f =
        InProcess (function _, [] -> DObj DvalMap.empty | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Object::merge"]
    ; ins = []
    ; p = [par "left" TObj; par "right" TObj]
    ; r = TObj
    ; d =
        "Return a combined object with both objects' keys and values. If the same key exists in both `left` and `right`, then use the value from `right`"
    ; f =
        InProcess
          (function
          | _, [DObj l; DObj r] ->
              DObj (Util.merge_right l r)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["assoc"]
    ; ins = []
    ; p = [par "obj" TObj; par "key" TStr; par "val" TAny]
    ; r = TObj
    ; d = "Return a copy of `obj` with the `key` set to `val`."
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr k; v] ->
              DObj (Map.set o ~key:(Unicode_string.to_string k) ~data:v)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["dissoc"]
    ; ins = []
    ; p = [par "obj" TObj; par "key" TStr]
    ; r = TObj
    ; d = "Return a copy of `obj` with `key` unset."
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr k] ->
              DObj (Map.remove o (Unicode_string.to_string k))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["toForm"]
    ; ins = []
    ; p = [par "obj" TObj; par "submit" TStr]
    ; r = TStr
    ; d =
        "For demonstration only. Returns a HTML form with the labels and types described in `obj`. `submit` is the form's action."
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr uri] ->
              let fmt =
                format_of_string
                  "<form action=\"%s\" method=\"post\">\n%s\n<input type=\"submit\" value=\"Save\">\n</form>"
              in
              let to_input (k, v) =
                let label =
                  Printf.sprintf "<label for=\"%s\">%s:</label>" k k
                in
                let input =
                  Printf.sprintf
                    "<input id=\"%s\" type=\"text\" name=\"%s\">"
                    k
                    k
                in
                label ^ "\n" ^ input
              in
              let inputs =
                o
                |> Map.to_alist
                |> List.map ~f:to_input
                |> String.concat ~sep:"\n"
              in
              Dval.dstr_of_string_exn
                (Printf.sprintf fmt (Unicode_string.to_string uri) inputs)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Object::toJSON"]
    ; ins = []
    ; p = [par "obj" TObj]
    ; r = TStr
    ; d = "Dumps `obj` to a JSON string"
    ; f =
        InProcess
          (function
          | _, [DObj o] ->
              DObj o
              |> Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Object::toJSON_v1"]
    ; ins = []
    ; p = [par "obj" TObj]
    ; r = TStr
    ; d = "Dumps `obj` to a JSON string"
    ; f =
        InProcess
          (function
          | _, [DObj o] ->
              DObj o
              |> Dval.to_pretty_machine_json_v1
              |> Dval.dstr_of_string_exn
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Int *)
    (* ====================================== *)
    { pns = ["Int::mod"]
    ; ins = ["%"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d =
        "Return `a` % `b`, the modulus of a and b. This is the integer remainder left when `a` is divided by `b`. For example, `15 % 6 = 3`."
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DInt (a mod b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::add"]
    ; ins = ["+"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Adds two integers together"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DInt (a + b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::subtract"]
    ; ins = ["-"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Subtracts two integers"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DInt (a - b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::multiply"]
    ; ins = ["*"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Multiplies two integers"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DInt (a * b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::power"]
    ; ins = ["^"]
    ; p = [par "base" TInt; par "exponent" TInt]
    ; r = TInt
    ; d = "Raise `base` to the power of `exponent`"
    ; f =
        InProcess
          (function
          | _, [DInt base; DInt exp] ->
              DInt (Int.pow base exp)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::divide"]
    ; ins = ["/"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TInt
    ; d = "Divides two integers"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DInt (a / b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::greaterThan"]
    ; ins = [">"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is greater than b"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DBool (a > b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::greaterThanOrEqualTo"]
    ; ins = [">="]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is greater than or equal to b"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DBool (a >= b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::lessThan"]
    ; ins = ["<"]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is less than b"
    ; f =
        InProcess
          (function _, [DInt a; DInt b] -> DBool (a < b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::lessThanOrEqualTo"]
    ; ins = ["<="]
    ; p = [par "a" TInt; par "b" TInt]
    ; r = TBool
    ; d = "Returns true if a is less than or equal to b"
    ; f =
        InProcess
          (function
          | _, [DInt a; DInt b] -> DBool (a <= b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::random"]
    ; ins = []
    ; p = [par "start" TInt; par "end" TInt]
    ; r = TInt
    ; d = "Returns a random integer between a and b (inclusive)"
    ; f =
        InProcess
          (function
          (*( +1 as Random.int is exclusive *)
          | _, [DInt a; DInt b] ->
              DInt (a + 1 + Random.int (b - a))
          | args ->
              fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["Int::sqrt"]
    ; ins = []
    ; p = [par "a" TInt]
    ; r = TFloat
    ; d = "Get the square root of an Int"
    ; f =
        InProcess
          (function
          | _, [DInt a] -> DFloat (float_of_int a |> sqrt) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::toFloat"]
    ; ins = []
    ; p = [par "a" TInt]
    ; r = TFloat
    ; d = "Converts an Int to a Float"
    ; f =
        InProcess
          (function
          | _, [DInt a] -> DFloat (float_of_int a) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Floats *)
    (* ====================================== *)
    { pns = ["Float::ceiling"; "Float::roundUp"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TInt
    ; d = "Round above to an integer value"
    ; f =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_up a |> int_of_float)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::floor"; "Float::roundDown"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TInt
    ; d = "Round down to an integer value"
    ; f =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round_down a |> int_of_float)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::round"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TInt
    ; d = "Round to nearest integer value"
    ; f =
        InProcess
          (function
          | _, [DFloat a] ->
              DInt (Float.round a |> int_of_float)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::sqrt"]
    ; ins = []
    ; p = [par "a" TFloat]
    ; r = TFloat
    ; d = "Get the square root of a float"
    ; f =
        InProcess
          (function _, [DFloat a] -> DFloat (sqrt a) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::divide"]
    ; ins = ["/"]
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Divide float `a` by float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a /. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::add"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Add float `a` to float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a +. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::multiply"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Multiply float `a` by float `b`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a *. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::subtract"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TFloat
    ; d = "Subtract float `b` from float `a`"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DFloat (a -. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Int::sum"]
    ; ins = []
    ; p = [par "a" TList]
    ; r = TInt
    ; d = "Returns the sum of all the ints in the list"
    ; f =
        InProcess
          (function
          | _, [DList l] ->
              l
              |> list_coerce ~f:Dval.to_int
              >>| Util.int_sum
              >>| (fun x -> DInt x)
              |> Result.map_error ~f:(fun (result, example_value) ->
                     RT.error
                       ~actual:(DList result)
                       ~result:(DList result)
                       ~long:
                         ( "Int::sum requires all values to be integers, but "
                         ^ Dval.to_developer_repr_v0 example_value
                         ^ " is a "
                         ^ Dval.tipename example_value )
                       ~expected:"every list item to be an int "
                       "Sum expects you to pass a list of ints" )
              |> Result.ok_exn
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::greaterThan"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is greater than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a >. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::greaterThanOrEqualTo"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is greater than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a >=. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::lessThan"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is less than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a <. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Float::lessThanOrEqualTo"]
    ; ins = []
    ; p = [par "a" TFloat; par "b" TFloat]
    ; r = TBool
    ; d = "Returns true if a is less than b"
    ; f =
        InProcess
          (function
          | _, [DFloat a; DFloat b] -> DBool (a <=. b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Any *)
    (* ====================================== *)
    { pns = ["toString"]
    ; ins = []
    ; p = [par "v" TAny]
    ; r = TStr
    ; d =
        "Returns a string representation of `v`, suitable for displaying to a user. Redacts passwords."
    ; f =
        InProcess
          (function
          | _, [a] ->
              Dval.dstr_of_string_exn (Dval.to_enduser_readable_text_v0 a)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["toRepr"]
    ; ins = []
    ; p = [par "v" TAny]
    ; r = TStr
    ; d =
        "Returns an adorned string representation of `v`, suitable for internal developer usage. Not designed for sending to end-users, use toString instead. Redacts passwords."
    ; f =
        InProcess
          (function
          | _, [a] ->
              Dval.dstr_of_string_exn (Dval.to_developer_repr_v0 a)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["JSON::read"]
    ; ins = []
    ; p = [par "json" TStr]
    ; r = TAny
    ; d =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; f =
        InProcess
          (function
          | _, [DStr json] ->
            ( try Dval.of_unknown_json_v0 (Unicode_string.to_string json)
              with _ -> DNull )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["equals"]
    ; ins = ["=="]
    ; p = [par "a" TAny; par "b" TAny]
    ; r = TBool
    ; d = "Returns true if the two value are equal"
    ; f =
        InProcess
          (function _, [a; b] -> DBool (equal_dval a b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["notEquals"]
    ; ins = ["!="]
    ; p = [par "a" TAny; par "b" TAny]
    ; r = TBool
    ; d = "Returns true if the two value are not equal"
    ; f =
        InProcess
          (function
          | _, [a; b] -> DBool (not (equal_dval a b)) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Bool *)
    (* ====================================== *)
    { pns = ["Bool::not"]
    ; ins = []
    ; p = [par "b" TBool]
    ; r = TBool
    ; d =
        "Returns the inverse of `b`: true is `b` is false and false if `b` is true"
    ; f =
        InProcess
          (function _, [DBool b] -> DBool (not b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::and"]
    ; ins = ["&&"]
    ; p = [par "a" TBool; par "b" TBool]
    ; r = TBool
    ; d = "Returns true if both a and b are true"
    ; f =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a && b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::or"]
    ; ins = ["||"]
    ; p = [par "a" TBool; par "b" TBool]
    ; r = TBool
    ; d = "Returns true if either a is true or b is true"
    ; f =
        InProcess
          (function
          | _, [DBool a; DBool b] -> DBool (a || b) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::isNull"]
    ; ins = []
    ; p = [par "check" TAny]
    ; r = TBool
    ; d = "Returns true if the `check` parameter is null"
    ; f =
        InProcess
          (function
          | _, [value] ->
            (match value with DNull -> DBool true | _ -> DBool false)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Bool::isError"]
    ; ins = []
    ; p = [par "check" TAny]
    ; r = TBool
    ; d = "Returns true if the `check` parameter is an error"
    ; f =
        InProcess
          (function
          | _, [value] ->
            (match value with DError _ -> DBool true | _ -> DBool false)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* String *)
    (* ====================================== *)
    { pns = ["String::foreach"]
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
              ( try DInt (int_of_string utf8) with e ->
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
              ( try DResult (ResOk (DInt (int_of_string utf8))) with e ->
                  error_result "Expected a string with only numbers" )
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
              DInt (String.length (Unicode_string.to_string s))
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
          | _, [DStr s] -> DInt (Unicode_string.length s) | args -> fail args)
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
              if l < 0
              then Exception.code "l should be a positive integer"
              else Dval.dstr_of_string_exn (Util.random_string l)
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
              if l < 0
              then error_result "l should be a positive integer"
              else
                DResult
                  (ResOk (Dval.dstr_of_string_exn (Util.random_string l)))
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
    ; dep = false }
  ; (* ====================================== *)
    (* List *)
    (* ====================================== *)
    { pns = ["List::head"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TAny
    ; d =
        "Returns the head of a list. Returns null if the empty list is passed."
    ; f =
        InProcess
          (function
          | _, [DList l] ->
            (match List.hd l with Some dv -> dv | None -> DNull)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::head_v1"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TOption
    ; d = "Fetches the head of the list and returns an option"
    ; f =
        InProcess
          (function
          | _, [DList l] ->
            ( match List.hd l with
            | Some dv ->
                DOption (OptJust dv)
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::empty"]
    ; ins = []
    ; p = []
    ; r = TList
    ; d = "Returns an empty list."
    ; f = InProcess (function _, [] -> DList [] | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::push"]
    ; ins = []
    ; p = [par "list" TList; par "val" TAny]
    ; r = TList
    ; d = "Add element `val` to front of list `list`"
    ; f =
        InProcess
          (* fake cf handled by call *)
          (function
          | _, [DList l; i] -> DList (i :: l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::pushBack"]
    ; ins = []
    ; p = [par "list" TList; par "val" TAny]
    ; r = TList
    ; d = "Add element `val` to back of list `list`"
    ; f =
        InProcess
          (function _, [DList l; i] -> DList (l @ [i]) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::last"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TAny
    ; d = "Returns the last item in a last. Returns null if the list is empty."
    ; f =
        InProcess
          (function
          | _, [DList []] ->
              DNull
          | _, [DList l] ->
              List.last_exn l
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::last_v1"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TOption
    ; d = "Returns the last item in the list as an option"
    ; f =
        InProcess
          (function
          | _, [DList []] ->
              DOption OptNothing
          | _, [DList l] ->
              DOption (OptJust (List.last_exn l))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::reverse"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TList
    ; d = "Reverses `list`"
    ; f =
        InProcess
          (function _, [DList l] -> DList (List.rev l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::findFirst"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d = "Find the first element of the list, for which `f` returns true"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let f (dv : dval) : bool = DBool true = fn [dv] in
              (match List.find ~f l with None -> DNull | Some dv -> dv)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::contains"]
    ; ins = []
    ; p = [par "l" TList; par "val" TAny]
    ; r = TBool
    ; d = "Returns if the value is in the list"
    ; f =
        InProcess
          (function
          | _, [DList l; i] ->
              DBool (List.mem ~equal:equal_dval l i)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::repeat"]
    ; ins = []
    ; p = [par "times" TInt; par "val" TAny]
    ; r = TList
    ; d = "Returns a list containing `val` repeated `count` times"
    ; f =
        InProcess
          (function
          | _, [DInt t; dv] ->
              DList (List.init t ~f:(fun _ -> dv))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::length"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TInt
    ; d = "Returns the length of the list"
    ; f =
        InProcess
          (function _, [DList l] -> DInt (List.length l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::range"]
    ; ins = []
    ; p =
        [ par "start" TInt ~d:"First number in the range, will be included"
        ; par "stop" TInt ~d:"Last number in the range, is included" ]
    ; r = TList
    ; d =
        "Return a list of increasing integers from `start` to `stop`, inclusive"
    ; f =
        InProcess
          (function
          | _, [DInt start; DInt stop] ->
              DList
                (List.range start (stop + 1) |> List.map ~f:(fun i -> DInt i))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::fold"]
    ; ins = []
    ; p = [par "l" TList; par "init" TAny; func ["accum"; "curr"]]
    ; r = TAny
    ; d =
        "Folds the list into a single value, by repeatedly apply `f` to any two pairs"
    ; f =
        InProcess
          (function
          | _, [DList l; init; DBlock fn] ->
              (* Fake cf should be propagated by the blocks so we dont need to check *)
              let f (dv1 : dval) (dv2 : dval) : dval = fn [dv1; dv2] in
              List.fold ~f ~init l
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::flatten"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TList
    ; d =
        "Returns a single list containing the elements of all the lists in `l`"
    ; f =
        InProcess
          (function
          | _, [DList l] ->
              let f a b =
                match (a, b) with
                | DList a, DList b ->
                    DList (List.append a b)
                | _ ->
                    RT.error ~actual:(DList [a; b]) "Flattening non-lists"
              in
              List.fold ~init:(DList []) ~f l
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::uniqueBy"]
    ; ins = []
    ; p = [par "l" TList; par "f" TBlock]
    ; r = TList
    ; d =
        "Returns the passed list, with only unique values, where uniqueness is based on the result of `f`. Only one of each value will be returned, but the order will not be maintained"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              DList
                (List.dedup_and_sort l ~compare:(fun a b ->
                     compare_dval (fn [a]) (fn [b]) ))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::isEmpty"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TBool
    ; d = "Returns true iff. the list `l` is empty"
    ; f =
        InProcess
          (function
          | _, [DList l] -> DBool (List.is_empty l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::sort"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TList
    ; d = "Returns `list` sorted in ascending order"
    ; f =
        InProcess
          (function
          | _, [DList list] ->
              list |> List.sort ~compare:compare_dval |> DList
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::sortBy"]
    ; ins = []
    ; p = [par "list" TList; par "f" TBlock]
    ; r = TList
    ; d = "Returns `list`, sorted using the results of `f`"
    ; f =
        InProcess
          (function
          | _, [DList list; DBlock fn] ->
              list
              |> List.sort ~compare:(fun a b -> compare_dval (fn [a]) (fn [b]))
              |> DList
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::append"]
    ; ins = []
    ; p = [par "l1" TList; par "l2" TList]
    ; r = TList
    ; d = "Returns the combined list of `l1` and `l2`"
    ; f =
        InProcess
          (function
          | _, [DList l1; DList l2] ->
              DList (List.append l1 l2) (* no checking for fake cf required *)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::filter"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d = "Return only values in `l` which meet the function's criteria"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let incomplete = ref false in
              let f (dv : dval) : bool =
                match fn [dv] with
                | DBool b ->
                    b
                | DIncomplete ->
                    incomplete := true ;
                    false
                | v ->
                    RT.error "Expecting fn to return bool" ~result:v ~actual:dv
              in
              if !incomplete then DIncomplete else DList (List.filter ~f l)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["List::filter_v1"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d = "Return only values in `l` which meet the function's criteria"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let fakecf = ref None in
              let f (dv : dval) : bool =
                let run = !fakecf = None in
                run
                &&
                match fn [dv] with
                | DBool b ->
                    b
                | (DIncomplete | DErrorRail _) as dv ->
                    fakecf := Some dv ;
                    false
                | v ->
                    RT.error "Expecting fn to return bool" ~result:v ~actual:dv
              in
              let result = List.filter ~f l in
              (match !fakecf with None -> DList result | Some v -> v)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::drop"]
    ; ins = []
    ; p = [par "l" TList; par "count" TInt]
    ; r = TList
    ; d = "Drops the first `count` items from the list"
    ; f =
        InProcess
          (function
          | _, [DList l; DInt c] -> DList (List.drop l c) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::take"]
    ; ins = []
    ; p = [par "l" TList; par "count" TInt]
    ; r = TList
    ; d = "Drops all but the first `count` items from the list"
    ; f =
        InProcess
          (function
          | _, [DList l; DInt c] -> DList (List.take l c) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::foreach"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d =
        "Call `f` on every item in the list, returning a list of the results of
  those calls"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let f (dv : dval) : dval = fn [dv] in
              DList (List.map ~f l)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["List::map"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d =
        "Call `f` on every item in the list, returning a list of the results of
  those calls"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let f (dv : dval) : dval = fn [dv] in
              Dval.to_list (List.map ~f l)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::getAt"]
    ; ins = []
    ; p = [par "l" TList; par "index" TInt]
    ; r = TOption
    ; d =
        "Returns `Just item` at `index` in list `l` if `index` is less than the length of the list otherwise returns `Nothing`"
    ; f =
        InProcess
          (function
          | _, [DList l; DInt index] ->
              List.nth l index
              |> Option.map ~f:(fun a -> DOption (OptJust a))
              |> Option.value ~default:(DOption OptNothing)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Date *)
    (* ====================================== *)
    { pns = ["Date::parse"]
    ; ins = []
    ; p = [par "s" TStr]
    ; r = TDate
    ; d =
        "Parses a string representing a date in the ISO format and returns a Date"
    ; f =
        InProcess
          (function
          | _, [DStr s] ->
            ( try DDate (Util.date_of_isostring (Unicode_string.to_string s))
              with e -> RT.error "Invalid date format" )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::now"]
    ; ins = []
    ; p = []
    ; r = TDate
    ; d =
        "Returns the number of seconds since the epoch (midnight, Jan 1, 1970)"
    ; f =
        InProcess (function _, [] -> DDate (Time.now ()) | args -> fail args)
    ; ps = false
    ; dep = false }
  ; { pns = ["Date::add"]
    ; ins = []
    ; p = [par "d" TDate; par "seconds" TInt]
    ; r = TDate
    ; d = "Returns a new Date `seconds` seconds after `d`"
    ; f =
        InProcess
          (function
          | _, [DDate d; DInt s] ->
              DDate (Time.add d (Time.Span.of_int_sec s))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::sub"]
    ; ins = []
    ; p = [par "d" TDate; par "seconds" TInt]
    ; r = TDate
    ; d = "Returns a new Date `seconds` seconds before `d`"
    ; f =
        InProcess
          (function
          | _, [DDate d; DInt s] ->
              DDate (Time.sub d (Time.Span.of_int_sec s))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::greaterThan"]
    ; ins = ["Date::>"]
    ; p = [par "d1" TDate; par "d2" TDate]
    ; r = TBool
    ; d = "Returns whether `d1` > ` d2`"
    ; f =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( > ) d1 d2)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::lessThan"]
    ; ins = ["Date::<"]
    ; p = [par "d1" TDate; par "d2" TDate]
    ; r = TBool
    ; d = "Returns whether `d1` < ` d2`"
    ; f =
        InProcess
          (function
          | _, [DDate d1; DDate d2] ->
              DBool (Time.( < ) d1 d2)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Date::toSeconds"]
    ; ins = []
    ; p = [par "date" TDate]
    ; r = TInt
    ; d =
        "Converts a Date `date` to an integer representing seconds since the Unix epoch"
    ; f =
        InProcess
          (function
          | _, [DDate d] ->
              d
              |> Time.to_span_since_epoch
              |> Time.Span.to_sec
              |> Float.iround_exn
              |> DInt
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Char *)
    (* ====================================== *)
    { pns = ["Char::toASCIICode"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TInt
    ; d = "Return `c`'s ASCII code"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["Char::toASCIIChar"]
    ; ins = []
    ; p = [par "i" TInt]
    ; r = TCharacter
    ; d = "convert an int to an ASCII character"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["Char::toLowercase"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TCharacter
    ; d = "Return the lowercase value of `c`"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
  ; { pns = ["Char::toUppercase"]
    ; ins = []
    ; p = [par "c" TCharacter]
    ; r = TCharacter
    ; d = "Return the uppercase value of `c`"
    ; f = InProcess (fun _ -> Exception.code "This function no longer exists.")
    ; ps = true
    ; dep = true }
    (* ====================================== *)
    (* UUIDs *)
    (* ====================================== *)
  ; { pns = ["Uuid::generate"]
    ; ins = []
    ; p = []
    ; r = TUuid
    ; d = "Generate a new UUID v4 according to RFC 4122"
    ; f =
        InProcess (function _, [] -> DUuid (Uuidm.v `V4) | args -> fail args)
        (* similarly to Date::now, it's not particularly fun for this to change
     * when live programming *)
    ; ps = false
    ; dep = false }
  ; (* ====================================== *)
    (* Errors *)
    (* ====================================== *)
    { pns = ["Error::toString"]
    ; ins = []
    ; p = [par "err" TError]
    ; r = TStr
    ; d = "Return a string representing the error"
    ; f =
        InProcess
          (function
          | _, [DError err] -> Dval.dstr_of_string_exn err | args -> fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Options *)
    (* ====================================== *)
    { pns = ["Option::map"]
    ; ins = []
    ; p = [par "option" TOption; par "f" TBlock]
    ; r = TOption
    ; d =
        "Transform an Option using `f`, only if the Option is a Just. If Nothing, doesn't nothing."
    ; f =
        InProcess
          (function
          | _, [DOption o; DBlock fn] ->
            ( match o with
            | OptJust dv ->
                DOption (OptJust (fn [dv]))
            | OptNothing ->
                DOption OptNothing )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Option::withDefault"]
    ; ins = []
    ; p = [par "option" TOption; par "default" TAny]
    ; r = TAny
    ; d =
        "Turn an option into a normal value, using `default` if the option is Nothing."
    ; f =
        InProcess
          (function
          | _, [DOption o; default] ->
            (match o with OptJust dv -> dv | OptNothing -> default)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* Results *)
    (* ====================================== *)
    { pns = ["Result::map"]
    ; ins = []
    ; p = [par "result" TResult; par "f" TBlock]
    ; r = TResult
    ; d =
        "Transform a Result using `f`, only if the Result is an Ok. If Error, doesn't nothing."
    ; f =
        InProcess
          (function
          | _, [DResult r; DBlock fn] ->
            ( match r with
            | ResOk dv ->
                DResult (ResOk (fn [dv]))
            | ResError _ ->
                DResult r )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Result::withDefault"]
    ; ins = []
    ; p = [par "option" TResult; par "default" TAny]
    ; r = TAny
    ; d =
        "Turn a result into a normal value, using `default` if the result is Error."
    ; f =
        InProcess
          (function
          | _, [DResult o; default] ->
            (match o with ResOk dv -> dv | ResError _ -> default)
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
