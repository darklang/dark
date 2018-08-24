open Core_kernel

open Lib
open Types.RuntimeT
module RT = Runtime

let list_repeat = Util.list_repeat

let list_preview =
  fun dv cursor -> match dv with
                   | [DList l; _] -> (match List.nth l cursor with
                     | Some v -> [v]
                     | None -> [DIncomplete])
                   | args -> [DIncomplete]

(* type coerces one list to another using a function *)
let list_coerce ~(f: dval -> 'a option) (l : dval list) :
  (('a list, (dval list * dval)) Result.t) =
    l
    |> List.map ~f:(fun dv -> match f dv with
        | Some v -> Result.Ok v
        | None -> Result.Error (l, dv))
    |> Result.all

let (>>|) = Result.(>>|)


let fns : Lib.shortfn list = [

  (* ====================================== *)
  (* Dict  *)
  (* ====================================== *)

  { pns = ["Dict::keys"]
  ; ins = []
  ; p = [par "dict" TObj]
  ; r = TList
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DObj o]) ->
            o
            |> DvalMap.keys
            |> List.map ~f:(fun k -> DStr k)
            |> fun l -> DList l
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Dict::values"]
  ; ins = []
  ; p = [par "dict" TObj]
  ; r = TList
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DObj o]) -> DList (DvalMap.data o)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Dict::get"]
  ; ins = []
  ; p = [par "dict" TObj; par "key" TStr]
  ; r = TAny
  ; d = "Looks up `key` in object `dict` and returns the value if found, and Error otherwise"
  ; f = InProcess
        (function
          | (_, [DObj o; DStr s]) ->
            (match DvalMap.find o s with
             | Some d -> d
             | None -> DNull)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Dict::foreach"]
  ; ins = []
  ; p = [par "dict" TObj; func ["value"]]
  ; r = TObj
  ; d = "Iterates each `value` in object `dict` and mutates it according to the provided lambda"
  ; f = InProcess
        (function
          | (_, [DObj o; DBlock fn]) ->
            let f (dv: dval) : dval = fn [dv]
            in
            DObj (Map.map ~f o)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  (* ====================================== *)
  (* Objects *)
  (* ====================================== *)

  { pns = ["Object::empty"]
  ; ins = []
  ; p = []
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
          | (_, []) ->
            DObj (DvalMap.empty)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Object::merge"]
  ; ins = []
  ; p = [par "left" TObj; par "right" TObj]
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DObj l; DObj r]) ->
            DObj (Util.merge_right l r)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["assoc"]
  ; ins = []
  ; p = [par "obj" TObj; par "key" TStr; par "val" TAny]
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DObj o; DStr k; v]) ->
            DObj (Map.set o ~key:k ~data:v)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["dissoc"]
  ; ins = []
  ; p = [par "obj" TObj; par "key" TStr]
  ; r = TObj
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DObj o; DStr k]) ->
            DObj (Map.remove o k)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["toForm"]
  ; ins = []
  ; p = [par "obj" TObj; par "submit" TStr]
  ; r = TStr
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DObj o; DStr uri]) ->
            let fmt =
              format_of_string
                "<form action=\"%s\" method=\"post\">\n%s\n<input type=\"submit\" value=\"Save\">\n</form>"
            in
            let to_input (k, v) =
              let label =
                Printf.sprintf "<label for=\"%s\">%s:</label>" k k
              in
              let input =
                Printf.sprintf "<input id=\"%s\" type=\"text\" name=\"%s\">" k k
              in
              label ^ "\n" ^ input
            in
            let inputs =
              o
              |> Map.to_alist
              |> List.map ~f:to_input
              |> String.concat ~sep:"\n"
            in
            DStr (Printf.sprintf fmt uri inputs)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Object::toJSON"]
  ; ins = []
  ; p = [par "obj" TObj]
  ; r = TStr
  ; d = "Dumps `obj` to a JSON string"
  ; f = InProcess
        (function
          | (_, [DObj o]) ->
            DStr (Dval.dval_to_json_string (DObj o))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  (* ====================================== *)
  (* Int *)
  (* ====================================== *)
  { pns = ["Int::mod"]
  ; ins = ["%"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DInt (a mod b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Int::add"]
  ; ins = ["+"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Adds two integers together"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DInt (a + b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Int::subtract"]
  ; ins = ["-"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Subtracts two integers"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DInt (a - b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Int::multiply"]
  ; ins = ["*"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Multiples two integers"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DInt (a * b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::power"]
  ; ins = ["^"]
  ; p = [par "base" TInt ; par "exponent" TInt]
  ; r = TInt
  ; d = "Raise `base` to the power of `exponent`"
  ; f = InProcess
        (function
          | (_, [DInt base; DInt exp]) -> DInt (Int.pow base exp)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Int::divide"]
  ; ins = ["/"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Divides two integers"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DInt (a / b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::greaterThan"]
  ; ins = [">"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is greater than b"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DBool (a > b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::greaterThanOrEqualTo"]
  ; ins = [">="]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is greater than or equal to b"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DBool (a >= b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::lessThan"]
  ; ins = ["<"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is less than b"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DBool (a < b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::lessThanOrEqualTo"]
  ; ins = ["<="]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is less than or equal to b"
  ; f = InProcess
        (function
          | (_, [DInt a; DInt b]) -> DBool (a <= b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::random"]
  ; ins = []
  ; p = [par "start" TInt ; par "end" TInt]
  ; r = TInt
  ; d = "Returns a random integer between a and b (inclusive)"
  ; f = InProcess
        (function
          (*( +1 as Random.int is exclusive *)
          | (_, [DInt a; DInt b]) -> DInt (a + 1 + (Random.int (b - a)))
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["Int::sqrt"]
  ; ins = []
  ; p = [par "a" TInt]
  ; r = TFloat
  ; d = "Get the square root of an Int"
  ; f = InProcess
        (function
          | (_, [DInt a]) -> DFloat (float_of_int a |> sqrt)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Int::toFloat"]
  ; ins = []
  ; p = [par "a" TInt]
  ; r = TFloat
  ; d = "Converts an Int to a Float"
  ; f = InProcess
        (function
          | (_, [DInt a]) -> DFloat (float_of_int a)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  (* ====================================== *)
  (* Floats *)
  (* ====================================== *)


  { pns = ["Float::ceiling"; "Float::roundUp"]
  ; ins = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round above to an integer value"
  ; f = InProcess
        (function
          | (_, [DFloat a]) -> DInt (Float.round_up a |> int_of_float)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Float::floor"; "Float::roundDown"]
  ; ins = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round down to an integer value"
  ; f = InProcess
        (function
          | (_, [DFloat a]) -> DInt (Float.round_down a |> int_of_float)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Float::round"]
  ; ins = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round to nearest integer value"
  ; f = InProcess
        (function
          | (_, [DFloat a]) -> DInt (Float.round a |> int_of_float)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Float::sqrt"]
  ; ins = []
  ; p = [par "a" TFloat]
  ; r = TFloat
  ; d = "Get the square root of a float"
  ; f = InProcess
        (function
          | (_, [DFloat a]) -> DFloat (sqrt a)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::divide"]
  ; ins = ["/"]
  ; p = [par "a" TFloat; par "b" TFloat]
  ; r = TFloat
  ; d = "Divide float `a` by float `b`"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DFloat (a /. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::add"]
  ; ins = []
  ; p = [par "a" TFloat; par "b" TFloat]
  ; r = TFloat
  ; d = "Add float `a` to float `b`"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DFloat (a +. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::multiply"]
  ; ins = []
  ; p = [par "a" TFloat; par "b" TFloat]
  ; r = TFloat
  ; d = "Multiply float `a` by float `b`"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DFloat (a *. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::subtract"]
  ; ins = []
  ; p = [par "a" TFloat; par "b" TFloat]
  ; r = TFloat
  ; d = "Subtract float `b` from float `a`"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DFloat (a -. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::sum"]
  ; ins = []
  ; p = [par "a" TList]
  ; r = TInt
  ; d = "Returns the sum of all the ints in the list"
  ; f = InProcess
        (function
          | (_, [DList l]) ->
            l
            |> list_coerce ~f:Dval.to_int
            >>| Util.int_sum
            >>| (fun x -> DInt x)
            |> Result.map_error ~f:(fun (result, example_value) ->
                RT.error
                  ~actual:(DList result)
                  ~result:(DList result)
                  ~long:("Int::sum requires all values to be integers, but " ^ (Dval.to_repr example_value) ^ " is a " ^ (Dval.tipename example_value))
                  ~expected:"every list item to be an int "
                  "Sum expects you to pass a list of ints")
            |> Result.ok_exn
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::greaterThan"]
  ; ins = []
  ; p = [par "a" TFloat ; par "b" TFloat]
  ; r = TBool
  ; d = "Returns true if a is greater than b"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DBool (a >. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::greaterThanOrEqualTo"]
  ; ins = []
  ; p = [par "a" TFloat ; par "b" TFloat]
  ; r = TBool
  ; d = "Returns true if a is greater than b"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DBool (a >=. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::lessThan"]
  ; ins = []
  ; p = [par "a" TFloat ; par "b" TFloat]
  ; r = TBool
  ; d = "Returns true if a is less than b"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DBool (a <. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Float::lessThanOrEqualTo"]
  ; ins = []
  ; p = [par "a" TFloat ; par "b" TFloat]
  ; r = TBool
  ; d = "Returns true if a is less than b"
  ; f = InProcess
        (function
          | (_, [DFloat a; DFloat b]) -> DBool (a <=. b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  (* ====================================== *)
  (* Any *)
  (* ====================================== *)
  { pns = ["toString"]
  ; ins = []
  ; p = [par "v" TAny]
  ; r = TStr
  ; d = "Returns a string representation of `v`"
  ; f = InProcess
        (function
          | (_, [a]) -> DStr (Dval.as_string a)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["toRepr"]
  ; ins = []
  ; p = [par "v" TAny]
  ; r = TStr
  ; d = "Returns an adorned string representation of `v`"
  ; f = InProcess
        (function
          | (_, [a]) -> DStr (Dval.to_repr a)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["JSON::read"]
  ; ins = []
  ; p = [par "json" TStr]
  ; r = TAny
  ; d = "Parses a json string and returns its value"
  ; f = InProcess
        (function
          | (_, [DStr json]) ->
            (match Dval.parse_basic_json json with
             | Some dv -> dv
             | _ -> DNull)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;




  { pns = ["equals"]
  ; ins = ["=="]
  ; p = [par "a" TAny; par "b" TAny]
  ; r = TBool
  ; d = "Returns true if the two value are equal"
  ; f = InProcess
        (function
          | (_, [a; b]) -> DBool (equal_dval a b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["notEquals"]
  ; ins = ["!="]
  ; p = [par "a" TAny; par "b" TAny]
  ; r = TBool
  ; d = "Returns true if the two value are not equal"
  ; f = InProcess
        (function
          | (_, [a; b]) -> DBool (not (equal_dval a b))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  (* ====================================== *)
  (* Bool *)
  (* ====================================== *)
  { pns = ["Bool::not"]
  ; ins = []
  ; p = [par "b" TBool]
  ; r = TBool
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DBool b]) -> DBool (not b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Bool::and"]
  ; ins = ["&&"]
  ; p = [par "a" TBool ; par "b" TBool]
  ; r = TBool
  ; d = "Returns true if both a and b are true"
  ; f = InProcess
        (function
          | (_, [DBool a; DBool b]) -> DBool (a && b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Bool::or"]
  ; ins = ["||"]
  ; p = [par "a" TBool ; par "b" TBool]
  ; r = TBool
  ; d = "Returns true if either a is true or b is true"
  ; f = InProcess
        (function
          | (_, [DBool a; DBool b]) -> DBool (a || b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Bool::isNull"]
  ; ins = []
  ; p = [par "check" TAny;]
  ; r = TBool
  ; d = "Returns true if the `check` parameter is null"
  ; f = InProcess
        (function
          | (_, [value]) ->
            (match value with
             | DNull -> DBool true
             | _ -> DBool false)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Bool::isError"]
  ; ins = []
  ; p = [par "check" TAny;]
  ; r = TBool
  ; d = "Returns true if the `check` parameter is an error"
  ; f = InProcess
        (function
          | (_, [value]) ->
            (match value with
             | DError _ -> DBool true
             | _ -> DBool false)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  (* ====================================== *)
  (* String *)
  (* ====================================== *)
  { pns = ["String::foreach"]
  ; ins = []
  ; p = [par "s" TStr; func ["char"]]
  ; r = TStr
  ; d = "Iterate over each character in the string, performing the operation in the block on each one"
  ; f = InProcess
        (function
          | (_, [DStr s; DBlock fn]) ->
            let result = s
                         |> String.to_list
                         |> List.map ~f:(fun c -> fn [(DChar c)]) in
            if List.exists ~f:((=) DIncomplete) result
            then DIncomplete
            else
              result
              |> list_coerce ~f:Dval.to_char
              >>| String.of_char_list
              >>| (fun x -> DStr x)
              |> Result.map_error ~f:(fun (result, example_value) ->
                  RT.error
                    ~actual:(DList result)
                    ~result:(DList result)
                    ~long:("String::foreach needs to get chars back in order to reassemble them into a string. The values returned by your code are not chars, for example " ^ (Dval.to_repr example_value) ^ " is a " ^ (Dval.tipename example_value))
                    ~expected:"every value to be a char"
                    "Foreach expects you to return chars")
              |> Result.ok_exn
          | args -> fail args)
  ; pr = Some
        (fun dv cursor ->
          match dv with
          | [DStr s; _] ->
              let s: string = (if s = "" then "example" else s) in
              let index: int = (min cursor ((String.length s) - 1)) in
              let c: char = String.get s index in
                [DChar c]
          | args -> [DIncomplete])
  ; ps = true
  }
  ;

  { pns = ["String::toList"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TList
  ; d = "Returns the list of characters in the string"
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
              DList (String.to_list s |> List.map ~f:(fun c -> DChar c))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["String::replaceAll"]
  ; ins = []
  ; p = [par "s" TStr; par "searchFor" TStr; par "replaceWith" TStr]
  ; r = TList
  ; d = "Replace all instances on `searchFor` in `s` with `replaceWith`"
  ; f = InProcess
        (function
          | (_, [DStr s; DStr searchFor; DStr replaceWith]) ->
              DStr (Util.string_replace searchFor replaceWith s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["String::toInt"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TInt
  ; d = "Returns the int value of the string"
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
              DInt (int_of_string s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::toFloat"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TFloat
  ; d = "Returns the float value of the string"
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
              DFloat (float_of_string s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["String::toUppercase"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TList
  ; d = "Returns the string, uppercased"
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
              DStr (String.uppercase s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::toLowercase"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TList
  ; d = "Returns the string, lowercased"
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
              DStr (String.lowercase s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;




  { pns = ["String::length"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TInt
  ; d = "Returns the length of the string"
  ; f = InProcess
        (function
          | (_, [DStr s]) -> DInt (String.length s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::append"]
  ; ins = ["++"]
  ; p = [par "s1" TStr; par "s2" TStr]
  ; r = TStr
  ; d = "Concatenates the two strings and returns the joined string"
  ; f = InProcess
        (function
          | (_, [DStr s1; DStr s2]) -> DStr (s1 ^ s2)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["String::slugify"]
  ; ins = []
  ; p = [par "string" TStr]
  ; r = TStr
  ; d = "Turns a string into a slug"
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
            let replace = Libtarget.regexp_replace in
            let to_remove  = "[^\\w\\s$*_+~.()'\"!\\-:@]" in
            let trim = "^\\s+|\\s+$" in
            let spaces = "[-\\s]+" in

            s
            |> replace ~pattern:to_remove ~replacement:""
            |> replace ~pattern:trim ~replacement:""
            |> replace ~pattern:spaces ~replacement:"-"
            |> String.lowercase
            |> fun x -> DStr x
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["String::reverse"]
  ; ins = []
  ; p = [par "string" TStr]
  ; r = TStr
  ; d = "Reverses `string`"
  ; f = InProcess
        (function
          | (_, [DStr s]) -> DStr (String.rev s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::split"]
  ; ins = []
  ; p = [par "s" TStr; par "separator" TStr]
  ; r = TList
  ; d = "Splits a string at the separator, returning a list of strings without the separator. If the separator is not present, returns a list containing only the initial string."
  ; f = InProcess
        (function
          | (_, [DStr s; DStr sep]) ->
            s
            |> Libtarget.string_split ~sep
            |> List.map ~f:(fun str -> DStr str)
            |> DList
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::join"]
  ; ins = []
  ; p = [par "l" TList; par "separator" TStr]
  ; r = TStr
  ; d = "Combines a list of strings with the provided separator"
  ; f = InProcess
        (function
          | (_, [DList l; DStr sep]) ->
            let s = List.map ~f:(fun s ->
                match s with
                | DStr st -> st
                | _  -> Dval.to_repr s) l
            in
            DStr (String.concat ~sep s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::fromList"]
  ; ins = []
  ; p = [par "l" TList]
  ; r = TStr
  ; d = "Returns the list of characters as a string"
  ; f = InProcess
        (function
          | (_, [DList l]) ->
              DStr (l |> List.map ~f:(function
                                      | DChar c -> c
                                      | dv -> RT.error ~actual:dv "expected a char")
                      |> String.of_char_list)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::fromChar"]
  ; ins = []
  ; p = [par "c" TChar]
  ; r = TChar
  ; d = "Converts a char to a string"
  ; f = InProcess
        (function
          | (_, [DChar c]) -> DStr (Char.to_string c)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::base64Encode"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TStr
  ; d = "Base64 encodes a string. Uses URL-safe encoding."
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
            DStr (B64.encode ~alphabet:B64.uri_safe_alphabet ~pad:false s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::base64Decode"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TStr
  ; d = "Base64 decodes a string."
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
            (try
               DStr (B64.decode ~alphabet:B64.uri_safe_alphabet s)
             with
             | Not_found_s _ | Caml.Not_found ->
               (try
                  DStr (B64.decode ~alphabet:B64.default_alphabet s)
                with
                | Not_found_s _ | Caml.Not_found ->
                  RT.error ~actual:(DStr s) "Not a valid base64 string"))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::digest"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TStr
  ; d = "Take a string and hash it to a cryptographically-secure digest.
  Don't rely on either the size or the algorithm."
  ; f = InProcess
        (function
          | (_, [DStr s]) -> DStr (Libtarget.digest384 s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["String::sha384"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TStr
  ; d = "Take a string and hash it using SHA384."
  ; f = InProcess
        (function
          | (_, [DStr s]) -> DStr (Libtarget.digest384 s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["String::sha256"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TStr
  ; d = "Take a string and hash it using SHA256."
  ; f = InProcess
        (function
          | (_, [DStr s]) -> DStr (Libtarget.digest256 s)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;




  { pns = ["String::random"]
  ; ins = []
  ; p = [par "length" TInt]
  ; r = TStr
  ; d = "Generate a string of length `length` from random characters."
  ; f = InProcess
        (function
          | (_, [DInt l]) ->
            if l < 0
            then Exception.user "l should be a positive integer"
            else
              DStr (Util.random_string l)
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["String::htmlEscape"]
  ; ins = []
  ; p = [par "html" TStr]
  ; r = TStr
  ; d = "Escape an untrusted string in order to include it safely in HTML output."
  ; f = InProcess
          (function
           | (_, [DStr s]) -> DStr (Util.html_escape s)
           | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["String::toUUID"]
  ; ins = []
  ; p = [par "uuid" TStr]
  ; r = TUuid
  ; d = "Parse a UUID of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX from \
         the input `uuid` string"
  ; f = InProcess
          (function
           | (_, [DStr s]) ->
             (match Uuidm.of_string s with
              | Some id -> DUuid id
              | None ->
                Exception.user
                  "`uuid` parameter was not of form XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX")
           | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["String::isSubstring"]
  ; ins = []
  ; p = [par "searchingFor" TStr; par "lookingIn" TStr]
  ; r = TBool
  ; d = "Checks if `lookingIn` contains `searchingFor`"
  ; f = InProcess
          (function
            | (_, [DStr needle; DStr haystack]) ->
              DBool (String.is_substring ~substring:needle haystack)
           | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  (* ====================================== *)
  (* List *)
  (* ====================================== *)
  { pns = ["List::head"]
  ; ins = []
  ; p = [par "list" TList]
  ; r = TAny
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DList l]) ->
            (match List.hd l with
             | Some dv -> dv
             | None -> DNull)

          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::empty"]
  ; ins = []
  ; p = []
  ; r = TList
  ; d = ""
  ; f = InProcess
        (function
          | (_, []) -> DList []
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::push"]
  ; ins = []
  ; p = [par "list" TList; par "val" TAny]
  ; r = TList
  ; d = "Add element `val` to front of list `list`"
  ; f = InProcess
        (function
          | (_, [DList l; i]) -> DList (i :: l)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::pushBack"]
  ; ins = []
  ; p = [par "list" TList; par "val" TAny]
  ; r = TList
  ; d = "Add element `val` to back of list `list`"
  ; f = InProcess
        (function
          | (_, [DList l; i]) -> DList (l @ [i])
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::last"]
  ; ins = []
  ; p = [par "list" TList]
  ; r = TAny
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DList []]) -> DNull
          | (_, [DList l]) -> List.last_exn l
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::reverse"]
  ; ins = []
  ; p = [par "list" TList]
  ; r = TList
  ; d = "Reverses `list`"
  ; f = InProcess
        (function
          | (_, [DList l]) -> DList (List.rev l)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::findFirst"]
  ; ins = []
  ; p = [par "l" TList; func ["val"]]
  ; r = TList
  ; d = "Find the first element of the list, for which `f` returns true"
  ; f = InProcess
        (function
          | (_, [DList l; DBlock fn]) ->
            (let f (dv: dval) : bool = DBool true = fn [dv]
            in
            match List.find ~f l with
            | None -> DNull
            | Some dv -> dv)
        | args -> fail args)
  ; pr = Some list_preview
  ; ps = true
  }
  ;


  { pns = ["List::contains"]
  ; ins = []
  ; p = [par "l" TList; par "val" TAny]
  ; r = TBool
  ; d = "Returns if the value is in the list"
  ; f = InProcess
        (function
          | (_, [DList l; i]) -> DBool (List.mem ~equal:equal_dval l i)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::repeat"]
  ; ins = []
  ; p = [par "times" TInt; par "val" TAny]
  ; r = TList
  ; d = "Returns a list containing `val` repeated `count` times"
  ; f = InProcess
        (function
          | (_, [DInt t; dv]) -> DList (List.init t ~f:(fun _ -> dv))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::length"]
  ; ins = []
  ; p = [par "l" TList]
  ; r = TInt
  ; d = "Returns the length of the list"
  ; f = InProcess
        (function
          | (_, [DList l]) -> DInt (List.length l)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::range"]
  ; ins = []
  ; p = [ par "start" TInt ~d:"First number in the range, will be included"
        ; par "stop" TInt ~d:"Last number in the range, is included"]
  ; r = TList
  ; d = "Return a list of increasing integers from `start` to `stop`, inclusive"
  ; f = InProcess
        (function
          | (_, [DInt start; DInt stop]) -> DList (List.range start (stop+1)
                                              |> List.map ~f:(fun i -> DInt i))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::fold"]
  ; ins = []
  ; p = [par "l" TList; par "init" TAny; func ["accum"; "curr"]]
  ; r = TAny
  ; d = "Folds the list into a single value, by repeatedly apply `f` to any two pairs"
  ; f = InProcess
        (function
          | (_, [DList l; init; DBlock fn]) ->
            let f (dv1: dval) (dv2: dval) : dval = fn [dv1; dv2] in
            List.fold ~f ~init l
          | args -> fail args)
  ; pr = Some
        (fun dv cursor ->
          match dv with
          | [DList l; init; DBlock fn] ->
            let short_l = List.take l cursor in
            let f = fun (accum:dval) (elt:dval) -> fn([accum; elt]) in
            let end_accum = List.fold ~f ~init short_l in
            let next_elt =
              (match List.nth l cursor with
              | Some elt -> elt
              | None -> DIncomplete)
            in [end_accum; next_elt]
          | args -> [DIncomplete])
  ; ps = true
  }
  ;


  { pns = ["List::flatten"]
  ; ins = []
  ; p = [par "l" TList]
  ; r = TList
  ; d = "Returns a single list containing the elements of all the lists in `l`"
  ; f = InProcess
        (function
          | (_, [DList l]) ->
              let f = fun a b ->
                match (a, b) with
                  | (DList a, DList b) -> DList (List.append a b)
                  | _ -> RT.error ~actual:(DList [a; b]) "Flattening non-lists"
              in
              List.fold ~init:(DList []) ~f l
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::uniqueBy"]
  ; ins = []
  ; p = [par "l" TList; par "f" TBlock]
  ; r = TList
  ; d = "Returns the passed list, with only unique values, where uniqueness is based on the result of `f`. Only one of each value will be returned, but the order will not be maintained"
  ; f = InProcess
        (function
          | (_, [DList l; DBlock fn]) ->
              DList (List.dedup_and_sort l ~compare:(fun a b ->
                compare_dval (fn [a]) (fn [b])
              ))

          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::isEmpty"]
  ; ins = []
  ; p = [par "l" TList]
  ; r = TBool
  ; d = "Returns true iff. the list `l` is empty"
  ; f = InProcess
        (function
          | (_, [DList l]) ->
            DBool (List.is_empty l)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::sort"]
  ; ins = []
  ; p = [par "list" TList]
  ; r = TList
  ; d = "Returns `list` sorted in ascending order"
  ; f = InProcess
        (function
          | (_, [DList list]) ->
            list
            |> List.sort ~compare:compare_dval
            |> DList
          | args -> fail args)
  ; pr = Some list_preview
  ; ps = true
  }
  ;


  { pns = ["List::sortBy"]
  ; ins = []
  ; p = [par "list" TList; par "f" TBlock]
  ; r = TList
  ; d = "Returns `list`, sorted using the results of `f`"
  ; f = InProcess
        (function
          | (_, [DList list; DBlock fn]) ->
            list
            |> List.sort ~compare:(fun a b -> compare_dval (fn [a]) (fn [b]))
            |> DList
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::append"]
  ; ins = []
  ; p = [par "l1" TList; par "l2" TList]
  ; r = TList
  ; d = "Returns the combined list of `l1` and `l2`"
  ; f = InProcess
        (function
          | (_, [DList l1; DList l2]) -> DList (List.append l1 l2)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;



  { pns = ["List::filter"]
  ; ins = []
  ; p = [par "l" TList; func ["val"]]
  ; r = TList
  ; d = "Return only values in `l` which meet the function's criteria"
  ; f = InProcess
        (function
          | (_, [DList l; DBlock fn]) ->
            let incomplete = ref false in
            let f (dv: dval) : bool =
            match fn [dv] with
            | DBool b -> b
            | DIncomplete ->
              incomplete := true;
              false
            | v -> RT.error "Expecting fn to return bool"
                     ~result:v ~actual:dv
            in
            if !incomplete
            then DIncomplete
            else DList (List.filter ~f l)
          | args -> fail args)
  ; pr = Some list_preview
  ; ps = true
  }
  ;

  { pns = ["List::drop"]
  ; ins = []
  ; p = [par "l" TList; par "count" TInt]
  ; r = TList
  ; d = "Drops the first `count` items from the list"
  ; f = InProcess
        (function
          | (_, [DList l; DInt c]) ->
            DList (List.drop l c)
          | args -> fail args)
  ; pr = Some list_preview
  ; ps = true
  }
  ;

  { pns = ["List::take"]
  ; ins = []
  ; p = [par "l" TList; par "count" TInt]
  ; r = TList
  ; d = "Drops all but the first `count` items from the list"
  ; f = InProcess
        (function
          | (_, [DList l; DInt c]) ->
            DList (List.take l c)
          | args -> fail args)
  ; pr = Some list_preview
  ; ps = true
  }
  ;


  { pns = ["List::foreach"]
  ; ins = []
  ; p = [par "l" TList; func ["val"]]
  ; r = TList
  ; d = "Call `f` on every item in the list, returning a list of the results of
  those calls"
  ; f = InProcess
        (function
          | (_, [DList l; DBlock fn]) ->
            let f (dv: dval) : dval = fn [dv]
            in
            DList (List.map ~f l)
          | args -> fail args)
  ; pr = Some list_preview
  ; ps = true
  }
  ;


  (* ====================================== *)
  (* Date *)
  (* ====================================== *)
  { pns = ["Date::parse"]
  ; ins = []
  ; p = [par "s" TStr]
  ; r = TDate
  ; d = "Parses a string representing a date in the ISO format and returns a Date"
  ; f = InProcess
        (function
          | (_, [DStr s]) ->
              (try
                DDate (Dval.date_of_isostring s)
              with e -> RT.error "Invalid date format")
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Date::now"]
  ; ins = []
  ; p = []
  ; r = TDate
  ; d = "Returns the number of seconds since the epoch (midnight, Jan 1, 1970)"
  ; f = InProcess
        (function
          | (_, []) -> DDate (Time.now ())
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

  { pns = ["Date::add"]
  ; ins = []
  ; p = [par "d" TDate; par "seconds" TInt]
  ; r = TDate
  ; d = "Returns a new Date `seconds` seconds after `d`"
  ; f = InProcess
        (function
          | (_, [DDate d; DInt s]) ->
            DDate (Time.add d (Time.Span.of_int_sec s))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Date::sub"]
  ; ins = []
  ; p = [par "d" TDate; par "seconds" TInt]
  ; r = TDate
  ; d = "Returns a new Date `seconds` seconds before `d`"
  ; f = InProcess
        (function
          | (_, [DDate d; DInt s]) ->
            DDate (Time.sub d (Time.Span.of_int_sec s))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Date::greaterThan"]
  ; ins = ["Date::>"]
  ; p = [par "d1" TDate; par "d2" TDate]
  ; r = TBool
  ; d = "Returns whether `d1` > ` d2`"
  ; f = InProcess
        (function
          | (_, [DDate d1; DDate d2]) ->
            DBool (Time.(>) d1 d2)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Date::lessThan"]
  ; ins = ["Date::<"]
  ; p = [par "d1" TDate; par "d2" TDate]
  ; r = TBool
  ; d = "Returns whether `d1` < ` d2`"
  ; f = InProcess
        (function
          | (_, [DDate d1; DDate d2]) ->
            DBool (Time.(<) d1 d2)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Date::toSeconds"]
  ; ins = []
  ; p = [par "date" TDate]
  ; r = TInt
  ; d = "Converts a Date `date` to an integer representing seconds since the Unix epoch"
  ; f = InProcess
        (function
          | (_, [DDate d]) ->
            d
            |> Time.to_span_since_epoch
            |> Time.Span.to_sec
            |> Float.iround_exn
            |> DInt
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  (* ====================================== *)
  (* Char *)
  (* ====================================== *)
  { pns = ["Char::toASCIICode"]
  ; ins = []
  ; p = [par "c" TChar]
  ; r = TInt
  ; d = "Return `c`'s ASCII code"
  ; f = InProcess
        (function
          | (_, [DChar c]) -> DInt (Char.to_int c)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Char::toASCIIChar"]
  ; ins = []
  ; p = [par "i" TInt]
  ; r = TChar
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DInt i]) -> DChar (Char.of_int_exn i)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Char::toLowercase"]
  ; ins = []
  ; p = [par "c" TChar]
  ; r = TChar
  ; d = "Return the lowercase value of `c`"
  ; f = InProcess
        (function
          | (_, [DChar c]) -> DChar (Char.lowercase c)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Char::toUppercase"]
  ; ins = []
  ; p = [par "c" TChar]
  ; r = TChar
  ; d = "Return the uppercase value of `c`"
  ; f = InProcess
        (function
          | (_, [DChar c]) -> DChar (Char.uppercase c)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Uuid::generate"]
  ; ins = []
  ; p = []
  ; r = TUuid
  ; d = "Generate a new UUID v4 according to RFC 4122"
  ; f = InProcess
        (function
         | (_, []) -> DUuid (Uuidm.v `V4)
         | args -> fail args)
  ; pr = None
  (* similarly to Date::now, it's not particularly fun for this to change
   * when live programming *)
  ; ps = false
  }
  ;
]
