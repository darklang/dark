open Core
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
          | [DObj o] -> o
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
          | [DObj o] -> DList (DvalMap.data o)
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
          | [] ->
            DObj (DvalMap.empty)
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
          | [DObj o; DStr k; v] ->
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
          | [DObj o; DStr k] ->
            DObj (Map.remove o k)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["to_form"]
  ; ins = []
  ; p = [par "obj" TObj; par "submit" TStr]
  ; r = TStr
  ; d = ""
  ; f = InProcess
        (function
          | [DObj o; DStr uri] ->
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
          | [DInt a; DInt b] -> DInt (a mod b)
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
          | [DInt a; DInt b] -> DInt (a + b)
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
          | [DInt a; DInt b] -> DInt (a - b)
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
          | [DInt a; DInt b] -> DInt (a * b)
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
          | [DInt base; DInt exp] -> DInt (Int.pow base exp)
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
          | [DInt a; DInt b] -> DInt (a / b)
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
          | [DInt a; DInt b] -> DBool (a > b)
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
          | [DInt a; DInt b] -> DBool (a < b)
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
          | [DInt a; DInt b] -> DBool (a <= b)
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
          | [DInt a; DInt b] -> DBool (a >= b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Int::random"]
  ; ins = []
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Returns a random integer between a and b (inclusive)"
  ; f = InProcess
        (function
          (* +1 as Random.int is exclusive *)
          | [DInt a; DInt b] -> DInt (a + 1 + (Random.int (b - a)))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Int::sqrt"]
  ; ins = []
  ; p = [par "a" TInt]
  ; r = TFloat
  ; d = "Get the square root of an Int"
  ; f = InProcess
        (function
          | [DInt a] -> DFloat (float_of_int a |> sqrt)
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
          | [DInt a] -> DFloat (float_of_int a)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  (* ====================================== *)
  (* Floats *)
  (* ====================================== *)


  { pns = ["Float::ceiling"; "Float::round_up"]
  ; ins = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round above to an integer value"
  ; f = InProcess
        (function
          | [DFloat a] -> DInt (Float.round_up a |> int_of_float)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Float::floor"; "Float::round_down"]
  ; ins = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round down to an integer value"
  ; f = InProcess
        (function
          | [DFloat a] -> DInt (Float.round_down a |> int_of_float)
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
          | [DFloat a] -> DInt (Float.round a |> int_of_float)
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
          | [DFloat a] -> DFloat (sqrt a)
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
          | [DList l] ->
            l
            |> list_coerce ~f:Dval.to_int
            >>| List.fold_left ~f:(+) ~init:0
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
          | [a] -> DStr (Dval.to_repr a)
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
          | [a; b] -> DBool (Dval.equal_dval a b)
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
          | [a; b] -> DBool (not (Dval.equal_dval a b))
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
          | [DBool b] -> DBool (not b)
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
          | [DBool a; DBool b] -> DBool (a && b)
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
          | [DBool a; DBool b] -> DBool (a || b)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Bool::isNull"]
  ; ins = []
  ; p = [par "check" TAny;]
  ; r = TBool
  ; d = "Ignores the first param and returns the 2nd."
  ; f = InProcess
        (function
          | [value] ->
            (match value with
             | DNull -> DBool true
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
          | [DStr s; DBlock fn] ->
            s
            |> String.to_list
            |> List.map ~f:(fun c -> fn [(DChar c)])
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
          | [DStr s] ->
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
          | [DStr s; DStr searchFor; DStr replaceWith] ->
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
          | [DStr s] ->
              DInt (int_of_string s)
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
          | [DStr s] -> DInt (String.length s)
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
          | [DStr s1; DStr s2] -> DStr (s1 ^ s2)
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
          | [DStr s] ->
            let re_compile = Re2.Regex.create_exn in
            let re_replace = Re2.Regex.replace_exn in
            let to_remove  = re_compile "[^\\w\\s$*_+~.()'\"!\\-:@]" in
            let trim = re_compile "^\\s+|\\s+$" in
            let spaces = re_compile "[-\\s]+" in
            s
            |> re_replace ~f:(fun _ -> "") to_remove
            |> re_replace ~f:(fun _ -> "") trim
            |> re_replace ~f:(fun _ -> "-") spaces
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
          | [DStr s] -> DStr (String.rev s)
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
          | [DStr s; DStr sep] ->
            let split = (Str.split (Str.regexp_string sep) s)
            in split
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
          | [DList l; DStr sep] ->
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
          | [DList l] ->
              DStr (l |> List.map ~f:(function
                                      | DChar c -> c
                                      | dv -> RT.raise_error ~actual:dv "expected a char")
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
          | [DChar c] -> DStr (Char.to_string c)
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
          | [DList l] ->
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
          | [] -> DList []
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["List::push"]
  ; ins = []
  ; p = [par "list" TList; par "val" TAny]
  ; r = TList
  ; d = ""
  ; f = InProcess
        (function
          | [DList l; i] -> DList (i :: l)
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
          | [DList l] -> List.last_exn l
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
          | [DList l] -> DList (List.rev l)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::find_first"]
  ; ins = []
  ; p = [par "l" TList; func ["val"]]
  ; r = TList
  ; d = "Find the first element of the list, for which `f` returns true"
  ; f = InProcess
        (function
          | [DList l; DBlock fn] ->
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
          | [DList l; i] -> DBool (List.mem ~equal:Dval.equal_dval l i)
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
          | [DInt t; dv] -> DList (List.init t ~f:(fun _ -> dv))
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
          | [DList l] -> DInt (List.length l)
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
          | [DInt start; DInt stop] -> DList (List.range start (stop+1)
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
          | [DList l; init; DBlock fn] ->
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
          | [DList l] ->
              let f = fun a b ->
                match (a, b) with
                  | (DList a, DList b) -> DList (List.append a b)
                  | _ -> DIncomplete
              in
              List.fold ~init:(DList []) ~f l
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::is_empty"]
  ; ins = []
  ; p = [par "l" TList]
  ; r = TBool
  ; d = "Returns true iff. the list `l` is empty"
  ; f = InProcess
        (function
          | [DList l] ->
            DBool (List.is_empty l)
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
          | [DList l1; DList l2] -> DList (List.append l1 l2)
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
          | [DList l; DBlock fn] ->
            let f (dv: dval) : bool =
            match fn [dv] with
            | DBool b -> b
            | dv -> fail [dv]
            in
            DList (List.filter ~f l)
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
          | [DList l; DInt c] ->
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
          | [DList l; DInt c] ->
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
          | [DList l; DBlock fn] ->
            let f (dv: dval) : dval = fn [dv]
            in
            DList (List.map ~f l)
          | args -> fail args)
  ; pr = Some list_preview
  ; ps = true
  }
  ;


  (* ====================================== *)
  (* Hacks for autocomplete*)
  (* ====================================== *)


  { pns = ["if"]
  ; ins = []
  ; p = [par "cond" TBool]
  ; r = TAny
  ; d = "If cond is true, executes the `then` expression. Otherwise runs the `else` expression."
  ; f = InProcess (fun _ -> failwith "If is a placeholer, we shouldn't be calling it" )
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["lambda"]
  ; ins = []
  ; p = [par "vars" TList; par "body" TAny]
  ; r = TAny
  ; d = "Creates an anonymous function, useful for iterating through foreach"
  ; f = InProcess (fun _ -> failwith "Lambda is a placeholer, we shouldn't be calling it" )
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["let"]
  ; ins = []
  ; p = [par "bindings" TList; par "body" TAny]
  ; r = TAny
  ; d = "Execute and bind the variables in binding, and then execute body, possibly using those expressions. Execution is strict."
  ; f = InProcess (fun _ -> failwith "Let is a placeholer, we shouldn't be calling it" )
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["true"]
  ; ins = []
  ; p = []
  ; r = TBool
  ; d = "The true value"
  ; f = InProcess (fun _ -> failwith "True is a placeholer, we shouldn't be calling it" )
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["false"]
  ; ins = []
  ; p = []
  ; r = TBool
  ; d = "The false value"
  ; f = InProcess (fun _ -> failwith "False is a placeholer, we shouldn't be calling it" )
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["null"]
  ; ins = []
  ; p = []
  ; r = TAny
  ; d = "The null value"
  ; f = InProcess (fun _ -> failwith "Null is a placeholer, we shouldn't be calling it" )
  ; pr = None
  ; ps = true
  }
  ;


  (* ====================================== *)
  (* Date *)
  (* ====================================== *)
  (* Different format than we use in postgres, this was useful for twitter *)
  (* pns n = ["Date::parse"] *)
  (* ins o = [] *)
  (* ; p = [par "s" TStr] *)
  (* ; r = TInt *)
  (* ; d = "Parses a time string, and return the number of seconds since the epoch (midnight, Jan 1, 1970)" *)
  (* ; f = InProcess *)
  (*       (function *)
  (*         | [DStr s] -> *)
  (*             (try *)
  (*               DInt (s *)
  (*                     |> Unix.strptime ~fmt:"%a %b %d %H:%M:%S %z %Y" *)
  (*                     |> Unix.timegm *)
  (*                     |> int_of_float *)
  (*                     ) *)
  (*             with e -> raise (TypeError [DStr "Invalid date format"])) *)
  (*         | args -> fail args) *)
  (* ; pr = None *)
  (* ; ps = true *)
  (* } *)
  (* ; *)
  (*  *)

  { pns = ["Date::now"]
  ; ins = []
  ; p = []
  ; r = TDate
  ; d = "Returns the number of seconds since the epoch (midnight, Jan 1, 1970)"
  ; f = InProcess
        (function
          | [] -> DDate (Time.now ())
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;


  { pns = ["Date::add"]
  ; ins = []
  ; p = [par "d" TDate; par "seconds" TInt]
  ; r = TDate
  ; d = "Returns a new Date `seconds` seconds after `d`"
  ; f = InProcess
        (function
          | [DDate d; DInt s] -> DDate (Time.add d (Time.Span.of_int_sec s))
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
          | [DDate d; DInt s] -> DDate (Time.add d (Time.Span.of_int_sec s))
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["Date::less_than"]
  ; ins = ["Date::<"]
  ; p = [par "d1" TDate; par "d2" TDate]
  ; r = TBool
  ; d = "Returns whetheer `d1` < ` d2`"
  ; f = InProcess
        (function
          | [DDate d1; DDate d2] ->
            DBool (Time.(<) d1 d2)
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
          | [DChar c] -> DInt (Char.to_int c)
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
          | [DInt i] -> DChar (Char.of_int_exn i)
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
          | [DChar c] -> DChar (Char.lowercase c)
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
          | [DChar c] -> DChar (Char.uppercase c)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

]
