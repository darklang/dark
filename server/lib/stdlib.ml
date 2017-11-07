open Core
open Runtime
open Lib

module B = Batteries

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
  (* { n = "Page::page" *)
  (* ; o = [] *)
  (* ; p = ["url"; "outputs"] *)
  (* ; f = function *)
  (*     | args -> expected "this to be implmented" args *)
  (* } *)
  (* ; *)


  (* ====================================== *)
  (* Dict  *)
  (* ====================================== *)

  { n = "Dict::keys"
  ; o = []
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
  ; pu = true
  }
  ;


  { n = "Dict::values"
  ; o = []
  ; p = [par "dict" TObj]
  ; r = TList
  ; d = ""
  ; f = InProcess
        (function
          | [DObj o] -> DList (DvalMap.data o)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;




  { n = "."
  ; o = ["get_field"]
  ; p = [par "value" TObj; par "fieldname" TStr]
  ; r = TAny
  ; d = ""
  ; f = InProcess
        (function
          | [DObj value; DStr fieldname] ->
            (match DvalMap.find value fieldname with
             | None -> raise_error
                         ("Value has no field named: " ^ fieldname)
                         ~actual:(DObj value)

             | Some v -> v)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  (* ====================================== *)
  (* Int *)
  (* ====================================== *)
  { n = "%"
  ; o = ["Int::mod"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = ""
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a mod b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "+"
  ; o = ["Int::add"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Adds two integers together"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a + b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "-"
  ; o = ["Int::subtract"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Subtracts two integers"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a - b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "*"
  ; o = ["Int::multiply"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Multiples two integers"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a * b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;

  { n = "^"
  ; o = ["Int::pow"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Raise a to the bth power"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (B.Int.pow a b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "/"
  ; o = ["Int::divide"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Divides two integers"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a / b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = ">"
  ; o = ["Int::greaterThan"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is greater than b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a > b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "<"
  ; o = ["Int::lessThan"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is less than b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a < b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;

  { n = "<="
  ; o = ["Int::lessThanOrEqualTo"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is less than or equal to b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a <= b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = ">="
  ; o = ["Int::greaterThanOrEqualTo"]
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TBool
  ; d = "Returns true if a is greater than or equal to b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a >= b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Int::random"
  ; o = []
  ; p = [par "a" TInt ; par "b" TInt]
  ; r = TInt
  ; d = "Returns a random integer between a and b (inclusive)"
  ; f = InProcess
        (function
          (* +1 as Random.int is exclusive *)
          | [DInt a; DInt b] -> DInt (a + 1 + (Random.int (b - a)))
          | args -> fail args)
  ; pr = None
  ; pu = false
  }
  ;

  { n = "Int::sqrt"
  ; o = []
  ; p = [par "a" TInt]
  ; r = TFloat
  ; d = "Get the square root of an Int"
  ; f = InProcess
        (function
          | [DInt a] -> DFloat (float_of_int a |> sqrt)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Int::toFloat"
  ; o = []
  ; p = [par "a" TInt]
  ; r = TFloat
  ; d = "Converts an Int to a Float"
  ; f = InProcess
        (function
          | [DInt a] -> DFloat (float a)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;

  (* ====================================== *)
  (* Floats *)
  (* ====================================== *)


  { n = "Float::ceiling"
  ; o = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round above to an integer value"
  ; f = InProcess
        (function
          | [DFloat a] -> DInt (Float.round_up a |> int_of_float)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Float::floor"
  ; o = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round down to an integer value"
  ; f = InProcess
        (function
          | [DFloat a] -> DInt (Float.round_down a |> int_of_float)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Float::round"
  ; o = []
  ; p = [par "a" TFloat]
  ; r = TInt
  ; d = "Round to nearest integer value"
  ; f = InProcess
        (function
          | [DFloat a] -> DInt (Float.round a |> int_of_float)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Float::sqrt"
  ; o = []
  ; p = [par "a" TFloat]
  ; r = TFloat
  ; d = "Get the square root of a float"
  ; f = InProcess
        (function
          | [DFloat a] -> DFloat (sqrt a)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Int::sum"
  ; o = []
  ; p = [par "a" TList]
  ; r = TInt
  ; d = "Returns the sum of all the ints in the list"
  ; f = InProcess
        (function
          | [DList l] ->
            l
            |> list_coerce ~f:to_int
            >>| List.fold_left ~f:(+) ~init:0
            >>| (fun x -> DInt x)
            |> Result.map_error ~f:(fun (result, example_value) ->
                error
                  ~actual:(DList result)
                  ~result:(DList result)
                  ~long:("Int::sum requires all values to be integers, but " ^ (to_repr example_value) ^ " is a " ^ (tipename example_value))
                  ~expected:"every list item to be an int "
                  "Sum expects you to pass a list of ints")
            |> Result.ok_exn
          | args -> fail args)
  ; pr = None
  ; pu = false
  }
  ;


  (* ====================================== *)
  (* Any *)
  (* ====================================== *)
  { n = "toString"
  ; o = []
  ; p = [par "v" TAny]
  ; r = TStr
  ; d = "Returns a string representation of `v`"
  ; f = InProcess
        (function
          | [a] -> DStr (to_repr a)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "=="
  ; o = ["equals"]
  ; p = [par "a" TAny; par "b" TAny]
  ; r = TBool
  ; d = "Returns true if the two value are equal"
  ; f = InProcess
        (function
          | [a; b] -> DBool (equal_dval a b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "!="
  ; o = ["notEquals"]
  ; p = [par "a" TAny; par "b" TAny]
  ; r = TBool
  ; d = "Returns true if the two value are not equal"
  ; f = InProcess
        (function
          | [a; b] -> DBool (not (equal_dval a b))
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  (* ====================================== *)
  (* Bool *)
  (* ====================================== *)
  { n = "Bool::not"
  ; o = []
  ; p = [par "b" TBool]
  ; r = TBool
  ; d = ""
  ; f = InProcess
        (function
          | [DBool b] -> DBool (not b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "&&"
  ; o = ["Bool::and"]
  ; p = [par "a" TBool ; par "b" TBool]
  ; r = TBool
  ; d = "Returns true if both a and b are true"
  ; f = InProcess
        (function
          | [DBool a; DBool b] -> DBool (a && b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "||"
  ; o = ["Bool::or"]
  ; p = [par "a" TBool ; par "b" TBool]
  ; r = TBool
  ; d = "Returns true if either a is true or b is true"
  ; f = InProcess
        (function
          | [DBool a; DBool b] -> DBool (a || b)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "_"
  ; o = []
  ; p = [par "ignore" TAny; par "value" TAny]
  ; r = TAny
  ; d = "Ignores the first param and returns the 2nd."
  ; f = InProcess
        (function
          | [_; value] -> value
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;




  (* ====================================== *)
  (* String *)
  (* ====================================== *)
  { n = "String::foreach"
  ; o = []
  ; p = [par "s" TStr; func ["char"]]
  ; r = TStr
  ; d = "Iterate over each character in the string, performing the operation in the block on each one"
  ; f = InProcess
        (function
          | [DStr s; DBlock (id, fn)] ->
            s
            |> String.to_list
            |> List.map ~f:(fun c -> fn [(DChar c)])
            |> list_coerce ~f:to_char
            >>| String.of_char_list
            >>| (fun x -> DStr x)
            |> Result.map_error ~f:(fun (result, example_value) ->
                error
                  ~actual:(DList result)
                  ~result:(DList result)
                  ~long:("String::foreach needs to get chars back in order to reassemble them into a string. The values returned by your code are not chars, for example " ^ (to_repr example_value) ^ " is a " ^ (tipename example_value))
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
  ; pu = true
  }
  ;

  { n = "String::toList"
  ; o = []
  ; p = [par "s" TStr]
  ; r = TList
  ; d = "Returns the list of characters in the string"
  ; f = InProcess
        (function
          | [DStr s] ->
              DList (String.to_list s |> List.map ~f:(fun c -> DChar c))
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "String::length"
  ; o = []
  ; p = [par "s" TStr]
  ; r = TInt
  ; d = "Returns the length of the string"
  ; f = InProcess
        (function
          | [DStr s] -> DInt (String.length s)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "String::append"
  ; o = []
  ; p = [par "s1" TStr; par "s2" TStr]
  ; r = TStr
  ; d = "Concatenates the two strings and returns the joined string"
  ; f = InProcess
        (function
          | [DStr s1; DStr s2] -> DStr (s1 ^ s2)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;




  { n = "String::join"
  ; o = []
  ; p = [par "l" TList; par "separator" TStr]
  ; r = TStr
  ; d = "Combines a list of strings with the provided separator"
  ; f = InProcess
        (function
          | [DList l; DStr sep] ->
            let s = List.map ~f:(fun s ->
                match s with
                | DStr st -> st
                | _  -> to_repr s) l
            in
            DStr (String.concat ~sep s)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "String::fromList"
  ; o = []
  ; p = [par "l" TList]
  ; r = TStr
  ; d = "Returns the list of characters as a string"
  ; f = InProcess
        (function
          | [DList l] ->
              DStr (l |> List.map ~f:(function
                                      | DChar c -> c
                                      | dv -> raise_error ~actual:dv "expected a char")
                      |> String.of_char_list)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "String::fromChar"
  ; o = []
  ; p = [par "c" TChar]
  ; r = TChar
  ; d = "Converts a char to a string"
  ; f = InProcess
        (function
          | [DChar c] -> DStr (Char.to_string c)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  (* ====================================== *)
  (* List *)
  (* ====================================== *)
  { n = "List::head"
  ; o = []
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
  ; pu = true
  }
  ;


  { n = "List::empty"
  ; o = []
  ; p = []
  ; r = TList
  ; d = ""
  ; f = InProcess
        (function
          | [] -> DList []
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;

  { n = "List::new"
  ; o = []
  ; p = [ par ~opt:true "i1" TAny
        ; par ~opt:true "i2" TAny
        ; par ~opt:true "i3" TAny
        ; par ~opt:true "i4" TAny
        ; par ~opt:true "i5" TAny
        ; par ~opt:true "i6" TAny]
  ; r = TList
  ; d = "Return a new list with the arguments provided"
  ; f = InProcess
        (function
          | args -> DList (List.filter ~f:(fun x -> x <> DIncomplete && x <> DNull) args))
  ; pr = None
  ; pu = true
  }
  ;


  { n = "List::push"
  ; o = []
  ; p = [par "val" TAny; par "list" TList]
  ; r = TList
  ; d = ""
  ; f = InProcess
        (function
          | [i; DList l] -> DList (i :: l)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "List::last"
  ; o = []
  ; p = [par "list" TList]
  ; r = TAny
  ; d = ""
  ; f = InProcess
        (function
          | [DList l] -> List.last_exn l
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;

  { n = "List::find_first"
  ; o = []
  ; p = [par "l" TList; func ["val"]]
  ; r = TList
  ; d = "Find the first element of the list, for which `f` returns true"
  ; f = InProcess
        (function
          | [DList l; DBlock (id, fn)] ->
            (let f (dv: dval) : bool = DBool true = fn [dv]
            in
            match List.find ~f l with
            | None -> DNull
            | Some dv -> dv)
        | args -> fail args)
  ; pr = Some list_preview
  ; pu = true
  }
  ;


  { n = "List::contains"
  ; o = []
  ; p = [par "l" TList; par "val" TAny]
  ; r = TBool
  ; d = "Returns if the value is in the list"
  ; f = InProcess
        (function
          | [DList l; i] -> DBool (List.mem ~equal:equal_dval l i)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "List::repeat"
  ; o = []
  ; p = [par "times" TInt; par "val" TAny]
  ; r = TList
  ; d = "Returns a list containing `val` repeated `count` times"
  ; f = InProcess
        (function
          | [DInt t; dv] -> DList (List.init t ~f:(fun _ -> dv))
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "List::length"
  ; o = []
  ; p = [par "l" TList]
  ; r = TInt
  ; d = "Returns the length of the list"
  ; f = InProcess
        (function
          | [DList l] -> DInt (List.length l)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "List::range"
  ; o = []
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
  ; pu = true
  }
  ;




  { n = "List::fold"
  ; o = []
  ; p = [par "l" TList; par "init" TAny; func ["accum"; "curr"]]
  ; r = TAny
  ; d = "Folds the list into a single value, by repeatedly apply `f` to any two pairs"
  ; f = InProcess
        (function
          | [DList l; init; DBlock (_, fn)] ->
            let f (dv1: dval) (dv2: dval) : dval = fn [dv1; dv2] in
            List.fold ~f ~init l
          | args -> fail args)
  ; pr = Some
        (fun dv cursor ->
          match dv with
          | [DList l; init; DBlock (_, fn)] ->
            let short_l = List.take l cursor in
            let f = fun (accum:dval) (elt:dval) -> fn([accum; elt]) in
            let end_accum = List.fold ~f ~init short_l in
            let next_elt =
              (match List.nth l cursor with
              | Some elt -> elt
              | None -> DIncomplete)
            in [end_accum; next_elt]
          | args -> [DIncomplete])
  ; pu = true
  }
  ;


  { n = "List::flatten"
  ; o = []
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
  ; pu = true
  }
  ;


  { n = "List::append"
  ; o = []
  ; p = [par "l1" TList; par "l2" TList]
  ; r = TList
  ; d = "Returns the combined list of `l1` and `l2`"
  ; f = InProcess
        (function
          | [DList l1; DList l2] -> DList (List.append l1 l2)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;



  { n = "List::filter"
  ; o = []
  ; p = [par "l" TList; func ["val"]]
  ; r = TList
  ; d = "Return only values in `l` which meet the function's criteria"
  ; f = InProcess
        (function
          | [DList l; DBlock (id, fn)] ->
            let f (dv: dval) : bool =
            match fn [dv] with
            | DBool b -> b
            | dv -> fail [dv]
            in
            DList (List.filter ~f l)
          | args -> fail args)
  ; pr = Some list_preview
  ; pu = true
  }
  ;


  { n = "List::foreach"
  ; o = []
  ; p = [par "l" TList; func ["val"]]
  ; r = TList
  ; d = "Call `f` on every item in the list, returning a list of the results of
  those calls"
  ; f = InProcess
        (function
          | [DList l; DBlock (_, fn)] ->
            let f (dv: dval) : dval = fn [dv]
            in
            DList (List.map ~f l)
          | args -> fail args)
  ; pr = Some list_preview
  ; pu = true
  }
  ;


  { n = "if"
  ; o = []
  ; p = [par "v" TAny; par "cond" TBool; func ~name:"ftrue" ["then"]; func ~name:"ffalse" ["else"]]
  ; r = TAny
  ; d = "If cond is true, calls the `then` function. Otherwise calls the `else`
  function. Both functions get 'v' piped into them"
  ; f = InProcess
        (function
          | [v; DBool cond; DBlock (_, fntrue); DBlock (_, fnfalse)] ->
              if cond then fntrue [v] else fnfalse [v]
          | args -> fail args)
  (* we could do better here by getting a value for which this is true/false *)
  ; pr = Some (fun dv cursor -> dv)
  ; pu = true
  }
  ;





  (* ====================================== *)
  (* Date *)
  (* ====================================== *)
  { n = "Date::parse"
  ; o = []
  ; p = [par "s" TStr]
  ; r = TInt
  ; d = "Parses a time string, and return the number of seconds since the epoch (midnight, Jan 1, 1970)"
  ; f = InProcess
        (function
          | [DStr s] ->
              (try
                DInt (s
                      |> Unix.strptime ~fmt:"%a %b %d %H:%M:%S %z %Y"
                      |> Unix.timegm
                      |> int_of_float
                      )
              with e -> raise (TypeError [DStr "Invalid date format"]))
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Date::now"
  ; o = []
  ; p = []
  ; r = TInt
  ; d = "Returns the number of seconds since the epoch (midnight, Jan 1, 1970)"
  ; f = InProcess
        (function
          | [] ->
              DInt (Unix.time ()
                    |> int_of_float
                    )
          | args -> fail args)
  ; pr = None
  ; pu = false
  }
  ;


  (* ====================================== *)
  (* Char *)
  (* ====================================== *)
  { n = "Char::toASCIICode"
  ; o = []
  ; p = [par "c" TChar]
  ; r = TInt
  ; d = "Return `c`'s ASCII code"
  ; f = InProcess
        (function
          | [DChar c] -> DInt (Char.to_int c)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Char::toASCIIChar"
  ; o = []
  ; p = [par "i" TInt]
  ; r = TChar
  ; d = ""
  ; f = InProcess
        (function
          | [DInt i] -> DChar (Char.of_int_exn i)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Char::toLowercase"
  ; o = []
  ; p = [par "c" TChar]
  ; r = TChar
  ; d = "Return the lowercase value of `c`"
  ; f = InProcess
        (function
          | [DChar c] -> DChar (Char.lowercase c)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Char::toUppercase"
  ; o = []
  ; p = [par "c" TChar]
  ; r = TChar
  ; d = "Return the uppercase value of `c`"
  ; f = InProcess
        (function
          | [DChar c] -> DChar (Char.uppercase c)
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  (* ====================================== *)
  (* Page *)
  (* ====================================== *)
  { n = "Page::GET"
  ; o = []
  ; p = [par "url" TStr; par "val" TAny]
  ; r = TNull
  ; d = "Create a page at `url` that prints `value`"
  ; f = InProcess
        (function
          | [DStr url; value] -> value
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


  { n = "Page::POST"
  ; o = []
  ; p = [par "url" TStr; par "val" TAny]
  ; r = TNull
  ; d = "Create a page at `url` that prints `value`"
  ; f = InProcess
        (function
          | [DStr url; value] -> value
          | args -> fail args)
  ; pr = None
  ; pu = true
  }
  ;


]
