open Core
open Runtime
open Lib

let list_repeat = Util.list_repeat

let list_preview =
  fun dv count -> match dv with
                   | [DList l; _] -> [List.take l count]
                   | args -> [list_repeat count DIncomplete]

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


  { n = "."
  ; o = ["get_field"]
  ; p = [par "value" TObj; par "fieldname" TStr]
  ; r = TAny
  ; d = ""
  ; f = InProcess
        (function
          | [DObj value; DStr fieldname] ->
            (match DvalMap.find value fieldname with
             | None -> error
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
          | [DStr s; DAnon (id, fn)] ->
            let all_chars = ref true in
            let example_value = ref DIncomplete in
            let result = s
              |> String.to_list
              |> List.map ~f:(fun c -> match fn [(DChar c)] with
                                       | DChar c -> DChar c
                                       | dv ->
                                           if !all_chars
                                           then
                                             (all_chars := false;
                                             example_value := dv);
                                           dv) in
            if !all_chars
            then DStr (result
                       |> List.map ~f:(function
                                         | DChar c -> c
                                         | _ -> Exception.internal "char?")
                       |> String.of_char_list)
            else
              error
                ~actual:(DList result)
                ~result:(DList result)
                ~long:("String::foreach needs to get chars back in order to reassemble them into a string. The values returned by your code are not chars, for example " ^ (to_repr !example_value) ^ " is a " ^ (get_type !example_value))
                ~expected:"every value to be a char"
                "Foreach expects you to return chars"
          | args -> fail args)
  ; pr = Some
        (fun dv count ->
          match dv with
          | [DStr s; _] ->
              let s = (if s = "" then "example" else s) in
              s
              |> String.to_list
              |> (fun l -> List.take l count)
              |> List.map ~f:(fun c -> DChar c)
              |> fun x -> [x]
          | args -> [list_repeat count DIncomplete])
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
                                      | dv -> error ~actual:dv "expected a char")
                      |> String.of_char_list)
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
          | [DList l] -> List.hd_exn l
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
          | [DList l; DAnon (id, fn)] ->
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
  ; p = [par "start" TInt; par "stop" TInt]
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
  ; p = [par "l" TList; par "init" TAny; func ["new"; "old"]]
  ; r = TAny
  ; d = "Folds the list into a single value, by repeatedly apply `f` to any two pairs"
  ; f = InProcess
        (function
          | [DList l; init; DAnon (_, fn)] ->
            let f (dv1: dval) (dv2: dval) : dval = fn [dv1; dv2] in
            List.fold ~f ~init l
          | args -> fail args)
  ; pr = Some
        (fun dv count ->
          match dv with
          | [DList l; init; DAnon (_, fn)] ->
            let l = List.take l count in
            let f (dv1, (i1, i2)) (dv2) : (dval * (dval list * dval list)) =
              (fn [dv1; dv2], (dv1 :: i1, dv2 :: i2)) in
            let (_, (i1, i2)) = List.fold ~f ~init:(init, ([], [])) l in
            [List.rev i1; List.rev i2]
          | args -> [ list_repeat count DIncomplete
                    ; list_repeat count DIncomplete])
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
          | [DList l; DAnon (id, fn)] ->
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
          | [DList l; DAnon (_, fn)] ->
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
          | [v; DBool cond; DAnon (_, fntrue); DAnon (_, fnfalse)] ->
              if cond then fntrue [v] else fnfalse [v]
          | args -> fail args)
  (* we could do better here by getting a value for which this is true/false *)
  ; pr = Some (fun dv count -> [dv])
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

]
