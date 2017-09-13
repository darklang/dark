open Core
open Runtime
open Lib

let fns : Lib.shortfn list = [
  (* { n = "Page::page" *)
  (* ; o = [] *)
  (* ; p = ["url"; "outputs"] *)
  (* ; f = function *)
  (*     | args -> expected "this to be implmented" args *)
  (* } *)
  (* ; *)
  { n = "Dict::keys"
  ; o = []
  ; p = [req "dict" tObj]
  ; r = tList
  ; d = ""
  ; f = InProcess
        (function
          | [DObj o] -> o
                        |> DvalMap.keys
                        |> List.map ~f:(fun k -> DStr k)
                        |> fun l -> DList l
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "List::head"
  ; o = []
  ; p = [req "list" tList]
  ; r = tAny
  ; d = ""
  ; f = InProcess
        (function
          | [DList l] -> List.hd_exn l
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "."
  ; o = ["get_field"]
  ; p = [req "value" tObj; req "fieldname" tStr]
  ; r = tAny
  ; d = ""
  ; f = InProcess
        (function
          | [DObj value; DStr fieldname] ->
            (match DvalMap.find value fieldname with
             | None -> Exception.raise ("Value has no field named: " ^ fieldname)
             | Some v -> v)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "%"
  ; o = ["Int::mod"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tInt
  ; d = ""
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a mod b)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "+"
  ; o = ["Int::add"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tInt
  ; d = "Adds two integers together"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a + b)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "-"
  ; o = ["Int::subtract"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tInt
  ; d = "Subtracts two integers"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a - b)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "*"
  ; o = ["Int::multiply"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tInt
  ; d = "Multiples two integers"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a * b)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "/"
  ; o = ["Int::divide"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tInt
  ; d = "Divides two integers"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a / b)
          | args -> fail args)
  ;  pr = None
  }
  ;

  { n = ">"
  ; o = ["Int::greaterThan"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tBool
  ; d = "Returns true if a is greater than b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a > b)
          | args -> fail args)
  ;  pr = None
  }
  ;

  { n = "<"
  ; o = ["Int::lessThan"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tBool
  ; d = "Returns true if a is less than b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a < b)
          | args -> fail args)
  ;  pr = None
  }
  ;

  { n = "<="
  ; o = ["Int::lessThanOrEqualTo"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tBool
  ; d = "Returns true if a is less than or equal to b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a <= b)
          | args -> fail args)
  ;  pr = None
  }
  ;

  { n = ">="
  ; o = ["Int::greaterThanOrEqualTo"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tBool
  ; d = "Returns true if a is greater than or equal to b"
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DBool (a >= b)
          | args -> fail args)
  ;  pr = None
  }
  ;

  { n = "&&"
  ; o = ["Bool::and"]
  ; p = [req "a" tBool ; req "b" tBool]
  ; r = tBool
  ; d = "Returns true if both a and b are true"
  ; f = InProcess
        (function
          | [DBool a; DBool b] -> DBool (a && b)
          | args -> fail args)
  ;  pr = None
  }
  ;

  { n = "||"
  ; o = ["Bool::or"]
  ; p = [req "a" tBool ; req "b" tBool]
  ; r = tBool
  ; d = "Returns true if either a is true or b is true"
  ; f = InProcess
        (function

          | [DBool a; DBool b] -> DBool (a || b)
          | args -> fail args)
  ;  pr = None
  }
  ;


  { n = "String::foreach"
  ; o = []
  ; p = [req "s" tStr; req "f" tFun]
  ; r = tStr
  ; d = "Run `f` on every character in the string, and combine them back into a
  string"
  ; f = InProcess
        (function
          | [DStr s; DAnon (id, fn)] ->
            let charf (c: char) : char =
              let result = fn [(DChar c)] in
              match result with
              | DChar c -> c
              | r -> Exception.raise "expected a char"
            in
            DStr (String.map ~f:charf s)
          | args -> fail args)
  ; pr = Some
        (function
          | [DStr s; _] ->
              if s = ""
              then DChar 'l'
              else DChar (String.get s 0)
          | args -> DIncomplete)
  }
  ;
  { n = "List::find_first"
  ; o = []
  ; p = [req "l" tList; req "f" tFun]
  ; r = tList
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
  ; pr = Some
        (function
          | [DList (i :: _); _] -> i
          | args -> DIncomplete)
  }
  ;

  { n = "List::contains"
  ; o = []
  ; p = [req "l" tList; req "item" tAny]
  ; r = tBool
  ; d = "Returns if the item is in the list"
  ; f = InProcess
        (function
          | [DList l; i] -> DBool (List.mem ~equal:equal_dval l i)
          | args -> fail args)
  ; pr = None
  }
  ;

  { n = "List::filter"
  ; o = []
  ; p = [req "l" tList; req "f" tFun]
  ; r = tList
  ; d = "Return only items in list which meet function criteria"
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
  ; pr = Some
        (function
          | [DList l; _] ->
              (match List.hd l with
              | Some dv -> dv
              | None -> DIncomplete)
          | args -> DIncomplete)
  }
  ;


  { n = "List::foreach"
  ; o = []
  ; p = [req "l" tList; req "f" tFun]
  ; r = tList
  ; d = "Call `f` on every item in the list, returning a list of the results of
  those calls"
  ; f = InProcess
        (function
          | [DList l; DAnon (id, fn)] ->
            let f (dv: dval) : dval = fn [dv]
            in
            DList (List.map ~f l)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "Char::code"
  ; o = []
  ; p = [req "c" tChar]
  ; r = tChar
  ; d = "Return `c`'s ASCII code"
  ; f = InProcess
        (function
          | [DChar c] -> DInt (Char.to_int c)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "Char::to_uppercase"
  ; o = []
  ; p = [req "c" tChar]
  ; r = tChar
  ; d = "Return the uppercase value of `c`"
  ; f = InProcess
        (function
          | [DChar c] -> DChar (Char.uppercase c)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "Date::parse"
  ; o = []
  ; p = [req "s" tStr]
  ; r = tInt
  ; d = "Parses a time string, and return the number of seconds since the epoch (midnight, Jan 1, 1970)"
  ; f = InProcess
        (function
          | [DStr s] ->
              DInt (s
                    |> Unix.strptime ~fmt:"%a %b %d %H:%M:%S %z %Y"
                    |> Util.inspect "strptime"
                    |> Unix.timegm
                    |> Util.inspect "timegm"
                    |> int_of_float
                    )
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "Date::now"
  ; o = []
  ; p = []
  ; r = tInt
  ; d = "Returns the number of seconds since the epoch (midnight, Jan 1, 1970)"
  ; f = InProcess
        (function
          | [] ->
              DInt (Unix.time ()
                    |> int_of_float
                    )
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "Char::chr"
  ; o = []
  ; p = [req "i" tInt]
  ; r = tChar
  ; d = ""
  ; f = InProcess
        (function
          | [DInt i] -> DChar (Char.of_int_exn i)
          | args -> fail args)
  ; pr = None
  }
  ;
  { n = "Bool::not"
  ; o = []
  ; p = [req "b" tBool]
  ; r = tBool
  ; d = ""
  ; f = InProcess
        (function
          | [DBool b] -> DBool (not b)
          | args -> fail args)
  ; pr = None
 }
]
