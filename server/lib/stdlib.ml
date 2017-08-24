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
  }
  ;
  { n = "+"
  ; o = ["Int::add"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tInt
  ; d = ""
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a + b)
          | args -> fail args)
  }
  ;
  { n = "-"
  ; o = ["Int::sub"]
  ; p = [req "a" tInt ; req "b" tInt]
  ; r = tInt
  ; d = ""
  ; f = InProcess
        (function
          | [DInt a; DInt b] -> DInt (a - b)
          | args -> fail args)
  }
  ;
  { n = "String::foreach"
  ; o = []
  ; p = [req "s" tStr; req "f" tFun]
  ; r = tStr
  ; d = ""
  ; f = InProcess
        (function
          | [DStr s; DAnon (id, fn)] ->
            let charf (c: char) : char =
              let result = fn (DChar c) in
              match result with
              | DChar c -> c
              | r -> failwith "expected a char"
            in
            DStr (String.map ~f:charf s)
          | args -> fail args)
  }
  ;
  { n = "Char::code"
  ; o = []
  ; p = [req "c" tChar]
  ; r = tChar
  ; d = ""
  ; f = InProcess
        (function
          | [DChar c] -> DInt (Char.to_int c)
          | args -> fail args)
  }
  ;
  { n = "Char::to_uppercase"
  ; o = []
  ; p = [req "c" tChar]
  ; r = tChar
  ; d = ""
  ; f = InProcess
        (function
          | [DChar c] -> DChar (Char.uppercase c)
          | args -> fail args)
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
  }
]
