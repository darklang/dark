open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns =
  [ { pns = ["Dict::keys"]
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
            ( match DvalMap.get o ~key:(Unicode_string.to_string s) with
            | Some d ->
                d
            | None ->
                DNull )
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["Dict::get_v1"]
    ; ins = []
    ; p = [par "dict" TObj; par "key" TStr]
    ; r = TOption
    ; d = "Looks up `key` in object `dict` and returns an option"
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
            ( match DvalMap.get o ~key:(Unicode_string.to_string s) with
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
    ; p = [par "dict" TObj; func ["val"]]
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
    ; dep = true }
  ; { pns = ["Dict::map"]
    ; ins = []
    ; p = [par "dict" TObj; func ["key"; "value"]]
    ; r = TObj
    ; d =
        "Iterates each `key` and `value` in Dictionary `dict` and mutates it according to the provided lambda"
    ; f =
        InProcess
          (function
          | _, [DObj o; DBlock fn] ->
              let f ~key ~(data : dval) : dval =
                fn [Dval.dstr_of_string_exn key; data]
              in
              DObj (Map.mapi ~f o)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::filter"]
    ; ins = []
    ; p = [par "dict" TObj; func ["key"; "value"]]
    ; r = TObj
    ; d =
        "Return only values in `dict` which meet the function's criteria. The function should return true to keep the entry or false to remove it."
    ; f =
        InProcess
          (function
          | _, [DObj o; DBlock fn] ->
              let incomplete = ref false in
              let f ~(key : string) ~(data : dval) : bool =
                match fn [Dval.dstr_of_string_exn key; data] with
                | DBool b ->
                    b
                | DIncomplete ->
                    incomplete := true ;
                    false
                | v ->
                    RT.error
                      "Expecting fn to return bool"
                      ~result:v
                      ~actual:data
              in
              if !incomplete then DIncomplete else DObj (Base.Map.filteri ~f o)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::empty"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Return an empty dictionary"
    ; f =
        InProcess (function _, [] -> DObj DvalMap.empty | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::merge"]
    ; ins = []
    ; p = [par "left" TObj; par "right" TObj]
    ; r = TObj
    ; d =
        "Return a combined dictionary with both dictionaries' keys and values. If the same key exists in both `left` and `right`, then use the value from `right`"
    ; f =
        InProcess
          (function
          | _, [DObj l; DObj r] ->
              DObj (Util.merge_right l r)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["Dict::toJSON"]
    ; ins = []
    ; p = [par "dict" TObj]
    ; r = TStr
    ; d = "Dumps `dict` to a JSON string"
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
    ; dep = false } ]
