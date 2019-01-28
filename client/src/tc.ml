(* This file is a local variant of Tablecloth. It allows us to add, modify,
 * etc, tablecloth function and type definitions, to be upstreamed later. *)

(* This allows us override submodules. See
 * http://gallium.inria.fr/blog/overriding-submodules/ *)
include (
  Tablecloth :
    module type of Tablecloth
    with module StrSet := Tablecloth.StrSet
     and module IntSet := Tablecloth.IntSet
     and module StrDict := Tablecloth.StrDict
     and module Option := Tablecloth.Option
     and module List := Tablecloth.List )

module StrSet = struct
  include Tablecloth.StrSet

  let remove ~(value : value) (set : t) = Set.remove set value

  let add ~(value : value) (set : t) = Set.add set value

  let member ~(value : value) (set : t) : bool = Set.has set value

  let set = add

  let has = member

  let pp (fmt : Format.formatter) (set : t) =
    Format.pp_print_string fmt "{ " ;
    Set.forEach set (fun v ->
        Format.pp_print_string fmt v ;
        Format.pp_print_string fmt ",  " ) ;
    Format.pp_print_string fmt "}" ;
    ()
end

module IntSet = struct
  include Tablecloth.IntSet

  let remove ~(value : value) (set : t) = Set.remove set value

  let add ~(value : value) (set : t) = Set.add set value

  let member ~(value : value) (set : t) : bool = Set.has set value

  let set = add

  let has = member

  let pp (fmt : Format.formatter) (set : t) =
    Format.pp_print_string fmt "{ " ;
    Set.forEach set (fun v ->
        Format.pp_print_int fmt v ;
        Format.pp_print_string fmt ",  " ) ;
    Format.pp_print_string fmt "}" ;
    ()
end

module StrDict = struct
  include Tablecloth.StrDict

  (* Js.String.make gives us "[object Object]", so we actually want our own
     toString. Not perfect, but slightly nicer (e.g., for App.ml's
     DisplayAndReportHttpError, info's values are all strings, which this
     handles) *)
  let toString d =
    d
    |> toList
    |> List.map (fun (k, v) -> "\"" ^ k ^ "\": \"" ^ Js.String.make v ^ "\"")
    |> String.join ~sep:", "
    |> fun s -> "{" ^ s ^ "}"
end

module Regex = struct
  let regex s : Js.Re.t = Js.Re.fromStringWithFlags ~flags:"g" s

  let contains ~(re : Js.Re.t) (s : string) : bool = Js.Re.test s re

  let replace (re : string) (repl : string) (str : string) =
    Js.String.replaceByRe (regex re) repl str


  let matches (re : Js.Re.t) (s : string) : Js.Re.result option =
    Js.Re.exec s re
end

module Option = struct
  include Tablecloth.Option

  let toOption ~(sentinel : 'a) (value : 'a) : 'a option =
    if value = sentinel then None else Some value
end

module List = struct
  include Tablecloth.List

  let maximum (l : 'a list) : 'a option = Tablecloth.List.maximum ~list:l

  let elemIndex ~(value : 'a) (l : 'a list) : int option =
    l
    |> Array.of_list
    |> Js.Array.findIndex (( = ) value)
    |> Option.toOption ~sentinel:(-1)
end
