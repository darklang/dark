(* This file is a local variant of Tablecloth. It allows us to add, modify,
 * etc, tablecloth function and type definitions, to be upstreamed later. *)

(* This allows us override submodules. See
 * http://gallium.inria.fr/blog/overriding-submodules/ *)
include (
  Tablecloth :
    module type of Tablecloth
    with module StrSet := Tablecloth.StrSet
     and module IntSet := Tablecloth.IntSet )

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
