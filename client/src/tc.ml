(* This file is a local variant of Tablecloth. It allows us to add, modify,
 * etc, tablecloth function and type definitions, to be upstreamed later. *)
module Caml = struct
  module String = String
  module List = List
end

(* This allows us override submodules. See
 * http://gallium.inria.fr/blog/overriding-submodules/ *)
include (
  Tablecloth :
    module type of Tablecloth
    with module StrDict := Tablecloth.StrDict
     and module IntDict := Tablecloth.IntDict )

(* Everything is upstreamed, but keep this here to show us how. *)
module StrDict = struct
  include Tablecloth.StrDict
end

module IntDict = struct
  include Tablecloth.IntDict
end
