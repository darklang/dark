(* This file is a local variant of Tablecloth. It allows us to add, modify,
 * etc, tablecloth function and type definitions, to be upstreamed later. *)
module Caml = struct
  module String = String
  module List = List
  module Char = Char
end

(* This allows us override submodules. See
 * http://gallium.inria.fr/blog/overriding-submodules/ *)
include (
  Tablecloth :
    module type of Tablecloth
    (* with module StrSet := Tablecloth.StrSet *)
    (*  and module IntSet := Tablecloth.IntSet *)
    (*  and module StrDict := Tablecloth.StrDict *)
    with module Option := Tablecloth.Option
    (*  and module Result := Tablecloth.Result *)
     and module List := Tablecloth.List )

module Option = struct
  include Tablecloth.Option

  let exec ~(f : 'a -> unit) (v : 'a option) : unit =
    match v with Some v -> f v | None -> ()


  let valueExn (value : 'a option) : 'a =
    match value with Some v -> v | None -> raise Not_found
end

module List = struct
  include Tablecloth.List

  let rec dropLeft ~(count : int) (l : 'a list) : 'a list =
    if count <= 0
    then l
    else
      match l with
      | [] ->
          []
      | [_] ->
          []
      | _ :: rest ->
          dropLeft ~count:(count - 1) rest


  let dropRight ~(count : int) (l : 'a list) : 'a list =
    l |> reverse |> dropLeft ~count |> reverse


  let range (start : int) (end_ : int) : 'a list =
    let length = end_ - start in
    if length < 0 then [] else Belt.List.makeBy length (fun i -> i + start)
end

module Tuple3 = struct
  let first (x, _, _) = x

  let second (_, x, _) = x

  let third (_, _, x) = x
end
