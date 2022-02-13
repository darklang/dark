module Tablecloth.Set

(** A {!Set} represents a collection of unique values.

    [Set] is an immutable data structure which means operations like {!Set.add}
    and {!Set.remove} do not modify the data structure, but return a new set
    with the desired changes.

    Unlike OCaml and Bucklescript versions of Sets, F# Sets work on any type
    with the comparison trait, and do not require different modules.
*)

type t<'a when 'a: comparison> = 'a Set

(** {1 Create}

  Specialised versions of the {!empty}, {!singleton}, {!fromList} and {!fromArray} functions available in the {!Set.Int} and {!Set.String} sub-modules.

*)

val empty: 'a t
(** A set with nothing in it.

    Often used as an initial value for functions like {!Array.fold}

    {2 Examples}

    {[
      Array.fold
        Set.empty
        Set.add
        [|'m'; 'i'; 's'; 's'; 'i'; 's'; 's';'i';'p';'p';'i'|]
      |> Set.toArray
      = [|'i'; 'm'; 'p'; 's'|]
    ]}
*)

val singleton: 'a -> 'a t
(** Create a set from a single {!Int}

  {2 Examples}

  {[Set.singleton 7 |> Set.toList = [7]]}
*)

val fromArray: 'a array -> 'a t
(** Create a set from an {!Array}

    {2 Examples}

    {[Set.fromArray [|"Ant"; "Bat"; "Bat"; "Goldfish"|] |> Set.toArray = [|"Ant";"Bat";"Goldfish"|]]}
*)

val from_array: 'a array -> 'a t

val fromList: 'a list -> 'a t
(** Create a set from a {!List}

    {2 Examples}

    {[Set.fromList (module Char) ['A'; 'B'; 'B'; 'G'] |> Set.toList = ['A';'B';'G']]}
*)

val from_list: 'a list -> 'a t

(** {1 Basic operations} *)

val add: 'a t -> 'a -> 'a t
(** Insert a value into a set.

    {2 Examples}

    {[Set.add (Set.Int.fromList [1; 2]) 3 |> Set.toList = [1; 2; 3]]}

    {[Set.add (Set.Int.fromList [1; 2]) 2 |> Set.toList = [1; 2]]}
*)

val remove: 'a t -> 'a -> 'a t
(** Remove a value from a set, if the set doesn't contain the value anyway, returns the original set

    {2 Examples}

    {[Set.remove (Set.Int.fromList [1; 2]) 2 |> Set.toList = [1]]}

    {[
      let originalSet = Set.Int.fromList [1; 2] in
      let newSet = Set.remove orignalSet 3 in
      originalSet = newSet
    ]}
*)

val includes: 'a -> 'a t -> bool
(** Determine if a value is in a set

    {2 Examples}

   {[Set.includes (Set.String.fromList ["Ant"; "Bat"; "Cat"]) "Bat" = true]}
*)

// val ( .?{} ) : 'a t -> 'a -> bool
(** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!includes}

    {b Note} Currently this is only supported by the OCaml syntax.

    {2 Examples}

    {[
      let animals = Set.String.fromList ["Ant"; "Bat"; "Cat"] in

      animals.Set.?{"Emu"} = false
    ]}
 *)

val length: 'a t -> int
(** Determine the number of elements in a set.

    {2 Examples}

    {[Set.length (Set.Int.fromList [1; 2; 3]) = 3]}
*)

val find: f: ('a -> bool) -> 'a t -> 'a option
(** Returns, as an {!Option}, the first element for which [f] evaluates to [true]. If [f] doesn't return [true] for any of the elements [find] will return [None].

    {2 Examples}

    {[Set.find Int.isEven (Set.Int.fromList [1; 3; 4; 8]) = Some 4]}

    {[Set.find Int.isOdd (Set.Int.fromList [0; 2; 4; 8]) = None]}

    {[Set.find Int.isEven Set.Int.empty = None]}
*)

(** {1 Query} *)

val isEmpty: 'a t -> bool
(** Check if a set is empty.

    {2 Examples}

    {[Set.isEmpty (Set.Int.empty) = true]}

    {[Set.isEmpty (Set.Int.singleton 4) = false]}
*)

val is_empty: 'a t -> bool

val any: f: ('a -> bool) -> 'a t -> bool
(** Determine if [f] returns true for [any] values in a set.

    {2 Examples}

    {[Set.any Int.isEven (Set.Int.fromArray [|2;3|]) = true]}

    {[Set.any Int.isEven (Set.Int.fromList [1;3]) = false]}

    {[Set.any Int.isEven (Set.Int.fromList []) = false]}
*)

val all: f: ('a -> bool) -> 'a t -> bool
(** Determine if [f] returns true for [all] values in a set.

    {2 Examples}

    {[Set.all Int.isEven (Set.Int.fromArray [|2;4|]) = true]}

    {[Set.all Int.isEven (Set.Int.fromLis [2;3]) = false]}

    {[Set.all Int.isEven Set.Int.empty = true]}
*)

(** {1 Combine} *)

val difference: 'a t -> 'a t -> 'a t
(** Returns a new set with the values from the first set which are not in the second set.

    {2 Examples}

    {[Set.difference (Set.Int.fromList [1;2;5]) (Set.Int.fromList [2;3;4]) |> Set.toList = [1;5]]}

    {[Set.difference (Set.Int.fromList [2;3;4]) (Set.Int.fromList [1;2;5]) |> Set.toList = [3;4]]}
*)

val intersection: 'a t -> 'a t -> 'a t
(** Get the intersection of two sets. Keeps values that appear in both sets.

    {2 Examples}

    {[Set.intersection (Set.Int.fromList [1;2;5]) (Set.Int.fromList [2;3;4]) |> Set.toList= [2]]}
*)

val union: 'a t -> 'a t -> 'a t
(** Get the union of two sets. Keep all values.

    {2 Examples}

    {[Set.union (Set.Int.fromList [1;2;5]) (Set.Int.fromList [2;3;4]) |> Set.toList = [1;2;3;4;5]]}
*)

(** {1 Transform} *)

val filter: f: ('a -> bool) -> 'a t -> 'a t
(** Keep elements that [f] returns [true] for.

    {2 Examples}

    {[Set.filter Int.isEven (Set.Int.fromList [1;2;3]) |> Set.toList = [2]]}
*)

val partition: f: ('a -> bool) -> 'a t -> 'a t * 'a t
(** Divide a set into two according to [f]. The first set will contain the values that [f] returns [true] for, values that [f] returns [false] for will end up in the second.

    {2 Examples}

    {[
      let numbers = Set.Int.fromList [1; 1; 5; 6; 5; 7; 9; 8] in
      let (evens, odds) = Set.partition Int.isEven numbers in
      Set.toList evens = [6; 8]
      Set.toList odds = [1; 5; 7; 9]
    ]}
*)

val fold: initial: 'b -> f: ('b -> 'a -> 'b) -> 'a t -> 'b
(** Transform a set into a value which is result of running each element in the set through [f], where each successive invocation is supplied the return value of the previous.

  See {!Array.fold} for a more in-depth explanation.

  {2 Examples}

  {[Set.fold 1 ( * ) (Set.Int.fromList [1;2;3;4]) = 24]}
*)

val forEach: f: ('a -> unit) -> 'a t -> unit
(** Runs a function [f] against each element of the set. *)

val for_each: f: ('a -> unit) -> 'a t -> unit

(** {1 Convert} *)

val toArray: 'a t -> 'a array
(** Converts a set into an {!Array} *)

val to_array: 'a t -> 'a array

val toList: 'a t -> 'a list
(** Converts a set into a {!List}. *)

val to_list: 'a t -> 'a list