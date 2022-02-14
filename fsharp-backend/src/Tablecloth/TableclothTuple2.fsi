module Tablecloth.Tuple2

(** Functions for manipulating pairs of values *)

type ('a, 'b) t = 'a * 'b

(** {1 Create} *)

val make: 'a -> 'b -> 'a * 'b
(** Create a two-tuple with the given values.

    The values do not have to be of the same type.

    {2 Examples}

    {[Tuple2.make 3 "Clementine" = (3, "Clementine")]}
*)

val fromArray: 'a array -> ('a * 'a) option
(** Create a tuple from the first two elements of an {!Array}.

    If the array is longer than two elements, the extra elements are ignored.

    If the array is less than two elements, returns [None]

    {2 Examples}

    {[Tuple2.fromArray [|1; 2|] = Some (1, 2)]}

    {[Tuple2.fromArray [|1|] = None]}

    {[Tuple2.fromArray [|4; 5; 6|] = Some (4, 5)]}
*)

val from_array: 'a array -> ('a * 'a) option

val fromList: 'a list -> ('a * 'a) option
(** Create a tuple from the first two elements of a {!List}.

    If the list is longer than two elements, the extra elements are ignored.

    If the list is less than two elements, returns [None]

    {2 Examples}

    {[Tuple2.fromList [1; 2] = Some (1, 2)]}

    {[Tuple2.fromList [1] = None]}

    {[Tuple2.fromList [4; 5; 6] = Some (4, 5)]}
*)

val from_list: 'a list -> ('a * 'a) option

val first: 'a * 'b -> 'a
(** Extract the first value from a tuple.

    {2 Examples}

    {[Tuple2.first (3, 4) = 3]}

    {[Tuple2.first ("john", "doe") = "john"]}
*)

val second: 'a * 'b -> 'b
(** Extract the second value from a tuple.

    {2 Examples}

    {[Tuple2.second (3, 4) = 4]}

    {[Tuple2.second ("john", "doe") = "doe"]}
*)

(** {1 Transform} *)

val mapFirst: f: ('a -> 'x) -> 'a * 'b -> 'x * 'b
(** Transform the {!first} value in a tuple.

    {2 Examples}

    {[Tuple2.mapFirst String.reverse ("stressed", 16) = ("desserts", 16)]}

    {[Tuple2.mapFirst String.length ("stressed", 16) = (8, 16)]}
*)

val map_first: f: ('a -> 'x) -> 'a * 'b -> 'x * 'b

val mapSecond: f: ('b -> 'c) -> 'a * 'b -> 'a * 'c
(** Transform the second value in a tuple.

    {2 Examples}

    {[Tuple2.mapSecond Float.squareRoot ("stressed", 16.) = ("stressed", 4.)]}

    {[Tuple2.mapSecond (~-) ("stressed", 16) = ("stressed", -16)]}
*)

val map_second: f: ('b -> 'c) -> 'a * 'b -> 'a * 'c

val mapEach: f: ('a -> 'x) -> g: ('b -> 'y) -> 'a * 'b -> 'x * 'y
(** Transform both values of a tuple, using [f] for the first value and [g] for the second.

    {2 Examples}

    {[Tuple2.mapEach String.reverse Float.squareRoot ("stressed", 16.) = ("desserts", 4.)]}

    {[Tuple2.mapEach String.length (~-) ("stressed", 16) = (8, -16)]}
*)

val map_each: f: ('a -> 'x) -> g: ('b -> 'y) -> 'a * 'b -> 'x * 'y

val mapAll: f: ('a -> 'b) -> 'a * 'a -> 'b * 'b
(** Transform both of the values of a tuple using the same function.

    [mapAll] can only be used on tuples which have the same type for each value.

    {2 Examples}

    {[Tuple2.mapAll (Int.add 1) (3, 4, 5) = (4, 5, 6)]}

    {[Tuple2.mapAll String.length ("was", "stressed") = (3, 8)]}
*)

val map_all: f: ('a -> 'b) -> 'a * 'a -> 'b * 'b

val swap: 'a * 'b -> 'b * 'a
(** Switches the first and second values of a tuple.

    {2 Examples}

    {[Tuple2.swap (3, 4) = (4, 3)]}

    {[Tuple2.swap ("stressed", 16) = (16, "stressed")]}
*)

(** {1 Convert} *)

val toArray: 'a * 'a -> 'a array
(** Turns a tuple into an {!Array} of length two.

    This function can only be used on tuples which have the same type for each value.

    {2 Examples}

    {[Tuple2.toArray (3, 4) = [|3; 4|]]}

    {[Tuple2.toArray ("was", "stressed") = [|"was"; "stressed"|]]}
*)

val to_array: 'a * 'a -> 'a array

val toList: 'a * 'a -> 'a list
(** Turns a tuple into a list of length two. This function can only be used on tuples which have the same type for each value.

    {2 Examples}

    {[Tuple2.toList (3, 4) = [3; 4]]}

    {[Tuple2.toList ("was", "stressed") = ["was"; "stressed"]]}
*)

val to_list: 'a * 'a -> 'a list

(** {1 Compare} *)

val equal: ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 'a * 'b -> 'a * 'b -> bool
(** Test two {!Tuple2}s for equality, using the provided functions to test the
    first and second components.

    {2 Examples}

    {[Tuple2.equal Int.equal String.equal (1, "Fox") (1, "Fox") = true]}

    {[Tuple2.equal Int.equal String.equal (1, "Fox") (2, "Hen") = false]}
*)

val compare: ('a -> 'a -> int) -> ('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int
(** Compare two {!Tuple2}s, using the provided functions to compare the first
    components then, if the first components are equal, the second components.

    {2 Examples}

    {[Tuple2.compare Int.compare String.compare (1, "Fox") (1, "Fox") = 0]}

    {[Tuple2.compare Int.compare String.compare (1, "Fox") (1, "Eel") = 1]}

    {[Tuple2.compare Int.compare String.compare (1, "Fox") (2, "Hen") = -1]}
*)