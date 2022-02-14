module Tablecloth.Array

(** A mutable vector of elements which must have the same type.

    Has constant time (O(1)) {!get}, {!set} and {!length} operations.

    Arrays have a fixed length, if you want to be able to add an arbitrary number of elements maybe you want a {!List}.
*)

type 'a t = 'a array

(** {1 Create}

    You can create an [array] in OCaml with the [[|1; 2; 3|]] syntax.
*)

val singleton: 'a -> 'a t
(** Create an array with only one element.

    {2 Examples}

    {[Array.singleton 1234 = [|1234|]]}

    {[Array.singleton "hi" = [|"hi"|]]}
*)

val repeat: 'a -> length: int -> 'a t
(** Creates an array of length [length] with the value [x] populated at each index.

    {2 Examples}

    {[Array.repeat ~length:5 'a' = [|'a'; 'a'; 'a'; 'a'; 'a'|]]}

    {[Array.repeat ~length:0 7 = [||]]}

    {[Array.repeat ~length:(-1) "Why?" = [||]]}
*)

val range: from: int -> int -> int t
(** Creates an array containing all of the integers from [from] up to but not including [to]. Returns an empty list for invalid arguments.

    {2 Examples}

    {[Array.range 2 5 = [|2; 3; 4|] ]}

    {[Array.range (-2) 3 = [|-2; -1; 0; 1; 2|] ]}
*)

val initialize: f: (int -> 'a) -> int -> 'a t
(** Initialize an array. [Array.initialize n ~f] creates an array of length [n] with
    the element at index [i] initialized to the result of [(f i)].

    {2 Examples}

    {[Array.initialize identity 4 = [|0; 1; 2; 3|]]}

    {[Array.initialize (fun n -> n * n) 4 = [|0; 1; 4; 9|]]} *)

val fromList: 'a list -> 'a t
(** Create an array from a {!List}.

    {2 Examples}

    {[Array.fromList [1;2;3] = [|1;2;3|]]}
*)

val from_list: 'a list -> 'a t

val clone: 'a t -> 'a t
(** Create a shallow copy of an array.

    {2 Examples}

    {[
      let numbers = [|1;2;3|] in
      let otherNumbers = Array.copy numbers in
      numbers.(1) <- 9;
      numbers = [|1;9;3|];
      otherNumbers = [|1;2;3|];
    ]}

    {[
      let numberGrid = [|
        [|1;2;3|];
        [|4;5;6|];
        [|7;8;9|];
      |] in

      let numberGridCopy = Array.copy numberGrid in

      numberGrid.(1).(1) <- 0;

      numberGridCopy.(1).(1) = 9;
    ]}
*)

(** {1 Basic operations} *)

val get: int -> 'a t -> 'a
(** Get the element at the specified index.

    The first element has index number 0.

    The last element has index number [Array.length a - 1].

    You should prefer using the dedicated literal syntax;

    {[array.[n]]}

    Or using the safer {!Array.getAt} function.

    {3 Exceptions}

    Raises [IndexOutOfRangeException] for indexes outside of the range [0] to [(Array.length a - 1)].

    {2 Examples}

    {Array.get 1 [[|1; 2; 3; 2; 1|] = 2]}

    {[
      let animals = [|"cat"; "dog"; "eel"|] in
      Array.get 2 animals = "eel"
    ]}
*)

val getAt: index: int -> 'a t -> 'a option
(** Returns, as an {!Option}, the element at index number [n] of array [a].

    Returns [None] if [n] is outside the range [0] to [(Array.length a - 1)].

    {2 Examples}

    {[Array.getAt [|0; 1; 2|] ~index:5 = None]}

    {[Array.getAt [||] ~index:0 = None]}
*)

val get_at: index: int -> 'a t -> 'a option

// val ( .?() ) : 'element array -> int -> 'element option
// (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!getAt}
//
//     {b Note} Currently this is only supported by the OCaml syntax.
//
//     {2 Examples}
//
//     {[Array.([||].?(3)) = Some 'g']}
//
//     {[Array.([||].?(9)) = None]}
//  *)

val set: int -> 'a -> 'a t -> unit
(** Modifies an array in place, replacing the element at [index] with [value].

    You should prefer either to write

    {[array.(index) <- value]}

    Or use the {!setAt} function instead.

    {3 Exceptions}

    Raises [IndexOutOfRangeException] if [n] is outside the range [0] to [Array.length a - 1].

    {2 Examples}

    {[
      let numbers = [|1;2;3|] in
      Array.set numbers 1 1;
      numbers.(2) <- 0;

      numbers = [|1;0;0|]
    ]}
*)

val setAt: index: int -> value: 'a -> 'a t -> unit
(** Same as {!set} *)

val set_at: index: int -> value: 'a -> 'a t -> unit

val first: 'a t -> 'a option
(** Get the first element of an array.

    Returns [None] if the array is empty.

    {2 Examples}

    {[Array.first [1;2;3] = Some 1]}

    {[Array.first [1] = Some 1]}

    {[Array.first [] = None]}
*)

val last: 'a t -> 'a option
(** Get the last element of an array.

    Returns [None] if the array is empty.

    {2 Examples}

    {[Array.last [1;2;3] = Some 3]}

    {[Array.last [1] = Some 1]}

    {[Array.last [] = None]}
*)

val slice: from: int -> ``to``: int -> 'a t -> 'a t
(** Get a sub-section of a list. [from] is a zero-based index where we will start our slice.

    The [to] is a zero-based index that indicates the end of the slice.

    The slice extracts up to but not including [to].

    Both the [from] and [to] indexes can be negative, indicating an offset from the end of the list.

    {2 Examples}

    {[Array.slice 0 3 [0; 1; 2; 3; 4] = [0; 1; 2]]}

    {[Array.slice 1 4 [0; 1; 2; 3; 4] = [1; 2; 3]]}

    {[Array.slice 5 3 [0; 1; 2; 3; 4] = []]}

    {[Array.slice 1 (-1) [0; 1; 2; 3; 4] = [1; 2; 3]]}

    {[Array.slice (-2) 5 [0; 1; 2; 3; 4] = [3; 4]]}

    {[Array.slice (-2) (-1) [0; 1; 2; 3; 4] = [3]]}
*)

val swap: int -> int -> 'a t -> unit
(** Swaps the values at the provided indicies.

    {3 Exceptions}

    Raises an [IndexOutOfRangeExceptionInvalid_argument] exception of either index is out of bounds for the array.

    {2 Examples}

    {[Array.swap [|1; 2; 3|] 1 2 = [|1; 3; 2|]]}
*)

val reverse: 'a t -> unit
(** Reverses an array {b in place}, mutating the existing array.

    {2 Examples}

    {[
      let numbers = [|1; 2; 3|] in
      Array.reverse numbers
      numbers = [|3; 2; 1|];
    ]}
*)

val sort: 'a t -> unit when 'a: comparison
(** Sort in place, modifying the existing array, using the provided [compare] function to determine order.

    On native it uses {{: https://en.wikipedia.org/wiki/Merge_sort } merge sort} which means the sort is stable,
    runs in constant heap space, logarithmic stack space and [n * log (n)] time.

    When targeting javascript the time and space complexity of the sort cannot be guaranteed as it depends on the implementation.

    {2 Examples}

    {[Array.sortInPlace [|5;6;8;3;6|] ~compare:compare = [|3;5;6;6;8|]]}
*)

(** {1 Query} *)

val isEmpty: 'a t -> bool
(** Check if an array is empty

    {2 Examples}

    {[Array.isEmpty [|1; 2, 3|] = false]}

    {[Array.isEmpty [||] = true]}
*)

val is_empty: 'a t -> bool

val length: 'a t -> int
(** Return the length of an array.

    {2 Examples}

    {[Array.length [|1; 2, 3|] = 3]}

    {[Array.length [||] = 0]}
*)

val any: f: ('a -> bool) -> 'a t -> bool
(** Determine if [f] returns true for [any] values in an array.

    Iteration is stopped as soon as [f] returns [true]

    {2 Examples}

    {[Array.any Int.isEven [|1;2;3;5|] = true]}

    {[Array.any Int.isEven [|1;3;5;7|] = false]}

    {[Array.any Int.isEven [||] = false]}
*)

val all: f: ('a -> bool) -> 'a t -> bool
(** Determine if [f] returns true for [all] values in an array.

    Iteration is stopped as soon as [f] returns [false]

    {2 Examples}

    {[Array.all Int.isEven [|2;4|] = true]}

    {[Array.all Int.isEven [|2;3|] = false]}

    {[Array.all Int.isEven [||] = true]}
*)

val count: f: ('a -> bool) -> 'a t -> int
(** Count the number of elements which [f] returns [true] for

    {2 Examples}

    {[Array.count Int.isEven [|7; 5; 8; 6|] = 2]}
*)

val find: f: ('a -> bool) -> 'a t -> 'a option
(** Returns, as an {!Option}, the first element for which [f] evaluates to [true].

    If [f] doesn't return [true] for any of the elements [find] will return [None]

    {2 Examples}

    {[Array.find Int.isEven [|1; 3; 4; 8;|] = Some 4]}

    {[Array.find Int.isOdd [|0; 2; 4; 8;|] = None]}

    {[Array.find Int.isEven [||] = None]}
*)

val findIndex: f: (int -> 'a -> bool) -> 'a t -> (int * 'a) option
(** Similar to {!Array.find} but [f] is also called with the current index, and the return value will be a tuple of the index the passing value was found at and the passing value.

    {2 Examples}

    {[Array.findIndex (fun index number -> index > 2 && Int.isEven number) [|1; 3; 4; 8;|] = Some (3, 8)]}
*)

val find_index: f: (int -> 'a -> bool) -> 'a t -> (int * 'a) option

val includes: 'a -> 'a t -> bool when 'a: equality
(** Test if an array contains the specified element.

    {2 Examples}

    {[Array.contains 2 [|1; 2; 3|] = true]}
*)

val minimum: 'a t -> 'a option when 'a: comparison
(** Find the smallest element using the provided [compare] function.

    Returns [None] if called on an empty array.

    {2 Examples}

    {[Array.minimum [|7;5;8;6|] = Some 5]}

    {[Array.minimum [||] = None]}
*)

val maximum: 'a t -> 'a option when 'a: comparison
(** Find the largest element using the provided [compare] function.

    Returns [None] if called on an empty array.

    {2 Examples}

    {[Array.maximum [|7;5;8;6|] = Some 8]}

    {[Array.maximum [||] = None]}
*)

val extent: 'a t -> ('a * 'a) option when 'a: comparison
(** Find a {!Tuple} of the {!minimum} and {!maximum} in a single pass

    Returns [None] if called on an empty array.

    {2 Examples}

    {[Array.extent [|7;5;8;6|] = Some (5, 8)]}

    {[Array.extent [|7|] = Some (7, 7)]}

    {[Array.extent [||] = None]}
*)

val inline sum: 'a t -> 'a
  when 'a: (static member (+): 'a * 'a -> 'a) and 'a: (static member Zero: 'a)

(** Calculate the sum of an array. This will work for any type with a (+) and a Zero operator

    {2 Examples}

    {[ Array.sum [|1; 2; 3|] = 6 ]}

    {[ Array.sum [|4.0; 4.5; 5.0|] = 13.5 ]}

    {[ Array.sum [|"a"; "b"; "c"|] = "abc" ]}
*)

(** {1 Transform} *)

val map: f: ('a -> 'b) -> 'a t -> 'b t
(** Create a new array which is the result of applying a function [f] to every element.

    {2 Examples}

    {[Array.map Float.squareRoot [|1.0; 4.0; 9.0|] = [|1.0; 2.0; 3.0|]]}
*)

val mapWithIndex: f: (int -> 'a -> 'b) -> 'a t -> 'b t
(** Apply a function [f] to every element with its index as the first argument.

    {2 Examples}

    {[Array.mapWithIndex ( * ) [|5; 5; 5|] = [|0; 5; 10|]]}
*)

val map_with_index: f: (int -> 'a -> 'b) -> 'a t -> 'b t

val filter: f: ('a -> bool) -> 'a t -> 'a t
(** Keep elements that [f] returns [true] for.

    {2 Examples}

    {[Array.filter Int.isEven [|1; 2; 3; 4; 5; 6|] = [|2; 4; 6|]]}
*)

val filterMap: f: ('a -> 'b option) -> 'a t -> 'b t
(** Allows you to combine {!map} and {!filter} into a single pass.

    The output array only contains elements for which [f] returns [Some].

    Why [filterMap] and not just {!filter} then {!map}?

    {!filterMap} removes the {!Option} layer automatically.

    If your mapping is already returning an {!Option} and you want to skip over [None]s, then [filterMap] is much nicer to use.

    {2 Examples}

    {[
      let characters = [|'a'; '9'; '6'; ' '; '2'; 'z' |] in
      Array.filterMap Char.toDigit characters = [|9; 6; 2|]
    ]}

    {[
      Array.filterMap
       (fun number ->
          if Int.isEven number then
            Some (number * number)
          else
            None) [|3; 4; 5; 6|] = [16; 36]
    ]}
*)

val filter_map: f: ('a -> 'b option) -> 'a t -> 'b t

val flatMap: f: ('a -> 'b t) -> 'a t -> 'b t
(** {!map} [f] onto an array and {!flatten} the resulting arrays

    {2 Examples}

    {[Array.flatMap (fun n -> [|n; n|]) [|1; 2; 3|] = [|1; 1; 2; 2; 3; 3|]]}
*)

val flat_map: f: ('a -> 'b t) -> 'a t -> 'b t

val fold: initial: 'b -> f: ('b -> 'a -> 'b) -> 'a t -> 'b
(** Produce a new value from an array.

    [fold] takes two arguments, an [initial] 'accumulator' value and a function [f].

    For each element of the array [f] will be called with two arguments; the current accumulator and an element.

    [f] returns the value that the accumulator should have for the next iteration.

    The [initial] value is the value the accumulator will have on the first call to [f].

    After applying [f] to every element of the array, [fold] returns the accumulator.

    [fold] iterates over the elements of the array from first to last.

    Folding is useful whenever you have a collection of something, and want to produce a single value from it.

    For examples if we have:

    {[
      let numbers = [|1, 2, 3|] in
      let sum =
        Array.fold 0 (fun accumulator element -> accumulator + element) numbers
      in
      sum = 6
    ]}

    Walking though each iteration step by step:

    + [accumulator: 0, element: 1, result: 1]
    + [accumulator: 1, element: 2, result: 3]
    + [accumulator: 3, element: 3, result: 6]

    And so the final result is [6]. (Note that in reality you probably want to use {!Array.sum})

    {2 Examples}

    {[Array.fold [] List.cons [|1; 2; 3|] = [3; 2; 1]]}

    {[
      Array.fold [|1; 1; 2; 2; 3|] Set.empty Set.add |> Set.toArray = [|1; 2; 3|]
    ]}

    {[
      let lastEven integers =
        Array.fold None (fun last int ->
          if Int.isEven then
            Some int
          else
            last
        ) integers
      in
      lastEven [|1;2;3;4;5|] = Some 4
    ]}
*)

val foldRight: initial: 'b -> f: ('b -> 'a -> 'b) -> 'a t -> 'b
(** This method is like {!fold} except that it iterates over the elements of the array from last to first.

    {2 Examples}

    {[Array.foldRight ~f:(+) ~initial:0 (Array.repeat ~length:3 5) = 15]}

    {[Array.foldRight ~f:List.cons ~initial:[] [|1; 2; 3|] = [1; 2; 3]]}
*)

val fold_right: initial: 'b -> f: ('b -> 'a -> 'b) -> 'a t -> 'b

(** {1 Combine} *)

val append: 'a t -> 'a t -> 'a t
(** Creates a new array which is the result of appending the second array onto the end of the first.

    {2 Examples}

    {[
      let fortyTwos = Array.repeat 2 42 in
      let eightyOnes = Array.repeat 3 81 in
      Array.append fourtyTwos eightyOnes = [|42; 42; 81; 81; 81|];
    ]}
*)

val flatten: 'a t t -> 'a t
(** Flatten an array of arrays into a single array:

    {2 Examples}

    {[Array.flatten [|[|1; 2|]; [|3|]; [|4; 5|]|] = [|1; 2; 3; 4; 5|]]}
*)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** Combine two arrays by merging each pair of elements into a {!Tuple}

    If one array is longer, the extra elements are dropped.

    The same as [Array.map2 Tuple.make]

    {2 Examples}

    {[Array.zip [|1;2;3;4;5|] [|"Dog"; "Eagle"; "Ferret"|] = [|(1, "Dog"); (2, "Eagle"); (3, "Ferret")|]]}
*)

val map2: f: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Combine two arrays, using [f] to combine each pair of elements.

    If one array is longer, the extra elements are dropped.

    {2 Examples}

    {[
      let totals (xs : int array) (ys : int array) : int array =
        Array.map2 (+) xs ys in

      totals [|1;2;3|] [|4;5;6|] = [|5;7;9|]
    ]}

    {[
      Array.map2
        Tuple.create
        [|"alice"; "bob"; "chuck"|]
        [|2; 5; 7; 8|] =
          [|("alice",2); ("bob",5); ("chuck",7)|]
    ]}
*)

val map3: f: ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
(** Combine three arrays, using [f] to combine each trio of elements.

    If one array is longer, the extra elements are dropped.

    {2 Examples}

    {[
      Array.map3
        Tuple3.create
        [|"alice"; "bob"; "chuck"|]
        [|2; 5; 7; 8;|]
        [|true; false; true; false|] =
          [|("alice", 2, true); ("bob", 5, false); ("chuck", 7, true)|]
    ]}
*)

(** {1 Deconstruct} *)

val partition: f: ('a -> bool) -> 'a t -> 'a t * 'a t
(** Split an array into a {!Tuple} of arrays. Values which [f] returns true for will end up in {!Tuple.first}.

    {2 Examples}

    {[Array.partition Int.isOdd [|1;2;3;4;5;6|] = ([|1;3;5|], [|2;4;6|])]}
*)

val splitAt: index: int -> 'a t -> 'a t * 'a t
(** Divides an array into a {!Tuple} of arrays.

    Elements which have index upto (but not including) [index] will be in the first component of the tuple.

    Elements with an index greater than or equal to [index] will be in the second.

    {3 Exceptions}

    Raises an [IndexOutOfRangeException] exception if [index] is less than zero

    {2 Examples}

    {[Array.splitAt 2 [|1;2;3;4;5|] = ([|1;2|], [|3;4;5|])]}

    {[Array.splitAt 10 [|1;2;3;4;5|] = ([|1;2;3;4;5|], [||])]}

    {[Array.splitAt 0 [|1;2;3;4;5|] = ([||], [|1;2;3;4;5|])]}
*)

val split_at: index: int -> 'a t -> 'a t * 'a t

val splitWhen: f: ('a -> bool) -> 'a t -> 'a t * 'a t
(** Divides an array at the first element [f] returns [true] for.

    Returns a {!Tuple}, the first component contains the elements [f] returned false for,
    the second component includes the element that [f] retutned [true] for an all the remaining elements.

    {2 Examples}

    {[
      Array.splitWhen
        Int.isEven
        [|5; 7; 8; 6; 4;|] =
        ([|5; 7|], [|8; 6; 4|])
    ]}

    {[
      Array.splitWhen
        (fun animal -> String.length animal > 3)
        [|"Ant"; "Bat"; "Cat"|]
        = ([|"Ant"; "Bat"; "Cat"|], [||])
    ]}

    {[
      Array.splitWhen Float.isInteger [|2.; Float.pi; 1.111|] =
        ([||], [|2.; Float.pi; 1.111|])
    ]}
*)

val split_when: f: ('a -> bool) -> 'a t -> 'a t * 'a t

val unzip: ('a * 'b) t -> 'a t * 'b t
(** Decompose an array of {!Tuple}s into a {!Tuple} of arrays.

    {2 Examples}

    {[Array.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
*)

(** {1 Iterate} *)

val forEach: f: ('a -> unit) -> 'a t -> unit
(** Iterates over the elements of invokes [f] for each element.

    {2 Examples}

    {[Array.forEach (fun int -> print (Int.toString int)) [|1; 2; 3|] ]}
*)

val for_each: f: ('a -> unit) -> 'a t -> unit

val forEachWithIndex: f: (int -> 'a -> unit) -> 'a t -> unit
(** Iterates over the elements of invokes [f] for each element.

    {2 Examples}

    {[
      Array.forEachI (fun index int -> printf "%d: %d" index int) [|1; 2; 3|]
      (*
        0: 1
        1: 2
        2: 3
      *)
    ]}
*)

val for_each_with_index: f: (int -> 'a -> unit) -> 'a t -> unit

val values: 'a option t -> 'a t
(** Return all of the [Some] values from an array of options

    {2 Examples}

    {[Array.values [|(Some "Ant"); None; (Some "Cat")|] = [|"Ant"; "Cat"|]]}

    {[Array.values [|None; None; None|] = [||]]}
*)

val intersperse: sep: 'a -> 'a t -> 'a t
(** Places [sep] between all the elements of the given array.

    {2 Examples}

    {[
      Array.intersperse "on" [|"turtles"; "turtles"; "turtles"|] =
      [|"turtles"; "on"; "turtles"; "on"; "turtles"|]
    ]}

    {[Array.intersperse 0 [||] = [||]]}
*)

val chunksOf: size: int -> 'a t -> 'a t t
(** Split an array into equally sized chunks.

    If there aren't enough elements to make the last 'chunk', those elements are ignored.

    {2 Examples}

    {[
      Array.chunksOf 2 [|"#FFBA49"; "#9984D4"; "#20A39E"; "#EF5B5B"; "#23001E"|] =  [|
        [|"#FFBA49"; "#9984D4"|];
        [|"#20A39E"; "#EF5B5B"|];
      |]
    ]}
 *)

val chunks_of: size: int -> 'a t -> 'a t t

val sliding: step: int -> size: int -> 'a t -> 'a t t
(** Provides a sliding 'window' of sub-arrays over an array.

    The first sub-array starts at index [0] of the array and takes the first [size] elements.

    The sub-array then advances the index [step] (which defaults to 1) positions before taking the next [size] elements.

    The sub-arrays are guaranteed to always be of length [size] and iteration stops once a sub-array would extend beyond the end of the array.

    {2 Examples}

    {[Array.sliding 1 1 [|1;2;3;4;5|] 1 1 = [|[|1|]; [|2|]; [|3|]; [|4|]; [|5|]|] ]}

    {[Array.sliding 1 2 [|1;2;3;4;5|] 1 2 = [|[|1;2|]; [|2;3|]; [|3;4|]; [|4;5|]|] ]}

    {[Array.sliding 1 3 [|1;2;3;4;5|] 1 3 = [|[|1;2;3|]; [|2;3;4|]; [|3;4;5|]|] ]}

    {[Array.sliding 2 2 [|1;2;3;4;5|] 2 2 = [|[|1;2|]; [|3;4|]|] ]}

    {[Array.sliding 1 3 [|1;2;3;4;5|] 1 3 = [|[|1|]; [|4|]|] ]}
*)

(** {1 Convert} *)

val join: sep: string -> string t -> string
(** Converts an array of strings into a {!String}, placing [sep] between each string in the result.

    {2 Examples}

    {[Array.join ", " [|"Ant"; "Bat"; "Cat"|] = "Ant, Bat, Cat"]}
 *)

val groupBy: ('value -> 'key) -> 'value t -> Map<'key, 'value list>
  when 'key: comparison
(** Collect elements which [f] produces the same key for

    Produces a map from ['key] to a {!List} of all elements which produce the same ['key]

    {2 Examples}

    {[
      let animals = ["Ant"; "Bear"; "Cat"; "Dewgong"] in
      Array.groupBy animals (module Int) ~f:String.length = Map.Int.fromList [
        (3, ["Cat"; "Ant"]);
        (4, ["Bear"]);
        (7, ["Dewgong"]);
      ]
    ]}
*)

val group_by: ('value -> 'key) -> 'value t -> Map<'key, 'value list>
  when 'key: comparison

val toList: 'a t -> 'a list
(** Create a {!List} of elements from an array.

    {2 Examples}

    {[Array.toList [|1;2;3|] = [1;2;3]]}

    {[Array.toList (Array.fromList [3; 5; 8]) = [3; 5; 8]]}
*)

val to_list: 'a t -> 'a list

val toIndexedList: 'a t -> (int * 'a) list
(** Create an indexed {!List} from an array. Each element of the array will be paired with its index as a {!Tuple}.

    {2 Examples}

    {[Array.toIndexedList [|"cat"; "dog"|] = [(0, "cat"); (1, "dog")]]}
*)

val to_indexed_list: 'a t -> (int * 'a) list

(** {1 Compare} *)

val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Test two arrays for equality using the provided function to test pairs of elements. *)

val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Compare two arrays using the provided function to compare pairs of elements.

    A shorter array is 'less' than a longer one.

    {2 Examples}

    {[Array.compare Int.compare [|1;2;3|] [|1;2;3;4|] = -1]}

    {[Array.compare Int.compare [|1;2;3|] [|1;2;3|] = 0]}

    {[Array.compare Int.compare [|1;2;5|] [|1;2;3|] = 1]}
*)