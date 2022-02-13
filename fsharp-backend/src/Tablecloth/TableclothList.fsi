module Tablecloth.List

(** Immutable singly-linked list of elements which must have the same type.

    Lists can have any number of elements.

    They are fast (O(1)) when:
    - Getting the first element using {!head}
    - Getting the {!tail}
    - Creating a new list by adding an element to the front using {!cons}

    They also support exhaustive pattern matching

    {[
      match aList with
      | [] -> "Empty"
      | [a] -> "Exactly one element"
      | [a, b] -> "Exactly two elements"
      | a :: b :: cs -> "More than two elements"
    ]}

    Lists are slow when:
    - You need to access an element that isn't at the front of the list
    - Counting how many elements are in the list

    As they have inefficent ([O(n)]) {!getAt} and {!length} operations.

    If those are important to your use-case, perhaps you need an {!Array}.
*)

type 'a t = 'a list

(** {1 Create}

    You can create a [list] with the [[1;2;3]] syntax.
*)

val empty: 'a t
(** An empty list.

    {2 Examples}

    {[List.empty = []]}

    {[List.length List.empty = 0]}
*)

val singleton: 'a -> 'a t
(** Create a list with only one element.

    {2 Examples}

    {[List.singleton 1234 = [1234]]}

    {[List.singleton "hi" = ["hi"]]}
*)

val repeat: count: int -> 'a -> 'a t
(** Creates a list of length [times] with the value [x] populated at each index.

    {2 Examples}

    {[List.repeat 5 'a' = ['a'; 'a'; 'a'; 'a'; 'a']]}

    {[List.repeat 0 7 = []]}

    {[List.repeat (-1) "Why?" = []]}
*)

val range: from: int -> ``to``: int -> int t
(** Creates a list containing all of the integers from [from] up to but not including [to]

    {2 Examples}

    {[List.range 5 = [0; 1; 2; 3; 4] ]}

    {[List.range 2 5 = [2; 3; 4] ]}

    {[List.range (-2) 3 = [-2; -1; 0; 1; 2] ]}
*)

val initialize: count: int -> f: (int -> 'a) -> 'a t
(** Initialize a list.

    [List.initialize n f] creates a list of length [n] by setting the element at position [index] to be [f(index)].

    {2 Examples}

    {[List.initialize 4 identity = [0; 1; 2; 3]]}

    {[List.initialize 4 (fun index -> index * index) = [0; 1; 4; 9]]}
*)

val fromArray: 'a array -> 'a t
(** Create a list from an {!Array}.

    {2 Examples}

    {[List.fromArray [|1;2;3|] = [1;2;3]]}
*)

val from_array: 'a array -> 'a t

(** {1 Basic operations} *)

val head: 'a t -> 'a option
(** Returns, as an {!Option}, the first element of a list.

    If the list is empty, returns [None]

    {2 Examples}

    {[List.head [1;2;3] = Some 1]}

    {[List.head [] = None]}
*)

val tail: 'a t -> 'a t option
(** Returns, as an {!Option}, a list without its first element.

    If the list is empty, returns [None]

    {2 Examples}

    {[List.tail [1;2;3] = Some [2;3]]}

    {[List.tail [1] = Some []]}

    {[List.tail [] = None]}
*)

val cons: 'a -> 'a t -> 'a t
(** Prepend a value to the front of a list.

    The [::] operator can also be used, in Reason you use the spread syntax
    instead.

    {2 Examples}

    {[List.cons 1 [2;3;4] = [1;2;3;4]]}

    {[1 :: [2;3;4] = [1;2;3;4]]}
*)

val take: count: int -> 'a t -> 'a t
(** Attempt to take the first [count] elements of a list.

   If the list has fewer than [count] elements, returns [None].

   {2 Examples}

   {[List.take 2 [1;2;3] = Some [1;2]]}

   {[List.take 2 [] = None]}

   {[List.take 8 [1;2;3;4] = None]}
*)

val takeWhile: f: ('a -> bool) -> 'a t -> 'a t
(** Take elements from a list until [f] returns [false]

    {2 Examples}

    {[
      List.takeWhile Int.isEven [2; 4; 6; 7; 8; 9] = [2; 4; 6]
      List.takeWhile Int.isEven [2; 4; 6] = [2; 4; 6]
      List.takeWhile Int.isEven [1; 2; 3] = []
    ]}
*)

val take_while: f: ('a -> bool) -> 'a t -> 'a t

val drop: count: int -> 'a t -> 'a t
(** Drop the first [count] elements from the front of a list.

    {2 Examples}

    {[List.drop 2 [1;2;3;4] = [3;4]]}

    {[List.drop 6 [1;2;3;4] = []]}
*)

val dropWhile: f: ('a -> bool) -> 'a t -> 'a t
(** Drop elements from a list until [f] returns [false]

    {2 Examples}

    {[List.dropWhile Int.isEven [2; 4; 6; 7; 8; 9] = [7; 8; 9]]}

    {[List.dropWhile Int.isEven [2; 4; 6; 8] = []]}

    {[List.dropWhile Int.isEven [1; 2; 3] = [1; 2; 3]]}
*)

val drop_while: f: ('a -> bool) -> 'a t -> 'a t

val initial: 'a t -> 'a t option
(** As an {!Option} get of all of the elements of a list except the last one.

    Returns [None] if the list is empty.

    {2 Examples}

    {[List.initial [1;2;3] = Some [1;2]]}

    {[List.initial [1] = Some []]}

    {[List.initial [] = None]}
*)

val last: 'a t -> 'a option
(** Get the last element of a list.

    Returns [None] if the list is empty.

    {b Warning} This will iterate through the entire list.

    {2 Examples}

    {[List.last [1;2;3] = Some 3]}

    {[List.last [1] = Some 1]}

    {[List.last [] = None]}
*)

val getAt: index: int -> 'a t -> 'a option
(** Returns the element at position [index] in the list.

    Returns [None] if [index] is outside of the bounds of the list.

    {2 Examples}

    {[List.getAt 1 [1;2;3] = Some 2]}

    {[List.getAt 2 [] = None]}

    {[List.getAt 100 [1;2;3] = None]}
*)

val get_at: index: int -> 'a t -> 'a option

val insertAt: index: int -> value: 'a -> 'a t -> 'a t
(** Insert a new element at the specified index.

    The element previously occupying [index] will now be at [index + 1]

    If [index] is greater than then length of the list, it will be appended:

    {e Exceptions}

    Raises an [Invalid_argument] exception if [index] is negative

    {2 Examples}

    {[
      List.insertAt
        2
        999
        [100; 101; 102; 103] =
          [100; 101; 999; 102; 103]
    ]}

    {[List.insertAt 0 999 [100; 101; 102; 103] = [999; 100; 101; 102; 103]]}

    {[List.insertAt 4 999 [100; 101; 102; 103] = [100; 101; 102; 103; 999]]}

    {[List.insertAt (-1) 999 [100; 101; 102; 103] = [999]]}

    {[List.insertAt 5 999 [100; 101; 102; 103] = [999]]}
*)

val insert_at: index: int -> value: 'a -> 'a t -> 'a t

val updateAt: index: int -> f: ('a -> 'a) -> 'a t -> 'a t
(** Returns a new list with the value at [index] updated to be the result of applying [f].

    If [index] is outside of the bounds of the list, returns the list as-is.

    {2 Examples}

    {[List.updateAt 1 (Int.add 3) [1; 2; 3] = [1; 5; 3]]}

    {[
      let animals = ["Ant"; "Bat"; "Cat"] in
      animals = List.updateAt 4 String.reverse animals
    ]}
*)

val update_at: index: int -> f: ('a -> 'a) -> 'a t -> 'a t

val removeAt: index: int -> 'a t -> 'a t
(** Creates a new list without the element at [index].

    If [index] is outside of the bounds of the list, returns the list as-is.

    {2 Examples}

    {[List.removeAt 2 [1; 2; 3] = [1; 2]]}

    {[
      let animals = ["Ant"; "Bat"; "Cat"] in
      animals = (List.removeAt 4 animals)
    ]}
*)

val remove_at: index: int -> 'a t -> 'a t

val reverse: 'a t -> 'a t
(** Reverse the elements in a list

    {2 Examples}

    {[List.reverse [1; 2; 3] = [3; 2; 1]]}
 *)

val sort: 'a t -> 'a t when 'a: comparison
(** Sort list using comparison trait

    Sorting is stable.

    {2 Examples}

    {[List.sort [5;6;8;3;6] = [3;5;6;6;8]]}
*)

(** {1 Query} *)

val isEmpty: 'a t -> bool
(** Determine if a list is empty.

    {2 Examples}

    {[List.isEmpty List.empty = true]}

    {[List.isEmpty [||] = true]}

    {[List.isEmpty [|1; 2; 3|] = false]}
*)

val is_empty: 'a t -> bool

val length: 'a t -> int
(** Return the number of elements in a list.

    {b Warning} [List.length] needs to access the {b entire} list in order to calculate its result.

    If you need fast access to the length, perhaps you need an {!Array}.

    A common mistake is to have something like the following:

    {[
      if (List.length someList) = 0 then (
        () (* It will take longer than you think to reach here *)
      ) else (
        () (* But it doesn't need to *)
      )
    ]}

    instead you should do

    {[
      if (List.isEmpty someList) then (
        () (* This happens instantly *)
      ) else (
        () (* Since List.isEmpty takes the same amount of time for all lists *)
      )
    ]}

    Or

    {[
      match someList with
      | [] -> () (* Spoilers *)
      | _ -> () (* This is how isEmptu is implemented *)
    ]}


    {2 Examples}

    {[List.length [] = 0]}

    {[List.length [7; 8; 9] = 3]}
*)

val any: f: ('a -> bool) -> 'a t -> bool
(** Determine if [f] returns true for [any] values in a list.

    Stops iteration as soon as [f] returns true.

    {2 Examples}

    {[List.any isEven [|2;3|] = true]}

    {[List.any isEven [|1;3|] = false]}

    {[List.any isEven [||] = false]}
*)

val all: f: ('a -> bool) -> 'a t -> bool
(** Determine if [f] returns true for [all] values in a list.

    Stops iteration as soon as [f] returns false.

    {2 Examples}

    {[List.all Int.isEven [|2;4|] = true]}

    {[List.all Int.isEven [|2;3|] = false]}

    {[List.all Int.isEven [||] = true]}
*)

val count: f: ('a -> bool) -> 'a t -> int
(** Count the number of elements which [f] returns [true] for

    {2 Examples}

    {[List.count Int.isEven [7;5;8;6] = 2]}
 *)

val find: f: ('a -> bool) -> 'a t -> 'a option
(** Returns, as an option, the first element for which [f] evaluates to true.

  If [f] doesn't return [true] for any of the elements [find] will return [None]

  {2 Examples}

  {[List.find Int.isEven [|1; 3; 4; 8;|] = Some 4]}

  {[List.find Int.isOdd [|0; 2; 4; 8;|] = None]}

  {[List.find Int.isEven [||] = None]}
*)

val findIndex: f: (int -> 'a -> bool) -> 'a t -> (int * 'a) option
(** Returns, as an option, a tuple of the first element and its index for which [f] evaluates to true.

    If [f] doesnt return [true] for any [(index, element)] pair, returns [None].

    {2 Examples}

    {[List.findIndex (fun index number -> index > 2 && Int.isEven number) [|1; 3; 4; 8;|] = Some (3, 8)]}
*)

val find_index: f: (int -> 'a -> bool) -> 'a t -> (int * 'a) option

val includes: 'a -> 'a t -> bool when 'a: equality

(** Test if a list contains the specified element using the provided [equal] to test for equality.

    This function may iterate the entire list, so if your code needs to
    repeatedly perform this check, maybe you want a {!Set} instead.

    {2 Examples}

    {[List.includes [1; 3; 5; 7] 3 = true]}

    {[List.includes [1; 3; 5; 7] 4 = false]}

    {[List.includes [] 5 = false]}
*)

val minimum: 'a t -> 'a option when 'a: comparison
(** Find the smallest element.

    Returns [None] if called on an empty array.

    {2 Examples}

    {[List.minimum [|7; 5; 8; 6|] = Some 5]}
*)

val maximum: 'a t -> 'a option when 'a: comparison
(** Find the largest element.

    Returns [None] if called on an empty array.

    {2 Examples}

    {[List.maximum [|7; 5; 8; 6|] = Some 8]}
*)

val extent: 'a t -> ('a * 'a) option when 'a: comparison
(** Find a {!Tuple} of the [(minimum, maximum)] elements.

    Returns [None] if called on an empty array.

    {2 Examples}

    {[List.extent [|7; 5; 8; 6|] = Some (5, 8)]}
*)

// when clause copied from
// https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/list.fs.
// Must be inline or we get a type error
val inline sum: 'a list -> 'a
  when 'a: (static member (+): 'a * 'a -> 'a) and 'a: (static member Zero: 'a)

(** Calculate the sum of a list. This will work for any type with a (+) and a Zero operator

    {2 Examples}

    {[List.sum [1;2;3] = 6]}

    {[List.sum [4.0;4.5;5.0] = 13.5]}

    {[ List.sum ["a"; "b"; "c"] = "abc" ]}
*)

(** {1 Transform} *)

val map: f: ('a -> 'b) -> 'a t -> 'b t
(** Create a new list which is the result of applying a function [f] to every element.

    {2 Examples}

    {[List.map Float.squareRoot [|1.0; 4.0; 9.0|] = [|1.0; 2.0; 3.0|]]}
*)

val mapWithIndex: f: (int -> 'a -> 'b) -> 'a t -> 'b t
(** Apply a function [f] to every element and its index.

    {2 Examples}

    {[
      List.mapWithIndex
        (fun index element ->
          (Int.toString index) ^ ": " ^ element)
        ["zero"; "one"; "two"]
        = ["0: zero"; "1: one"; "2: two"]
    ]}
*)

val map_with_index: f: (int -> 'a -> 'b) -> 'a t -> 'b t

val filter: f: ('a -> bool) -> 'a t -> 'a t
(** Keep elements that [f] returns [true] for.

    {2 Examples}

    {[List.filter Int.isEven [1; 2; 3; 4; 5; 6] = [2; 4; 6]]}
*)

val filterWithIndex: f: (int -> 'a -> bool) -> 'a t -> 'a t
(** Like {!filter} but [f] is also called with each elements index. *)

val filter_with_index: f: (int -> 'a -> bool) -> 'a t -> 'a t

val filterMap: f: ('a -> 'b option) -> 'a t -> 'b t
(** Allows you to combine {!map} and {!filter} into a single pass.

    The output list only contains elements for which [f] returns [Some].

    Why [filterMap] and not just {!filter} then {!map}?

    {!filterMap} removes the {!Option} layer automatically.
    If your mapping is already returning an {!Option} and you want to skip over Nones, then [filterMap] is much nicer to use.

    {2 Examples}

    {[
      let characters = ['a'; '9'; '6'; ' '; '2'; 'z'] in
      List.filterMap Char.toDigit characters = [9; 6; 2]
    ]}

    {[
      List.filterMap (fun number ->
        if Int.isEven number then
          Some (number * number)
        else
          None
      ) [3; 4; 5; 6] = [16; 36]
    ]}
*)

val filter_map: f: ('a -> 'b option) -> 'a t -> 'b t

val flatMap: f: ('a -> 'b t) -> 'a t -> 'b t
(** Apply a function [f] onto a list and {!flatten} the resulting list of lists.

    {2 Examples}

    {[List.flatMap f xs = List.map f xs |> List.flatten]}

    {[List.flatMap (fun n -> [|n; n|]) [|1; 2; 3|] = [|1; 1; 2; 2; 3; 3|]]}
*)

val flat_map: f: ('a -> 'b t) -> 'a t -> 'b t

val fold: initial: 'b -> f: ('b -> 'a -> 'b) -> 'a t -> 'b
(** Transform a list into a value

    After applying [f] to every element of the list, [fold] returns the accumulator.

    [fold] iterates over the elements of the list from first to last.

    For examples if we have:

    {[
      let numbers = [1, 2, 3] in
      let sum =
        List.fold 0 (fun accumulator element -> accumulator + element) numbers
      in
      sum = 6
    ]}

    Walking though each iteration step by step:

    + [accumulator: 0, element: 1, result: 1]
    + [accumulator: 1, element: 2, result: 3]
    + [accumulator: 3, element: 3, result: 6]

    And so the final result is [6]. (Note that in this case you probably want to use {!List.sum})

    {b Examples continued}

    {[List.fold [] List.cons [1; 2; 3] = [3; 2; 1]]}

    {[
      let unique integers =
        List.fold Set.Int.empty Set.add integers |> Set.toList
      in
      unique [1; 1; 2; 3; 2] = [1; 2; 3]
    ]}

    {[
      let lastEven integers =
        List.fold None (fun last int ->
          if Int.isEven then
            Some int
          else
            last
        ) integers
      in
      lastEven [1;2;3;4;5] = Some 4
    ]}
*)

val foldRight: initial: 'b -> f: ('b -> 'a -> 'b) -> 'a t -> 'b
(** This method is like {!fold} except that it iterates over the elements of the list from last to first. *)

val fold_right: initial: 'b -> f: ('b -> 'a -> 'b) -> 'a t -> 'b

(** {1 Combine} *)

val append: 'a t -> 'a t -> 'a t
(** Creates a new list which is the result of appending the second list onto the end of the first.

    {2 Examples}

    {[
      let fortyTwos = List.repeat 2 42 in
      let eightyOnes = List.repeat 3 81 in
      List.append fourtyTwos eightyOnes = [42; 42; 81; 81; 81];
    ]}
*)

val flatten: 'a t t -> 'a t
(** Concatenate a list of lists into a single list:

    {2 Examples}

    {[List.flatten [[1; 2]; [3]; [4; 5]] = [1; 2; 3; 4; 5]]}
*)

val zip: 'a t -> 'b t -> ('a * 'b) t
(** Combine two lists by merging each pair of elements into a {!Tuple}

    If one list is longer, the extra elements are dropped.

    The same as [List.map2 (,)]

    {2 Examples}

    {[List.zip [|1;2;3;4;5|] [|"Dog"; "Eagle"; "Ferret"|] = [|(1, "Dog"); (2, "Eagle"); (3, "Ferret")|]]}
*)

val map2: f: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Combine two lists, using [f] to combine each pair of elements.

    If one list is longer, the extra elements are dropped.

    {2 Examples}

    {[List.map2 (+) [|1;2;3|] [|4;5;6|] = [|5;7;9|]]}

    {[
      List.map2
        Tuple.create
        [|"alice"; "bob"; "chuck"|]
        [|3; 5; 7; 9; 11; 13; 15; 17; 19|]
          = [|("alice", 3); ("bob", 5); ("chuck", 7)|]
    ]}
*)

val map3: f: ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
(** Combine three lists, using [f] to combine each trio of elements.

    If one list is longer, the extra elements are dropped.

    {2 Examples}

    {[
      List.map3
        Tuple3.create
        [|"alice"; "bob"; "chuck"|]
        [|2; 5; 7; 8;|]
        [|true; false; true; false|] =
          [|("alice", 2, true); ("bob", 5, false); ("chuck", 7, true)|]
    ]}
*)

(** {1 Deconstruct} *)

val partition: f: ('a -> bool) -> 'a t -> 'a t * 'a t
(** Split a list into a {!Tuple} of lists. Values which [f] returns true for will end up in {!Tuple.first}.

    {2 Examples}

    {[List.partition Int.isOdd [1;2;3;4;5;6] = ([1;3;5], [2;4;6])]}
*)

val splitAt: index: int -> 'a t -> 'a t * 'a t
(** Divides a list into a {!Tuple} of lists.

    Elements which have index upto (but not including) [index] will be in the first component of the tuple.

    Elements with an index greater than or equal to [index] will be in the second.

    If [index] is outside of the bounds of the list, all elements will be in the first component of the tuple.

    {2 Examples}

    {[List.splitAt 2 [1;2;3;4;5] 2 = ([1;2], [3;4;5])]}
*)

val split_at: index: int -> 'a t -> 'a t * 'a t

val splitWhen: f: ('a -> bool) -> 'a t -> 'a t * 'a t
(** Divides a list into a {!Tuple} at the first element [f] returns [true] for.

    Elements up to (but not including) the first element [f] returns [true] for
    will be in the first component of the tuple, the remaining elements will be
    in the second

    {2 Examples}

    {[List.splitWhen Int.isEven [2; 4; 5; 6; 7]= ([2; 4], [5; 6; 7])]}

    {[List.splitWhen (Fun.constant false) [2; 4; 5; 6; 7] = ([2; 4; 5; 6; 7], [])]}
*)

val split_when: f: ('a -> bool) -> 'a t -> 'a t * 'a t

val unzip: ('a * 'b) t -> 'a t * 'b t
(** Decompose a list of {!Tuple} into a {!Tuple} of lists.

    {2 Examples}

    {[List.unzip [(0, true); (17, false); (1337, true)] = ([0;17;1337], [true; false; true])]}
*)

(** {1 Iterate} *)

val forEach: f: ('a -> unit) -> 'a t -> unit
(** Iterates over the elements of invokes [f] for each element.

    The function you provide must return [unit], and the [forEach] call itself also returns [unit].

    You use [List.forEach] when you want to process a list only for side effects.


    {2 Examples}

    {[
      List.forEach (fun int -> print (Int.toString int) [1; 2; 3] )
      (*
        Prints
        1
        2
        3
      *)
    ]}
*)

val for_each: f: ('a -> unit) -> 'a t -> unit

val forEachWithIndex: f: (int -> 'a -> unit) -> 'a t -> unit
(** Like {!forEach} but [f] is also called with the elements index.

    {2 Examples}

    {[
      List.forEachI (fun index int -> printf "%d: %d" index int) [1; 2; 3]
      (*
        Prints
        0: 1
        1: 2
        2: 3
      *)
    ]}
*)

val for_each_with_index: f: (int -> 'a -> unit) -> 'a t -> unit

val intersperse: sep: 'a -> 'a t -> 'a t
(** Places [sep] between all the elements of the given list.

    {2 Examples}

    {[List.intersperse "on" ["turtles"; "turtles"; "turtles"] = ["turtles"; "on"; "turtles"; "on"; "turtles"]]}

    {[List.intersperse 0 [] = []]}
*)

val chunksOf: size: int -> 'a t -> 'a t t
(** Split a list into equally sized chunks.

    If there aren't enough elements to make the last 'chunk', those elements are ignored.

    {2 Examples}

    {[
      List.chunksOf 2 ["#FFBA49"; "#9984D4"; "#20A39E"; "#EF5B5B"; "#23001E"] =  [
        ["#FFBA49"; "#9984D4"];
        ["#20A39E"; "#EF5B5B"];
      ]
    ]}
 *)

val chunks_of: size: int -> 'a t -> 'a t t

val sliding: step: int -> size: int -> 'a t -> 'a t t
(** Provides a sliding 'window' of sub-lists over a list.

    The first sub-list starts at the head of the list and takes the first [size] elements.

    The sub-list then advances [step] (which defaults to 1) positions before taking the next [size] elements.

    The sub-lists are guaranteed to always be of length [size] and iteration stops once a sub-list would extend beyond the end of the list.

    {2 Examples}

    {[List.sliding 1 1 [1;2;3;4;5] = [[1]; [2]; [3]; [4]; [5]] ]}

    {[List.sliding 1 2 [1;2;3;4;5] = [[1;2]; [2;3]; [3;4]; [4;5]] ]}

    {[List.sliding 1 3 [1;2;3;4;5] = [[1;2;3]; [2;3;4]; [3;4;5]] ]}

    {[List.sliding 2 2 [1;2;3;4;5] = [[1;2]; [3;4]] ]}

    {[List.sliding 1 3 [1;2;3;4;5] = [[1]; [4]] ]}

    {[List.sliding 2 3 [1;2;3;4;5] = [[1; 2]; [4; 5]]]}

    {[List.sliding 1 7 [1;2;3;4;5] = []]}
*)

val groupWhile: f: ('a -> 'a -> bool) -> 'a t -> 'a t t
(** Divide a list into groups.

    [f] is called with consecutive elements, when [f] returns [false] a new group is started.

    {2 Examples}

    {[
      List.groupWhile (Fun.constant false) [1; 2; 3;]  = [[1]; [2]; [3]]
    ]}

    {[
      List.groupWhile (Fun.constant true) [1; 2; 3;] = [[1; 2; 3]]
    ]}

    {[
      List.groupWhile
        String.equal
        ["a"; "b"; "b"; "a"; "a"; "a"; "b"; "a"] =
          [["a"]; ["b"; "b"]; ["a"; "a"; "a";] ["b"]; ["a"]]
    ]}

    {[
      List.groupWhile
        (fun x y -> x mod 2 = y mod 2)
        [2; 4; 6; 5; 3; 1; 8; 7; 9] =
          [[2; 4; 6]; [5; 3; 1]; [8]; [7; 9]]
    ]}
*)

val group_while: f: ('a -> 'a -> bool) -> 'a t -> 'a t t

(** {1 Convert} *)

val join: sep: string -> string t -> string
(** Converts a list of strings into a {!String}, placing [sep] between each string in the result.

    {2 Examples}

    {[List.join ", " ["Ant"; "Bat"; "Cat"] = "Ant, Bat, Cat"]}
 *)

val groupBy: ('value -> 'key) -> 'value t -> Map<'key, 'value list>
(** Collect elements which [f] produces the same key for

    Produces a map from ['key] to a {!List} of all elements which produce the same ['key]

    {2 Examples}

    {[
      let animals = [|"Ant"; "Bear"; "Cat"; "Dewgong"|] in
      Array.groupBy String.length animals = Map.fromList [
        (3, ["Cat"; "Ant"]);
        (4, ["Bear"]);
        (7, ["Dewgong"]);
      ]
    ]}
*)

val group_by: ('value -> 'key) -> 'value t -> Map<'key, 'value list>

val toArray: 'a t -> 'a array
(** Converts a list to an {!Array}. *)

val to_array: 'a t -> 'a array

(** {1 Compare} *)

val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Test two lists for equality using the provided function to test elements. *)

val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Compare two lists using the provided function to compare elements.

    A shorter list is 'less' than a longer one.

    {2 Examples}

    {[List.compare Int.compare [1;2;3] [1;2;3;4] = -1]}

    {[List.compare Int.compare [1;2;3] [1;2;3] = 0]}

    {[List.compare Int.compare [1;2;5] [1;2;3] = 1]}
*)