module Tablecloth.Map

(** A [Map] represents a unique mapping from keys to values.

    [Map] is an immutable data structure which means operations like {!Map.add}
    and {!Map.remove} do not modify the data structure, but return a new map
    with the desired changes.

    Unlike OCaml and Bucklescript versions of Maps, F# Maps work on any type
    with the comparison trait, and do not require different modules.  *)

type t<'key, 'value when 'key: comparison> = Map<'key, 'value>

(** {1 Create}

    Specialised versions of the {!empty}, {!singleton}, {!fromList} and {!fromArray} functions available in the {!Set.Int} and {!Set.String} sub-modules.
*)

val empty: t<'key, 'value>
(** A map with nothing in it.

    Often used as an intial value for functions like {!Array.fold}

    {2 Examples}

    {[
      Array.fold
        [|"Pear", "Orange", "Grapefruit"|]
        Map.empty
        (fun lengthToFruit fruit ->
          Map.add lengthToFruit (String.length fruit) fruit
        )
      |> Map.toArray
      = [|(4, "Pear"); (6, "Orange"), (10, "Grapefruit")|]
    ]}

    In this particular case you might want to use {!Array.groupBy}
*)

val singleton: key: 'key -> value: 'value -> t<'key, 'value>
(** Create a map from a key and value

    {2 Examples}

    {[Map.singleton (module Int) ~key:1 ~value:"Ant" |> Map.toList = [(1, "Ant")]]}
*)

val fromArray: ('key * 'value) array -> t<'key, 'value>
(** Create a map from an {!Array} of key-value tuples *)

val from_array: ('key * 'value) array -> t<'key, 'value>

val fromList: ('key * 'value) list -> t<'key, 'value>
(** Create a map of a {!List} of key-value tuples *)

val from_list: ('key * 'value) list -> t<'key, 'value>

(** {1 Basic operations} *)

val add: key: 'key -> value: 'value -> t<'key, 'value> -> t<'key, 'value>
(** Adds a new entry to a map. If [key] is allready present, its previous value is replaced with [value].

    {2 Examples}

    {[
      Map.add
        3
        "Cat"
        (Map.Int.fromList [(1, "Ant"); (2, "Bat")])
      |> Map.toList = [(1, "Ant"); (2, "Bat"); (3, "Cat")]
    ]}

    {[Map.add 2 "Bug" (Map.Int.fromList [(1, "Ant"); (2, "Bat")]) |> Map.toList = [(1, "Ant"); (2, "Bug")]]}
*)

// val ( .?{}<- ) :
//   ('key, 'value) t -> 'key -> 'value -> ('key, 'value) t
// (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!add}
//
//     {b Note} Currently this is only supported by the OCaml syntax.
//
//     {2 Examples}
//
//     {[
//       let indexToAnimal = Map.Int.fromList [(1, "Ant");(2, "Bat");(3, "Cat")] in
//       let indexToAnimal = numbers.Map.?{4} <- "Dog" in
//       indexToAnimal.Map.?{4} = Some "Dog"
//     ]}
//  *)
//
val remove: 'key -> t<'key, 'value> -> t<'key, 'value>
(** Removes a key-value pair from a map based on they provided key.

    {2 Examples}
    {[
      let animalPopulations = Map.String.fromList [
        ("Elephant", 3_156);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ] in
      Map.remove animalPopulations "Mosquito" |> Map.toList = [
        ("Elephant", 3_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ]
    ]}
*)

val get: 'key -> t<'key, 'value> -> 'value option
(** Get the value associated with a key. If the key is not present in the map, returns [None].

    {2 Examples}

    let animalPopulations = Map.String.fromList [
      ("Elephant", 3_156);
      ("Mosquito", 56_123_156);
      ("Rhino", 3);
      ("Shrew", 56_423);
    ] in
    Map.get animalPopulations "Shrew" = Some 56_423;
*)

// val ( .?{} ) : ('key, 'value) t -> 'key -> 'value option
// (** The {{: https://caml.inria.fr/pub/docs/manual-ocaml/indexops.html } index operator} version of {!Map.get}
//
//     {b Note} Currently this is only supported by the OCaml syntax.
//
//     {2 Examples}
//
//     {[
//       let indexToAnimal = Map.Int.fromList [(1, "Ant");(2, "Bat");(3, "Cat")] in
//       indexToAnimal.Map.?{3} = Some "Cat"
//     ]}
//  *)
//
val update:
  key: 'key ->
  f: ('value option -> 'value option) ->
  t<'key, 'value> ->
    t<'key, 'value>
(** Update the value for a specific key using [f]. If [key] is not present in the map [f] will be called with [None].

    {2 Examples}

    {[
      let animalPopulations = Map.fromList [
        ("Elephant", 3_156);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ] in

      Map.update animalPopulations "Hedgehog" (fun population ->
        match population with
        | None -> Some 1
        | Some count -> Some (count + 1)
      )
      |> Map.toList = [
        ("Elephant", 3_156);
        ("Hedgehog", 1);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ]
    ]}
*)

(** {1 Query} *)

val isEmpty: t<'key, 'value> -> bool
(** Determine if a map is empty. *)

val is_empty: t<'key, 'value> -> bool

val length: t<'key, 'value> -> int
(** Returns the number of key-value pairs present in the map.

    {2 Examples}

    {[
      Map.Int.fromList [(1, "Hornet"); (3, "Marmot")]
      |> Map.length = 2
    ]}
*)

val any: f: ('value -> bool) -> t<'key, 'value> -> bool
(** Determine if [f] returns [true] for [any] values in a map. *)

val all: f: ('value -> bool) -> t<'key, 'value> -> bool
(** Determine if [f] returns [true] for [all] values in a map. *)

val find: f: ('key -> 'value -> bool) -> t<'key, 'value> -> ('key * 'value) option
(** Returns, as an {!Option} the first key-value pair for which [f] evaluates to true.

    If [f] doesn't return [true] for any of the elements [find] will return [None].

    Searches starting from the smallest {b key}

    {2 Examples}

    {[
      Map.String.fromList [
        ("Elephant", 3_156);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ]
      |> Map.find (fun key value -> value > 10_000)
        = Some ("Mosquito", 56_123_156)
    ]}
*)

val includes: 'key -> t<'key, 'value> -> bool
(** Determine if a map includes [key].  *)

val minimum: t<'key, 'value> -> 'key option
(** Returns, as an {!Option}, the smallest {b key} in the map.

    Returns [None] if the map is empty.

    {2 Examples}

    {[
      Map.Int.fromList [(8, "Pigeon"); (1, "Hornet"); (3, "Marmot")]
      |> Map.minimum = Some 1
    ]}
*)

val maximum: t<'key, 'value> -> 'key option
(** Returns the largest {b key } in the map.

    Returns [None] if the map is empty.

    {2 Examples}

    {[
      Map.Int.fromList [(8, "Pigeon"); (1, "Hornet"); (3, "Marmot")]
      |> Map.maximum = Some 8
    ]}
*)

val extent: t<'key, 'value> -> ('key * 'key) option
(** Returns, as an {!Option}, a {!Tuple} of the [(minimum, maximum)] {b key}s in the map.

    Returns [None] if the map is empty.

    {2 Examples}

    {[
      Map.fromList [(8, "Pigeon"); (1, "Hornet"); (3, "Marmot")]
      |> Map.extent = Some (1, 8)
    ]}
*)

(** {1 Combine} *)

// val merge:
//   f:('key -> 'v1 option -> 'v2 option -> 'v3 option)
//    -> t<'key, 'v1> -> t<'key, 'v2> -> t<'key, 'v3>
(** Combine two maps.

    You provide a function [f] which is provided the key and the optional
    value from each map and needs to account for the three possibilities:

    1. Only the 'left' map includes a value for the key.
    2. Both maps contain a value for the key.
    3. Only the 'right' map includes a value for the key.

    You then traverse all the keys, building up whatever you want.

    {2 Examples}

    {[
      let animalToPopulation =
        Map.fromList [
          ("Elephant", 3_156);
          ("Shrew", 56_423);
        ]
      in
      let animalToPopulationGrowthRate = Map.fromList [
        ("Elephant", 0.88);
        ("Squirrel", 1.2);
        ("Python", 4.0);
      ] in

      Map.merge
        (fun _animal population growth ->
          match (Option.both population growth) with
          | Some (population, growth) ->
              Some Float.((ofInt population) * growth)
          | None -> None
        )
        animalToPopulation
        animalToPopulationGrowthRate
      |> Map.toList
        = [("Elephant", 2777.28)]
    ]}
*)

(** {1 Transform} *)

val map: f: ('value -> 'b) -> t<'key, 'value> -> t<'key, 'b>
(** Apply a function to all values in a dictionary.

    {2 Examples}

    {[
      Map.String.fromList [
        ("Elephant", 3_156);
        ("Shrew", 56_423);
      ]
      |> Map.map ~f:Int.toString
      |> Map.toList
        = [
        ("Elephant", "3156");
        ("Shrew", "56423");
      ]
    ]}
*)

val mapWithIndex: f: ('key -> 'value -> 'b) -> t<'key, 'value> -> t<'key, 'b>
(** Like {!map} but [f] is also called with each values corresponding key *)

val map_with_index: f: ('key -> 'value -> 'b) -> t<'key, 'value> -> t<'key, 'b>

val filter: f: ('value -> bool) -> t<'key, 'value> -> t<'key, 'value>
(** Keep elements that [f] returns [true] for.

    {2 Examples}

    {[
      Map.String.fromList [
        ("Elephant", 3_156);
        ("Shrew", 56_423);
      ]
      |> Map.filter (fun population -> population > 10_000)
      |> Map.toList
        = [
        ("Shrew", "56423");
      ]
    ]}
*)

val filterWithIndex:
  f: ('key -> 'value -> bool) -> t<'key, 'value> -> t<'key, 'value>
(** Keep elements that [f] returns [true] for.

    {2 Examples}

    {[
      Map.String.fromList [
        ("Elephant", 3_156);
        ("Shrew", 56_423);
      ]
      |> Map.filter (fun _ population -> population > 10_000)
      |> Map.toList
        = [
        ("Shrew", "56423");
      ]
    ]}
*)

val filter_with_index:
  f: ('key -> 'value -> bool) -> t<'key, 'value> -> t<'key, 'value>

val partition:
  f: ('key -> 'value -> bool) -> t<'key, 'value> -> t<'key, 'value> * t<'key, 'value>
(** Divide a map into two, the first map will contain the key-value pairs that [f] returns [true] for, pairs that [f] returns [false] for will end up in the second.

    {2 Examples}

    {[
      let (endangered, notEndangered) = Map.String.fromList [
        ("Elephant", 3_156);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ]
      |> Map.partition ~f:(fun population -> population < 10_000)
      in

      Map.toList endangered = [
        ("Elephant", 3_156);
        ("Rhino", 3);
      ];

      Map.toList notEndangered = [
        ("Mosquito", 56_123_156);
        ("Shrew", 56_423);
      ];
    ]}
*)

val fold: initial: 'a -> f: ('a -> 'key -> 'value -> 'a) -> t<'key, 'value> -> 'a
(** Like {!Array.fold} but [f] is also called with both the [key] and [value] *)

(** {1 Iterate} *)

val forEach: f: ('value -> unit) -> t<'key, 'value> -> unit
(** Runs a function [f] against each {b value} in the map. *)

val for_each: f: ('value -> unit) -> t<'key, 'value> -> unit

val forEachWithIndex: f: ('key -> 'value -> unit) -> t<'key, 'value> -> unit
(** Like {!Map.forEach} except [~f] is also called with the corresponding key *)

val for_each_with_index: f: ('key -> 'value -> unit) -> t<'key, 'value> -> unit

(** {1 Convert} *)

val keys: t<'key, 'value> -> 'key list
(** Get a {!List} of all of the keys in a map.

    {2 Examples}

    {[
      Map.fromList [
        ("Elephant", 3_156);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ]
      |> Map.keys = [
        "Elephant";
        "Mosquito";
        "Rhino";
        "Shrew";
      ]
    ]}
*)

val values: t<'key, 'value> -> 'value list
(** Get a {!List} of all of the values in a map.

    {2 Examples}

    {[
      Map.fromList [
        ("Elephant", 3_156);
        ("Mosquito", 56_123_156);
        ("Rhino", 3);
        ("Shrew", 56_423);
      ]
      |> Map.values = [
        3_156;
        56_123_156;
        3;
        56_423;
      ]
    ]}
*)

val toArray: t<'key, 'value> -> ('key * 'value) array
(** Get an {!Array} of all of the key-value pairs in a map. *)

val to_array: t<'key, 'value> -> ('key * 'value) array

val toList: t<'key, 'value> -> ('key * 'value) list
(** Get a {!List} of all of the key-value pairs in a map. *)

val to_list: t<'key, 'value> -> ('key * 'value) list