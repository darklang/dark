module Tablecloth.Option

(** {!Option} represents a value which may not be present.

    It is a variant containing the [(Some 'a)] and [None] constructors

    {[
      type 'a t =
        | Some of 'a
        | None
    ]}

    Many other languages use [null] or [nil] to represent something similar.

    {!Option} values are very common and they are used in a number of ways:
    - Initial values
    - Optional function arguments
    - Optional record fields
    - Return values for functions that are not defined over their entire input range (partial functions).
    - Return value for otherwise reporting simple errors, where None is returned on error.

    Lots of functions in [Standard] return options, one you have one you can
    work with the value it might contain by:

    - Pattern matching
    - Using {!map} or {!andThen} (or their operators in {!Infix})
    - Unwrapping it using {!unwrap}, or its operator {!(|?)}
    - Converting a [None] into an exception using{!unwrapUnsafe}

    If the function you are writing can fail in a variety of ways, use a {!Result} instead to
    better communicate with the caller.

    If a function only fails in unexpected, unrecoverable ways, maybe you want raise exception.
*)

type 'a t = 'a option

val some: 'a -> 'a option
(** A function version of the [Some] constructor.

    In most situations you just want to use the [Some] constructor directly.

    However OCaml doesn't support piping to variant constructors.

    Note that when using the Reason syntax you {b can} use fast pipe ([->]) with variant constructors, so you don't need this function.

    See the {{: https://reasonml.github.io/docs/en/pipe-first#pipe-into-variants} Reason docs } for more.

    {2 Examples}

    {[String.reverse("desserts") |> Option.some = Some "desserts" ]}
 *)

val and_: 'a t -> 'a t -> 'a t
(** Returns [None] if the first argument is [None], otherwise return the second argument.

  Unlike the built in [&&] operator, the [and_] function does not short-circuit.

  When you call [and_], both arguments are evaluated before being passed to the function.

  {2 Examples}

  {[Option.and_ (Some 11) (Some 22) = Some 22]}

  {[Option.and_ None (Some 22) = None]}

  {[Option.and_ (Some 11) None = None]}

  {[Option.and_ None None = None]}
*)

val or_: 'a t -> 'a t -> 'a t
(** Return the first argument if it {!isSome}, otherwise return the second.

    Unlike the built in [||] operator, the [or_] function does not short-circuit.
    When you call [or_], both arguments are evaluated before being passed to the function.

    {2 Examples}

    {[Option.or_ (Some 11) (Some 22) = Some 11]}

    {[Option.or_ None (Some 22) = Some 22]}

    {[Option.or_ (Some 11) None = Some 11]}

    {[Option.or_ None None = None]}
*)

val orElse: 'a t -> 'a t -> 'a t
(** Return the second argument if it {!isSome}, otherwise return the first.

    Like {!or_} but in reverse. Useful when using the [|>] operator

    {2 Examples}

    {[Option.orElse (Some 11) (Some 22) = Some 22]}

    {[Option.orElse None (Some 22) = Some 22]}

    {[Option.orElse (Some 11) None = Some 11]}

    {[Option.orElse None None = None]}
*)

val or_else: 'a t -> 'a t -> 'a t

val both: 'a t -> 'b t -> ('a * 'b) t
(** Transform two options into an option of a {!Tuple}.

    Returns None if either of the aguments is None.

    {2 Examples}

    {[Option.both (Some 3004) (Some "Ant") = Some (3004, "Ant")]}

    {[Option.both (Some 3004) None = None]}

    {[Option.both None (Some "Ant") = None]}

    {[Option.both None None = None]}
*)

val flatten: 'a t t -> 'a t
(** Flatten two optional layers into a single optional layer.

    {2 Examples}

    {[Option.flatten (Some (Some 4)) = Some 4]}

    {[Option.flatten (Some None) = None]}

    {[Option.flatten (None) = None]}
*)

val map: f: ('a -> 'b) -> 'a t -> 'b t
(** Transform the value inside an option.

    Leaves [None] untouched.

    See {!(>>|)} for an operator version of this function.

    {2 Examples}

    {[Option.map (fun x -> x * x) (Some 9) = Some 81]}

    {[Option.map Int.toString (Some 9) = Some "9"]}

    {[Option.map (fun x -> x * x) None = None]}
*)

val map2: f: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** Combine two {!Option}s

    If both options are [Some] returns, as [Some] the result of running [f] on both values.

    If either value is [None], returns [None]

    {2 Examples}

    {[Option.map2 Int.add (Some 3) (Some 4) = Some 7]}

    {[Option.map2 Tuple.make (Some 3) (Some 4) = Some (3, 4)]}

    {[Option.map2 Int.add (Some 3) None = None]}

    {[Option.map2 Int.add None (Some 4) = None]}
*)

val andThen: f: ('a -> 'b t) -> 'a t -> 'b t
(** Chain together many computations that may not return a value.

    It is helpful to see its definition:
    {[
      let andThen f t =
        match t with
        | Some x -> f x
        | None -> None
    ]}

    This means we only continue with the callback if we have a value.

    For example, say you need to parse some user input as a month:

    {[
      let toValidMonth (month: int) : (int option) =
        if (1 <= month && month <= 12) then
          Some month
        else
          None
      in

      let userInput = "5" in

      Int.fromString userInput
      |> Option.andThen toValidMonth
    ]}

    If [String.toInt] produces [None] (because the [userInput] was not an
    integer) this entire chain of operations will short-circuit and result in
    [None]. If [toValidMonth] results in [None], again the chain of
    computations will result in [None].

    See {!(>>=)} for an operator version of this function.

    {2 Examples}

    {[Option.andThen List.head (Some [1, 2, 3]) = Some 1]}

    {[Option.andThen List.head (Some []) = None]}
*)

val and_then: f: ('a -> 'b t) -> 'a t -> 'b t

val unwrap: ``default``: 'a -> 'a t -> 'a
(** Unwrap an [option('a)] returning [default] if called with [None].

    This comes in handy when paired with functions like {!Map.get} or {!List.head} which return an {!Option}.

    See {!(|?)} for an operator version of this function.

    {b Note} This can be overused! Many cases are better handled using pattern matching, {!map} or {!andThen}.

    {2 Examples}

    {[Option.unwrap 99 (Some 42) = 42]}

    {[Option.unwrap 99 None = 99]}

    {[Option.unwrap "unknown" (Map.get Map.String.empty "Tom") = "unknown"]}
*)

val unwrapUnsafe: 'a t -> 'a
(** Unwrap an [option('a)] returning the enclosed ['a].

    {b Note} in most situations it is better to use pattern matching, {!unwrap}, {!map} or {!andThen}.
    Can you structure your code slightly differently to avoid potentially raising an exception?

    {3 Exceptions}

    Raises an [System.ArgumentException] exception if called with [None]

    {2 Examples}

    {[List.head [1;2;3] |> Option.unwrapUnsafe = 1]}

    {[List.head [] |> Option.unwrapUnsafe]}
*)

val unwrap_unsafe: 'a t -> 'a

val isSome: 'a t -> bool
(** Check if an {!Option} is a [Some].

    In most situtations you should just use pattern matching instead.

    {2 Examples}

    {[Option.isSome (Some 3004) = true]}

    {[Option.isSome None = false]}
*)

val is_some: 'a t -> bool

val isNone: 'a t -> bool
(** Check if an {!Option} is a [None].

    In most situtations you should just use pattern matching instead.

    {2 Examples}

    {[Option.isNone (Some 3004) = false]}

    {[Option.isNone None = true]}
*)

val is_none: 'a t -> bool

val tap: f: ('a -> unit) -> 'a t -> unit
(** Run a function against a value, if it is present. *)

val toArray: 'a t -> 'a array
(** Convert an option to a {!Array}.

    [None] is represented as an empty list and [Some] is represented as a list of one element.

    {2 Examples}

    {[Option.toArray (Some 3004) = [|3004|]]}

    {[Option.toArray (None) = [||]]}
*)

val to_array: 'a t -> 'a array

val toList: 'a t -> 'a list
(** Convert an option to a {!List}.

    [None] is represented as an empty list and [Some] is represented as a list of one element.

    {2 Examples}

    {[Option.toList (Some 3004) = [3004]]}

    {[Option.toList (None) = []]}
*)

val to_list: 'a t -> 'a list

(** {1 Compare} *)

val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Test two optional values for equality using the provided function

    {2 Examples}

    {[Option.equal Int.equal (Some 1) (Some 1) = true]}

    {[Option.equal Int.equal (Some 1) (Some 3) = false]}

    {[Option.equal Int.equal (Some 1) None = false]}

    {[Option.equal Int.equal None None = true]}
*)

val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Compare two optional values using the provided function.

    A [None] is "less" than a [Some]

    {2 Examples}

    {[Option.compare Int.compare (Some 1) (Some 3) = -1]}

    {[Option.compare Int.compare (Some 1) None = 1]}

    {[Option.compare Int.compare None None = 0]}
*)

(** {1 Operators}

    For code that works extensively with {!Option}s these operators can make things
    significantly more concise at the expense of placing a greater cognitive burden
    on future readers.

    {[
      let nameToAge = Map.String.fromArray [|
        ("Ant", 1);
        ("Bat", 5);
        ("Cat", 19);
      |] in

      let catAge = Map.get nameToAge "Cat" |? 8 in
      (* 19 *)

      Option.(
        Map.get nameToAge "Ant" >>= (fun antAge ->
          Map.get nameToAge "Bat" >>| (fun batAge ->
            Int.absolute(batAge - antAge)
          )
        )
      )
      (* Some (4) *)
    ]}
*)

val (|?): 'a t -> 'a -> 'a
(** The operator version of {!get}

    {2 Examples}

    {[Some 3004 |? 8 = 3004]}

    {[None |? 8 = 8]}
*)

val (>>|): 'a t -> ('a -> 'b) -> 'b t
(** The operator version of {!map}

    {2 Examples}

    {[Some "desserts" >>| String.reverse = Some "stressed"]}

    {[None >>| String.reverse = None]}
*)

val (>>=): 'a t -> ('a -> 'b t) -> 'b t
(** The operator version of {!andThen}

    {2 Examples}

    {[Some [1, 2, 3] >>= List.head = Some 1]}

    {[Some [] >>= List.head = None]}
*)