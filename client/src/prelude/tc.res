/* This file is a local variant of Tablecloth. It allows us to add, modify,
 * etc, tablecloth function and type definitions, to be upstreamed later. */
module Caml = {
  module String = String
  module List = List
  module Char = Char
  module Array = Array
  module Map = Map
  module Set = Set
}

/* This allows us override submodules. See
 * http://gallium.inria.fr/blog/overriding-submodules/ */
include (
  Tablecloth: module type of Tablecloth
    with module Option := Tablecloth.Option
    and module Int := Tablecloth.Int
    and module Float := Tablecloth.Float
    and module String := Tablecloth.String
    and module Array := Tablecloth.Array
    and module Result := Tablecloth.Result
    and module List := Tablecloth.List
    and module Set := Tablecloth.Set
    and module Map := Tablecloth.Map
)

let \"<|" = (a, b) => a(b)

let \">>" = (f1: 'a => 'b, f2: 'b => 'c): ('a => 'c) => x => x |> f1 |> f2

let \"<<" = (f1: 'b => 'c, f2: 'a => 'b): ('a => 'c) => x => x |> f2 |> f1

let identity = (value: 'a): 'a => value

module Array = {
  include Tablecloth.Array
}

module Option = {
  include Tablecloth.Option

  let values = (l: list<option<'a>>): list<'a> => {
    let valuesHelper = (l: list<'a>, item: option<'a>): list<'a> =>
      switch item {
      | None => l
      | Some(v) => list{v, ...l}
      }

    Tablecloth.List.foldRight(~f=valuesHelper, ~initial=list{}, l)
  }

  let orLazy = (v: option<'a>, v2: unit => option<'a>): option<'a> =>
    switch v {
    | Some(v) => Some(v)
    | None => v2()
    }

  let orElseLazy = (v: unit => option<'a>, v2: option<'a>): option<'a> =>
    switch v2 {
    | Some(v2) => Some(v2)
    | None => v()
    }

  let pair = (a: option<'a>, b: option<'b>): option<('a, 'b)> =>
    switch (a, b) {
    | (Some(a), Some(b)) => Some(a, b)
    | _ => None
    }

  let andThen2 = (a: option<'a>, b: option<'b>, ~f: ('a, 'b) => option<'c>): option<'c> =>
    switch (a, b) {
    | (Some(a), Some(b)) => f(a, b)
    | _ => None
    }

  let isSomeEqualTo = (~value: 'a, o: option<'a>): bool => Some(value) == o

  /* If a is some, then apply fn to a, return both a and the result.
    if either a or b is none, then return none
 */
  let thenAlso = (a: option<'a>, ~f: 'a => option<'b>): option<('a, 'b)> => {
    let b = andThen(~f, a)
    pair(a, b)
  }

  let unwrapLazy = (a: option<'a>, ~default: unit => 'a): 'a =>
    switch a {
    | Some(a) => a
    | None => default()
    }
}

module Either = {
  type t<'a, 'b> =
    | Left('a)
    | Right('b)
}

module List = {
  include Tablecloth.List

  let getBy = (~f: 'a => bool, l: list<'a>): option<'a> => Belt.List.getBy(l, f)

  let uniqueBy = (~f: 'a => string, l: list<'a>): list<'a> => {
    let rec uniqueHelper = (
      f: 'a => string,
      existing: Belt.Set.String.t,
      remaining: list<'a>,
      accumulator: list<'a>,
    ) =>
      switch remaining {
      | list{} => reverse(accumulator)
      | list{first, ...rest} =>
        let computedFirst = f(first)
        if Belt.Set.String.has(existing, computedFirst) {
          uniqueHelper(f, existing, rest, accumulator)
        } else {
          uniqueHelper(
            f,
            Belt.Set.String.add(existing, computedFirst),
            rest,
            list{first, ...accumulator},
          )
        }
      }

    uniqueHelper(f, Belt.Set.String.empty, l, list{})
  }

  let sortBy = (~f: 'a => 'b, l: list<'a>): list<'a> =>
    Belt.List.sort(l, (a, b) => {
      let a' = f(a)
      let b' = f(b)
      if a' == b' {
        0
      } else if a' < b' {
        -1
      } else {
        1
      }
    })

  let sortWith = (f: ('a, 'a) => int, l: list<'a>): list<'a> => Belt.List.sort(l, f)

  // From https://github.com/janestreet/base/blob/eaab227499b36bb90c2537bc6358a2d5caf75227/src/list.ml#L247
  let findMap = (t, ~f) => {
    let rec loop = x =>
      switch x {
      | list{} => None
      | list{x, ...l} =>
        switch f(x) {
        | None => loop(l)
        | Some(_) as r => r
        }
      }

    loop(t)
  }

  let member = (~value: 'v, l: list<'v>): bool => Tablecloth.List.includes(~equal=\"=", l, value)

  let foldr = (~init, ~f, list) => Tablecloth.List.fold_right(~initial=init, ~f, list)

  let findWithIndex = (~f: (int, 'a) => bool, l: list<'a>): option<int> => {
    let rec findIndexHelper = (~i: int, ~predicate: (int, 'a) => bool, l: list<'a>): option<int> =>
      switch l {
      | list{} => None
      | list{x, ...rest} =>
        if predicate(i, x) {
          Some(i)
        } else {
          findIndexHelper(~i=i + 1, ~predicate, rest)
        }
      }

    findIndexHelper(~i=0, ~predicate=f, l)
  }

  let range = (start: int, end_: int): list<'a> => {
    let length = end_ - start
    if length < 0 {
      list{}
    } else {
      Belt.List.makeBy(length, i => i + start)
    }
  }

  // Takes everything before and after, but not including nexted element
  let splitOn = (~index: int, l: list<'a>): (list<'a>, list<'a>) => (
    take(~count=index, l),
    drop(~count=index + 1, l),
  )

  /* Moves item in oldPos into the position at newPos, pushing the element already at newPos down. Ex:
    l = [a b c d]
    moveInto 3 1 l, takes d and moves it between a & b. => [a d b c]
    NOTE: This is not swapping the elements in newPos & oldPos
 */

  let moveInto = (~oldPos: int, ~newPos: int, l: list<'a>): list<'a> =>
    switch getAt(~index=oldPos, l) {
    | Some(value) =>
      let index = // Checks to see if we need to offset the newPos by -1, after removing the element at oldPos
      if newPos > oldPos {
        let len = List.length(l)
        // Clamp at list length to prevent overflow
        if newPos > len {
          len - 1
        } else {
          newPos - 1
        }
      } else {
        newPos
      }

      l |> removeAt(~index=oldPos) |> insertAt(~index, ~value)
    | None => l
    }

  /* Partition into two lists, of potentially different type, using function
   * `f`.  Returns value in the first list for `Left` and second list for
   * `Right`. */
  let partitionMap = (~f: 'c => Either.t<'a, 'b>, items: list<'c>): (list<'a>, list<'b>) =>
    Tablecloth.List.foldRight(
      ~initial=(list{}, list{}),
      ~f=((lefts, rights), item) =>
        switch f(item) {
        | Left(a) => (list{a, ...lefts}, rights)
        | Right(b) => (lefts, list{b, ...rights})
        },
      items,
    )

  let elemIndex = (~value: 'a, l: list<'a>): option<int> =>
    l |> Tablecloth.List.findIndex(~f=(_i, v) => v == value) |> Option.map(~f=Tuple2.first)
}

module Result = {
  include Tablecloth.Result

  let combine = (l: list<t<'ok, 'err>>): t<list<'ok>, 'err> =>
    List.foldRight(~f=map2(~f=(accum, r) => list{r, ...accum}), ~initial=Ok(list{}), l)
}

module Float = {
  include Tablecloth.Float
}

module Int = {
  include Tablecloth.Int
}

module String = {
  include Tablecloth.String

  let splitAt = (~index: int, s: string): (string, string) => (
    slice(~from=0, ~to_=index, s),
    slice(~from=index, ~to_=length(s), s),
  )

  let left = (~count: int, s: string): string => slice(~from=0, ~to_=count, s)

  let rec segment = (~size: int, s: string): list<string> => {
    let (front, back) = splitAt(~index=size, s)
    if back == "" {
      list{front}
    } else {
      list{front, ...segment(~size, back)}
    }
  }

  let replaceChunk = (~from: int, ~to_: int, ~replacement: string, s): string =>
    slice(~from=0, ~to_=from, s) ++ (replacement ++ slice(~from=to_, ~to_=length(s), s))

  // returns the index of the last occurrence of character c in string s before position i+1 or None if c does not occur in s before position i+1.
  let rindex_from_opt = (~pos: int, s: string, c: char): option<int> =>
    String.rindex_from_opt(s, pos, c)

  /* returns the index of the first occurrence of character c in string s after position i or None if c does not occur in s after position i.
   */
  let index_from_opt = (~pos: int, s: string, c: char): option<int> =>
    String.index_from_opt(s, pos, c)

  let join = (~sep: string, strings: list<string>) => Js.Array.joinWith(sep, List.toArray(strings))
}

module Map = {
  // Include this way to allow adding to String and Int submodules
  include (
    Tablecloth.Map: module type of Tablecloth.Map
      with module String := Tablecloth.Map.String
      and module Int := Tablecloth.Map.Int
  )

  let updateIfPresent = (~key: 'key, ~f: 'value => 'value, dict: t<'key, 'value, 'id>): t<
    'key,
    'value,
    'id,
  > => Tablecloth.Map.update(~key, ~f=Option.map(~f), dict)

  let mergeLeft = (dict1: t<'key, 'value, 'id>, dict2: t<'key, 'value, 'id>): t<
    'key,
    'value,
    'id,
  > => Tablecloth.Map.merge(~f=(_key: 'key, v1: option<'v>, v2: option<'v>) =>
      switch (v1, v2) {
      | (Some(_), _) => v1
      | (None, _) => v2
      }
    , dict1, dict2)

  let mergeRight = (dict1: t<'key, 'value, 'id>, dict2: t<'key, 'value, 'id>): t<
    'key,
    'value,
    'id,
  > => Tablecloth.Map.merge(~f=(_key: 'key, v1: option<'v>, v2: option<'v>) =>
      switch (v1, v2) {
      | (_, Some(_)) => v2
      | (_, None) => v1
      }
    , dict1, dict2)

  let mapValues = (dict: t<'key, 'value, 'id>, ~f: 'value => 'x): list<'x> =>
    dict |> Tablecloth.Map.values |> List.map(~f)

  let filterMapValues = (dict: t<'key, 'value, 'id>, ~f: 'value => option<'x>): list<'x> =>
    dict |> Tablecloth.Map.values |> List.filterMap(~f)

  let remove = (dict: t<'key, 'value, 'id>, ~key: 'key): t<'key, 'value, 'id> =>
    Tablecloth.Map.remove(dict, key)

  let removeMany = (dict: t<'key, 'value, 'id>, ~keys: list<'key>): t<'key, 'value, 'id> =>
    Belt.Map.removeMany(dict, Belt.List.toArray(keys))

  let has = (~key, dict: t<'key, 'value, 'id>): bool => Tablecloth.Map.includes(dict, key)

  let get = (~key, dict: t<'key, 'value, 'id>): option<'value> => Tablecloth.Map.get(dict, key)

  // Js.String.make gives us "[object Object]", so we actually want our own toString. Not perfect, but slightly nicer (e.g., for App.ml's DisplayAndReportHttpError, info's values are all strings, which this handles)
  let toString = (d: t<'key, 'value, 'id>) =>
    d
    |> toList
    |> List.map(~f=((k, v)) => "\"" ++ (k ++ ("\": \"" ++ (Js.String.make(v) ++ "\""))))
    |> List.join(~sep=", ")
    |> (s => "{" ++ (s ++ "}"))

  module String = {
    include Tablecloth.Map.String
  }

  module Int = {
    include Tablecloth.Map.Int
  }
}

module Set = {
  // Include this way to allow adding to String and Int submodules
  include (
    Tablecloth.Set: module type of Tablecloth.Set
      with module String := Tablecloth.Set.String
      and module Int := Tablecloth.Set.Int
  )

  module String = {
    include Tablecloth.Set.String
  }

  module Int = {
    include Tablecloth.Set.Int
  }

  let removeMany = (~values: list<'key>, set: t<'key, 'id>): t<'key, 'id> =>
    Belt.Set.removeMany(set, Array.fromList(values))

  let addMany = (~values: list<'key>, set: t<'key, 'id>): t<'key, 'id> =>
    Tablecloth.List.fold(values, ~initial=set, ~f=(acc, v) => Tablecloth.Set.add(acc, v))

  let add = (~value: 'key, set: t<'key, 'id>): t<'key, 'id> => Tablecloth.Set.add(set, value)

  let member = (~value: 'key, set: t<'key, 'id>): bool => Tablecloth.Set.includes(set, value)

  let remove = (~value: 'key, set: t<'key, 'id>): t<'key, 'id> => Tablecloth.Set.remove(set, value)

  let empty = Tablecloth.Set.empty
}
