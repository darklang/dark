open Prelude

let toID = (b: blankOr<'a>): id =>
  switch b {
  | Blank(id) => id
  | F(id, _) => id
  }

let new_ = ((): unit): blankOr<'a> => Blank(gid())

let newF = (a: 'a): blankOr<'a> => F(gid(), a)

let clone = (fn: 'a => 'a, b: blankOr<'a>): blankOr<'a> =>
  switch b {
  | Blank(_) => Blank(gid())
  | F(_, val_) => F(gid(), fn(val_))
  }

let isBlank = (b: blankOr<'a>): bool =>
  switch b {
  | Blank(_) => true
  | F(_, _) => false
  }

let isF = (b: blankOr<'a>): bool => !isBlank(b)

let isFilledValue = (b: blankOr<'a>, a: 'a): bool =>
  switch b {
  | F(_, v) if v == a => true
  | _ => false
  }

let valueWithDefault = (a: 'a, b: blankOr<'a>): 'a =>
  switch b {
  | F(_, v) => v
  | Blank(_) => a
  }

let map = (~f: 'a => 'b, bo: blankOr<'a>): blankOr<'b> =>
  switch bo {
  | F(id, v) => F(id, f(v))
  | Blank(id) => Blank(id)
  }

let toOption = (b: blankOr<'a>): option<'a> =>
  switch b {
  | F(_, v) => Some(v)
  | Blank(_) => None
  }

let ofOption = (o: option<'a>): blankOr<'a> =>
  switch o {
  | Some(v) => newF(v)
  | None => new_()
  }

let toOptionID = (b: blankOr<'a>): (option<'a>, ID.t) =>
  switch b {
  | F(id, v) => (Some(v), id)
  | Blank(id) => (None, id)
  }

let fromOptionID = (o: option<'a>, id: ID.t): blankOr<'a> =>
  switch o {
  | Some(v) => F(id, v)
  | None => Blank(id)
  }

/// We're trying to remove blankOrs, and instead use strings. However, for
/// compatibility we need to switch between them somethings. This creates a blankOr
/// from a string,id pair, taking into account if the string is empty when choosing
/// whether to make it Blank or Filled.
let fromStringID = (s: string, id: ID.t): blankOr<string> =>
  if s == "" {
    Blank(id)
  } else {
    F(id, s)
  }

let toStringID = (bo: blankOr<string>): (string, ID.t) =>
  switch bo {
  | F(id, str) => (str, id)
  | Blank(id) => ("", id)
  }

let replace = (search: id, replacement: blankOr<'a>, bo: blankOr<'a>): blankOr<'a> =>
  if toID(bo) == search {
    replacement
  } else {
    bo
  }
