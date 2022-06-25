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

let replace = (search: id, replacement: blankOr<'a>, bo: blankOr<'a>): blankOr<'a> =>
  if toID(bo) == search {
    replacement
  } else {
    bo
  }
