@ppx.deriving(show({with_path: false}))
type rec t<'a> =
  | Blank(ID.t)
  | F(ID.t, 'a)

let encode = (encoder: 'a => Js.Json.t, v: t<'a>) => {
  open Json_encode_extended
  switch v {
  | F(i, s) => variant("Filled", list{ID.encode(i), encoder(s)})
  | Blank(i) => variant("Blank", list{ID.encode(i)})
  }
}

let decode = d => {
  open Json_decode_extended
  variants(list{
    ("Filled", variant2((id, v) => F(id, v), ID.decode, d)),
    ("Blank", variant1(id => Blank(id), ID.decode)),
  })
}

let toID = (b: t<'a>): ID.t =>
  switch b {
  | Blank(id) => id
  | F(id, _) => id
  }

let new_ = ((): unit): t<'a> => Blank(ID.generate())

let newF = (a: 'a): t<'a> => F(ID.generate(), a)

let clone = (fn: 'a => 'a, b: t<'a>): t<'a> =>
  switch b {
  | Blank(_) => Blank(ID.generate())
  | F(_, val_) => F(ID.generate(), fn(val_))
  }

let isBlank = (b: t<'a>): bool =>
  switch b {
  | Blank(_) => true
  | F(_, _) => false
  }

let isF = (b: t<'a>): bool => !isBlank(b)

let isFilledValue = (b: t<'a>, a: 'a): bool =>
  switch b {
  | F(_, v) if v == a => true
  | _ => false
  }

let valueWithDefault = (a: 'a, b: t<'a>): 'a =>
  switch b {
  | F(_, v) => v
  | Blank(_) => a
  }

let map = (~f: 'a => 'b, bo: t<'a>): t<'b> =>
  switch bo {
  | F(id, v) => F(id, f(v))
  | Blank(id) => Blank(id)
  }

let toOption = (b: t<'a>): option<'a> =>
  switch b {
  | F(_, v) => Some(v)
  | Blank(_) => None
  }

let ofOption = (o: option<'a>): t<'a> =>
  switch o {
  | Some(v) => newF(v)
  | None => new_()
  }

let toOptionID = (b: t<'a>): (option<'a>, ID.t) =>
  switch b {
  | F(id, v) => (Some(v), id)
  | Blank(id) => (None, id)
  }

let fromOptionID = (o: option<'a>, id: ID.t): t<'a> =>
  switch o {
  | Some(v) => F(id, v)
  | None => Blank(id)
  }

/// We're trying to remove blankOrs, and instead use strings. However, for
/// compatibility we need to switch between them sometimes. This creates a
/// blankOr from a (string*id) pair, taking into account if the string is empty
/// when choosing whether to make it Blank or Filled.
let fromStringID = (s: string, id: ID.t): t<string> =>
  if s == "" {
    Blank(id)
  } else {
    F(id, s)
  }

let optionToString = (bo: option<t<string>>): string =>
  switch bo {
  | Some(F(_, str)) => str
  | None | Some(Blank(_)) => ""
  }

let toString = (bo: t<string>): string =>
  switch bo {
  | F(_, str) => str
  | Blank(_) => ""
  }

let toStringID = (bo: t<string>): (string, ID.t) =>
  switch bo {
  | F(id, str) => (str, id)
  | Blank(id) => ("", id)
  }

let replace = (search: ID.t, replacement: t<'a>, bo: t<'a>): t<'a> =>
  if toID(bo) == search {
    replacement
  } else {
    bo
  }
