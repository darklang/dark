open Prelude
open Types

let toID (b : 'a blankOr) : id =
  match b with Blank id -> id | F (id, _) -> id


let new_ (() : unit) : 'a blankOr = Blank (gid ())

let newF (a : 'a) : 'a blankOr = F (gid (), a)

let clone (fn : 'a -> 'a) (b : 'a blankOr) : 'a blankOr =
  match b with Blank _ -> Blank (gid ()) | F (_, val_) -> F (gid (), fn val_)


let isBlank (b : 'a blankOr) : bool =
  match b with Blank _ -> true | F (_, _) -> false


let isF (b : 'a blankOr) : bool = not (isBlank b)

let asF (b : 'a blankOr) : 'a option =
  match b with F (_, v) -> Some v | Blank _ -> None


let valueWithDefault (a : 'a) (b : 'a blankOr) : 'a =
  match b with F (_, v) -> v | Blank _ -> a


let toMaybe (b : 'a blankOr) : 'a option =
  match b with F (_, v) -> Some v | Blank _ -> None


let ofOption (o : 'a option) : 'a blankOr =
  match o with Some v -> newF v | None -> new_ ()


let replace (search : id) (replacement : 'a blankOr) (bo : 'a blankOr) :
    'a blankOr =
  if toID bo = search then replacement else bo

let deBlank (msg : string) (x : 'a blankOr) : 'a =
  match x with
  | F (_, y) ->
      y
  | Blank _ ->
      impossible ("got Blank but expected Flagged: " ^ msg)
