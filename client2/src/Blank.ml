open Belt
open Tea
open Porting
open Prelude
open Types

let toID b = match b with Blank id -> id | F (id, _) -> id

let toMaybe b = match b with F (_, v) -> Some v | Blank _ -> None

let new_ () = Blank (gid ())

let newF a = F (gid (), a)

let clone fn b =
  match b with
  | Blank id -> Blank (gid ())
  | F (id, val_) -> F (gid (), fn val_)

let isF = isBlank >> not

let isBlank b = match b with Blank _ -> true | F (_, _) -> false

let asF b = match b with F (_, v) -> Some v | Blank _ -> None

let replace search replacement bo =
  if toID bo = search then replacement else bo
