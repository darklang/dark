module Blank exposing (..)

-- dark
import Types exposing (..)

toID : BlankOr a -> ID
toID b =
  case b of
    Blank id -> id
    Filled id _ -> id

toMaybe : BlankOr a -> Maybe a
toMaybe b =
  case b of
    Blank _ -> Nothing
    Filled _ v -> Just v

new : () -> BlankOr a
new () =
  Blank (gid ())

newFilled : a -> BlankOr a
newFilled a =
  Filled (gid ()) a

map : (a -> b) -> BlankOr a -> BlankOr b
map fn a =
  case a of
    Filled id f -> Filled id (fn f)
    Blank id -> Blank id

clone : (a -> a) -> BlankOr a -> BlankOr a
clone fn b =
  case b of
    Blank id -> Blank (gid())
    Filled id val -> Filled (gid()) (fn val)

isFilled : BlankOr a -> Bool
isFilled b =
  case b of
    Blank _ -> False
    Filled _ _ -> True

isBlank : BlankOr a -> Bool
isBlank = isFilled >> not

toP : PointerType -> BlankOr a -> Pointer
toP t b =
  case b of
    Blank id -> PBlank t id
    Filled id _ -> PFilled t id



