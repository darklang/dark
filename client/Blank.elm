module Blank exposing (..)

-- dark
import Types exposing (..)

toID : BlankOr a -> ID
toID b =
  case b of
    Blank id -> id
    F id _ -> id

toMaybe : BlankOr a -> Maybe a
toMaybe b =
  case b of
    Blank _ -> Nothing
    F _ v -> Just v

new : () -> BlankOr a
new () =
  Blank (gid ())

newF : a -> BlankOr a
newF a =
  F (gid ()) a

map : (a -> b) -> BlankOr a -> BlankOr b
map fn a =
  case a of
    F id f -> F id (fn f)
    Blank id -> Blank id

clone : (a -> a) -> BlankOr a -> BlankOr a
clone fn b =
  case b of
    Blank id -> Blank (gid())
    F id val -> F (gid()) (fn val)

isF : BlankOr a -> Bool
isF b =
  case b of
    Blank _ -> False
    F _ _ -> True

isBlank : BlankOr a -> Bool
isBlank = isF >> not

toP : PointerType -> BlankOr a -> Pointer
toP t b =
  case b of
    Blank id -> PBlank t id
    F id _ -> PFilled t id

asF : BlankOr a -> Maybe a
asF a =
  case a of
    Blank _ -> Nothing
    F _ v -> Just v
