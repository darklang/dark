module Blank exposing (..)

-- dark
import Types exposing (..)

toID : BlankOr a -> ID
toID b =
  case b of
    Blank id -> id
    F id _ -> id
    Flagged id _ _ _ _ -> id

toMaybe : BlankOr a -> Maybe a
toMaybe b =
  case b of
    F _ v -> Just v
    Flagged _ _ _ _ _ -> b |> flattenFF |> toMaybe
    Blank _ -> Nothing


new : () -> BlankOr a
new () =
  Blank (gid ())

newF : a -> BlankOr a
newF a =
  F (gid ()) a

clone : (a -> a) -> BlankOr a -> BlankOr a
clone fn b =
  case b of
    Blank id -> Blank (gid())
    F id val -> F (gid()) (fn val)
    Flagged id msg setting a b ->
      Flagged (gid()) msg setting (clone fn a) (clone fn b)

isF : BlankOr a -> Bool
isF = isBlank >> not

isBlank : BlankOr a -> Bool
isBlank b =
  case b of
    Blank _ -> True
    F _ _ -> False
    Flagged _ _ _ _ _ -> b |> flattenFF |> isBlank

asF : BlankOr a -> Maybe a
asF b =
  case b of
    F _ v -> Just v
    Blank _ -> Nothing
    Flagged _ _ _ _ _ -> b |> flattenFF |> asF

-- flatten the feature flag as appropriate
flattenFF : BlankOr a -> BlankOr a
flattenFF bo =
  case bo of
    Flagged _ _ setting a b ->
      if setting >= 50
      then b
      else a
    _ -> bo

