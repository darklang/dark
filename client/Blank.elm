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

replace : ID -> BlankOr a -> BlankOr a -> BlankOr a
replace search replacement bo =
  if toID bo == search
  then replacement
  else
    case bo of
      -- note that we can't replace msg here, because it's a BlankOr
      -- String, and this funcion can only replace BlankOr 'a
      Flagged thisId msg setting l r ->
        Flagged thisId msg setting
          (replace search replacement l)
          (replace search replacement r)
      _ -> bo

-- Basically a copy of replace because the elm type system doesn't
-- support this.
replaceFFMsg : ID -> BlankOr String -> BlankOr a -> BlankOr a
replaceFFMsg search replacement bo =
  case bo of
    Flagged thisId msg setting l r ->
      if toID msg == search
      then
        Flagged thisId replacement setting l r
      else
        Flagged thisId msg setting
          (replaceFFMsg search replacement l)
          (replaceFFMsg search replacement r)
    _ -> bo

-- checks if the ID is in the blank. Does not recurse past the Blank
-- definitions (eg, it will find a deeply nested Flagged, but won't find
-- a node within the AST)
within : BlankOr a -> ID -> Bool
within bo id =
  case bo of
    Flagged thisId msg setting l r ->
      id == thisId || within l id || within r id || within msg id
    Blank thisId -> id == thisId
    F thisId _ -> id == thisId
