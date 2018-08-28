module Blank exposing
    ( asF
    , clone
    , isBlank
    , isF
    , new
    , newF
    , replace
    , toID
    , toMaybe
    )

-- dark

import Prelude exposing (..)
import Types exposing (..)


toID : BlankOr a -> ID
toID b =
    case b of
        Blank id ->
            id

        F id _ ->
            id


toMaybe : BlankOr a -> Maybe a
toMaybe b =
    case b of
        F _ v ->
            Just v

        Blank _ ->
            Nothing


new : () -> BlankOr a
new () =
    Blank (gid ())


newF : a -> BlankOr a
newF a =
    F (gid ()) a


clone : (a -> a) -> BlankOr a -> BlankOr a
clone fn b =
    case b of
        Blank id ->
            Blank (gid ())

        F id val ->
            F (gid ()) (fn val)


isF : BlankOr a -> Bool
isF =
    isBlank >> not


isBlank : BlankOr a -> Bool
isBlank b =
    case b of
        Blank _ ->
            True

        F _ _ ->
            False


asF : BlankOr a -> Maybe a
asF b =
    case b of
        F _ v ->
            Just v

        Blank _ ->
            Nothing


replace : ID -> BlankOr a -> BlankOr a -> BlankOr a
replace search replacement bo =
    if toID bo == search then
        replacement

    else
        bo
