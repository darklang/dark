module Random.Int exposing
    ( eight
    , five
    , four
    , nine
    , one
    , seven
    , six
    , three
    , two
    )

import Random
import Util


{-| All of the (Two - Nine) types contain that many random Ints
Note: Elm 0.19 only supports tuples that are 2-tuples,
so we use records
-}
type alias Two =
    { r1 : Int
    , r2 : Int
    }


type alias Three =
    { r1 : Int
    , r2 : Int
    , r3 : Int
    }


type alias Four =
    { r1 : Int
    , r2 : Int
    , r3 : Int
    , r4 : Int
    }


type alias Five =
    { r1 : Int
    , r2 : Int
    , r3 : Int
    , r4 : Int
    , r5 : Int
    }


type alias Six =
    { r1 : Int
    , r2 : Int
    , r3 : Int
    , r4 : Int
    , r5 : Int
    , r6 : Int
    }


type alias Seven =
    { r1 : Int
    , r2 : Int
    , r3 : Int
    , r4 : Int
    , r5 : Int
    , r6 : Int
    , r7 : Int
    }


type alias Eight =
    { r1 : Int
    , r2 : Int
    , r3 : Int
    , r4 : Int
    , r5 : Int
    , r6 : Int
    , r7 : Int
    , r8 : Int
    }


type alias Nine =
    { r1 : Int
    , r2 : Int
    , r3 : Int
    , r4 : Int
    , r5 : Int
    , r6 : Int
    , r7 : Int
    , r8 : Int
    , r9 : Int
    }


{-| All of the (one - nine) functions generate a type holding that many Ints
Note: Elm 0.19 only supports tuples that are 2-tuples
-}
one : Random.Seed -> ( Int, Random.Seed )
one seed =
    intoOne (\x -> x) seed


two : Random.Seed -> ( Two, Random.Seed )
two seed =
    intoTwo Two seed


three : Random.Seed -> ( Three, Random.Seed )
three seed =
    intoThree Three seed


four : Random.Seed -> ( Four, Random.Seed )
four seed =
    intoFour Four seed


five : Random.Seed -> ( Five, Random.Seed )
five seed =
    intoFive Five seed


six : Random.Seed -> ( Six, Random.Seed )
six seed =
    intoSix Six seed


seven : Random.Seed -> ( Seven, Random.Seed )
seven seed =
    intoSeven Seven seed


eight : Random.Seed -> ( Eight, Random.Seed )
eight seed =
    intoEight Eight seed


nine : Random.Seed -> ( Nine, Random.Seed )
nine seed =
    intoNine Nine seed


{-| All of the intoX (intoOne - intoNine) functions
just let you generate x random numbers.

For example:

    intoThree (,,) (Random.initialSeed 0)
        => ( ( 5, 24, 12 ), nextSeed )

etc...

-}
intoOne : (Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoOne f seed =
    Util.randomNumber seed
        |> Tuple.mapFirst f


intoTwo : (Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoTwo f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoOne nextF nextSeed


intoThree : (Int -> Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoThree f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoTwo nextF nextSeed


intoFour : (Int -> Int -> Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoFour f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoThree nextF nextSeed


intoFive : (Int -> Int -> Int -> Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoFive f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoFour nextF nextSeed


intoSix : (Int -> Int -> Int -> Int -> Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoSix f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoFive nextF nextSeed


intoSeven : (Int -> Int -> Int -> Int -> Int -> Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoSeven f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoSix nextF nextSeed


intoEight : (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoEight f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoSeven nextF nextSeed


intoNine : (Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> a) -> Random.Seed -> ( a, Random.Seed )
intoNine f seed =
    let
        ( nextF, nextSeed ) =
            Util.randomNumber seed
                |> Tuple.mapFirst f
    in
    intoEight nextF nextSeed
