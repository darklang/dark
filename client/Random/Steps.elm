module Random.Steps exposing
    ( eight
    , five
    , four
    , nine
    , one
    , seven
    , six
    , ten
    , three
    , two
    )

import Random


{-| All of the (Two - Ten) types contain that many values of the same type
-}
type alias Two a =
    { first : a
    , second : a
    }


type alias Three a =
    { first : a
    , second : a
    , third : a
    }


type alias Four a =
    { first : a
    , second : a
    , third : a
    , fourth : a
    }


type alias Five a =
    { first : a
    , second : a
    , third : a
    , fourth : a
    , fifth : a
    }


type alias Six a =
    { first : a
    , second : a
    , third : a
    , fourth : a
    , fifth : a
    , sixth : a
    }


type alias Seven a =
    { first : a
    , second : a
    , third : a
    , fourth : a
    , fifth : a
    , sixth : a
    , seventh : a
    }


type alias Eight a =
    { first : a
    , second : a
    , third : a
    , fourth : a
    , fifth : a
    , sixth : a
    , seventh : a
    , eighth : a
    }


type alias Nine a =
    { first : a
    , second : a
    , third : a
    , fourth : a
    , fifth : a
    , sixth : a
    , seventh : a
    , eighth : a
    , ninth : a
    }


type alias Ten a =
    { first : a
    , second : a
    , third : a
    , fourth : a
    , fifth : a
    , sixth : a
    , seventh : a
    , eighth : a
    , ninth : a
    , tenth : a
    }


{-| All of the (one - ten) functions generate a type holding that many values
Note: Elm 0.19 only supports tuples that are maximum 3-tuples
-}
one : Random.Generator a -> Random.Seed -> ( a, Random.Seed )
one gen seed =
    stepOne (\x -> x) gen seed


two : Random.Generator a -> Random.Seed -> ( Two a, Random.Seed )
two gen seed =
    stepTwo Two gen seed


three : Random.Generator a -> Random.Seed -> ( Three a, Random.Seed )
three gen seed =
    stepThree Three gen seed


four : Random.Generator a -> Random.Seed -> ( Four a, Random.Seed )
four gen seed =
    stepFour Four gen seed


five : Random.Generator a -> Random.Seed -> ( Five a, Random.Seed )
five gen seed =
    stepFive Five gen seed


six : Random.Generator a -> Random.Seed -> ( Six a, Random.Seed )
six gen seed =
    stepSix Six gen seed


seven : Random.Generator a -> Random.Seed -> ( Seven a, Random.Seed )
seven gen seed =
    stepSeven Seven gen seed


eight : Random.Generator a -> Random.Seed -> ( Eight a, Random.Seed )
eight gen seed =
    stepEight Eight gen seed


nine : Random.Generator a -> Random.Seed -> ( Nine a, Random.Seed )
nine gen seed =
    stepNine Nine gen seed


ten : Random.Generator a -> Random.Seed -> ( Ten a, Random.Seed )
ten gen seed =
    stepTen Ten gen seed


{-| All of the stepX (stepOne - stepTen) functions
just let you generate x random values.

For example:

    stepThree (,,) Util.randomInt (Random.initialSeed 0)
        => ( ( 5, 24, 12 ), nextSeed )

etc...

-}
stepOne : (a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepOne f gen seed =
    Random.step gen seed
        |> Tuple.mapFirst f


stepTwo : (a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepTwo f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepOne nextF gen nextSeed


stepThree : (a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepThree f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepTwo nextF gen nextSeed


stepFour : (a -> a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepFour f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepThree nextF gen nextSeed


stepFive : (a -> a -> a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepFive f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepFour nextF gen nextSeed


stepSix : (a -> a -> a -> a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepSix f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepFive nextF gen nextSeed


stepSeven : (a -> a -> a -> a -> a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepSeven f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepSix nextF gen nextSeed


stepEight : (a -> a -> a -> a -> a -> a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepEight f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepSeven nextF gen nextSeed


stepNine : (a -> a -> a -> a -> a -> a -> a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepNine f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepEight nextF gen nextSeed


stepTen : (a -> a -> a -> a -> a -> a -> a -> a -> a -> a -> b) -> Random.Generator a -> Random.Seed -> ( b, Random.Seed )
stepTen f gen seed =
    let
        ( nextF, nextSeed ) =
            Random.step gen seed
                |> Tuple.mapFirst f
    in
    stepNine nextF gen nextSeed
