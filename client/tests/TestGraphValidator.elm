module TestGraphValidator exposing (..)

-- tests
import Test exposing (..)
import Expect exposing (Expectation, pass, fail)

-- dark
import GraphValidator exposing (NodeBounds)
import Types exposing (..)

buildBound : (Int, Int, Int, Int) -> NodeBounds
buildBound (top, bottom, left, right) = { id = ID 0, top = top, bottom = bottom, left = left, right = right }


checkOverlap : Test
checkOverlap =
    let t l r e =
            test (toString l ++ " <=> " ++ toString r)
                (\_ -> let builtL = buildBound l
                           builtR = buildBound r
                       in
                           if GraphValidator.checkOverlap (builtL, builtR) == e
                           then Expect.pass
                           else Expect.equal l r
                |> Expect.onFail (if e then "was False, expected True" else "was True, expected False"))
    in
        describe "checkOverlap"
        [ t (10, 20, 10, 20) (30, 40, 30, 40) False
        , t (10, 20, 10, 20) (10, 20, 10, 20) True
        , t (10, 20, 10, 20) (10, 20, 30, 40) False
        , t (30, 40, 10, 20) (10, 20, 10, 20) False
        , t (10, 20, 30, 40) (15, 25, 35, 45) True
        ]
