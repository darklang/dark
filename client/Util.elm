module Util exposing (..)

-- builtin
import Regex
import Char

-- lib
import List.Extra as LE

-- dark
import Native.Window
import Native.Timestamp
import Native.Random
import Native.Size

timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a

windowSize : () -> (Int, Int)
windowSize a = let size = Native.Window.size a
               in (size.width, size.height - 45) -- TODO: fix this

random : () -> Int
random a = Native.Random.random a

htmlSize : String -> (Float, Float)
htmlSize str = let size = Native.Size.size str
               in (size.width, size.height)

deMaybe : Maybe a -> a
deMaybe x = case x of
              Nothing -> Debug.crash "not possible"
              Just y -> y

rematch : String -> String -> Bool
rematch re s = Regex.contains (Regex.regex re) s

replace : String -> String -> String -> String
replace re repl str = Regex.replace Regex.All (Regex.regex re) (\_ -> repl) str

findIndex : (a -> Bool) -> List a -> Maybe (Int, a)
findIndex fn l =
  LE.find (\(_, a) -> fn a) (List.indexedMap (,) l)


zip : List a -> List b -> List (a, b)
zip a b = List.map2 (,) a b

resultIsOk : Result a b -> Bool
resultIsOk r =
  case r of
    Ok _ -> True
    Err _ -> False

int2letter : Int -> String
int2letter i = 'a' |> Char.toCode |> (+) i |> Char.fromCode |> String.fromChar

letter2int : String -> Int
letter2int s = s |> String.uncons |> Maybe.withDefault ('!', "") |> Tuple.first |> Char.toCode |> (-) (Char.toCode 'a') |> (*) (-1)

logF : String -> (a -> b) -> a -> a
logF msg fn obj =
  let _ = Debug.log msg (fn obj) in obj

-- like Result.Extra.combine but also combines errors
combineResult : List (Result x a) -> Result (List x) (List a)
combineResult results =
    List.foldl
      (\r rs ->
        case (r, rs) of
          (Ok a, Ok bs) -> Ok (a :: bs)
          (Err a, Ok _) -> Err [a]
          (Ok _, Err bs) -> Err bs
          (Err a, Err bs) -> Err (a :: bs)
      )
      (Ok [])
      results
