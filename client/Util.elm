module Util exposing (..)

-- builtin
import Regex
import Char

-- lib
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import Native.Window
import Native.Random
import Native.Size
import Native.Cache

windowSize : () -> (Int, Int)
windowSize a = let size = Native.Window.size a
               in (size.width, size.height - 45) -- TODO: fix this

random : () -> Int
random a = Native.Random.random a

htmlSize : String -> (Float, Float)
htmlSize str = let size = Native.Size.size str
               in (size.width, size.height)

toIntWithDefault : Int -> String -> Int
toIntWithDefault d s =
  s
  |> String.toInt
  |> Result.withDefault d

reContains : String -> String -> Bool
reContains  re s = Regex.contains (Regex.regex re) s

reExactly : String -> String -> Bool
reExactly re s = reContains ("^" ++ re ++ "$") s

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

listContainsOrdered : List a -> List a -> Bool
listContainsOrdered needle haystack =
  case (needle, haystack) of
    ([], _) -> True
    (_, []) -> False
    (n :: ns, h :: hs) -> if n == h
                          then listContainsOrdered ns hs
                          else listContainsOrdered (n :: ns) hs

stringContainsOrdered : String -> String -> Bool
stringContainsOrdered needle haystack =
  case String.uncons needle of
    Just (c, newneedle) ->
      let char = String.fromChar c in
      String.contains char haystack
        && stringContainsOrdered newneedle (haystack
                                            |> String.split char
                                            |> List.drop 1
                                            |> String.join char)
    Nothing -> True



-- Given [1,2,3,4], this will return [(1,2), (1,3), (1,4), (2,3), (2,4), (3,4)]
uniqueCombinations : List a -> List (a, a)
uniqueCombinations xs = xs
                      |> LE.tails
                      |> zip xs
                      |> List.concatMap (\(x, ys) -> List.map (\y -> (x, y)) ys)
                      |> List.filter (\(x, y) -> x /= y)

listPrevious : a -> List a -> Maybe a
listPrevious a l =
  l
  |> LE.elemIndex a
  |> Maybe.map (\x -> x - 1)
  |> Maybe.andThen (\i -> LE.getAt i l)

listNext : a -> List a -> Maybe a
listNext a l =
  l
  |> LE.elemIndex a
  |> Maybe.map (\x -> x + 1)
  |> Maybe.andThen (\i -> LE.getAt i l)

listPreviousWrap : a -> List a -> Maybe a
listPreviousWrap a l =
  l
  |> listPrevious a
  |> ME.orElse (LE.last l)

listNextWrap : a -> List a -> Maybe a
listNextWrap a l =
  l
  |> listNext a
  |> ME.orElse (List.head l)

cacheSet : k -> v -> Maybe v
cacheSet k v =
  -- let _ = Debug.log "setting cache" k in
  Native.Cache.set k v

cacheGet : k -> v
cacheGet k =
  -- let _ = Debug.log "reading cache" k in
  Native.Cache.get k

cacheClear : k -> ()
cacheClear k =
  -- let _ = Debug.log "clearing cache" k in
  Native.Cache.clear k

-- The view we see is different from the value representation in a few
-- ways:
-- - the start and end quotes are skipped
-- - all other quotes are escaped
transformToStringEntry : String -> String
transformToStringEntry s_ =
  -- the first time we won't have a closing quote so add it
  let s = if String.endsWith "\"" s_ then s_ else s_ ++ "\"" in
  s
  |> String.dropLeft 1
  |> String.dropRight 1
  |> replace "\\\\\"" "\""

transformFromStringEntry : String -> String
transformFromStringEntry s =
  let s2 = s
           |> replace "\"" "\\\""
  in
  "\"" ++ s2 ++ "\""


