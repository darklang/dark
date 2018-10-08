module Util exposing (..)

-- builtin
import Regex

-- lib
import List.Extra as LE
import Maybe.Extra as ME

-- dark
import Native.Window
import Native.Random
import Native.Cache

windowSize : () -> (Int, Int)
windowSize a = let size = Native.Window.size a
               in (size.width, size.height - 45) -- TODO: fix this

random : () -> Int
random a = Native.Random.random a

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


