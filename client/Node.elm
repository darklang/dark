module Node exposing (..)

-- builtin
import Char
import List
import Set
import Maybe

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Defaults
import Util exposing (deMaybe, int2letter, letter2int)

gen_id : () -> ID
gen_id _ = ID (Util.random ())

isArg : Node -> Bool
isArg n = n.tipe == Arg

isNotArg : Node -> Bool
isNotArg = not << isArg

isBlock : Node -> Bool
isBlock n = n.tipe == Block

isNotBlock : Node -> Bool
isNotBlock = not << isBlock

isFunctionCall : Node -> Bool
isFunctionCall n = n.tipe == FunctionCall

isNotFunctionCall : Node -> Bool
isNotFunctionCall = not << isFunctionCall

nodeWidth : Node -> Int
nodeWidth n =
  let
    space = 3.5
    fours = Set.fromList ['i', 'l', '[', ',', ']', 'l', ':', '/', '.', ' ', ',', '{', '}']
    fives = Set.fromList ['I', 't', Char.fromCode 34 ] -- '"'
    len name = name
             |> String.toList
             |> List.map (\c -> if c == ' '
                                then 3.5
                                else if Set.member c fours
                                     then 4.0
                                     else if Set.member c fives
                                          then 5.0
                                          else 8.0)
             |> List.sum
    paramLen = n.arguments
               |> List.map (\(p, a) ->
                 if p.tipe == TBlock then -space -- remove spaces
                 else
                   case a of
                     Const c -> if c == "null" then 8 else (len c)
                     _ -> 14)
               |> List.sum
    -- nameMultiple = case n.tipe of
    --                  Datastore -> 2
    --                  Page -> 2.2
    --                  _ -> 1
    width = 6.0 + len n.name + paramLen + (n.arguments |> List.length |> toFloat |> (+) 1.0 |> (*) space)
  in
    round(width)

nodeHeight : Node -> Int
nodeHeight n =
  case n.tipe of
    Datastore -> Defaults.nodeHeight * ( 1 + (List.length n.fields))
    _ -> Defaults.nodeHeight

nodeSize : Node -> (Int, Int)
nodeSize node =
  (nodeWidth node, nodeHeight node)

getArgument : ParamName -> Node -> Argument
getArgument pname n =
  case LE.find (\(p, _) -> p.name == pname) n.arguments of
    Just (_, a) -> a
    Nothing ->
      Debug.crash <| "Looking for a name which doesn't exist: " ++ pname ++ toString n

isPrimitive : Node -> Bool
isPrimitive n =
  case n.liveValue.tipe of
    TInt        -> True
    TStr        -> False
    TChar       -> True
    TBool       -> True
    TFloat      -> True
    TObj        -> False
    TList       -> False
    TAny        -> False
    TBlock      -> False
    TOpaque     -> False
    TNull       -> False
    TIncomplete -> False

