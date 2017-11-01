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

isParentEdge : ID -> Argument -> Bool
isParentEdge id arg = getParentID arg == Just id

isBlockEdge : Argument -> Bool
isBlockEdge arg =
  case arg of
    Edge _ True -> True
    _ -> False

isFnEdge : Argument -> Bool
isFnEdge arg =
  case arg of
    Edge _ False -> True
    _ -> False

getParentID : Argument -> Maybe ID
getParentID arg =
  case arg of
    Edge id _ -> Just id
    _ -> Nothing

isFunctionCall : Node -> Bool
isFunctionCall n = n.tipe == FunctionCall

isNotFunctionCall : Node -> Bool
isNotFunctionCall = not << isFunctionCall

hasFace : Node -> Bool
hasFace n = String.length n.face > 0

nodeWidth : Node -> Int
nodeWidth n =
  let
    space = 4.5
    fours = Set.fromList ['i', 'l', '[', ',', ']', 'l', ':', '/', '.', ' ', ',', '{', '}']
    fives = Set.fromList ['I', 't', Char.fromCode 34 ] -- '"'
    len name = name
             |> String.toList
             |> List.map (\c -> if c == ' '
                                then 4.5
                                else if Set.member c fours
                                     then 4.5
                                     else if Set.member c fives
                                          then 7.0
                                          else 9.5)
             |> List.sum
    faceLen = len n.face
    paramLen =  if faceLen > 0
                then faceLen
                else
                  n.arguments
                          |> List.map (\(p, a) ->
                            if p.tipe == TBlock then -space -- remove spaces
                            else
                              case a of
                                Const c -> if c == "null" then 8 else (len c)
                                _ -> 16)
                          |> List.sum
    -- nameMultiple = case n.tipe of
    --                  Datastore -> 2
    --                  Page -> 2.2
    --                  _ -> 1
    width = 7.0 + len n.name + paramLen + (n.arguments |> List.length |> toFloat |> (+) 1.0 |> (*) space)
  in
    round(width)

nodeHeight : Node -> Int
nodeHeight n =
  Defaults.nodeHeight

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

generateFace : Node -> NodeList -> Node
generateFace ifn parents =
  if ifn.name /= "if"
  then Debug.crash "Tried to generate a face for a node that's not an if"
  else
    let face =
        case parents of
          [] -> Debug.crash "Tried to generate a face for an if w/ no parents"
          [a] -> nodeToFace a Nothing
          a :: b :: [] -> nodeToFace a (Just (nodeToFace b Nothing))
          _ -> Debug.crash "Tried to generate a face for an if w/ 2 many ps"
    in
        { ifn | face = face }

nodeToFace : Node -> Maybe String -> String
nodeToFace a b =
  let placeholder =
      case b of
        Just s -> "(" ++ s ++ ")"
        Nothing -> "$_"
      argLen = List.length a.arguments
      arguments = List.map (Tuple.second) a.arguments
      transformedArguments =
        List.map (\p ->
          case p of
            Const s -> s
            Edge _ _ -> placeholder
            NoArg   -> "") arguments
  in
      if argLen == 2
      then String.join (" " ++ a.name ++ " ") transformedArguments
      else a.name ++ " " ++ (String.join " " transformedArguments)

