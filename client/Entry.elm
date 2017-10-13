module Entry exposing (..)

-- builtins
import Task
import Result exposing (Result)
import Regex

-- lib
import Dom
import List.Extra as LE
import Parser.Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore, oneOf, lazy, keep, repeat, end, oneOrMore )
import Parser.Parser.Internal as PInternal exposing (Step(..))

-- dark
import Defaults
import Graph as G
import Types exposing (..)
import Util
import Runtime as RT
import Autocomplete
import Viewport


nodeFromHole : Hole -> Node
nodeFromHole h = case h of
                   ResultHole n -> n
                   ParamHole n _ _ -> n

holeCreatePos : Model -> Hole -> Pos
holeCreatePos m hole =
  case hole of
    ParamHole n _ i -> {x=n.pos.x-50+(i*50), y=n.pos.y-40}
    ResultHole n ->
      let connected = G.entireSubgraph m n
          lowest = connected
                   |> List.map (\n -> n.pos.y)
                   |> List.maximum
                   |> Maybe.withDefault n.pos.y
      in
      {x=n.pos.x, y=lowest+40}


entryNodePos : EntryCursor -> Pos
entryNodePos c =
  case c of
    Creating p -> p -- todo this is a vpos
    Filling n _ -> n.pos


---------------------
-- Nodes
---------------------
-- Reenter an existing node to edit the existing inputs
reenter : Model -> ID -> Int -> Modification
reenter m id i =
  -- TODO: Allow the input to be edited
  let n = G.getNodeExn m id
      args = G.args n
  in
    case LE.getAt i args of
      Nothing -> NoChange
      Just (p, a) ->
        let enter = Enter True <| Filling n (ParamHole n p i) in
        case a of
          Edge eid -> Many [ enter
                          , AutocompleteMod (ACQuery <| "$" ++ G.toLetter m eid)]
          NoArg -> enter
          Const c -> Many [ enter
                          , AutocompleteMod (ACQuery c)]

-- Enter this exact node
enterExact : Model -> Node -> Modification
enterExact _ selected =
  Filling selected (G.findHole selected)
  |> cursor2mod

-- Enter the next needed node, searching from here
enterNext : Model -> Node -> Modification
enterNext m n =
  cursor2mod <|
    case G.findNextHole m n of
      Nothing -> Filling n (ResultHole n)
      Just hole -> Filling (nodeFromHole hole) hole

cursor2mod : EntryCursor -> Modification
cursor2mod cursor =
  Many [ Enter False cursor
       , case cursor of
           Filling n (ResultHole _) ->
             AutocompleteMod <| ACFilterByLiveValue n.liveValue
           Filling _ (ParamHole _ p _) ->
             Many [ AutocompleteMod <| ACFilterByParamType p.tipe
                  , AutocompleteMod <| ACOpen False ]
           Creating _ ->
             NoChange
       ]



updateValue : String -> Modification
updateValue target =
  Many [ AutocompleteMod <| ACQuery target, Phantom ]

createFindSpace : Model -> Modification
createFindSpace m = Enter False <| Creating (Viewport.toAbsolute m Defaults.initialPos)
---------------------
-- Focus
---------------------

focusEntry : Cmd Msg
focusEntry = Dom.focus Defaults.entryID |> Task.attempt FocusEntry


---------------------
-- Submitting the entry form to the server
---------------------

isValueRepr : String -> Bool
isValueRepr name = String.toLower name == "null"
                   || String.toLower name == "true"
                   || String.toLower name == "false"
                   || Util.rematch "^[\"\'[01-9{].*" name
                   || String.startsWith "-" name && Util.rematch "-[0-9.].+" name

addAnonParam : Model -> ID -> MPos -> ParamName -> List String -> (List RPC, Focus)
addAnonParam _ id pos name anon_args =
  let sid = G.gen_id ()
      argids = List.map (\_ -> G.gen_id ()) anon_args
      anon = AddAnon sid pos argids anon_args
      edge = SetEdge sid (id, name)
  in
    ([anon, edge], FocusNext id)


addFunction : Model -> ID -> Name -> MPos -> (List RPC, Focus)
addFunction m id name pos =
  let fn = Autocomplete.findFunction m.complete name in
  case fn of
    -- not a real function, but hard to thread an error here, so let the
    -- server fail instead
    Nothing ->
      ([AddFunctionCall id name pos], FocusSame)
    Just fn ->
      -- automatically add anonymous functions
      let fn_args = List.filter (\p -> p.tipe == TFun) fn.parameters
          anonpairs = List.map (\p -> addAnonParam m id pos p.name p.anon_args) fn_args
          anonarg = anonpairs |> List.head |> Maybe.map Tuple.second
          anons = anonpairs |> List.unzip |> Tuple.first
          focus = case anonarg of
            Just f -> f
            Nothing -> FocusNext id
      in
        (AddFunctionCall id name pos :: List.concat anons, focus)

updatePreviewCursor : Model -> ID -> Int -> List RPC
updatePreviewCursor m id step =
  let baseNode = G.getNodeExn m id in
  let anonFuncNode = Debug.log "parent anon" (G.findParentAnon m baseNode) in
  case anonFuncNode of
    Just n -> [UpdateNodeCursor n.id (n.cursor + step)]
    Nothing -> []

refocus : Bool -> Focus -> Focus
refocus re default =
  case default of
    FocusNext id -> if re then FocusExact id else FocusNext id
    f -> f

type alias EAST = EExpr
type EExpr = EExpr String (List EArg)
type EArg = EValue String
          | ENode String
          | EKnownTipe Tipe
          | EAnother EExpr


type alias SyntaxTree = STExpr

type STValue = STValue String

type STExpr = STEFnCall STFnCall
            | STEValue STValue
            | STEVar STVar

type STVar = STVar String

--type FnCall = FnCall (STExpr) String (List STExpr)
type STFnCall = STFnCall String (List STExpr)

--------------------------------
-- parsing framework
--------------------------------

parseFully : String -> Result String SyntaxTree
parseFully str =
  Parser.Parser.run full str |> Result.mapError toString

token : String -> (String -> a) -> String -> Parser a
token name ctor re =  token_ name ctor ("^" ++ re |> Regex.regex)

tokenCI : String -> (String -> a) -> String -> Parser a
tokenCI name ctor re =  token_ name ctor ("^" ++ re |> Regex.regex |> Regex.caseInsensitive)

token_ : String -> (String -> a) -> Regex.Regex -> Parser a
token_ name ctor re =
  PInternal.Parser <| \({ source, offset, indent, context, row, col } as state) ->
    let substring = String.dropLeft offset source in
    case Regex.find (Regex.AtMost 1) re substring of
      [{match}] -> Good (ctor match) { state | offset = offset + String.length match 
                                             , col = col + String.length match}
      [] -> Bad (Parser.Parser.Fail <| "Regex " ++ name ++ " not matched: /" ++ toString re ++ "/") state
      _ -> Debug.crash <| "Should never get more than 1 match for regex: " ++ name

debug : String -> Parser a -> Parser a
debug name =
  Parser.Parser.map (Debug.log name)
  
      
----------------------
-- the actual parser
----------------------

full : Parser STExpr
full =
  succeed identity
    |= expr
    |. end

value : Parser STExpr
value = oneOf [string, number, char, list, obj, true, false, null]

expr : Parser STExpr
expr =
  succeed identity
    |. whitespace
    |= oneOf [value, var, fnCall]
    |. whitespace

whitespace : Parser String
whitespace = token "whitespace" identity "\\s*"

string : Parser STExpr
string = token "string" (STValue >> STEValue) "\"(?:[^\"\\\\]|\\\\.)*\""

char : Parser STExpr
char = token "char" (STValue >> STEValue) "'[a-z]'" 

number : Parser STExpr
number = token "number" (STValue >> STEValue) "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"

var : Parser STExpr
var = token "var" (STVar >> STEVar) "\\$[a-z]" 

list : Parser STExpr
list = token "list" (STValue >> STEValue) "\\[.*\\]"

true : Parser STExpr
true = tokenCI "true" (STValue >> STEValue) "true"

false : Parser STExpr
false = tokenCI "false" (STValue >> STEValue) "false"

null : Parser STExpr
null = tokenCI "null" (STValue >> STEValue) "null"

obj : Parser STExpr
obj = token "obj" (STValue >> STEValue) "{.*}"

fnCall : Parser STExpr
fnCall = 
  succeed STEFnCall
    |= (succeed STFnCall
        |. whitespace
        |= fnName
        |. whitespace
        |= repeat zeroOrMore fnArg
        )

fnName : Parser String
fnName = token "fnName" identity "[a-zA-Z:!@#%&\\*\\-_\\+\\|/\\?><]+"

fnArg : Parser STExpr
fnArg =
  succeed identity
    |. whitespace
    |= oneOf [value, var]
    |. whitespace
  
 
submit2 : Model -> Bool -> EntryCursor -> String -> Modification
submit2 m re cursor value =
  let ast = parseFully value
  in case ast of
    Ok east -> Error <| toString east
    Err error -> Error <| toString error 


submit : Model -> Bool -> EntryCursor -> String -> Modification
submit = submit2

submit1 : Model -> Bool -> EntryCursor -> String -> Modification
submit1 m re cursor value =
  let id = G.gen_id () in
  case cursor of
    Creating pos ->
      RPC <| if isValueRepr value
             then ([AddValue id value (Just pos)], FocusNext id)
             else addFunction m id value (Just pos)

    Filling n (ParamHole target param _ as hole) ->
      case String.uncons value of
        Nothing ->
          if param.optional
          then RPC ([SetConstant "null" (target.id, param.name)]
                  , FocusNext target.id |> refocus re)
          else NoChange

        Just ('$', letter) ->
          case G.fromLetter m letter of
            Just source ->
              RPC ([ SetEdge source.id (target.id, param.name)]
                   , FocusNext target.id |> refocus re)
            Nothing -> Error <| "No node named '" ++ letter ++ "'"

        _ ->
          if isValueRepr value
          then RPC ([ SetConstant value (target.id, param.name)]
                    , FocusNext target.id |> refocus re)
          else
            let (name, arg, extras) = case String.split " " value of
                                (name :: arg :: es) -> (name, Just arg, es)
                                [name] -> (name, Nothing, [])
                                [] -> ("", Nothing, [])
                fn = Autocomplete.findFunction m.complete name
                argEdges = case (fn, arg) of
                  (Just fn, Just arg) ->
                    if isValueRepr arg
                    then 
                      let tipedP = Autocomplete.findParamByType fn (RT.tipeOf arg) in
                      case tipedP of
                        Just tipedP -> Ok <| [SetConstant arg (id, tipedP.name)]
                        Nothing -> Err <| "No parameter for argument: " ++ arg
                    else
                      case String.uncons arg of
                        Just ('$', letter) ->
                          case G.fromLetter m letter of
                            Nothing -> Err <| "No node named '" ++ letter ++ "'"
                            Just lNode ->
                              let tipedP = Autocomplete.findParamByType fn (lNode.liveValue.tipe) in
                              case tipedP of
                                Nothing -> Err <| "No parameter for argument with the right type: " ++ arg
                                Just tipedP -> Ok <| [SetEdge lNode.id (id, tipedP.name)]
                        Just _ ->
                          Err <| "We don't currently support arguments like `" ++ arg ++ "`"
                        Nothing -> Ok [] -- empty string
                  _ -> Ok []
            in
            if extras /= []
            then Error <| "Too many arguments: `" ++ String.join " " extras ++ "`"
            else
              let (f, focus) = addFunction m id name Nothing
                  edges = [SetEdge id (target.id, param.name)]
              in
              case argEdges of
                Ok argEdges -> RPC (f ++ edges ++ argEdges, focus)
                Err err -> Error err


    Filling n (ResultHole source as hole) ->
      case String.uncons value of
        Nothing -> NoChange

        -- TODO: this should be an opcode
        Just ('.', fieldname) ->
          RPC ([ AddFunctionCall id "." Nothing
               , SetEdge source.id (id, "value")
               , SetConstant ("\"" ++ fieldname ++ "\"") (id, "fieldname")]
              , FocusSame)

        Just ('$', letter) ->
          case G.fromLetter m letter of
            Nothing ->
              Error <| "No node named '" ++ letter ++ "'"
            Just target ->
              -- TODO: use type
              case G.findParam target of
                Nothing -> Error "There are no argument slots available"
                Just (_, (param, _)) ->
                  RPC ([ SetEdge source.id (target.id, param.name)]
                       , FocusExact target.id)

        _ ->
          -- this is new functions only
          -- lets allow 1 thing, integer only, for now
          -- so we find the first parameter that isnt that parameter

          let (name, arg, extras) = case String.split " " value of
                              (name :: arg :: es) -> (name, Just arg, es)
                              [name] -> (name, Nothing, [])
                              [] -> ("", Nothing, [])
              (f, focus) = if isValueRepr name
                           then ([ AddFunctionCall id "_" Nothing
                                 , SetConstant name (id, "value")
                                 , SetEdge source.id (id, "ignore")
                                 ]
                                , FocusNext id)
                           else addFunction m id name Nothing
          in
          case Autocomplete.findFunction m.complete name of
            Nothing ->
              -- Unexpected, let the server reply with an error
              RPC (f, focus)
            Just fn ->
              -- tipedP: parameter to connect the previous node to
              -- arg: the 2nd word in the autocmplete box
              -- otherP: first argument that isn't tipedP
              let tipedP = Autocomplete.findParamByType fn n.liveValue.tipe
                  otherP = Autocomplete.findFirstParam fn tipedP

                  tipedEdges = case tipedP of
                    Nothing -> []
                    Just p -> [SetEdge source.id (id, p.name)]

                  argEdges = case (arg, otherP) of
                    (Nothing, _) -> Ok []
                    (Just arg, Nothing) ->
                        Err <| "No parameter exists for arg: " ++ arg
                    (Just arg, Just p) ->
                      if isValueRepr arg
                      then Ok <| [SetConstant arg (id, p.name)]
                      else
                        case String.uncons arg of
                          Just ('$', letter) ->
                            case G.fromLetter m letter of
                              Just lNode ->
                                Ok <| [SetEdge lNode.id (id, p.name)]
                              Nothing -> Err <| "No node named '"
                                             ++ letter ++ "'"
                          Just _ ->
                            Err <| "We don't currently support arguments like `" ++ arg ++ "`"
                          Nothing -> Ok [] -- empty string
              in
              if extras /= []
              then Error <| "Too many arguments: `" ++ String.join " " extras ++ "`"
              else
                case argEdges of
                  Ok edges -> RPC (f ++ tipedEdges ++ edges, focus)
                  Err err -> Error err


