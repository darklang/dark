module Entry exposing (..)

-- builtins
import Task
import Result exposing (Result)
import Regex
import Char

-- lib
import Dom
import List.Extra as LE
import Result.Extra as RE
import Maybe.Extra as ME
import Parser.Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore, oneOf, lazy, keep, repeat, end, oneOrMore, map , Count(..), inContext)
import Parser.Parser.Internal as PInternal exposing (Step(..))

-- dark
import Defaults
import Graph as G
import Types exposing (..)
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

--------------------------------
-- parsing framework
--------------------------------

parseFully : String -> Result String ParseTree
parseFully str =
  Parser.Parser.run full (Debug.log "starting to parse" str)
    |> Result.mapError toString
    |> Result.map (Debug.log "parsed")

token : String -> (String -> a) -> String -> Parser a
token name ctor re =  token_ (name++":/" ++ re ++ "/") ctor ("^" ++ re |> Regex.regex)

tokenCI : String -> (String -> a) -> String -> Parser a
tokenCI name ctor re =  token_ (name++":/" ++ re ++ "/") ctor ("^" ++ re |> Regex.regex |> Regex.caseInsensitive)

token_ : String -> (String -> a) -> Regex.Regex -> Parser a
token_ name ctor re =
  inContext name <|
  PInternal.Parser <| \({ source, offset, indent, context, row, col } as state) ->
    let substring = String.dropLeft offset source in
    case Regex.find (Regex.AtMost 1) re substring of
      [{match}] -> Good (ctor match) { state | offset = offset + String.length match 
                                             , col = col + String.length match}
      [] -> Bad (Parser.Parser.Fail <| "Regex not matched: " ++ name) state
      _ -> Debug.crash <| "Should never get more than 1 match for regex: " ++ name

debug : String -> Parser a -> Parser a
debug name =
  Parser.Parser.map (Debug.log name)
  
      
----------------------
-- the actual parser
----------------------

type ParseTree = PBlank
               | PExpr PExpr
               | PFieldname String

type PExpr = PFnCall String (List PExpr)
           | PValue String
           | PVar String

full : Parser ParseTree
full =
  inContext "full" <|
  succeed identity
    |= top
    |. end

top : Parser ParseTree
top =
  inContext "top" <|
  oneOf [fieldname, map PExpr fnCall, map PExpr expr, blank]

blank : Parser ParseTree
blank =
  succeed PBlank
    |. end

fieldname : Parser ParseTree
fieldname =
  inContext "fieldname" <|
  succeed PFieldname
    |. symbol "."
    |= fnName

parensExpr : Parser PExpr
parensExpr =
  inContext "parensExpr" <|
  succeed identity
    |. symbol "("
    |= lazy (\_ -> expr)
    |. symbol ")"
  

value : Parser PExpr
value =
  inContext "value" <|
  oneOf [string, number, char, list, obj, true, false, null]

expr : Parser PExpr
expr =
  inContext "expr" <|
  succeed identity
    |. whitespace
    |= oneOf [value, var, lazy (\_ -> fnCallWithParens), lazy (\_ -> parensExpr)]
    |. whitespace

whitespace : Parser String
whitespace = token "whitespace" identity "\\s*"

string : Parser PExpr
string = token "string" PValue "\"(?:[^\"\\\\]|\\\\.)*\""

char : Parser PExpr
char = token "char" PValue "'[a-z]'" 

number : Parser PExpr
number = token "number" PValue "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"

var : Parser PExpr
var =
  inContext "var" <|
  succeed PVar 
    |. symbol "$"
    |= keep (Exactly 1) Char.isLower

list : Parser PExpr
list = token "list" PValue "\\[.*\\]"

true : Parser PExpr
true = tokenCI "true" PValue  "true"

false : Parser PExpr
false = tokenCI "false" PValue "false"

null : Parser PExpr
null = tokenCI "null" PValue "null"

obj : Parser PExpr
obj = token "obj" PValue "{.*}"

fnCallWithParens : Parser PExpr
fnCallWithParens = 
  inContext "fnCallWithParens" <|
  succeed identity
    |. symbol "("
    |= lazy (\_ -> fnCall)
    |. symbol ")"

fnCall : Parser PExpr
fnCall = 
  inContext "fnCall" <|
  oneOf [lazy (\_ -> prefixFnCall), lazy (\_ -> infixFnCall)]



prefixFnCall : Parser PExpr
prefixFnCall =
  inContext "prefixFnCall" <|
  succeed PFnCall
    |= fnName
    |. whitespace
    -- this lazy shouldn't be necessary, but there's a run-time error if
    -- you don't
    |= repeat zeroOrMore (lazy (\_ -> fnArg))

infixFnCall : Parser PExpr
infixFnCall =
  inContext "infixFnCall" <|
  succeed (\arg name args -> PFnCall name (arg :: args))
    |= lazy (\_ -> fnArg)
    |. whitespace
    |= fnName
    |. whitespace
    -- this lazy shouldn't be necessary, but there's a run-time error if
    -- you don't
    |= repeat zeroOrMore (lazy (\_ -> fnArg))



fnName : Parser String
fnName = token "fnName" identity "[a-zA-Z:!@#%&\\*\\-_\\+\\|/\\?><=][a-zA-Z0-9:!@#%&\\*\\-_\\+\\|/\\?><=]+"

fnArg : Parser PExpr
fnArg =
  inContext "fnArg" <|
  succeed identity
    |. whitespace
    |= lazy (\_ -> expr)
    |. whitespace



----------------------
-- the AST
----------------------
type AST = ACreating Pos ACreating
         | AFillParam AFillParam
         | AFillResult AFillResult
         | AError String
         | ANothing

type ACreating = ACFnCall String (List AExpr)
               | ACValue String

type AExpr = AFnCall String (List AExpr)
           | AValue String
           | AVar Node

type AFillParam = APBlank (Node, Parameter)
                | APVar (Node, Parameter) Node 
                | APConst (Node, Parameter) String 
                | APFnCall (Node, Parameter) String (List AExpr) 

type AFillResult = ARVar Node Node
                 | ARNewValue Node String 
                 | ARFnCall Node String (List AExpr)
                 | ARFieldName Node String

convertArgs : Model -> List PExpr -> Result String (List AExpr)
convertArgs m args =
  (List.map (convertArg m) args) |> RE.combine


convertArg : Model -> PExpr -> Result String AExpr
convertArg m pexpr =
  case pexpr of
    PFnCall name args ->
      case convertArgs m args of
        Ok converted -> Ok <| AFnCall name converted
        Err msg -> Err msg
    PValue value -> Ok <| AValue value
    PVar letter -> 
      case G.fromLetter m letter of
        Just source ->
          Ok <| AVar source
        Nothing ->
          Err <| "letter doesnt exist: " ++ letter

pt2ast : Model -> EntryCursor -> ParseTree -> AST
pt2ast m cursor pt =
  case (cursor, pt) of

    -- Creating 
    (Creating _, PBlank) ->
      ANothing
    (Creating _, PFieldname _) ->
      AError <| "cant have a fieldname here"
    (Creating _, PExpr (PVar _)) -> 
      AError <| "cant have a var here"
    (Creating pos, PExpr (PValue value)) -> 
      ACreating pos <| ACValue value
    (Creating pos, PExpr (PFnCall name args)) -> 
      case convertArgs m args of
        Ok converted ->
          ACreating pos <| ACFnCall name converted
        Err msg ->
          AError msg

    -- Filling Params
    (Filling _ (ParamHole target param _), PBlank) ->
      if param.optional
      then AFillParam <| APConst (target, param) "null"
      else ANothing
    (Filling _ (ParamHole target param _), PFieldname _) ->
      AError <| "cant have a fieldname here"
    (Filling _ (ParamHole target param _), PExpr (PVar letter)) -> 
      case G.fromLetter m letter of
        Just source ->
          AFillParam <| APVar (target, param) source
        Nothing ->
          AError <| "letter doesnt exist: " ++ letter
    (Filling _ (ParamHole target param _), PExpr (PValue value )) ->
      AFillParam <| APConst (target, param) value
    (Filling _ (ParamHole target param _), PExpr (PFnCall name args)) ->
      case convertArgs m args of
        Ok converted ->
          AFillParam <| APFnCall (target, param) name converted
        Err msg ->
          AError msg

    -- Filling Result
    (Filling _ (ResultHole source), PBlank) ->
      ANothing
    (Filling _ (ResultHole source), PFieldname fieldname) ->
      AFillResult <| ARFieldName source fieldname
    (Filling _ (ResultHole source), PExpr (PVar letter)) -> 
      case G.fromLetter m letter of
        Nothing ->
          AError <| "letter doesnt exist: " ++ letter
        Just target ->
          AFillResult <| ARVar source target
    (Filling _ (ResultHole source), PExpr (PValue value )) ->
      AFillResult <| ARNewValue source value
    (Filling _ (ResultHole source), PExpr (PFnCall name args)) ->
      case convertArgs m args of
        Ok converted ->
          AFillResult <| ARFnCall source name converted
        Err msg ->
          AError msg


----------------------
-- execute the AST
----------------------
createArg : Model -> Function -> ID -> (AExpr, Parameter) -> Result String (List RPC)
createArg m fn id (arg, param) = 
  case arg of
   AVar n -> Ok [SetEdge n.id (id, param.name)]
   AValue v -> Ok [SetConstant v (id, param.name)]
   AFnCall name args -> 
     let fnid = G.gen_id ()
     in createFn m fnid name args Nothing Nothing
        |> Result.map (\rpcs -> rpcs ++ [SetEdge fnid (id, param.name)])

createFn : Model -> ID -> String -> List AExpr -> Maybe Pos -> Maybe Node -> Result String (List RPC)
createFn m id name args mpos implicit = 
  let fn = Autocomplete.findFunction m.complete name
  in case fn of
       Nothing -> Err "fn doesnt exist"
       Just fn ->
         let (fs, _) = addFunction m id name mpos
             -- get param for implicit
             impP = Maybe.andThen
               (\n -> Autocomplete.findParamByType fn n.liveValue.tipe)
               implicit
             impA = Maybe.map AVar implicit

             -- strip param used for implicit
             params = List.filter (\p -> (Just p) /= impP) fn.parameters

             -- combine
             extra = Maybe.map2 (,) impA impP |> ME.toList
             pairs = extra ++ (List.map2 (,) args params)

             argEdges = List.map (createArg m fn id) pairs
         in
          if List.length pairs < List.length extra + List.length args
          then Err <| "Too many argument and not enough parameters: " ++ (toString pairs) ++ (toString extra) ++ (toString args)
          else (Ok fs) :: argEdges
               |> RE.combine 
               |> Result.map List.concat 

execute : Model -> Bool -> AST -> Modification
execute m re ast =
  let id = G.gen_id () in
  case ast of
    ACreating pos (ACValue value) ->
      RPC <| ([AddValue id value (Just pos)]
             , FocusNext id)

    ACreating pos (ACFnCall name args) ->
      case createFn m id name args (Just pos) Nothing of
        Ok fns -> RPC (fns, FocusNext id)
        Err msg -> Error msg

    AFillParam (APBlank (target, param)) ->
      RPC ([SetConstant "null" (target.id, param.name)]
           , FocusNext target.id |> refocus re)

    AFillParam (APVar (target, param) source) ->
      RPC ([SetEdge source.id (target.id, param.name)]
          , FocusNext target.id |> refocus re)

    AFillParam (APConst (target, param) value) ->
      RPC ([SetConstant value (target.id, param.name)]
          , FocusNext target.id |> refocus re)

    AFillParam (APFnCall (target, param) name args) ->
      -- TODO: take over the positioning
      case createFn m id name args Nothing Nothing of
        Ok fns -> RPC (fns ++ [SetEdge id (target.id, param.name)]
                    , FocusNext target.id)
        Err msg -> Error msg

    AFillResult (ARFieldName source name) ->
      RPC ([ AddFunctionCall id "." Nothing
           , SetEdge source.id (id, "value")
           , SetConstant ("\"" ++ name ++ "\"") (id, "fieldname")]
          , FocusNext id)

    AFillResult (ARVar source target) ->
      -- TODO: use type
      case G.findParam target of
        Nothing -> Error "There are no argument slots available"
        Just (_, (param, _)) ->
          RPC ([ SetEdge source.id (target.id, param.name)]
              , FocusExact target.id)

    AFillResult (ARNewValue source value) ->
      RPC ([ AddFunctionCall id "_" Nothing
           , SetConstant value (id, "value")
           , SetEdge source.id (id, "ignore")]
          , FocusNext id)

    AFillResult (ARFnCall source name args) ->
      case createFn m id name args Nothing (Just source) of
        Ok fns -> RPC (fns, FocusNext id)
        Err msg -> Error msg

    AError msg ->
      Error msg

    ANothing ->
      NoChange
        

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  let pt = parseFully value
  in case pt of
    Ok pt -> execute m re <| pt2ast m cursor pt 
    Err error -> Error <| toString error
