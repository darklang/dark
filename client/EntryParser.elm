module EntryParser exposing (..)

-- builtins
import Result exposing (Result)
import Regex
import Char

-- lib
import Result.Extra as RE
import Parser.Parser exposing (Parser, (|.), (|=), succeed, symbol, float, ignore, zeroOrMore, oneOf, lazy, keep, repeat, end, oneOrMore, map , Count(..), inContext, delayedCommit, Problem)
import Parser.Parser.Internal as PInternal exposing (Step(..))

-- dark
import Graph as G
import Types exposing (..)

--------------------------------
-- parsing framework
--------------------------------

parseFully : String -> Result ParseError ParseTree
parseFully str =
  Parser.Parser.run full str
    |> Result.mapError fromParserError

token : String -> (String -> a) -> String -> Parser a
token name ctor re =  token_ (name++": /" ++ re ++ "/") ctor ("^" ++ re |> Regex.regex)

tokenCI : String -> (String -> a) -> String -> Parser a
tokenCI name ctor re =  token_ (name++": /" ++ re ++ "/") ctor ("^" ++ re |> Regex.regex |> Regex.caseInsensitive)

token_ : String -> (String -> a) -> Regex.Regex -> Parser a
token_ name ctor re =
  inContext name <|
  PInternal.Parser <| \({ source, offset, indent, context, row, col } as state) ->
    let substring = String.dropLeft offset source in
    case Regex.find (Regex.AtMost 1) re substring of
      [{match}] -> Good (ctor match) { state | offset = offset + String.length match
                                             , col = col + String.length match}
      [] -> Bad (Parser.Parser.Fail <| "Regex not matched: " ++ name ++ (" on \"" ++ state.source ++ "\"")) state
      _ -> Debug.crash <| "Should never get more than 1 match for regex: " ++ name

debug : String -> Parser a -> Parser a
debug name =
  Parser.Parser.map (Debug.log name)

----------------------
-- the actual parser
----------------------

type alias ParseError = { source : String
                        , problem : Problem
                        , cursor  : Maybe EntryCursor
                        }

fromParserError : Parser.Parser.Error -> ParseError
fromParserError p = { source = p.source, problem = p.problem, cursor = Nothing }

addCursorToError : ParseError -> EntryCursor -> ParseError
addCursorToError pe c = { pe | cursor = Just c }


maxErrorSize : Int
maxErrorSize = 70

toErrorMessage : ParseError -> String
toErrorMessage pe = pe
                  |> toFullErrorMessage
                  |> (\x -> if (String.length x) > maxErrorSize
                            then (String.slice 0 (maxErrorSize - 3) x) ++ "..."
                            else x
                     )

toFullErrorMessage : ParseError -> String
toFullErrorMessage pe = case pe.cursor of
                      Just c  -> case c of
                                     Creating _ -> noContextErrorMessage pe
                                     Filling n h -> case h of
                                                        ResultHole _ -> noContextErrorMessage pe
                                                        ParamHole n2 p i -> (noContextErrorMessage pe) ++ " in argument #" ++ (toString i) ++ " for an `" ++ (n2.name) ++ "`"
                      Nothing -> noContextErrorMessage pe

noContextErrorMessage : ParseError -> String
noContextErrorMessage pe = "Error parsing expression: `" ++ pe.source ++ "`: " ++ (ppProblem pe.problem)

ppProblem : Problem -> String
ppProblem p = case p of
                  Parser.Parser.BadOneOf l -> case List.head l of
                                                  Just h -> toString h
                                                  Nothing -> "Unknown"
                  _ -> toString p


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
  oneOf [ fieldname
        , blank
       -- Expressions combined with infixFnCalls are left-recursive. To
       -- prevent infinite recursion, we specify a recursion limit.
       -- Because we are really only allowing a short string here, 3 or
       -- 4 should be plenty. Increasing this makes parsing incredibly
       -- slow, as each expression requires n! recurses.
       , map PExpr (expr 8)]

blank : Parser ParseTree
blank =
  succeed PBlank
    |. end

fieldname : Parser ParseTree
fieldname =
  inContext "fieldname" <|
  delayedCommit whitespace <|
  succeed PFieldname
    |. symbol "."
    |= longFnName
    |. end

parensExpr : Int -> Parser PExpr
parensExpr depth =
  inContext "parensExpr" <|
  succeed identity
    |. symbol "("
    |= lazy (\_ -> expr depth)
    |. symbol ")"

value : Parser PExpr
value =
  inContext "value" <|
  delayedCommit whitespace <|
  succeed identity
    |= oneOf [string, number, char, list, obj, true, false, null]
    |. whitespace


expr : Int -> Parser PExpr
expr depth =

 if depth == 1
  then oneOf [value, var]
  else
    let d = depth-1 in
    inContext "expr" <|
    delayedCommit whitespace <|
    succeed identity
      |= oneOf [lazy (\_ -> fnCall d), lazy (\_ -> parensExpr d), value, var]
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
    |= keep (Exactly 1) (\x -> (Char.isLower x) || (x == '_'))

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

fnCall : Int -> Parser PExpr
fnCall depth =
  inContext "fnCall" <|
  delayedCommit whitespace <|
  succeed identity
    -- because a prefixFnCall can match false/null/-6 that can be valid first args of infix, do infix first
    |= oneOf [lazy (\_ -> infixFnCall depth), lazy (\_ -> prefixFnCall depth)]
    |. whitespace

prefixFnCall : Int -> Parser PExpr
prefixFnCall depth =
  inContext "prefixFnCall" <|
  succeed PFnCall
    |= fnName
    |. whitespace
    -- this lazy shouldn't be necessary, but there's a run-time error if
    -- you don't
    |= repeat zeroOrMore (lazy (\_ -> fnArg depth))
    |. whitespace

infixFnCall : Int -> Parser PExpr
infixFnCall depth =
  inContext "infixFnCall" <|
  succeed (\arg name args -> PFnCall name (arg :: args))
    |= lazy (\_ -> fnArg depth)
    |. whitespace
    |= fnName
    |. whitespace
    -- this lazy shouldn't be necessary, but there's a run-time error if
    -- you don't
    |= repeat zeroOrMore (lazy (\_ -> fnArg depth))
    |. whitespace

fnName : Parser String
fnName = oneOf [ twoLetterFnName, oneLetterFnName, longFnName ]

oneLetterFnName : Parser String
oneLetterFnName = token "oneLetterFnName" identity "[%&\\*\\-_\\+><=]"

twoLetterFnName : Parser String
twoLetterFnName = token "twoLetterFnName" identity "=="

longFnName : Parser String
longFnName = token "longFnName" identity "[a-zA-Z][0-9a-zA-Z:!@#%&\\*\\-_\\+\\|/\\?><=]*"

fnArg : Int -> Parser PExpr
fnArg depth =
  inContext "fnArg" <|
  delayedCommit whitespace <|
  succeed identity
    |= lazy (\_ -> expr depth)
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

type ARef = APlaceholder | ANode Node

type AExpr = AFnCall String (List AExpr)
           | AValue String
           | AVar ARef

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
      if letter == G.implicitPlaceholderLetter
      then
          Ok <| AVar APlaceholder
      else
        case G.fromLetter m letter of
          Just source ->
            Ok <| AVar (ANode source)
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
