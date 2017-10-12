module Parser.LanguageKit exposing
  ( variable
  , list, record, tuple, sequence, Trailing(..)
  , whitespace, LineComment(..), MultiComment(..)
  )


{-|

# Variables
@docs variable

# Lists, records, and that sort of thing
@docs list, record, tuple, sequence, Trailing

# Whitespace
@docs whitespace, LineComment, MultiComment

-}


import Set exposing (Set)
import Parser exposing (..)
import Parser.Internal as I exposing (Step(..), State)
import ParserPrimitives as Prim



-- VARIABLES


{-| Create a parser for variables. It takes two `Char` checkers. The
first one is for the first character. The second one is for all the
other characters.

In Elm, we distinguish between upper and lower case variables, so we
can do something like this:

    import Char
    import Parser exposing (..)
    import Parser.LanguageKit exposing (variable)
    import Set

    lowVar : Parser String
    lowVar =
      variable Char.isLower isVarChar keywords

    capVar : Parser String
    capVar =
      variable Char.isUpper isVarChar keywords

    isVarChar : Char -> Bool
    isVarChar char =
      Char.isLower char
      || Char.isUpper char
      || Char.isDigit char
      || char == '_'

    keywords : Set.Set String
    keywords =
      Set.fromList [ "let", "in", "case", "of" ]
-}
variable : (Char -> Bool) -> (Char -> Bool) -> Set String -> Parser String
variable isFirst isOther keywords =
  I.Parser <| \({ source, offset, indent, context, row, col } as state1) ->
    let
      firstOffset =
        Prim.isSubChar isFirst offset source
    in
      if firstOffset == -1 then
        Bad ExpectingVariable state1

      else
        let
          state2 =
            if firstOffset == -2 then
              varHelp isOther (offset + 1) (row + 1) 1 source indent context
            else
              varHelp isOther firstOffset row (col + 1) source indent context

          name =
            String.slice offset state2.offset source
        in
          if Set.member name keywords then
            Bad ExpectingVariable state1

          else
            Good name state2


varHelp : (Char -> Bool) -> Int -> Int -> Int -> String -> Int -> List ctx -> State ctx
varHelp isGood offset row col source indent context =
  let
    newOffset =
      Prim.isSubChar isGood offset source
  in
    if newOffset == -1 then
      { source = source
      , offset = offset
      , indent = indent
      , context = context
      , row = row
      , col = col
      }

    else if newOffset == -2 then
      varHelp isGood (offset + 1) (row + 1) 1 source indent context

    else
      varHelp isGood newOffset row (col + 1) source indent context



-- SEQUENCES


{-| Parse a comma-separated list like `[ 1, 2, 3 ]`. You provide
a parser for the spaces and for the list items. So if you want
to parse a list of integers, you would say:

    import Parser exposing (Parser)
    import Parser.LanguageKit as Parser

    intList : Parser (List Int)
    intList =
      Parser.list spaces Parser.int

    spaces : Parser ()
    spaces =
      Parser.ignore zeroOrMore (\char -> char == ' ')

    -- run intList "[]"            == Ok []
    -- run intList "[ ]"           == Ok []
    -- run intList "[1,2,3]"       == Ok [1,2,3]
    -- run intList "[ 1, 2, 3 ]"   == Ok [1,2,3]
    -- run intList "[ 1 , 2 , 3 ]" == Ok [1,2,3]
    -- run intList "[ 1, 2, 3, ]"  == Err ...
    -- run intList "[, 1, 2, 3 ]"  == Err ...

**Note:** If you want trailing commas, check out the
[`sequence`](#sequence) function.
-}
list : Parser () -> Parser a -> Parser (List a)
list spaces item =
  sequence
    { start = "["
    , separator = ","
    , end = "]"
    , spaces = spaces
    , item = item
    , trailing = Forbidden
    }


{-| Help parse records like `{ a = 2, b = 2 }`. You provide
a parser for the spaces and for the list items, you might say:

    import Parser exposing ( Parser, (|.), (|=), zeroOrMore )
    import Parser.LanguageKit as Parser

    record : Parser (List (String, Int))
    record =
      Parser.record spaces field

    field : Parser (String, Int)
    field =
      Parser.succeed (,)
        |= lowVar
        |. spaces
        |. Parser.symbol "="
        |. spaces
        |= int

    spaces : Parser ()
    spaces =
      Parser.ignore zeroOrMore (\char -> char == ' ')

    -- run record "{}"               == Ok []
    -- run record "{ }"              == Ok []
    -- run record "{ x = 3 }"        == Ok [ ("x",3) ]
    -- run record "{ x = 3, }"       == Err ...
    -- run record "{ x = 3, y = 4 }" == Ok [ ("x",3), ("y",4) ]
    -- run record "{ x = 3, y = }"   == Err ...

**Note:** If you want trailing commas, check out the
[`sequence`](#sequence) function.
-}
record : Parser () -> Parser a -> Parser (List a)
record spaces item =
  sequence
    { start = "{"
    , separator = ","
    , end = "}"
    , spaces = spaces
    , item = item
    , trailing = Forbidden
    }


{-| Help parse tuples like `(3, 4)`. Works just like [`list`](#list)
and [`record`](#record). And if you need something custom, check out
the [`sequence`](#sequence) function.
-}
tuple : Parser () -> Parser a -> Parser (List a)
tuple spaces item =
  sequence
    { start = "("
    , separator = ","
    , end = ")"
    , spaces = spaces
    , item = item
    , trailing = Forbidden
    }


{-| Handle things *like* lists and records, but you can customize the
details however you need. Say you want to parse C-style code blocks:

    import Parser exposing (Parser)
    import Parser.LanguageKit as Parser exposing (Trailing(..))

    block : Parser (List Stmt)
    block =
      Parser.sequence
        { start = "{"
        , separator = ";"
        , end = "}"
        , spaces = spaces
        , item = statement
        , trailing = Mandatory -- demand a trailing semi-colon
        }

    -- spaces : Parser ()
    -- statement : Parser Stmt

**Note:** If you need something more custom, do not be afraid to check
out the implementation and customize it for your case. It is better to
get nice error messages with a lower-level implementation than to try
to hack high-level parsers to do things they are not made for.
-}
sequence
  : { start : String
    , separator : String
    , end : String
    , spaces : Parser ()
    , item : Parser a
    , trailing : Trailing
    }
  -> Parser (List a)
sequence { start, end, spaces, item, separator, trailing } =
  symbol start
    |- spaces
    |- sequenceEnd end spaces item separator trailing


{-| What’s the deal with trailing commas? Are they `Forbidden`?
Are they `Optional`? Are they `Mandatory`? Welcome to [shapes
club](http://poorlydrawnlines.com/comic/shapes-club/)!
-}
type Trailing = Forbidden | Optional | Mandatory


ignore : Parser ignore -> Parser keep -> Parser keep
ignore ignoreParser keepParser =
  map2 revAlways ignoreParser keepParser


(|-) : Parser ignore -> Parser keep -> Parser keep
(|-) =
  ignore


revAlways : ignore -> keep -> keep
revAlways _ keep =
  keep


sequenceEnd : String -> Parser () -> Parser a -> String -> Trailing -> Parser (List a)
sequenceEnd end spaces parseItem sep trailing =
  let
    chompRest item =
      case trailing of
        Forbidden ->
          sequenceEndForbidden end spaces parseItem sep [item]

        Optional ->
          sequenceEndOptional end spaces parseItem sep [item]

        Mandatory ->
          spaces
            |- symbol sep
            |- spaces
            |- sequenceEndMandatory end spaces parseItem sep [item]
  in
    oneOf
      [ parseItem
          |> andThen chompRest
      , symbol end
          |- succeed []
      ]


sequenceEndForbidden : String -> Parser () -> Parser a -> String -> List a -> Parser (List a)
sequenceEndForbidden end spaces parseItem sep revItems =
  let
    chompRest item =
      sequenceEndForbidden end spaces parseItem sep (item :: revItems)
  in
    ignore spaces <|
      oneOf
        [ symbol sep
            |- spaces
            |- andThen chompRest parseItem
        , symbol end
            |- succeed (List.reverse revItems)
        ]


sequenceEndOptional : String -> Parser () -> Parser a -> String -> List a -> Parser (List a)
sequenceEndOptional end spaces parseItem sep revItems =
  let
    parseEnd =
      andThen (\_ -> succeed (List.reverse revItems)) (symbol end)

    chompRest item =
      sequenceEndOptional end spaces parseItem sep (item :: revItems)
  in
    ignore spaces <|
      oneOf
        [ symbol sep
            |- spaces
            |- oneOf [ andThen chompRest parseItem, parseEnd ]
        , parseEnd
        ]


sequenceEndMandatory : String -> Parser () -> Parser a -> String -> List a -> Parser (List a)
sequenceEndMandatory end spaces parseItem sep revItems =
  let
    chompRest item =
      sequenceEndMandatory end spaces parseItem sep (item :: revItems)
  in
    oneOf
      [ andThen chompRest <|
          parseItem
            |. spaces
            |. symbol sep
            |. spaces
      , symbol end
          |- succeed (List.reverse revItems)
      ]



-- WHITESPACE


{-| Create a custom whitespace parser. It will always chomp the
`' '`, `'\r'`, and `'\n'` characters, but you can customize some
other things. Here are some examples:

    elm : Parser ()
    elm =
      whitespace
        { allowTabs = False
        , lineComment = LineComment "--"
        , multiComment = NestableComment "{-" "-}"
        }

    js : Parser ()
    js =
      whitespace
        { allowTabs = True
        , lineComment = LineComment "//"
        , multiComment = UnnestableComment "/*" "*/"
        }

If you need further customization, please open an issue describing your
scenario or check out the source code and write it yourself. This is all
built using stuff from the root `Parser` module.
-}
whitespace
  : { allowTabs : Bool
    , lineComment : LineComment
    , multiComment : MultiComment
    }
  -> Parser ()
whitespace { allowTabs, lineComment, multiComment } =
  let
    tabParser =
      if allowTabs then
        [ Parser.ignore zeroOrMore isTab ]
      else
        []

    lineParser =
      case lineComment of
        NoLineComment ->
          []

        LineComment start ->
          [ symbol start
              |. ignoreUntil "\n"
          ]

    multiParser =
      case multiComment of
        NoMultiComment ->
          []

        UnnestableComment start end ->
          [ symbol start
              |. ignoreUntil end
          ]

        NestableComment start end ->
          [ nestableComment start end
          ]
  in
    whitespaceHelp <|
      oneOf (tabParser ++ lineParser ++ multiParser)


chompSpaces : Parser ()
chompSpaces =
  Parser.ignore zeroOrMore isSpace


isSpace : Char -> Bool
isSpace char =
  char == ' ' || char == '\n' || char == '\r'


isTab : Char -> Bool
isTab char =
  char == '\t'


whitespaceHelp : Parser a -> Parser ()
whitespaceHelp parser =
  ignore chompSpaces <|
    oneOf [ andThen (\_ -> whitespaceHelp parser) parser, succeed () ]


{-| Are line comments allowed? If so, what symbol do they start with?

    LineComment "--"   -- Elm
    LineComment "//"   -- JS
    LineComment "#"    -- Python
    NoLineComment      -- OCaml
-}
type LineComment = NoLineComment | LineComment String


{-| Are multi-line comments allowed? If so, what symbols do they start
and end with?

    NestableComment "{-" "-}"    -- Elm
    UnnestableComment "/*" "*/"  -- JS
    NoMultiComment               -- Python

In Elm, you can nest multi-line comments. In C-like languages, like JS,
this is not allowed. As soon as you see a `*/` the comment is over no
matter what.
-}
type MultiComment
  = NoMultiComment
  | NestableComment String String
  | UnnestableComment String String


nestableComment : String -> String -> Parser ()
nestableComment start end =
  case (String.uncons start, String.uncons end) of
    (Nothing, _) ->
      fail "Trying to parse a multi-line comment, but the start token cannot be the empty string!"

    (_, Nothing) ->
      fail "Trying to parse a multi-line comment, but the end token cannot be the empty string!"

    ( Just (startChar, _), Just (endChar, _) ) ->
      let
        isNotRelevant char =
          char /= startChar && char /= endChar
      in
        symbol start
          |. nestableCommentHelp isNotRelevant start end 1


nestableCommentHelp : (Char -> Bool) -> String -> String -> Int -> Parser ()
nestableCommentHelp isNotRelevant start end nestLevel =
  lazy <| \_ ->
    ignore (Parser.ignore zeroOrMore isNotRelevant) <|
      oneOf
        [ ignore (symbol end) <|
            if nestLevel == 1 then
              succeed ()
            else
              nestableCommentHelp isNotRelevant start end (nestLevel - 1)
        , ignore (symbol start) <|
            nestableCommentHelp isNotRelevant start end (nestLevel + 1)
        , ignore (Parser.ignore (Exactly 1) isChar) <|
            nestableCommentHelp isNotRelevant start end nestLevel
        ]


isChar : Char -> Bool
isChar char =
  True
