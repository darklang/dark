module Parser.LowLevel exposing
  ( getIndentLevel
  , withIndentLevel

  , getPosition
  , getRow
  , getCol

  , getOffset
  , getSource
  )

{-| You are unlikely to need any of this under normal circumstances.

# Indentation
@docs getIndentLevel, withIndentLevel

# Row, Column, Offset, and Source
@docs getPosition, getRow, getCol, getOffset, getSource

-}

import Parser exposing (Parser)
import Parser.Internal as I exposing (State)



-- INDENTATION


{-| This parser tracks “indentation level” so you can parse indentation
sensitive languages. Indentation levels correspond to column numbers, so
it starts at 1.
-}
getIndentLevel : Parser Int
getIndentLevel =
  I.Parser <| \state -> I.Good state.indent state


{-| Run a parser with a given indentation level. So you will likely
use `getCol` to get the current column, `andThen` give that to
`withIndentLevel`.
-}
withIndentLevel : Int -> Parser a -> Parser a
withIndentLevel newIndent (I.Parser parse) =
  I.Parser <| \state1 ->
    case parse (changeIndent newIndent state1) of
      I.Good a state2 ->
        I.Good a (changeIndent state1.indent state2)

      I.Bad x state2 ->
        I.Bad x (changeIndent state1.indent state2)


changeIndent : Int -> State ctx -> State ctx
changeIndent newIndent { source, offset, context, row, col } =
  { source = source
  , offset = offset
  , indent = newIndent
  , context = context
  , row = row
  , col = col
  }



-- POSITION


{-| Code editors treat code like a grid. There are rows and columns.
In most editors, rows and colums are 1-indexed. You move to a new row
whenever you see a `\n` character.

The `getPosition` parser succeeds with your current row and column
within the string you are parsing.
-}
getPosition : Parser (Int, Int)
getPosition =
  I.Parser <| \state -> I.Good (state.row, state.col) state


{-| The `getRow` parser succeeds with your current row within
the string you are parsing.
-}
getRow : Parser Int
getRow =
  I.Parser <| \state -> I.Good state.row state


{-| The `getCol` parser succeeds with your current column within
the string you are parsing.
-}
getCol : Parser Int
getCol =
  I.Parser <| \state -> I.Good state.col state


{-| Editors think of code as a grid, but behind the scenes it is just
a flat array of UTF16 characters. `getOffset` tells you your index in
that flat array. So if you have read `"\n\n\n\n"` you are on row 5,
column 1, and offset 4.

**Note:** browsers use UTF16 strings, so characters may be one or two 16-bit
words. This means you can read 4 characters, but your offset will move by 8.
-}
getOffset : Parser Int
getOffset =
  I.Parser <| \state -> I.Good state.offset state


{-| Get the entire string you are parsing right now. Paired with
`getOffset` this can let you use `String.slice` to grab substrings
with very little intermediate allocation.
-}
getSource : Parser String
getSource =
  I.Parser <| \state -> I.Good state.source state

