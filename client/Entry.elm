module Entry exposing (..)

-- builtins
import Task
-- import Result exposing (Result)
-- import Dict
-- import Set

-- lib
import Dom
-- import Result.Extra as RE
-- import Maybe.Extra as ME

-- dark
-- import Util
import Defaults
import Types exposing (..)
-- import Autocomplete
import Viewport
-- import EntryParser exposing (AST(..), ACreating(..), AExpr(..), AFillParam(..), AFillResult(..), ARef(..))
import Util
import AST
import Toplevel as TL


---------------------
-- Nodes
---------------------
updateValue : String -> Modification
updateValue target =
  Many [ AutocompleteMod <| ACSetQuery target ]

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
-- refocus : Bool -> Focus -> Focus
-- refocus re default =
--   case default of
--     FocusNext id -> if re then Refocus id else default
--     FocusExact id -> if re then Refocus id else default
--     f -> f
--

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  let id = TLID (Util.random ())
      hid1 = HID (Util.random ())
      hid2 = HID (Util.random ())
      hid3 = HID (Util.random ())
      ast = case value of
              "if" ->
                (If (Hole hid1) (Hole hid2) (Hole hid3))
              str ->
                FnCall str [Hole hid1]
  in
  case cursor of
    Creating pos ->
      RPC ([SetAST id pos ast], FocusNext id)
    Filling tlid hid ->
      let tl = TL.getTL m tlid in
      RPC ([ SetAST tl.id tl.pos (AST.replaceHole hid ast tl.ast)]
           , FocusNext tlid)

  -- let pt = EntryParser.parseFully value
  -- in case pt of
  --   Ok pt -> execute m re <| EntryParser.pt2ast m cursor pt
  --   Err error -> Error <| EntryParser.toErrorMessage <| EntryParser.addCursorToError error cursor
