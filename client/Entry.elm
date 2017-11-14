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
import Runtime as RT


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

tlid : () -> TLID
tlid unit = TLID (Util.random unit)

hid : () -> HID
hid unit = HID (Util.random unit)

createFunction : Model -> FnName -> Maybe Expr
createFunction m name =
  let holes count = List.map (\_ -> Hole (hid ())) (List.range 1 count)
      fn = m.complete.functions
           |> List.filter (\fn -> fn.name == name)
           |> List.head
  in
    case fn of
      Just function ->
        Just <| FnCall name (holes (List.length function.parameters))
      Nothing -> Nothing

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  let id = tlid ()
      hid1 = hid ()
      hid2 = hid ()
      hid3 = hid ()
      ast = case value of
              "if" ->
                Just (If (Hole hid1) (Hole hid2) (Hole hid3))
              str ->
                if RT.tipeOf str == TIncomplete || AST.isInfix str
                then createFunction m value
                else Just <| Value str
  in
  case (ast, cursor) of
    (Nothing, _) -> NoChange
    (Just ast, Creating pos) ->
      RPC ([SetAST id pos ast], FocusNext id)
    (Just ast, Filling tlid hid) ->
      let tl = TL.getTL m tlid in
      RPC ([ SetAST tl.id tl.pos (AST.replaceHole hid ast tl.ast)]
           , FocusNext tlid)

  -- let pt = EntryParser.parseFully value
  -- in case pt of
  --   Ok pt -> execute m re <| EntryParser.pt2ast m cursor pt
  --   Err error -> Error <| EntryParser.toErrorMessage <| EntryParser.addCursorToError error cursor
