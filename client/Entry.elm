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
        Just <| FnCall (hid ()) name (holes (List.length function.parameters))
      Nothing -> Nothing

submit : Model -> Bool -> EntryCursor -> String -> Modification
submit m re cursor value =
  let id = tlid ()
      eid = hid ()
      hid1 = hid ()
      hid2 = hid ()
      hid3 = hid ()
      parseAst v =
        case v of
          "if" ->
            Just (If eid (Hole hid1) (Hole hid2) (Hole hid3))
          "let" ->
              Just (Let eid [(BindHole hid1, Hole hid2)] (Hole hid3))
          "lambda" ->
            Just (Lambda eid ["var"] (Hole hid1))
          str ->
            if RT.tipeOf str == TIncomplete || AST.isInfix str
            then createFunction m value
            else Just <| Value eid str

  in
  case cursor of
    Creating pos ->
      case parseAst value of
        Nothing -> NoChange
        Just v -> RPC ([SetAST id pos v], FocusNext id)
    Filling tlid hid ->
      let tl = TL.getTL m tlid
      in
          if TL.isBindHole m tlid hid
          then
            RPC ([SetAST tl.id tl.pos (AST.replaceBindHole hid value tl.ast)]
            , FocusNext tl.id)
          else
            case parseAst value of
              Nothing -> NoChange
              Just v -> RPC ([SetAST tl.id tl.pos (AST.replaceHole hid v tl.ast)]
              , FocusNext tl.id)

  -- let pt = EntryParser.parseFully value
  -- in case pt of
  --   Ok pt -> execute m re <| EntryParser.pt2ast m cursor pt
  --   Err error -> Error <| EntryParser.toErrorMessage <| EntryParser.addCursorToError error cursor
